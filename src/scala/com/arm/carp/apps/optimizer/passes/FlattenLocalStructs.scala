/*
 * Copyright (c) 2013-2014, ARM Limited
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.arm.carp.apps.optimizer.passes

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import com.arm.carp.pencil._

/** Analyze a function for local struct variables that can be flattened.
  *
  * We first build a set of candidate variables.  We then build a blacklist
  * of variables that are used in constructs that we do not flatten.
  */
private object Analyzer {
  /** Return true if argument is a type that we consider for flattening.
    */
  private def isFlattenableType(in: Type) = {
    in match {
      case s: StructType => s.fields forall (_._2.isScalar)
      case _ => false
    }
  }

  /** Return true if we can expand the given expression.
    */
  private def isExpandable(in: Expression) = {
    in match {
      case _: ScalarUnaryExpression |
           _: ScalarBinaryExpression |
           _: ScalarVariableRef |
           _: ScalarIdxExpression |
           _: ScalarStructSubscription => true
      case _ => false
    }
  }

  /** Find struct variables that are candidates for flattening.
    */
  private object CandidateFinder extends Walker {
    val config = WalkerConfig.expressions

    private val candidates = Set[Variable]()

    override def walkScalarVariable(in: ScalarVariableRef) = {
      if (isFlattenableType(in.expType)) {
        candidates += in.variable
      }
      super.walkScalarVariable(in)
    }

    override def walkScalarStructSubscription(in: ScalarStructSubscription) = {
      in.base match {
        case s: ScalarVariableRef =>
          if (isFlattenableType(s.expType)) {
            candidates += s.variable
          }
        case _ =>
      }
      (in, None)
    }

    override def walkAssignment(in: AssignmentOperation) = {
      if (isExpandable(in.lvalue) && isExpandable(in.rvalue)) {
        super.walkAssignment(in)
      }
      else {
        Some(in)
      }
    }

    def findCandidates(f: Function): List[Variable] = {
      candidates.clear
      walkFunction(f)
      candidates.toList
    }
  }

  /** Find variables that we do not flatten because they are not local (e.g.
    * function parameters) or are used in constructs that we do not flatten
    * (e.g. return statements).
    */
  private object BlacklistBuilder extends Walker {
    val config = WalkerConfig.expressions

    private val blacklist = Set[Variable]()

    override def walkScalarVariable(in: ScalarVariableRef) = {
      blacklist += in.variable
      super.walkScalarVariable(in)
    }

    override def walkScalarStructSubscription(in: ScalarStructSubscription) = {
      in.base match {
        case s: ScalarVariableRef => (in, None)  // Don't blacklist
        case _ => super.walkScalarStructSubscription(in)
      }
    }

    override def walkAssignment(in: AssignmentOperation) = {
      if (!isExpandable(in.lvalue) || !isExpandable(in.rvalue)) {
        super.walkAssignment(in)
      }
      else {
        Some(in)
      }
    }

    def buildBlacklist(f: Function): List[Variable] = {
      blacklist.clear
      blacklist ++= f.params
      walkFunction(f)
      blacklist.toList
    }
  }

  def getWorklist(f: Function): List[Variable] = {
    val candidates = CandidateFinder.findCandidates(f)
    val blacklist = BlacklistBuilder.buildBlacklist(f)
    candidates.toList filter (!blacklist.toList.contains(_))
  }
}

/** Flatten local structs variables into individual fields.
  *
  * For example, local struct variable v in the following code:
  *   struct ComplexDouble v;  // has two double fields, Re and Im
  *   v.Re = ...
  *   v.Im = ...
  * is flattened into the following code:
  *   double v__field0;
  *   double v__field1;
  *   v__field0 = ...
  *   v__field1 = ...
  */
class FlattenLocalStructs extends Pass("flatten-local-structs", true) {
  val config = WalkerConfig.minimal

  /** Return struct fields for given type.
    */
  private def getFields(in: Type): Seq[(String, Type)] = {
    in match {
      case s: StructType => s.fields
      case a: ArrayType => getFields(a.base)
      case _ => Seq.empty
    }
  }

  /** Return ScalarType of i-th field of type.
    */
  private def getFieldScalarType(t: Type, i: Int): ScalarType = {
    val fields = getFields(t)
    fields(i)._2.asInstanceOf[ScalarType]
  }

  /** Expand all AssignmentOperations dealing with the variables in worklist.
    *
    * That is, expand struct copy assignments into a list of scalar
    * assignments, one for each struct field.
    */
  private class AssignmentExpander(worklist: List[Variable]) extends Walker {
    val config = WalkerConfig.minimal

    private var needsExpansion = false

    private def updateNeedsExpansion(in: Variable) = {
      if (needsFlattening(in)) {
        needsExpansion = true
      }
    }

    private def needsFlattening(in: Variable) = {
      worklist.contains(in)
    }

    /** Return n pairs of int,ScalarVariableRef, with each ScalarVariableRef
      * being unique.
      */
    private def makeScalarRefWorkList(s: ScalarVariableRef, n: Int): List[(Int, ScalarVariableRef)] = {
      List.range(0, n) zip List.tabulate(n)(i => new ScalarVariableRef(s.variable))
    }

    /** Return n pairs of int,ScalarIdxExpression, with any xxxVariableRefs
      * being unique.
      */
    private def makeScalarIdxWorkList(s: ScalarIdxExpression, n: Int): List[(Int, ScalarIdxExpression)] = {
      List.range(0, n) zip List.tabulate(n)(
        i => new ScalarIdxExpression(
          ExpressionCloner.walkArrayExpression(s.base)._1, ExpressionCloner.walkScalarExpression(s.op2)._1
        )
      )
    }

    /** Proxy to expandScalarVariableRef for LValues.
      */
    private def expandScalarVariableRefLValue(s: ScalarVariableRef, n: Int): Seq[ScalarExpression with LValue] = {
      expandScalarVariableRef(s, n) map (
        s => s match {
          case se: ScalarExpression with LValue => se
          case _ => Checkable.ice(s, "LValue expected")
        }
      )
    }

    /** Expand the ScalarVariableRef if it is a local struct variable.
      */
    private def expandScalarVariableRef(s: ScalarVariableRef, n: Int): Seq[ScalarExpression] = {
      makeScalarRefWorkList(s, n) map {
        (i => new ScalarStructSubscription(i._2, i._1))
      }
    }

    /** Proxy to expandScalarIdx for LValues.
      */
    private def expandScalarIdxLValue(s: ScalarIdxExpression, n: Int): Seq[ScalarExpression with LValue] = {
      expandScalarIdx(s, n) map (
        s => s match {
          case se: ScalarExpression with LValue => se
          case _ => Checkable.ice(s, "LValue expected")
        }
      )
    }

    /** Expand the ScalarIdxExpression if it is not a local variable.
      */
    private def expandScalarIdx(s: ScalarIdxExpression, n: Int): Seq[ScalarExpression] = {
      makeScalarIdxWorkList(s, n) map {
        (i => new ScalarStructSubscription(i._2, i._1))
      }
    }

    /** Return a list in which the input lhs expression has been expanded.
      */
    private def expandLValueStructs(in: ScalarExpression with LValue): Seq[ScalarExpression with LValue] = {
      in match {
        case v: ScalarVariableRef =>
          updateNeedsExpansion(v.variable)
          val n = getFields(v.variable.expType).length
          expandScalarVariableRefLValue(v, n)

        case i: ScalarIdxExpression =>
          val n = getFields(i.base.expType).length
          expandScalarIdxLValue(i, n)

        case _ =>
          Seq.empty
      }
    }

    /** Return a list in which the input rhs expression has been expanded.
      */
    private def expandStructs(in: ScalarExpression): Seq[ScalarExpression] = {
      in match {
        case s: ScalarVariableRef =>
          updateNeedsExpansion(s.variable)
          val n = getFields(s.variable.expType).length
          expandScalarVariableRef(s, n)

        case i: ScalarIdxExpression =>
          val n = getFields(i.base.expType).length
          expandScalarIdx(i, n)

        case _ =>
          Seq.empty
      }
    }

    private def createNewAssignment(pair: ((ScalarExpression with LValue, ScalarExpression))) = {
      new AssignmentOperation(pair._1, pair._2)
    }

    /** Create new BlockOperation containing AssignmentOperations if either lhs
      * or rhs contains a local struct that should be expanded.
      */
    def updateAssignment(in: AssignmentOperation): Operation = {
      needsExpansion = false
      val newlhs = expandLValueStructs(in.lvalue)
      val newrhs = expandStructs(in.rvalue)
      if (newlhs.length == newrhs.length && needsExpansion) {
        new BlockOperation((newlhs zip newrhs) map createNewAssignment)
      } else {
        in
      }
    }

    override def walkAssignment(in: AssignmentOperation) = {
      Some(updateAssignment(in))
    }
  }

  /** Replace struct subscriptions by their flat variables
    * (e.g., p.x becomes p__field_x).
    */
  private class SubscrReplacer(worklist: List[Variable]) extends Walker {
    val config = WalkerConfig.expressions

    /** (structvar,fieldnr) -> scalarvar
      * Contains the scalars that have been introduced for the fields of
      * the flattened structs.
      */
    private val structFieldToScalar = new HashMap[(ScalarVariableDef,Int),ScalarVariableDef]

    /** Create a new local ScalarVariableDef for field 'fieldnr' of variable 's'.
      */
    private def createNewLocalScalarVariable(s: ScalarVariableRef, fieldnr: Int) = {
      val newdef = new ScalarVariableDef(getFieldScalarType(s.variable.expType, fieldnr),
                                         s.variable.name + "__field" + fieldnr, None)
      structFieldToScalar.put((s.variable, fieldnr), newdef)
      newdef
    }

    /** Return a new ScalarVariableRef for field 'fieldnr' of variable 's'.
      * Create a new ScalarVariable if necessary.
      */
    private def getLocalScalarVariableRef(s: ScalarVariableRef, fieldnr: Int) = {
      structFieldToScalar.get((s.variable, fieldnr)) match {
        case Some(vardef) => new ScalarVariableRef(vardef)
        case None => new ScalarVariableRef(createNewLocalScalarVariable(s, fieldnr))
      }
    }

    /** Update ScalarStructSubscription if it has been replaced by a scalar
      * variable.
      */
    def updateScalarStructSubscription(in: ScalarStructSubscription) = {
      in.base match {
        case s: ScalarVariableRef if (worklist.contains(s.variable)) =>
          getLocalScalarVariableRef(s, in.field)
        case _ => in
      }
    }

    override def walkScalarStructSubscription(in: ScalarStructSubscription) = {
      (updateScalarStructSubscription(in), None)
    }
  }

  /** Entry point for this pass.
    */
  override def walkFunction(f: Function) = {
    val worklist = Analyzer.getWorklist(f)
    val expander = new AssignmentExpander(worklist)
    expander.walkFunction(f) match {
      case None => None
      case Some(newf) =>
        val replacer = new SubscrReplacer(worklist)
        replacer.walkFunction(newf)
    }
  }
}

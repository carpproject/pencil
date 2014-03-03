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
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import com.arm.carp.pencil._


object FlattenStructs extends Pass("flatten-structs") {
  val config = WalkerConfig.full.copy(consts = false)

  val locals = registerOption[Boolean]("locals", true)
  val flatten_external = registerOption[Boolean]("externals", true)

  private class FlattenedStructBase

  private case class NestedFlattenedStruct (val fields: Seq[FlattenedStructBase]) extends FlattenedStructBase

  private case class FlattenedField (val variable: Variable) extends FlattenedStructBase

  private val flattened = HashMap[Variable, FlattenedStructBase]()
  private val arrays_to_declare = ListBuffer[ArrayVariableDef]()
  val calls = Stack[Function]()

  private def getFlatName(base: String, fname: String) = base + "__" + fname

  private def buildFlattenStruct(base: String, atype: Type, arg: Boolean): FlattenedStructBase  = {
    atype match {
      case s: StructType => new NestedFlattenedStruct(s.fields map (f => buildFlattenStruct(getFlatName(base, f._1), f._2, arg)))
      case t: ScalarType => new FlattenedField(new ScalarVariableDef(t, base, None))
      case t: ArrayType =>
        val variable = new ArrayVariableDef(t, base, true, None)
        if (!arg) {
          arrays_to_declare += variable
        }
        new FlattenedField(variable)
    }
  }

  private def getAllVariables (in: FlattenedStructBase): Seq[Variable] = {
    in match {
      case field: FlattenedField => Seq(field.variable)
      case nested: NestedFlattenedStruct =>
        (nested.fields map getAllVariables).flatten
    }
  }

  private def getAllVariableRefs (in: FlattenedStructBase): Seq[Expression] = {
    getAllVariables(in) map (v => v match {
        case s:ScalarVariableDef => new ScalarVariableRef (s)
        case a:ArrayVariableDef => new ArrayVariableRef (a)
      })
  }

  override def walkFunctionArguments(in: Seq[Variable]) = {
    (in map (arg => arg match {
        case sv: ScalarVariableDef => {
          sv.expType match {
            case s: StructType =>
              val res = flattenStruct(sv, true)
              getAllVariables(res)
            case _ => List(arg)
          }
        }
        case av: ArrayVariableDef =>
          walkArrayType(av.expType)
          List(av)
      }
    )).flatten
  }

  override def walkFunction(in: Function) = {
    if (in.ops.isDefined || flatten_external()) {
      arrays_to_declare.clear
      val res = super.walkFunction(in).get
      res.ops match {
        case None =>
        case Some(ops) => {
          val init = arrays_to_declare map (v => new ArrayDeclOperation(v))
          res.ops = Some(new BlockOperation (init ++ ops.ops))
        }
      }
      Some(res)
    } else {
      Some(in)
    }
  }

  private def getInnermostStruct(in: ScalarExpression): Option[ScalarVariableRef] = {
    in match {
      case s: ScalarStructSubscription => getInnermostStruct(s.base)
      case v: ScalarVariableRef => Some(v)
      case _ => None
    }
  }

  private def buildSubsList(exp: ScalarExpression): List[Int] = {
    exp match {
      case _: ScalarVariableRef => List()
      case s: ScalarStructSubscription => s.field :: buildSubsList(s.base)
      case field => ice(field, "unexpected structure field (nested struct expected)")
    }
  }

  private def extractVariables(exp: ScalarExpression, data: FlattenedStructBase): FlattenedStructBase = {
    val subs = buildSubsList(exp).reverse
    subs.foldLeft(data)((d, f) => d match {
        case NestedFlattenedStruct(fields) => fields(f)
      })
  }

  private def getFlattenedVariable(in: ScalarStructSubscription, base: ScalarVariableRef) = {
    val flats = flattened(base.variable)
    extractVariables(in, flats) match {
      case FlattenedField(variable: ScalarVariableDef) => (new ScalarVariableRef(variable), None)
      case other => ice(other, "unexpected flattening result")
    }
  }

  private def flattenStruct(in: ScalarVariableDef, arg: Boolean) = {
    val tmp = buildFlattenStruct(in.name, in.expType, arg)
    flattened.put(in, tmp)
    tmp
  }

  private def tryFlattenLocalStruct (in: ScalarVariableDef) = {
    flattened.get(in) match {
      case Some(data) => Some(data)
      case None if locals() => Some(flattenStruct(in, false))
      case None => None
    }
  }

  override def walkScalarStructSubscription(in: ScalarStructSubscription) = {
    getInnermostStruct(in) match {
      case Some(struct) if tryFlattenLocalStruct(struct.variable).isDefined => getFlattenedVariable(in, struct)
      case _ => super.walkScalarStructSubscription(in)
    }
  }

  private def buildArrayAssignment (e1: Expression, e2: Expression): Operation = {
    (e1, e2) match {
      case (a1:ArrayExpression, a2: ArrayExpression) =>
        val iterator = new ScalarVariableDef(IntegerType(false, 32, false), "siter", None)
        val (s1, s2) = a1.expType.base match {
          case _:ArrayType => (new ArrayIdxExpression(a1, new ScalarVariableRef(iterator)),
                               new ArrayIdxExpression(a2, new ScalarVariableRef(iterator)))
          case _:ScalarType => (new ScalarIdxExpression(a1, new ScalarVariableRef(iterator)),
                                new ScalarIdxExpression(a2, new ScalarVariableRef(iterator)))
        }
        val body = buildArrayAssignment(s1, s2)
        val range = new Range(new ScalarVariableRef(iterator), Constants.Integer32Constant0,
                              new MinusExpression(a1.expType.range, Constants.Integer32Constant1),
                              Constants.Integer32Constant1)
        new ForOperation(Seq(), range, new BlockOperation(List(body)))
      case (s1:ScalarExpression with LValue, s2: ScalarExpression) => new AssignmentOperation(s1, s2)
    }
  }

  private def buildAssignmentList(lvalues: Seq[Expression], rvalues: Seq[Expression]): Operation = {
    Checkable.assert(lvalues.size == rvalues.size, (lvalues, rvalues), "invalid arguments for assignment flattening")
    val ops = (lvalues, rvalues).zipped.map((l,r) => (l,r) match {
        case (s1:ScalarExpression with LValue, s2:ScalarExpression) => new AssignmentOperation(s1, s2)
        case (a1:ArrayExpression, a2: ArrayExpression) => buildArrayAssignment(a1, a2)
      })
    new BlockOperation(ops)
  }

  private def flattenRvalue(lvalue: Expression, rvalue: FlattenedStructBase) = {
    val llist = generateStructSubs(walkExpression(lvalue)._1)
    val rlist = getAllVariableRefs(rvalue)
    buildAssignmentList(llist, rlist)
  }

  private def flattenLvalue(lvalue: FlattenedStructBase, rvalue: Expression) = {
    val llist = getAllVariableRefs(lvalue)
    val rlist = generateStructSubs(walkExpression(rvalue)._1)
    buildAssignmentList(llist, rlist)
  }

  private def flattenLRvalue(lvalue: FlattenedStructBase, rvalue: FlattenedStructBase) = {
    val llist = getAllVariableRefs(lvalue)
    val rlist = getAllVariableRefs(rvalue)
    buildAssignmentList(llist, rlist)
  }

  private def tryFlattenAssignment(in: AssignmentOperation): Operation = {
    val lvalue_base = getInnermostStruct(in.lvalue)
    val rvalue_base = getInnermostStruct(in.rvalue)
    (lvalue_base, rvalue_base) match {
      case (None, None) => in
      case (Some(var1), None) =>
        tryFlattenLocalStruct (var1.variable) match {
          case None => in
          case Some(data) =>  flattenLvalue(data, in.rvalue)
        }
      case (None, Some(var2)) =>
        tryFlattenLocalStruct (var2.variable) match {
          case None => in
          case Some(data) => flattenRvalue(in.lvalue, data)
        }
      case (Some(var1), Some(var2)) =>
        (tryFlattenLocalStruct(var1.variable), tryFlattenLocalStruct(var2.variable)) match {
          case (None, None) => in
          case (Some(data), None) => flattenLvalue(data, in.rvalue)
          case (None, Some(data)) => flattenRvalue(in.lvalue, data)
          case (Some(ldata), Some(rdata)) => flattenLRvalue(ldata, rdata)
        }
    }
  }

  override def walkAssignment (in: AssignmentOperation) = {
    in.lvalue.expType match {
      case s: StructType =>
        super.walkAssignment(in) match {
          case Some(mov: AssignmentOperation) => Some(tryFlattenAssignment(mov))
          case op => ice(op, "unexpected operation")
        }
      case _ => super.walkAssignment(in)
    }
  }

  private def generateStructSubs(in: Expression): Seq[Expression] = {
    (in, in.expType) match {
      case (s: ScalarExpression, st:StructType) =>
      (0 until st.fields.size).map(n => generateStructSubs(st.fields(n)._2 match {
          case _:ScalarType => ScalarStructSubscription(ExpressionCloner.walkScalarExpression(s)._1, n)
          case _:ArrayType => ArrayStructSubscription(ExpressionCloner.walkScalarExpression(s)._1, n)
        })).flatten
      case _ => List(in)
    }
  }

  private def flattenArgument(in: ScalarExpression, etype: StructType): Seq[Expression] = {
    getInnermostStruct(in) match {
      case Some(variable) => {
        val data = flattened.get(variable.variable) match {
          case None => flattenStruct(variable.variable, true)
          case Some(data) => data
        }
        val actual = extractVariables(in, data)
        getAllVariableRefs(actual)
      }
      case _ => generateStructSubs(in)
    }
  }

  private def unflattenArgument(in: ScalarExpression, etype: StructType): (ScalarExpression, Option[Operation]) = {
    getInnermostStruct(in) match {
      case Some(variable) => {
        tryFlattenLocalStruct(variable.variable) match {
          case None => (in, None)
          case Some(pdata) => {
            val tmp = new ScalarVariableDef(etype, "tmp", None)
            val init = flattenRvalue(new ScalarVariableRef(tmp), pdata)
            (new ScalarVariableRef(tmp), Some(init))
          }
        }
      }
      case _ => (in, None)
    }
  }

  private def flattenArgList(in: Seq[Expression], internal: Boolean) = {
    val init = new ListBuffer[Operation]
    val res = (in map (arg => {
        (arg, arg.expType) match {
          case (s: ScalarExpression, st:StructType) => {
            if (flatten_external() || internal)
              flattenArgument(s, st)
            else {
              val res = unflattenArgument(s, st)
              res._2 match {
                case None =>
                case Some(op) => init += op
              }
              List(res._1)
            }
          }
          case _ => List(walkExpression(arg)._1)
        }
      }
    )).flatten
    (res, init.toList)
  }

  override def walkReturn (in: ReturnOperation) = {
    in.op match {
      case None => super.walkReturn(in)
      case Some(exp) =>
        exp.expType match {
          case st:StructType  =>
            val op = unflattenArgument(exp, st)
            in.op = Some(op._1)
            makeBlock(op._2, Some(in))
          case _ => super.walkReturn(in)
        }
    }
  }

  override def walkCallExpression(in: CallExpression) = {
    calls.push(in.func)
    val args = flattenArgList(in.args, in.func.ops.isDefined)
    val res = super.walkCallExpression(in.copy(args = args._1))
    calls.pop
    (res._1, Some(new BlockOperation(args._2)))
  }

}

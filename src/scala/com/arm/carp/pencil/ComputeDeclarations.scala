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

package com.arm.carp.pencil

/** This object specifies a set of scalar variables, defined in a
    [[BlockOperation]] it is attached to.  */
class DefinedVariables(val defs: Set[ScalarVariableDef]) extends OperationPassInfo

/** This object assigns each variable to the block, where this variable should
    be declared. The variable is assigned to the innermost block, which contains
    all uses of the variable (i.e. it tries to make each declaration as local
    as possible with respect to correctness).  */
object ComputeDeclarations {

  /**
    * Intermediate set of variable, used by a statement.
    * direct - variables, used by the statement directly
    * inherited - variable, inherited from children statements
    */
  class UsedVariablesInt(val direct: Set[ScalarVariableDef], val inherited: Set[ScalarVariableDef]) extends OperationPassInfo

  /** Collect definitions of all scalar variables used in expression.  */
  private def getVariables(in: Expression) = {
    ExpressionAnalyzer.getScalarVariables(in).map(_.variable)
  }

  /** Collect definitions of all scalar variables used in type declaration.  */
  private def getVariables(in: Type):Set[ScalarVariableDef] = {
    in match {
      case _:ScalarType => Set()
      case ArrayType(base, range) => getVariables(base) ++ getVariables(range)
    }
  }

  /**
    * Compute [[UsedVariablesInt]] information for each statement and store
    * it as statement pass specific information.
    *
    * Note, that for block statement, variables directly used by its direct
    *
    * @returns Computed sets.
    */
  private def directWalk(in: Operation): (Set[ScalarVariableDef], Set[ScalarVariableDef]) = {
    val defs: (Set[ScalarVariableDef], Set[ScalarVariableDef]) = in match {
      case mov: AssignmentOperation =>
        (getVariables(mov.lvalue) ++ getVariables(mov.rvalue), Set())
      case body: BlockOperation =>
        body.ops.foldLeft((Set[ScalarVariableDef](), Set[ScalarVariableDef]()))((a,b) => {
          val tmp = directWalk(b)
          (a._1 ++ tmp._1, a._2 ++ tmp._2)
        })
      case ifop: IfOperation =>
        val guard = getVariables(ifop.guard)
        val ops = directWalk(ifop.ops)
        val eops = ifop.eops match {
          case None => (Set[ScalarVariableDef](), Set[ScalarVariableDef]())
          case Some(body) =>
            directWalk(body)
        }
        (guard, ops._1 ++ ops._2 ++ eops._1 ++ eops._2)
      case forop: ForOperation =>
        val ops = directWalk(forop.ops)
        val range = getVariables(forop.range.low) ++ getVariables(forop.range.upper) ++ Set(forop.range.iter.variable)
        (range, ops._1 ++ ops._2)
      case whileop: WhileOperation =>
        val ops = directWalk(whileop.ops)
        val guard = getVariables(whileop.guard)
        (guard, ops._1 ++ ops._2)
      case ret: ReturnOperation =>
        ret.op match {
          case None => (Set(), Set())
          case Some(op) =>
            (getVariables(op), Set())
        }
      case exp: CallOperation => (getVariables(exp.op), Set())
      case decl: ArrayDeclOperation => (getVariables(decl.array.expType), Set())
      case _ => (Set(), Set())
    }
    in.info = Some(new UsedVariablesInt(defs._1, defs._2))
    (defs._1, defs._2)
  }

  /**
    * Use the [[UsedVariablesInt]] information to assign variable declarations
    * to blocks. Variable is assigned to the first block where one of the
    * following is true:
    *  - variable is directly referenced in the block
    *  - variables is referenced in more than one child of the block
    * In the second case variable must be defined in the block to avoid
    * false privatization.
    */
  def reverseWalk(in: BlockOperation, defined: Set[ScalarVariableDef]): Unit = {
    val info = in.info.get.asInstanceOf[UsedVariablesInt]
    val toDefine = info.direct ++ info.inherited.filter(p => in.ops.count(op => {
      val info = op.info.get.asInstanceOf[UsedVariablesInt]
      info.direct.contains(p) || info.inherited.contains(p)
    }) > 1)
    in.info = Some(new DefinedVariables(toDefine--defined))
    val ndefined = toDefine ++ defined
    in.ops.foreach(op => {
      op match {
        case block:BlockOperation => reverseWalk(block, ndefined)
        case ifop:IfOperation =>
          reverseWalk(ifop.ops, ndefined)
          if (ifop.eops.isDefined) {
        	  reverseWalk(ifop.eops.get, ndefined)
          }
        case whileop:WhileOperation => reverseWalk(whileop.ops, ndefined)
        case forop:ForOperation => reverseWalk(forop.ops, ndefined)
        case _ =>
      }
    })
  }

  def computeForFunction(in: Function, cst: Set[Variable]) = {
    in.ops match {
      case Some(body) =>
        directWalk(body)
        val defined = in.params.toSet ++ cst
        reverseWalk(body, defined.filter(_.expType.isScalar).map(_.asInstanceOf[ScalarVariableDef]))
      case None =>
    }
  }
}

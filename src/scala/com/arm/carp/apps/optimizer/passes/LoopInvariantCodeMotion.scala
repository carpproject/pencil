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

import com.arm.carp.pencil._
import com.arm.carp.pencil.ParentComputer
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

/**
 * This class implements basic loop invariant code motion.
 *
 * It moves the assignment statements in a loop body, with loop invariant as a RHS
 * outside the loop.
 */

object LICM extends Pass("licm") {
  val config = WalkerConfig.expressions

  private val loops = Stack[ForOperation]()

  private val invariants = Stack[ListBuffer[AssignmentOperation]]()

  private var idxExpressionCounter: Int = 0

  private def addInvariant(op: AssignmentOperation) = {
    op.parent = loops.top.parent
    invariants.top.append(op)
  }

  private def getInvariants() = invariants.top.toSeq

  private def enterFor (loop: ForOperation) = {
    invariants.push(ListBuffer[AssignmentOperation]())
    loops.push(loop)
  }

  private def leaveFor = {
    invariants.pop
    loops.pop
  }

  case class InvariantInfo(val invariant: Boolean) extends ExpressionPassInfo with OperationPassInfo

  private def isOutsideBlock(in: Operation, block: BlockOperation): Boolean = {
    in.parent match {
      case Some(parent) if block == parent => false
      case Some(parent) => isOutsideBlock(parent, block)
      case None => true
    }
  }

  private def isOuterLoopIterator (variable: ScalarVariableDef) = {
    loops.find(op => op.range.iter.variable == variable) match {
      case None => false
      case Some(op) => op != loops.top
    }
  }

  private def isInvariantInBlock(variable: ScalarVariableRef, block: BlockOperation) = {
    variable.info match {
      case Some(defs:DefineSet) =>
        if (variable.variable.iter) {
          assert(defs.data.isEmpty, defs.data, "unexpected def set")
          idxExpressionCounter == 0 && isOuterLoopIterator(variable.variable)
        } else {
          defs.data.forall(isOutsideBlock(_, block))
        }
      case _ => ice(variable, "reaching definition information expected")
    }
  }

  override def walkIterationVariable(in: ScalarVariableRef) = {
    (in, None)
  }

  override def walkScalarVariable(in: ScalarVariableRef) = {
    if (!loops.isEmpty) {
      val invariant = isInvariantInBlock(in, loops.top.ops)
      in.info = Some(new InvariantInfo(invariant))
    }
    super.walkScalarVariable(in)
  }

  override def walkConvertExpression(in: ConvertExpression) = {
    val op = walkScalarExpression(in.op1)
    (asInvariantExp(in.update(op._1), isInvariant(op._1)), op._2)
  }

  override def walkScalarConstant(in: Constant with ScalarExpression) =
    (asInvariantExp(in), None)

  private def asInvariant(in: AnnotatedScalarExpression, invariant: Boolean = true) = {
    (asInvariantExp(in._1), in._2)
  }

  private def asInvariantExp[T <: ScalarExpression](in: T, invariant: Boolean = true) = {
    in.info = Some(InvariantInfo(invariant))
    in
  }

  private def walkScalarBinaryExpressionOrig(in: ScalarBinaryExpression) = {
    val op1 = walkScalarExpression(in.op1)
    val op2 = walkScalarExpression(in.op2)
    (asInvariantExp(in.update(op1._1, op2._1), isInvariant(op1._1, op2._1)), make(op1._2, op2._2))
  }

  private def walkScalarBianryExpressionSum(in: ScalarBinaryExpression) = {
    val flattened = listFromSumTree(in).map(walkScalarExpression)
    val (raw_invariants, raw_rest) = flattened.partition(exp => isInvariant(exp._1))
    val invariants = raw_invariants.unzip
    val rest = raw_rest.unzip
    val init = make((invariants._2 ::: rest._2):_*)
    val exp = if (invariants._1.isEmpty) {
      sumTreeFromList(rest._1)
    } else {
      val invariant = liftInvariants(asInvariantExp(sumTreeFromList(invariants._1), true))
      sumTreeFromList(invariant :: rest._1)
    }
    (exp, init)
  }

  override def walkScalarBinaryExpression(in: ScalarBinaryExpression) = {
    in match {
      case _: PlusExpression | _: MinusExpression => walkScalarBianryExpressionSum(in)
      case _ => walkScalarBinaryExpressionOrig(in)
    }
  }

  override def walkScalarTernaryExpression (in: TernaryExpression) = {
    val op1 = walkScalarExpression(in.op1)
    val op2 = walkScalarExpression(in.op2)
    val op3 = walkScalarExpression(in.op3)
    (asInvariantExp(in.update(op1._1, op2._1, op3._1), isInvariant(op1._1, op2._1, op3._1)),
     make(op1._2, op2._2, op3._2))
  }

  override def walkScalarUnaryExpression(in: ScalarUnaryExpression) = {
    val op = walkScalarExpression(in.op1)
    (asInvariantExp(in.update(op._1), isInvariant(op._1)), op._2)
  }

  private def withUpdatedRvalue(mov: AssignmentOperation, upd: ScalarExpression) = {
    mov.rvalue = upd
    mov
  }

  private def isInvariant (in: ScalarExpression*) = {
    !loops.isEmpty && in.forall(_.info match {
      case Some(InvariantInfo(true)) => true
      case _ => false
    })
  }

  private def liftInvariants(exp: ScalarExpression) = {
    exp match {
      case _:ScalarExpression with Constant | _:ScalarVariableRef => exp
      case _ if isInvariant(exp) => {
        val tmp = ScalarVariableDef(exp.expType.updateConst(false), "loop_invariant", None)
        addInvariant(new AssignmentOperation(new ScalarVariableRef(tmp), exp))
        asInvariantExp(new ScalarVariableRef(tmp))
        }
      case _ => exp
    }
  }

  override def walkScalarExpression(in: ScalarExpression) = {
    (liftInvariants(super.walkScalarExpression(in)._1), None)
  }

  override def walkScalarIdxExpression (in: ScalarIdxExpression) = {
    idxExpressionCounter += 1
    val res = super.walkScalarIdxExpression(in)
    idxExpressionCounter -= 1
    res
  }

  override def walkArrayIdxExpression (in: ArrayIdxExpression) = {
    idxExpressionCounter += 1
    val res = super.walkArrayIdxExpression(in)
    idxExpressionCounter -= 1
    res
  }

  override def walkAssignment(in: AssignmentOperation) = {
    val rvalue = walkScalarExpression(in.rvalue)._1
    in.rvalue = rvalue
    in.lvalue match {
      case variable:ScalarVariableRef => Some(in)
      case _ => {
        in.lvalue = walkLValueExpression(in.lvalue)._1
        Some(in)
      }
    }
  }

  override def walkRange(in: Range) = (in, None)

  override def walkFor(in: ForOperation) = {
    enterFor(in)
    val res = super.walkFor(in)
    val invariants = getInvariants
    if (invariants.size > 0) {
      set_changed
    }
    leaveFor
    make (Some(new BlockOperation(invariants)), res)
  }

  override def walkFunction(in: Function) = {
    ParentComputer.walkFunction(in)
    ReachingDefinitions.computeForFunction(in)
    super.walkFunction(in)
  }
}

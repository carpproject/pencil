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

/**
 * This class implements basic loop invariant code motion.
 *
 * It moves the assignment statements in a loop body, with loop invariant as a RHS
 * outside the loop.
 */

class LICM extends Pass("licm", true) {
  val config = WalkerConfig.minimal

  val loops = Stack[ForOperation]()

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
          isOuterLoopIterator(variable.variable)
        } else {
          defs.data.forall(isOutsideBlock(_, block))
        }
      case _ => ice(variable, "reaching definition information expected")
    }
  }

  override def walkScalarVariable(in: ScalarVariableRef) = {
    val invariant = isInvariantInBlock(in, loops.top.ops)
    in.info = Some(new InvariantInfo(invariant))
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

  override def walkScalarBinaryExpression(in: ScalarBinaryExpression) = {
    val op1 = walkScalarExpression(in.op1)
    val op2 = walkScalarExpression(in.op2)
    (asInvariantExp(in.update(op1._1, op2._1), isInvariant(op1._1, op2._1)), make(op1._2, op2._2))
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

  private def canBeMoved(variable: ScalarVariableRef, self: AssignmentOperation, block: BlockOperation) = {
    variable.info match {
      case Some(defs:DefineSet) => defs.data.filter(_ != self).forall(isOutsideBlock(_, block))
      case _ => ice(variable, "reaching definition information expected")
    }
  }

  private def withUpdatedRvalue(mov: AssignmentOperation, upd: AnnotatedScalarExpression) = {
    mov.rvalue = upd._1
    (Some(mov), upd._2)
  }

  private def moveUp(mov: AssignmentOperation, block: BlockOperation) = {
    mov.parent = block.parent
    (None, Some(mov))
  }

  private def tryMoveAssignment(mov: AssignmentOperation, block: BlockOperation) = {
    val processed = walkScalarExpression(mov.rvalue)
    if (isInvariant(processed._1)) {
      mov.lvalue match {
        case variable:ScalarVariableRef if canBeMoved(variable, mov, block)
        => moveUp(mov, block)
        case _ => withUpdatedRvalue(mov, liftInvariants(processed))
      }
    } else {
      withUpdatedRvalue(mov, processed)
    }
  }

  private def tryMove(in: Operation, block: BlockOperation) = {
    in match {
      case mov: AssignmentOperation => tryMoveAssignment(mov, block)
      case _ => (Some(in), None)
    }
  }

  private def isInvariant (in: ScalarExpression*) = {
    in.forall(_.info match {
      case Some(InvariantInfo(true)) => true
      case _ => false
    })
  }

  private def liftInvariants(in: AnnotatedScalarExpression) = {
    val code = in._2
    val exp = in._1
    exp match {
      case _:ScalarExpression with Constant | _:ScalarVariableRef => in
      case _ if isInvariant(exp) => {
        val tmp = ScalarVariableDef(in._1.expType.updateConst(false), "loop_invariant", None)
        (asInvariantExp(new ScalarVariableRef(tmp)),
         make (code, Some(new AssignmentOperation(new ScalarVariableRef(tmp), in._1))))
        }
      case _ => in
    }
  }

  override def walkScalarExpression(in: ScalarExpression) = {
    liftInvariants(super.walkScalarExpression(in))
  }

  override def walkFor(in: ForOperation) = {
    loops.push(in)
    val res = super.walkFor(in)
    val block = in.ops

    val processed = block.ops.map(tryMove(_, block))

    in.ops.ops = processed.map(_._1).flatten
    loops.pop
    Some(new BlockOperation(List(new BlockOperation(processed.map(_._2).flatten), in)))
  }

  override def walkFunction(in: Function) = {
    ParentComputer.walkFunction(in)
    ReachingDefinitions.computeForFunction(in)
    super.walkFunction(in)
  }
}

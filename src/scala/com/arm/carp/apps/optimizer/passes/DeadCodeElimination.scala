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
import com.arm.carp.pencil.ReachingDefinitions
import com.arm.carp.pencil.ReachSet
import scala.collection.mutable.Set

/**
  * Remove unreachable (dead) and useless code from PENCIL program.
  *
  * The following is considered dead code:
  * - if statement with empty body and else-body and side effect free guard
  * - while statement with empty body and side effect free guard
  * - empty block statement
  * - assignment statement with side effect free lvalue and rvalue expressions,
  *   which doesn't reach any other expression
  * - if (const) {...} statements
  *     if (false) {A;}else{B;} is transformed into {B;}
  *     if (true) {A;}else{B;} is transformed into {A;}
  * - while (false) {...} statements
  * - for loop with empty body
  * - Statements after return in the same block
  */
object DeadCodeElimination extends Pass("dce") {

  val config = WalkerConfig.expressions

  val called = Set[Function]()

  override def walkIf(in: IfOperation): Option[Operation] = {
    walkGuardExpression(in.guard)
    in.guard match {
      case BooleanConstant(Some(false)) =>
        set_changed
        walkBlock(in.eops)
      case BooleanConstant(Some(true)) =>
        set_changed
        walkBlock(in.ops)
      case guard =>
        val body = walkBlock(in.ops)
        val ebody = walkBlock(in.eops)
        if (noSideEffects(guard) && body.isEmpty && ebody.isEmpty) {
          set_changed
          None
        } else {
          in.ops = getCleanBody(body)
          in.eops = ebody
          Some(in)
        }
    }
  }

  override def walkWhile(in: WhileOperation): Option[Operation] = {
    walkGuardExpression(in.guard)
    in.guard match {
      case BooleanConstant(Some(false)) =>
        set_changed
        None
      case _ =>
        in.ops = getCleanBody(walkBlock(in.ops))
        Some(in)
    }
  }

  override def walkBlock(in: BlockOperation) = {
    var ret = false
    in.ops = in.ops.map(walkOperation(_) match {
      case _ if ret => None
      case None => None
      case Some(op:ReturnOperation) =>
        ret = true
        Some(op)
      case Some(op) => Some(op)
    }).flatten
    if (in.ops.size == 0) {
      None
    } else {
      Some(in)
    }
  }

  override def walkAssignment(in: AssignmentOperation) = {
    super.walkAssignment(in)
    val dead = in.info match {
      case Some(reaches: ReachSet) => reaches.data.filter(_ != in.lvalue).size == 0
      case _ => false
    }
    if (dead && noSideEffects(in.rvalue) && noSideEffects(in.lvalue)) {
      set_changed
      None
    } else {
      Some(in)
    }
  }

  private def noSideEffects(in: Expression): Boolean = {
    in match {
      case call: CallExpression => call.func.const && call.args.forall(noSideEffects(_))
      case _: ArraySubscription => false
      case _: VariableRef => true
      case _: Constant => true
      case exp: SingleArgumentExpression[_] => noSideEffects(exp.op1)
      case exp: DoubleArgumentExpression[_, _] => noSideEffects(exp.op1) && noSideEffects(exp.op2)
      case exp: TripleArgumentExpression[_, _, _] => noSideEffects(exp.op1) && noSideEffects(exp.op2) && noSideEffects(exp.op3)
      case _ => Checkable.ice(in, "unexpected expression")
    }
  }

  override def walkFor(in: ForOperation) = {
    walkRange(in.range)
    val body = walkBlock(in.ops)
    if (body.isEmpty && noSideEffects(in.range.low) && noSideEffects(in.range.upper)) {
      set_changed
      None
    } else {
      in.ops = getCleanBody(body)
      Some(in)
    }
  }

  override def walkCallExpression(in: CallExpression) = {
    called.add(in.func)
    super.walkCallExpression(in)
  }

  override def walkFunctions(in: Traversable[Function]) = {
    super.walkFunctions(in).filter(func => !func.local || called.contains(func))
  }

  override def walkProgram(in: Program) = {
    called.clear
    ReachingDefinitions.compute(in)
    super.walkProgram(in)
  }
}

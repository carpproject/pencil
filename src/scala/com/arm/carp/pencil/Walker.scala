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

/**
  * This class represents a [[Walker]] configuration.
  *
  * The options available:
  *  - expressions: walk expression trees
  *  - types: walk global type declarations
  *  - consts: walk global constant declarations
  *  - functions: walk function declarations
  *  - args: walk functon arguments
  */
case class WalkerConfigStorage(expressions: Boolean, types: Boolean, consts: Boolean, functions: Boolean, args: Boolean)

/** Contains predefined walker configurations.  */
object WalkerConfig {
  /**
    * Walk the full program tree.
    *
    * Use this config with caution since walking the full program tree might be expensive.
    * In general you should consider using a cheaper configuration.
    */
  val full = WalkerConfigStorage(true, true, true, true, true)

  /** Minimal configuration to walk CFG of each function without touching
      the expression trees. */
  val minimal = WalkerConfigStorage(false, false, false, true, false)

  /** Configuration to walk CFG of each function including the expression trees. */
  val expressions = WalkerConfigStorage(true, false, false, true, false)
}

/**
  * Provides the default program tree walker.
  *
  * It doesn't modify any parts of the program, only visits them.
  *
  * Optimization/analysis passes should inherit this trait and
  * override function, operation on transformed/analyzed constructs.
  * See [[Linearize]] pass for example.
  *
  * It's essential for the overriding function to call the original function
  * to ensure the correct processing of the children nodes.
  */
trait Walker extends Assertable with CommonOps with ExpressionWalker {

  val config: WalkerConfigStorage

  /**
   * Default action for leaf processing.
   * Can be overridden to call ICE (to ensure that all processing is implemented by pass).
   */
  def doNothing() {}

  def walkLValueExpression(in: ScalarExpression with LValue) = {
    walkScalarExpressionProxy(in) match {
      case (lvalue: ScalarExpression with LValue, init) => (lvalue, init)
      case rvalue => ice(rvalue, "unexpected rvalue")
    }
  }

  def walkScalarExpressionProxy(in: ScalarExpression) = {
    if (config.expressions) {
      walkScalarExpression(in)
    } else {
      (in, None)
    }
  }

  def walkExpressionProxy(in: Expression) = {
    if (config.expressions) {
      walkExpression(in)
    } else {
      (in, None)
    }
  }

  def walkGuardExpression(in: ScalarExpression) = {
    walkScalarExpressionProxy(in)
  }

  def walkAssignment(in: AssignmentOperation): Option[Operation] = {
    val lvalue = walkLValueExpression(in.lvalue)
    val rvalue = walkScalarExpressionProxy(in.rvalue)
    in.lvalue = lvalue._1
    in.rvalue = rvalue._1
    make(lvalue._2, rvalue._2, Some(in))
  }

  def walkRange(in: Range) = {
    val iter = walkIterationVariable(in.iter)
    val low = walkScalarExpressionProxy(in.low)
    val upper = walkScalarExpressionProxy(in.upper)
    (in.copy(low = low._1, upper = upper._1, iter = iter._1), make(low._2, upper._2, iter._2))
  }

  def getCleanBody(in: Option[BlockOperation]) = {
    in match {
      case Some(body) => body
      case None => new BlockOperation(List())
    }
  }

  def walkFor(in: ForOperation): Option[Operation] = {
    val body = getCleanBody(walkBlock(in.ops))
    in.ops = body
    val range = walkRange(in.range)
    in.range = range._1
    make(range._2, Some(in))
  }

  def walkIf(in: IfOperation): Option[Operation] = {
    in.ops = getCleanBody(walkBlock(in.ops))
    in.eops = walkBlock(in.eops)
    val guard = walkGuardExpression(in.guard)
    in.guard = guard._1
    make(guard._2, Some(in))
  }

  def walkWhile(in: WhileOperation): Option[Operation] = {
    in.ops = getCleanBody(walkBlock(in.ops))
    val guard = walkGuardExpression(in.guard)
    in.guard = guard._1
    make(guard._2, Some(in))
  }

  def walkReturn(in: ReturnOperation): Option[Operation] = {
    in.op match {
      case Some(op) =>
        val nop = walkScalarExpressionProxy(op)
        in.op = Some(nop._1)
        make(nop._2, Some(in))
      case None => Some(in)
    }
  }

  def walkCallOp(in: CallOperation): Option[Operation] = {
    walkCallExpression(in.op) match {
      case (call:CallExpression, init) =>
        in.op = call
        make(init, Some(in))
      case _ => None
    }
  }

  def walkBreak(in: BreakOperation): Option[Operation] = {
    Some(in)
  }

  def walkContinue(in: ContinueOperation): Option[Operation] = {
    Some(in)
  }

  def walkPENCILOperation(in: Operation with PENCILOperation): Option[Operation] = {
    val op = walkExpressionProxy(in.op)
    in.op = op._1
    make(op._2, Some(in))
  }

  def walkArrayType (in: ArrayType): Option[Operation] = {
    val range = walkScalarExpressionProxy(in.range)
    in.range = range._1
    in.base match {
      case _:ScalarType => range._2
      case arr:ArrayType =>
        val base = walkArrayType(arr)
        make(base, range._2)
    }
  }

  def walkArrayDeclOperation(in: ArrayDeclOperation) = {
    make(walkArrayType(in.array.expType), Some(in))
  }

  def walkOperation(in: Operation): Option[Operation] = {
    in match {
      case op: BlockOperation => walkBlock(op)
      case op: AssignmentOperation => walkAssignment(op)
      case op: ForOperation => walkFor(op)
      case op: IfOperation => walkIf(op)
      case op: WhileOperation => walkWhile(op)
      case op: CallOperation => walkCallOp(op)
      case op: BreakOperation => walkBreak(op)
      case op: ContinueOperation => walkContinue(op)
      case op: ReturnOperation => walkReturn(op)
      case op: PENCILOperation => walkPENCILOperation(op)
      case op: ArrayDeclOperation => walkArrayDeclOperation(op)
      case op => ice(op, "unexpected operation")
    }
  }

  def walkBlock(in: Option[BlockOperation]): Option[BlockOperation] = {
    in match {
      case Some(body) => walkBlock(body)
      case None => None
    }
  }

  def walkBlock(in: BlockOperation): Option[BlockOperation] = {
    in.ops = in.ops.map(walkOperation).filter(_.isDefined).map(_.get)
    if (in.ops.size == 0) {
      None
    } else {
      Some(in)
    }
  }

  def walkFunctionBody(in: Option[BlockOperation]) = {
    in match {
      case None => None
      case Some(block) => Some(getCleanBody(walkBlock(block)))
    }
  }

  def walkFunctionArgument(in: Variable) = {
    in
  }

  def walkFunctionArguments(in: Seq[Variable]) = {
    if (config.args) {
      in.map(walkFunctionArgument)
    } else {
      in
    }
  }

  def walkAccessFunction(in: Option[Function]) {
  }

  def walkFunction(f: Function): Option[Function] = {
    f.params = walkFunctionArguments(f.params)
    walkAccessFunction(f.access)
    f.ops = walkFunctionBody(f.ops)
    Some(f)
  }

  def walkGlobalType(t: StructType) = {
    Some(t)
  }

  def walkGlobalConst(c: Variable) = {
    Some(c)
  }

  def walkFunctions(in: Traversable[Function]) = {
    if (config.functions) {
      in.map(walkFunction).filter(_.isDefined).map(_.get)
    } else {
      in
    }
  }

  def walkGlobalTypes(in: Seq[StructType]) = {
    if (config.types) {
      in.map(walkGlobalType).filter(_.isDefined).map(_.get)
    } else {
      in
    }
  }

  def walkGlobalConsts(in: Seq[Variable]) = {
    if (config.consts) {
      in.map(walkGlobalConst).filter(_.isDefined).map(_.get)
    } else {
      in
    }
  }

  def walkProgram(program: Program) = new Program(walkFunctions(program.functions), walkGlobalTypes(program.types), walkGlobalConsts(program.consts))
}

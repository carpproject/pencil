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

/** Base for pass-specific information, attachable to operation object.  */
trait OperationPassInfo

/**
 * Iteration range representation.
 *
 * Represents an iteration range from (and including) low, to (and including)
 * upper, with stride step.
 */
case class Range(iter: ScalarVariableRef, low: ScalarExpression, upper: ScalarExpression, step: IntegerConstant) {
  iter.variable.iter = true
}

/** Base class for all PENCIL operations.  */
abstract class Operation {
  /**
   * PENCIL access block associated with this operation.
   */
  var access: Option[BlockOperation] = None
  var info: Option[OperationPassInfo] = None

  protected def create_copy(): Operation

  protected def update[T <: Operation](in: T):T = {
    in.scop = scop
    in
  }

  def copy() = {
    update(create_copy())
  }

  var parent: Option[BlockOperation] = None
  var scop: Boolean = false
}

/** Trait for PENCIL built-in functions.  */
trait PENCILOperation {
  val str: String
  var op: Expression
  override def toString = str + "(" + op.toString + ")"
}

/** Counter used to assignment unique ids to variables and functions.  */
object Counter {
  private var variables = 0;
  private var functions = 0;
  private var structs = 0;

  /**
   * @return Unique id for the variable.
   */
  def nextVariable() = {
    variables = variables + 1
    variables
  }

  /**
   * @return Unique id for the function.
   */
  def nextFunction() = {
    functions = functions + 1
    functions
  }
}

/** PENCIL assignment operation.  */
class AssignmentOperation(var lvalue: ScalarExpression with LValue, var rvalue: ScalarExpression) extends Operation {
  protected def create_copy = new AssignmentOperation(lvalue, rvalue)
  override def toString() = lvalue.toString + " = " + rvalue.toString
}

/** PENCIL return operation.  */
class ReturnOperation(var op: Option[ScalarExpression]) extends Operation {
  protected def create_copy = new ReturnOperation(op)
  override def toString() = {
    op match {
      case None => "return"
      case Some(op) => "return " + op.toString
    }
  }
}

/** PENCIL block operation.  */
class BlockOperation(var ops: Seq[Operation]) extends Operation {
  protected def create_copy = new BlockOperation(ops)
  override def copy() = update(create_copy())
  override def toString = "{\n" + ops.map(_.toString).mkString("\n") + "\n}"
}

/**
 * PENCIL call operation.
 *
 * Unlike C, pencil forbids using expressions as operations. For example,
 * the following line is valid (but has no effect) in C, but invalid in PENCIL:
 * 1 + 2;
 * The reason for this is that only AssignmentOperation is permitted
 * to have side effects.
 *
 * The only exception is a function call, which might be used outside assignment.
 */
class CallOperation(var op: CallExpression) extends Operation {
  protected def create_copy = new CallOperation(op)
  override def toString = op.toString
}

/** PENCIL if operation.  */
class IfOperation(
  var guard: ScalarExpression,
  var ops: BlockOperation,
  var eops: Option[BlockOperation]) extends Operation {
  protected def create_copy = new IfOperation(guard, ops, eops)
  override def toString = {
    "if (" + guard.toString + ")\n" + ops.toString + (eops match {
      case None => ""
      case Some(body) => "\nelse\n" + body.toString
    })
  }
}

/** Base class for `for' loop properties.  */
abstract class ForProperties

/** This property indicates that the corresponding loop is marked as a candidate for vectorization. */
object IvdepLoop extends ForProperties

/**
 * This property indicates that loop statements `labels' have no loop carried dependencies.
 * If the statement list is omitted, all statements are considered as having no loop carried dependencies.
 */
class IndependentLoop(val labels: Option[Seq[Operation]]) extends ForProperties

/**
 * PENCIL for operation.
 *
 * PENCIL for loops are counted loops, so the iteration space is defined as a Range.
 * A loop can also have additional properties attached (see ForProperties).
 */
class ForOperation(
  var properties: Seq[ForProperties],
  var range: Range, var ops: BlockOperation) extends Operation {
  protected def create_copy = new ForOperation(properties, range, ops)
  override def toString = "for (" + range.toString + ")\n" + ops.toString
}

/** PENCIL while operation.  */
class WhileOperation(
  var guard: ScalarExpression,
  var ops: BlockOperation) extends Operation {
  protected def create_copy = new WhileOperation(guard, ops)
  override def toString = "while (" + guard.toString + ")\n" + ops.toString
}

/** PENCIL break operation.  */
class BreakOperation extends Operation {
  protected def create_copy = new BreakOperation
  override def toString = "break"
}

/** PENCIL continue operation.  */
class ContinueOperation extends Operation {
  protected def create_copy = new ContinueOperation
  override def toString = "continue"
}

class ArrayDeclOperation(val array: ArrayVariableDef) extends Operation {
  protected def create_copy = new ArrayDeclOperation(array)
  override def toString = "DECL(" + array.toString + ")"
}

/** PENCIL function.  */
class Function(
  /* Name of the function.  */
  val name: String,
  /*Function parameters.  */
  var params: Seq[Variable],
  /*Function body. Is None for function declarations. */
  var ops: Option[BlockOperation],
  val retType: ScalarType,
  /* Corresponding summary function. */
  val access: Option[Function],
  /* Const function attribute. */
  val const: Boolean,
  /* Whether this function is a static/local function. */
  val local: Boolean,
  /* Whether this function is a summary function (unused by now). */
  val isSummary: Boolean,
  /* Unique id of the function.  */
  val id: Int = Counter.nextFunction) {
  def getName() = {
    if (local) {
      name + "_" + id
    } else {
      name
    }
  }
  override def toString() = name
}

/** __pencil_use built-in function.  */
class USEOperation(var op: Expression) extends Operation with PENCILOperation {
  val str = "__pencil_use"
  protected def create_copy = new USEOperation(op)
}

/** __pencil_def built-in function.  */
class DEFOperation(var op: Expression) extends Operation with PENCILOperation {
  val str = "__pencil_def"
  protected def create_copy = new DEFOperation(op)
}

/** __pencil_kill built-in function.  */
class KillOperation(var op: Expression) extends Operation with PENCILOperation {
  val str = "__pencil_kill"
  protected def create_copy = new KillOperation(op)
}

/** __pencil_assume built-in function.  */
class AssumeOperation(var op: Expression) extends Operation with PENCILOperation {
  val str = "__pencil_assume"
  protected def create_copy = new AssumeOperation(op)
}

/**
 * PENCIL program.
 *
 * A PENCIL program is a set of top-level declarations and definitions: function declarations and definitions,
 * structural type definitions (type aliases are resolved in PENCIL parser and are not stored), and global
 * constant definitions (global non-constant variables are not allowed).
 */
class Program(val functions: Traversable[Function], val types: Seq[StructType], val consts: Seq[Variable])

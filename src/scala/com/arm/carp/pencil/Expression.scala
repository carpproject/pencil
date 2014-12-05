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

/** Base for pass-specific information, attachable to expression objects.  */
trait ExpressionPassInfo

/** Base for all PENCIL expressions.  */
trait Expression {
  val expType: Type
  var info: Option[ExpressionPassInfo] = None // Pass specific information.
}

/** Base for PENCIL scalar expressions.  */
abstract class ScalarExpression extends Expression {
  override val expType: ScalarType
}

/** Base for PENCIL non-scalar (array) expressions.  */
abstract class ArrayExpression extends Expression {
  override val expType: ArrayType
}

/**
  * Trait for single argument expressions.
  *
  * This trait should be mixed (directly on indirectly) to any expression,
  * which takes single argument.
  */
trait SingleArgumentExpression[T <: Expression] {
  val op1: T
  def update(op1: T): SingleArgumentExpression[T]
}

/**
  * Trait for double argument expressions.
  *
  * This trait should be mixed (directly on indirectly) to any expression,
  * which takes two arguments.
  */

trait DoubleArgumentExpression[T1 <: Expression, T2 <: Expression] {
  val op1: T1
  val op2: T2
  def update(op1: T1, op2: T2): DoubleArgumentExpression[T1, T2]
}

/**
  * Trait for triple argument expressions.
  *
  * This trait should be mixed (directly on indirectly) to any expression,
  * which takes three arguments.
  */
trait TripleArgumentExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression] {
  val op1: T1
  val op2: T2
  val op3: T3
  def update(op1: T1, op2: T2, op3: T3): TripleArgumentExpression[T1, T2, T3]
}

/** Trait for all constants.  */
trait Constant

/** Trait for scalar binary expression, with result type equal to the type of the operands.  */
trait ScalarMathTyped {
  val op1: ScalarExpression
  val op2: ScalarExpression
  val expType = op1.expType.updateConst(true)
}

/** Trait for scalar unary expression, with result type equal to the type of the operands.  */
trait ScalarMathTypedUnary {
  val op1: ScalarExpression
  val expType = op1.expType.updateConst(true)
}

/* Level 1. Generic base class for scalar unary expressions.  */
abstract class ScalarUnaryExpression extends ScalarExpression with SingleArgumentExpression[ScalarExpression] {
  val op: String
  override def update(op1: ScalarExpression): ScalarUnaryExpression
}

/* Level 2. Specific base classes for different types of unary scalar expressions.  */

abstract class ScalarBooleanUnaryExpression extends ScalarUnaryExpression {
  val expType = BooleanType(true)
}

abstract class ScalarBitUnaryExpression extends ScalarUnaryExpression with ScalarMathTypedUnary

abstract class ScalarMathUnaryExpression extends ScalarUnaryExpression with ScalarMathTypedUnary

/*============================================================================*/

/* Level 1. Generic base class for scalar binary expressions.  */
abstract class ScalarBinaryExpression extends ScalarExpression with DoubleArgumentExpression[ScalarExpression, ScalarExpression] {
  val op: String
  override def update(op1: ScalarExpression, op2: ScalarExpression): ScalarBinaryExpression
}

/* Level 2. Specific base classes for different types of binary scalar expressions.  */
abstract class ScalarMathBinaryExpression extends ScalarBinaryExpression with ScalarMathTyped

abstract class ScalarComparisonBinaryExpression extends ScalarBinaryExpression {
  val expType = BooleanType(true)
}
abstract class ScalarBooleanBinaryExpression extends ScalarBinaryExpression {
  val expType = BooleanType(true)
}

abstract class ScalarBitBinaryExpression extends ScalarBinaryExpression with ScalarMathTyped

/*============================================================================*/

/** Trait to be mixed to any expression, which can serve as lvalue. */
trait LValue

/** Trait to be mixed with all variable definitions.  */
trait Variable {
  val expType: Type
  val name: String
  val id: Int
  var iter = false
  var global = false

  def getName() = if (global) name else name + "_" + id
}

/**
  * Trait to be mixed with all variable references.
  *
  * PENCIL IR separates variable definitions and variable declarations.
  * In order for variable to be used as expression, it must be wrapped into
  * reference object.
  */
trait VariableRef extends LValue with Expression {
  val variable: Variable
}

/** Trait for all array subscription expressions.  */
trait ArraySubscription extends LValue with Expression {
  val base: ArrayExpression
}

/** Trait for all struct subscription expressions.  */
trait StructSubscription extends LValue with Expression with SingleArgumentExpression[ScalarExpression]

/* Arithmetic expressions.  */

case class PlusExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarMathBinaryExpression {
  val op = "+"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class MultExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarMathBinaryExpression {
  val op = "*"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class MinusExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarMathBinaryExpression {
  val op = "-"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class DivExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarMathBinaryExpression {
  val op = "/"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class ModExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarMathBinaryExpression {
  val op = "%"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class UnaryMinusExpression(op1: ScalarExpression) extends ScalarMathUnaryExpression {
  val op = "-"
  def update(nop1: ScalarExpression) = this.copy(op1 = nop1)
}

case class UnaryPlusExpression(op1: ScalarExpression) extends ScalarMathUnaryExpression {
  val op = "+"
  def update(nop1: ScalarExpression) = this.copy(op1 = nop1)
}

/* Bit operation.  */
case class BitOrExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBitBinaryExpression {
  val op = "|"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class BitAndExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBitBinaryExpression {
  val op = "&"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class BitXorExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBitBinaryExpression {
  val op = "^"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class BitRShiftExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBitBinaryExpression {
  val op = ">>"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class BitLShiftExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBitBinaryExpression {
  val op = "<<"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class BitNegExpression(op1: ScalarExpression) extends ScalarBitUnaryExpression {
  val op = "~"
  def update(nop1: ScalarExpression) = this.copy(op1 = nop1)
}

/* Comparison expressions: Base: ScalarComparisonBinaryExpression.  */
case class EqualExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarComparisonBinaryExpression {
  val op = "=="
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class NEqualExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarComparisonBinaryExpression {
  val op = "!="
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class GreaterExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarComparisonBinaryExpression {
  val op = ">"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class LessExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarComparisonBinaryExpression {
  val op = "<"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class GreaterEqExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarComparisonBinaryExpression {
  val op = ">="
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class LessEqExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarComparisonBinaryExpression {
  val op = "<="
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

/* Boolean expressions: Base: ScalarBooleanBinaryExpression.  */

case class AndExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBooleanBinaryExpression {
  val op = "&&"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class OrExpression(op1: ScalarExpression, op2: ScalarExpression) extends ScalarBooleanBinaryExpression {
  val op = "||"
  def update(nop1: ScalarExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class NotExpression(op1: ScalarExpression) extends ScalarBooleanUnaryExpression {
  val op = "!"
  def update(nop1: ScalarExpression) = this.copy(op1 = nop1)
}

/* Idx expressions: Base DoubleArgumentExpression[ArrayExpression, ScalarExpression].  */
case class ArrayIdxExpression(op1: ArrayExpression, op2: ScalarExpression) extends ArrayExpression with ArraySubscription
  with DoubleArgumentExpression[ArrayExpression, ScalarExpression] {
  val expType = {
    op1.expType.base match {
      case array: ArrayType => array.updateConst(op1.expType.const)
      case _ => Asserts.ice(op1.expType.base, "unexpected scalar type")
    }
  }
  val base = op1
  def update(nop1: ArrayExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
}

case class ScalarIdxExpression(op1: ArrayExpression, op2: ScalarExpression) extends ScalarExpression with ArraySubscription
  with DoubleArgumentExpression[ArrayExpression, ScalarExpression] {
  def update(nop1: ArrayExpression, nop2: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2)
  val expType = {
    op1.expType.base match {
      case scalar: ScalarType => scalar.updateConst(op1.expType.const)
      case _ => Asserts.ice(op1.expType.base, "unexpected array type")
    }
  }
  val base = op1
}

/* Terms: Base: Term.  */
case class ScalarVariableDef(expType: ScalarType, name: String, init: Option[ScalarExpression], id: Int = Counter.nextVariable) extends Variable

case class ScalarVariable(base: ScalarVariableDef) extends ScalarExpression with VariableRef {
  val expType = base.expType
  val variable = base
}

class ScalarVariableRef(val variable: ScalarVariableDef, isCst: Boolean = false) extends ScalarExpression with VariableRef {
  val expType = if (!isCst) variable.expType else variable.expType.updateConst(true)
  override def toString() = "ScalarVariableRef(" + variable.toString + ")"
}

case class ArrayVariableDef(expType: ArrayType, name: String, restrict: Boolean, init: Option[ArrayConstant], id: Int = Counter.nextVariable) extends Variable

case class ArrayVariable(base: ArrayVariableDef) extends ArrayExpression with VariableRef {
  val expType = base.expType
  val variable = base
}

class ArrayVariableRef(val variable: ArrayVariableDef) extends ArrayExpression with VariableRef {
  val expType = variable.expType
  override def toString() = "ArrayVariableRef(" + variable.toString + ")"
}

case class IntegerConstant(expType: ScalarType, value: Int) extends ScalarExpression with Constant

case class FloatConstant(expType: ScalarType, value: Double) extends ScalarExpression with Constant

case class BooleanConstant(value: Option[Boolean]) extends ScalarExpression with Constant {
  val expType = BooleanType(true)
}

/* Rest.  */

case class ArrayConstant(expType: ArrayType, data: Iterable[Expression with Constant]) extends ArrayExpression with Constant

case class ScalarStructSubscription(base: ScalarExpression, field: Int) extends ScalarExpression with StructSubscription {
  val expType = ({
    base.expType match {
      case StructType(fields, _, _) => {
        Asserts.assert(field >= 0 && field < fields.size, (base, field), "invalid struct subscription")
        fields(field)._2 match {
          case t: ScalarType => Some(t.updateConst(base.expType.const))
          case _ => Asserts.ice((base, field), "invalid struct subscription (scalar field expected)")
        }
      }
      case _ => Asserts.ice((base, field), "invalid struct subscription")
    }
  }).get
  val op1 = base
  def update(nop1: ScalarExpression) = this.copy(base = nop1)
}

case class ArrayStructSubscription(base: ScalarExpression, field: Int) extends ArrayExpression with StructSubscription {
  val expType = ({
    base.expType match {
      case StructType(fields, _, _) => {
        Asserts.assert(field >= 0 && field < fields.size, (base, field), "invalid struct subscription")
        fields(field)._2 match {
          case t: ArrayType => Some(t.updateConst(base.expType.const))
          case _ => Asserts.ice((base, field), "invalid struct subscription (scalar field expected)")
        }
      }
      case _ => None
    }
  }).get
  val op1 = base
  def update(nop1: ScalarExpression) = this.copy(base = nop1)
}

case class CallExpression(func: Function, args: Seq[Expression]) extends ScalarExpression {
  val expType = func.retType.updateConst(true)
}

case class SizeofExpression(obj: Type) extends ScalarExpression {
  val expType = IntegerType(false, 32, true)
}

case class ConvertExpression(expType: ScalarType, op1: ScalarExpression) extends ScalarExpression with SingleArgumentExpression[ScalarExpression] {
  def update(nop1: ScalarExpression) = this.copy(op1 = nop1)
}

case class TernaryExpression(op1: ScalarExpression, op2: ScalarExpression, op3: ScalarExpression)
  extends ScalarExpression with TripleArgumentExpression[ScalarExpression, ScalarExpression, ScalarExpression] {
  val expType = {
    if (!op2.expType.compatible(op3.expType)) {
      Asserts.ice((op2, op3), "invalid ternary expression")
    }
    op2.expType.updateConst(true)
  }
  def update(nop1: ScalarExpression, nop2: ScalarExpression, nop3: ScalarExpression) = this.copy(op1 = nop1, op2 = nop2, op3 = nop3)
}

/** Pool of common integer constants. */
object Constants {
  val Integer32Constant0 = IntegerConstant(IntegerType(true, 32, true), 0)
  val Integer64Constant0 = IntegerConstant(IntegerType(true, 64, true), 0)
  val Float32Constant0 = FloatConstant(FloatType(32, true), 0.0)
  val Float64Constant0 = FloatConstant(FloatType(64, true), 0.0)
  val Integer32Constant1 = IntegerConstant(IntegerType(true, 32, true), 1)
  val Integer64Constant1 = IntegerConstant(IntegerType(true, 64, true), 1)
  val BooleanConstantTrue = BooleanConstant(Some(true))
  val BooleanConstantFalse = BooleanConstant(Some(false))
  val BooleanConstantMaybe = BooleanConstant(None)
}

/* Compile-time computation of constants. */
object ConstantComputer {

  /** Perform arithmetic operation on pair of integer constants.  */
  def compute(operation: (Int, Int) => Int, op1: IntegerConstant, op2: IntegerConstant): IntegerConstant = {
    new IntegerConstant(op1.expType, operation(op1.value, op2.value))
  }

  /** Perform arithmetic operation on pair of floating point constants.  */
  def compute(operation: (Double, Double) => Double, op1: FloatConstant, op2: FloatConstant): FloatConstant = {
    new FloatConstant(op1.expType, operation(op1.value, op2.value))
  }

  /** Perform comparison operation on pair of integer constants.  */
  def compute(operation: (Int, Int) => Boolean, op1: IntegerConstant, op2: IntegerConstant): BooleanConstant = {
    new BooleanConstant(Some(operation(op1.value, op2.value)))
  }

  /** Perform comparison operation on pair of floating point constants.  */
  def compute(operation: (Double, Double) => Boolean, op1: FloatConstant, op2: FloatConstant): BooleanConstant = {
    new BooleanConstant(Some(operation(op1.value, op2.value)))
  }

  /** Fold the scalar bit expression with constant integer operands.  */
  def compute(exp: ScalarBitBinaryExpression, op1: IntegerConstant, op2: IntegerConstant): IntegerConstant = {
    exp match {
      case _: BitLShiftExpression => compute((a, b) => a << b, op1, op2)
      case _: BitRShiftExpression => compute((a, b) => a >> b, op1, op2)
      case _: BitAndExpression => compute((a, b) => a & b, op1, op2)
      case _: BitOrExpression => compute((a, b) => a | b, op1, op2)
      case _: BitXorExpression => compute((a, b) => a ^ b, op1, op2)
    }
  }

  /** Fold the scalar arithmetic expression with constant integer operands.  */
  def compute(exp: ScalarMathBinaryExpression, op1: IntegerConstant, op2: IntegerConstant): IntegerConstant = {
    exp match {
      case _: PlusExpression => compute((a, b) => a + b, op1, op2)
      case _: MinusExpression => compute((a, b) => a - b, op1, op2)
      case _: DivExpression => compute((a, b) => a / b, op1, op2)
      case _: ModExpression => compute((a, b) => a % b, op1, op2)
      case _: MultExpression => compute((a, b) => a * b, op1, op2)
    }
  }

  /** Fold the scalar arithmetic expression with constant floating point operands.  */
  def compute(exp: ScalarMathBinaryExpression, op1: FloatConstant, op2: FloatConstant): FloatConstant = {
    exp match {
      case _: PlusExpression => compute((a, b) => a + b, op1, op2)
      case _: MinusExpression => compute((a, b) => a - b, op1, op2)
      case _: DivExpression => compute((a, b) => a / b, op1, op2)
      case _: ModExpression => compute((a, b) => a % b, op1, op2)
      case _: MultExpression => compute((a, b) => a * b, op1, op2)
    }
  }

  /** Fold the scalar comparison expression with constant integer operands.  */
  def compute(exp: ScalarComparisonBinaryExpression, op1: IntegerConstant, op2: IntegerConstant): BooleanConstant = {
    exp match {
      case _: EqualExpression => compute((a: Int, b: Int) => a == b, op1, op2)
      case _: NEqualExpression => compute((a: Int, b: Int) => a != b, op1, op2)
      case _: LessExpression => compute((a: Int, b: Int) => a < b, op1, op2)
      case _: GreaterExpression => compute((a: Int, b: Int) => a > b, op1, op2)
      case _: LessEqExpression => compute((a: Int, b: Int) => a <= b, op1, op2)
      case _: GreaterEqExpression => compute((a: Int, b: Int) => a >= b, op1, op2)
    }
  }

  /** Fold the scalar comparison expression with constant floating point operands.  */
  def compute(exp: ScalarComparisonBinaryExpression, op1: FloatConstant, op2: FloatConstant): BooleanConstant = {
    exp match {
      case _: EqualExpression => compute((a: Double, b: Double) => a == b, op1, op2)
      case _: NEqualExpression => compute((a: Double, b: Double) => a != b, op1, op2)
      case _: LessExpression => compute((a: Double, b: Double) => a < b, op1, op2)
      case _: GreaterExpression => compute((a: Double, b: Double) => a > b, op1, op2)
      case _: LessEqExpression => compute((a: Double, b: Double) => a <= b, op1, op2)
      case _: GreaterEqExpression => compute((a: Double, b: Double) => a >= b, op1, op2)
    }
  }

  def compute(exp: ScalarComparisonBinaryExpression, op1: BooleanConstant, op2: BooleanConstant): BooleanConstant = {
    exp match {
      case _: EqualExpression => BooleanConstant(Some(op1.value == op2.value))
      case _: NEqualExpression => BooleanConstant(Some(op1.value != op2.value))
    }
  }



  /** Fold the scalar boolean expression with constant boolean operands.  */
  def compute(exp: ScalarBooleanBinaryExpression, op1: BooleanConstant, op2: BooleanConstant): BooleanConstant = {
    (exp, op1, op2) match {
      case (_: AndExpression, BooleanConstant(Some(value1)), BooleanConstant(Some(value2))) => BooleanConstant(Some(value1 && value2))
      case (_: OrExpression, BooleanConstant(Some(value1)), BooleanConstant(Some(value2))) => BooleanConstant(Some(value1 && value2))
      case (_, BooleanConstant(None), _) => BooleanConstant(None)
      case (_, _, BooleanConstant(None)) => BooleanConstant(None)
      case _ => Asserts.ice((exp, op1), "can not compute binary boolean constant expression")
    }
  }

  /** Fold the scalar binary expression with constant operands.  */
  def compute(exp: ScalarBinaryExpression, op1: ScalarExpression with Constant, op2: ScalarExpression with Constant): ScalarExpression with Constant = {
    Checkable.assert(op1.expType.compatible(op2.expType), (op1, op2), "incompatible types for compute")
    (exp, op1, op2) match {
      case (exp: ScalarBitBinaryExpression, op1: IntegerConstant, op2: IntegerConstant) => compute(exp, op1, op2)
      case (exp: ScalarMathBinaryExpression, op1: IntegerConstant, op2: IntegerConstant) => compute(exp, op1, op2)
      case (exp: ScalarMathBinaryExpression, op1: FloatConstant, op2: FloatConstant) => compute(exp, op1, op2)
      case (exp: ScalarComparisonBinaryExpression, op1: IntegerConstant, op2: IntegerConstant) => compute(exp, op1, op2)
      case (exp: ScalarComparisonBinaryExpression, op1: FloatConstant, op2: FloatConstant) => compute(exp, op1, op2)
      case (exp: ScalarComparisonBinaryExpression, op1: BooleanConstant, op2: BooleanConstant) => compute(exp, op1, op2)
      case (exp: ScalarBooleanBinaryExpression, op1: BooleanConstant, op2: BooleanConstant) => compute(exp, op1, op2)
    }
  }

  /** Fold the scalar unary expression with constant operand.  */
  def compute(exp: ScalarUnaryExpression, op1: ScalarExpression with Constant): ScalarExpression with Constant = {
    (exp, op1) match {
      case (_: UnaryMinusExpression, IntegerConstant(itype, value)) => new IntegerConstant(itype, -value)
      case (_: UnaryMinusExpression, FloatConstant(itype, value)) => new FloatConstant(itype, -value)
      case (_: NotExpression, BooleanConstant(Some(value))) => new BooleanConstant(Some(!value))
      case _ => Asserts.ice((exp, op1), "can not compute unary constant expression")
    }
  }
}

/** Computes a set of scalar variables, referenced in a given expression.  */
object ExpressionAnalyzer {
  def getScalarVariables(in: Expression): Set[ScalarVariableRef] = {
    in match {
      case _: Constant => Set()
      case variable: ScalarVariableRef => Set(variable)
      case variable: ArrayVariableRef => Set()
      case exp: SingleArgumentExpression[_] => getScalarVariables(exp.op1)
      case exp: DoubleArgumentExpression[_, _] => getScalarVariables(exp.op1) ++ getScalarVariables(exp.op2)
      case exp: TripleArgumentExpression[_, _, _] => getScalarVariables(exp.op1) ++ getScalarVariables(exp.op2) ++ getScalarVariables(exp.op3)
      case call: CallExpression => call.args.map(getScalarVariables).fold(Set())((b, a) => b ++ a)
      case size: SizeofExpression => Set()
      case _ => Checkable.ice(in, "unexpected expression")
    }
  }
}

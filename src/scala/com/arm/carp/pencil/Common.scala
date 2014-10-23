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

import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

/**
  * Provides basic mechanism to handle internal compiler errors.
  *
  * The internal compiler error are reported as follows:
  *  - Message, supplied by the caller
  *  - Additional data, supplied be the caller
  */

trait Assertable {
  /**
    * Report internal compilation error and throw RuntimeException.
    *
    * @param data additional data to include in error message
    * @param message error message
    *
    */
  def ice(data: Any, message: String): Nothing = {
    System.err.print("Internal compiler error (" + message + ")" + " in " + this.getClass.getSimpleName +
      " Pass in object: \n" + data.toString + "\n")
    throw new RuntimeException
  }

  /**
    * Check the condition and calls [[Assertable.ice]] if condition is false.
    *
    * @param cond condition to check
    * @param data additional data to include in error message
    * @param message error message
    *
    */
  def assert(cond: Boolean, data: Any, message: String) = {
    if (cond == false)
      ice(data, message)
  }
}

/** Provides base implementation of the [[Assertable]] trait.  */
object Asserts extends Assertable

/** Provides base implementation of the compile-time warnings.  */
object Warnings {
  /**
    * Print warning message.
    *
    * @param message message to print
    */
  def warning(message: String): Unit = {
    System.err.println("Warning:" + message)
  }
}


/**
  * Provides compile time consistency checks for internal PENCIL
  * representation.
  *
  */
trait Checks extends Walker{

  val refs = HashSet[VariableRef]()

  val ops = HashSet[Operation]()

  var current_function: Option[Function] = None

  val calls = new CallGraph

  val loops = Stack[Operation]()

  def ice(data: Any, message: String): Unit
  def assert(cond: Boolean, data: Any, message: String): Unit

  val config = WalkerConfig.minimal.copy(expressions = true)

  /**
    * Check return statement.
    *
    * Type of the return statement operand must be compatible with the return
    * type of the function.
    */
  override def walkReturn(in: ReturnOperation) = {
    in.op match {
      case Some(exp) =>
        assert(exp.expType.compatible(current_function.get.retType), in, "invalid return expression")
      case None => assert(current_function.get.retType == NopType, in, "return expression expected")
    }
    super.walkReturn(in)
  }

  /**
    * Check break statement.
    *
    * Break statement must have at least one surrounding loop.
    */
  override def walkBreak(in: BreakOperation) = {
    assert(!loops.isEmpty, in, "missing loop for break")
    super.walkBreak(in)
  }

  /**
    * Check continue statement.
    *
    * Continue statement must have at least one surrounding loop.
    */
  override def walkContinue(in: ContinueOperation) = {
    assert(!loops.isEmpty, in, "missing loop for continue")
    super.walkContinue(in)
  }

  /**
    * Check assignment statement.
    *
    * The following properties must be satisfied:
    * - lvalue must not be constant
    * - type of the rvalue expression must be compatible with type of the type
    *   lvalue expression.
    */
  override def walkAssignment(in: AssignmentOperation) = {
    assert(in.lvalue.expType.compatible(in.rvalue.expType), in, "assignment to lvalue of different type(" + in.lvalue + " = " + in.rvalue + ")")
    assert(!in.lvalue.expType.const, in, "assignment to non-assignable expression")
    super.walkAssignment(in)
  }

  /**
    * Check the if/while guard expression.
    *
    * Guard expression must have boolean type.
    */
  override def walkGuardExpression(in: ScalarExpression) = {
    assert(in.expType.isBoolean, in, "if guard must have boolean type")
    super.walkGuardExpression(in)
  }

  /** Check while loop.  */
  override def walkWhile(in: WhileOperation) = {
    loops.push(in)
    val res = super.walkWhile(in)
    loops.pop
    res
  }

  /**
    * Check for loop iteration range.
    *
    * Iteration variable and lower and upper bound must have integer type.
    * Iteration variable must not be constant.
    */
  override def walkRange(in: Range) = {
    assert(in.iter.expType.isInt, in.iter,
      "range iter must have integer type")
    assert(!in.iter.expType.const, in.iter,
      "range iter cannot be constant")
    assert(in.low.expType.isInt, in.low,
      "range lower bound must have integer type")
    assert(in.upper.expType.isInt, in.upper,
      "range upper bound must have integer type")
    super.walkRange(in)
  }

  /**
    * Check for loop annotations.
    *
    * All labels in independent property must be unique.
    */
  def checkForProperties(in: Seq[ForProperties], op: ForOperation): Unit = {
    for (item <- in) {
      item match {
        case IvdepLoop =>
        case indep: IndependentLoop => {
          indep.labels match {
            case Some(labels) => {
              assert(labels.size == labels.toSet.size, item, "labels in pragma independent must be unique")
              //TODO check for label locations
            }
            case None =>
          }
        }
        case _ => ice(item, "unexpected for property")
      }
    }
  }

  /** Check for loop. */
  override def walkFor(in: ForOperation) = {
    checkForProperties(in.properties, in)
    loops.push(in)
    val res = super.walkFor(in)
    loops.pop
    res
  }

  /**
    * Check general scalar binary expression.
    *
    * For ModExpression (%) both operands must have integer type.
    * For Boolean expressions both operands must have boolean type.
    * For comparison expressions (==, !=) both operands must have numeric or
    * boolean types.
    * For comparison expressions (except == and !=)  both operands must have
    * numeric types.
    */
  override def walkScalarBinaryExpression(in: ScalarBinaryExpression) = {
    assert(in.op1.expType.compatible(in.op2.expType), in, "binary operation argument types must be the same")
    in match {
      case ModExpression(op1, op2) =>
        assert(op1.expType.isInt, in, "second operand of mod must have integer type")
        assert(op2.expType.isInt, in, "second operand of mod must have integer type")
      case exp: ScalarBooleanBinaryExpression =>
        assert(exp.op1.expType.isBoolean, exp, "boolean operation must have boolean operands")
        assert(exp.op2.expType.isBoolean, exp, "boolean operation must have boolean operands")
      case exp: ScalarBitBinaryExpression =>
        assert(exp.op1.expType.isInt, exp, "bit operation must have int operands")
        assert(exp.op2.expType.isInt, exp, "bit operation must have int operands")
      case _: EqualExpression | _:NEqualExpression =>
        assert(in.op1.expType.isNumeric || in.op1.expType.isBoolean, in,
               "comparison operation (==, !=) can be applied to numerical and boolean types only")
        assert(in.op2.expType.isNumeric || in.op2.expType.isBoolean, in,
               "comparison operation (==, !=) can be applied to numerical and boolean types only")
      case _: ScalarComparisonBinaryExpression | _: ScalarMathBinaryExpression =>
        assert(in.op1.expType.isNumeric, in, "comparison operation must have numeric operands")
        assert(in.op2.expType.isNumeric, in, "comparison operation must have numeric operands")
      case _ =>
    }
    super.walkScalarBinaryExpression(in)
  }

  /**
    * Check ternary expression.
    *
    * The guard expression must have boolean type.
    * Both alternatives must have compatible types.
    */
  override def walkScalarTernaryExpression(in: TernaryExpression) = {
    assert(in.op1.expType.isBoolean, in, "ternary operation guard type must be boolean")
    assert(in.op2.expType.compatible(in.op3.expType), in, "ternary operation alternative types must be the same")
    super.walkScalarTernaryExpression(in)
  }

  /**
    * Check array indexing expression.
    *
    * Index expression must have integer type.
    */
  override def walkArrayIdxExpression(in: ArrayIdxExpression) = {
    assert(in.op2.expType.isInt, in, "array idx must have integer type")
    super.walkArrayIdxExpression(in)
  }

  /**
    * Check array indexing expression.
    *
    * Index expression must have integer type.
    */
  override def walkScalarIdxExpression(in: ScalarIdxExpression) = {
    assert(in.op2.expType.isInt, in, "array idx must have integer type")
    super.walkScalarIdxExpression(in)
  }

  /**
    * Check array variable reference.
    *
    * Same variable reference cannot be used twice.
    * If variable has initializer, its type must be compatible with type of the variable.
    */
  override def walkArrayVariable(in: ArrayVariableRef) = {
    in.variable.init match {
      case Some(exp) => assert(exp.expType.compatible(in.expType), in, "initializer of different type")
      case None =>
    }
    assert(!refs.contains(in), in.variable, "same variable reference is used twice")
    refs+=in
    super.walkArrayVariable(in)
  }

  /**
    * Check scalar variable reference.
    *
    */
  override def walkScalarVariable(in: ScalarVariableRef) = {
    assert(!refs.contains(in), in.variable, "same variable reference is used twice")
    refs+=in
    super.walkScalarVariable(in)
  }

  /** Same operation object can not be used twice.  */
  override def walkOperation(in: Operation) = {
    assert(!in_scop || !in.scop, in, "nested SCoPs are forbidden")
    assert(!ops.contains(in), in, "same operation object is used twice")
    ops+=in
    super.walkOperation(in)
  }

  /**
    * Check array constant.
    *
    * The type of the elements must be compatible with the base type of the
    * constant type.
    */
  override def walkArrayConstant(in: ArrayConstant) = {
    val base = in.expType.base
    in.data.foreach { item =>
      assert(item.expType.compatible(base), item, "invalid array constant")
      walkExpression(item)
    }
    super.walkArrayConstant(in)
  }

  /**
    * Check convert expression.
    *
    * Operand of the convert expression must have type, convertible to the target type.
    */
  override def walkConvertExpression(in: ConvertExpression) = {
    assert(in.op1.expType.convertible(in.expType), in, "invalid scalar conversion")
    super.walkConvertExpression(in)
  }

  /**
    * Check call expression.
    *
    * Arguments supplied must be compatable with corresponding functions parameters.
    * No recursion is allowed.
    */
  override def walkCallExpression(in: CallExpression) = {
    val params = in.func.params
    val args = in.args
    (params, args).zipped.foreach { (param, arg) =>
      assert(arg.expType.convertible(param.expType), in, "invalid call expression (argument type)")
      assert(arg.expType.isScalar || !arg.expType.const || param.expType.const, in, "invalid call expression (argument type)")
    }
    calls.addCall(current_function.get, in.func)
    super.walkCallExpression(in)
  }

  /**
    * Check scalar unary expression.
    *
    * For unary minus expression the operand must have numeric type.
    * For boolen not expression the operand type must have boolean type.
    * For bit reverse (~) expression the operand must have integer type.
    */
  override def walkScalarUnaryExpression(in: ScalarUnaryExpression) = {
    in match {
      case UnaryMinusExpression(op1) =>
        assert(op1.expType.isNumeric, in, "unary minus can not be applied to non-numeric type")
      case NotExpression(op1) =>
        assert(op1.expType.isBoolean, in, "logical not must be applied to boolean type")
      case BitNegExpression(op1) =>
        assert(op1.expType.isInt, in, "bitwise not must be applied to int type")
      case _ => ice(in, "unknown unary expression")
    }
    super.walkScalarUnaryExpression(in)
  }

  /** Sizeof expression cannot be applied to void type.  */
  override def walkSizeofExpression(in: SizeofExpression) = {
    assert(in.obj != NopType, in, "sizeof can not be applied to void type")
    super.walkSizeofExpression(in)
  }

  override def walkFunction(in: Function) = {
    current_function = Some(in)
    super.walkFunction(in)
  }

  override def walkProgram(in: Program) = {
    calls.clear
    refs.clear
    ops.clear
    val res = super.walkProgram(in)
    calls.getRecursion match {
      case None => res
      case Some(function) => ice(function, "recursion detected")
    }
  }
}

/** Provides base implementation of the [[Checks]] trait.  */
object Checkable extends Checks with Assertable


/** Hosts some common operations over PENCIL expressions.  */
trait Common {

  def convertScalar(exp: ScalarExpression, _type: ScalarType) = {
    if (exp.expType.compatible(_type)) {
      exp
    } else {
      Checkable.assert(exp.expType.convertible(_type), (exp, _type), "can not convert expression to required type")
      ConvertExpression(_type, exp)
    }
  }

  def getScalarZero(in: ScalarType): ScalarExpression = {
    in match {
      case IntegerType(_, 32, _) => Constants.Integer32Constant0
      case IntegerType(_, 64, _) => Constants.Integer64Constant0
      case FloatType(32, _) => Constants.Float32Constant0
      case FloatType(64, _) => Constants.Float64Constant0
      case _ => Checkable.ice(in, "numeric type expected")
    }
  }

  def getScalarType(in: Type): ScalarType = {
    in match {
      case scalar: ScalarType => scalar
      case array: ArrayType => getScalarType(array.base)
    }
  }

  def getMaxType(t1: ScalarType, t2: ScalarType): ScalarType = {
    (t1, t2) match {
      case (s1: StructType, s2: StructType) =>
        Checkable.assert(s1.compatible(s2), (t1, t2), "Invalid types to uplift");
        s1
      case (_: BooleanType, _: BooleanType) => BooleanType(true)
      case (_: IntegerType, _: FloatType) => t2.updateConst(true)
      case (_: FloatType, _: IntegerType) => t1.updateConst(true)
      case (FloatType(b1, _), FloatType(b2, _)) => FloatType(math.max(b1, b2), true)
      case (IntegerType(true, b1, _), IntegerType(true, b2, _)) => IntegerType(true, math.max(b1, b2), true)
      case (IntegerType(false, b1, _), IntegerType(false, b2, _)) => IntegerType(false, math.max(b1, b2), true)
      case (IntegerType(true, b1, _), IntegerType(false, b2, _)) => if (b1 > b2) IntegerType(true, b1, true) else IntegerType(false, b2, true)
      case (IntegerType(false, b1, _), IntegerType(true, b2, _)) => if (b2 > b1) IntegerType(true, b2, true) else IntegerType(false, b1, true)
      case _ => Checkable.ice((t1, t2), "Invalid type to uplift")
    }
  }
  def compatibleWithFunction (f1: Function, retType: ScalarType, params: Seq[Variable]) = {
    f1.retType.compatible(retType) && f1.params.size == params.size &&
    (f1.params, params).zipped.forall((p1, p2) => p1.expType.compatible(p2.expType))
  }

  def compatibleFunctions (f1: Function, f2: Function) = {
    compatibleWithFunction(f1, f2.retType, f2.params)
  }

  /** Construct a list of terms from a summation/subtraction expression tree.
    */
  def listFromSumTree(in: ScalarExpression): List[ScalarExpression] = {
    in match {
      case x: PlusExpression =>  listFromSumTree(x.op1) ::: listFromSumTree(x.op2)
      case x: MinusExpression => listFromSumTree(x.op1) ::: listFromSumTree(UnaryMinusExpression(x.op2))
      case y => List(y)
    }
  }

  /** Create a MinusExpression if right is a UnaryMinusExpression; otherwise
    * create a PlusExpression.
    */
  private def createPlusOrMinus(left: ScalarExpression, right: ScalarExpression): ScalarExpression = {
    right match {
      case x: UnaryMinusExpression => new MinusExpression(left, x.op1)
      case _ => new PlusExpression(left, right)
    }
  }

  /** Construct a summation tree from a list of expressions.
    */
  def sumTreeFromList(terms: List[ScalarExpression]): ScalarExpression = {
    (terms.head /: terms.tail)(createPlusOrMinus(_, _))
  }

}

/** Hosts some common operations over PENCIL statements.  */
trait CommonOps {
    def make(init: Option[Operation]*): Option[Operation] = {
    val buff = new ListBuffer[Operation]
    for (item <- init) {
      item match {
        case Some(ops: BlockOperation) if !ops.scop => buff.appendAll(ops.ops)
        case Some(operation) => buff.append(operation)
        case None =>
      }
    }
    val initializer = buff.toList
    initializer.size match {
      case 0 => None
      case 1 => Some(initializer(0))
      case _ => Some(new BlockOperation(initializer))
    }
  }

  def makeBlock(init: Option[Operation]*): Option[BlockOperation] = {
    make(init:_*) match {
      case None => None
      case Some(block:BlockOperation) => Some(block)
      case Some(op:Operation) => Some(new BlockOperation(List(op)))
    }
  }
}

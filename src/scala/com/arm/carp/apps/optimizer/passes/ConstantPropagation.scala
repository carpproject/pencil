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
import scala.collection.mutable.HashMap
import com.arm.carp.pencil.ReachingDefinitions
import com.arm.carp.pencil.DefineSet

/** Perform constant propogation and constant folding. */
class ConstantPropagation extends Pass("cp", true) {

  type RenameMap = HashMap[AssignmentOperation, ScalarVariableDef]
  type CstMap = HashMap[AssignmentOperation, ScalarExpression with Constant]

  val renames = new RenameMap
  val constants = new CstMap

  private def getDefinedValue(in: AssignmentOperation) = {
    renames.get(in) match {
      case Some(variable) => Some(new ScalarVariableRef(variable))
      case None => constants.get(in)
    }
  }

  val config = WalkerConfig.expressions

  override def walkScalarBinaryExpression (exp: ScalarBinaryExpression) = {
    val op1 = walkScalarExpression (exp.op1)._1
    val op2 = walkScalarExpression (exp.op2)._1
    val res = (exp, op1, op2) match {
          case (_, cop1: ScalarExpression with Constant, cop2: ScalarExpression with Constant) => ConstantComputer.compute(exp, cop1, cop2)
          case (_: AndExpression, BooleanConstant(Some(false)), _) => Constants.BooleanConstantFalse
          case (_: AndExpression, _, BooleanConstant(Some(false))) => Constants.BooleanConstantFalse
          case (_: OrExpression, BooleanConstant(Some(true)), _) => Constants.BooleanConstantTrue
          case (_: OrExpression, _, BooleanConstant(Some(true))) => Constants.BooleanConstantTrue

          case (_: AndExpression, BooleanConstant(Some(true)), op2: ScalarExpression) => op2
          case (_: AndExpression, op1: ScalarExpression, BooleanConstant(Some(true))) => op1
          case (_: OrExpression, BooleanConstant(Some(false)), op2: ScalarExpression) => op2
          case (_: OrExpression, op1: ScalarExpression, BooleanConstant(Some(false))) => op1

          case (_: PlusExpression, IntegerConstant(_, 0), op2: ScalarExpression) => op2
          case (_: PlusExpression, op1: ScalarExpression, IntegerConstant(_, 0)) => op1

          case (_: PlusExpression, FloatConstant(_, 0.0), op2: ScalarExpression) => op2
          case (_: PlusExpression, op1: ScalarExpression, FloatConstant(_, 0.0)) => op1

          case (_: MinusExpression, IntegerConstant(_, 0), op2: ScalarExpression) => UnaryMinusExpression(op2)
          case (_: MinusExpression, op1: ScalarExpression, IntegerConstant(_, 0)) => op1

          case (_: MinusExpression, FloatConstant(_, 0.0), op2: ScalarExpression) => UnaryMinusExpression(op2)
          case (_: MinusExpression, op1: ScalarExpression, FloatConstant(_, 0.0)) => op1

          case (_: MultExpression, IntegerConstant(_, 0), op2: ScalarExpression) => op1
          case (_: MultExpression, op1: ScalarExpression, IntegerConstant(_, 0)) => op2

          case (_: MultExpression, FloatConstant(_, 0.0), op2: ScalarExpression) => op1
          case (_: MultExpression, op1: ScalarExpression, FloatConstant(_, 0.0)) => op2

          case (_: MultExpression, IntegerConstant(_, 1), op2: ScalarExpression) => op2
          case (_: MultExpression, op1: ScalarExpression, IntegerConstant(_, 1)) => op1

          case (_: MultExpression, FloatConstant(_, 1.0), op2: ScalarExpression) => op2
          case (_: MultExpression, op1: ScalarExpression, FloatConstant(_, 1.0)) => op1

          case (_: DivExpression, op1: ScalarExpression, IntegerConstant(_, 1)) => op1
          case (_: DivExpression, op1: ScalarExpression, FloatConstant(_, 1.0)) => op1

          case _ => exp.update(op1, op2)
        }
    (res, None)
  }

  override def walkConvertExpression (in: ConvertExpression) = {
    val res = super.walkConvertExpression(in)._1 match {
      case ConvertExpression(expType: IntegerType, IntegerConstant(_, value)) => IntegerConstant(expType, value)
      case ConvertExpression(expType: FloatType, IntegerConstant(_, value)) => FloatConstant(expType, value)
      case ConvertExpression(expType: FloatType, FloatConstant(_, value)) => FloatConstant(expType, value)
      case ConvertExpression(expType: IntegerType, FloatConstant(_, value)) => IntegerConstant(expType, value.toInt)
      case exp => exp
    }
    (res, None)
  }

  override def walkScalarUnaryExpression (exp: ScalarUnaryExpression) = {
    val op1 = walkScalarExpression(exp.op1)._1
    val res = (exp, op1) match {
      case (_, cop1: ScalarExpression with Constant) => ConstantComputer.compute(exp, cop1)
      case (_:UnaryMinusExpression, UnaryMinusExpression(exp)) => exp
      case _ => exp.update(op1)
    }
    (res, None)
  }

  override def walkScalarVariable(variable: ScalarVariableRef) = {
    val res = variable.info match {
      case Some(defs: DefineSet) if defs.data.size == 1 => {
        val cst = getDefinedValue(defs.data.head)
        if (cst.isEmpty) {
          variable
        } else {
          cst.get
        }
      }
      case _ => variable
    }
    (res, None)
  }

  override def walkScalarTernaryExpression (exp: TernaryExpression) = {
    val op1 = walkScalarExpression(exp.op1)._1
    val op2 = walkScalarExpression(exp.op2)._1
    val op3 = walkScalarExpression(exp.op3)._1
    val res = op1 match {
      case guard: BooleanConstant =>
      assert(guard.value.isDefined, guard, "unexpected __pencil_maybe")
      if (guard.value.get) op1 else op2
      case _ =>
      exp.copy(op1 = op1, op2 = op2, op3 = op3)
    }
    (res, None)
  }

  private def getTypeSize (in: Type, as: ScalarType): ScalarExpression = {
    in match {
      case _:BooleanType => IntegerConstant (as, 4)
      case FloatType(bits, _) => IntegerConstant (as, bits / 8)
      case IntegerType(_, bits, _) => IntegerConstant (as, bits / 8)
      case ArrayType (base, range) => MultExpression(getTypeSize(base, as), range)
      case StructType(fields, _, _) =>
      fields.foldLeft[ScalarExpression](Constants.Integer32Constant0)((b,a)=>
        PlusExpression(b, getTypeSize(a._2, as)))
    }
  }

  override def walkSizeofExpression(in: SizeofExpression) = {
    walkScalarExpression(getTypeSize(in.obj, in.expType))
  }

  override def walkIntrinsicCallExpression (exp: IntrinsicCallExpression) = {
    val nargs = exp.args.map(walkScalarExpression(_)._1)
    val res = (exp.name, nargs) match {
      case ("max", Seq(arg1: IntegerConstant, arg2: IntegerConstant)) => if (arg1.value > arg2.value) arg1 else arg2
      case ("fmax", Seq(arg1: FloatConstant, arg2: FloatConstant)) => if (arg1.value > arg2.value) arg1 else arg2

      case ("min", Seq(arg1: IntegerConstant, arg2: IntegerConstant)) => if (arg1.value < arg2.value) arg1 else arg2
      case ("fmin", Seq(arg1: FloatConstant, arg2: FloatConstant)) => if (arg1.value < arg2.value) arg1 else arg2

      case ("abs", Seq(arg: IntegerConstant)) => if (arg.value >= 0) arg else new IntegerConstant(arg.expType, -arg.value)
      case ("fabs", Seq(arg: FloatConstant)) => if (arg.value >= 0) arg else new FloatConstant(arg.expType, -arg.value)

      case _ => exp.copy(args = nargs)
    }
    (res, None)
  }

  override def walkAssignment(in: AssignmentOperation) = {
    in.rvalue = walkScalarExpression(in.rvalue)._1
    in.lvalue match {
      case _: ScalarVariableRef =>
        in.rvalue match {
          case cst:Constant  => constants += ((in, cst))
          case ref:ScalarVariableRef  => renames += ((in, ref.variable))
          case _ =>
        }
      case _ => in.lvalue = walkLValueExpression(in.lvalue)._1
    }
    Some(in)
  }

  override def walkFunction(f: Function) = {
    renames.clear
    super.walkFunction(f)
  }

  override def execute(in: Program) = {
    ReachingDefinitions.compute(in)
    walkProgram(in)
  }
}

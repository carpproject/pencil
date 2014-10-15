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
  * Provides def default expression walker.
  *
  * The default walker visits each node of the PENCIL expression without modifying them.
  *
  * Each function processes a particular type of expressions and should return a
  * scalar or array expression depending on the type of the input expression.
  * Functions are also allowed to generate additional statements, which would
  * be inserted in the PENCIL program right before the statement containing
  * the expression being walked.
  * For example, one can implement conversion to SSA-like form as follows:
  * for each sub-expression, return a new variable annotated with an assignment operation,
  * which assigns this sub-expression to the created variable.
  */
trait ExpressionWalker extends CommonOps {

  type Annotated[T] = (T, Option[Operation])
  type AnnotatedScalarExpression = Annotated[ScalarExpression]
  type AnnotatedArrayExpression = Annotated[ArrayExpression]

  def walkScalarUnaryExpression(in: ScalarUnaryExpression): AnnotatedScalarExpression = {
    val op1 = walkScalarExpression(in.op1)
    (in.update(op1._1), op1._2)
  }

  def walkScalarBinaryExpression(in: ScalarBinaryExpression): AnnotatedScalarExpression = {
    val op1 = walkScalarExpression(in.op1)
    val op2 = walkScalarExpression(in.op2)
    (in.update(op1._1, op2._1), make(op1._2, op2._2))
  }

  def walkScalarTernaryExpression(in: TernaryExpression): AnnotatedScalarExpression = {
    val op1 = walkScalarExpression(in.op1)
    val op2 = walkScalarExpression(in.op2)
    val op3 = walkScalarExpression(in.op3)
    (in.update(op1._1, op2._1, op3._1), make(op1._2, op2._2, op3._2))
  }

  def walkScalarConstant(in: Constant with ScalarExpression): AnnotatedScalarExpression = {
    (in, None)
  }

  def walkScalarVariable(in: ScalarVariableRef): AnnotatedScalarExpression = {
    (in, None)
  }

  def walkScalarIdxExpression(in: ScalarIdxExpression): AnnotatedScalarExpression = {
    val base = walkArrayExpression(in.base)
    val idx = walkScalarExpression(in.op2)
    (in.update(base._1, idx._1), make(base._2, idx._2))
  }

  def walkArrayIdxExpression(in: ArrayIdxExpression): AnnotatedArrayExpression = {
    val base = walkArrayExpression(in.base)
    val idx = walkScalarExpression(in.op2)
    (in.update(base._1, idx._1), make(base._2, idx._2))
  }

  def walkArrayVariable(in: ArrayVariableRef): AnnotatedArrayExpression = {
    (in, None)
  }

  def walkArrayStructSubscription(in: ArrayStructSubscription): AnnotatedArrayExpression = {
    val base = walkScalarExpression(in.base)
    (in.copy(base = base._1), base._2)
  }

  def walkArrayConstant(in: ArrayConstant): AnnotatedArrayExpression = {
    (in, None)
  }

  def walkArrayExpression(in: ArrayExpression): AnnotatedArrayExpression = {
    in match {
      case exp: ArrayVariableRef => walkArrayVariable(exp)
      case exp: ArrayVariable => walkArrayVariableInt(exp)
      case exp: ArrayIdxExpression => walkArrayIdxExpression(exp)
      case exp: ArrayStructSubscription => walkArrayStructSubscription(exp)
      case exp: ArrayConstant => walkArrayConstant(exp)
      case _ => Checkable.ice(in, "unexpected expression")
    }
  }

  def walkExpression(in: Expression) = {
    in match {
      case exp: ScalarExpression => walkScalarExpression(exp)
      case exp: ArrayExpression => walkArrayExpression(exp)
      case _ => Checkable.ice(in, "unexpected expression")
    }
  }

  def walkScalarStructSubscription(in: ScalarStructSubscription): AnnotatedScalarExpression = {
    val base = walkScalarExpression(in.base)
    (in.copy(base = base._1), base._2)
  }

  def walkCallExpression(in: CallExpression): AnnotatedScalarExpression = {
    val args = in.args.map(walkExpression)
    (in.copy(args = args.map(_._1)), make(args.map(_._2):_*))
  }

  def walkSizeofExpression(in: SizeofExpression): AnnotatedScalarExpression = {
    (in, None)
  }

  def walkConvertExpression(in: ConvertExpression): AnnotatedScalarExpression = {
    val op1 = walkScalarExpression(in.op1)
    (in.copy(op1 = op1._1), op1._2)
  }

  /** All raw variables must be eliminated before leaving the front-end.  */
  def walkScalarVariableInt(in: ScalarVariable): AnnotatedScalarExpression = {
    Checkable.ice(in, "scalar variable without references are forbidden by default")
  }

  /** All raw variables must be eliminated before leaving the front-end.  */
  def walkArrayVariableInt(in: ArrayVariable): AnnotatedArrayExpression = {
    Checkable.ice(in, "array variable without references are forbidden by default")
  }
  def walkIterationVariable(in: ScalarVariableRef): Annotated[ScalarVariableRef] = {
    walkScalarVariable(in) match {
      case (iter:ScalarVariableRef, tail) =>
        iter.variable.iter = true
        (iter, tail)
      case err => Checkable.ice(err, "invalid iteration variable update")
    }
  }

  def walkScalarExpression(in: ScalarExpression): AnnotatedScalarExpression = {
    in match {
      case exp:ScalarVariableRef => walkScalarVariable(exp)
      case exp:ScalarVariable => walkScalarVariableInt(exp)
      case exp:Constant => walkScalarConstant(exp)
      case exp:ScalarBinaryExpression => walkScalarBinaryExpression(exp)
      case exp:ScalarUnaryExpression => walkScalarUnaryExpression(exp)
      case exp:TernaryExpression => walkScalarTernaryExpression(exp)
      case exp:ScalarIdxExpression => walkScalarIdxExpression(exp)
      case exp:ScalarStructSubscription => walkScalarStructSubscription(exp)
      case exp:CallExpression => walkCallExpression(exp)
      case exp:SizeofExpression => walkSizeofExpression(exp)
      case exp:ConvertExpression => walkConvertExpression(exp)
      case _ => Checkable.ice(in, "unexpected expression")
    }
  }
}

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

/**
  * Array index expression extraction pass.
  *
  * Extracts array index expressions that are considered "complex" into
  * separate variables.  For example:
  *
  *   x = a[(i*i)/2];
  *
  * becomes
  *
  *   idx_3 = (i*i)/2;
  *   x = a[idx_3];
  *
  * The purpose of this transformation is to aid code generation using PPCG.
  * PPCG currently cannot handle the former case, whereas it can handle the
  * latter.
  */

object IndexExtraction extends Pass("index-extraction") {
  val config = WalkerConfig.expressions

  /** Return true if given expression is "complex".
    *
    * We are a bit pessimistic for simplicity.  For example a[2*2] or a[j*2]
    * wouldn't need extraction, but a[j*j] would; we extract all three cases.
    */
  def isComplex(in: ScalarExpression): Boolean = {
    in match {
      case _: ScalarVariableRef => false
      case _: IntegerConstant => false
      case x: PlusExpression => isComplex(x.op1) || isComplex(x.op2)
      case x: MinusExpression => isComplex(x.op1) || isComplex(x.op2)
      case _ => true
    }
  }

  /** If the expression is considered complex, then replace it with a variable
    * and an assignment to this variable.
    */
  override def walkScalarIdxExpression(in: ScalarIdxExpression) = {
    if (isComplex(in.op2)) {
      val newVar = ScalarVariableDef(in.op2.expType.updateConst(false), "idx", None)
      val assignOp = new AssignmentOperation(new ScalarVariableRef(newVar), in.op2)
      (new ScalarIdxExpression(in.op1, new ScalarVariableRef(newVar)), Some(assignOp))
    } else {
      (in, None)
    }
  }
}

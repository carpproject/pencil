/*
 * Copyright (c) 2014, ARM Limited
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

import com.arm.carp.pencil

object Example {

  /**
    * void vector_add (int N, double A[const static restrict N],
    *                         double B[const static restrict N],
    *                         double C[const static restrict N])
    * {
    * #pragma pencil independent
    *   for (int i = 0; i < N; ++i)
    *   {
    *      C[i] = A[i] + B[i];
    *   }
    * }
    */

  def generateVectorAddFunction() = {
    /**
      * Create function arguments. These variables will be marked as function
      * arguments when added to the argument list.
      */

    val int_type = pencil.IntegerType(true, 32, false)
    val N = pencil.ScalarVariableDef(int_type, "N", None)

    /* Array arguments type requires N as a range specifier.  */
    val double_type = pencil.FloatType(64, false)
    val array_type = pencil.ArrayType(double_type, new pencil.ScalarVariableRef(N))
    val A = pencil.ArrayVariableDef(array_type, "A", true, None)
    val B = pencil.ArrayVariableDef(array_type, "B", true, None)
    val C = pencil.ArrayVariableDef(array_type, "C", true, None)

    /* Create local iteration variable.  */
    val i = pencil.ScalarVariableDef(int_type, "i", None)

    /* New Ref must be created for each use of the variable.  */
    val A_i = pencil.ScalarIdxExpression(new pencil.ArrayVariableRef(A),
                                         new pencil.ScalarVariableRef(i))

    val B_i = pencil.ScalarIdxExpression(new pencil.ArrayVariableRef(B),
                                         new pencil.ScalarVariableRef(i))

    val C_i = pencil.ScalarIdxExpression(new pencil.ArrayVariableRef(C),
                                         new pencil.ScalarVariableRef(i))

    /* Ref are only required for variable, not for expressions.  */
    val plus = pencil.PlusExpression(A_i, B_i)
    val assignment = new pencil.AssignmentOperation(C_i, plus)

    /* Create For loop. */
    val low = pencil.Constants.Integer32Constant0
    val upper = pencil.MinusExpression(new pencil.ScalarVariableRef(N),
                                       pencil.Constants.Integer32Constant1)
    val step = pencil.Constants.Integer32Constant1
    val range = pencil.Range(new pencil.ScalarVariableRef(i), low, upper, step)
    val for_loop = new pencil.ForOperation(List(), range, new pencil.BlockOperation(List(assignment)))

    /* Add independent pragma to the loop. */
    for_loop.properties = List(new pencil.IndependentLoop(None))

    val function = new pencil.Function(/* name */ "vector_add",
                                       /* parameters */ List(N, A, B, C),
                                       /* body */ Some(new pencil.BlockOperation(List(for_loop))),
                                       /* return type */ pencil.NopType,
                                       /* access function */ None,
                                       /* const */ false,
                                       /* static */ false,
                                       /* is summary */false)

    List(function)
  }

  def generatePencil() = {
    val types = List()
    val constants = List()
    val functions = generateVectorAddFunction
    new pencil.Program(functions, types, constants)
  }

  def main(args: Array[String]) {

    val ir = generatePencil

    /* Check whether generated PENCIL code is valid.  */
    pencil.Checkable.walkProgram(ir)

    val writer = new pencil.Printer

    /* Convert PENCIL in-memory representation to string.  */
    val code = writer.toPencil(ir, /* print prototypes */ false,
                                   /* print functions */ true)
    println(code)
  }
}

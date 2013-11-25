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
import scala.collection.mutable.ListBuffer

/** Perform global constant propagation. */
class GCP extends Pass("gcp", true) {

  private val clones = new ListBuffer[Function]()

  /**
    * Create new version of the function, replacing constant arguments by
    * assignments inside function body.
    *
    * For example for call:
    * foo(1, i)
    * to the function
    * void foo (int a, int i){...}
    * the following function will be generated:
    * void foo_clone0 (int i)
    * {
    *   int a = 1;
    *   ....
    * }
    */
  private class Propogater(val args: Seq[Expression]) extends FunctionCloner {

    override def walkFunction(in: Function) = {
      val init = ListBuffer[Operation]()
      val params = ListBuffer[Variable]()
      (in.params, args).zipped.foreach((p, a) => {
        (p, a) match {
          case (p: ScalarVariableDef, cst: ScalarExpression with Constant) =>
            init += new AssignmentOperation(new ScalarVariableRef(p), convertScalar(cst, p.expType))
          case _ => params += p
        }
      })
      in.params = walkFunctionArguments(params)
      init += in.ops.get
      in.ops = walkFunctionBody(Some(new BlockOperation(init.toList)))
      Some(in)
    }
  }

  val config = WalkerConfig.expressions

  /** Try to create partially-specialized function for each call.  */
  override def walkCallExpression(in: CallExpression) = {
    val args = in.args.map(walkExpression(_)._1)
    val nargs = args.filter(!_.isInstanceOf[Constant]) //Non constant arguments
    if (args.size == nargs.size || in.func.ops.isEmpty) {
      (in.copy(args = args), None)
    } else {
      val actor = new Propogater(args)
      val copy = actor.cloneFunction(in.func, in.func.name + "_cloned")
      clones += copy
      (new CallExpression(copy, nargs), None)
    }
  }

  override def walkFunctions(in: Traversable[Function]) = {
    super.walkFunctions(in) ++ clones.toList
  }
}

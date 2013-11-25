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

import scala.collection.mutable.HashMap

/**
  * Provides function cloning functionality.
  * It generates a new identical copy of the given function ensuring
  * that no objects are reused between functions.
  */
trait FunctionCloner extends Walker {

  val config = WalkerConfig.minimal.copy(expressions = true, args = true)

  private val scalars = new HashMap[ScalarVariableDef, ScalarVariableDef]
  private val arrays = new HashMap[ArrayVariableDef, ArrayVariableDef]

  def getScalarRenames() = scalars
  def getArrayRenames() = arrays

  override def walkOperation(in: Operation) = {
    super.walkOperation(in.copy)
  }

  private def updateScalarVariable(in: ScalarVariableDef) = {
    scalars.get(in) match {
      case Some(variable) => variable
      case None =>
        val res = new ScalarVariableDef(in.expType.updateConst(false), in.name, in.init)
          scalars.put(in, res)
          res
    }
  }

  private def updateArrayVariable(in: ArrayVariableDef) = {
    arrays.get(in) match {
      case Some(variable) => variable
      case None =>
        val res = new ArrayVariableDef(in.expType, in.name, in.restrict, in.init)
          arrays.put(in, res)
          res
    }
  }

  override def walkFunctionArgument(in: Variable) = {
    in match {
      case arg:ArrayVariableDef => updateArrayVariable(arg)
      case arg:ScalarVariableDef => updateScalarVariable(arg)
      case _ => Checkable.ice(in, "unexpected argument")
    }
  }

  override def walkBlock(in: BlockOperation) = {
    super.walkBlock(in.copy)
  }

  override def walkScalarVariable(in: ScalarVariableRef): (ScalarExpression, Option[Operation]) = (new ScalarVariableRef(updateScalarVariable(in.variable)), None)

  override def walkArrayVariable(in: ArrayVariableRef): (ArrayExpression, Option[Operation]) = (new ArrayVariableRef(updateArrayVariable(in.variable)), None)

  def reset() = {
    scalars.clear
    arrays.clear
  }

  /**
    * Create a copy of the given function with the given name.
    *
    * @param in function to clone
    * @param name of the new function
    */
  def cloneFunction(in: Function, name: String) = {
    reset
    walkFunction(new Function(name, in.params, in.ops, in.retType, in.access, in.const, true, in.isSummary)).get
  }
}

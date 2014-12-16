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

import com.arm.carp.pencil.WalkerConfig
import com.arm.carp.pencil._
import scala.collection.mutable.HashMap
import scala.language.postfixOps

/**
  * Function inline pass.
  *
  * Single-return PENCIL function (with return statement as a last stement)
  * can be inlined.
  */

object Inline extends Pass("inline") {
  val config = WalkerConfig.expressions

  type ScalarVarMap = Map[ScalarVariableDef, ScalarVariableDef]
  type ArrayVarMap = Map[ArrayVariableDef, ArrayExpression]

  /** Create a copy of the function suitable for inlining. */
  private class FunctionUpdater(val result: Option[ScalarVariableDef],
    val scalars: ScalarVarMap, val arrays: ArrayVarMap,
    val eliminate_scops: Boolean) extends FunctionCloner {

    var returnCount = 0
    var arrayStack = 0

    override def walkScalarVariable(in: ScalarVariableRef) = {
      scalars.get(in.variable) match {
        case Some(variable) => (new ScalarVariableRef(variable), None)
        case None =>
        if (arrayStack == 0) {
            super.walkScalarVariable(in)
          } else {
            (new ScalarVariableRef(in.variable), None)
          }
      }
    }

    /** Since array parameters cannot be assigned, they must be replaced inside
        function code. */
    override def walkArrayVariable(in: ArrayVariableRef) = {
      arrays.get(in.variable) match {
        case Some(exp) =>
          arrayStack += 1
          val res = walkArrayExpression(exp)
          arrayStack -= 1
          res
        case None =>
          if (arrayStack == 0) {
            super.walkArrayVariable(in)
          } else {
            (new ArrayVariableRef(in.variable), None)
          }
      }
    }

    /**
      * Remove return statement.
      *
      * Return value (if any) is assigned to result variable.
      */
    override def walkReturn(in: ReturnOperation) = {
      returnCount = returnCount + 1
      (in.op, result) match {
        case (None, None) => None
        case (Some(op: ScalarExpression), Some(result: ScalarVariableDef)) =>
          Some(new AssignmentOperation(new ScalarVariableRef(result), convertScalar(walkScalarExpression(op)._1, result.expType)))
        case _ => Checkable.ice(in, "invalid return expression")
      }
    }

    override def walkOperation(in: Operation) = {
      if (eliminate_scops) {
        in.scop = false
      }
      if (returnCount < 2) {
        super.walkOperation(in)
      } else {
        /* If returnCount >= 2 we can not inline, so there is no need to continue. */
        None
      }
    }

    def update(in: BlockOperation) = {
      reset
      super.walkBlock(in)
    }
  }

  /** Currently pass tries to inline all defined functions. */
  private def shouldBeInlined(in: CallExpression) = {
    val defined = in.func.ops.isDefined
    defined
  }

  private def genInitialAssignment(param: Variable, arg: Expression, mapping: ScalarVarMap) = {
    (param, arg) match {
      case (param: ScalarVariableDef, arg: ScalarExpression) => {
        val variable = mapping.get(param).get
        Some(new AssignmentOperation(new ScalarVariableRef(variable), convertScalar(arg, variable.expType)))
      }
      case (_: ArrayVariableDef, _) => None
      case _ => ice((param, arg), "invalid argument")
    }
  }

  /** Generate initial code to assign scalar arguments to parameters. */
  private def getInitialCode(in: CallExpression, mapping: ScalarVarMap) = {
    Some(new BlockOperation((in.func.params, in.args).zipped.map((p, a) => genInitialAssignment(p, a, mapping)).flatten))
  }

  /** Map non-scalar function parameters to actual arguments.  */
  private def getArrayMapping(in: CallExpression) = {
    val keys = in.func.params.filter(p => !p.expType.isScalar).map(p => p.asInstanceOf[ArrayVariableDef])
    val vals = in.args.filter(a => !a.expType.isScalar).map(a => a.asInstanceOf[ArrayExpression])
    Checkable.assert(keys.size == vals.size, in, "invalid argument list")
    keys zip vals toMap
  }

  /** Generate a set of scalar variables to hold scalar function arguments.  */
  private def getScalarMapping(in: CallExpression) = {
    val keys = in.func.params.filter(_.expType.isScalar).map(_.asInstanceOf[ScalarVariableDef])
    keys.map(key => (key, new ScalarVariableDef(key.expType.updateConst(false), key.name, None))).toMap
  }

  /** Check whether the last operation in block is a possibly nested
      return statement. */
  private def lastIsReturn(in: BlockOperation):Boolean = {
    in.ops.last match {
      case _:ReturnOperation => true
      case block: BlockOperation => lastIsReturn(block)
      case _ => false
    }
  }

  /** Inline given call expression and place the returned value into
      scalar value (if provided).  */
  private def inline(in: CallExpression, result: Option[ScalarVariableDef]): Option[BlockOperation] = {
    Checkable.assert(in.func.ops.isDefined, in, "can not inline external function")
    Checkable.assert(in.args.size == in.func.params.size, in, "invalid number of arguments for function call")
    val arrayMapping = getArrayMapping(in)
    val scalarMapping = getScalarMapping(in)
    val updater = new FunctionUpdater(result, scalarMapping, arrayMapping, in_scop)
    val scalarArgsInit = getInitialCode(in, scalarMapping)
    val ops = updater.update(in.func.ops.get)
    if (updater.returnCount > 1 || updater.returnCount == 1 && !lastIsReturn(in.func.ops.get)) {
      None
    } else {
      set_changed
      makeBlock(scalarArgsInit, ops) match {
        case Some(block) => Some(block)
        case None => Some(new BlockOperation(List()))
      }
    }
  }

  /**
    * Inline function call, which is part of some expression.
    *
    * The returned value is placed into temporary variable, which replaces the
    * original call expression.
    */
  override def walkCallExpression(in: CallExpression) = {
    if (shouldBeInlined(in)) {
      Checkable.assert(in.func.retType != NopType, in.func, "Non void function expected")
      val result = ScalarVariableDef(in.func.retType.updateConst(false), "inline_ret", None)
      inline(in, Some(result)) match {
        case None => (in, None)
        case Some(init) => (new ScalarVariableRef(result), Some(init))
      }
    } else {
      super.walkCallExpression(in)
    }
  }

  /** Call operation doesn't require any additional assignments to be generated,
    since the return value (if any) is discarded.  */
  override def walkCallOp(in: CallOperation) = {
    if (shouldBeInlined(in.op)) {
      val result = in.op.func.retType match {
        case NopType => None
        case _ => Some(ScalarVariableDef(in.op.func.retType.updateConst(false), "inline_ret", None))
      }
      inline(in.op, result) match {
        case None => Some(in)
        case Some(init) => Some(init)
      }
    } else {
      super.walkCallOp(in)
    }
  }
}

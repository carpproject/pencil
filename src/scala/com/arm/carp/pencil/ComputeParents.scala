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
  * Updates the parent information for PENCIL statements.
  *
  * For a given PENCIL statement the parent statement is the innermost
  * block statement, which contains this statement.
  * The object can be used as follows: ParentComputer.walkProgram(program)
  */
object ParentComputer extends Walker {

  val config = WalkerConfig.minimal

  override def walkBlock(body: BlockOperation): Option[BlockOperation] = {
    body.ops.foreach(_.parent = Some(body))
    super.walkBlock(body)
  }

  override def walkFor(in: ForOperation) = {
    in.ops.parent = in.parent
    super.walkFor(in)
  }

  override def walkWhile(in: WhileOperation) = {
    in.ops.parent = in.parent
    super.walkWhile(in)
  }

  override def walkIf(in: IfOperation) = {
    in.ops.parent = in.parent
    in.eops match {
      case Some(block) => block.parent = in.parent
      case None =>
    }
    super.walkIf(in)
  }
}

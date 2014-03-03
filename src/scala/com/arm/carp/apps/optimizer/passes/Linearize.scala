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

/**
  * Remove unnecessary nested blocks in PENCIL code.
  *
  * Optimization passes might introduce redundant nested
  * [[BlockOperation]]:
  * {
  *   {
  *     A;
  *     B;
  *   }
  *   {
  *     C;
  *     D;
  *   }
  * }
  * This pass would remove such block:
  * {
  *   A;
  *   B;
  *   C;
  *   D;
  * }
  */

object Linearize extends Pass("linearize") {

  val config = WalkerConfig.minimal

  override def walkBlock(body: BlockOperation): Option[BlockOperation] = {
    val buff = ListBuffer[Operation]()
    for (op <- body.ops) {
      val processed = walkOperation(op)
      processed match {
        case Some(ops: BlockOperation) if !ops.scop =>
          buff.appendAll(ops.ops)
        case Some(op: Operation) => buff.append(op)
        case None =>
      }
    }
    body.ops = buff.toList
    Some(body)
  }
}

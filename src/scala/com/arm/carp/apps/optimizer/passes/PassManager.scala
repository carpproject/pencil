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

import com.arm.carp.pencil.Program
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

/**
  * Maintains an ordered sequence of passes.
  *
  * Each individual pass can be enabled/disabled.
  */
class PassManager {

  private var passes: ListBuffer[(Pass, Boolean)] = new ListBuffer

  private val index = Map[String, List[Int]]()

  private def enableInt(idx: Int, enabled: Boolean) = {
    val orig = passes(idx)
    passes(idx) = (orig._1, enabled)
  }

  /**
    * Enable/disable all passes.
    * @param enabled <code>true</code> - enable all passes,
    *                <code>false</code> - disable all passes
    */
  def enable(enabled: Boolean) = {
    for (i <- 0 to (passes.length - 1)) {
      enableInt(i, enabled)
    }
  }

  /**
    * Enable/disable pass with the given name.
    * @param name name of the pass
    * @param enabled <code>true</code> - enable the pass,
    *                <code>false</code> - disable the pass
    * @param idx index of the individual all (-1 for all
    *        instances  of the pass)
    */
  def enable(name: String, enabled: Boolean, idx: Int) = {
    index.get(name) match {
      case None => false
      case Some(list) =>
        if (idx >= list.length) {
          false
        } else {
          if (idx == -1) {
            list.foreach(enableInt(_, enabled))
          } else {
            enableInt(list(list.length - idx - 1), enabled)
          }
          true
        }
    }
  }

  private def getEnabledPasses () = passes.filter(_._2).map(_._1)

  /** Print all available passes.  */
  def dump() {
    println("Optimization passes {")
    passes.foreach(pass => println((if (pass._2) " Enabled: " else "Disabled: ") + pass._1.flag))
    println("}")
  }

  private def updateIndex(idx: Int, flag: String) = {
    index.get(flag) match {
      case None => index.update(flag, List(idx))
      case Some(list) => index.update(flag, idx :: list)
    }
  }

  /** Add new pass to the end of the sequence.  */
  def addPass(pass: Pass) {
    passes.append((pass, pass.enabled))
    updateIndex(passes.length - 1, pass.flag)
  }

  /** Run all enabled pass on the supplied program.  */
  def run(program: Program): Program = {
    getEnabledPasses.foldLeft(program)((prog, pass) => pass.run(prog))
  }
}
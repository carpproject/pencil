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
import scala.collection.mutable.HashSet
import com.arm.carp.pencil.Checkable

/**
  * Maintains an ordered sequence of passes.
  *
  * Each individual pass can be enabled/disabled.
  */
class PassManager {

  class PassRef (val pass: Pass, var enabled: Boolean, val flag: String, val args: PassArgumentStorage)

  private val passes: ListBuffer[PassRef] = new ListBuffer

  private val global_index = HashSet[String]()

  private val index = Map[String, Int]()

  private def enableInt(idx: Int, enabled: Boolean) = {
    val orig = passes(idx)
    passes(idx).enabled = enabled
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
    */
  def enable(name: String, enabled: Boolean) = {
    index.get(name) match {
      case Some(idx) =>
        enableInt(idx, enabled)
        true
      case None if !global_index.contains(name) => false
      case None => {
        passes foreach(p => if (p.pass.name == name) p.enabled = enabled)
        true
      }
    }
  }

  private def getClassName(in: Any) = {
    in match {
      case _:Boolean => "Boolean[yes/no]"
      case _:String => "String"
      case _:Integer => "Integer"
      case _ => "Unknown"
    }
  }

  private def getValue(in: Any) = {
    in match {
      case b:Boolean => if(b) "yes" else "no"
      case s:String => s
      case i:Integer => i.toString
      case _ => "Unknown"
    }

  }

  /** Print all available passes.  */
  def dump() {
    println("Optimization passes {")
    passes.foreach(pass => {
        println((if (pass.enabled) " Enabled: " else "Disabled: ") + pass.flag + "(" + pass.pass.name + ")")
        pass.args.list.foreach(arg => println("   argument:" + arg._1 + " type:" + getClassName(arg._2) + " value:" + getValue(arg._2)))
      })
    println("}")
  }

  /** Add new pass to the end of the sequence.  */
  def addPass(pass: Pass, flag: String, default: Boolean) {
    passes.append(new PassRef(pass, default, flag, pass.getArgsTemplate))
    global_index += pass.name
    index.get(flag) match {
      case Some(_) => Checkable.ice(flag, "Redefinition of the flag")
      case None => index.put(flag, passes.length - 1)
    }
  }

  private def setArgumentForPass (args: PassArgumentStorage, arg: String, value: String) = {
    args.set(arg, value)
  }

  def registerPassArgument(pass: String, arg: String, value: String): Boolean = {
    index.get(pass) match {
      case Some(idx) => setArgumentForPass(passes(idx).args, arg, value)
      case None if !global_index.contains(pass) => false
      case None => passes forall(p =>
        if (p.pass.name == pass)
          setArgumentForPass(p.args, arg, value)
        else
          true
      )
    }
  }

  /** Run all enabled pass on the supplied program.  */
  def run(program: Program): Program = {
    passes.filter(_.enabled).foldLeft(program)((prog, ref) => ref.pass.run(prog, ref.args))
  }
}

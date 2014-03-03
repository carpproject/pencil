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

import com.arm.carp.pencil._;
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

class PassArgumentStorage {
  private val data = HashMap[String, Any]()

  def get[T](name: String): T = {
    data.get(name) match {
      case Some(data) => data.asInstanceOf[T]
      case data => Checkable.ice(data, "invalid pass argument")
    }
  }

  def set(name: String, value: String): Boolean = {
    data.get(name) match {
      case Some(arg) => {
        arg match {
          case _:String =>
            data(name) = value
            true
          case _:Boolean if value == "yes" =>
            data(name) = true
            true
          case _:Boolean if value == "no" =>
            data(name) = false
            true
          case _:Int if value.matches("\\d+") =>
            data(name) = Integer.parseInt(value)
            true
          case _ => false
        }
      }
      case _ => false
    }
  }

  def add[T](name: String, default: T) = {
    data.get(name) match {
      case None => data(name) = default
      case data => Checkable.ice((data, name, default), "pass option redefinition")
    }
  }

  def copy = {
    val res = new PassArgumentStorage
    res.data ++= data
    res
  }

  def list = data.toList
}


/**
  * Base class for all PENCIL transformation passes.
  */
abstract class Pass(val name: String) extends Common with Assertable with Walker {

  private var args: Option[PassArgumentStorage] = None

  private val default_args = new PassArgumentStorage

  def registerOption[T](oname: String, default: T): () => T = {
    default_args.add(oname, default)
    () => {
      args match {
        case Some(args) => args.get[T](oname)
        case None =>
          ice(name, "Cannot find a pass argument set. Are you using val to" +
              "store the result of registerOption call? The result must be" +
              "stored to def (i.e. only evaluated when the pass is executed," +
              "not when the pass is created).")
      }
    }
  }

  def getArgsTemplate = default_args.copy

  var changed: Boolean = true

  def rerun = changed
  def set_changed () = {changed = true}
  def reset_changed () = {changed = false}

  private final def execute(in: Program): Program = {
    reset_changed
    val res = walkProgram(in)
    if (rerun) {
      execute(res)
    } else {
      res
    }
  }

  final def run(in: Program, pargs: PassArgumentStorage): Program = {
    args = Some(pargs)
    Checkable.walkProgram(execute(in))
  }
}

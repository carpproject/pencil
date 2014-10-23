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

import java.io.File;
import java.io.PrintWriter;
import com.arm.carp.pencil._

/**
 * This class puts each non-static function definition into a new file.
 * Based on the call graph, each new file will contain the definitions of all
 * directly or indirectly called functions that were defined in the input
 * program.
 */
object SplitFile extends Pass("splitfile") {

  val config = WalkerConfig.expressions

  private val outfile = registerOption("fname", "splitfile-out")

  /* Contains current function when building call graph */
  private var current_function: Option[Function] = None

  private val callgraph = new CallGraph

  private var progConsts: Seq[Variable] = Seq()
  private var progTypes: Seq[StructType] = Seq()

  private def dirname = (outfile() match {
    case "-" => ""
    case filename =>
      val dirsep = filename.lastIndexOf("/")
      if (dirsep > 0) {
        filename.substring(0, dirsep) + "/"
      }
      else {
        ""
      }
  })

  /**
   * Write given function to its own file, including all functions it calls.
   * @param function  Function to write.
   */
  private def printToFile(function: Function): Unit = {
    val filename = dirname + function.getName() + ".c"
    System.err.println("Writing " + filename)

    val functions = callgraph.getAllCallees(function)
    val program = new Program(functions + function, progTypes, progConsts)

    val printer = new Printer
    val writer = new PrintWriter(new File(filename))
    writer.append(printer.toPencil(program, true, true, false))
    writer.close
  }

  /**
   * Print given function to a new file if it is defined and is not a local
   * function.
   */
  private def processFunction(function: Function): Unit = {
    function.ops match {
      case Some(x) => if (!function.local) printToFile(function)
      case None =>
    }
  }

  /** Build the call graph */
  override def walkCallExpression(call: CallExpression) = {
    callgraph.addCall(current_function.get, call.func)
    super.walkCallExpression(call)
  }

  /** Build the call graph */
  override def walkFunction(function: Function) = {
    current_function = Some(function)
    super.walkFunction(function)
  }

  /**
   * Execute SplitFile pass on a program.
   * @param program  Program to act on.
   * @return         Original program.
   */
  override def walkProgram(program: Program) = {
    progConsts = program.consts
    progTypes = program.types
    callgraph.clear
    val res = super.walkProgram(program)
    program.functions.foreach(processFunction(_))
    res
  }
}

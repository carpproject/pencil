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
 * This class puts each function definition into a new file.  The main
 * purpose of this is to overcome the current behaviour of PPCG, which can
 * handle only one SCoP per invocation.
 */
class SplitFile(outfile: Option[String]) extends Pass("splitfile", false) {

  val config = WalkerConfig.minimal

  var dirname = (outfile match {
    case Some(filename) =>
      val dirsep = filename.lastIndexOf("/")

      if (dirsep > 0) {
        filename.substring(0, dirsep) + "/"
      }
      else {
        ""
      }
    case None => ""
  })
  var progConsts: Seq[Variable] = Seq()

  /**
   * Write given function to its own file.
   * @param function  Function to write.
   */
  def printToFile(function: Function): Unit = {
    val filename = dirname + function.getName() + ".c"
    System.err.println("Writing " + filename)

    val printer = new Printer
    val writer = new PrintWriter(new File(filename))
    writer.append(printer.processFunctionDefinition(function, progConsts))
    writer.close
  }

  /**
   * Execute SplitFile pass on a program.
   * @param program  Program to act on.
   * @return         Original program.
   */
  override def execute(program: Program) = {
    progConsts = program.consts
    program.functions.foreach(printToFile(_))
    program
  }
}

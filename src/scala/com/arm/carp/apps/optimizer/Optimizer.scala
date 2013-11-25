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

package com.arm.carp.apps.optimizer

import com.arm.carp.pencil.Printer
import java.io.PrintWriter
import java.io.File
import com.arm.carp.frontends.pencil.PencilFrontEnd
import com.arm.carp.apps.optimizer.passes._

/**
  * Optimizer application.
  *
  * Optimizer executes a set of transformation passes on a supplied PENCIL
  * program and output transformed PENCIL program (i.e. works as source-to-source
  * code transformator).
  */
object Main {

  private var inputFileName: Option[String] = None
  private var outputFileName: Option[String] = None

  private val passes = new PassManager

  private def handlePassFlag(raw: String) {
    val last = (raw takeRight 1)(0)

    /** Try to extract pass index.  */
    val (idx, flag) = if (Character.isDigit(last))
      ((last.toInt - '0'.toInt),raw.dropRight(1) )  else (-1, raw)

    val ok = if (flag.startsWith("-fno-")) {
      passes.enable(flag.stripPrefix("-fno-"), false, idx)
    } else {
      passes.enable(flag.stripPrefix("-f"), true, idx)
    }
    if (!ok) {
      complain("Unknown flag " + flag)
    }
  }

  private def initPasses() {
    val linearize = new Linearize
    val cp = new ConstantPropagation
    val dce = new DeadCodeElimination
    passes.addPass(linearize)
    passes.addPass(new FlattenStructParams)
    passes.addPass(new FlattenLocalStructs)
    passes.addPass(cp)
    passes.addPass(dce)
    passes.addPass(new GCP)
    passes.addPass(new Inline)
    passes.addPass(cp)
    passes.addPass(dce)
    passes.addPass(linearize)
    passes.addPass(new LICM)
    passes.addPass(new SplitFile(outputFileName))     // Should be the last one
  }

  private def parseCommandLine(args: List[String]): Boolean = {
    args match {
      case Nil => inputFileName.isDefined
      case "-h" :: rest =>
        sayHelp(); false
      case "-o" :: x :: rest =>
        outputFileName = Some(x)
        parseCommandLine(rest)
      case "-fall" :: rest =>
        passes.enable(true)
        parseCommandLine(rest)
      case "-fno-all" :: rest =>
        passes.enable(false)
        parseCommandLine(rest)
      case x :: rest if x.startsWith("-f") =>
        handlePassFlag(x)
        parseCommandLine(rest)
      case "-dump-passes" :: rest =>
        passes.dump
        parseCommandLine(rest)
      case x :: rest =>
        inputFileName = Some(x)
        parseCommandLine(rest)
    }
  }

  private def sayHelp() {
    System.err.println("Usage: input-file [-o output-file] [-f[no-]all] [-dump-passes] [passes options]")
    System.err.println("       passes can enabled/disabled with -f[-no]<PASS NAME>[idx] option")
    System.err.println("       use idx after pass name to disable specific occurence of pass")
    System.err.println("       Example: -fno-cp0 would disable the first occurance of the CP pass")
    System.err.println("       use -dump-passes to get the list of all passes")
    System.exit(0)
  }

  private def complain(message: String) {
    System.err.println("Error:" + message)
    System.exit(-1)
  }

  def main(args: Array[String]) {
    initPasses
    if (parseCommandLine(args.toList) != true) {
      complain("Invalid command line arguments (use -h for help).")
    }

    val frontend = new PencilFrontEnd
    val pencil = frontend.parse(inputFileName.get)

    if (pencil.isEmpty) {
      System.exit(-1)
    }

    val writer = new Printer

    val optimized = passes.run(pencil.get)
    val code = writer.toPencil(optimized)

    outputFileName match {
      case Some(fname) =>
        val file = new PrintWriter(new File(fname))
        file.append(code)
        file.close
      case None =>
        println(code)
    }
  }
}

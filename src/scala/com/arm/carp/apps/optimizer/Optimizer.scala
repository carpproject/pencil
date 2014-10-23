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

import com.arm.carp.apps.Version
import com.arm.carp.pencil.Printer
import java.io.PrintWriter
import java.io.File
import com.arm.carp.frontends.pencil.PencilFrontEnd
import com.arm.carp.apps.optimizer.passes._
import scala.collection.mutable.ListBuffer

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

  private val headers = ListBuffer[String]()

  private val passes = new PassManager

  private var debug = false

  private def handlePassFlag(flag: String) {
    val ok = flag match {
      case fno if fno.startsWith("-fno-") => passes.enable(flag.stripPrefix("-fno-"), false)
      case f if f.startsWith("-f") => passes.enable(flag.stripPrefix("-f"), true)
    }
    if (!ok) {
      complain("Unknown flag " + flag)
    }
  }

  private def handlePassArgument(flag: String, value: String) {
    val tmp = flag.split(":")
    if (tmp.size != 2) {
      complain("Unknown flag" + flag)
    }
    val (pass, option) = (tmp(0), tmp(1))
    passes.registerPassArgument(pass, option, value)
  }

  private def initPasses() {
    passes.addPass(Linearize, "linearize0", true)
    passes.addPass(FlattenStructs, "flatten-structs0", false)
    passes.addPass(ConstantPropagation, "cp0", true)
    passes.addPass(DeadCodeElimination, "dce0", true)
    passes.addPass(GCP, "gcp0", true)
    passes.addPass(Inline, "inline0", true)
    passes.addPass(ConstantPropagation, "cp1", true)
    passes.addPass(LICM, "licm0", false)
    passes.addPass(DeadCodeElimination, "dce2", true)
    passes.addPass(IndexExtraction, "index-extraction0", false)
    passes.addPass(Linearize, "linearize2", true)
    passes.addPass(SplitFile, "splitfile0", false)     // Should be the last one
  }

  private def parseCommandLine(args: List[String]): Boolean = {
    args match {
      case Nil => inputFileName.isDefined
      case "--include" :: x :: rest => headers.append(x); parseCommandLine(rest)
      case "-" :: rest =>
        inputFileName = Some("")
        parseCommandLine(rest)
      case "-d" :: rest =>
        debug = true
        parseCommandLine(rest)
      case "-h" :: rest =>
        sayHelp(); false
      case "--version" :: rest =>
        sayVersion(); false
      case "-o" :: x :: rest =>
        outputFileName = Some(x)
        parseCommandLine(rest)
      case "-fall" :: rest =>
        passes.enable(true)
        parseCommandLine(rest)
      case "-fno-all" :: rest =>
        passes.enable(false)
        parseCommandLine(rest)
      case "-dump-passes" :: rest =>
        passes.dump
        parseCommandLine(rest)
      case f :: v :: rest if f.startsWith("-p") =>
        handlePassArgument(f.stripPrefix("-p"), v)
        parseCommandLine(rest)
      case x :: rest if x.startsWith("-f") =>
        handlePassFlag(x)
        parseCommandLine(rest)
      case x :: rest =>
        inputFileName match
        {
          case Some(_) => false
          case None =>
          {
            inputFileName = Some(x)
            parseCommandLine(rest)
          }
        }
      case flag =>
        complain("Unknown flag " + flag)
        false
    }
  }

  private def sayHelp() {
    System.err.println("Usage: input-file [--version] [-o output-file] [-f[no-]all] [-dump-passes] [passes options] [--include header]*")
    System.err.println("       if input-file is a single hyphen (-) then input is taken from standard input")
    System.err.println("       passes can enabled/disabled with -f[-no]<PASS NAME>[idx] option")
    System.err.println("       use idx after pass name to disable specific occurence of pass")
    System.err.println("       Example: -fno-cp0 would disable the first occurance of the CP pass")
    System.err.println("       -pflatten-structs:local yes")
    System.err.println("       to enable local structs flattening")
    System.err.println("       Some passes support additional arguments:")
    System.err.println("       use -dump-passes to get the list of all passes and arguments")
    System.exit(0)
  }

  private def sayVersion() {
    System.err.println("PENCIL Optimizer")
    System.err.println(Version.getFullVersionInfo)
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
    val pencil = frontend.parse(inputFileName.get, headers, debug)

    if (pencil.isEmpty) {
      System.exit(-1)
    }

    val writer = new Printer

    val optimized = passes.run(pencil.get)
    val code = writer.toPencil(optimized, true, true)

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

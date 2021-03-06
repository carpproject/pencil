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

package com.arm.carp.apps.linker

import com.arm.carp.apps.Version
import com.arm.carp.pencil.Printer
import java.io.PrintWriter
import java.io.File
import com.arm.carp.frontends.pencil.PencilFrontEnd
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import com.arm.carp.pencil.Program
import com.arm.carp.pencil.Function
import com.arm.carp.pencil.StructType
import com.arm.carp.pencil.ScalarVariableDef
import com.arm.carp.pencil.Variable
import com.arm.carp.pencil.Checkable
import com.arm.carp.pencil.Common

/**
  * Linker application.
  *
  * Linker links multiple PENCIL files in a single file, automatically
  * assigning unique names to static functions. For non-static
  * functions, global types and constants names must be unique.
  */
object Main extends Common {

  private var correct = true

  private var debug = false

  private var prototypes_only = false

  private def same_constants(c1: Variable, c2: Variable) = {
    (c1, c2) match {
      case (s1: ScalarVariableDef, s2: ScalarVariableDef) => s1.expType == s2.expType && s1.init == s2.init
      case _ => false
    }
  }

  private def link(in: Seq[Program]): Program = {
    val names = HashSet[String]()
    val functions = ListBuffer[Function]()
    val types = HashMap[String, StructType]()
    val prototypes = HashMap[String, Function]()
    val structs = ListBuffer[StructType]()
    val consts = HashMap[String, Variable]()
    val const_list = ListBuffer[Variable]()

    def add_function_prototype(in: Function): Boolean = {
      if (!names.add(in.getName)) {
        prototypes.get(in.getName) match {
          case Some(function) if (function.ops.isEmpty || in.ops.isEmpty) => {
            if (!compatibleFunctions(function, in)) {
              complain("Incompatible previous function declarations for function: " + in.name)
              false
            } else {
              true
            }
          }
          case _ => {
            complain("Name conflict during linking: function " + in.name)
            false
          }
        }
      } else {
        true
      }
    }

    def add_constant(in: Variable): Boolean = {
      if (!names.add(in.name)) {
        consts.get(in.name) match {
          case Some(cst) => same_constants(in, cst)
          case _ => false
        }
      } else {
        const_list.append(in)
        consts.put (in.name, in)
        true
      }
    }

    for (prog <- in) {
      for (gtype <- prog.types) {
        types.get (gtype.name) match {
          case None => {
            types.put(gtype.name, gtype)
            structs.append(gtype)
          }
          case Some(struct) if !struct.compatible(gtype) =>
          complain("Name conflict during linking: struct " + gtype.name)
          case Some(struct) =>
        }
      }
      for (function <- prog.functions) {
        if (add_function_prototype(function)) {
          prototypes.put (function.getName, function)
          if (function.ops.isDefined) {
            functions.append(function)
          }
        }
      }
      for (const <- prog.consts) {
        if (!add_constant(const)) {
          complain("Name conflict during linking: const " + const.name)
        }
      }
    }
    val external = prototypes.values.filter(!functions.contains(_))
    functions.appendAll(external)
    new Program(functions.toList, structs.toList, const_list.toList)
  }

  private val inputFileNames = ListBuffer[(String, Boolean)]()
  private val headers = ListBuffer[String]()
  private var outputFileName: Option[String] = None

  private def parseCommandLine(args: List[String]): Unit = {
    args match {
      case Nil =>
      case "-d" :: rest =>
        debug = true
        parseCommandLine(rest)
      case "-h" :: rest => sayHelp()
      case "--version" :: rest => sayVersion()
      case "-o" :: x :: rest =>
        outputFileName = Some(x); parseCommandLine(rest)
      case "--prototypes-only" :: rest =>
        prototypes_only = true
        parseCommandLine(rest)
      case "--static" :: x :: rest => inputFileNames.append((x, true)); parseCommandLine(rest)
      case "--include" :: x :: rest => headers.append(x); parseCommandLine(rest)
      case x :: rest => inputFileNames.append((x, false)); parseCommandLine(rest)
    }
  }

  private def sayHelp() {
    System.err.println("Usage: [[--static] input-file]* [--version] [-o output-file] [--include header]*")
    System.exit(0)
  }

  private def sayVersion() {
    System.err.println("PENCIL Linker")
    System.err.println(Version.getFullVersionInfo)
    System.exit(0)
  }

  private def complain(message: String) {
    System.err.println("Linker error:" + message)
    correct = false
  }

  def main(args: Array[String]) {
    parseCommandLine(args.toList)

    if (inputFileNames.isEmpty) {
      return
    }

    val frontend = new PencilFrontEnd

    val buff = new ListBuffer[Program]()

    for ((prog, static) <- inputFileNames) {
      val pencil = frontend.parse(prog, headers, debug, static)
      pencil match {
        case None => complain("one of the input files contains an error")
        case Some(pprogram) =>
          {
            Checkable.walkProgram(pprogram)
            buff.append(pprogram)
          }
      }
    }

    val pencil = link(buff.toList)

    if (!correct) {
      System.exit(-1)
    }
    val writer = new Printer
    val code = writer.toPencil(pencil, true, !prototypes_only)

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

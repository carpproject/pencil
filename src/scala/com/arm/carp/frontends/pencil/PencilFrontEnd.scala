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

package com.arm.carp.frontends.pencil

import org.antlr.runtime.ANTLRFileStream
import org.antlr.runtime.CommonTokenStream
import com.arm.carp.pencil.parser.pencilLexer
import com.arm.carp.pencil.parser.pencilParser
import com.arm.carp.pencil.Program
import java.io.File

/**
  * PENCIL fron-end class.
  *
  * It parses a supplied PENCIL program file and returns either
  * valid PENCIL program or reports error and returns None.
  *
  * It uses ANTLR to parse the program and build AST, which
  * is supplied to pencil [[com.arm.carp.frontends.pencil.Transformer]].
  */

class PencilFrontEnd {
  def parse(file: String): Option[Program] = {
    if (!(new File(file)).exists()) {
      System.err.println("File " + file + " not found")
      return None
    }
    val input = new ANTLRFileStream(file)
    val lexer = new pencilLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new pencilParser(tokens)
    val pencil = parser.program()
    if (parser.isCorrect()) {
      val transformer = new Transformer(file)
      transformer.transformProgram(pencil)
    } else {
      None
    }
  }
}
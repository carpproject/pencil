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

package com.arm.carp.pencil

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

/**
  * Represents the call graph of the PENCIL program.
  *
  * Currently it support the following:
  * - Check for recursion.
  */
class CallGraph {

  private val graph = new HashMap[Function, HashSet[Function]]

  /** Record new call. */
  def addCall(callee: Function, called: Function) = {
    graph.get(callee) match {
      case None => graph.put(callee, HashSet(called))
      case Some(calls) => calls.add(called)
    }
  }

  private def getCalls(in: Function) = {
    graph.get(in) match {
      case Some(calls) => calls
      case None => HashSet[Function]()
    }
  }

  private def DFS(curr: Function, visited: Set[Function]): Option[Function] = {
    if (visited.contains(curr)) {
      Some(curr)
    } else {
      getCalls(curr).find(f => DFS(f, visited + curr).isDefined)
    }
  }

  /**
   * Return all functions called directly or indirectly by the given function.
   */
  def getAllCallees(in: Function): HashSet[Function] = {
    val calls = getCalls(in)
    calls.foreach(i => calls ++= getAllCallees(i))
    calls
  }

  /**
    * Check for recursion.
    *
    * @returns Function, which is recursively calling itself (directly or indirectly).
    */
  def getRecursion():Option[Function] = {
    if(graph.isEmpty) {
      None
    } else {
      DFS(graph.head._1, Set())
    }
  }

  def clear = graph.clear
}

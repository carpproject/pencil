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

import scala.collection.mutable.MutableList

/**
  * Set of reaching definitions for a given variable reference.
  *
  * This set includes all assignment to a variable, which can reach a
  * given reference to this variable.
  */
class DefineSet(val version: Int, val data: Set[AssignmentOperation]) extends ExpressionPassInfo


/** Set of scalar variable references, reachable by annotated assignment operation.  */
class ReachSet(val version: Int) extends OperationPassInfo {
  val data = new MutableList[ScalarVariableRef]
}

/**
  * Implements reaching definition analysis.
  *
  * The object computes the following:
  * - for each assignment operation: set of all scalar variable references,
  *   reachable by this assignment.
  * - for each variable reference: set of all assignment operation to the given
  *   variable reaching this reference.
  */
object ReachingDefinitions {

  private var version = 0

  private type DefTableEntry = Map[ScalarVariableDef, Set[AssignmentOperation]]

  private class DefTable(val defs: DefTableEntry, val mdefs: DefTableEntry)

  private def recordUse(user: ScalarVariableRef, defines: DefTable) = {
    user.info = user.info match {
      case Some(existing: DefineSet) if existing.version == version => user.info
      case _ => None
    }
    val defs = defines.defs.getOrElse(user.variable, Set())
    val mdefs = defines.mdefs.getOrElse(user.variable, Set())
    val total = defs ++ mdefs
    user.info match {
      case Some(existing: DefineSet) =>
        user.info = Some(new DefineSet(version, total ++ existing.data))
      case _ =>
        user.info = Some(new DefineSet(version, total))
    }
    for (item <- total) {
      item.info match {
        case Some(reach: ReachSet) => reach.data += user
        case _ => Checkable.ice(item, "missing reach set for assignment")
      }
    }
  }

  private def computeForExpression(in: ScalarExpression, defines: DefTable) = {
    for (user <- ExpressionAnalyzer.getScalarVariables(in)) {
      recordUse(user, defines)
    }
  }

  private def computeForDirectAssignment(lvalue: ScalarVariableRef, in: AssignmentOperation, defines: DefTable): DefTable = {
    computeForExpression(in.rvalue, defines)
    computeForExpression(in.lvalue, defines)
    new DefTable(Map(lvalue.variable -> Set(in)), Map())
  }

  private def computeForIndAssignment(in: AssignmentOperation, defines: DefTable): DefTable = {
    in.info = None
    computeForExpression(in.rvalue, defines)
    computeForExpression(in.lvalue, defines)
    new DefTable(Map(), Map())
  }

  private def getBaseVariable(in: ScalarStructSubscription): Option[ScalarVariableRef] = {
    in.base match {
      case variable: ScalarVariableRef => Some(variable)
      case struct: ScalarStructSubscription => getBaseVariable(struct)
      case _ => None
    }
  }

  private def computeForStructAssignment(struct: ScalarStructSubscription, in: AssignmentOperation, defines: DefTable): DefTable = {
    computeForExpression(in.rvalue, defines)
    computeForExpression(in.lvalue, defines)
    getBaseVariable(struct) match {
      case Some(variable) =>
        new DefTable(Map(), Map(variable.variable -> Set(in)))
      case None =>
        in.info = None
        new DefTable(Map(), Map())
    }
  }

  private def computeForAssignment(in: AssignmentOperation, defines: DefTable): DefTable = {
    in.info = in.info match {
      case Some(reach: ReachSet) if reach.version == version => in.info
      case _ => Some(new ReachSet(version))
    }
    in.lvalue match {
      case variable: ScalarVariableRef => computeForDirectAssignment(variable, in, defines)
      case struct: ScalarStructSubscription => computeForStructAssignment(struct, in, defines)
      case _ => computeForIndAssignment(in, defines)
    }
  }

  private def computeForReturn(in: ReturnOperation, defines: DefTable) = {
    in.op match {
      case Some(exp) => computeForExpression(exp, defines)
      case None =>
    }
    new DefTable(Map(), Map())
  }

  private def computeForFor(in: ForOperation, defines: DefTable): DefTable = {
    val base = computeForBlock(in.ops, defines)
    val updated = new DefTable(defines.defs, join(defines.mdefs, base.defs, base.mdefs))
    computeForExpression(in.range.low, updated)
    computeForExpression(in.range.upper, updated)
    val main = computeForBlock(in.ops, updated)
    new DefTable(Map(), join(main.defs, main.mdefs))
  }

  private def computeForIf(in: IfOperation, defines: DefTable): DefTable = {
    computeForExpression(in.guard, defines)
    val main = computeForBlock(in.ops, defines)
    val mdef = in.eops match {
      case None => new DefTable(Map(), Map())
      case Some(body) => computeForBlock(body, defines)
    }
    new DefTable(Map(), join(main.defs, main.mdefs, mdef.defs, mdef.mdefs))
  }

  private def computeForWhile(in: WhileOperation, defines: DefTable): DefTable = {
    val base = computeForBlock(in.ops, defines)
    val updated = joinTables(defines, base)
    computeForExpression(in.guard, updated)
    val main = computeForBlock(in.ops, updated)
    new DefTable(Map(), join(main.defs, main.mdefs))
  }

  private def joinTables(t1: DefTable, t2: DefTable) = {
    val mdefs = join(t1.mdefs, t2.mdefs)
    new DefTable(t1.defs ++ t2.defs, mdefs)
  }

  private def join(tabs: DefTableEntry*): DefTableEntry = {
    tabs.reduce((t1, t2) => {
      val keys = t1.keys ++ t2.keys
      keys.map(k => (k, t1.getOrElse(k, Set()) ++ t2.getOrElse(k, Set()))).toMap
    })
  }

  private def computeForCallOp(in: CallOperation, defines: DefTable) = {
    computeForExpression(in.op, defines)
    new DefTable(Map(), Map())
  }

  private def computeForType(in: Type, defines: DefTable):Unit = {
    in match {
      case _:ScalarType =>
      case ArrayType(base, range) =>
        computeForType(base, defines)
        computeForExpression(range, defines)
    }
  }

  private def computeForArrayDecl(in: ArrayDeclOperation, defines: DefTable) = {
    computeForType(in.array.expType, defines)
    new DefTable(Map(), Map())
  }

  private def computeForPencil(in: Operation with PENCILOperation, defines: DefTable) = {
    in.op match {
      case op: ScalarExpression => computeForExpression(op, defines)
      case _ =>
    }
    new DefTable(Map(), Map())
  }

  private def computeForBlock(in: BlockOperation, defines: DefTable): DefTable = {
    val res = in.ops.foldLeft(new DefTable(Map(), Map()))((defs, op) => {
      val curr = joinTables(defines, defs)
      val res = op match {
        case mov: AssignmentOperation => computeForAssignment(mov, curr)
        case ret: ReturnOperation => computeForReturn(ret, curr)
        case loop: ForOperation => computeForFor(loop, curr)
        case ifop: IfOperation => computeForIf(ifop, curr)
        case wloop: WhileOperation => computeForWhile(wloop, curr)
        case body: BlockOperation => computeForBlock(body, curr)
        case exp: CallOperation => computeForCallOp(exp, curr)
        case _: BreakOperation | _: ContinueOperation => new DefTable(Map(), Map())
        case pencil: PENCILOperation => computeForPencil(pencil, curr)
        case decl: ArrayDeclOperation => computeForArrayDecl(decl, curr)
        case _ =>
          Asserts.ice(op, "unexpected operation")
      }
      joinTables(defs, res)
    })
    res
  }

  /** Populate function with reaching definition information.  */
  def computeForFunction(in: Function) = {
    in.ops match {
      case Some(body) => computeForBlock(body, new DefTable(Map(), Map()))
      case None =>
    }
    version = version + 1
  }

  /** Populate each function of the program with reaching definition information.  */
  def compute(in: Program) = {
    for (function <- in.functions) {
      computeForFunction(function)
    }
  }
}

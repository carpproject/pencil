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

import com.arm.carp.pencil._

/**
 * This class flattens struct types in function arguments and then updates the operations.
 */
class FlattenStructParams extends Pass("struct-flatten", false) {

  val config = WalkerConfig.minimal

  // Maps old array definition (with structsubscriptions in range) to new array definition (with flat variables in range).
  var arrayMap: Map[ArrayVariableDef, ArrayVariableDef] = Map()

  // TODO: these two functions should be unified with getFlatParameters, because now there are two places where we create the flattened names.
  /**
   * Return name of struct field.
   */
  private def getFieldName(tp: Type, field: Int): String = {
    tp match {
      case s: StructType =>
        s.fields(field)._1
    }
  }

  /**
   * Given a (possibly nested) ScalarStructSubscription expression, return the flattened name.
   */
  private def getFlatFieldName(in: Expression, name: String): String = {
    in match {
      case s: ScalarStructSubscription =>
        getFlatFieldName(s.base, "") + "__" + getFieldName(s.base.expType, s.field)
      case s: ScalarVariableRef =>
        s.variable.name
    }
  }

  /**
   * Takes a struct parameter name and type, and returns the list of new parameters.
   * @param name   name of original parameter.
   * @param stype  struct type that needs to be flattened.
   * @return       sequence of new variables representing the flattened parameter.
   */
  private def getFlatParameters(name: String, stype: StructType): Seq[ScalarVariableDef] = {
    var newParams: Seq[ScalarVariableDef] = Seq()
    for (field <- stype.fields) {
      val newName = name + "__" + field._1

      newParams = newParams ++ (field._2 match {
        case s: StructType =>
          getFlatParameters(newName, s)
        case s: ScalarType =>
          Seq(new ScalarVariableDef(s, newName, None))
        case _ => ice(field._2, "unhandled type")
      })
    }
    newParams
  }

  /**
   * Flatten all struct parameters of given function.
   * @param in  function in which struct types should be removed from header.
   * @return    (sequence of all original parameters that were flattened,
   *             sequence of all newly introduced parameters).
   */
  private def flattenParameters(in: Function): (Seq[Variable], Seq[ScalarVariableDef]) = {
    var newParams: Seq[Variable] = Seq()
    var flattened: Seq[Variable] = Seq()
    var flatVars: Seq[ScalarVariableDef] = Seq()

    for (arg <- in.params) {
      arg match {
        case sv: ScalarVariableDef => {
          sv.expType match {
            case s: StructType =>
              val expanded = getFlatParameters(sv.name, s)
              newParams = newParams ++ expanded
              flattened = flattened :+ arg
              flatVars = flatVars ++ expanded
            case _ =>
              newParams = newParams :+ arg
          }
        }
        case a: ArrayVariableDef => {
          if (a.expType.base == StructType) {
            System.err.println("Warning: arrays of structure parameters are not flattened")
          }
          newParams = newParams :+ arg
        }
      }
    }
    in.params = newParams
    (flattened, flatVars)
  }

  /**
   * Return the struct variable for an ArrayExpression (that is a child of a ScalarIdxExpression, which in turn is a child of
   * a ScalarStructSubscription).
   */
  private def getStructVariable(in: ArrayExpression): VariableRef = {
    in match {
      case v: ArrayVariableRef => v
      case a: ArrayIdxExpression => getStructVariable(a.op1)
    }
  }

  /**
   * Return the root struct variable for a ScalarStructSubscription.
   */
  private def getStructVariable(in: ScalarStructSubscription): VariableRef = {
    in.base match {
      case i: ScalarIdxExpression =>
        getStructVariable(i.op1)
      case s: ScalarStructSubscription =>
        getStructVariable(s)
      case v: VariableRef => v
    }
  }

  /**
   * Return the scalar variable with the given name.
   */
  private def getFlatScalarVarFromName(name: String, flats: Seq[ScalarVariableDef]): ScalarVariableRef = {
    val flatVar = flats.find(_.name == name)
    flatVar match {
      case Some(v: ScalarVariableDef) => new ScalarVariableRef(v)
      case None => Checkable.ice(name, "Could not find associated scalar variable")
    }
  }

  /**
   * Return the scalar variable corresponding to the ScalarStructSubscription.
   */
  private def getScalarForStructSubscription(in: ScalarStructSubscription, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): ScalarVariableRef = {
    val structVar = getStructVariable(in)
    val varName = getFlatFieldName(in, structVar.variable.name);
    getFlatScalarVarFromName(varName, flats)
  }

  /**
   * Return the sequence of scalar variables a given struct was flattened to.
   */
  private def getScalarVarsForFlattenedStruct(in: StructType, varName: String, flats: Seq[ScalarVariableDef]): Seq[ScalarVariableRef] = {
    var scalars: Seq[ScalarVariableRef] = Seq()
    val params = getFlatParameters(varName, in)
    for (p <- params) {
      scalars = scalars :+ getFlatScalarVarFromName(p.name, flats)
    }
    scalars
  }

  /**
   * Return updated function call parameter list for given parameter.  If the given parameter is a struct, then it is
   * expanded into a sequence of the corresponding `flats'.
   */
  private def updateCallParameter(in: ScalarVariableRef, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Seq[ScalarVariableRef] = {
    var newParams: Seq[ScalarVariableRef] = Seq()
    in.expType match {
      case s: StructType =>
        newParams = newParams ++ getScalarVarsForFlattenedStruct(s, in.variable.name, flats)
      case _ =>
        newParams = newParams :+ in
    }
    newParams
  }

  /**
   * Recursively update an array expression and flatten any struct parameters encountered.
   */
  private def updateArrayExpression(in: ArrayExpression, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): ArrayExpression = {
    in match {
      case v: ArrayVariableRef => new ArrayVariableRef(arrayMap(v.variable))

      case idx: ArrayIdxExpression =>
        val op1 = updateArrayExpression(idx.op1, flattened, flats)
        val op2 = updateExpression(idx.op2, flattened, flats)
        idx.update(op1, op2)

      case s: ArrayStructSubscription =>
        s // Just s, because we do not yet flatten arrays of struct parameters.
    }
  }

  /**
   * Recursively update a scalar expression and flatten any struct parameters encountered.
   */
  private def updateExpression(in: ScalarExpression, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): ScalarExpression = {
    in match {
      case v: ScalarVariableRef =>
        v

      case exp: ConvertExpression =>
        val op1 = updateExpression(exp.op1, flattened, flats)
        exp.update(op1)

      case exp: ScalarUnaryExpression =>
        val op1 = updateExpression(exp.op1, flattened, flats)
        exp.update(op1)

      case exp: ScalarBinaryExpression =>
        val op1 = updateExpression(exp.op1, flattened, flats)
        val op2 = updateExpression(exp.op2, flattened, flats)
        exp.update(op1, op2)

      case exp: TernaryExpression =>
        val op1 = updateExpression(exp.op1, flattened, flats)
        val op2 = updateExpression(exp.op2, flattened, flats)
        val op3 = updateExpression(exp.op3, flattened, flats)
        exp.update(op1, op2, op3)

      case exp: CallExpression =>
        var nargs: Seq[Expression] = Seq()
        for (arg <- exp.args) {
          arg match {
            case svar: ScalarVariableRef =>
              nargs = nargs ++ updateCallParameter(svar, flattened, flats)
            case sexp: ScalarExpression =>
              nargs = nargs :+ updateExpression(sexp, flattened, flats)
            case aexp: ArrayExpression =>
              nargs = nargs :+ updateArrayExpression(aexp, flattened, flats)
          }
        }
        exp.copy(args = nargs)

      case exp: IntrinsicCallExpression =>
        val nargs = exp.args.map(arg => updateExpression(arg, flattened, flats))
        exp.copy(args = nargs)

      case idx: ScalarIdxExpression =>
        val op1 = updateArrayExpression(idx.op1, flattened, flats)
        val op2 = updateExpression(idx.op2, flattened, flats)
        idx.update(op1, op2)

      case s: ScalarStructSubscription =>
        val structVar = getStructVariable(s)
        if (flattened.contains(structVar.variable)) {
          getScalarForStructSubscription(s, flattened, flats)
        } else {
          s.update(updateExpression(s.op1, flattened, flats))
        }

      case exp: ScalarExpression with Constant => exp

      case _ => Checkable.ice(in, "unexpected expression")
    }
  }

  /**
   * Return updated ArrayType, whose range has been updated for any flattened function arguments.
   */
  private def updateType(in: ArrayType, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): ArrayType = {
    ArrayType(updateType(in.base, flattened, flats), updateExpression(in.range, flattened, flats))
  }

  /**
   * Return updated Type, updated for any flattened function arguments.
   */
  private def updateType(in: Type, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Type = {
    in match {
      case a: ArrayType =>
        ArrayType(updateType(a.base, flattened, flats), updateExpression(a.range, flattened, flats))
      case _ =>
        in
    }
  }

  /**
   * Return updated function parameter list.
   */
  private def updateParamRefsInHeader(in: Seq[Variable], flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Seq[Variable] = {
    var newParams = in
    for (arg <- in) {
      arg match {
        case a: ArrayVariableDef =>
          val newArrayDef = a.copy(expType = updateType(a.expType, flattened, flats))
          arrayMap = arrayMap + (a -> newArrayDef)
          newParams = newParams.updated(in.indexOf(arg), newArrayDef)
        case _ =>
      }
    }
    newParams
  }

  /**
   * Return updated Range in which struct parameters are replaced by their flattened counterparts.
   */
  private def updateRange(in: Range, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Range = {
    val lower = updateExpression(in.low, flattened, flats)
    val upper = updateExpression(in.upper, flattened, flats)
    in.copy(low = lower, upper = upper)
  }

  /**
   * Replace all struct parameters in indirect AssignmentOperation by their flat counterparts.
   */
  private def updateIndAssignment(in: AssignmentOperation, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Unit = {
    in.lvalue = updateExpression(in.lvalue, flattened, flats) match {
      case lvalue: ScalarExpression with LValue => lvalue
      case exp: ScalarExpression => Checkable.ice(exp, "unexpected lvalue")
    }
  }

  /**
   * Replace all struct parameters in AssignmentOperation by their flat counterparts.
   */
  private def updateAssignment(in: AssignmentOperation, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Unit = {
    in.rvalue = updateExpression(in.rvalue, flattened, flats)
    in.lvalue match {
      case _: ScalarVariableRef =>
      case _ => updateIndAssignment(in, flattened, flats)
    }
  }

  /**
   * Replace all struct parameters in Operation by their flat counterparts.
   */
  private def updateOperation(in: Operation, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Unit = {
    in match {
      case op: AssignmentOperation =>
        updateAssignment(op, flattened, flats)

      case op: IfOperation =>
        updateParamRefsInBody(op.ops, flattened, flats)
        op.eops match {
          case Some(b) => updateParamRefsInBody(b, flattened, flats)
          case None => None
        }
        op.guard = updateExpression(op.guard, flattened, flats)

      case op: ForOperation =>
        updateParamRefsInBody(op.ops, flattened, flats)
        op.range = updateRange(op.range, flattened, flats)

      case op: WhileOperation =>
        updateParamRefsInBody(op.ops, flattened, flats)
        op.guard = updateExpression(op.guard, flattened, flats)

      case op: CallOperation =>
        op.op = updateExpression(op.op, flattened, flats) match {
          case exp: CallExpression => exp
          case exp: ScalarExpression => Checkable.ice(exp, "call expression expected")
        }

      case op: ReturnOperation =>
        op.op = op.op match {
          case Some(exp) => Some(updateExpression(exp, flattened, flats))
          case None => None
        }

      case op: AssumeOperation =>
        op.op = op.op match {
          case se: ScalarExpression => updateExpression(se, flattened, flats)
          case ae: ArrayExpression => updateArrayExpression(ae, flattened, flats)
        }

      case op: BreakOperation =>

      case op: ContinueOperation =>

      case _ => Checkable.ice(in, "unexpected operation")
    }
  }

  /**
   * Replace all struct parameters in BlockOperation by their flat counterparts.
   */
  private def updateParamRefsInBody(in: BlockOperation, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Unit = {
    in.ops.foreach(op => updateOperation(op, flattened, flats))
  }

  /**
   * Replace all references to the flattened parameters by their corresponding
   * scalars.
   * @param in         Function whose struct parameters have been flattened.
   * @param flattened  Seq of struct parameters that were flattened.
   * @param flats      Seq of new non-struct parameters that replace the struct parameters.
   */
  private def updateParamRefs(in: Function, flattened: Seq[Variable], flats: Seq[ScalarVariableDef]): Unit = {
    in.params = updateParamRefsInHeader(in.params, flattened, flats)
    in.ops match {
      case Some(body) =>
        updateParamRefsInBody(body, flattened, flats)
      case None =>
    }
  }

  /**
   * Execute FlattenStructParams pass on a Program.
   * @param program  Program to act on.
   * @return         New program in which all struct parameters have been flattened.
   */
  override def execute(program: Program) = {
    for (f <- program.functions) {
      val flatParams = flattenParameters(f)
      updateParamRefs(f, flatParams._1, flatParams._2)
    }
    program
  }
}

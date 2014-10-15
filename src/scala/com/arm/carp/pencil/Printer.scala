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

/** Generates a string representation of the PENCIL program. */
class Printer extends Assertable {

  private var buff = new StringBuilder

  private var in_macro = false

  private val glabels = HashMap[Operation, Int]()

  private def getVarName(in: Variable) = in.name + "_" + in.id
  private def getFuncName(in: Function) = in.getName

  private def processWithSep(in: Iterable[Expression], sep: String) = {
    var suffix = ""
    for (item <- in) {
      buff.append(suffix)
      process(item)
      suffix = sep
    }
  }

  private def processArrayConstant(in: Iterable[Expression]) = {
    buff.append("{")
    processWithSep(in, ",")
    buff.append("}")
  }

  private def processStructSubs(base: ScalarExpression, field: Int, stype: StructType) = {
    process(base)
    buff.append(".")
    buff.append(stype.fields(field)._1)
  }

  /**
   * Returns whether we should omit printing parentheses around the given expression.
   * @param in  expression that is about to be printed.
   * @return    true if parentheses should be omitted.
   */
  private def omitExprParentheses(in: Expression): Boolean = {
    in.isInstanceOf[Constant] || in.isInstanceOf[LValue] || in.isInstanceOf[CallExpression]
  }

  private def process(in: Expression): Unit = {
    if (!omitExprParentheses(in))
      buff.append("(");
    in match {
      case bin: ScalarBinaryExpression =>
        process(bin.op1); buff.append(bin.op); process(bin.op2);
      case un: ScalarUnaryExpression =>
        buff.append(un.op); process(un.op1);
      case ArrayIdxExpression(array, idx) =>
        process(array); buff.append("["); process(idx); buff.append("]")
      case ScalarIdxExpression(array, idx) =>
        process(array); buff.append("["); process(idx); buff.append("]")
      case variable: VariableRef => buff.append(getVarName(variable.variable))
      case IntegerConstant(_, value) => buff.append(value)
      case FloatConstant(FloatType(64, _), value) => buff.append(value)
      case FloatConstant(FloatType(32, _), value) =>
        buff.append(value)
        buff.append("f")
      case cst: ArrayConstant => processArrayConstant(cst.data)
      case BooleanConstant(value) =>
        value match {
          case Some(op) => buff.append(op)
          case None => buff.append("__pencil_maybe()")
        }
      case ConvertExpression(expType, op) =>
        buff.append("(")
        process(expType)
        buff.append(")")
        process(op)
      case TernaryExpression(guard, op1, op2) => {
        process(guard)
        buff.append("?")
        process(op1)
        buff.append(":")
        process(op2)
      }
      case CallExpression(func, args) => {
        buff.append(getFuncName(func))
        buff.append("(")
        processWithSep(args, ",")
        buff.append(")")
      }
      case ScalarStructSubscription(base, field) => processStructSubs(base, field, base.expType.asInstanceOf[StructType])
      case ArrayStructSubscription(base, field) => processStructSubs(base, field, base.expType.asInstanceOf[StructType])
      case SizeofExpression(obj) =>
        buff.append("sizeof(")
        declareVariable("", obj, "")
        buff.append(")")
      case _ => ice(in, "unknown expression")
    }
    if (!omitExprParentheses(in))
      buff.append(")");
  }

  private def process(in: AssignmentOperation): Unit = {
    process(in.lvalue)
    buff.append(" = ")
    process(in.rvalue)
    buff.append(";")
  }

  private def process(in: BlockOperation): Unit = {
    buff.append("{")
    if (!in_macro) {
      buff.append("\n")
    }
    val info = in.info match {
      case Some(defs:DefinedVariables) => defs
      case _ => ice(in.info, "defined variables info expected")
    }
    processDeclarations(info.defs.toSeq)
    for (op <- in.ops) {
      process(op)
      if (!in_macro) {
        buff.append("\n")
      }
    }
    buff.append("}")
  }

  private def process(in: IfOperation): Unit = {
    buff.append("if(")
    process(in.guard)
    buff.append(")")
    process(in.ops)
    in.eops match {
      case None =>
      case Some(ops) =>
        buff.append("else\n")
        process(ops)
    }
  }

  private def process(in: WhileOperation): Unit = {
    buff.append("while(")
    process(in.guard)
    buff.append(")")
    process(in.ops)
  }

  private def process(in: ForOperation): Unit = {
    for (option <- in.properties) {
      option match {
        case IvdepLoop => buff.append("#pragma pencil ivdep\n")
        case labels: IndependentLoop => {
          buff.append("#pragma pencil independent")
          labels.labels match {
            case Some(list) =>
              buff.append("(")
              var sep = ""
              for (item <- list) {
                val id = glabels.size
                buff.append(sep)
                buff.append("l" + id)
                glabels += ((item, id))
                sep = ","
              }
              buff.append(")")
            case None =>
          }
          buff.append("\n")
        }
      }
    }
    buff.append("for(")
    process(in.range.iter.expType)
    process(in.range.iter)
    buff.append(" = ")
    process(in.range.low)
    buff.append(";")
    process(in.range.iter)

    if (in.range.step.value > 0) {
      buff.append(" <= ")
    } else {
      buff.append(" >= ")
    }

    process(in.range.upper)
    buff.append(";")
    process(in.range.iter)
    buff.append(" += ")
    process(in.range.step)
    buff.append(")")
    process(in.ops)
  }

  private def process(in: ReturnOperation): Unit = {
    buff.append("return")
    if (in.op.isDefined) {
      buff.append(" (")
      process(in.op.get)
      buff.append(")")
    }
    buff.append(";")
  }

  private def process(in: CallOperation): Unit = {
    process(in.op)
    buff.append(";")
  }

  def process(_type: Type) {
    if (_type.const)
      buff.append("const ")
    _type match {
      case BooleanType(_) => buff.append("bool ")
      case IntegerType(true, 8, _) => buff.append("char ")
      case IntegerType(true, 16, _) => buff.append("short ")
      case IntegerType(true, 32, _) => buff.append("int ")
      case IntegerType(true, 64, _) => buff.append("long ")
      case IntegerType(false, 8, _) => buff.append("unsigned char ")
      case IntegerType(false, 16, _) => buff.append("unsigned short ")
      case IntegerType(false, 32, _) => buff.append("unsigned int ")
      case IntegerType(false, 64, _) => buff.append("unsigned long ")
      case FloatType(16, _) => buff.append("half ")
      case FloatType(32, _) => buff.append("float ")
      case FloatType(64, _) => buff.append("double ")
      case NopType => buff.append("void ")
      case StructType(_, _, name) => buff.append("struct " + name + " ")
      case _ => ice(_type, "unsupported type")
    }
  }

  private def process(in: ArrayDeclOperation): Unit = {
    processDeclaration(in.array)
  }

  private def process(in: Operation): Unit = {
    if (in.scop) {
      buff.append("\n#pragma scop\n")
    }
    in.access match {
      case Some(body) =>
        in_macro = true
        buff.append("\n#pragma pencil access")
        process(body)
        in_macro = false
        buff.append("\n")
      case None =>
    }
    val label = glabels.get(in)
    label match {
      case Some(id) => buff.append("l" + id + ":")
      case None =>
    }
    in match {
      case mov: AssignmentOperation => process(mov)
      case body: BlockOperation => process(body)
      case ifop: IfOperation => process(ifop)
      case whileop: WhileOperation => process(whileop)
      case forop: ForOperation => process(forop)
      case break: BreakOperation => buff.append("break;")
      case break: ContinueOperation => buff.append("continue;")
      case call: CallOperation => process(call)
      case ret: ReturnOperation => process(ret)
      case decl: ArrayDeclOperation => process(decl)
      case op: PENCILOperation => {
        buff.append(op.str)
        buff.append("(")
        process(op.op)
        buff.append(");")
      }
    }
    if (in.scop) {
      buff.append("\n#pragma endscop\n")
    }
  }

  private def needsQualifier(in: Type): Boolean = {
    in match {
      case _: ScalarType => false
      case ArrayType(base, idx: IntegerConstant) => needsQualifier(base)
      case _ => true
    }
  }

  private def getArgQualifier(in: Variable) = {
    in match {
      case arr: ArrayVariableDef =>
        if (!needsQualifier(arr.expType)) {
          ""
        } else {
          if (arr.restrict) {
            "restrict const static "
          } else {
            "const static "
          }
        }
      case _ => ""
    }
  }

  /* prints the part '[scalar-expression]'
  **/
  private def declareArrayVar( base: Type, range: ScalarExpression, lbuff: StringBuilder, qualifier: String): Unit = {
    lbuff.append('[')
    lbuff.append(qualifier)
    range match {
      case cst: IntegerConstant => lbuff.append(cst.value)
      case exp: ScalarExpression =>
        val tmp = buff
        buff = lbuff
        process(exp)
        buff = tmp
      case _ => ice(range, "Invalid array size")
    }
    lbuff.append(']')
    base match {
      case ArrayType(nbase, nrange) => declareArrayVar( nbase, nrange, lbuff, "")
      case _ => buildVariable(base, "", lbuff );
    }
  }

  /** Casts 'var as '*var' and 'var[.]' as '(*var)[.]'
    */
   private def declarePointerVar( _type: Type, lbuff: StringBuilder ): Unit= {
     _type match{
       case ArrayType(base, range) => {
         lbuff.insert(0, "( *"); lbuff.append(")")                        /* wrap the pointer as (*lbuff) */
         declareArrayVar(base, range, lbuff, "")
       }
       case _ => {
         lbuff.insert(0, "*")
         buildVariable( _type, "", lbuff)
       }
     }
   }



  /** Follows recursive style of variable construction to support complex pointer type
   */
  private def buildVariable( _type: Type, qualifier: String, lbuff : StringBuilder ): Unit  = {
    _type match {
      case ArrayType(base, range) =>
         declareArrayVar(base, range, lbuff, qualifier)
      case PointerType(base) =>
         declarePointerVar( base, lbuff )
      case _ => {
         process( _type);                              /*primitive-struct which appears leftmost in buff itself*/
      }
    }
  }



  /** The base type is printed by 'process( _type)' directly into buff, when all array and pointer wrappers are unfolded by
    * buildVariable. Here we start lbuff with variable name and print the array, '*', cast parts around it.
    * As last step, the lbuff is appended to the buff which already has the base-type prefixed to it.
    */
  private def declareVariable(name: String, _type: Type, qualifier: String): Unit = {
    var lbuff = (new StringBuilder).append(name)
    buildVariable( _type, qualifier, lbuff )             /*build rest around name e.g. (*name[10])[20] */
    buff.append( lbuff )
  }


  private def processDeclaration(variable: Variable) = {
    declareVariable(getVarName(variable), variable.expType, "")
      variable match {
        case ArrayVariableDef(_, _, _, Some(init), _) =>
          buff.append(" = ")
          process(init)
        case ScalarVariableDef(_, _, Some(init), _) =>
          buff.append(" = ")
          process(init)
        case _ =>
      }
      buff.append(";\n")
  }

  private def processDeclarations(in: Seq[Variable]) = {
    for (variable <- in.filter(!_.iter)) {
      processDeclaration(variable)
    }
  }

  private def processParams(in: Seq[Variable]) = {
    var suffix = ""
    for (variable <- in) {
      buff.append(suffix)
      declareVariable(getVarName(variable), variable.expType, getArgQualifier(variable))
      suffix = ","
    }
  }

  private def processFunctionDeclaration(in: Function) = {
    if (in.local) buff.append("static ")
    process(in.retType)
    buff.append(getFuncName(in))
    buff.append(" (")
    processParams(in.params)
    buff.append(")")
  }

  /**
   * Print function definition.
   * @param func  Function whose body should be printed.
   * @param csts  Constants defined outside of the function.
   */
  def processFunctionDefinition(func: Function, csts: Seq[Variable]) = {
    ComputeDeclarations.computeForFunction(func, csts.toSet)

    processFunctionDeclaration(func)

    if (func.const) {
      buff.append(" /*__attribute__((const))*/")
    }
    process(func.ops.get)
    buff.append("\n\n")
  }

  def processStructDefinitions(in: Seq[StructType]) = {
    for (sdef <- in) {
      buff.append("struct ")
      buff.append(sdef.name)
      buff.append("{\n")
      for (field <- sdef.fields) {
        declareVariable(field._1, field._2, "")
        buff.append(";\n")
      }
      buff.append("};\n")
    }
  }

  def toPencil(in: Program, prototypes: Boolean, fbodies: Boolean,
               external_prototypes: Boolean = false): String = {
    processDeclarations(in.consts)
    processStructDefinitions(in.types)
    if (prototypes) {
      buff.append("\n// Function prototypes\n")
      for (func <- in.functions) {
        if (func.ops.isDefined || external_prototypes) {
          processFunctionDeclaration(func)
          if (func.access.isDefined) {
            buff.append(" __attribute__((access(")
            buff.append(getFuncName(func.access.get))
            buff.append(")))")
          }
          if (func.const) {
            buff.append(" __attribute__((const))")
          }
          buff.append(";\n")
        }
      }
    }
    if (fbodies) {
      buff.append("\n// Function definitions\n")
      for (func <- in.functions.filter(_.ops.isDefined)) {
        processFunctionDefinition(func, in.consts)
      }
    }
    buff.toString
  }
}

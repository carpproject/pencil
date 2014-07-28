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

import com.arm.carp.pencil.Common
import com.arm.carp.pencil.Program
import com.arm.carp.pencil.parser.pencilParser._
import scala.collection.mutable.ListBuffer
import com.arm.carp.pencil._
import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import org.antlr.runtime.tree.Tree
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet


/** Nested namespaces support.  */
class NamespaceStack {
  private type Namespace = Map[String, (Variable, Boolean)]

  private val data = Stack[Namespace]()

  /** Labeled operations. */
  private val ldata = Map[String, Operation]()

  /**
    * Create new namespace.
    *
    * This function should be called when entering new PENCIL block.
    */
  def push() = {
    data.push(Map[String, (Variable, Boolean)]())
  }

  /**
    * Pop top namespace.
    *
    * This function should be called when leaving a PENCIL block.
    */
  def pop() = {
    data.pop
    if (data.size == 1) {
      /* We are in the global namespace, so label information should be purged.  */
      ldata.clear
    }
  }

  /**
    * Declare new variable in current namespace.
    *
    * @returns <code>true</code> if the variable is declared, <code>false</code> if
    * variable with such name already exists in current namespace.
    */
  def addVariable(in: Variable, name: String, cst: Boolean = false) = {
    if (data.top.contains(name)) {
      false
    } else {
      data.top += ((name, (in, cst)))
      true
    }
  }

  /** Find variable with the given name.  */
  def getVariable(name: String): Option[VariableRef with Expression] = {
    val idx = data.indexWhere(_.contains(name))
    if (idx == -1) {
      None
    } else {
      data(idx).get(name) match {
        case Some((scalar: ScalarVariableDef, cst: Boolean)) => Some(new ScalarVariableRef(scalar, cst))
        case Some((scalar: ArrayVariableDef, _)) => Some(new ArrayVariableRef(scalar))
        case _ => Checkable.ice(name, "required variable not found")
      }
    }
  }

  /** Register new label.  */
  def addLabel(in: Operation, name: String) = {
    if (ldata.contains(name)) {
      false
    } else {
      ldata += ((name, in))
      true
    }
  }

  /** Get operation associated with the given label.  */
  def getLabel(label: String): Option[Operation] = {
    ldata.get(label)
  }
}

/**
  * Convert PENCIL AST obtained from ANTLR to PENCIL IR.
  *
  * Transformer contains a set of transformXXX functions, to convert
  * corresponding ASTs to PENCIL IR trees.
  */
class Transformer(val filename: String) extends Common with Assertable {

  private val callGraph = new CallGraph

  private val calls = new HashSet[Function]

  /**
    * Check whether given node has the expected tag and number of child nodes.
    *
    * Number of children check can be omitted if expected number is not specified
    * (or specified as -1).
    */

  private def checkNode(in: Tree, tag: Int, text: String, children: Int = -1) = {
    if (children != -1) {
      assert(in.getChildCount == children, in, "Unexpected number of children (" + children + " expected, but " + in.getChildCount + " found)")
    }
    assert(in.getType == tag, in, text + " expected")
  }

  private val IntegerConstantType = IntegerType(true, 32, false)
  private val DoubleConstantType = FloatType(64, false)
  private val FloatConstantType = FloatType(32, false)

  private val structtypes = ListBuffer[StructType]()
  private val structnames = Set[String]()
  private val consts = ListBuffer[Variable]()
  private var error = false

  private val varmap = new NamespaceStack
  private val fmap = Map[String, Function]()
  private val typemap = Map[String, Type]()
  private val stypemap = Map[String, Type]()

  private var current_function_type: Option[ScalarType] = None

  /** For debugging use. Print given ANTLR tree.  */
  private def printTree(in: Tree, indent: String): Unit = {
    System.out.println(indent + in.getText());
    for (i <- 0 to (in.getChildCount - 1)) {
      printTree(in.getChild(i), indent + " ")
    }
  }

  private def complain(in: Tree, message: String) {
    error = true
    System.err.println(filename + " line " + in.getLine() + ":" + in.getCharPositionInLine + " error:" + message)
  }

  private def check(cond: Boolean, in: Tree, message: String): Boolean = {
    if (!cond)
      complain(in, message)
    cond
  }

  private def transformIntConstant(in: Tree): Option[IntegerConstant] = {
    in.getType match {
      case NUMBER => Some(IntegerConstant(IntegerConstantType, Integer.parseInt(in.getText)))
      case OCTAL_NUMBER => Some(IntegerConstant(IntegerConstantType, Integer.parseInt(in.getText, 8)))
      case HEX_NUMBER => Some(IntegerConstant(IntegerConstantType, Integer.decode(in.getText)))
      case _ => ice(in, "invalid node for int constant")
    }
  }

  private def transformFloatConstant(in: Tree): Option[FloatConstant] = {
    val ctype = in.getType match {
      case DOUBLE_NUMBER => DoubleConstantType
      case FLOAT_NUMBER => FloatConstantType
    }
    Some(FloatConstant(ctype, in.getText.toDouble))
  }

  private def transformTernaryExpression(in: Tree): Option[TernaryExpression] = {
    checkNode(in, TERNARY, "TERNARY", 3)
    val op1 = transformScalarExpression(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    val op3 = transformScalarExpression(in.getChild(2))
    (op1, op2, op3) match {
      case (Some(op1), Some(op2), Some(op3)) =>
        if (check(op1.expType.isBoolean && op3.expType.convertible(op2.expType), in, "invalid ternary expression")) {
          val _type = getMaxType(op2.expType, op3.expType)
          Some(TernaryExpression(convertScalar(op1, BooleanType(true)), convertScalar(op2, _type), convertScalar(op3, _type)))
        } else {
          None
        }
      case _ => None
    }
  }

  private def transformBooleanBinaryExpression(in: Tree): Option[ScalarBooleanBinaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    (op1, op2) match {
      case (Some(op1), Some(op2)) => {
        if (!check(op1.expType.isBoolean && op2.expType.isBoolean, in, "invalid argument types for boolean operation")) {
          None
        } else {
          val _type = getMaxType(op1.expType, op2.expType)
          in.getType match {
            case LAND => Some(AndExpression(op1, op2))
            case LOR => Some(OrExpression(op1, op2))
            case _ => ice(in, "unexpected boolean binary expression")
          }
        }
      }
      case _ => None
    }
  }

  private def transformBitBinaryExpression(in: Tree): Option[ScalarBitBinaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    (op1, op2) match {
      case (Some(op1), Some(op2)) => {
        if (!check(op1.expType.isInt && op2.expType.isInt, in, "invalid argument types for bit operation")) {
          None
        } else {
          in.getType match {
            case BITOR => Some(BitOrExpression(op1, op2))
            case BITAND => Some(BitAndExpression(op1, op2))
            case BITXOR => Some(BitXorExpression(op1, op2))
            case BITLSHIFT => Some(BitLShiftExpression(op1, op2))
            case BITRSHIFT => Some(BitRShiftExpression(op1, op2))
            case _ => ice(in, "unexpected bit binary expression")
          }
        }
      }
      case _ => None
    }
  }

  private def transformComparisonExpression(in: Tree): Option[ScalarComparisonBinaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    (op1, op2) match {
      case (Some(op1), Some(op2)) => {
        val isNumeric = op1.expType.isNumeric && op2.expType.isNumeric
        val isBoolean = op1.expType.isBoolean && op2.expType.isBoolean
        val eqComparison = in.getType == EQ || in.getType == NEQ
        if (!check(isNumeric || (isBoolean && eqComparison), in, "invalid argument types for comparison operation")) {
          None
        } else {
          val _type = getMaxType(op1.expType, op2.expType)
          val _op1 = convertScalar(op1, _type)
          val _op2 = convertScalar(op2, _type)
          in.getType match {
            case EQ => Some(EqualExpression(_op1, _op2))
            case NEQ => Some(NEqualExpression(_op1, _op2))
            case GREATER => Some(GreaterExpression(_op1, _op2))
            case LESS => Some(LessExpression(_op1, _op2))
            case GEQ => Some(GreaterEqExpression(_op1, _op2))
            case LEQ => Some(LessEqExpression(_op1, _op2))
            case _ => ice(in, "unexpected comparison binary expression")
          }
        }
      }
      case _ => None
    }
  }

  private def transformMathBinaryExpression(in: Tree): Option[ScalarMathBinaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    (op1, op2) match {
      case (Some(op1), Some(op2)) => {
        if (!check(op1.expType.isNumeric && op2.expType.isNumeric, in, "invalid argument types for math operation")) {
          None
        } else {
          val _type = getMaxType(op1.expType, op2.expType)
          val _op1 = convertScalar(op1, _type)
          val _op2 = convertScalar(op2, _type)
          in.getType match {
            case PLUS => Some(PlusExpression(_op1, _op2))
            case MINUS => Some(MinusExpression(_op1, _op2))
            case DIV => Some(DivExpression(_op1, _op2))
            case MULT => Some(MultExpression(_op1, _op2))
            case _ => ice(in, "unexpected math binary expression")
          }
        }
      }
      case _ => None
    }
  }

  private def transformModExpression(in: Tree): Option[ModExpression] = {
    checkNode(in, MOD, "MOD", 2)
    val op1 = transformScalarExpression(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    (op1, op2) match {
      case (Some(op1), Some(op2)) => {
        if (!check(op1.expType.isInt && op2.expType.isInt, in, "invalid argument types for MOD operation")) {
          None
        } else {
          Some(ModExpression(op1, op2))
        }
      }
      case _ => None
    }
  }

  private def transformCastExpression(in: Tree): Option[ConvertExpression] = {
    checkNode(in, CAST, "CAST", 2)
    val op1 = transformBuiltInType(in.getChild(0))
    val op2 = transformScalarExpression(in.getChild(1))
    (op1, op2) match {
      case (Some(op1), Some(op2)) =>
        if (!check(op2.expType.convertible(op1), in, "invalid argument for cast operation")) {
          None
        } else {
          Some(ConvertExpression(op1, op2))
        }
      case _ => None
    }
  }

  private def transformSizeofExpression(in: Tree): Option[SizeofExpression] = {
    checkNode(in, SIZEOF, "SIZEOF", 1)
    val op = in.getChild(0)
    (if (op.getType == TYPE) {
      transformType(op)
    } else {
      getExpType(op)
    }) match {
      case Some(_type) => Some(SizeofExpression(_type))
      case None => None
    }
  }

  private def transformBitUnaryExpression(in: Tree): Option[ScalarBitUnaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    op1 match {
      case Some(op1) => {
        in.getType match {
          case BITNEG => {
            if (!check(op1.expType.isInt, in, "invalid argument for BITNEG operation")) {
              None
            } else {
              Some(BitNegExpression(op1))
            }
          }
          case _ => ice(in, "unexpected bit unary expression")
        }
      }
      case _ => None
    }
  }

  private def transformBooleanUnaryExpression(in: Tree): Option[ScalarBooleanUnaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    op1 match {
      case Some(op1) => {
        in.getType match {
          case LNOT => {
            if (!check(op1.expType.isBoolean, in, "invalid argument for unary NOT operation")) {
              None
            } else {
              Some(NotExpression(op1))
            }
          }
          case _ => ice(in, "unexpected boolean unary expression")
        }
      }
      case _ => None
    }
  }

  private def transformMathUnaryExpression(in: Tree): Option[ScalarMathUnaryExpression] = {
    val op1 = transformScalarExpression(in.getChild(0))
    op1 match {
      case Some(op1) => {
        in.getType match {
          case UMINIS => {
            if (!check(op1.expType.isNumeric, in, "invalid argument for unary minus operation")) {
              None
            } else {
              Some(UnaryMinusExpression(op1))
            }
          }
          case _ => ice(in, "unexpected math unary expression")
        }
      }
      case _ => None
    }
  }

  private def transformCallExpression(in: Tree): Option[ScalarExpression] = {
    val fname = in.getChild(0)
    checkNode(fname, NAME, "invalid function call syntax", 0)

    val args = (intWrapper(1) to (in.getChildCount - 1)).map(i => {
      val narg = in.getChild(i)
      narg.getType match {
        case NAME | STRUCT_SUBS | ARRAY_SUBS=> transformTerm(narg)
        case _ => transformScalarExpression(narg)
      }
    }).filter(_.isDefined).map(_.get)
    fmap.get(fname.getText) match {
      case Some(function) => {
        if (!check(function.params.size == args.size, in, "invalid number of function arguments")) {
          None
        } else {
          if (!(function.params, args).zipped.forall((variable, arg) =>
            check(arg.expType.convertible(variable.expType), in, "invalid function argument for " + variable.name + " (" + arg.expType + " -> " + variable.expType + ")"))) {
            None
          } else {
            calls += function
            Some(CallExpression(function, args))
          }
        }
      }
      case None => {
        BuiltIn.function.get(fname.getText) match {
          case Some(nargs) =>
            if (!check(nargs == args.size, in, "invalid number of intrinsic arguments")) {
              None
            } else {
              val valid = (args).forall(arg => arg.expType.convertible(GenType))
              if (!check(valid, in, "invalid intrinsic arguments")) {
                None
              } else {
                Some(IntrinsicCallExpression(fname.getText, args.map(_ match {
                  case exp: ScalarExpression => exp
                  case exp: Expression => ice(exp, "unexpected expression")
                })))
              }
            }
          case None =>
            complain(fname, "Function " + fname.getText + " undeclared")
            None
        }
      }
    }
  }

  private def transformScalarVariable(in: Tree): Option[ScalarVariableRef] = {
    checkNode(in, NAME, "NAME", 0)
    varmap.getVariable(in.getText) match {
      case Some(st: ScalarVariableRef) => Some(st)
      case Some(at) => {
        complain(in, "scalar variable expected")
        None
      }
      case None => {
        complain(in, "undeclared variable: " + in.getText)
        None
      }
    }
  }

  private def transformTerm(in: Tree): Option[Expression] = {
    val res = in.getType match {
      case STRUCT_SUBS => {
        checkNode(in, STRUCT_SUBS, "STRUCT_SUBS", 2)
        val name = in.getChild(1)
        checkNode(name, NAME, "NAME", 0)
        transformTerm(in.getChild(0)) match {
          case Some(base: ScalarExpression) => {
            base.expType match {
              case StructType(fields, _, _) => {
                val idx = fields.indexWhere(field => field._1 == name.getText)
                if (idx == -1) {
                  complain(in, "unknown struct field: " + name.getText)
                  None
                } else {
                  fields(idx)._2 match {
                    case _: ScalarType => Some(ScalarStructSubscription(base, idx))
                    case _: ArrayType => Some(ArrayStructSubscription(base, idx))
                    case _ => None
                  }
                }
              }
              case _ => {
                complain(in.getChild(0), "struct expected")
                None
              }
            }
          }
          case _ => None
        }
      }
      case ARRAY_SUBS => {
        checkNode(in, ARRAY_SUBS, "ARRAY_SUBS", 2)
        val base = transformTerm(in.getChild(0))
        val idx = transformScalarExpression(in.getChild(1))
        (base, idx) match {
          case (Some(base: ArrayExpression), Some(idx)) => {
            if (!check(idx.expType.isInt, in, "invalid array subscription")) {
              None
            } else {
              base.expType.base match {
                case _: ArrayType => Some(ArrayIdxExpression(base, idx))
                case _: ScalarType => Some(ScalarIdxExpression(base, idx))
              }
            }
          }
          case (Some(_), _) => {
            complain(in.getChild(0), "array expected")
            None
          }
          case _ => None
        }
      }
      case NAME => {
        checkNode(in, NAME, "NAME", 0)
        varmap.getVariable(in.getText) match {
          case Some(res) => Some(res)
          case None => {
            complain(in, "undeclared variable " + in.getText)
            None
          }
        }
      }
      case CALL => transformCallExpression(in);
    }
    res
  }

  private def transformScalarArraySubs(in: Tree): Option[ScalarIdxExpression] = {
    transformTerm(in) match {
      case Some(res: ScalarIdxExpression) => Some(res)
      case Some(_) => {
        complain(in, "scalar result expected")
        None
      }
      case None => None
    }
  }

  private def transformScalarStructSubs(in: Tree): Option[ScalarStructSubscription] = {
    transformTerm(in) match {
      case Some(res: ScalarStructSubscription) => Some(res)
      case Some(_) => {
        complain(in, "scalar result expected")
        None
      }
      case None => None
    }
  }

  private def transformScalarInit(in: Tree): Option[ScalarExpression] = {
    checkNode(in, SCALAR_INIT, "SCALAR_INIT", 1)
    transformScalarExpression(in.getChild(0))
  }

  private def transformScalarExpression(in: Tree): Option[ScalarExpression] = {
    val res = in.getType match {
      case TERNARY => transformTernaryExpression(in)
      case LOR | LAND => transformBooleanBinaryExpression(in)
      case BITOR | BITXOR | BITAND | BITLSHIFT | BITRSHIFT => transformBitBinaryExpression(in)
      case EQ | NEQ | GREATER | LESS | LEQ | GEQ => transformComparisonExpression(in)
      case PLUS | MINUS | DIV | MULT => transformMathBinaryExpression(in)
      case MOD => transformModExpression(in)
      case CAST => transformCastExpression(in)
      case SIZEOF => transformSizeofExpression(in)
      case LNOT => transformBooleanUnaryExpression(in)
      case UMINIS => transformMathUnaryExpression(in)
      case BITNEG => transformBitUnaryExpression(in)
      case CALL => transformCallExpression(in)
      case ARRAY_SUBS => transformScalarArraySubs(in)
      case STRUCT_SUBS => transformScalarStructSubs(in)
      case NAME => transformScalarVariable(in)
      case HEX_NUMBER | NUMBER => transformIntConstant(in)
      case FLOAT_NUMBER | DOUBLE_NUMBER => transformFloatConstant(in)
      case SCALAR_INIT => transformScalarInit(in)
      case TRUE => Some(BooleanConstant(Some(true)))
      case FALSE => Some(BooleanConstant(Some(false)))
      case PENCIL_MAYBE => Some(BooleanConstant(None))
      case _ => {
        ice(in, "unexpected scalar expression")
        None
      }
    }
    res
  }

  //Array init expression
  private def transformArrayInit(in: Tree, _type: ArrayType): Option[ArrayConstant] = {
    checkNode(in, ARRAY_INIT, "ARRAY_INIT")
    val raw_items: Seq[Option[Expression with Constant]] =
      (intWrapper(0) to (in.getChildCount - 1)).map(i => {
        val elem = in.getChild(i)
        (elem.getType, _type.base) match {
          case (ARRAY_INIT, _type: ArrayType) => transformArrayInit(elem, _type)
          case (NUMBER | HEX_NUMBER, _: IntegerType) => transformIntConstant(elem)
          case (FLOAT_NUMBER | DOUBLE_NUMBER, _: FloatType) => transformFloatConstant(elem)
          case (ARRAY_INIT | NUMBER | HEX_NUMBER | FLOAT_NUMBER | DOUBLE_NUMBER, _) => {
            complain(in, "invalid array init expression")
            None
          }
          case _ => ice(in, "invalid array init expression")
        }
      })
    val items = raw_items.filter(_.isDefined).map(_.get)
    if (items.size != raw_items.size) {
      None
    } else {
      _type.range match {
        case IntegerConstant(_, num) =>
          if (!check(num == items.size, in, "invalid array init expression")) {
            None
          } else {
            Some(ArrayConstant(_type, items))
          }
        case _ =>
          complain(in, "variable-sized object may not be initialized")
          None
      }
    }
  }

  //General expressions
  private def getExpType(in: Tree): Option[Type] = {
    (in.getType match {
      case STRUCT_SUBS | ARRAY_SUBS | NAME => transformTerm(in)
      case _ => transformScalarExpression(in)
    }) match {
      case Some(exp) => Some(exp.expType)
      case None => None
    }
  }

  //Operations
  private def transformIndependentPragma(in: Tree): Option[IndependentLoop] = {
    checkNode(in, INDEPENDENT, "INDEPENDENT")
    in.getChildCount match {
      case 0 => Some(new IndependentLoop(None))
      case 1 => {
        val names = in.getChild(0)
        checkNode(names, NAMES, "NAMES")
        val labels = (intWrapper(0) to (names.getChildCount - 1)).map(i => {
          val name = names.getChild(i)
          checkNode(name, NAME, "NAME")
          val op = varmap.getLabel(name.getText)
          op match {
            case Some(op) => Some(op)
            case None => {
              complain(name, "unknown label " + name.getText)
              None
            }
          }
        }).filter(_.isDefined).map(_.get)
        Some(new IndependentLoop(Some(labels)))
      }
      case _ => ice(in, "unexpected number of children for independent node")
    }
  }

  private def transformRange(in: Tree): Option[Range] = {
    checkNode(in, RANGE, "RANGE", 3)
    val ninit = in.getChild(0)
    val nguard = in.getChild(1)
    val nstep = in.getChild(2)

    checkNode(ninit, INITIAL, "INITIAL", 3)
    checkNode(nguard, GUARD, "GUARD", 3)
    checkNode(nstep, STEP, "STEP", 1)

    val nname = ninit.getChild(0)
    checkNode(nname, NAME, "NAME", 0)
    val name = nname.getText
    val init = transformScalarExpression(ninit.getChild(1))
    val iter = transformBaseType(ninit.getChild(2)) match {
      case Some(st: IntegerType) => {
        val res = ScalarVariableDef(st, name, None)
        varmap.addVariable(res, name)
        Some(new ScalarVariableRef(res))
      }
      case _ => {
        complain(ninit.getChild(2), "invalid iterator type")
        None
      }
    }

    val step = nstep.getChild(0)
    val siter = step.getChild(0)
    val svalue = step.getChild(1)
    checkNode(siter, NAME, "NAME", 0)
    if (!siter.getText.equals(name)) {
      complain(siter, "invalid variable for step (expected " + name + " found " + siter.getText + ")")
    }

    val tmp = transformIntConstant(svalue).get.value
    assert(step.getType == MOVPLUS || step.getType == MOVMINUS, step, "invalid step node for range")
    val rstep = if (step.getType == MOVPLUS) tmp else tmp * -1

    val giter = nguard.getChild(0)
    checkNode(giter, NAME, "NAME", 0)
    if (!giter.getText.equals(name)) {
      complain(giter, "invalid variable for guard (expected " + name + " found " + giter.getText + ")")
    }

    val upper = transformScalarExpression(nguard.getChild(2)) match {
      case None => {
        None
      }
      case Some(exp) => {
        if (check(exp.expType.isInt, nguard, "invalid guard condition")) {
          nguard.getChild(1).getType match {
            case GREATER =>
              if (rstep < 0) {
                Some(PlusExpression(exp, Constants.Integer32Constant1))
              } else {
                complain(nguard, "invalid range for `for' loop")
                None
              }
            case LESS =>
              if (rstep > 0) {
                Some(MinusExpression(exp, Constants.Integer32Constant1))
              } else {
                complain(nguard, "invalid range for `for' loop")
                None
              }
            case LEQ =>
              if (rstep > 0) {
                Some(exp)
              } else {
                complain(nguard, "invalid range for `for' loop")
                None
              }
            case GEQ =>
              if (rstep < 0) {
                Some(exp)
              } else {
                complain(nguard, "invalid range for `for' loop")
                None
              }
            case _ => ice(nguard.getChild(1), "unexpected range guard node")
          }
        } else {
          None
        }
      }
    }
    (iter, init, upper) match {
      case (Some(iter), Some(init), Some(upper)) => Some(new Range(iter, init, upper, IntegerConstant(IntegerConstantType, rstep)))
      case _ => None
    }
  }

  private def transformFor(in: Tree, access: Boolean): Option[ForOperation] = {
    checkNode(in, FOR, "FOR", 3)
    val nattrs = in.getChild(0)
    val nrange = in.getChild(1)
    val nbody = in.getChild(2)
    varmap.push
    val range = transformRange(nrange)
    varmap.push
    val ops = transformBlock(nbody, access, false)
    val attrs = (intWrapper(0) to (nattrs.getChildCount - 1)).map(i => {
      val attr = nattrs.getChild(i)
      attr.getType match {
        case IVDEP => Some(IvdepLoop)
        case INDEPENDENT => transformIndependentPragma(attr)
      }
    }).filter(_.isDefined).map(_.get)
    varmap.pop
    varmap.pop
    range match {
      case Some(range) => Some(new ForOperation(attrs, range, ops))
      case _ => {
        None
      }
    }
  }

  private def transformIf(in: Tree, access: Boolean): Option[IfOperation] = {
    checkNode(in, IF, "IF")
    val guard = transformScalarExpression(in.getChild(0))
    val body = transformBlock(in.getChild(1), access)
    guard match {
      case Some(guard) => {
        if (check(guard.expType.isBoolean, in, "invalid guard expression")) {
          in.getChildCount match {
            case 2 => Some(new IfOperation(guard, body, None))
            case 3 => Some(new IfOperation(guard, body, Some(transformBlock(in.getChild(2), access))))
          }
        } else {
          None
        }
      }
      case _ => {
        None
      }
    }
  }

  private def transformWhile(in: Tree, access: Boolean): Option[WhileOperation] = {
    checkNode(in, WHILE, "WHILE", 2)
    val guard = transformScalarExpression(in.getChild(0))
    val body = transformBlock(in.getChild(1), access)
    guard match {
      case Some(guard) =>
        if (check(guard.expType.isBoolean, in, "invalid guard expression")) {
          Some(new WhileOperation(guard, body))
        } else {
          None
        }
      case _ => {
        None
      }
    }
  }

  private def transformBreak(in: Tree, access: Boolean): Option[BreakOperation] = {
    checkNode(in, BREAK, "BREAK", 0)
    Some(new BreakOperation)
  }

  private def transformContinue(in: Tree, access: Boolean): Option[ContinueOperation] = {
    checkNode(in, CONTINUE, "CONTINUE", 0)
    Some(new ContinueOperation)
  }

  private def transformReturn(in: Tree, access: Boolean): Option[ReturnOperation] = {
    checkNode(in, RETURN, "RETURN")
    in.getChildCount match {
      case 0 =>
        if (current_function_type.isDefined) {
          check(current_function_type.get == NopType, in, "invalid return operation (expression required)")
        }
        Some(new ReturnOperation(None))
      case 1 => {
        transformScalarExpression(in.getChild(0)) match {
          case Some(exp) =>
            if (current_function_type.isDefined) {
              if (check(exp.expType.convertible(current_function_type.get), in, "invalid return operation (no expression expected)")) {
                Some(new ReturnOperation(Some(convertScalar(exp, current_function_type.get))))
              } else {
                None
              }
            } else {
              None
            }
          case None => {
            None
          }
        }
      }
      case _ => ice(in, "unexpected number of children for return node")
    }
  }

  private def transformModify(in: Tree, access: Boolean): Option[Operation] = {
    checkNode(in, MODIFY, "MODIFY", 1)
    val node = in.getChild(0)
    val exp = node.getType match {
      case MOV => None
      case MOVPLUS | PLUSPLUS => Some(PlusExpression)
      case MOVMINUS | MINUSMINUS => Some(MinusExpression)
      case MOVMOD => Some(ModExpression)
      case MOVMULT => Some(MultExpression)
      case MOVDIV => Some(DivExpression)
      case MOVLXOR => Some(BitXorExpression)
      case MOVLAND => Some(BitAndExpression)
      case MOVLOR => Some(BitOrExpression)
      case MOVLSHIFT => Some(BitLShiftExpression)
      case MOVRSHIFT => Some(BitRShiftExpression)
      case _ => ice(node, "unexpected modify node")
    }
    val lvalue = transformScalarExpression(node.getChild(0)) match {
      case Some(exp:ScalarExpression with LValue) if !exp.expType.const => Some(exp)
      case Some(_) => {
        complain(node.getChild(0), "invalid lvalue")
        None
      }
      case None => None
    }
    val rvalue = node.getType match {
      case MOV => transformScalarExpression(node.getChild(1))
      case PLUSPLUS | MINUSMINUS => Some(Constants.Integer32Constant1)
      case _ => transformScalarExpression(node.getChild(1))
    }
    (lvalue, rvalue, exp) match {
      case (Some(lvalue: ScalarExpression), Some(rvalue: ScalarExpression), None) =>
        if (rvalue.expType.convertible(lvalue.expType)) {
          Some(new AssignmentOperation(lvalue, convertScalar(rvalue, lvalue.expType)))
        } else {
          complain(in, "invalid assignment")
          None
        }
      case (Some(lvalue: ScalarExpression), Some(rvalue: ScalarExpression), Some(op)) =>
        if (rvalue.expType.convertible(lvalue.expType)) {
          val operand1 = transformScalarExpression(node.getChild(0)).get
          Some(new AssignmentOperation(lvalue, convertScalar(op(operand1, convertScalar(rvalue, lvalue.expType)), lvalue.expType)))
        } else {
          complain(in, "invalid assignment")
          None
        }
      case _ => None
    }
  }

  private def transformExpressionStatement(in: Tree, access: Boolean): Option[Operation] = {
    checkNode(in, EXPRESSION_STATEMENT, "EXPRESSION_STATEMENT", 1)
    val op = in.getChild(0)
    op.getType match {
      case CALL =>
        checkNode(op, CALL, "CALL")
        transformCallExpression(in.getChild(0)) match {
          case Some(exp: CallExpression) => Some(new CallOperation(exp))
          case Some(_) =>
            complain(in, "invalid call statement")
            None
          case None => None
        }
      case PENCIL_USE =>
        checkNode(op, PENCIL_USE, "PENCIL_USE", 1)
        transformTerm(op.getChild(0)) match {
          case None => None
          case Some(term) => Some(new USEOperation(term))
        }
      case PENCIL_DEF =>
        checkNode(op, PENCIL_DEF, "PENCIL_DEF", 1)
        transformTerm(op.getChild(0)) match {
          case None => None
          case Some(term) => Some(new DEFOperation(term))
        }
      case PENCIL_KILL =>
        checkNode(op, PENCIL_KILL, "PENCIL_KILL", 1)
        transformTerm(op.getChild(0)) match {
          case None => None
          case Some(term) => Some(new KillOperation(term))
        }
      case PENCIL_ASSUME =>
        checkNode(op, PENCIL_ASSUME, "PENCIL_ASSUME", 1)
        transformScalarExpression(op.getChild(0)) match {
          case None => None
          case Some(exp) if exp.expType.isBoolean => Some(new AssumeOperation(exp))
          case Some(_) =>
            complain(op, "boolean expression expected")
            None
        }
    }

  }

  private def transformStatement(in: Tree, access: Boolean): Option[Operation] = {
    val (lstmt, access_block) = if (in.getType == ANNOTATED_STATEMENT) {
      val block = Some(transformBlock(in.getChild(0), true))
      if (check(!access, in, "nested access blocks are not allowed")) {
        (in.getChild(1), None)
      } else {
        (in.getChild(1), block)
      }
    } else {
      (in, None)
    }

    val (stmt, label) = if (lstmt.getType == LABEL) {
      (lstmt.getChild(1), Some(lstmt.getChild(0).getText))
    } else {
      (lstmt, None)
    }

    (stmt.getType match {
      case FOR => transformFor(stmt, access)
      case WHILE => transformWhile(stmt, access)
      case IF => transformIf(stmt, access)
      case BREAK => transformBreak(stmt, access)
      case CONTINUE => transformContinue(stmt, access)
      case RETURN => transformReturn(stmt, access)
      case EMPTY_STATEMENT => None
      case BLOCK => Some(transformBlock(stmt, access))
      case EXPRESSION_STATEMENT => transformExpressionStatement(stmt, access)
      case DECL => transformCallOrDecln(in)
      case DECL_AND_INIT =>
        transformVariableDeclarationStmt(in)
      case MODIFY => transformModify(stmt, access)
    }) match {
      case Some(op) =>
        op.access = access_block
        label match {
          case Some(lname) =>
            check(varmap.addLabel(op, lname), in, "label" + lname + " has already been declared")
          case None =>
        }
        Some(op)
      case None => None
    }
  }

  private def transformScop(in: Tree, access: Boolean) = {
    val (statement, scop) = (if (in.getType == SCOP) {
        checkNode(in, SCOP, "SCOP", 1)
        check(!access, in, "pragma scop cannot be used inside access blocks")
        (in.getChild(0), true)
      } else {(in, false)})
    val ret = transformStatement(statement, access)
    ret match {
      case Some(statement) => statement.scop = scop
      case None =>
    }
    ret
  }

  private def transformBlock(in: Tree, access: Boolean, push: Boolean = true): BlockOperation = {
    checkNode(in, BLOCK, "BLOCK")
    if (push) varmap.push
    val res = (intWrapper(0) to (in.getChildCount - 1)).map(i => {
      transformScop(in.getChild(i), access)
    }).filter(_.isDefined).map(_.get)
    if (push) varmap.pop
    new BlockOperation(res)
  }

  private def transformFunction(in: Tree, static: Boolean): Option[Function] = {
    varmap.push
    calls.clear
    checkNode(in, FUNCTION, "FUNCTION", 5)
    var lerror = false

    val ntype = in.getChild(0)
    val nname = in.getChild(1)
    val nargs = in.getChild(2)
    val nattrs = in.getChild(3)
    val nbody = in.getChild(4)

    checkNode(nname, NAME, "NAME", 0)
    checkNode(nargs, FUNCTION_ARGS, "FUNCTION_ARGS")
    checkNode(nattrs, FUNCTION_ATTRS, "FUNCTION_ATTRS")

    val _type = if (ntype.getType == TYPE_VOID) {
      Some(NopType)
    } else {
      checkNode(ntype, TYPE, "TYPE")
      transformType(ntype) match {
        case Some(st: ScalarType) => Some(st)
        case _ => {
          complain(ntype, "invalid function type")
          lerror = true
          None
        }
      }
    }
    current_function_type = _type
    val name = nname.getText
    val params = (0 to (nargs.getChildCount - 1)).map(i =>
      transformVariableDeclaration(nargs.getChild(i)) match {
        case Some(decl) => Some(decl)
        case None => {
          complain(in, "invalid function parameters")
          lerror = true
          None
        }
      }).filter(_.isDefined).map(_.get)
    var const = false
    var access: Option[Function] = None
    var local = false
    for (i <- 0 to (nattrs.getChildCount - 1)) {
      val attr = nattrs.getChild(i)
      attr.getType match {
        case CONST_FUNCTION => const = true
        case ACCESS_FUNCTION => {
          fmap.get(attr.getChild(0).getText) match {
            case Some(function) => access = Some(function)
            case None => {
              lerror = true
              complain(in, "invalid function attributes")
            }
          }
        }
        case TYPE_STATIC => local = true
        case _ => ice(attr, "unexpected function attribute")
      }
    }
    val fbody: Option[BlockOperation] = if (nbody.getType == EMPTY_BODY) {
      None
    } else {
      Some(transformBlock(nbody, false))
    }
    varmap.pop
    if (lerror) None else {
      val existing = fmap.get(nname.getText)
      existing match {
        case None =>
          val res = new Function(name, params, fbody, _type.get, access, const, local || static, false, local)
          calls.foreach(f => callGraph.addCall(res, f))
          fmap += ((nname.getText, res))
          Some(res)
        case Some(function) => {
          if (function.ops.isDefined) {
            complain(in, "Function " + nname.getText + " has already been declared")
            None
          } else {
            val correct = compatibleWithFunction(function, _type.get, params)
            if (check(correct, in, "function declaration conflicts with previous declaration")) {
              function.ops = fbody
              function.params = params
              calls.foreach(f => callGraph.addCall(function, f))
            }
            None
          }
        }
      }
    }
  }

  //Constants/Variables

  private def checkRestrict(in: Tree): Boolean = {
    val attrs = in.getChild(1).getChild(1)
    for (i <- 0 to attrs.getChildCount() - 1) {
      if (attrs.getChild(i).getText() == "restrict") {
        return true
      }
    }
    return false
  }

  /** Helper function to process array suffix* [..][..]...
    * start, stop and step indicate which children of 'in' are array-suffix*
    * baseType is the initial type and [..] evolves the type to 'ArrayType of type'
    */
  private def processArraySuffix(in : Tree, start: Int, stop: Int, step: Int, baseType: Option[Type]): Option[Type] = {
    (start to stop by step).foldLeft(baseType)((tmpType, idx) => {
      val size = transformScalarExpression(in.getChild(idx).getChild(0))
      (tmpType, size) match {
        case (Some(_type), Some(_size)) => {
          if (check(_size.expType.isInt, in.getChild(idx), "invalid array size expression")) {
            Some(ArrayType(_type, _size))
          } else None
        }
        case _ => None
      }
    })
  }

  /** Receives baseType and declarator tree. Returns the Name and Type as Options.
    * BNF{ declarator  : pointer* direct_declarator -> ^(DECLARATOR pointer* direct_declarator) }
    */
  private def transformDeclarator(baseType: Option[Type], in: Tree): (Option[String], Option[Type]) = {
    checkNode(in, DECLARATOR, "DECLARATOR")

    val type1 = baseType match {
      case Some(type1)  =>
        Some((type1 /: List.range(1, in.getChildCount))((tmpType, _) => PointerType(tmpType)))   /*Some(Pointer(..Pointer(baseType)..)) */
      case _ => None
    }
    transformDirectDeclarator(type1, in.getChild(in.getChildCount - 1))  /*last child is ddeclr */
  }

  /** Handles both direct and indirect declarator
    * DIRECT_DECLARATOR  : NAME array_type_suffix*
    * INDIRECT_DECLARATOR: '(' declarator ')' array_type_suffix*
    */
  private def transformDirectDeclarator(baseType: Option[Type], in: Tree): (Option[String], Option[Type]) = {
    if (in.getType == DIRECT_DECLARATOR) {
      checkNode(in.getChild(0), NAME, "NAME")
      val name = in.getChild(0).getText
      (Some(name), processArraySuffix(in, (in.getChildCount-1), 1, -1, baseType))
    }
    else if (in.getType == INDIRECT_DECLARATOR) {
      val type1 = processArraySuffix(in, (in.getChildCount-1), 1, -1, baseType)
      transformDeclarator(type1, in.getChild(0))
    }
    else (None,None)
  }

  /** Handles BNF 'variable_decl_int: base_type declarator -> ^(DECL base_type declarator)'
    */
  private def transformVariableDeclarationInt(decl: Tree, init: Option[Tree]): Option[Variable] = {
    checkNode(decl, DECL, "DECL", 2)
    val baseType = decl.getChild(0)
    val declr    = decl.getChild(1)

    val _type1 = transformBaseType( baseType )
    val (name, _type2) = transformDeclarator( _type1, declr )    /*_type2 evolves from _typ1: e.g. Array(PointerType(Array( _type1)))*/

    (name,_type2) match {
      case (Some(name), Some(st: ScalarType)) => {
        init match {
          case Some(sinit) => {
            transformScalarExpression(sinit) match {
              case Some(init) =>
                if (check(init.expType.convertible(st), decl, "invalid initializer found in declaration")) {
                  Some(ScalarVariableDef(st, name, Some(convertScalar(init, st))))
                } else {
                  None
                }
              case None => {
                None
              }
            }
          }
          case None => {
            Some(ScalarVariableDef(st, name, None))
          }
        }
      }
      case (Some(name), Some(at: ArrayType)) => {
        init match {
          case Some(sinit) => {
            transformArrayInit(sinit, at) match {
              case Some(init) =>
                Some(ArrayVariableDef(at, name, true, Some(init)))
              case None => {
                None
              }
            }
          }
          case None => {
            Some(ArrayVariableDef(at, name, true, None))
          }
        }
      }
      case (_, Some(_type)) => Checkable.ice(_type, "unexpected type found in declaration")
      case (_,_) => None
    }
  }

  /** Handles BNF: variable_decl_init: variable_decl_int MOV init_expression -> ^(DECL_AND_INIT variable_decl_int init_expression)
    */
  private def transformVariableDeclarationStmt(in: Tree) = {
    checkNode(in, DECL_AND_INIT, "DECL_AND_INIT", 2)
    val decl = in.getChild(0)
    val init = in.getChild(1)

    val (name, type1) = transformVariableDeclarationInt(decl, Some(init)) match {
      case Some(st: ScalarVariableDef) => (Some(st.name), Some(st))
      case Some(at: ArrayVariableDef ) => (Some(at.name), Some(at))
      case _ => (None, None)
    }

    (name, type1 ) match {
      case (_, None) => None

      case (Some(name), Some(av: ArrayVariableDef)) =>
        check(varmap.addVariable(av, name), decl, "variable " + name + " has already been declared")
        Some(new ArrayDeclOperation(av))

      case (Some(name), Some(sv: ScalarVariableDef)) => {
        val variable = sv.copy(init = None, expType = sv.expType.updateConst((false)))
        if (check(varmap.addVariable(variable, name, sv.expType.const), decl, "variable " + name + " has already been declared")) {
          Some(new AssignmentOperation(new ScalarVariableRef(variable), sv.init.get))
        } else {
          None
        }
      }
      case (_,Some(op)) => Checkable.ice(op, "unexpected variable")
    }
  }

  private def transformVariableDeclaration(in: Tree) = {
    val (decl, init) = if (in.getType == DECL_AND_INIT) {
      checkNode(in, DECL_AND_INIT, "DECL_AND_INIT", 2)
      (in.getChild(0), Some(in.getChild(1)))
    } else {
      (in, None)
    }

    val (name, type1) = transformVariableDeclarationInt(decl, init) match {
      case Some(st: ScalarVariableDef) => (Some(st.name), Some(st))
      case Some(at: ArrayVariableDef ) => (Some(at.name), Some(at))
      case _ => (None, None)
    }

    (name, type1) match {
      case (Some(name), Some(variable)) =>
        check(varmap.addVariable(variable, name), decl, "variable " + name + " has already been declared")
        Some(variable)
      case (_,_)  => None
    }
  }

  private def transformGlobalConstant(in: Tree) = {
    checkNode(in, DECL_AND_INIT, "DECL_AND_INIT", 2)
    transformVariableDeclaration(in) match {
      case None =>
      case Some(decl) => {
        if (!decl.expType.const) {
          complain(in, "global variables must be constants")
        }
        consts += decl
      }
    }
  }

  //Types.

  private def transformBuiltInType(in: Tree): Option[ScalarType] = {
    checkNode(in, BUILTIN_TYPE, "BUILTIN_TYPE")
    var isConst = false
    var isLong = false
    var isShort = false

    var isSigned = false
    var isUnsigned = false

    var isDouble = 0
    var isFloat = 0
    var isHalf = 0
    var isInt = 0
    var isVoid = 0
    var isBoolean = 0
    var isChar = false

    for (i <- intWrapper(0) to (in.getChildCount - 1)) {
      val attr = in.getChild(i).getType
      attr match {
        case TYPE_CONST => isConst = true

        case TYPE_DOUBLE => isDouble = isDouble + 1
        case TYPE_FLOAT => isFloat = isFloat + 1
        case TYPE_INT => isInt = isInt + 1
        case TYPE_BOOL => isBoolean = isBoolean + 1
        case TYPE_VOID => isVoid = isVoid + 1
        case TYPE_CHAR =>
          isInt = isInt + 1
          isChar = true

        case TYPE_HALF => isHalf = isHalf + 1
        case TYPE_LONG => isLong = true
        case TYPE_SHORT => isShort = true

        case TYPE_SIGNED => isSigned = true
        case TYPE_UNSIGNED => isUnsigned = true
      }
    }
    (isDouble, isFloat, isHalf, isInt, isVoid, isBoolean) match {
      case (1, 0, 0, 0, 0, 0) =>
        if (isLong || isShort || isSigned || isUnsigned) {
          complain(in, "invalid basic type specification")
          None
        } else {
          Some(FloatType(64, isConst)) //double
        }
      case (0, 1, 0, 0, 0, 0) =>
        if (isLong || isShort || isSigned || isUnsigned) {
          complain(in, "invalid basic type specification")
          None
        } else {
          Some(FloatType(32, isConst)) //float
        }
      case (0, 0, 1, 0, 0, 0) =>
        if (isLong || isShort || isSigned || isUnsigned) {
          complain(in, "invalid basic type specification")
          None
        } else {
          Some(FloatType(16, isConst)) //half
        }

      case (0, 0, 0, 0, 0, 1) =>
        if (isLong || isShort || isSigned || isUnsigned) {
          complain(in, "invalid basic type specification")
          None
        } else {
          Some(BooleanType(isConst)) //bool
        }
      case (0, 0, 0, 0, 0, 0) | (0, 0, 0, 1, 0, 0) => {
        (isLong, isShort, isChar, isSigned, isUnsigned) match {
          case (false, false, false, _, false) => Some(IntegerType(true, 32, isConst)) //int
          case (false, false, false, false, true) => Some(IntegerType(false, 32, isConst)) //unsigned

          case (false, true, false, _, false) => Some(IntegerType(true, 16, isConst)) //short
          case (false, true, false, false, true) => Some(IntegerType(false, 16, isConst)) //unsigned short

          case (true, false, false, _, false) => Some(IntegerType(true, 64, isConst)) //long
          case (true, false, false, false, true) => Some(IntegerType(false, 64, isConst)) //unsigned long

          case (false, false, true, _, false) => Some(IntegerType(true, 8, isConst)) //char
          case (false, false, true, false, true) => Some(IntegerType(false, 8, isConst)) //unsigned char
          case _ => {
            complain(in, "invalid basic type specification")
            None
          }
        }
      }
      case _ => {
        complain(in, "invalid basic type specification")
        None
      }
    }
  }

  private def getTypeByName(in: Tree, struct: Boolean): Option[Type] = {
    val name = in.getChild(0).getText()
    val _type = if (struct) stypemap.get(name) else typemap.get(name)

    _type match {
      case None => {
        complain(in.getChild(0), "unknown type: " + name)
        None
      }
      case Some(ptype) => {
        if (in.getChildCount > 1) {
          val child = in.getChild(1)
          if (in.getChildCount != 2 || child.getType != TYPE_CONST) {
            complain(in, "invalid type attributes")
            None
          } else {
            Some(ptype.updateConst(true))
          }
        } else {
          Some(_type.get)
        }
      }
    }

  }

  private def transformUserType(in: Tree): Option[Type] = {
    checkNode(in, USER_TYPE, "USER_TYPE")
    getTypeByName(in, false)
  }

  private def transformUserStructType(in: Tree): Option[Type] = {
    checkNode(in, USER_STRUCT_TYPE, "USER_STRUCT_TYPE")
    getTypeByName(in, true)
  }

  private def transformBaseType(in: Tree): Option[Type] = {
    in.getType match {
      case BUILTIN_TYPE => transformBuiltInType(in)
      case USER_TYPE => transformUserType(in)
      case USER_STRUCT_TYPE => transformUserStructType(in)
      case INLINE_STRUCT_TYPE => Some(registerStructType(in.getChild(0)))
    }
  }

  private def transformType(in: Tree): Option[Type] = {
    checkNode(in, TYPE, "TYPE")
    assert(in.getChildCount > 0, in, "insufficient children count")
    val base: Option[Type] = transformBaseType(in.getChild(0))
    (intWrapper(in.getChildCount - 1) to 1 by -1).foldLeft(base)((base, idx) => {
      val size = transformScalarExpression(in.getChild(idx).getChild(0))
      (base, size) match {
        case (Some(_type), Some(_size)) => {
          if (check(_size.expType.isInt, in.getChild(idx), "invalid size expression")) {
            Some(ArrayType(_type, _size))
          } else {
            None
          }
        }
        case _ => {
          None
        }
      }
    })
  }

  private var anonStructCounter = 0

  private def registerStructType(in: Tree) = {
    checkNode(in, STRUCT, "STRUCT")

    val fields = in.getChildCount() match {
      case 1 => in.getChild(0)
      case 2 => in.getChild(1)
      case num => ice(in, "Unexpected number of children (1 or 2 expected, but " + num + " found)")
    }

    checkNode(fields, STRUCT_FIELDS, "STRUCT_FIELDS")

    val names = Set[String]()
    val fdecl = (0 to (fields.getChildCount - 1)).map(i =>
      {
        val field = fields.getChild(i)                                    /*field is ^(DECL base_type declarator); */
        checkNode(field, DECL, "DECL", 2)

        val _type = transformBaseType( field.getChild(0) )
        val (fname, ftype) = transformDeclarator( _type, field.getChild(1) )

        val res = (fname, ftype) match {
          case (Some(aname),Some(atype)) =>
            if (check(!names.contains(aname), field, "field " + aname + " already exists")) {
              (aname, Some(atype))
            } else {
              (aname, None)
            }
          case (Some(aname), None) => (aname, None)
          case error => Checkable.ice(error, "unexpected input")
        }

        names.add(res._1)
        res
      }).filter(_._2.isDefined).map(pair => (pair._1, pair._2.get))


    val _type = if (in.getChildCount() == 2) {
      val name = in.getChild(0)
      checkNode(name, NAME, "NAME", 0)
      val _type = StructType(fdecl, false, name.getText)
      stypemap += ((name.getText, _type))
      _type
    } else {
      anonStructCounter = anonStructCounter + 1
      StructType(fdecl, false, "_struct_type_" + anonStructCounter)
    }
    if (!structnames.add(_type.name)) {
      complain(in, "Struct " + _type.name + " has already been declared")
    } else {
      structtypes.append(_type)
    }
    _type
  }

  private def registerTypedef(in: Tree) = {
    checkNode(in, TYPEDEF, "TYPEDEF", 2)
    val name = in.getChild(0)
    val _type = in.getChild(1)
    checkNode(name, NAME, "NAME", 0)
    checkNode(_type, TYPE, "TYPE")
    val ptype = transformType(_type)
    ptype match {
      case Some(ptype) => typemap += ((name.getText, ptype))
      case None =>
    }
  }

  private def transformTopDecl(in: Tree, static: Boolean): Option[Function] = {
    in.getType match {
      case FUNCTION => transformFunction(in, static)
      case STRUCT =>
        registerStructType(in); None
      case TYPEDEF =>
        registerTypedef(in); None
      case DECL_AND_INIT =>
        transformGlobalConstant(in);
        None
      case _ => ice(in, "Unexpected top level construction"); None
    }
  }

  def transformProgram(in: program_return, debug: Boolean, static: Boolean): Option[Program] = {
    val tree = in.getTree.asInstanceOf[Tree]
    checkNode(tree, PROGRAM, "PROGRAM")
    if (debug) {
      printTree(tree, "")
    }
    varmap.push
    val functions = (intWrapper(0) to (tree.getChildCount - 1)).map(num => transformTopDecl(tree.getChild(num), static))
    varmap.pop
    callGraph.getRecursion match {
      case None =>
      case Some(function) =>
        complain(tree, "Possible recursion detected for function " + function.name)
    }
    if (!error) {
      val res = new Program(functions.filter(_.isDefined).map(_.get), structtypes.toList, consts.toList)
      Checkable.walkProgram(res)
      Some(res)
    } else {
      None
    }
  }

  /** Returns 'NAME' of potential function, if base is purely an undefined name.
    */
  private def checkBaseUnknown(base: Tree): Option[Tree] = {
    base.getType match {
      case USER_TYPE => {
        if (base.getChildCount == 1) { /*calls cannot have type attributes info */
          val nameTree = base.getChild(0)
          checkNode(nameTree, NAME, "NAME")
          val name = nameTree.getText()

          typemap.get(name) match {
            case None => Some(nameTree) /*name is not a type, so could be a function name*/
            case _ => None
          }
        } else None
      }
      case _ => None
    }
  }

  /** Returns 'NAME' of potential arg if decl is purely a name (wrapped in'()' but no pointers or array-suffix).
    */
  private def checkDeclrSimple(declr: Tree, castCounter: Int): Option[Tree] =
    {
      checkNode(declr, DECLARATOR, "DECLARATOR")
      if (declr.getChildCount != 1) {
        None; /*declarator has pointers */
      } else {
        checkDirectDeclrSimple(declr.getChild(0), castCounter)
      }
    }

  private def checkDirectDeclrSimple(ddeclr: Tree, castCounter: Int): Option[Tree] = {
    if (ddeclr.getType == DIRECT_DECLARATOR) {
      if (ddeclr.getChildCount == 1) {
        val name = ddeclr.getChild(0)
        checkNode(name, NAME, "NAME")
        if (castCounter > 0)
          Some(name)
        else
          None /* e.g. 'foo a' is not a valid call. cast'( )' is needed*/
      } else
        None /*has array suffix*/
    } else if (ddeclr.getType == INDIRECT_DECLARATOR) {
      if (ddeclr.getChildCount == 1)
        checkDeclrSimple(ddeclr.getChild(0), castCounter + 1)
      else
        None /*contains array suffix*/
    } else
      None
  }

  private def transformCallOrDecln(in: Tree): Option[Operation] = {
    checkNode(in, DECL, "DECL", 2)
    val base = in.getChild(0)
    val declr = in.getChild(1)

    val funcTree = checkBaseUnknown(base)
    val argTree = checkDeclrSimple(declr, 0)

    (funcTree, argTree) match {
      case (Some(fname), Some(arg)) => changeDeclnToCall(fname, arg, in)

      case (_, _) =>
        /* treat as a normal declaration */
        transformVariableDeclaration(in) match {
          case Some(variable: ArrayVariableDef) => Some(new ArrayDeclOperation(variable))
          case _ => None
        }
    }
  }

  private def changeDeclnToCall(fname: Tree, arg: Tree, fullTree: Tree): Option[Operation] = {
    varmap.getVariable(arg.getText) match {
      case None => {
        complain(fullTree, " argument is an undeclared variable: " + arg.getText)
        None
      }
      case Some(argvar) => {
        fmap.get(fname.getText) match { /*find function using function name */
          case Some(function) => {
            if (!check(function.params.size == 1, fullTree, "invalid number of function arguments")) {
              None
            } else {
              val variable = function.params(0)
              if (!check(argvar.expType.convertible(variable.expType), fullTree, "invalid function argument for " +
                variable.name + " (" + argvar.expType + " -> " + variable.expType + ")")) {
                None
              } else {
                calls += function
                val argvars = Seq(argvar)
                Some(new CallOperation(new CallExpression(function, argvars)))
              }
            }
          }
          case None =>
            complain(fname, "Function " + fname.getText + " undeclared")
            None
        }
      }
    }
  }
}

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

/** Base type for every PENCIL type.  */
abstract class Type(val const: Boolean) {

  /**
   * Check whether this type can be converted to type t.
   * @param t - Type to convert to.
   * @return <code>true</code> if this type can be converted to t, <code>false</code> otherwise.
   */
  def convertible(t: Type): Boolean = t == this

  /**
   * Check whether this type is the same (except constant qualifier), as the type t.
   * See below for more details.
   * @param t - Type to compare to.
   * @return <code>true</code> if this type is the same, as t, <code>false</code> otherwise.
   */
  def compatible(t: Type): Boolean = t == this

  /**
   * Create a new type by setting constant qualifier of the this type to specific value.
   * @param in - New value of the constant qualifier.
   * @return New type.
   */
  def updateConst(in: Boolean): Type

  /**
   * <code>true</code> for integer types.
   */
  val isInt = false

  /**
   * <code>true</code> for boolean types.
   */
  val isBoolean = false

  /**
   * <code>true</code> for numeric (integer and floating point) types.
   */
  val isNumeric = false

  /**
   * <code>true</code> for scalar types.
   */
  val isScalar = false
}

/** Base class for scalar types.  */
abstract class ScalarType(uconst: Boolean) extends Type(uconst) {
  def updateConst(in: Boolean): ScalarType
  override val isScalar = true
}

abstract class NumericType(uconst: Boolean) extends ScalarType(uconst) {
  override val isNumeric = true
  override def convertible(t: Type) = t.isNumeric || t.isBoolean
}

/**
 * Boolean type representation.
 *
 * PENCIL boolean can only be converted to boolean (not to integer and/or floating point)
 * and can be used as boolean type only (is not compatible to non-boolean types).
 *
 */
case class BooleanType(uconst: Boolean) extends ScalarType(uconst) {
  override def convertible(t: Type) = t.isBoolean
  override def compatible(t: Type) = t.isBoolean
  def updateConst(in: Boolean) = this.copy(uconst = in)
  override val isBoolean = true
}

/**
 * Pointer type representation.
 *
 * PENCIL pointer cannot be converted to any other type
 * and cannot  be used as anywhere currently (future extension)
 *
 */
case class PointerType(base: Type) extends ScalarType(false) {

  override def convertible(t: Type): Boolean =  t match {
      case  PointerType( _ ) => true
      case _                => false
  }

  override def compatible(t: Type): Boolean = t match {
      case PointerType( _ ) => true
      case _                => false
  }

  def updateConst(in: Boolean) = this
}


/**
 * Void type representation.
 *
 * Void type can only be used as a function return type.
 */
case object NopType extends ScalarType(false) {
  def updateConst(in: Boolean) = this
}

/**
 * Floating point type representation.
 *
 * The concrete type is defined by the number of bits required for type representation.
 * Valid numbers are 32 for float, and 64 for double.
 *
 * A float type can be converted to any numeric type.
 *
 */
case class FloatType(bits: Int, uconst: Boolean) extends NumericType(uconst) {
  override def compatible(t: Type) = {
    t match {
      /* Floating point type is defined by the number of bits required for representation.  */
      case FloatType(nbits, _) => nbits == bits
      case _ => false
    }
  }
  def updateConst(in: Boolean) = this.copy(uconst = in)
}

/**
 * Integer (signed and unsigned) type representation.
 *
 * The concrete type is defined by the number of bits required for type representation.
 * Valid numbers are 8 for byte, 16 for short, 32 for int, and 64 for long.
 *
 * An integer type can be converted to any numeric type.
 *
 */
case class IntegerType(signed: Boolean, bits: Int, uconst: Boolean) extends NumericType(uconst) {
  override val isInt = true

  override def compatible(t: Type) = {
    t match {
      case IntegerType(nsigned, nbits, _) => nbits == bits
      case _ => false
    }
  }
  def updateConst(in: Boolean) = this.copy(uconst = in)
}

/**
 * Structural type representation.
 *
 * A structural type is represented as an ordered sequence of pairs: name and type of fields.
 *
 * Objects of a structural type can not be converted.
 */
case class StructType(fields: Seq[(String, Type)], uconst: Boolean, name: String) extends ScalarType(uconst) {
  /**
   * If t and this are the same types no conversion required, so in that case
   * types can be considered as convertible.
   */
  override def convertible(t: Type) = compatible(t)

  override def compatible(t: Type) = {
    t match {
      case struct: StructType =>
        struct.fields.size == fields.size && (fields, struct.fields).zipped.forall((f1, f2) => f1._2.compatible(f2._2))
    }
  }
  def updateConst(in: Boolean) = this.copy(uconst = in)
}

/**
 * Array type representation.
 *
 * An array type is defined by its base element type and the size of the array (as a scalar expression).
 *
 * Since non-scalar casts are forbidden in PENCIL, the only situation when array conversion is needed
 * is when passing arguments to a function. In that case the only requirement is that base scalar types for
 * both (parameter and argument) array types are compatible.
 *
 */
case class ArrayType(base: Type, var range: ScalarExpression) extends Type(base.const) {
  override def compatible(t: Type) = {
    this == t || (t match {
      case ArrayType(tbase, trange) => tbase.compatible(base) && (range == trange || (
        (range, trange) match {
          case (IntegerConstant(_, v1), IntegerConstant(_, v2)) => v1 == v2
          case (_: ScalarExpression, _: ScalarExpression) => true
          case _ => false
        }))
      case _ => false
    })
  }
  override def convertible(t: Type) = {
    t match {
      case ArrayType(tbase, _) => {
        (base, tbase) match {
          case (t1: ScalarType, t2: ScalarType) => t1.compatible(t2)
          case _ => base.convertible(tbase)
        }
      }
      case _ => false
    }
  }
  def updateConst(in: Boolean) = this.copy(base = base.updateConst(in))
}

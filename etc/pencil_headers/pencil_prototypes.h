/*
 * Copyright (c) 2014, ARM Limited
 * Copyright (c) 2014, Realeyes
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

#ifndef PENCIL_PROTOTYPES_H
#define PENCIL_PROTOTYPES_H

#define __PENCIL_IMPL_DECLARE_OVERLOADS(name, param) \
    __PENCIL_IMPL_DECLARE_OVERLOADS_ ## param(name);

#define __PENCIL_IMPL_DECLARE_OVERLOADS_FLT(functionname)    \
    float  __attribute((overloadable)) functionname(float ); \
    double __attribute((overloadable)) functionname(double); \

#define __PENCIL_IMPL_DECLARE_OVERLOADS_FLT_FLT(functionname)        \
    float  __attribute((overloadable)) functionname(float , float ); \
    double __attribute((overloadable)) functionname(double, double); \

#define __PENCIL_IMPL_DECLARE_OVERLOADS_FLT_FLTPTR(functionname)      \
    float  __attribute((overloadable)) functionname(float , float *); \
    double __attribute((overloadable)) functionname(double, double*); \

#define __PENCIL_IMPL_DECLARE_OVERLOADS_INT_INT(functionname)                                            \
      signed short int __attribute((overloadable)) functionname(  signed short int,   signed short int); \
      signed       int __attribute((overloadable)) functionname(  signed       int,   signed       int); \
      signed  long int __attribute((overloadable)) functionname(  signed  long int,   signed  long int); \
    unsigned short int __attribute((overloadable)) functionname(unsigned short int, unsigned short int); \
    unsigned       int __attribute((overloadable)) functionname(unsigned       int, unsigned       int); \
    unsigned  long int __attribute((overloadable)) functionname(unsigned  long int, unsigned  long int); \
                  char __attribute((overloadable)) functionname(              char,               char); \
      signed      char __attribute((overloadable)) functionname(  signed      char,   signed      char); \
    unsigned      char __attribute((overloadable)) functionname(unsigned      char, unsigned      char); \

#define __PENCIL_IMPL_DECLARE_OVERLOADS_FLT_FLT_FLT(functionname)            \
    float  __attribute((overloadable)) functionname(float , float , float ); \
    double __attribute((overloadable)) functionname(double, double, double); \

#define __PENCIL_IMPL_DECLARE_OVERLOADS_INT_INT_INT(functionname)                                                          \
    signed short int __attribute((overloadable)) functionname(  signed short int,   signed short int,   signed short int); \
    signed       int __attribute((overloadable)) functionname(  signed       int,   signed       int,   signed       int); \
    signed  long int __attribute((overloadable)) functionname(  signed  long int,   signed  long int,   signed  long int); \
  unsigned short int __attribute((overloadable)) functionname(unsigned short int, unsigned short int, unsigned short int); \
  unsigned       int __attribute((overloadable)) functionname(unsigned       int, unsigned       int, unsigned       int); \
  unsigned  long int __attribute((overloadable)) functionname(unsigned  long int, unsigned  long int, unsigned  long int); \
                char __attribute((overloadable)) functionname(              char,               char,               char); \
    signed      char __attribute((overloadable)) functionname(  signed      char,   signed      char,   signed      char); \
  unsigned      char __attribute((overloadable)) functionname(unsigned      char, unsigned      char, unsigned      char); \

    __PENCIL_IMPL_DECLARE_OVERLOADS(  min, FLT_FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(  min, INT_INT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(  max, FLT_FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(  max, INT_INT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(clamp, FLT_FLT_FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(clamp, INT_INT_INT)

    __PENCIL_IMPL_DECLARE_OVERLOADS( tan , FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(atan , FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(atan2, FLT_FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(hypot, FLT_FLT)

    __PENCIL_IMPL_DECLARE_OVERLOADS(exp, FLT)

    __PENCIL_IMPL_DECLARE_OVERLOADS( ceil, FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(floor, FLT)
    __PENCIL_IMPL_DECLARE_OVERLOADS(fract, FLT_FLTPTR)

    /* TODO: add more definitions */

#undef __PENCIL_IMPL_DECLARE_OVERLOADS_FLT
#undef __PENCIL_IMPL_DECLARE_OVERLOADS_FLT_FLT
#undef __PENCIL_IMPL_DECLARE_OVERLOADS_FLT_FLTPTR
#undef __PENCIL_IMPL_DECLARE_OVERLOADS_INT_INT
#undef __PENCIL_IMPL_DECLARE_OVERLOADS_FLT_FLT_FLT
#undef __PENCIL_IMPL_DECLARE_OVERLOADS_INT_INT_INT

#endif

/*
 * Copyright (c) 2014, ARM Limited
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

/* There are 3 ways to use PENCIL file:
 *
 * 1. As Normal C file:
 *    - All PENCIL specific functions (like __pencil_assert) must be ignored.
 *    - All PENCIL built-in functions (math functions, for example) must be
 *      implemented.
 *
 * 2. As PENCIL file supplied to the PENCIL compiler:
 *    - All PENCIL specific functions must be preserved.
 *    - All PENCIL build-in function must be declared (to make the front-end
 *      happy), but not implemented.
 * 3. As C file produced by the PENCIL compiler:
 *    - All PENCIL specific functions (like __pencil_assert) must be ignored.
 *    - All PENCIL built-in functions (math functions, for example) must be
 *      implemented.
 */

#ifndef PENCIL_H
#define PENCIL_H

#ifdef __PENCIL__
/* The file is processed by the PENCIL-to-OpenCL code generator. */

/* PENCIL built-in functions prototypes only. */
#include "pencil_prototypes.h"

#else
/* The file is processed as a C file. */

/* PENCIL to C compatibility layer. */
#include "pencil_compat.h"

/* PENCIL built-in functions prototypes and (possible) implementations. */
#include "pencil_lib.h"

#endif
#endif

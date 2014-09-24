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

#ifndef PENCIL_LIB_H
#define PENCIL_LIB_H

    /* Use implementations from tgmath.h */
#include <tgmath.h>

    /* Implementations for functions that are missing from tgmath.h */

    /* This solution uses a GCC extension to avoid multiple evaluation of macro parameters.
     * It might be better to use C11 _Generic macros, but that's only supported from GCC 4.9 */
#define min(a,b) ({ \
    __typeof__(a) _a_temp_ = (a); \
    __typeof__(b) _b_temp_ = (b); \
    _a_temp_ <= _b_temp_ ? _a_temp_ : _b_temp_; \
    })
#define max(a,b) ({ \
    __typeof__(a) _a_temp_ = (a); \
    __typeof__(b) _b_temp_ = (b); \
    _a_temp_ >= _b_temp_ ? _a_temp_ : _b_temp_; \
    })
#define clamp(val, min, max) ({ \
    __typeof__(val) _val_temp_ = (val); \
    __typeof__(min) _min_temp_ = (min); \
    __typeof__(max) _max_temp_ = (max); \
    (_val_temp_ < _min_temp_) ? _min_temp_ : (_val_temp_ > _max_temp_) ? _max_temp_ : _val_temp_; \
    })

#endif

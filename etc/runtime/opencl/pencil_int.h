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

#ifndef PENCIL_INT_H
#define PENCIL_INT_H

#if defined(__APPLE__)
#include <OpenCL/opencl.h>
#else
#include <CL/opencl.h>
#endif

typedef  struct __int_pencil_cl_kernel *pencil_cl_kernel;
typedef  struct __int_pencil_cl_program *pencil_cl_program;
typedef  struct __int_pencil_cl_mem *pencil_cl_mem;

/* Create and compile OpenCL program from a file.  */
extern pencil_cl_program opencl_create_program_from_file (const char *, const char *);

/* Create and compile OpenCL program from a string.  */
extern pencil_cl_program opencl_create_program_from_string (const char *,
                                                            size_t,
                                                            const char *);

/* Releases the OpenCL program.  */
extern void opencl_release_program (pencil_cl_program);

/* Creates the kernel.  */
extern pencil_cl_kernel opencl_create_kernel (pencil_cl_program, const char *);

/* Releases the kernel.  */
extern void opencl_release_kernel (pencil_cl_kernel);

/* Create device memory buffer.  */
extern pencil_cl_mem opencl_create_device_buffer (cl_mem_flags, size_t, void *);

/* Release the device memory buffer.  */
extern void opencl_release_buffer (pencil_cl_mem);

/* Copy host memory buffer to device memory.  */
extern void opencl_copy_to_device (pencil_cl_mem, size_t, void *);

/* Copy device memory buffer to host memory.  */
extern void opencl_copy_to_host (pencil_cl_mem, size_t, void *);

extern void opencl_set_kernel_arg (pencil_cl_kernel, cl_uint, size_t,
                                   const void *, int);

extern void opencl_launch_kernel (pencil_cl_kernel, cl_uint, const size_t *,
                                  const size_t *, const size_t *);
#endif

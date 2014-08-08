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

#include <stdlib.h>

#include "pencil_int.h"
#include "pencil_runtime.h"

pencil_cl_program opencl_create_program (const char *filename, const char *opts)
{
}

void opencl_release_program (pencil_cl_program program)
{
}

pencil_cl_kernel opencl_create_kernel (pencil_cl_program program, const char *name)
{
}

void opencl_release_kernel (pencil_cl_kernel kernel)
{
}

pencil_cl_mem opencl_create_device_buffer (cl_mem_flags flags, size_t size, void *host_ptr)
{
}

void opencl_release_buffer (pencil_cl_mem buffer)
{
}

void opencl_copy_to_device (pencil_cl_mem dev, size_t size, void *host)
{
}

void opencl_copy_to_host (pencil_cl_mem dev, size_t size, void *host)
{
}

void *pencil_alloc (size_t size)
{
}

void pencil_free (void *ptr)
{
}

void pencil_init ()
{
}

void pencil_shutdown ()
{
}

void opencl_set_kernel_arg (pencil_cl_kernel kernel, cl_uint idx, size_t size,
                            const void *value, int buffer)
{
}

void opencl_launch_kernel (pencil_cl_kernel kernel, cl_uint dim,
                           const size_t *goffset,
                           const size_t *gws, const size_t *lws)
{
}

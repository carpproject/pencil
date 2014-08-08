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

#ifndef IMPL_H
#define IMPL_H

#ifdef __cplusplus
extern "C" {
#endif

    typedef  struct __int_pencil_cl_kernel *pencil_cl_kernel;
    typedef  struct __int_pencil_cl_program *pencil_cl_program;
    typedef  struct __int_pencil_cl_mem *pencil_cl_mem;

    pencil_cl_program __int_opencl_create_program (const char *, const char *);
    void __int_opencl_release_program (pencil_cl_program);
    pencil_cl_kernel __int_opencl_create_kernel (pencil_cl_program,
                                                 const char *);
    void __int_opencl_release_kernel (pencil_cl_kernel);
    pencil_cl_mem __int_opencl_create_device_buffer (cl_mem_flags, size_t,
                                                     void *);
    void __int_opencl_release_buffer (pencil_cl_mem);
    void __int_opencl_copy_to_device (pencil_cl_mem, size_t, void *);
    void __int_opencl_copy_to_host (pencil_cl_mem, size_t, void *);
    void *__int_pencil_alloc (size_t);
    void __int_pencil_free (void *);
    void __int_pencil_init ();
    void __int_pencil_shutdown ();
    void __int_opencl_set_kernel_arg (pencil_cl_kernel, cl_uint, size_t,
                                      const void *, int);
    void __int_opencl_launch_kernel (pencil_cl_kernel, cl_uint, const size_t *,
                                     const size_t *, const size_t *);

#ifdef __cplusplus
}
#endif

#endif

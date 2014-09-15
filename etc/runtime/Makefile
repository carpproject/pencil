# Copyright (c) 2013-2014, ARM Limited
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

ifeq ($(DEBUG),1)
	CFLAGS = -O0 -g -Wall -W
else
	CFLAGS = -O3 -g -Wall -W -DNDEBUG -Wno-unused-variable -Wno-unused-but-set-variable
endif

ifeq ($(HOST_ALLOC),1)
	CFLAGS += -DHOST_ALLOC=1
endif

CXXFLAGS = ${CFLAGS} -std=c++11
CC = gcc
CXX = g++

OCL_UTIL =

all: libocl_pencil.so lib/libocl_pencil.so

ocl_pencil_opt.o: opencl-opt/pencil_runtime.c opencl/pencil_int.h pencil_runtime.h
	${CC} -c -fpic -I. ${CFLAGS} $< -o $@

pencil_runtime_impl.o: opencl-opt/impl/pencil_runtime_impl.cpp opencl-opt/impl/impl.h
	${CXX} -c -fpic ${CXXFLAGS} $< -o $@

ocl_pencil.o: opencl/pencil_runtime.c opencl/pencil_int.h pencil_runtime.h
	${CC} -c -fpic -I. ${CFLAGS} $< -o $@ -Wno-unused-parameter -Wno-return-type

ocl_utilities.o: ${OCL_UTIL}
	${CC} -c -fpic ${CFLAGS} $< -o $@

libocl_pencil_ref.so: ocl_pencil.o ocl_utilities.o
	${CC} -shared $^ -o $@

libocl_pencil_opt.so: ocl_pencil_opt.o pencil_runtime_impl.o ocl_utilities.o
	${CXX} -shared $^ -o $@

libocl_pencil.so: libocl_pencil_opt.so
	ln -sf $< $@

lib/libocl_pencil.so: libocl_pencil_ref.so
	mkdir -p lib
	ln -sf ../$< $@

clean:
	rm -f *.o *.so

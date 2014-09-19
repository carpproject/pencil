#!/bin/bash

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

cd `dirname $0`

TOOL_ROOT=`pwd`

WORK_ROOT=`pwd`/work

DEB_PACKAGE_ROOT=${TOOL_ROOT}/packages/debian/

DEB_PACKAGE_NAME=pencil-tools.deb

DEB_PACKAGE=${TOOL_ROOT}/${DEB_PACKAGE_NAME}

GENTOO_EBUILD=${TOOL_ROOT}/packages/ebuild/app-misc/pencil-tools/pencil-tools-9999.ebuild

# Create DEB package
rm -rf ${WORK_ROOT}
mkdir -p ${WORK_ROOT}
make install PREFIX=${WORK_ROOT}
cp -r ${DEB_PACKAGE_ROOT}/DEBIAN ${WORK_ROOT}
chown -R root:root ${WORK_ROOT}
dpkg-deb -b ${WORK_ROOT} ${DEB_PACKAGE}

# Create RPM package
alien --to-rpm ${DEB_PACKAGE}

# Create ebuild
cp ${GENTOO_EBUILD} .

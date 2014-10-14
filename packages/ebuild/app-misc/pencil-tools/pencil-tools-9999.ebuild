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


EAPI="3"

PYTHON_DEPEND="2"

inherit git-2

DESCRIPTION="PENCIL tools developed within the CARP project."

HOMEPAGE="https://github.com/carpproject/pencil"

EGIT_REPO_URI="http://github.com/carpproject/pencil.git"

LICENSE="MIT"
SLOT="0"
DEPEND=""
RDEPEND=${DEPEND}

ANTLR_TMP_NAME=antlr-tmp

get_antlr() {
    ANTLR_HOME=`readlink -f $1`
    mkdir -p $ANTLR_HOME
    wget http://www.antlr3.org/download/antlr-3.5.2-complete-no-st3.jar -O $ANTLR_HOME/antlr3.jar
    wget http://www.antlr3.org/share/1169924912745/antlr3-task.zip -O `pwd`/antlr3-task.zip
    unzip `pwd`/antlr3-task.zip -d `pwd`/antlr3-task
    cp `pwd`/antlr3-task/antlr3-task/ant-antlr3.jar $ANTLR_HOME/ant-antlr3.jar
    rm -rf `pwd`/antlr3-task `pwd`/antlr3-task.zip
}

src_prepare() {
    get_antlr $ANTLR_TMP_NAME
    autoconf
}

src_configure() {
    econf --with-antlr3=$ANTLR_TMP_NAME
}

src_install() {
    emake PREFIX="${D}" install
}

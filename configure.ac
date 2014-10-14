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

AC_INIT([pencil], [0.01], [carp@arm.com])
AC_CONFIG_MACRO_DIR([m4])

dnl Scala installation
AC_SUBST(SCALA_HOME)
AC_SUBST(SCALA_RUNTIME)
AC_ARG_WITH([scala],
  [AS_HELP_STRING([--with-scala=dir],[directory containing scala])],
  [
   SCALA_HOME="$with_scala"
   SCALA_RUNTIME="$with_scala"/bin/scala
  ],
  [
   SCALA_HOME="/usr/share/scala"
   SCALA_RUNTIME=scala
  ]
)

AC_CHECK_FILE(["$SCALA_HOME/lib/scala-compiler.jar"],[],[AC_MSG_ERROR([could not find file])])
AC_CHECK_FILE(["$SCALA_HOME/lib/scala-library.jar"],[],[AC_MSG_ERROR([could not find file])])
AC_CHECK_FILE(["$SCALA_HOME/lib/scala-reflect.jar"],[],[AC_MSG_ERROR([could not find file])])

dnl FileCheck installation
AC_SUBST(FILE_CHECK_TOOL)
AC_ARG_WITH([filecheck-home],
            [AS_HELP_STRING([--with-filecheck-home=path],[Directory containing FileCheck])],
            [FILE_CHECK_HOME="$with_filecheck_home"],[])
AC_CHECK_PROG([filecheck_home_found], ["FileCheck"], [yes], [no], ["$FILE_CHECK_HOME:$PATH"])
if test "x$filecheck_home_found" != "xyes"; then
  if test "x$FILE_CHECK_HOME" != "x"; then
      AC_MSG_ERROR([could not find FileCheck executable])
  else
      AC_MSG_WARN([FileCheck is disabled (use --with-filecheck-home=path to enable)])
      FILE_CHECK_TOOL=
  fi
else
  FILE_CHECK_TOOL=`PATH=$PATH:$FILE_CHECK_HOME which FileCheck`
fi

dnl ANTLR3 jar
AC_SUBST(ANTLR_HOME)
AC_ARG_WITH([antlr3],
  [AS_HELP_STRING([--with-antlr3=dir],[directory containing ANTLR3 jars])],
  [ANTLR_HOME="$with_antlr3"],
  [AC_MSG_ERROR([please specify path to ANTLR3 jars using --with-antlr3])]
)
AC_CHECK_FILE(["$ANTLR_HOME/antlr3.jar"],[],[AC_MSG_ERROR([could not find file])])

dnl ant-antlr3 jar
AC_SUBST(ANT_ANTLR_HOME)
AC_ARG_WITH([ant-antlr3],
  [AS_HELP_STRING([--with-ant-antlr3=dir],[directory containing ant-antlr3 jar])],
  [ANT_ANTLR_HOME="$with_ant_antlr3"],
  [ANT_ANTLR_HOME="$ANTLR_HOME"]
)
AC_CHECK_FILE(["$ANT_ANTLR_HOME/ant-antlr3.jar"],[],[AC_MSG_ERROR([could not find file])])

AC_CONFIG_FILES([linker], [chmod +x linker])
AC_CONFIG_FILES([optimizer], [chmod +x optimizer])
AC_CONFIG_FILES([pencil-linker], [chmod +x pencil-linker])
AC_CONFIG_FILES([pencil-optimizer], [chmod +x pencil-optimizer])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

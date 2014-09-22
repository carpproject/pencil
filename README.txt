Experimental PENCIL tools repository

Tools
-----

 * PENCIL cross-component optimizer
 * PENCIL linker

Coding Style
-----------
C      - please follow linux kernel coding style [https://www.kernel.org/doc/Documentation/CodingStyle]
Scala  - please follow Scala Style Guide [http://docs.scala-lang.org/style]


Repository Contents
-------------------

 * grammar/         ANTLR3 Grammar for PENCIL
 * res/             Resources
 * src/             Scala source code:
   * .../apps/        Tool-specific source code
   * .../frontends/   PENCIL frontend code
   * .../pencil/      PENCIL class definitions
 * testsuite/       Test files:
   * code/            Unit tests
   * lib/             Testing infrastructure files

Building process
----------------
Summary:

autoconf
./configure --with-scala=$SCALA_HOME --with-antlr3=$ANTLR_HOME [--with-filecheck=$FILECHECK_HOME] [--prefix=$PREFIX]
make

For more information on what $SCALA_HOME and $ANTLR_HOME should point to,
refer to 'Build requirements' below.


Build requirements
------------------
 * Antlr3 is installed and ANTLR_HOME is set:
    % ANTLR_HOME=/opt/antlr # Or any other
    % mkdir -p $ANTLR_HOME
    % wget http://www.antlr3.org/download/antlr-3.5.2-complete-no-st3.jar -O $ANTLR_HOME/antlr3.jar
    % wget http://www.antlr3.org/share/1169924912745/antlr3-task.zip -O /tmp/antlr3-task.zip
    % unzip /tmp/antlr3-task.zip -d /tmp/antlr3-task
    % cp /tmp/antlr3-task/antlr3-task/ant-antlr3.jar $ANTLR_HOME/ant-antlr3.jar
    % rm -rf /tmp/antlr3-task /tmp/antlr3-task.zip
    % ls $ANTLR_HOME
    ant-antlr3.jar  antlr3.jar
 * Scala tools are installed and SCALA_HOME is set:
    % SCALA_HOME=/opt/scala # Or any other
    % mkdir -p $SCALA_HOME
    % wget http://www.scala-lang.org/files/archive/scala-2.10.4.tgz
    % tar xf scala-2.10.4.tgz --strip-component=1 -C $SCALA_HOME
    % rm scala-2.10.4.tgz
    % ls $SCALA_HOME/lib -1
     akka-actors.jar
     jline.jar
     scala-actors.jar
     scala-actors-migration.jar
     scala-compiler.jar
     scala-library.jar
     scala-partest.jar
     scalap.jar
     scala-reflect.jar
     scala-swing.jar
     typesafe-config.jar
  * Antlr3 tools are accessible via CLASSPATH:
    % export CLASSPATH=$ANTLR_HOME/ant-antlr3.jar:$ANTLR_HOME/antlr3.jar
  * [Optional] FileCheck tool from LLVM testsuite:
    % ls $FILECHECK_HOME/FileCheck
    FileCheck
    If present this tool will be used to perform more accurate testing
    of the PENCIL tools.

This build has been tested on the following configuration:
 % scalac -version
 Scala compiler version 2.10.4 -- Copyright 2002-2013, LAMP/EPFL
 % ant -version
 Apache Ant version 1.7.1 compiled on September 8 2010
 % java -jar $ANTLR_HOME/antlr3.jar -version
 ANTLR Parser Generator  Version 3.5.2

Running tools
-------------
Linker: linker <files>
Optimizer: optimizer <file>
Each tool supports -h for help

Creating the binary packages
----------------------------
The following binary packages can be created:
 * Debian package (deb)
 * Red Hat package (rpm)

The following programs must be available:
 * dpkg-deb
 * fakeroot
 * alien

The packages are created as follows:
 % autoconf
 % ./configure --with-scala=$TARGET_SCALA_HOME --with-antlr3=$TAREGT_ANTLR_HOME [--prefix=$target_prefix]
 % make
 % make packages

Variables:
 * TARGET_SCALA_HOME - scala home location on the system, where package will be installed
   (/usr/share/scala for example for system wide scala in Ubuntu).
 * TARGET_ANTLR_HOME - antlr home location on the system, where package will be installed
   (/usr/share/java for example for system wide antlr in Ubuntu).
 * TAREGT_PREFIX - prefix for file installation (/usr/local/ by default).

After the package is built it can be installed as follows:
% sudo dpkg -i pencil-tools.deb # for deb package

After the package is installed the following commands should be available:
% pencil-optimizer
% pencil-linker


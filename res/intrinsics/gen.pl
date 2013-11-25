#!/usr/bin/env perl

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

use strict;
use feature 'state';

sub scala
{
  state $sep = "";
  my $res =  "    $sep\"$_[0]\" -> $_[1]\n";
  $sep = ",";
  return $res;
}

sub java
{
  return "      functions.put(\"$_[0]\", $_[1]);\n";
}

my %generators = (scala => \&scala,
                  java => \&java);

($#ARGV == 2) || die "Three arguments exprected";

my $input = @ARGV[0];
my $output = $ARGV[1];
my $lang = @ARGV[2];

open FILE, $input or die $!;
open OFILE, ">$output" or die $!;

open HEADER, "$lang.header" or die $!;

while (<HEADER>) {
  print OFILE $_;
}

while (<FILE>) {
  if (/gentype *(\w+) *\((.*)\)/) {
    my $len = split(',', $2);
    my $result = $generators{$lang}->($1, $len);
    print OFILE $result;
  }
}


open FOOTER, "$lang.footer" or die $!;

while (<FOOTER>) {
  print OFILE $_;
}

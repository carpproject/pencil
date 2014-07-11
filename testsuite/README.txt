How to write a PENCIL test
--------------------------

Each test is a PENCIL program with additional testing directives in the form
of comments. There are two kinds of directives:
 * Testing commands:
   /* <tool> {@<command> {arg}} */
 * Additional C code for test application:
   /* <tool> {<any c statement>} */

The tool field can be one of the following:
 * optimizer - the directive will only be applied when testing the optimizer.
 * linker - the directive will only be applied when testing the linker.
 * all - the directive will be applied for all the tools (linker and optimizer).

The following commands are supported:
 * error - indicates that the current line contains a compilation error,
   which must be reported by a tested tool
 * assert {cond} - indicates that the given condition must hold during
   test execution.
 * require-option {option} - indicates that the given option must be supplied
   to a tested tool.
 * check {pattern} - indicates that the given pattern should be supplied to
   FileCheck tool. The FileCheck tool (if present) is executed on the tool
   output.
 * link-with {file} - indicates that the input file should be linked (when
   testing the linker) with the given file.
 * link-error - indicates that a link time error is expected.


All commands except for `error' can be placed anywhere in the test file.

Running the tests
-----------------
* to run the optimizer testsuite: runtest --tool optimizer
* to run the linker testsuite: runtest --tool linker
* to run a single linker test: LINKER_TEST=<path-to-test-file> runtest --tool linker
* to run a single optimizer test: OPTIMIZER_TEST=<path-to-test-file> runtest --tool optimizer

Example 1
---------
/* optimizer {@require-option {-fdce}} */ //Require optimizer to execute dead code elimination pass
int f (int i)
{
    int i = 1;
    if (false)
    {
        int dead_code;
        int j = 12;
        dead_code = j;
    }
    return i;
}
/* all {@assert {f(1) == 1}} */ // Add correctness check.
/* optimizer {@check {CHECK-NOT: dead_code}} */ //Check that dead code is eliminated.


Example 2
---------
/* all {int A[1] = {12};} */ // Add initialization code
void foo(int A[1], int i)
{
    int j = 12;
    int B[i + 1 + j];
    B[0] = i;
    A[0] = B[0];
}
/* all {foo(A, 1);} */ // Call function
/* all {@assert {A[0] == 1}} */ // Add correctness check


Example 3
---------
int foo(int i)
{
    int j = 12;
    int A[i + 1 + j];
    A[0] = i + B[1]; /* all {@error} */ // Mark line with error
    return A[0] + i;
}

Example 4
---------
a.pencil:
/* linker {@link-with {b.pencil}} */ //Link this file with b.pencil
/* linker {@link-error} */ //Link error must be reported
void foo(){}


b.pencil:
void foo(){} //foo is redefined (should generate link error)

Since a.pencil is requesting linking with b.pencil the @link-error command is
only given in a.pencil (b.pencil can also be considered as a standalone test).

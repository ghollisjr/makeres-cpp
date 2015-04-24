makeres-cpp allows cl-ana's makeres to generate C++ code for efficient
data analysis.  There are cases where the cost of forcing Lisp to be
efficient enough for a task is too high in computer time, programmer
time, or both.  makeres-cpp thus gives makeres the ability to generate
C++ code in such cases.

C++ code is represented via its abstract syntax tree with each
operator or function being applied with Lisp syntax (operator at front
of list, arguments follow), but with symbols directly representing the
C++ operators being used.

Some C++ functions or operators are expressed like Lisp does.  For
example, arithmetic functions like +, -, *, / accept any number of
arguments and are compiled to a form with repeated application.

Parentheses are automatically placed around arguments to ensure that
order of operations are represented as they are in the abstract syntax
tree.

A let* operator is provided as well which creates a sub context with
braces {,} and declares/defines C++ variables.

Types must be used for all variables and functions.  Types come before
variables in any applicable operators; since they are required whereas
declared values are not, this is one area where C/C++ syntax is
properly optimized.

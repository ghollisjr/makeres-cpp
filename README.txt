makeres-cpp allows cl-ana's makeres to generate C++ code for efficient
data analysis.  There are cases where Lisp can be too expensive in
computer time, programmer time, or both.  makeres-cpp thus gives
makeres the ability to generate C++ code in such cases using more
Lisp-like conventions.  For example, the source code for a hello world
program could be generated via

(program
  (function int main ()
            (<< cout (str "Hello, World!") endl)
            (return 0)))

resulting in

#include<iostream>
int main();
int main() {
  std::cout << "Hello, World!" << std::endl;
  return (0);
}

with any headers and user-defined functions automatically included in
the source based on the tokens used in the C++ program S-expression.

Built on top of this basic C++ S-expression language are operators
analogous to the tab, ltab, srctab and dotab operators from
cl-ana.makeres-table.  ROOT (from CERN) TTrees as tables are supported
(HDF5 dataset support pending).  If root2hdf script
(https://github.com/ghollisjr/root2hdf) is installed then ROOT file
results can be converted directly to HDF5 datasets which can be
analysed by the hdf-table functionality already supported by cl-ana.

C++ code is represented via its abstract syntax tree with each
operator or function being applied with Lisp syntax (operator at front
of list, arguments follow), but with symbols directly representing the
C++ operators being used.

Some C++ functions or operators are expressed like Lisp does.  For
example, arithmetic functions like +, -, *, / accept any number of
arguments and are compiled to a form with repeated application.

Types must be used for all variables and functions.  Types come before
variables in any applicable operators; since they are required whereas
declared values are not, this is one area where C/C++ syntax is
properly optimized.

WARNING: At the moment, this library assumes GCC as the compiler;
doing otherwise makes this project much more complicated than is
reasonable for the author's current situation.

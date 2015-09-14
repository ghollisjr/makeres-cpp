;;;; makeres-cpp is a Common Lisp data analysis library.
;;;; Copyright 2015 Gary Hollis
;;;; 
;;;; This file is part of makeres-cpp.
;;;;
;;;; makeres-cpp is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;; makeres-cpp is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-cpp.  If not, see
;;;; <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(asdf:defsystem #:makeres-cpp
  :serial t
  :author "Gary Hollis"
  :description "makeres-cpp is a table transformation for makeres from
  cl-ana which allows the generation and compilation of C++ code for
  efficient computation."
  :license "GPLv3"
  :depends-on (#:cl-ana
               #:external-program
               #:alexandria)
  :components ((:file "package")
               ;; Reader macros:
               (:file "read-macro")
               ;; Fundamental C++ code compilation & generation
               ;; utilities
               (:file "utils")
               (:file "cpp-utils")
               ;; C++ function definition
               (:file "function")

               ;; Shell utilities
               (:file "shell")

               ;; Conversion utilties
               (:file "convert")

               ;; Read/Write ROOT histogram via HDF5 cl-ana formatting
               (:file "histogram")

               ;; Convert Lisp lists to C++ vectors
               (:file "list")

               ;; ROOT table type:
               (:file "root-table")

               ;; C++ table operators
               (:file "cpp-table-operators")
               ;; ;; makeres transformation stuff
               (:file "cpptrans")
               
               ;; Syntactic structures
               (:file "syntax")
               ;; Headers
               (:file "headers")
               ;; Variable handling
               (:file "var")
               ;; Control structures
               (:file "control")
               ;; Boolean operators
               (:file "bool")
               ;; Standard library functions
               (:file "std")
               ;; Math functions and operators
               (:file "math")
               ;; Code generation, compilation & execution
               (:file "exe")
               ;; POSIX functions
               (:file "posix")

               ;; HDF5
               (:file "hdf")
               ;; ROOT
               (:file "root")
               ))

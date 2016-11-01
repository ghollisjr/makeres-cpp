;;;; makeres-cpp is a Common Lisp data analysis library.
;;;; Copyright 2016 Gary Hollis
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

(require 'cl-ana)
(require 'makeres-cpp)

;;;; Test project section:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage test
    (:use :cl
          :makeres-cpp))

  (cl-ana.package-utils:use-package-group :cl-ana :test))

(in-package :test)

(defrawcppfun void printtest ()
              "int x = 5;
std::cout << x << std::endl;
return;"
              :headers ("iostream"))

(defun raw-test ()
  (exe "/home/ghollisjr/raw-test"
       ((function int main ()
                  (printtest)
                  (return 0)))
       :output *standard-output*))

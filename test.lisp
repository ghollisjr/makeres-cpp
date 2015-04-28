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

(require 'makeres-cpp)

(in-package :makeres-cpp)

(defun test ()
  (let ((main
         '(function int main
           ((var int argc)
            (var (pointer (pointer char)) argv))
           (for (var int i 0) "i < 5" "++i"
            (<< cout argc endl))
           (var ifstream infile "\"test.dat\"")
           (var float line)
           (while (>> infile line)
             (<< cout (sqrt line) endl))
           (method infile close))))
    (format t "Required headers: ~a~%" (required-headers main))
    (format t "C++ Code:~%~a~%" (cpp main)))
  nil)

(defun progtest (&optional (n 1000000))
  (exe "/home/ghollisjr/test/exe"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (var long n (atoi (aref argv 1)))
                  (var (type long long) sum 0)
                  (var int count 0)
                  (for (var long i 0) (< i n) (incf i)
                       (setf sum
                             (+ sum i))
                       (incf count))
                  (<< cout
                      (/ (typecast float sum)
                         (typecast float count))
                      endl)))
       :flags '("-O3")
       :arguments (list (lisp->cpp n))
       :output *standard-output*))

(defcppfun void makeres_cpp_test
    ((var int x)
     (var float y))
  (<< cout x "\" \"" y endl))

(defun progtest2 ()
  (exe "/home/ghollisjr/test/exe2"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (makeres_cpp_test 1 2)))
       :flags '("-O3")
       :output *standard-output*))

(defun roottest1 ()
  (exe "/home/ghollisjr/test/roottest1"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (var (pointer th1d)
                       h
                       (new th1d
                            "\"hist\""
                            "\"hist\""
                            100
                            -3
                            3))
                  (var trandom3 gen)
                  (for (var int i 0) (< i 1000) (incf i)
                       (pmethod h fill
                                (method gen gaus 0 1)))
                  (<< cout (pmethod h entries) endl)))
       :output *standard-output*))

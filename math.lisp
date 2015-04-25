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
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-cpp.  If not, see
;;;; <http://www.gnu.org/licenses/>.
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :makeres-cpp)

(defcpp + (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^+~})" (mapcar #'cpp forms))))

(defcpp - (&rest forms)
  (with-output-to-string (out)
    (if (single forms)
        (format out "(-~a)" (cpp (first forms)))
        (format out "(~{~a~^-~})" (mapcar #'cpp forms)))))

(defcpp * (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^*~})" (mapcar #'cpp forms))))

;; Note: Not valid for single argument, only works with two or more arguments
(defcpp / (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^/~})" (mapcar #'cpp forms))))

(defcpp expt (x y)
  (with-output-to-string (out)
    (format out "pow(~a,~a)"
            (cpp x)
            (cpp y))))

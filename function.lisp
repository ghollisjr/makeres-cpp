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

(in-package :makeres-cpp)

(defparameter *cpp-funs*
  (make-hash-table :test 'eq)
  "Map from C++ function symbol to code")

(defmacro defcppfun (type fname cpp-args &body body)
  "Defines a C++ function which is loaded automatically into a
program's code."
  `(setf (gethash ',fname
                  *cpp-funs*)
         (list :type ',type
               :cpp-args ',cpp-args
               :body ',body)))

(defmacro undefcppfun (fname)
  `(remhash ',fname *cpp-funs*))

(defun required-functions (code)
  "Returns list of required functions for code"
  (list->set
   (cond
     ((null code) nil)
     ((listp code)
      (cond
        ((eq (first code)
             'res)
         (required-functions `(,(cpp-loader (resfn (second code)))
                                (str ""))))
        ((eq (first code)
             'eval)
         nil)
        (t
         (apply #'append
                (required-functions (first code))
                (mapcar #'required-functions (rest code)))))
      ;; (apply #'append
      ;;        (required-functions (first code))
      ;;        (mapcar #'required-functions (rest code)))
      )
     ((atom code)
      (if (gethash code *cpp-funs*)
          (list code)
          nil)))
   #'eq))

(defun prototype (function-code)
  "Returns C++ code for the prototype of a function definition"
  (with-output-to-string (out)
    (destructuring-bind (type fname cpp-args &rest body)
        (rest function-code)
      (format out "~a ~a(~{~a~^,~})"
              (cpp type)
              (cpp fname)
              (mapcar #'cpp cpp-args)))))

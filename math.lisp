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

(defcpp incf (var)
  (format nil "(++~a)" (cpp var)))

(defcpp decf (var)
  (format nil "(--~a)" (cpp var)))

;;; modular arithmetic
(defcpp mod (x divisor)
  (format nil "((~a) % (~a))"
          (cpp x)
          (cpp divisor)))

(defcpp int-/ (num den)
  (cpp `(/ (typecast int ,num)
           (typecast int ,den))))

(defcpp long-/ (num den)
  (cpp `(/ (typecast long ,num)
           (typecast long ,den))))

;;;; Comparison functions:

(macrolet ((defboolop (sym &optional str)
             `(defcpp ,sym (&rest forms)
                (with-output-to-string (out)
                  (format out "(")
                  (loop
                     for fcons on forms
                     when (second fcons)
                     do (let ((left (first fcons))
                              (right (second fcons)))
                          (format out "~a ~a ~a"
                                  (cpp left)
                                  ,(if str
                                       str
                                       (string-downcase (string sym)))
                                  (cpp right))
                          (when (third fcons)
                            (format out " && "))))
                  (format out ")")))))
  (defboolop <)
  (defboolop <=)
  (defboolop >)
  (defboolop >=)
  (defboolop = "=="))

;;;; Extra math functions from cmath

(defheader "cmath"
    (cos
     sin
     tan
     acos
     asin
     atan
     atan2
     cosh
     sinh
     tanh
     acosh
     asinh
     atanh
     exp
     frexp
     ldexp
     log
     log10
     modf
     exp2
     expm1
     ilogb
     log1p
     log2
     logb
     scalbn
     scalbln
     pow
     sqrt
     cbrt
     hypot
     erf
     erfc
     tgamma
     lgamma
     ceil
     floor
     fmod
     trunc
     round
     lround
     llround
     rint
     lrint
     llrint
     nearbyint
     remainder
     remquo
     copysign
     nan
     nextafter
     nexttoward
     fdim
     fmax
     fmin
     fabs
     abs
     fma
     math_errhandling
     infinity
     nan-macro
     huge-val
     huge-valf
     huge-vall)
  :flags "-std=c++11")

(defcpp nan-macro (&rest args)
  (with-output-to-string (out)
    (format out "NAN(~{~a~^,~})"
            (mapcar #'cpp args))))

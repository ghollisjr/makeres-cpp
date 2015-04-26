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

;;;; Function definition

(defcpp function (type fname arg-list &body body)
  (with-output-to-string (out)
    (format out "~a ~a(~{~a~^,~}) {~%"
            (cpp type)
            (cpp fname)
            (mapcar #'cpp arg-list))
    (loop
       for expr in body
       do (format out "~a;~%" (cpp expr)))
    (format out "}~%")))

;;;; Object methods/members & pointer methods/ members

;; call method on object
(defcpp method (object method &rest arguments)
  (with-output-to-string (out)
    (format out "~a.~a(~{~a~^,~})"
            (cpp object)
            (cpp method)
            (mapcar #'cpp arguments))))

;; access member of object
(defcpp member (object member)
  (with-output-to-string (out)
    (format out "~a.~a"
            (cpp object)
            (cpp member))))

;; call method on object pointed to
(defcpp pmethod (pointer method &rest arguments)
  (with-output-to-string (out)
    (format out "~a->~a(~{~a~^,~})"
            (cpp pointer)
            (cpp method)
            (mapcar #'cpp arguments))))

;; access member on object pointed to
(defcpp pmember (pointer member)
  (with-output-to-string (out)
    (format out "~a->~a"
            (cpp pointer)
            (cpp member))))

;; Assignment (didn't know exactly where to place this)
(defcpp setf (place value)
  (with-output-to-string (out)
    (format out "~a = ~a"
            (cpp place)
            (cpp value))))

;; type casting
(defcpp typecast (type val)
  (with-output-to-string (out)
    (format out "((~a) ~a)"
            (cpp type)
            (cpp val))))

;; Array referencing (operator[])
(defcpp aref (array &rest indices)
  (with-output-to-string (out)
    (format out "~a~{[~a]~}"
            (cpp array)
            (mapcar #'cpp indices))))

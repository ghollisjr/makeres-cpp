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

;; guaranteed context
(defcpp progn (&body body)
  (with-output-to-string (out)
    (format out "{~%")
    (loop
       for expr in body
       do (format out "~a;~%" (cpp expr)))
    (format out "}")))

(defcpp let (bindings &body body)
  (with-output-to-string (out)
    (format out "{~%")
    (loop
       for binding in bindings
       do (destructuring-bind (type var &optional init) binding
            (format out "~a ~a" (cpp type) (cpp var))
            (when init
              (format out " = ~a" (cpp init)))
            (format out ";~%")))
    (loop
       for expr in body
       do (format out "~a;~%"
                  (cpp expr)))
    (format out "}~%")))

(defcpp cond (&rest test-bodies)
  (with-output-to-string (out)
    (loop
       for test-body in test-bodies
       for entry from 0
       do (let ((opstr (if (zerop entry)
                           "if"
                           "else if")))
            (destructuring-bind (test &rest body) test-body
              (if (not (eq test t))
                  (format out "~a(~a) {~%"
                          opstr (cpp test))
                  (format out "else {~%"))
              (loop
                 for expr in body
                 do (format out "~a;~%" (cpp expr)))
              (format out "}~%")
              (if (eq test t)
                  (return t)))))))

(defcpp if (test expr &optional else)
  (with-output-to-string (out)
    (format out "if(~a) {~%" (cpp test))
    (format out "~a;~%" (cpp expr))
    (format out "}~%")
    (when else
      (format out "else {~%")
      (format out "~a;~%" (cpp else))
      (format out "}~%"))))

(defcpp when (test &rest body)
  (cpp `(if ,test (progn ,@body))))

;; Transliteration of C++ for
(defcpp for (init test incr &body body)
  (with-output-to-string (out)
    (format out "for(~a; ~a; ~a) {~%"
            (cpp init) (cpp test) (cpp incr))
    (loop
       for expr in body
       do (format out "~a;~%" (cpp expr)))
    (format out "}~%")))

;; Same for while
(defcpp while (test &body body)
  (with-output-to-string (out)
    (format out "while(~a) {~%"
            (cpp test))
    (loop
       for expr in body
       do (format out "~a;~%" (cpp expr)))
    (format out "}~%")))

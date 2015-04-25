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

(defcpp var (type name &optional value)
  (with-output-to-string (out)
    (format out "~a ~a" (cpp type) (cpp name))
    (when value
      (format out " = ~a" value))
    (format out ";~%")))

;; Returns a type which requires many type-tokens
(defcpp type (&rest type-tokens)
  (with-output-to-string (out)
    (format out "~{~a~^ ~}"
            (mapcar #'cpp type-tokens))))

;; Returns the pointer type for a given type
(defcpp pointer (type)
  (with-output-to-string (out)
    (format out "~a*" (cpp type))))

(defcpp new (type-or-constructor &rest args)
  (with-output-to-string (out)
    (format out "new ~a" (cpp type-or-constructor))
    (if args
        (format out "(~{~a~^,~})"
                (mapcar #'cpp args))
        (format out "()"))))

(defcpp new[] (type size)
  (with-output-to-string (out)
    (format out "new ~a" (cpp type))
    (format out "[~a]"
            (cpp size))))

(defcpp delete (var)
  (with-output-to-string (out)
    (format out "delete ~a;~%" (cpp var))))

(defcpp delete[] (var)
  (with-output-to-string (out)
    (format out "delete[] ~a;~%" (cpp var))))

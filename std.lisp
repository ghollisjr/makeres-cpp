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

(defheader "iostream"
    (;; Standard streams
     cout
     cin
     ;; Operators
     <<
     >>
     ;; Special objects
     endl))

(defmacro defstdobj (symbol &optional string)
  (alexandria:with-gensyms (out)
    `(defcpp ,symbol ()
       (with-output-to-string (,out)
         (format ,out "std::~a"
                 ,(if string
                      string
                      (string-downcase (mkstr symbol))))))))

(defstdobj cout)
(defstdobj cin)
(defstdobj endl)

(defcpp << (stream &rest args)
  (with-output-to-string (out)
    (format out "~a~{~^ ~^<<~^ ~a~}"
            (cpp stream)
            (mapcar #'cpp args))))

(defcpp >> (stream &rest args)
  (with-output-to-string (out)
    (format out "~a~{~^ ~^>>~^ ~a~}"
            (cpp stream)
            (mapcar #'cpp args))))

(defheader "fstream"
    (;; input and output file streams
     ifstream
     ofstream
     ;; General fstream
     fstream))

(defstdobj ifstream)
(defstdobj ofstream)
(defstdobj fstream)

;;;; Strings

(defheader "string"
    (;; string type
     string))

(defstdobj string)

(defheader "vector"
    (vector))

(defcpp vector (type)
  (format nil "std::vector<~a>"
          (cpp type)))

;; c_str method
(defcpp c-str ()
  "c_str")

;;;; Standard lib from C
(defheader "cstdlib"
    (;; reading numbers from c-strings
     atoi
     atof))

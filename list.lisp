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

;; These functions support lists which can be read from files by read,
;; not recursively stored due to non-printable elements.

(defcppfun (pointer (vector double)) read_list
    ((var string filename))
  (var stringstream ss)
  (varcons ifstream infile
           (method filename c-str))
  (var char c)
  (>> infile c)
  (var double d)
  (var (pointer (vector double)) result
       (new (vector double) 0))
  (while (>> infile d)
    (pmethod result push-back d))
  (method infile close)
  (return result))

(defcppfun void write_list
    ((var (reference (vector double)) lst)
     (var string filename))
  (varcons ofstream outfile
           (method filename c-str))
  (<< outfile (str "("))
  (<< outfile (aref lst 0))
  (var int lstsize (method lst size))
  (for (var int i 1) (< i lstsize) (incf i)
       (<< outfile (str " ") (aref lst i)))
  (<< outfile (str ")") endl)
  (method outfile close)
  (return))

(defmethod cpp-loader ((obj cons))
  'read_list)

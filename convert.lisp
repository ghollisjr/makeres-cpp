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

(defun root2hdf (name root-file-path hdf-file-path)
  "Converts a ROOT file with a TTree located at root-file-path to an
HDF5 file containing the TTree information located at hdf-file-path
with root2hdf script"
  (let* ((tmp-path
          (cut-newline (sh mktemp
                           "-d" "-p" ".")))
         (tmpdir
          (make-pathname :directory
                         (list :relative tmp-path)))
         (convpath
          (merge-pathnames "converter"
                           tmpdir)))
    (with-input-from-string (s root-file-path)
      (run "root2hdf"
           (list name
                 convpath)
           :output *standard-output*
           :input s))
    (run convpath
         (list root-file-path
               hdf-file-path))
    (sb-ext:delete-directory tmpdir :recursive t)
    hdf-file-path))

;;;; Conversion from Lisp target results to C++:
;;;;
;;;; cpp-loader should return the symbol identifying the C++ function
;;;; responsible for interpreting Lisp values stored on disk.

(defgeneric cpp-loader (obj)
  (:documentation "Returns name of C++ function to read object from
  path"))

(defcpp res (id)
  (let ((*print-pretty* nil))
    (format nil "~a(~s)"
            (string-downcase
             (string (cpp-loader (resfn id))))
            (namestring (target-path id "data")))))

;;; Due to Lisp's float and double-float syntax, it is necessary to
;;; provide functions which parse floats and doubles, and to use these
;;; functions for processing results.

;; Basic utility function for reading a file's contents into a single
;; string:
(defcppfun string read_file ((var string path))
  (varcons ifstream infile
           (method path c-str))
  (var stringstream ss)
  (var string line)
  (while (getline infile line)
    (<< ss line endl))
  (method infile close)
  (return (method ss stringstream.str)))

;; Utility function that replaces all f and d characters with e:

(defcppfun string replace_fd ((var string s))
  (var int nchars (method s length))
  (var string result s)
  (for (var int i 0) (< i nchars) (incf i)
       (if (or (= (aref result i)
                  "'d'")
               (= (aref result i)
                  "'f'"))
           (setf (aref result i)
                 "'e'")))
  (return result))

;;; Some basic loaders:

;; Doubles
(defcppfun double read_double ((var string path))
  (var double result)
  (varcons stringstream ss
           (replace_fd (read_file path)))
  (>> ss result)
  (return result))
(defcppfun void write_double ((var double x)
                              (var string path))
  (varcons ofstream outfile
           (method path c-str))
  (<< outfile x endl)
  (method outfile close)
  (return))

(defmethod cpp-loader ((obj double-float))
  'read_double)

;; Float
(defcppfun float read_float ((var string path))
  (var float result)
  (varcons stringstream ss
           (replace_fd (read_file path)))
  (>> ss result)
  (return result))
(defcppfun void write_float ((var float x)
                             (var string path))
  (varcons ofstream outfie
           (method path c-str))
  (<< outfile x endl)
  (method outfile close)
  (return))

(defmethod cpp-loader ((obj single-float))
  'read_float)

;; Integers (long only)
(defcppfun long read_long ((var string path))
  (var long result)
  (varcons ifstream infile
           (method path c-str))
  (>> infile result)
  (method infile close)
  (return result))
(defcppfun void write_long ((var long x)
                            (var string path))
  (varcons ofstream outfile
           (method path c-str))
  (<< outfile x endl)
  (method outfile close)
  (return))

(defmethod cpp-loader ((obj integer))
  'read_long)

;; Strings:
(defcppfun string read_string ((var string path))
  (var stringstream result)
  (varcons ifstream infile
           (method path c-str))
  (var int state 0)
  (var char c)
  ;; Possible states:
  ;;
  ;; 0. Search for initial quote
  ;; 1. Reading
  ;; 2. Found escape backslash
  ;; 3. Found ending quote
  (while (and (>> infile c)
              (not (= state 3)))
    (cond
      ((= state 0)
       (when (= c
                "'\"'")
         (setf state 1)))
      ((= state 1)
       (cond
         ((= c "'\\\\'")
          (setf state 2))
         ((= c "'\"'")
          (setf state 3))
         (t
          (<< result c))))
      ((= state 2)
       (<< result c)
       (setf state 1))))
  (method infile close)
  (return (method result stringstream.str)))
(defcppfun string write_string
    ((var string x)
     (var string path))
  (varcons ofstream outfile
           (method path c-str))
  (<< outfile
      "'\"'")
  (<< outfile
      x
      "'\"'"
      endl)
  (method outfile close)
  (return))

(defmethod cpp-loader ((obj string))
  'read_string)

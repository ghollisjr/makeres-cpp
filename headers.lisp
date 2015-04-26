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

(defparameter *headers*
  (make-hash-table :test 'equal))

(defparameter *cheaders*
  (make-hash-table :test 'equal))

(defun defheader-fn (fname provided
                     &key
                       flags)
  "Defines a C++ header by its filename fname, the C++ objects it
provides, and the flags necessary for compiling and linking.

flags must be a string of compile & link flags for your C++ compiler."
  (setf (gethash fname *headers*)
        (list :provided provided
              :flags (if flags
                         flags
                         ""))))

(defmacro defheader (fname provided
                     &key
                       flags)
  "Macro version of defheader-fn"
  `(defheader-fn ',fname ',provided
     ,@(when flags `(:flags ,flags))))

(defun defcheader-fn (fname provided
                      &key
                        flags)
  "Defines C header by its file name, the provided C objects, and the
compile and link flags needed for your C++ compiler."
  (setf (gethash fname *cheaders*)
        (list :provided provided
              :flags (if flags
                         flags
                         ""))))

(defmacro defcheader (fname provided
                      &key
                        flags)
  "Macro version of defcheader-fn"
  `(defcheader-fn ',fname ',provided
     ,@(when flags `(:flags ,flags))))

(defun generate-cpp->header ()
  "Returns map from cpp to header for that cpp symbol"
  (let ((result (make-hash-table :test 'eq)))
    (loop
       for header being the hash-keys in *headers*
       for val being the hash-values in *headers*
       do (destructuring-bind (&key provided flags) val
            (loop
               for p in provided
               do (setf (gethash p result)
                        header))))
    result))

(defun generate-cpp->cheader ()
  "Returns map from cpp to cheader for that cpp symbol"
  (let ((result (make-hash-table :test 'eq)))
    (loop
       for cheader being the hash-keys in *cheaders*
       for val being the hash-values in *cheaders*
       do (destructuring-bind (&key provided flags) val
            (loop
               for p in provided
               do (setf (gethash p result)
                        cheader))))
    result))

(defun required-headers (form)
  "Returns the list of required headers for a form"
  (let ((cpp->header (generate-cpp->header)))
    (labels ((rec (f)
               (let ((result nil))
                 (cond
                   ((null f)
                    nil)
                   ((listp f)
                    (let ((sub-results (mapcar #'rec f)))
                      (loop
                         for i in sub-results
                         when i
                         do (setf result
                                  (mapcar #'car
                                          (compress
                                           (append result
                                                   i)
                                           :test #'equal
                                           :singleton-pairs t))))))
                   ((atom f)
                    (when (gethash f cpp->header)
                      (setf result
                            (list (gethash f cpp->header))))))
                 result)))
      (rec form))))

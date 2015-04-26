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

(defparameter *proj->bin-path*
  (make-hash-table :test 'equal)
  "Map from project to binary directory path for project")

(defun defprog-fn ()
  "Defines C++ program"
  )

(defmacro defprog ()
  "Defines a C++ program"
  )

(defparameter *proj->cpp*
  (make-hash-table :test 'equal)
  "Map from project to map from opsymbol to opfunction.")

(defun ensure-project ()
  (when (not (gethash *project-id* *proj->cpp*))
    (setf (gethash *project-id* *proj->cpp*)
          (make-hash-table :test 'eq))))

(defmacro defcpp (opname lambda-list &body body)
  "Defines an operator in the current project which takes arguments as
specified in lambda-list and executes the body.  Body must return the
resulting string for the generated C++ code.

&body will be replaced with &rest in the lambda-list since it will be
supplied to the lambda operator."
  (alexandria:with-gensyms (ops)
    `(symbol-macrolet ((,ops (gethash *project-id*
                                      *proj->cpp*)))
       (ensure-project)
       (setf (gethash ',opname ,ops)
             (lambda ,(mapcar (lambda (token)
                                (if (eq token '&body)
                                    '&rest
                                    token))
                              lambda-list)
               ,@body)))))

;; Rules for cpp function:
;;
;; If form is an atom, then if there is a defined cpp generation
;; function for that atom it is called.  Else, the atom is passed to
;; mkstr and the resulting string is downcased and returned.
;;
;; If form is a list, then the first element in the list is searched
;; in the map, and the function found in the map is called on the
;; remaining elements in the form.  If the first element is not found
;; in the map, then an error is thrown.

(defun cpp (form)
  "Looks up either 1. Form if form is an atom, or 2. First element of
form if form is a list in the operator table *proj->cpp* and calls the
function mapped there with no arguments for atoms or the remaining
arguments in the form for lists.  If there is no defined operator
corresponding to the first element of the list, standard function call
notation is assumed and the symbol is downcased as a string for the
function name."
  (when form
    (let ((sym->fn (gethash *project-id* *proj->cpp*)))
      (if (atom form)
          (aif (gethash form sym->fn)
               (funcall it)
               (if (stringp form)
                   form
                   (string-downcase (mkstr form))))
          (aif (gethash (first form) sym->fn)
               (apply it
                      (rest form))
               (format nil "~a(~{~a~^,~})"
                       (string-downcase (string (first form)))
                       (mapcar #'cpp (rest form))))))))

(defgeneric lisp->cpp (lisp-object &rest args)
  (:documentation "Generates a C++ string for the Lisp object.  args
  can be used along with side effects to ensure that the C++ string is
  sensible.")
  (:method ((x float) &rest args)
    (map 'string
         (lambda (char)
           (if (or (char= char #\d)
                   (char= char #\f))
               #\e
               char))
         (format nil "~e" x)))
  (:method ((x integer) &rest args)
    (format nil "~d" x))
  (:method ((x string) &rest args)
    (with-output-to-string (out)
      (format out "\"")
      (loop
         for char across x
         do (if (char= char #\Newline)
                (format out "\\n")
                (format out "~a" char)))
      (format out "\"")))
  (:method (x &rest args)
    (format nil "~a" x)))

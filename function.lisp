;;;; makeres-cpp is a Common Lisp data analysis library.
;;;; Copyright 2015, 2016 Gary Hollis
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

(defparameter *cpp-alt-funs*
  (make-hash-table :test 'eq)
  "Map from C++ function symbol to alternative definitions")

(defparameter *explicit-funs*
  (make-hash-table :test 'equal)
  "Map from C++ function symbol to explicit function symbol
dependencies.")

(defun set-explicit-cpp-functions (symbol functions &optional (op :set))
  "Defines explicit function dependencies.  op can be :set, :add,
or :reset."
  (symbol-macrolet ((fns (gethash symbol *explicit-funs*)))
    (case op
      (:set (setf fns functions))
      (:add (setf fns (append functions fns)))
      (:reset (setf fns nil)))
    symbol))

(defun explicit-cpp-functions (symbol)
  "Returns explicit C++ function dependencies of that symbol."
  (values (gethash symbol *explicit-funs*)))

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

(defmacro defcppaltfun (type fname cpp-args &body body)
  (alexandria:with-gensyms (fnam typ cas bod altfuns)
    `(let ((,typ ',type)
           (,fnam ',fname)
           (,cas ',cpp-args)
           (,bod ',body))
       (symbol-macrolet ((,altfuns (gethash ,fnam
                                            *cpp-alt-funs*)))
         (setf ,altfuns
               (adjoin 
                (list :type ,typ
                      :cpp-args ,cas
                      :body ,bod)
                ,altfuns
                :test #'equal))))))

(defun undefcppaltfuns (fname)
  (setf (gethash fname *cpp-alt-funs*) nil))

;; (defun required-functions (code)
;;   "Returns list of required functions for code"
;;   (list->set
;;    (cond
;;      ((null code) nil)
;;      ((listp code)
;;       (cond
;;         ((eq (first code)
;;              'res)
;;          (required-functions `(,(cpp-loader (resfn (second code)))
;;                                 (str ""))))
;;         ((eq (first code)
;;              'eval)
;;          nil)
;;         (t
;;          (apply #'append
;;                 (required-functions (first code))
;;                 (mapcar #'required-functions (rest code)))))
;;       ;; (apply #'append
;;       ;;        (required-functions (first code))
;;       ;;        (mapcar #'required-functions (rest code)))
;;       )
;;      ((atom code)
;;       (if (gethash code *cpp-funs*)
;;           (list code)
;;           nil)))
;;    #'eq))

(defun required-functions (code)
  "Returns list of required functions for code"
  (let ((traversed (make-hash-table :test 'eq)))
    (labels ((rec (code)
               ;; recurses through code
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
                     ((gethash (first code) traversed)
                      nil)
                     (t
                      (setf (gethash (first code) traversed)
                            t)
                      (apply #'append
                             (required-functions (first code))
                             (mapcar #'required-functions (rest code)))))
                   ;; (apply #'append
                   ;;        (required-functions (first code))
                   ;;        (mapcar #'required-functions (rest code)))
                   )
                  ((atom code)
                   (cond
                     ((gethash code traversed)
                      nil)
                     ((gethash code *cpp-funs*)
                      (append
                       ;; Automatic:
                       (list* code
                              (rec `(progn ,@(getf (gethash code *cpp-funs*)
                                                   :body))))
                       ;; Explicit function dependencies:
                       (explicit-cpp-functions code)))
                     (t nil))))
                #'eq)))
      (rec code))))

(defun definition (fsym)
  "Returns C++ code for the definition of a function."
  (destructuring-bind (&key type cpp-args body)
      (gethash fsym *cpp-funs*)
    `(function ,type ,fsym ,cpp-args ,@body)))

(defun prototype (function-code)
  "Returns C++ code for the prototype of a function definition"
  (with-output-to-string (out)
    (destructuring-bind (type fname cpp-args &rest body)
        (rest function-code)
      (format out "~a ~a(~{~a~^,~})"
              (cpp type)
              (cpp fname)
              (mapcar #'cpp cpp-args)))))

(defun altdefinitions (fsym)
  (loop
     for def in (gethash fsym *cpp-alt-funs*)
     collecting
       (destructuring-bind (&key type cpp-args body)
           def
         `(function ,type ,fsym ,cpp-args ,@body))))

(defun altprototypes (altdefs)
  (loop
     for function-code in altdefs
     collecting
       (with-output-to-string (out)
         (destructuring-bind (type fname cpp-args &rest body)
             (rest function-code)
           (format out "~a ~a(~{~a~^,~})"
                   (cpp type)
                   (cpp fname)
                   (mapcar #'cpp cpp-args))))))

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

(defun program-source (&rest top-level-forms)
  "Returns full program source code (minus headers) given top-level
forms"
  (let* (;; many functions require proper code
         (raw-progned-forms
          (cons 'progn top-level-forms))
         (required-functions
          (required-functions raw-progned-forms))

         ;; Prototyping required:
         (explicit-function-forms
          (remove-if-not (lambda (form)
                           (eq (first form)
                               'function))
                         top-level-forms))
         (implicit-function-forms
          (mapcar (lambda (fsym)
                    (destructuring-bind (&key type cpp-args body)
                        (gethash fsym *cpp-funs*)
                      `(function ,type ,fsym ,cpp-args ,@body)))
                  required-functions))
         (function-forms (append implicit-function-forms
                                 explicit-function-forms))
         (prototypes
          (mapcar #'prototype function-forms))
         (non-function-forms
          (remove-if (lambda (form)
                       (eq (first form)
                           'function))
                     top-level-forms)))
    (append prototypes
            non-function-forms
            function-forms)))

(defun program-fn (&rest top-level-forms)
  "Returns string for the entire C++ program consisting of the
top-level forms preceded by any required headers"
  (let* (;; many functions require proper code
         (raw-progned-forms
          (cons 'progn top-level-forms))
         (required-functions
          (required-functions raw-progned-forms))

         ;; Prototyping required:
         (explicit-function-forms
          (remove-if-not (lambda (form)
                           (eq (first form)
                               'function))
                         top-level-forms))
         (implicit-function-forms
          (mapcar (lambda (fsym)
                    (destructuring-bind (&key type cpp-args body)
                        (gethash fsym *cpp-funs*)
                      `(function ,type ,fsym ,cpp-args ,@body)))
                  required-functions))
         (function-forms (append implicit-function-forms
                                 explicit-function-forms))
         (prototypes
          (mapcar #'prototype function-forms))
         (non-function-forms
          (remove-if (lambda (form)
                       (eq (first form)
                           'function))
                     top-level-forms))
         (progned-forms
          (cons 'progn
                (append function-forms
                        non-function-forms)))
         (headers
          (required-headers progned-forms))
         (cheaders
          (required-cheaders progned-forms)))
    (with-output-to-string (out)
      (loop
         for hs in headers
         do (if (atom hs)
                (format out "#include<~a>~%" hs)
                (loop
                   for h in hs
                   do (format out "#include<~a>~%" hs))))
      (loop
         for chs in cheaders
         do (if (atom chs)
                (format out "extern \"C\" {~%#include<~a>~%}~%" chs)
                (loop
                   for ch in chs
                   do (format out "extern \"C\" {~%#include<~a>~%}~%" ch))))
      (loop
         for expr in prototypes
         do (format out "~a;~%" expr))
      (loop
         for expr in non-function-forms
         do (format out "~a;~%" (cpp expr)))
      (loop
         for expr in function-forms
         do (format out "~a~%" (cpp expr))))))

(defmacro program (&body top-level-forms)
  `(program-fn ,@(loop
                    for expr in top-level-forms
                    collecting `',expr)))

(defun exe-fn (exe-path top-level-forms
               &key
                 arguments
                 flags
                 input
                 output
                 (error *error-output*))
  "Executes code using exe-path as the path to the executable and
exe-path.cc as the source code path.  flags should be a list of
strings used as additional arguments to the compiler/linker."
  (let* ((source-path
          (string-append (namestring exe-path) ".cc"))
         (source-string (apply #'program-fn top-level-forms))
         (required-flags (compile-flags
                          (cons 'progn
                                (apply #'program-source top-level-forms)))))
    (ensure-directories-exist exe-path)
    (with-open-file (source-file source-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (format source-file "~a" source-string))
    (let ((ret
           (second
            (multiple-value-list
             (external-program:run
              "g++"
              (flet ((unique (list)
                       (nreverse
                        (reduce (lambda (x y)
                                  (adjoin y x :test #'equal))
                                list
                                :initial-value nil))))
                (unique
                 (append
                  flags
                  (let ((rf
                         (split-sequence:split-sequence #\space
                                                        required-flags)))
                    (if (equal rf '(""))
                        ()
                        rf))
                  (list "-o" exe-path source-path))))
              :output *standard-output*
              :error error)))))
      (when (not (zerop ret))
        (error "Error Compiling ~a.cc~%g++ return value ~a" exe-path ret)))
    (let ((ret
           (second
            (multiple-value-list
             (external-program:run exe-path
                                   arguments
                                   :output output
                                   :error *error-output*
                                   :input input)))))
      (when (not (zerop ret))
        (error "Error Executing ~a~%Return value ~a" exe-path ret)))))

(defmacro exe (exe-path top-level-forms
               &key
                 arguments
                 flags
                 input
                 output
                 (error '*error-output*))
  "Macro version of exe-fn.  Only argument not evaluated is
top-level-forms"
  `(exe-fn ,exe-path ',top-level-forms
           ,@(when arguments
                   `(:arguments ,arguments))
           ,@(when flags
                   `(:flags ,flags))
           ,@(when input
                   `(:input ,input))
           ,@(when output
                   `(:output ,output))
           ,@(when error
                   `(:error ,error))))

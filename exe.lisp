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

(defun program-fn (&rest top-level-forms)
  "Returns string for the entire C++ program consisting of the
top-level forms preceded by any required headers"
  (let ((headers
         (required-headers (cons 'progn top-level-forms)))
        (cheaders
         (required-cheaders (cons 'progn top-level-forms))))
    (with-output-to-string (out)
      (loop
         for h in headers
         do (format out "#include<~a>~%" h))
      (loop
         for ch in cheaders
         do (format out "extern \"C\" {~%#include<~a>~%}~%"
                    ch))
      (loop
         for expr in top-level-forms
         do (format out "~a;~%" (cpp expr))))))

(defmacro program (&body top-level-forms)
  `(program-fn ,@(loop
                    for expr in top-level-forms
                    collecting `',expr)))

(defun exe-fn (exe-path top-level-forms
               &key
                 arguments
                 flags
                 input
                 output)
  "Executes code using exe-path as the path to the executable and
exe-path.cc as the source code path.  flags should be a list of
strings used as additional arguments to the compiler/linker."
  (let* ((source-path
          (string-append (namestring exe-path) ".cc"))
         (source-string (apply #'program-fn top-level-forms))
         (required-flags (compile-flags (cons 'progn top-level-forms))))
    (ensure-directories-exist exe-path)
    (with-open-file (source-file source-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (format source-file "~a" source-string))
    (external-program:run "g++"
                          (append
                           flags
                           (let ((rf
                                  (split-sequence:split-sequence #\space
                                                                 required-flags)))
                             (if (equal rf '(""))
                                 ()
                                 rf))
                           (list "-o" exe-path source-path))
                          :output *standard-output*)
    (external-program:run exe-path
                          arguments
                          :output output
                          :input input)))

(defmacro exe (exe-path top-level-forms
               &key
                 arguments
                 flags
                 input
                 output)
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
                   `(:output ,output))))

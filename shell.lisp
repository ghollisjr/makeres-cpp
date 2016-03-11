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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-str (x)
    "Provides a shell-like treatment of symbols.  Symbols starting
with $ are evaluated, others are returned as downcased strings."
    (cond
      ((symbolp x)
       (let ((package (symbol-package x))
             (str (string x)))
         (if (char= (elt str 0)
                    #\$)
             (intern (subseq str 1)
                     package)
             (string-downcase str))))
      (t x)))

  ;; Shell utilities:
  (defmacro sh (command &rest args)
    (let ((command
           (to-str command))
          (s (gensym)))
      `(with-output-to-string (,s)
         (run ,command (list ,@args)
              :output ,s))))

  (defun cd (dir)
    (sb-posix:chdir dir))

  (defun pwd ()
    (sb-posix:getcwd))

  (defun cut-newline (string)
    (let ((length (length string)))
      (if (not (zerop length))
          (if (char= #\Newline (elt string (1- length)))
              (subseq string 0 (1- length))
              string)
          "")))

  (defun mktemp (&optional delete-p)
    (let ((result
           (cut-newline
            (sh mktemp))))
      (when delete-p
        (delete-file result))
      result))

  (defun mktempdir ()
    (cut-newline (sh mktemp "-d")))

  (defmacro pipe (&rest commands)
    "Each command should be a list starting with the command string or
symbol to be lowercased followed by the arguments to that command
which will be evaluated.  The output of the previous command is piped
into the input of the next command."
    (let* ((commands
            (mapcar (lambda (command)
                      (cons (to-str (first command))
                            (rest command)))
                    commands))
           (functions
            (mapcar (lambda (command)
                      (alexandria:with-gensyms (stream s)
                        `(lambda (,stream)
                           (with-output-to-string (,s)
                             (run ,(first command) (list ,@(rest command))
                                  :input ,stream
                                  :output ,s)))))
                    commands)))
      (alexandria:with-gensyms (fns left right stream)
        `(let ((,fns
                (list ,@functions)))
           (reduce (lambda (,left ,right)
                     (with-input-from-string (,stream ,left)
                       (funcall ,right ,stream)))
                   ,fns
                   :initial-value "")))))

  ;; (defun lines (x)
  ;;   "Convnert string into list of lines"
  ;;   (remove ""
  ;;           (split-sequence:split-sequence
  ;;            #\Newline
  ;;            (cut-newline x))
  ;;           :test #'string=))

  ;; (defun unlines (x)
  ;;   "Convert list of lines into string"
  ;;   (apply #'string-append (intersperse (format nil "~%") x)))
  )

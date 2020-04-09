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

(require 'cl-ana)
(require 'makeres-cpp)

;;;; Test project section:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage test
    (:use :cl
          :makeres-cpp))

  (cl-ana.package-utils:use-package-group :cl-ana :test))

(in-package :test)

(defproject makeres-cpp
    "/home/ghollisjr/test/makeres-cpp/smallproject"
  (list #'macrotrans #'branchtrans #'tabletrans #'cpptrans #'progresstrans)
  (fixed-cache 5))

(set-cpp-work-path "/home/ghollisjr/test/makeres-cpp/small-cpp-work")

(setf *cpp-print-progress* 10000)
(setf *print-progress* 10000)

(defparameter *cpp-paths*
  (mapcar #'namestring
          (directory "/home/ghollisjr/phd/data/skims/e1e/deuteron/a1ntp_36*.skim")))

(defparameter *hdf5-paths*
  (mapcar #'namestring
          (directory "/home/ghollisjr/phd/data/hdf/skims/e1e/deuteron/a1ntp_36*.h5")))

(defres src
  (cpp-srctab ;; (list "/home/ghollisjr/test/a1ntp_36516_pass1.a00.rzn.root.skim")
   ;; *cpp-paths*
   ;; (subseq *cpp-paths* 0 50)
   (subseq *cpp-paths* 0 5)
   "h10"))

(defres filtered
  (cpp-ltab (res src)
      ()
    (let ((int num_neutral 0)
          (int num_positive 0)
          (int num_negative 0))
      (for (var int i 0) (< i (field |gpart|)) (incf i)
           (cond
             ((< (aref (field |q|) i)
                 0)
              (incf num_negative))
             ((= (aref (field |q|) i)
                 0)
              (incf num_neutral))
             (t (incf num_positive))))
      (when (and (= num_positive 1)
                 (= num_negative 2)
                 (or (= num_neutral 0)
                     (= num_neutral 1)))
        (push-fields)))))

(defres canon
  (cpp-tab (res filtered)
      (list (list "gpart" "Int_t")
            (list "p" "Float_t" :length "gpart" :max-length "4")
            (list "b" "Float_t" :length "gpart" :max-length "4"))
      ()
      (work-path "filtered.root")
    (setf (ofield |gpart|)
          (field |gpart|))
    (for (var int i 0) (< i (field |gpart|)) (incf i)
         (setf (aref (ofield |p|)
                     i)
               (aref (field |p|)
                     i))
         (setf (aref (ofield |b|)
                     i)
               (aref (field |b|)
                     i)))
    (push-fields)))

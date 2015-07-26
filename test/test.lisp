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
    "/home/ghollisjr/test/makeres-cpp/project"
  (list #'macrotrans #'branchtrans #'tabletrans #'cpptrans #'progresstrans)
  (fixed-cache 5))

(ensure-cpp-table-binding-ops)
(ensure-cpp-table-op-expanders)

(set-cpp-work-path "/home/ghollisjr/test/makeres-cpp/cpp-work")

(setf *cpp-print-progress* 10000)

(defres src
  (cpp-srctab (list "/home/ghollisjr/test/a1ntp_36516_pass1.a00.rzn.root.skim")
              "h10"))

(defres (src p b)
  (cpp-dotab (res src)
      ((varcons TH2D (uniq hist)
                (str (uniq hist)) (str (uniq hist))
                100 0d0 3d0
                100 0d0 1.2d0)
       (vararray string (uniq names) (2)
                 (str "p")
                 (str "b")))
      ((varcons TFile (uniq file)
                (str (eval (work-path "src-p-b.root")))
                (str "RECREATE"))
       (method (uniq file) cd)
       (method (uniq hist) write)
       (method (uniq file) root-close)
       (write_histogram (address (uniq hist))
                        2
                        (str (eval (work-path "src-p-b.h5")))
                        (uniq names)))
      (load-object 'sparse-histogram
                   (eval (work-path "src-p-b.h5")))
    (for (var int i 0) (< i (field |gpart|)) (incf i)
         ;; (<< cout
         ;;     (aref (field |p|) i)
         ;;     (str " ")
         ;;     (aref (field |b|) i)
         ;;     endl)
         (method (uniq hist) fill
                 (aref (field |p|) i)
                 (aref (field |b|) i)))))

(defres filtered
  (cpp-tab (res src)
      (list (list "gpart" "Int_t")
            (list "p" "Float_t" :length "gpart" :max-length "4")
            (list "b" "Float_t" :length "gpart" :max-length "4"))
      ()
      (work-path "filtered.root")
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
        (push-fields)))))

(defres (filtered p b)
  (cpp-dotab (res filtered)
      ((varcons TH2D (uniq hist)
                (str (uniq hist)) (str (uniq hist))
                100 0d0 3d0
                100 0d0 1.2d0)
       (vararray string (uniq names) (2)
                 (str "p")
                 (str "b")))
      ((varcons TFile (uniq file)
                (str (eval (work-path "filtered-p-b.root")))
                (str "RECREATE"))
       (method (uniq file) cd)
       (method (uniq hist) write)
       (method (uniq file) root-close)
       (write_histogram (address (uniq hist))
                        2
                        (str (eval (work-path "filtered-p-b.h5")))
                        (uniq names)))
      (load-object 'sparse-histogram
                   (eval (work-path "filtered-p-b.h5")))
    (for (var int i 0) (< i (field |gpart|)) (incf i)
         (method (uniq hist) fill
                 (aref (field |p|) i)
                 (aref (field |b|) i)))))

(defres (filtered subset)
  (cpp-ltab (res filtered) ()
    (when (< (aref (field |p|) 0)
             0.4d0)
      (push-fields))))

(defres (filtered subset p b)
    (cpp-dotab (res (filtered subset))
               ((varcons TH2D (uniq hist)
                         (str (uniq hist)) (str (uniq hist))
                         100 0d0 3d0
                         100 0d0 1.2d0)
                (vararray string (uniq names) (2)
                          (str "p")
                          (str "b")))
        ((varcons TFile (uniq file)
                  (str (eval (work-path "filtered-subset-p-b.root")))
                  (str "RECREATE"))
         (method (uniq file) cd)
         (method (uniq hist) write)
         (method (uniq file) root-close)
         (write_histogram (address (uniq hist))
                                 2
                                 (str (eval (work-path "filtered-subset-p-b.h5")))
                                 (uniq names)))
               (load-object 'sparse-histogram
                            (eval (work-path "filtered-subset-p-b.h5")))
               (for (var int i 0) (< i (field |gpart|)) (incf i)
                    (method (uniq hist) fill
                            (aref (field |p|) i)
                            (aref (field |b|) i)))))

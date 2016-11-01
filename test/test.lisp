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

(ensure-table-binding-ops)
(ensure-table-op-expanders)
(ensure-cpp-table-binding-ops)
(ensure-cpp-table-op-expanders)

(set-cpp-work-path "/home/ghollisjr/test/makeres-cpp/cpp-work")

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
   (subseq *cpp-paths* 0 50)
   "h10"))

(defcpphist (src p b)
    (res src)
    '((:name "p"
       :nbins 100
       :low 0d0
       :high 3d0)
      (:name "b"
       :nbins 100
       :low 0d0
       :high 1.2d0))
    ()
  (for (var int i 0) (< i (field |gpart|)) (incf i)
       (hins (aref (field |p|) i)
             (aref (field |b|) i))))

(defres (src hdf5)
  (srctab (hdf-chain-opener ;; *hdf5-paths*
           ;; (subseq *hdf5-paths* 0 50)
           (subseq *hdf5-paths* 0 1)
           :group "/h10")))

(defres ((src hdf5) p b)
  (dotab (res (src hdf5))
      ((hist (make-shist '((:name "p" :nbins 100 :low 0d0 :high 2d0)
                           (:name "b" :nbins 100 :low 0d0 :high 1.2d0)))))
      hist
    (loop
       for i below (field |gpart|)
       do (hins hist
                (list (aref (field |p|) i)
                      (aref (field |b|) i))))))

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

(defres (convert (filtered hdf5))
  (root2hdf (root-table-name (res filtered))
            (first
             (root-table-paths (res filtered)))
            (work-path "filtered.h5")))

(defres (filtered hdf5)
  (srctab (hdf-chain-opener (list (work-path "filtered.h5"))
                            :group "/FILTERED")
          (res (convert (filtered hdf5)))))

(defres ((filtered hdf5) p b)
  (dotab (res (filtered hdf5))
      ((hist (make-shist '((:name "p" :nbins 100 :low 0d0 :high 2d0)
                           (:name "b" :nbins 100 :low 0d0 :high 1.2d0)))))
      hist
    (loop
       for i below (field |gpart|)
       do
         (hins hist
               (list (aref (field |p|) i)
                     (aref (field |b|) i))))))

(defcpphist (filtered p b)
    (res filtered)
    '((:name "p"
       :nbins 100
       :low 0d0
       :high 3d0)
      (:name "b"
       :nbins 100
       :low 0d0
       :high 1.2d0))
    ()
  (for (var int i 0) (< i (field |gpart|)) (incf i)
       (hins (aref (field |p|) i)
             (aref (field |b|) i))))

(defcpphist (filtered b)
    (res filtered)
    '((:name "b"
       :nbins 100
       :low 0d0
       :high 1.2d0))
    ()
  (for (var int i 0) (< i (field |gpart|)) (incf i)
       (hins (aref (field |b|) i))))

(defres (filtered subset)
  (cpp-ltab (res filtered) ()
    (when (< (aref (field |p|) 0)
             0.4d0)
      (push-fields))))

(defcpphist (filtered subset p b)
    (res (filtered subset))
    '((:name "p"
       :nbins 100
       :low 0d0
       :high 3d0)
      (:name "b"
       :nbins 100
       :low 0d0
       :high 1.2d0))
    ()
  (for (var int i 0) (< i (field |gpart|)) (incf i)
       (hins (aref (field |p|) i)
             (aref (field |b|) i))))

(defcpphist (filtered subset p b test)
    (res (filtered subset))
    '((:name "p"
       :nbins 100
       :low 0d0
       :high 2d0)
      (:name "b"
       :nbins 100
       :low 0d0
       :high 1.2d0))
    ()
  (for (var int i 0) (< i (field |gpart|)) (incf i)
       (hins (aref (field |p|)
                   i)
             (aref (field |b|)
                   i))))

(defres (src 4part)
  (cpp-ltab (res src) ()
    (when (>= (field |gpart|)
              2)
      (push-fields))))

(defcpphist (src 4part p)
    (res (src 4part))
    '((:name "p1"
       :nbins 50
       :low 0d0
       :high 2d0)
      (:name "p2"
       :nbins 50
       :low 0d0
       :high 2d0)
      (:name "p3"
       :nbins 50
       :low 0d0
       :high 2d0)
      (:name "p4"
       :nbins 50
       :low 0d0
       :high 2d0))
    ()
  (hins (aref (field |p|) 0)
        (aref (field |p|) 0)
        (aref (field |p|) 1)
        (aref (field |p|) 1)))

(defres (saved (src p b))
  (save-object (res (src p b))
               (work-path "src-p-b.h5"))
  t)

(defres (saved (src 4part p))
  (save-object (res (src 4part p))
               (work-path "src-4part.h5"))
  t)

(defres conversion-test
  (exe (work-path "exe/convert-test")
       ((function int main ()
                  (var (pointer TH2D) hist
                       (typecast (pointer TH2D)
                                 (res (src p b))))
                  (var int nbins
                       (* (pmethod hist nbinsx)
                          (pmethod hist nbinsy)))
                  (for (var int i 0) (< i nbins) (incf i)
                       (var double content
                            (pmethod hist get-bin-content i))
                       (when (> content 0)
                         (<< cout content
                             endl)))))
       :output *standard-output*))

(defres list
  (list 1 2 3 4 5))

(defres list-conversion-test
  (exe (work-path "exe/list-conversion-test")
       ((function int main ()
                  (var (pointer (vector double))
                       vec
                       (res list))
                  (var int size
                       (pmethod vec size))
                  (for (var int i 0) (< i size) (incf i)
                       (<< cout (aref (value vec) i) endl))))
       :output *standard-output*))

;; (defres read-2d-test
;;   (when (res (saved (src p b)))
;;     (exe (work-path "exe/read-2d-test")
;;          ((function int main ()
;;                     (var (pointer TH2D) hist
;;                          (typecast (pointer TH2D)
;;                                    (read_histogram
;;                                     (str
;;                                      (eval
;;                                       (work-path "src-p-b.h5"))))))
;;                     (var int nbins
;;                          (* (pmethod hist nbinsx)
;;                             (pmethod hist nbinsy)))
;;                     (for (var int i 0) (< i nbins) (incf i)
;;                          (var double content
;;                               (pmethod hist get-bin-content i))
;;                          (when (> content 0)
;;                            (<< cout content endl)))))
;;          :output *standard-output*)))

;; (defres read-4d-test
;;   (when (res (saved (src 4part p)))
;;     (exe (work-path "exe/read-4d-test")
;;          ((function int main ()
;;                     (<< cout (str
;;                               (eval
;;                                (work-path "src-4part.h5")))
;;                         endl)
;;                     (var (pointer THnSparseD) hist
;;                          (typecast (pointer THnSparseD)
;;                                    (read_histogram
;;                                     (str
;;                                      (eval
;;                                       (work-path "src-4part.h5"))))))
;;                     (var int nbins (pmethod hist axis.nbins))
;;                     (for (var int i 0) (< i nbins) (incf i)
;;                          (var double content
;;                               (pmethod hist get-bin-content i))
;;                          (when (> content 0)
;;                            (<< cout content endl)))))
;;          :output *standard-output*)))

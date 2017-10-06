;;;; makeres-cpp is a Common Lisp data analysis library.
;;;; Copyright 2017 Gary Hollis
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

(defun nested-list-descriptor (tree)
  "Crawls through the tree returning a list of lengths which allow the
nested list to be reconstructed from a flattened form."
  (let* ((result nil))
    (labels ((rec (tr)
               (cond
                 ((listp tr)
                  (push (length tr) result)
                  (mapcar #'rec tr))
                 (t
                  ;; negative numbers are always safe to use for
                  ;; denoting an atom, since 0 denotes an empty list.
                  (push -1 result)))))
      (rec tree))
    (nreverse result)))

;; (progn
;; (defun unflatten-nested-list (flat desc)
;;   "Unflattens a flattened list using the nested list descriptor desc."
;;   (let* ((result nil)
;;          (flat (copy-list flat))
;;          (desc (copy-list desc)))
;;     (labels (;; Worker function: Reverse nested lists
;;              (nested-reverse (x)
;;                (if (atom x)
;;                    x
;;                    (reverse
;;                     (mapcar #'nested-reverse x))))
;;              (rec (state &optional res)
;;                (cond
;;                  ;; empty description
;;                  ((null desc)
;;                   (nested-reverse result))
;;                  ;; Process next input
                 
;; )))))))

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

(defun listify (x)
  (if (listp x)
      x
      (list x)))

(defun cpp-serialize-map (map)
  "Returns a list containing the serialized contents of the map.  map
is presumed to be a map which maps from a tensor of doubles to a
tensor of doubles.  tensor can have rank 0 for scalar values."
  (let ((alist (map->alist map)))
    (when alist
      (let* ((first (first alist))
             (k (car first))
             (ksize (tensor-dimensions k))
             (v (cdr first))
             (vsize (tensor-dimensions v)))
        (values
         ;; Serialized map:
         (loop
            for i in alist
            appending
              (append (tensor-flatten (car i))
                      (tensor-flatten (cdr i))))
         ;; Specs for key and value sizes
         ksize
         vsize)))))

(defun cpp-unserialize-map (serialized key-dims val-dims)
  (let* ((key-size (if key-dims
                       (product key-dims)
                       1))
         (val-size (if val-dims
                       (product val-dims)
                       1))
         (chunk-size (+ key-size val-size))
         (key (when key-dims
                (make-tensor key-dims :type 'list)))
         (val (when val-dims
                (make-tensor val-dims :type 'list)))
         (result nil)
         (state 0))
    (loop
       for x in serialized
       for i from 0
       for chunk = (floor i chunk-size)
       do (let* ((chunk-i
                  (mod i chunk-size)))
            ;; State machine:
            (case state
              ;; 0. Read key
              (0
               (if key-dims
                   (setf (tensor-flat-ref key chunk-i)
                         x)
                   (setf key x))
               ;; Adjust state
               (when (>= (1+ chunk-i) key-size)
                 (incf state))
               )
              ;; 1. Read value
              (1
               ;; adjust index
               (setf chunk-i
                     (- chunk-i key-size))
               (if val-dims
                   (setf (tensor-flat-ref val chunk-i)
                         x)
                   (setf val x))
               ;; Adjust state
               (when (>= (1+ chunk-i) val-size)
                 (incf state)))
              ;; 2. Push result
              (2
               ;; Push cons into result
               (push (cons key val)
                     result)
               ;; Reset state
               (setf state 0)))))
    (reverse result)))

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
(progn
(defun unflatten-nested-list (flat desc)
  "Unflattens a flattened list using the nested list descriptor desc."
  (let* ((result nil)
         (flat (copy-list flat))
         (desc (copy-list desc)))
    (labels (;; Worker function: Reverse nested lists
             (nested-reverse (x)
               (if (atom x)
                   x
                   (reverse
                    (mapcar #'nested-reverse x))))
             (rec (state &optional res)
               (cond
                 ;; empty description
                 ((null desc)
                  (nested-reverse result))
                 ;; Process next input
                 
)))))))

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

(defvar *proj->cpp-tab->lfields*
  (make-hash-table :test 'equal)
  "Map from table id to any lfields defined via deflfields.")

;; logical lfield definition:
(defun cpp-deflfieldsfn (table-id lfields
                         &key
                           (op :set))
  "function version of cpp-deflfields.  op can be :set, :add-front
or :add-back"
  (when (not (gethash (project) *proj->cpp-tab->lfields*))
    (setf (gethash (project) *proj->cpp-tab->lfields*)
          (make-hash-table :test 'equal)))
  (symbol-macrolet ((lfs (gethash table-id
                                  (gethash (project)
                                           *proj->cpp-tab->lfields*))))
    (case op
      (:set
       (setf lfs
             (remove-duplicates lfields :test #'equal)))
      (:add-front
       (setf lfs
             (remove-duplicates (append lfields lfs)
                                :test #'equal)))
      (:add-back
       (setf lfs
             (remove-duplicates (append lfs
                                        lfields)
                                :test #'equal)))))
  nil)

;; and the macro:
(defmacro cpp-deflfields (table-id &body lfields)
  "Sets logical fields for table-id; unlike deflfields, fields are not
referenced via field but by their symbols alone."
  `(cpp-deflfieldsfn ',table-id ',lfields))

;; Physical table reductions:
(defmacro cpp-tab (source fields-types inits path
                   &body body)
  "Operator for generating physical tables via cpp-table-pass.

source is the source table to be iterated over.

fields-types is a list of (field type &rest counts) describing the
field names, types and for arrays the dimensionalities of the columns
in the output table.

inits are used for bindings outside the table-pass loop.

body will have push-field operator available, accepting all arguments
to table-push-field minus the destination table (will be supplied the
result table)."
  nil)

;; Logical table reductions:
(defmacro cpp-ltab (source inits &body body)
  "Like tab, but for logical tables.  Returns nil.  Requires special
  treatment since logical tables don't yield a result.  Arguments are
  simply for tracking the logical table."
  nil)

;; Example usage: Histograms could be saved to file, then the return
;; form would read the histogram from file and erase the file, finally
;; returning the histogram.  Alternatively, finding a way to use
;; interprocess communication to transmit the histogram such as pipes
;; or Unix sockets could provide a better option for local computation
;; at least.  Writing to file and then reading again is closer to what
;; is needed for JLab code however, and all I would need to do is find
;; a way to avoid double-writing the histogram data itself and instead
;; just write the target metadata.
(defmacro cpp-dotab (source inits posts return &body body)
  "return should be a Lisp form which handles cleaning up or opening
  the Lisp object to return and returning it as the output value."
  nil)

(defun cpp-srctab (paths name &optional bootstrap)
  "Returns ROOT table object for use as source table"
  (declare (ignore bootstrap))
  (make-root-table :paths paths
                   :fields-types
                   (read-fields-types paths name)
                   :nrows (read-nrows paths name)
                   :name name))

;;;; NOTES
;;;;
;;;; * lfields have a different syntax than that of the makeres-table
;;;;   operators.  The lfields are just general C++ expressions and
;;;;   should occur in the exact order in which they should be
;;;;   executed.  There is no need to use the field operator to
;;;;   reference lfields, scoping will be used to gaurantee distinct
;;;;   references.  Additionally, it may become useful or necessary to
;;;;   add an set of cleanup forms to call at the end of the pass
;;;;   loop.
;;;;
;;;;   This destroys the ability to control which lfields are
;;;;   evaluated, but at the moment the only other options would
;;;;   require quite a bit of work.  I need something now.

;; general purpose table iteration, more functional than do-table,
;; used as implementation backbone for all makeres-table
;; transformations
(defun cpp-table-pass (ttree-paths ttree-name exe-path
                       fields-types
                       inits posts lfields
                       &rest body)
  "Loops over table with external bindings inits and final execuation
forms posts, executing body once per row.

Any dependent lfields should always be given after their dependencies.

fields-types must be a list with elements of the following
form: (field-symbol type &rest counts)

macro field yields the field value of current row.

long integer variable row_number yields the row number of current row.

long integer variable nrows yields the number of events in the source
table.

Limitations: Make sure no forms (field X) occur which are not meant to
reference the field value.  I've tried various options to make this
work via macros but nothing short of code walking looks viable, and
this is my naive code walking strategy's fault.

When used with makeres, each cpp-table-pass is guaranteed to have
independent lfields and inits, no matter what symbol names you choose.
If you need a common lfield, use deflfields.  If you need a common
init binding, at the moment the only solution is to combine the
targets manually (usually more conceptually clear incidentally)."
  (labels (;; Compares symbols as if they're in the same package
           (sym-equal (x y)
             (tree-equal x y
                         :test (lambda (x y)
                                 (or (and (symbolp x)
                                          (symbolp y)
                                          (equal (keywordify x)
                                                 (keywordify y)))
                                     (equal x y))))))
    (let* ((tab (gsym))
           (fields
            (list->set
             (append
              (cl-ana.makeres::find-dependencies lfields 'field)
              (cl-ana.makeres::find-dependencies body 'field))
             #'sym-equal))
           (field->symbol
            (let ((result (make-hash-table)))
              (loop
                 for f in fields
                 do (setf (gethash (keywordify f) result)
                          (gsym)))
              result))
           (field->type
            (let ((result (make-hash-table)))
              (loop
                 for ft in fields-types
                 do (destructuring-bind (field type &rest counts)
                        ft
                      (setf (gethash (keywordify field) result)
                            (cons type counts))))
              result))
           (unfielded-body
            (sublis (loop
                       for field being the hash-keys in field->symbol
                       for symbol being the hash-values in field->symbol
                       collecting
                         (let ((counts
                                (second (gethash (keywordify field)
                                                 field->type))))
                           (if counts
                               (cons `(field ,field)
                                     symbol)
                               (cons `(field ,field)
                                     `(value ,symbol)))))
                    body
                    :test #'sym-equal))
           (unfielded-lfields
            (sublis (loop
                       for field being the hash-keys in field->symbol
                       for symbol being the hash-values in field->symbol
                       collecting
                         (let ((counts
                                (second (gethash (keywordify field)
                                                 field->type))))
                           (if counts
                               (cons `(field ,field)
                                     symbol)
                               (cons `(field ,field)
                                     `(value ,symbol)))))
                    lfields
                    :test #'sym-equal)))
      (exe-fn exe-path
              `((function
                 int main ()
                 (varcons tchain ,tab (str ,ttree-name) (str ,ttree-name))
                 ,@(loop
                      for path in ttree-paths
                      collecting `(method ,tab add-file (str ,path)))
                 ,@(loop
                      for field being the hash-keys in field->symbol
                      for symbol being the hash-values in field->symbol
                      appending
                      ;; need handler-case here detecting destructuring
                      ;; bind failure
                        (destructuring-bind (type &rest counts)
                            (gethash (keywordify field) field->type)
                          `((var (pointer ,type) ,symbol
                                 ,(if counts
                                      `(new[] ,type ,@counts)
                                      `(new ,type)))
                            (method ,tab
                                    set-branch-address
                                    (str ,(string field))
                                    ,symbol))))
                 ,@inits

                 ;; main loop
                 (var long nrows
                      (method ,tab
                              entries))
                 (for (var long row_number 0)
                      (< row_number nrows)
                      (incf row_number)
                      (method ,tab get-event
                              row_number)
                      ,@unfielded-lfields
                      ,@unfielded-body)

                 ;; posts
                 ,@posts
                 ;; Cleanup:
                 (exit 0)))
              :output *standard-output*))))

;;;; Special operators

;; (defmacro defhist (id src expr init
;;                    &key (test nil test-supplied-p))
;;   "Defines a histogram reduction of table src with expr inserted into
;; the result histogram.  test is a form which, when evaluated in the
;; table pass body should return non-nil for events to be inserted into
;; the histogram."
;;   (alexandria:with-gensyms (hist)
;;     `(defres ,id
;;        (dotab (res ,src)
;;            ((,hist ,init))
;;            ,hist
;;          ,(if test-supplied-p
;;               `(when ,test
;;                  (hins ,hist ,expr))
;;               `(hins ,hist ,expr))))))

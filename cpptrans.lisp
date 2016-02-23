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

(declaim (optimize (debug 3)))

(defvar *cpp-print-progress* nil
  "Set this to nil if you don't want to see progress messages printed;
set this to an integer value to specify the number of rows at which to
print a progress update message.  Note that this should only be used
for tables which know their size (so CSV tables don't work with
this).")

(defun cpp-work-path (&rest format-args)
  "Creates pathname under *cpp-work-path* using format args.  Use
empty string to get top cpp work directory."
  (let* ((raw-pathname (apply #'format
                              nil
                              format-args))
         (pathname
          (merge-pathnames raw-pathname
                           (make-pathname
                            :directory *cpp-work-path*))))
    (ensure-directories-exist pathname)
    pathname))

(defun purgecpp ()
  "Deletes all cpp work contents"
  #+sbcl
  (sb-ext:delete-directory (cpp-work-path "")
                           :recursive t)
  (cpp-work-path ""))

(defun cpp-exe-path ()
  (merge-pathnames "exe"
                   (make-pathname :directory *cpp-work-path*)))

(defun cpp-table-reduction? (expr)
  "True if expr is a cpp-dotab, cpp-ltab or cpp-tab expression"
  (when (and expr
             (listp expr))
    (destructuring-bind (progn &rest forms) expr
      (when (listp (first forms))
        (let ((tab-op (first (first forms))))
          (member tab-op (list ;; 'cpp-table-pass
                          'cpp-dotab
                          'cpp-ltab
                          'cpp-tab)
                  :test 'eq))))))

(defun cpp-table-pass? (expr)
  "True if expr is a cpp-table-pass expression"
  (when expr
    (destructuring-bind (progn &rest forms) expr
      (let ((tab-op (first (first forms))))
        (eq tab-op 'cpp-table-pass)))))

(defun cpp-dotab? (expr)
  "True if expr is a cpp-dotab expression"
  (when expr
    (destructuring-bind (progn &rest forms) expr
      (let ((tab-op (first (first forms))))
        (eq tab-op 'cpp-dotab)))))

(defun cpp-tab? (expr)
  "True if expr is a tab expression"
  (when (and expr
             (listp expr))
    (destructuring-bind (progn &rest forms) expr
      (when (listp (first forms))
        (let ((tab-op (first (first forms))))
          (eq tab-op 'cpp-tab))))))

(defun cpp-ltab? (expr)
  "True if expr is an cpp-ltab expression"
  (when (and expr
             (listp expr))
    (destructuring-bind (progn &rest forms) expr
      (when (listp (first forms))
        (let ((tab-op (first (first forms))))
          (eq tab-op 'cpp-ltab))))))

(defun resform? (expr)
  "Returns true if expr is of the form (res x)"
  (and (listp expr)
       (eq (first expr)
           'res)))

(defun unres (expr)
  "Gets id from a res form if it is a res form, if not, returns expr."
  (if (resform? expr)
      (second expr)
      expr))

(defun mkres (expr)
  "Ensures that expr is a res form"
  (if (resform? expr)
      expr
      (list 'res expr)))

(defun cpp-table-reduction-source (expr)
  "Returns source for table reduction, nil if expr is not of a
table-reduction."
  (when (cpp-table-reduction? expr)
    (cadadr expr)))

(defun (setf cpp-table-reduction-source) (value expr)
  (when (cpp-table-reduction? expr)
    (setf (cadadr expr)
          value)))

(defun cpp-table-reduction-inits (expr)
  "Returns init bindings when expr is a table-reduction, nil
otherwise."
  (when (cpp-table-reduction? expr)
    (destructuring-bind (progn tab-form) expr
      (cond
        ((cpp-table-pass? expr)
         (elt tab-form 5))
        ((cpp-tab? expr)
         (elt tab-form 3))
        (t
         (elt tab-form
              2))))))

;; call on cpp-dotab only
(defun cpp-table-reduction-return (expr)
  (when (cpp-dotab? expr)
    (destructuring-bind (progn tab-form) expr
      (elt tab-form 4))))

(defun cpp-table-reduction-posts (expr)
  (when (cpp-dotab? expr)
    (destructuring-bind (progn form) expr
      (elt form 3))))

(defun cpp-table-reduction-body (expr)
  (when (cpp-table-reduction? expr)
    (destructuring-bind (progn tab-form) expr
      (cond
        ((cpp-table-pass? expr)
         (nthcdr 8 tab-form))
        ((cpp-dotab? expr)
         (nthcdr 5 tab-form))
        ((cpp-tab? expr)
         (nthcdr 5 tab-form))
        ((cpp-ltab? expr)
         (nthcdr 3 tab-form))))))

(defun cpp-tab-fields-types (expr)
  (when (cpp-tab? expr)
    (destructuring-bind (progn form) expr
      (elt form 2))))

(defun cpp-tab-path (expr)
  (when (cpp-tab? expr)
    (destructuring-bind (progn form) expr
      (elt form 4))))

(defun immediate-reductions (target-table tab)
  "Returns list of immediately dependent table reductions for a
table"
  (remove-if-not (lambda (id)
                   (let* ((tar (gethash id target-table))
                          (expr (target-expr tar)))
                     (and (cpp-table-reduction? expr)
                          (equal (unres (cpp-table-reduction-source expr))
                                 tab))))
                 (hash-keys target-table)))

(defun cpp-ltab-chains (target-table src)
  "Returns all cpp-ltab chains stemming from src.  A cpp-ltab chain is
simply a list of cpp-ltab ids which are chained reductions of either
cpp-ltabs or the source table with id src along with the first
non-cpp-ltab id which is a reduction of the next to last cpp-ltab in
the chain."
  (labels ((rec (red &optional chain)
             (if (cpp-ltab? (target-expr (gethash red target-table)))
                 (let* ((imms (immediate-reductions target-table red)))
                   (mapcan (lambda (r)
                             (copy-list (rec r (cons red chain))))
                           imms))
                 (list (reverse (cons red chain))))))
    (mapcan (lambda (r)
              (rec r (list src)))
            (immediate-reductions target-table src))))

(defun cpp-ltab-chained-reductions (target-table src)
  "Returns all reductions directly from cpp-ltab chains stemming from
src"
  (mapcar #'alexandria:last-elt
          (cpp-ltab-chains target-table src)))

(defun necessary-pass-reductions (target-table tab)
  "Returns list of reductions of a table which must be computed via a
pass over the table; equivalent to the union set of all immediate
non-cpp-ltab reductions and any reductions chained directly to tab via
logical tables."
  (list->set
   (append (remove-if (lambda (id)
                        (cpp-ltab?
                         (target-expr
                          (gethash id target-table))))
                      (immediate-reductions target-table tab))
           (cpp-ltab-chained-reductions target-table tab))
   #'equal))

(defun chained-reductions (target-table src)
  "Returns list of ids for targets from target-table which are
connected via a chain of reductions from src."
  (let ((imm-reds (immediate-reductions target-table src)))
    (when imm-reds
      (append imm-reds
              (mapcan (lambda (red)
                        (chained-reductions target-table red))
                      (remove-if-not
                       (lambda (red)
                         (cpp-table-reduction?
                          (target-expr (gethash red target-table))))
                       imm-reds))))))

(defun group-ids-by-pass (target-table src
                          &key
                            dep<
                            (test (constantly t)))
  "Groups all ids from target-table according the the pass required
over src using the dependency checker dep<."
  (let* ((dep< (if dep<
                   dep<
                   (dep< target-table)))
         (chained
          (chained-reductions target-table src))
         (sorted-ids
          (remove-if-not (lambda (x)
                           (and (member x chained :test #'equal)
                                (funcall test x)))
                         (depsort-graph target-table dep<))))
    (when sorted-ids
      (let ((pass (list (pop sorted-ids)))
            (result nil))
        (labels ((rec ()
                   (dolist (i sorted-ids)
                     (when (every (lambda (p)
                                    (funcall dep<
                                             i
                                             p))
                                  pass)
                       (push i pass)))
                   (push (reverse pass) result)
                   (setf sorted-ids
                         (remove-if (lambda (sid)
                                      (member sid pass
                                              :test #'equal))
                                    sorted-ids))
                   (if sorted-ids
                       (progn
                         (setf pass (list (pop sorted-ids)))
                         (rec))
                       (nreverse result))))
          (rec))))))

(defun ultimate-source-tables (target-table &optional ignore)
  "Returns list of source table ids which are not table reductions of
non-ignored sources."
  (let ((srcs nil)
        (reds nil))
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (let ((expr (target-expr tar)))
            (when (cpp-table-reduction? expr)
              (let* ((raw-src (cpp-table-reduction-source expr))
                     (src (if (and (listp raw-src)
                                   (eq (first raw-src) 'res))
                              (second raw-src)
                              raw-src)))
                (when (not (member src
                                   ignore
                                   :test #'equal))
                  (push id reds)
                  (setf srcs
                        (adjoin src
                                srcs
                                :test #'equal)))))))
    (set-difference srcs reds :test #'equal)))

(defun removed-source-dep< (target-table)
  (let ((depmap (make-hash-table :test 'equal)))
    (labels ((rec (id)
               ;; returns full list of dependencies for id, ignoring
               ;; t-stat targets (otherwise we neglect possible
               ;; optimizations)
               (let* ((src
                       (if (cpp-table-reduction?
                            (target-expr (gethash id target-table)))
                           (unres
                            (cpp-table-reduction-source
                             (target-expr (gethash id target-table))))
                           nil))
                      (deps
                       (remove-if
                        (lambda (d)
                          (and (not (equal d src))
                               (target-stat (gethash d target-table))))
                        (copy-list (target-deps (gethash id target-table))))))
                 ;; (format t "rec-id: ~a, deps: ~a~%"
                 ;;         id deps)
                 (when deps
                   (list->set
                    (reduce (lambda (ds d)
                              (adjoin d ds :test #'equal))
                            (mapcan #'rec
                                    (append
                                     ;; immediate dependencies
                                     deps
                                     ;; dependencies from source
                                     (let ((expr
                                            (target-expr (gethash id target-table))))
                                       (when (cpp-table-reduction? expr)
                                         (target-deps
                                          (gethash
                                           (unres
                                            (cpp-table-reduction-source expr))
                                           target-table))))))
                            :initial-value
                            (let ((expr
                                   (target-expr (gethash id target-table))))
                              (if (cpp-table-reduction? expr)
                                  (append
                                   (destructuring-bind (progn tab-form) expr
                                     (cl-ana.makeres::find-dependencies
                                      (cddr tab-form)
                                      'res))
                                   (target-deps
                                    (gethash
                                     (unres
                                      (cpp-table-reduction-source expr))
                                     target-table)))
                                  deps)))
                    #'equal)))))
      (loop
         for id being the hash-keys in target-table
         do (setf (gethash id depmap)
                  (rec id)))
      (lambda (x y)
        (not (member y (gethash x depmap)
                     :test #'equal))))))

(defun removed-cpp-ltab-source-dep< (target-table)
  (let ((depmap (make-hash-table :test 'equal)))
    (labels ((rec (id)
               ;; returns full list of dependencies for id, ignoring
               ;; t-stat dependencies (otherwise we neglect possible
               ;; optimizations).
               (let* ((src
                       (if (cpp-table-reduction?
                            (target-expr (gethash id target-table)))
                           (unres
                            (cpp-table-reduction-source
                             (target-expr (gethash id target-table))))
                           nil))
                      (deps
                       (remove-if
                        (lambda (d)
                          (and (not (equal d src))
                               (target-stat (gethash d target-table))))
                        (copy-list (target-deps (gethash id target-table))))))
                 (when deps
                   (list->set
                    (reduce (lambda (ds d)
                              (adjoin d ds :test #'equal))
                            (mapcan #'rec
                                    (append
                                     ;; immediate dependencies
                                     deps
                                     ;; dependencies from source
                                     (let ((expr
                                            (target-expr (gethash id target-table))))
                                       (when (cpp-table-reduction? expr)
                                         (target-deps
                                          (gethash
                                           (unres
                                            (cpp-table-reduction-source expr))
                                           target-table))))))
                            :initial-value
                            (let ((expr
                                   (target-expr (gethash id target-table))))
                              (if (cpp-ltab? expr)
                                  (append
                                   (destructuring-bind (progn tab-form) expr
                                     (cl-ana.makeres::find-dependencies (cddr tab-form)
                                                                        'res))
                                   (target-deps
                                    (gethash
                                     (unres
                                      (cpp-table-reduction-source expr))
                                     target-table)))
                                  deps)))
                    #'equal)))))
      (loop
         for id being the hash-keys in target-table
         do (setf (gethash id depmap)
                  (rec id)))
      (lambda (x y)
        (not (member y (gethash x depmap)
                     :test #'equal))))))

;;; Table pass expression components

;; Context tree functions:
(shadowing-import 'cl-ana.makeres-table::node)
(shadowing-import 'cl-ana.makeres-table::node-id)
(shadowing-import 'cl-ana.makeres-table::node-content)
(shadowing-import 'cl-ana.makeres-table::node-children)
(shadowing-import 'cl-ana.makeres-table::tree-ids)
(shadowing-import 'cl-ana.makeres-table::node-subcontent)

;; must be given complete target graph, not just the null-stat targets
(defun cpp-table-reduction-context-tree (graph src ids)
  "Returns tree of contexts each pass would be inside if collapsed up
to src.  Physical table reductions are treated as reductions with
themselves as context."
  (let (;; map from source to immediate reductions used by ids
        (source->reds (make-hash-table :test 'equal)))
    (labels ((build-source->reds (id)
               ;; builds map from source table to immediate reductions
               (when (and (gethash id graph)
                          (not (equal id src)))
                 (let ((source (unres
                                (cpp-table-reduction-source
                                 (target-expr
                                  (gethash id graph))))))
                   (setf (gethash source source->reds)
                         (adjoin id (gethash source source->reds)
                                 :test #'equal))
                   (when (not (equal source src))
                     (build-source->reds source)))))
             (source->tree (source)
               ;; generates context tree for ids from source
               ;;
               ;; context tree nodes consist of source, content, and
               ;; child nodes.
               ;;
               ;; content is list of immediate reduction ids
               (let* (;; all reductions encountered
                      (reds (gethash source source->reds))
                      ;; reductions which need to be placed in contexts
                      (need-context-reds
                       (append (remove-if
                                (lambda (red)
                                  (or (target-stat (gethash red graph))
                                      (cpp-ltab? (target-expr (gethash red graph)))))
                                reds)
                               (when (and (gethash source graph)
                                          (cpp-tab? (target-expr (gethash source graph)))
                                          (member source ids :test #'equal))
                                 (list source))))
                      ;; reductions used as sources
                      (source-reds
                       (remove-if-not (lambda (red)
                                        (gethash red source->reds))
                                      reds))
                      ;; child nodes:
                      (children (mapcar #'source->tree source-reds)))
                 (apply #'node
                        source
                        ;; remove any reds needing context which are
                        ;; covered by some child node:
                        (let ((subcontent
                               (list->set
                                (mapcan (lambda (x)
                                          (copy-list (node-subcontent x)))
                                        children)
                                #'equal)))
                          (remove-if (lambda (r)
                                       (member r subcontent
                                               :test #'equal))
                                     need-context-reds))
                        children)))
             (cpp-tab-cleanup (tree)
               ;; Ensures that the tree obtained from source->tree
               ;; properly stores physical table nodes.  Physical
               ;; table nodes should never be content of the immediate
               ;; source table, they should be child nodes which
               ;; contain at least themselves as content.
               (let* ((source (node-id tree))
                      (content (node-content tree))
                      (newcontent (remove-if (lambda (id)
                                               (and (cpp-tab?
                                                     (target-expr (gethash id graph)))
                                                    (not (equal id source))))
                                             content))
                      (cpp-tabs (remove-if-not (lambda (id)
                                                 (and (cpp-tab?
                                                       (target-expr (gethash id graph)))
                                                      (not (equal id source))))
                                               content))
                      (children (copy-tree (node-children tree))))
                 ;; modify children appropriately
                 (loop
                    for cpp-tab in cpp-tabs
                    do (loop
                          for child in children
                          when (and (equal (node-id child)
                                           cpp-tab)
                                    (not (member cpp-tab (node-content child))))
                          do (progn
                               (push cpp-tab (node-content child))
                               (return))
                          finally (push (node cpp-tab (list cpp-tab))
                                        children)))
                 ;; return result
                 (apply #'node
                        source
                        newcontent
                        (mapcar #'cpp-tab-cleanup
                                children)))))
      (mapcar #'build-source->reds ids)
      (cpp-tab-cleanup
       (source->tree src)))))

(shadowing-import 'cl-ana.makeres-table::find-push-fields)
(shadowing-import 'cl-ana.makeres-table::replace-push-fields)

;; must be given complete target graph, not just the null-stat targets
(defun make-pass-target-expr (graph src pass)
  "Return expression for pass target over src, collapsing all results
from pass up to src."
  (flet ((htref (ht &rest keys)
           ;; looks up values stored in nested hash tables, one key
           ;; per hash table layer
           (reduce (lambda (h k)
                     (gethash k h))
                   keys
                   :initial-value ht)))
    ;; Context handling:
    (let* (;; tree of contexts, each node contains context name,
           ;; reduction ids needing to be placed in this context, and
           ;; sub context trees.
           (context-tree
            (cpp-table-reduction-context-tree graph src pass))
           ;; set of reductions generated:
           (nodes
            (remove src
                    (list->set
                     (labels
                         ((rec (n)
                            (cons (node-id n)
                                  (append
                                   (node-content n)
                                   (mapcan #'rec
                                           (node-children n))))))
                       (rec context-tree)))))
           (reductions
            (remove src
                    (list->set
                     (labels
                         ((rec (n)
                            (let ((subcontent
                                   (append
                                    (node-content n)
                                    (mapcan #'rec
                                            (node-children n)))))
                              (if (cpp-tab? (target-expr
                                             (gethash (node-id n)
                                                      graph)))
                                  subcontent
                                  (cons (node-id n)
                                        subcontent)))))
                       (rec context-tree)))))
           ;; map from reduction id to map from uniq variable to gsym
           (reduction->uniqsym->gsym
            (make-hash-table :test 'equal))
           ;; map from reduction to return form:
           (reduction->return
            (make-hash-table :test 'equal)))
      (flet ((replace-uniqsyms (id expr)
               (let ((uniqsym->gsym
                      (gethash id reduction->uniqsym->gsym)))
                 (when uniqsym->gsym
                   (sublis
                    (loop
                       for u being the hash-keys in uniqsym->gsym
                       for gsym being the hash-values in uniqsym->gsym
                       collecting (cons `(uniq ,u) gsym))
                    expr
                    :test #'equal)))))
        ;; Initialize context maps:
        (macrolet ((setht (place k)
                     `(setf (gethash ,k ,place)
                            (make-hash-table :test 'equal))))
          (loop
             for r in nodes
             do (progn
                  (setht reduction->uniqsym->gsym r)
                  (setht reduction->return r))))
        ;; Make maps from uniqsyms to gsyms for each reduction as well
        ;; as map from reduction to return form
        (loop
           for r in nodes
           do (let ((uniqsym->gsym
                     (gethash r reduction->uniqsym->gsym))
                    (tar (gethash r graph))
                    (processed-uniqsym-bindings nil))
                ;; returns:
                (setf (gethash r reduction->return)
                      (let ((expr (target-expr tar)))
                        (when (not (cpp-ltab? expr))
                          (if (cpp-tab? expr)
                              `(make-root-table
                                :paths (list ,(cpp-tab-path expr))
                                :fields-types
                                (read-fields-types (list ,(cpp-tab-path expr))
                                                   ,(mkstr r))
                                :nrows (read-nrows (list ,(cpp-tab-path expr))
                                                   ,(mkstr r))
                                :name ,(mkstr r))
                              (cpp-table-reduction-return expr)))))
                ;; uniqs:
                (loop
                   for uniqsym in (cl-ana.makeres::find-dependencies
                                   (target-expr tar) 'uniq)
                   do (setf (gethash uniqsym uniqsym->gsym)
                            (gsym)))))
        ;; Make body via recursing through context tree

        (let* (;; gsymed field bindings for all cpp-tabs in pass:
               (cpp-tab->fields
                (let ((result (make-hash-table :test 'equal)))
                  (loop
                     for n in nodes
                     do (setf (gethash n result)
                              (cl-ana.makeres::find-dependencies
                               (target-expr (gethash n graph))
                               'ofield)))
                  result))
               ;; gsyms for tree and file for cpp-tabs
               (cpp-tab->file-tree-gsyms
                (let ((result (make-hash-table :test 'equal)))
                  (loop
                     for n in nodes
                     when (cpp-tab? (target-expr (gethash n graph)))
                     do (setf (gethash n result)
                              (list (gsym) (gsym))))
                  result))
               (cpp-tab->field->gsym
                (let ((result (make-hash-table :test 'equal)))
                  (loop
                     for n being the hash-keys in cpp-tab->fields
                     for fields being the hash-values in cpp-tab->fields
                     do
                       (symbol-macrolet ((field->gsym (gethash n result)))
                         (setf field->gsym (make-hash-table :test 'equal))
                         (loop
                            for f in fields
                            do (let ((gsym (gsym)))
                                 (setf (gethash f field->gsym)
                                       gsym)
                                 (setf (gethash (string f) field->gsym)
                                       gsym)))))
                  result))
               (inits
                (loop
                   for r in reductions
                   appending
                     (replace-uniqsyms
                      r
                      (let ((expr (target-expr (gethash r graph))))
                        (append
                         (when (cpp-tab? expr)
                           (let ((file-tree (gethash r
                                                     cpp-tab->file-tree-gsyms))
                                 (pathform `(str (eval ,(cpp-tab-path expr)))))
                             (destructuring-bind (file tree)
                                 file-tree
                               `((varcons TFile ,file
                                          ,pathform
                                          (str "RECREATE"))
                                 (varcons TTree ,tree
                                          (str ,(mkstr r))
                                          (str ,(mkstr r)))
                                 ,@(let ((field->gsym
                                          (gethash r cpp-tab->field->gsym)))
                                        (loop
                                           for field-type in (eval (cpp-tab-fields-types expr))
                                           appending
                                             (destructuring-bind (field type &key length max-length)
                                                 field-type
                                               (let ((field-gsym
                                                      (gethash field
                                                               field->gsym)))
                                                 (append
                                                  (if max-length
                                                      `((vararray ,type ,field-gsym (,max-length)))
                                                      `((var ,type ,field-gsym)))
                                                  `((method ,tree branch
                                                            (str ,(string field))
                                                            ,(if max-length
                                                                 field-gsym
                                                                 `(address ,field-gsym))
                                                            (str ,(if max-length
                                                                      (format nil
                                                                              "~a[~a]/~a"
                                                                              (string field)
                                                                              length
                                                                              (gethash type
                                                                                       *root-branch-type-map*))
                                                                      (format nil
                                                                              "~a/~a"
                                                                              (string field)
                                                                              (gethash type *root-branch-type-map*)))))))))))))))
                         (cpp-table-reduction-inits
                          (target-expr (gethash r graph)))
                         )))))
               ;; posts for all reductions
               (posts
                (loop
                   for r in reductions
                   appending
                     (replace-uniqsyms
                      r
                      (let ((expr (target-expr (gethash r graph))))
                        (cond
                          ((cpp-tab? expr)
                           (let ((file-tree (gethash r
                                                     cpp-tab->file-tree-gsyms)))
                             (destructuring-bind (file tree)
                                 file-tree
                               `((method ,file cd)
                                 (method ,tree write)
                                 (method ,file root-close)))))
                          ((cpp-dotab? expr)
                           (cpp-table-reduction-posts expr))
                          (t nil))))))
               ;; list of result forms making use of any gsymed values:
               (result-list
                (progn
                  `(list
                    ,@(loop
                         for r in pass
                         collect
                           (replace-uniqsyms
                            r
                            (gethash r reduction->return))))))
               ;; Map from cpp-tab to ofields present in expression

               ;; map from table to lfields for table:
               (cpp-tab->lfields
                (gethash (project) *proj->cpp-tab->lfields*))
               ;; lfields expanded:
               (lfields
                ;; lfields from source
                (when cpp-tab->lfields
                  (gethash src cpp-tab->lfields)))
               ;; resulting pass body:
               (body
                (labels
                    ((rec (node)
                       (let* ((c (node-id node))
                              (expr
                               ;; replace uniq first
                               (replace-uniqsyms c (target-expr (gethash c graph))))
                              (lfields
                               (let ((cpp-tab->lfields
                                      (gethash (project) *proj->cpp-tab->lfields*)))
                                 (when cpp-tab->lfields
                                   (gethash c cpp-tab->lfields))))
                              (children-exprs
                               (when (node-children node)
                                 (mapcar #'rec
                                         (node-children node))))
                              (sub-bodies
                               (loop
                                  for fill in (if (equal c src)
                                                  (list nil)
                                                  (find-push-fields expr))
                                  collect
                                  ;; replace (field x) with x for x for every
                                  ;; x in the push-field-bindings
                                    `(progn
                                       ,@(sublis
                                          (append
                                           ;; fields
                                           (loop
                                              for field in (gethash c cpp-tab->fields)
                                              collect
                                                (cons
                                                 `(field ,field)
                                                 (gethash field
                                                          (gethash c
                                                                   cpp-tab->field->gsym))))
                                           ;; ofields
                                           (loop
                                              for field in (gethash c cpp-tab->fields)
                                              collect
                                                (cons
                                                 `(ofield ,field)
                                                 (gethash field
                                                          (gethash c
                                                                   cpp-tab->field->gsym)))))
                                          (append
                                           (mapcar
                                            (lambda (id)
                                              `(progn
                                                 ,@(let*
                                                    ((rawexpr
                                                      (target-expr
                                                       (gethash id graph)))
                                                     (expr
                                                      (replace-uniqsyms
                                                       id rawexpr)))
                                                    (if
                                                     (cpp-tab? expr)
                                                     (let* ((file-tree-gsyms
                                                             (gethash id
                                                                      cpp-tab->file-tree-gsyms))
                                                            (file (first file-tree-gsyms))
                                                            (tree (second file-tree-gsyms)))
                                                       `((method ,tree fill)))
                                                     (cpp-table-reduction-body expr)))))
                                            (node-content node))
                                           children-exprs)
                                          :test #'equal)))))
                         (if (and (not (equal c src))
                                  (cpp-table-reduction? expr))
                             (let ((result
                                    (replace-push-fields
                                     `(progn
                                        ;; Don't want lfields here if
                                        ;; in sub-bodies
                                        ;; 
                                        ;; ,@lfields
                                        ,@(let ((bod
                                                 (cpp-table-reduction-body expr)))
                                               (if (cpp-tab? expr)
                                                   (sublis
                                                    (loop
                                                       for field in (gethash c cpp-tab->fields)
                                                       collect
                                                         (cons
                                                          `(ofield ,field)
                                                          (gethash field
                                                                   (gethash c
                                                                            cpp-tab->field->gsym))))
                                                    bod
                                                    :test #'equal)
                                                   bod)))
                                     ;; Old lfield-bugged version:
                                     ;; sub-bodies
                                     ;; New lfield-fixed version:
                                     (mapcar (lambda (sb)
                                               `(progn ,@lfields ,sb))
                                             sub-bodies)
                                     
                                     )))
                               result)
                             (first sub-bodies)))))
                  (rec context-tree)))
               (row-var (gsym))
               (nrows-var (gsym))
               (print-pass-targets
                (when *cpp-print-progress*
                  `((<< cout
                        (str "Pass over ")
                        (str ,(mkstr src))
                        (str " to compute:")
                        endl)
                    ,@(loop
                         for r in pass
                         collecting
                           `(<< cout (str ,(mkstr r)) endl)))))
               (print-progress-inits
                (when *cpp-print-progress*
                  `((var long ,row-var 0)
                    (var long ,nrows-var
                         (eval (root-table-nrows
                         ,(if (and (listp src)
                                   (eq (first src) 'res))
                              src
                              `(res ,src)))))
                    ;; message specifying what the pass will accomplish
                    ,@print-pass-targets)))
               (print-progress
                (when *cpp-print-progress*
                  `((progn
                      (when (= (mod ,row-var
                                    (eval ,*cpp-print-progress*))
                               0)
                        (<< cout
                            (set-precision 2)
                            fixed
                            (str "Event ") ,row-var
                            (str ", ")
                            (* 1d2
                               (/ (typecast float ,row-var)
                                  (typecast float ,nrows-var)))
                            (str "% complete")
                            endl))
                      (incf ,row-var))))))
          `(progn
             ,@(let ((src-target
                      (if (and (listp src)
                               (eq (first src) 'res))
                          src
                          `(res ,src))))
                    `((cpp-table-pass (root-table-paths
                                       ,src-target)
                                      (root-table-name
                                       ,src-target)
                                      (cpp-exe-path)
                                      (cpp-tab-fields-types->src-fields-types
                                       (root-table-fields-types
                                        ,src-target))
                                      '(,@inits
                                        ,@print-progress-inits)
                                      ',posts
                                      ',lfields
                                      '(progn
                                        ,@print-progress
                                        ,body))
                      ,result-list))))))))

(defun set-pass-result-targets! (result-graph id pass)
  "Sets result-graph targets from pass so that they make use of the
  returned results for the pass target id."
  (loop
     for p in pass
     for i from 0
     do (setf (gethash p result-graph)
              (make-target p `(elt (res ,id)
                                   ,i)
                           :val (target-val (gethash p result-graph))
                           :stat (target-stat (gethash p result-graph)))))
  nil)

(defun ht-filter (fn ht)
  "Returns a new hash table with entries from ht only when fn returns
true when given the key and value from ht."
  (let ((result (make-hash-table :test (hash-table-test ht))))
    (loop
       for k being the hash-keys in ht
       for v being the hash-values in ht
       when (funcall fn k v)
       do (setf (gethash k result)
                v))
    result))

(defparameter *cpp-table-binding-ops*
  (list 'cpp-tab
        'cpp-ltab
        'cpp-dotab))

(defun ensure-cpp-table-binding-ops ()
  (ensure-binding-ops)
  (symbol-macrolet ((binding-ops
                     (gethash (project) *proj->binding-ops*)))
    (setf binding-ops
          (list->set (append binding-ops
                             *cpp-table-binding-ops*)))))

(defun ensure-cpp-table-op-expanders ()
  (symbol-macrolet ((op->expander
                     (gethash (project)
                              *proj->op->expander*)))
    ;; Create table & set expanders for cl:
    (ensure-op-expanders)
    ;; Set table expanders:
    ;; cpp-ltab
    (setf (gethash 'cpp-ltab op->expander)
          (lambda (expander form)
            (destructuring-bind (op source inits &rest body)
                form
              (list* op
                     (funcall expander source)
                     inits
                     body))))
    ;; cpp-tab
    (setf (gethash 'cpp-tab op->expander)
          (lambda (expander form)
            (destructuring-bind (op source fields-types
                                    inits path &rest body)
                form
              (list* op
                     (funcall expander source)
                     fields-types
                     inits
                     (funcall expander path)
                     body))))
    ;; cpp-dotab
    (setf (gethash 'cpp-dotab op->expander)
          (lambda (expander form)
            (destructuring-bind (op source inits posts return &rest body)
                form
              (list* op
                     (funcall expander source)
                     inits
                     posts
                     (funcall expander return)
                     body))))))

(defun cpptrans (target-table)
  "Performs necessary graph transformations for table operators"
  ;; reset gsym map:
  (reset-gsym)
  ;; initialize operator expansion
  (ensure-cpp-table-binding-ops)
  (ensure-cpp-table-op-expanders)

  ;; Setup exe path
  (when (not *cpp-work-path*)
    (error "C++ work path not set"))

  (ensure-directories-exist (cpp-exe-path))

  ;; establish *proj->cpp-tab->lfields*:
  (when (not (gethash (project) *proj->cpp-tab->lfields*))
    (setf (gethash (project) *proj->cpp-tab->lfields*)
          (make-hash-table :test 'equal)))
  (let* ((graph (copy-target-table target-table))
         ;; special dep< for treating reductions as if they did not
         ;; depend on src as a source table, but preserving other
         ;; dependencies as a consequence of being a reduction of the
         ;; source.
         (remsrc-dep<
          (removed-source-dep< target-table))
         ;; special dep< which only adds ltabs sources as dependencies
         ;; when used somewhere other than as the source additionally.
         (remcpp-ltab-dep< (removed-cpp-ltab-source-dep< target-table))
         ;; result
         (result-graph
          (copy-target-table target-table))
         ;; list of source targets already processed
         (processed-srcs nil)
         ;; list of reduction targets already processed:
         (processed-reds nil))
    (labels
        ((trans ()
           (let ((srcs
                  ;; not sure if subtracting processed-srcs is really
                  ;; necessary, but I'm leaving it in until further
                  ;; testing confirms it's unnecessary
                  (set-difference
                   (ultimate-source-tables graph processed-srcs)
                   processed-srcs)))
             (when srcs
               (dolist (src srcs)
                 (push src processed-srcs)
                 (let ((cpp-ltabs
                        (list->set
                         (alexandria:flatten
                          (mapcar #'butlast
                                  (mapcar #'rest
                                          (cpp-ltab-chains target-table
                                                           src)))))))
                   (setf processed-srcs
                         (list->set
                          (append processed-srcs
                                  cpp-ltabs))))
                 (let* (;; reductions which must be computed via a
                        ;; pass over src
                        (nec-reds
                         (remove-if
                          (lambda (k)
                            (target-stat (gethash k graph)))
                          (remove-if
                           (lambda (k)
                             (member k processed-reds
                                     :test #'equal))
                           (necessary-pass-reductions
                            graph src))))
                        ;; necessary passes:
                        (nec-passes
                         (remove
                          nil
                          (mapcar
                           (lambda (pass)
                             (remove-if (lambda (p)
                                          (cpp-ltab? (target-expr (gethash p graph))))
                                        pass))
                           (mapcar
                            (lambda (pass)
                              (remove-if-not (lambda (p)
                                               (member p nec-reds
                                                       :test #'equal))
                                             pass))
                            (group-ids-by-pass
                             graph src
                             :dep< remcpp-ltab-dep<
                             :test (lambda (i)
                                     (not
                                      (or (not (member i nec-reds :test #'equal))
                                          (cpp-ltab? (target-expr (gethash i graph))))))
                             
                             )))))
                        ;; passes relative to ultimate source:
                        (ult-passes
                         (remove
                          nil
                          (mapcar
                           (lambda (pass)
                             (remove-if (lambda (p)
                                          (target-stat (gethash p graph)))
                                        pass))
                           (mapcar
                            (lambda (pass)
                              (remove-if (lambda (p)
                                           (cpp-ltab? (target-expr
                                                       (gethash p graph))))
                                         pass))
                            (mapcar
                             (lambda (pass)
                               (remove-if (lambda (p)
                                            (member p processed-reds
                                                    :test #'equal))
                                          pass))
                             (group-ids-by-pass
                              ;; must remove logical tables and previously
                              ;; processed reduction targets:
                              graph
                              src
                              :dep< remsrc-dep<
                              :test (lambda (i)
                                      (not
                                       (or (member i processed-reds :test #'equal)
                                           (cpp-ltab? (target-expr
                                                       (gethash i graph)))
                                           (target-stat (gethash i graph)))))
                              ))))))
                        ;; collapsible reductions of src:
                        (collapsible-passes
                         (mapcar (lambda (x y)
                                   y)
                                 nec-passes ult-passes)))
                   (dolist (pass collapsible-passes)
                     (dolist (p pass)
                       (push p processed-reds))
                     (let ((id (gensym)))
                       (setf (gethash id result-graph)
                             (make-target id
                                          (make-pass-target-expr
                                           target-table
                                           src
                                           pass)))
                       (set-pass-result-targets!
                        result-graph
                        id
                        pass)))))
               ;; process next sources:
               (trans)))))
      (trans))
    ;; unmodified results are already present, so return result-graph
    result-graph))

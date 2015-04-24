(in-package :makeres-cpp)

(defparameter *proj->bin-path*
  (make-hash-table :test 'equal)
  "Map from project to binary directory path for project")

(defun defprog-fn ()
  "Defines C++ program"
  )

(defmacro defprog ()
  "Defines a C++ program"
  )

(defparameter *proj->cpp*
  (make-hash-table :test 'equal)
  "Map from project to map from opsymbol to opfunction.")

(defun ensure-project ()
  (when (not (gethash *project-id* *proj->cpp*))
    (setf (gethash *project-id* *proj->cpp*)
          (make-hash-table :test 'eq))))

(defmacro defcpp (opname lambda-list &body body)
  "Defines an operator in the current project which takes arguments as
specified in lambda-list and executes the body.  Body must return the
resulting string for the generated C++ code.

&body will be replaced with &rest in the lambda-list since it will be
supplied to the lambda operator."
  (alexandria:with-gensyms (ops)
    `(symbol-macrolet ((,ops (gethash *project-id*
                                      *proj->cpp*)))
       (ensure-project)
       (setf (gethash ',opname ,ops)
             (lambda ,(mapcar (lambda (token)
                                (if (eq token '&body)
                                    '&rest
                                    token))
                              lambda-list)
               ,@body)))))

;; Rules for cpp function:
;;
;; If form is an atom, then if there is a defined cpp generation
;; function for that atom it is called.  Else, the atom is passed to
;; mkstr and the resulting string is downcased and returned.
;;
;; If form is a list, then the first element in the list is searched
;; in the map, and the function found in the map is called on the
;; remaining elements in the form.  If the first element is not found
;; in the map, then an error is thrown.

(defun cpp (form)
  "Looks up either 1. Form if form is an atom, or 2. First element of
form if form is a list in the operator table *proj->cpp* and calls the
function mapped there with no arguments for atoms or the remaining
arguments in the form for lists."
  (when form
    (let ((sym->fn (gethash *project-id* *proj->cpp*)))
      (if (atom form)
          (aif (gethash form sym->fn)
               (funcall it)
               (string-downcase (mkstr form)))
          (aif (gethash (first form) sym->fn)
               (apply (gethash (first form) sym->fn)
                      (rest form))
               (error "cpp: ~a not found in operator table" (first form)))))))

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

(defparameter *proj->ops*
  (make-hash-table :test 'equal)
  "Map from project to map from opsymbol to opfunction.")

(defun ensure-project ()
  (when (not (gethash *project-id* *proj->ops*))
    (setf (gethash *project-id* *proj->ops*)
          (make-hash-table :test 'eq))))

(defmacro defop (opname lambda-list &body body)
  "Defines an operator in the current project which takes arguments as
specified in lambda-list and executes the body.  Body must return the
resulting string for the generated C++ code."
  (alexandria:with-gensyms (ops)
    `(symbol-macrolet ((,ops (gethash *project-id*
                                      *proj->ops*)))
       (ensure-project)
       (setf (gethash ',opname ,ops)
             (lambda ,lambda-list ,@body)))))

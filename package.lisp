(defpackage #:makeres-cpp
  (:use :cl
        :external-program)
  (:export
   ;; C++ transformation for makeres
   :cpptrans
   ;; define a C++ program
   :defprog
   :defprog-fn

   ;; define C++ library
   :deflib))

(cl-ana.package-utils:use-package-group :cl-ana :makeres-cpp)

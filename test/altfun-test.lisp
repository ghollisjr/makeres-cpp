(require 'makeres-cpp)

(defpackage #:test
  (:use :cl
        :makeres-cpp))

(cl-ana.package-utils:use-package-group :cl-ana :test)

(in-package :test)

(defcppfun int square ((var int x))
  (return (* x x)))

(defcppaltfun double square ((var double x))
  (return (* x x)))

(defun test ()
  (exe "/home/ghollisjr/test/altfuntest/exe"
       ((function int main ()
                  ;; this should return 1
                  (<< cout (/ (square 2) 3) endl)
                  ;; and this should return 1.333...
                  (<< cout (/ (square 2d0) 3) endl)))
       :output *standard-output*))

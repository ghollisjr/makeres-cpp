(asdf:defsystem #:makeres-cpp
  :serial t
  :author "Gary Hollis"
  :description "makeres-cpp is a table transformation for makeres from
  cl-ana which allows the generation and compilation of C++ code for
  efficient computation."
  :license "GPLv3"
  :depends-on (#:cl-ana
               #:external-program
               #:alexandria)
  :components ((:file "package")
               ;; makeres transformation stuff
               (:file "cpptrans")
               ;; C++ code compilation & generation stuff
               (:file "cppcode")))

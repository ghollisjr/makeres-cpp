;;;; makeres-cpp is a Common Lisp data analysis library.
;;;; Copyright 2016 Gary Hollis
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

(defmacro defrawcppfun (type name cpp-args body-string
                        &key
                          headers
                          cheaders
                          functions)
  "Defines a C++ function from its type, name, argument list, string
for body, and allows specification of explicit header, cheader and
function dependencies of the function.  No arguments are evaluated."
  `(progn
     (defcppfun ,type ,name ,cpp-args ,body-string)
     ,@(when headers
             `((set-explicit-headers ',name ',headers)))
     ,@(when cheaders
             `((set-explicit-cheaders ',name ',cheaders)))
     ,@(when functions
             `((set-explicit-cpp-functions ',name ',functions)))))

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

(defpackage #:makeres-cpp
  (:use :cl
        :external-program)
  (:export
   ;; Basic C++ utility functions
   :cpp
   ;; C++ transformation for makeres
   :cpptrans
   ;; define a C++ program
   :defprog
   :defprog-fn

   ;; define C++ library
   :deflib

   ;; Program generation and execution
   :exe
   :exe-fn
   :program
   :program-fn

   ;; ROOT tables
   :root-table
   :make-root-table
   :read-fields-types
   :read-nrows
   ;; Table operators
   :cpp-srctab
   :cpp-tab
   :cpp-ltab
   :cpp-dotab
   :cpp-table-pass
   ;; Special reduction operators
   :ofield
   :uniq

   ;; Project settings:
   :*cpp-print-progress*
   :set-cpp-work-path
   :ensure-cpp-table-binding-ops
   :ensure-cpp-table-op-expanders

   ;; C++ Functions
   :read_histogram
   :write_histogram

   ;; ROOT symbols
   ;; :fill
   ;; :write
   ;; :root-close
   ;; :fit
   ;; :draw
   ;; :nbinsx
   ;; :nbinsy
   ;; :nbinsz
   ;; :axis.nbins
   ;; :entries
   ;; :get-event
   ;; :Get
   ;; :GetObject
   ;; :get-bin-center
   ;; :get-bin-xyz
   ;; :get-bin-content
   ;; :set-bin-content
   ;; :bin-error
   ;; :get-bin-error2
   ;; :set-bin-error
   ;; :bin
   ;; :find-bin
   ;; :bin-low-edge
   ;; :bin-up-edge
   ;; :get-sumw2-n
   ;; :get-calculate-errors
   ;; :get-axis
   ;; :x-axis
   ;; :y-axis
   ;; :z-axis
   ;; :Taxis
   ;; :axis
   ;; :th1
   ;; :th1d
   ;; :th1f
   ;; :th1i
   ;; :th2d
   ;; :th2f
   ;; :th2i
   ;; :th3d
   ;; :th3f
   ;; :th3i
   ;; :thnsparsed
   ;; :thnsparsef
   ;; :ttree
   ;; :branch
   ;; :set-branch-address
   ;; :tchain
   ;; :add-file
   ;; :root-open
   ;; :tcanvas
   ;; :trandom
   ;; :trandom1
   ))

(cl-ana.package-utils:use-package-group :cl-ana :makeres-cpp)

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

;;;; General re-used method names:

(defcpp fill () "Fill")
(defcpp write () "Write")
;; have to make different token due to close in fstream
(defcpp root-close () "Close")
(defcpp fit () "Fit") ; don't think I'll use it, but ok
(defcpp draw () "Draw")
(defcpp nbinsx () "GetNbinsX")
(defcpp entries () "GetEntries")
(defcpp Get () "Get")
(defcpp GetObject () "GetObject")

;;; Histogram methods

;; bin-center only meaningful for 1-D
(defcpp bin-center ()
  "GetBinCenter")
(defcpp bin-content ()
  "GetBinContent")
(defcpp bin-error ()
  "GetBinError")
(defcpp bin ()
  "GetBin")

;; Axes
(defcpp x-axis ()
  "GetXaxis")
(defcpp y-axis ()
  "GetYaxis")
(defcpp z-axis ()
  "GetZaxis")

(defparameter *root-flags*
  (remove #\Newline
          (with-output-to-string (out)
            (external-program:run "root-config"
                                  (list "--cflags"
                                        "--libs")
                                  :output out))))

;; macro for making this easier
(defmacro with-root-header (&rest args)
  `(with-defheader ,@args :flags *root-flags*))

;;;; Histograms:

(with-root-header "TH1.h"
  ((defcpp th1 () "TH1")))

;; TH1D
(with-root-header "TH1D.h"
  ((defcpp th1d ()
     "TH1D")))

;; TH1F
(with-root-header "TH1F.h"
  ((defcpp th1f ()
     "TH1F")))

;; TH1I
(with-root-header "TH1I.h"
  ((defcpp th1I ()
     "TH1I")))

;;;; TTrees:
(with-root-header "TTree.h"
  ((defcpp ttree ()
     "TTree")
   (defcpp branch ()
     "Branch")
   (defcpp set-branch-address ()
     "SetBranchAddress")))

;;;; TFiles
(with-root-header "TFile.h"
  ((defcpp tfile ()
     "TFile")
   (defcpp root-open ()
     "Open")))

;;;; TCanvas
(with-root-header "TCanvas.h"
  ((defcpp tcanvas ()
     "TCanvas")))

;;;; TRandom

;; basic
(with-root-header "TRandom.h"
  ((defcpp trandom ()
     "TRandom")))

;; variants
(with-root-header "TRandom1.h"
  ((defcpp trandom1 ()
     "TRandom1")))

(with-root-header "TRandom2.h"
  ((defcpp trandom2 ()
     "TRandom2")))

(with-root-header "TRandom3.h"
  ((defcpp trandom3 ()
     "TRandom3")))

;; methods
(defcpp gaus () "Gaus")

;;;; TLorentzVector


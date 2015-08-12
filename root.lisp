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
(defcpp nbinsy () "GetNbinsY")
(defcpp nbinsz () "GetNbinsZ")
(defcpp axis.nbins () "GetNbins")
(defcpp entries () "GetEntries")
(defcpp get-event () "GetEvent")
(defcpp Get () "Get")
(defcpp GetObject () "GetObject")

;;; Histogram methods

;; bin-center only meaningful for 1-D
(defcpp get-bin-center ()
  "GetBinCenter")
(defcpp get-bin ()
  "GetBin")
(defcpp get-bin-xyz ()
  "GetBinXYZ")
(defcpp get-bin-content ()
  "GetBinContent")
(defcpp set-bin-content ()
  "SetBinContent")
(defcpp bin-error ()
  "GetBinError")
(defcpp get-bin-error2 ()
  "GetBinError2")
(defcpp set-bin-error ()
  "SetBinError")
(defcpp set-bin-error2  ()
  "SetBinError2")
(defcpp bin ()
  "GetBin")
(defcpp find-bin ()
  "FindBin")
(defcpp bin-low-edge ()
  "GetBinLowEdge")
(defcpp bin-up-edge ()
  "GetBinUpEdge")
(defcpp get-sumw2-n ()
  "GetSumw2N")
(defcpp get-calculate-errors ()
  "GetCalculateErrors")

;; Axes
(defcpp get-axis ()
  (format nil "GetAxis"))
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

;;;; Axes
(defheader "TAxis.h"
    (TAxis
     axis))

(defcpp TAxis () "TAxis")

;;;; Histograms:

(with-root-header "TH1.h"
  ((defcpp th1 () "TH1")))

;;; 1-D

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

;;; 2-D

;; TH2D
(with-root-header "TH2D.h"
  ((defcpp th2d ()
     "TH2D")))

;; TH2F
(with-root-header "TH2F.h"
  ((defcpp th2f ()
     "TH2F")))

;; TH2I
(with-root-header "TH2I.h"
  ((defcpp th2I ()
     "TH2I")))

;;; 3-D

;; TH3D
(with-root-header "TH3D.h"
  ((defcpp th3d ()
     "TH3D")))

;; TH3F
(with-root-header "TH3F.h"
  ((defcpp th3f ()
     "TH3F")))

;; TH3I
(with-root-header "TH3I.h"
  ((defcpp th3I ()
     "TH3I")))

;; Sparse
(with-root-header "THnSparse.h"
  ((defcpp THnSparseD ()
     "THnSparseD")
   (defcpp THnSparseF ()
     "THnSparseF")))

;;;; TTrees:
(with-root-header "TTree.h"
  ((defcpp ttree ()
     "TTree")
   (defcpp branch ()
     "Branch")
   (defcpp set-branch-address ()
     "SetBranchAddress")))

(with-root-header "TChain.h"
  ((defcpp tchain ()
     "TChain")
   (defcpp add-file ()
     "AddFile")))

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
(with-root-header "TLorentzVector.h"
  ((defcpp TLorentzVector ()
     "TLorentzVector")))

;;;; 3-vectors
(with-root-header "TVector3.h"
  ((defcpp TVector3 ()
     "TVector3")))

(defcpp angle ()
  "Angle")
(defcpp beta ()
  "Beta")
(defcpp boost ()
  "Boost")
(defcpp boost-vector ()
  "BoostVector")
(defcpp cos-theta ()
  "CosTheta")
(defcpp energy ()
  "Energy")
(defcpp minkowski-norm ()
  "M")
(defcpp minkowski-norm2 ()
  "M2")
(defcpp set-xyzm ()
  "SetXYZM")
(defcpp set-xyzt ()
  "SetXYZT")
(defcpp vect ()
  "Vect")
(defcpp unit ()
  "Unit")
(defcpp dot ()
  "Dot")
(defcpp cross-product ()
  "Cross")
(defcpp mag ()
  "Mag")
(defcpp mag2 ()
  "Mag2")

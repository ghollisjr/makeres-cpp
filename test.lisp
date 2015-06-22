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

(require 'makeres-cpp)

(in-package :makeres-cpp)

(defun test ()
  (let ((main
         '(function int main
           ((var int argc)
            (var (pointer (pointer char)) argv))
           (for (var int i 0) "i < 5" "++i"
            (<< cout argc endl))
           (var ifstream infile "\"test.dat\"")
           (var float line)
           (while (>> infile line)
             (<< cout (sqrt line) endl))
           (method infile close))))
    (format t "Required headers: ~a~%" (required-headers main))
    (format t "C++ Code:~%~a~%" (cpp main)))
  nil)

(defun progtest (&optional (n 1000000))
  (exe "/home/ghollisjr/test/exe"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (var long n (atoi (aref argv 1)))
                  (var (type long long) sum 0)
                  (var int count 0)
                  (for (var long i 0) (< i n) (incf i)
                       (setf sum
                             (+ sum i))
                       (incf count))
                  (<< cout
                      (/ (typecast float sum)
                         (typecast float count))
                      endl)))
       :flags '("-O3")
       :arguments (list (lisp->cpp n))
       :output *standard-output*))

(defcppfun void makeres_cpp_test
    ((var int x)
     (var float y))
  (<< cout x "\" \"" y endl))

(defun progtest2 ()
  (exe "/home/ghollisjr/test/exe2"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (makeres_cpp_test 1 2)))
       :flags '("-O3")
       :output *standard-output*))

(defun roottest1 ()
  (exe "/home/ghollisjr/test/roottest1"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (var (pointer th1d)
                       h
                       (new th1d
                            "\"hist\""
                            "\"hist\""
                            100
                            -3
                            3))
                  (var trandom3 gen)
                  (for (var int i 0) (< i 1000) (incf i)
                       (pmethod h fill
                                (method gen gaus 0 1)))
                  (<< cout (pmethod h entries) endl)))
       :output *standard-output*))

(defun roottest2 ()
  (exe "/home/ghollisjr/test/roottest2"
       ((function int main ((var int argc)
                            (var (pointer (pointer char)) argv))
                  (var (pointer th1d)
                       h
                       (new th1d
                            (str "hist")
                            (str "hist")
                            100
                            -3
                            3))
                  (var trandom3 gen)
                  (for (var int i 0) (< i 1000000) (incf i)
                       (pmethod h fill
                                (method gen gaus 0 1)))
                  ;; post filling
                  (var int nbins
                       (pmethod h nbinsx))
                  (for (var int i 1) (<= i nbins) (incf i)
                       (var double x
                            (pmethod h bin-center
                                     i))
                       (var int y
                            (pmethod h bin-content
                                     i))
                       (<< cout
                           x (str " ") y
                           endl))
                  (delete h)))
       :output *standard-output*))

(defun roottest3 ()
  (exe "/home/ghollisjr/test/roottest3"
       ((function int main ()
                  ;; output tfile
                  (varcons TFile outfile
                           (str "/home/ghollisjr/test/roottest3.root")
                           (str "RECREATE"))
                  ;; output ttree
                  (varcons TTree tree
                           (str "tree")
                           (str "tree"))
                  ;; Branch x
                  (var float x)
                  (method tree Branch
                          (str "x")
                          (address x))
                  ;; Branch y
                  (var float y)
                  (method tree Branch
                          (str "y")
                          (address y))
                  ;; Branch r
                  (var float r)
                  (method tree Branch
                          (str "r")
                          (address r))
                  ;; Write random numbers to TTree
                  (var TRandom3 gen)
                  (for (var int i 0) (< i 1000000) (incf i)
                       (setf x
                             (method gen Gaus
                                     0 1))
                       (setf y
                             (method gen Gaus
                                     0 1))
                       (setf r
                             (sqrt
                              (pow (+ (pow x 2)
                                      (pow y 2))
                                   2)))
                       (method tree Fill))
                  ;; Close file
                  (method tree Write)
                  (method outfile root-close)
                  (return 0)))))

(defun roottest4 ()
  (exe "/home/ghollisjr/test/roottest4"
       ((function int main ()
                  (varcons TFile infile
                           (str "/home/ghollisjr/test/roottest3.root")
                           (str "READ"))
                  (var (pointer TTree) tree
                       (typecast (pointer TTree)
                                 (method infile Get
                                         (str "tree"))))
                  (<< cout (pmethod tree entries) endl)
                  (method infile root-close)))
       :output *standard-output*))

(defun histtest1 ()
  (exe "/home/ghollisjr/test/histtest1"
       ((function int main ()
                  (var (pointer TH2D) hist
                       (typecast (pointer TH2D)
                                 (read_histogram
                                  (str "/home/ghollisjr/test/hist.h5"))))
                  (var int nbins
                       (* (pmethod hist
                                   nbinsx)
                          (pmethod hist
                                   nbinsy)))
                  (<< cout nbins endl)

                  ;; (for (var int i 1) (<= i nbins) (incf i)
                  ;;      (<< cout
                  ;;          (pmethod hist
                  ;;                   bin-content
                  ;;                   i)
                  ;;          (str " +- ")
                  ;;          (pmethod hist
                  ;;                   bin-error
                  ;;                   i)
                  ;;          endl))
                  ))
       :output *standard-output*))

(defun histtest2 ()
  (exe "/home/ghollisjr/test/histtest2"
       ((function int main ()
                  (var (pointer TH1D) hist
                       (typecast (pointer TH1D)
                                 (read_histogram
                                  (str "/home/ghollisjr/test/hist2.h5"))))
                  (var int nbins
                       (pmethod hist
                                nbinsx))
                  (for (var int i 1) (<= i nbins) (incf i)
                       (var double content
                            (pmethod hist
                                     bin-content
                                     i))
                       (if (not (= content 0))
                           (<< cout
                               content
                               (str " +- ")
                               (pmethod hist
                                        bin-error
                                        i)
                               endl)))))
       :output *standard-output*))

(defun whisttest ()
  (exe "/home/ghollisjr/test/whisttest"
       ((function int main ()
                  (varcons TH3D hist
                           (str "hist")
                           (str "hist")
                           10
                           -3
                           3
                           20
                           -4
                           4
                           50
                           -5
                           5)
                  (method hist Fill
                          (typecast double 0)
                          (typecast double 0)
                          (typecast double 0))
                  (var (pointer string) field_names
                       (new[] string 3))
                  (setf (aref field_names 0)
                        (str "first-field"))
                  (setf (aref field_names 1)
                        (str "second-field"))
                  (setf (aref field_names 2)
                        (str "third-field"))
                  (write_histogram (address hist)
                                   3
                                   (str "/home/ghollisjr/test/whist.h5")
                                   field_names)))
       :output *standard-output*))

;; (defun pass-test ()
;;   (cpp-table-pass ))

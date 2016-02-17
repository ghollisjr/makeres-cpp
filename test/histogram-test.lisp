(require 'makeres-cpp)

(in-package :makeres-cpp)

;;;; Histogram circle test
;;;;
;;;; Checks all 4 types of histograms to see if Lisp write -> C++ read
;;;; -> C++ write -> Lisp read results in an identical histogram.

(defparameter *workdir* "/home/ghollisjr/cpptest/")

(defun path1 ()
  (let ((path (merge-pathnames "path1.h5" *workdir*)))
    (ensure-directories-exist path)
    (namestring path)))

(defun path2 ()
  (let ((path (merge-pathnames "path2.h5" *workdir*)))
    (ensure-directories-exist path)
    (namestring path)))

(defparameter *nbins* 3
  "Number of bins per dimension")

(set-cpp-work-path "/home/ghollisjr/cpptest/cpp-work")

(defun genhist (ndims)
  (let ((hist
         (make-shist
          (loop
             for i from 1 to ndims
             collecting (append (list :name (format nil "x~a" i))
                                (discrete-dim-spec :low 1d0 :high *nbins*))))))
    (loop
       for set in (apply #'cartesian-product
                         (loop
                            for dim below ndims
                            collecting (range 1 *nbins*)))
       for i from 1
       do (hins hist (if (length-equal set 1)
                         (first set)
                         set)
                i))
    hist))

(defun whist (histogram)
  (with-open-hdf-file (file (path1)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-histogram histogram file "/histogram")))

(defun cppcopy (ndims)
  "Copies a histogram from (path1) to (path2) using C++ code"
  (when (probe-file (path2))
    (delete-file (path2)))
  (let ((type
         (cond
           ((= ndims 1)
            '(pointer TH1D))
           ((= ndims 2)
            '(pointer TH2D))
           ((= ndims 3)
            '(pointer TH3D))
           (t
            '(pointer THnSparseD)))))
    (exe-fn (cpp-work-path "exe")
            `((function int main ()
                        (var ,type
                             hist
                             (typecast ,type
                                       (read_histogram (str ,(path1)))))
                        ;; This shows that read_histogram is broken,
                        ;; as only the first 1000 events are being
                        ;; read from file.
                        ;; 
                        ;; DEBUG
                        ;;(<< cout (pmethod hist axis.nbins) endl)
                        ;; END DEBUG
                        
                        ;;; The following debug section proved that
                        ;;; 2-D histogram reading was OK, so it's most
                        ;;; likely a problem with histogram writing.
                        ;; ;; DEBUG
                        ;; (var int numbinsx (pmethod hist nbinsx))
                        ;; (var int numbinsy (pmethod hist nbinsy))
                        ;; (for (var int i 1)
                        ;;      (<= i (* numbinsx numbinsy))
                        ;;      (incf i)
                        ;;      (<< cout (pmethod hist get-bin-content i) endl))
                        ;; (varcons TFile rfile
                        ;;          (str "/home/ghollisjr/cpptest/rfile.root")
                        ;;          (str "RECREATE"))
                        ;; (method rfile cd)
                        ;; (pmethod hist write)
                        ;; (method rfile root-close)
                        ;; ;; END DEBUG
                        (vararray string field_names (,ndims)
                                  ,@(loop
                                       for i from 1 to ndims
                                       collecting
                                         `(str ,(format nil "x~a" i))))
                        (write_histogram hist
                                         ,ndims
                                         (str ,(path2))
                                         field_names)
                        (return 0)))
            :output *standard-output*)))

(defun rhist ()
  (with-open-hdf-file (file (path2)
                            :direction :input)
    (read-histogram file "/histogram")))

(defun comparehist (h1 h2)
  (let ((diff (- h1 h2)))
    (every (lambda (x)
             (zerop (cdr x)))
           (map->alist diff))))

(defun circtest (ndims)
  (let ((orighist (genhist ndims))
        copyhist)
    (whist orighist)
    (cppcopy ndims)
    (setf copyhist
          (rhist))
    (comparehist orighist copyhist)))

(defun circtests (&optional nbins)
  (let ((*nbins*
         (if nbins
             nbins
             *nbins*)))
    (loop
       for i from 1 to 4
       collecting (circtest i))))

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

;; C++ histogram definition operator

;; OLD:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun replace-hins-old (ndims body)
    (cond
      ((null body)
       nil)
      ((listp body)
       (if (eq (first body)
               'hins)
           (let ((rest-body (rest body)))
             (if (<= ndims 3)
                 `(method (uniq hist) fill ,@(rest body))
                 `(progn
                    ,@(loop
                         for dim below ndims
                         for form in rest-body
                         collecting
                           `(setf (aref (uniq xs) ,dim)
                                  ,form))
                    (method (uniq hist) fill
                            (uniq xs)
                            ,@(when (> (length rest-body) ndims)
                                `(,@(last rest-body)))))))
           (mapcar (lambda (x) (replace-hins-old ndims x))
                   body)))
      ((atom body)
       body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun replace-hins (body)
    (flet ((safe (list min-length)
             (let ((length (length list)))
               (if (< length min-length)
                   (append list
                           (loop
                              for i below (- min-length length)
                              collecting 0d0))
                   list))))
                 
    (cond
      ((null body)
       nil)
      ((listp body)
       (if (eq (first body)
               'hins)
           (let ((rest-body (rest body)))
             `(cond
                ((= (uniq ndims) 1)
                 (pmethod (typecast (pointer TH1D)
                                    (uniq hist))
                          fill ,@(subseq (safe (rest body) 1)
                                         0 (min 2
                                                (length (safe (rest body) 1))))))
                ((= (uniq ndims) 2)
                 (pmethod (typecast (pointer TH2D)
                                    (uniq hist))
                          fill ,@(subseq (safe (rest body) 2)
                                         0 (min 3
                                                (length (safe (rest body) 2))))))
                ((= (uniq ndims) 3)
                 (pmethod (typecast (pointer TH3D)
                                    (uniq hist))
                          fill ,@(subseq (safe (rest body) 3)
                                         0 (min 4
                                                (length (safe (rest body) 3))))))
                (t
                 (var double (uniq weight) 1d0)
                 (when (< (uniq ndims) ,(length rest-body))
                   (setf (uniq weight) ,@(last rest-body)))
                 ,@(loop
                      for dim below (- (length rest-body) 1)
                      for form in (butlast rest-body)
                      collecting
                        `(setf (aref (uniq xs) ,dim)
                               ,form))
                 (when (not (< (uniq ndims) ,(length rest-body)))
                   (setf (uniq weight)
                         ,(first (last rest-body))))
                 (pmethod (typecast (pointer THnSparseD)
                                    (uniq hist))
                          fill
                          (uniq xs)
                          (uniq weight)))))
           (mapcar (lambda (x) (replace-hins x))
                   body)))
      ((atom body)
       body)))))

(defmacro defcpphist-old (id src bin-specs inits &body body)
  "Defines a cpp-dotab target resulting in a C++ histogram.  Provides
the following operators for use in the body: hins supplies any
operators given to it to the Fill method applied to the histogram
object being filled. (uniq hist) references the result
histogram, (uniq hist-file) references the temporary file for the
histogram."
  `(eval `(defcpphist-raw ,',id ,',src ,,bin-specs ,',inits ,@',body)))

(defmacro defcpphist-raw (id src bin-specs inits &body body)
  "Defines a do-cpptab target resulting in a C++ histogram.  Provides
the following operators for use in the body: hins supplies any
operators given to it to the Fill method applied to the histogram
object being filled. (uniq hist) references the result histogram."
  (let* ((ndims (length bin-specs)))
    `(defres ,id
       (cpp-dotab ,src
           ,(append
             (cond
               ((= ndims 1)
                `((varcons TH1D (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,(getf (first bin-specs)
                                  :nbins)
                           ,(getf (first bin-specs)
                                  :low)
                           ,(getf (first bin-specs)
                                  :high))))
               ((= ndims 2)
                `((varcons TH2D (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,(getf (first bin-specs)
                                  :nbins)
                           ,(getf (first bin-specs)
                                  :low)
                           ,(getf (first bin-specs)
                                  :high)
                           ,(getf (second bin-specs)
                                  :nbins)
                           ,(getf (second bin-specs)
                                  :low)
                           ,(getf (second bin-specs)
                                  :high))))
               ((= ndims 3)
                `((varcons TH3D (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,(getf (first bin-specs)
                                  :nbins)
                           ,(getf (first bin-specs)
                                  :low)
                           ,(getf (first bin-specs)
                                  :high)
                           ,(getf (second bin-specs)
                                  :nbins)
                           ,(getf (second bin-specs)
                                  :low)
                           ,(getf (second bin-specs)
                                  :high)
                           ,(getf (third bin-specs)
                                  :nbins)
                           ,(getf (third bin-specs)
                                  :low)
                           ,(getf (third bin-specs)
                                  :high))))
               (t
                `((vararray int (uniq nbins) (,ndims)
                            ,@(loop
                                 for bs in bin-specs
                                 collecting
                                   (getf bs :nbins)))
                  (vararray double (uniq low) (,ndims)
                            ,@(loop
                                 for bs in bin-specs
                                 collecting
                                   (getf bs :low)))
                  (vararray double (uniq high) (,ndims)
                            ,@(loop
                                 for bs in bin-specs
                                 collecting
                                   (getf bs :high)))
                  (vararray double (uniq xs) (,ndims))
                  (varcons THnSparseD (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,ndims
                           (uniq nbins)
                           (uniq low)
                           (uniq high)))))
             `((vararray string (uniq names) (,ndims)
                         ,@(loop
                              for bs in bin-specs
                              collecting `(str ,(getf bs :name)))))
             inits
             `((method (uniq hist) sumw2)))
           ((write_histogram (address (uniq hist))
                             ,ndims
                             (str (eval (cpp-work-path (uniq hist-file))))
                             (uniq names)))
           (let ((result
                  (load-object 'sparse-histogram
                               (cpp-work-path (uniq hist-file)))))
             (delete-file (cpp-work-path (uniq hist-file)))
             result)
         ,@(replace-hins-old ndims body)))))

(defmacro defcpphist-uniq-old (id src bin-specs inits &body body)
  "Defines a cpp-dotab target resulting in a C++ histogram.  Provides
the following operators for use in the body: hins supplies any
operators given to it to the Fill method applied to the histogram
object being filled. (uniq hist) references the result
histogram, (uniq hist-file) references the temporary output file for
the histogram."
  `(eval `(defcpphist-uniq-raw ,',id ,',src ,,bin-specs ,',inits ,@',body)))

(defmacro defcpphist-uniq-raw (id src bin-specs inits &body body)
  "Defines a do-cpptab target resulting in a C++ histogram.  Provides
the following operators for use in the body: hins supplies any
operators given to it to the Fill method applied to the histogram
object being filled. (uniq hist) references the result histogram."
  (let* ((ndims (length bin-specs)))
    `(defres-uniq ,id
       (cpp-dotab ,src
           ,(append
             (cond
               ((= ndims 1)
                `((varcons TH1D (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,(getf (first bin-specs)
                                  :nbins)
                           ,(getf (first bin-specs)
                                  :low)
                           ,(getf (first bin-specs)
                                  :high))))
               ((= ndims 2)
                `((varcons TH2D (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,(getf (first bin-specs)
                                  :nbins)
                           ,(getf (first bin-specs)
                                  :low)
                           ,(getf (first bin-specs)
                                  :high)
                           ,(getf (second bin-specs)
                                  :nbins)
                           ,(getf (second bin-specs)
                                  :low)
                           ,(getf (second bin-specs)
                                  :high))))
               ((= ndims 3)
                `((varcons TH3D (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,(getf (first bin-specs)
                                  :nbins)
                           ,(getf (first bin-specs)
                                  :low)
                           ,(getf (first bin-specs)
                                  :high)
                           ,(getf (second bin-specs)
                                  :nbins)
                           ,(getf (second bin-specs)
                                  :low)
                           ,(getf (second bin-specs)
                                  :high)
                           ,(getf (third bin-specs)
                                  :nbins)
                           ,(getf (third bin-specs)
                                  :low)
                           ,(getf (third bin-specs)
                                  :high))))
               (t
                `((vararray int (uniq nbins) (,ndims)
                            ,@(loop
                                 for bs in bin-specs
                                 collecting
                                   (getf bs :nbins)))
                  (vararray double (uniq low) (,ndims)
                            ,@(loop
                                 for bs in bin-specs
                                 collecting
                                   (getf bs :low)))
                  (vararray double (uniq high) (,ndims)
                            ,@(loop
                                 for bs in bin-specs
                                 collecting
                                   (getf bs :high)))
                  (vararray double (uniq xs) (,ndims))
                  (varcons THnSparseD (uniq hist)
                           (str (uniq hist))
                           (str (uniq hist))
                           ,ndims
                           (uniq nbins)
                           (uniq low)
                           (uniq high)))))
             `((vararray string (uniq names) (,ndims)
                         ,@(loop
                              for bs in bin-specs
                              collecting `(str ,(getf bs :name)))))
             inits
             `((method (uniq hist) sumw2)))
           ((write_histogram (address (uniq hist))
                             ,ndims
                             (str (eval (cpp-work-path (uniq hist-file))))
                             (uniq names)))
           (let ((result
                  (load-object 'sparse-histogram
                               (cpp-work-path (uniq hist-file)))))
             (delete-file (cpp-work-path (uniq hist-file)))
             result)
         ,@(replace-hins-old ndims body)))))

;; Add support for histogram binning specifications as Lisp objects:
(defcpp vardimspecs (name dimspecs)
  (let* ((dimspecs (eval dimspecs))
         (ndims (length dimspecs)))
    (with-output-to-string (s)
      (format s
              "std::vector<std::vector<double> > ~a = {"
              (cpp name))
      (loop
         for dimspec in dimspecs
         for i from 1
         do
           (format s "{")
           (format s "~{~a~^,~}"
                   (list (cpp (getf dimspec :nbins))
                         (cpp (getf dimspec :low))
                         (cpp (getf dimspec :high))))
           (format s "}")
           (when (< i ndims)
             (format s ",")))
      (format s "}"))))

(defcpp vardimspecnames (name dimspecs)
  (let* ((dimspecs (eval dimspecs))
         (ndims (length dimspecs)))
    (with-output-to-string (s)
      (format s
              "std::string ~a[] = {"
              (cpp name))
      (loop
         for dimspec in dimspecs
         for i from 1
         do
           (format s "\"~a\""
                   (cpp (getf dimspec :name)))
           (when (< i ndims)
             (format s ",")))
      (format s "}"))))

(defmacro defcpphist (id src bin-specs inits &body body)
  "Defines a do-cpptab target resulting in a C++ histogram.  Provides
the following operators for use in the body: hins supplies any
operators given to it to the Fill method applied to the histogram
object being filled. (uniq hist) references the result histogram."
  `(defres ,id
     (cpp-dotab ,src
         ,(append
           `((vardimspecs (uniq dimspecs) ,bin-specs)
             (vardimspecnames (uniq names) ,bin-specs)
             (var (const int) (uniq ndims)
                  (method (uniq dimspecs) size))
             (var (pointer void) (uniq hist))
             (vararray double (uniq xs) ((uniq ndims)))
             (cond
               ((= (uniq ndims) 1)
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new TH1D
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (aref (uniq dimspecs) 0 0)
                                     (aref (uniq dimspecs) 0 1)
                                     (aref (uniq dimspecs) 0 2))))
                (pmethod (typecast (pointer th1d)
                                   (uniq hist))
                         set-directory 0)
                )
               ((= (uniq ndims) 2)
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new TH2D
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (aref (uniq dimspecs) 0 0)
                                     (aref (uniq dimspecs) 0 1)
                                     (aref (uniq dimspecs) 0 2)
                                     (aref (uniq dimspecs) 1 0)
                                     (aref (uniq dimspecs) 1 1)
                                     (aref (uniq dimspecs) 1 2))))
                (pmethod (typecast (pointer th2d)
                                   (uniq hist))
                         set-directory 0))
               ((= (uniq ndims) 3)
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new TH3D
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (aref (uniq dimspecs) 0 0)
                                     (aref (uniq dimspecs) 0 1)
                                     (aref (uniq dimspecs) 0 2)
                                     (aref (uniq dimspecs) 1 0)
                                     (aref (uniq dimspecs) 1 1)
                                     (aref (uniq dimspecs) 1 2)
                                     (aref (uniq dimspecs) 2 0)
                                     (aref (uniq dimspecs) 2 1)
                                     (aref (uniq dimspecs) 2 2))))
                (pmethod (typecast (pointer th3d)
                                   (uniq hist))
                         set-directory 0)
                )
               (t
                (vararray int (uniq nbins) ((uniq ndims)))
                (for (var int (uniq i) 0)
                     (< (uniq i) (uniq ndims))
                     (incf (uniq i))
                     (setf (aref (uniq nbins) (uniq i))
                           (aref (uniq dimspecs) (uniq i) 0)))

                (vararray double (uniq low) ((uniq ndims)))
                (for (var int (uniq i) 0)
                     (< (uniq i) (uniq ndims))
                     (incf (uniq i))
                     (setf (aref (uniq low) (uniq i))
                           (aref (uniq dimspecs) (uniq i) 1)))

                (vararray double (uniq high) ((uniq ndims)))
                (for (var int (uniq i) 0)
                     (< (uniq i) (uniq ndims))
                     (incf (uniq i))
                     (setf (aref (uniq high) (uniq i))
                           (aref (uniq dimspecs) (uniq i) 2)))

                (setf (uniq hist)
                      (typecast (pointer void)
                                (new THnSparseD
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (uniq ndims)
                                     (uniq nbins)
                                     (uniq low)
                                     (uniq high))))
                ;; (pmethod (typecast (pointer THnSparseD)
                ;;                    (uniq hist))
                ;;          set-directory 0)
                )))
           inits
           `((cond
               ((= (uniq ndims) 1)
                (pmethod (typecast (pointer TH1D)
                                   (uniq hist))
                         sumw2))
               ((= (uniq ndims) 2)
                (pmethod (typecast (pointer TH2D)
                                   (uniq hist))
                         sumw2))
               ((= (uniq ndims) 3)
                (pmethod (typecast (pointer TH3D)
                                   (uniq hist))
                         sumw2))
               (t
                (pmethod (typecast (pointer THnSparseD)
                                   (uniq hist))
                         sumw2)))))
         ((write_histogram (uniq hist)
                           (uniq ndims)
                           (str (eval (cpp-work-path (uniq hist-file))))
                           (uniq names)))
         (let ((result
                (load-object 'sparse-histogram
                             (cpp-work-path (uniq hist-file)))))
           (delete-file (cpp-work-path (uniq hist-file)))
           result)
       ,@(replace-hins body))))

(defmacro defcpphist-uniq (id src bin-specs inits &body body)
  "Defines a do-cpptab target resulting in a C++ histogram.  Provides
the following operators for use in the body: hins supplies any
operators given to it to the Fill method applied to the histogram
object being filled. (uniq hist) references the result histogram."
  `(defres-uniq ,id
     (cpp-dotab ,src
         ,(append
           `((vardimspecs (uniq dimspecs) ,bin-specs)
             (vardimspecnames (uniq names) ,bin-specs)
             (var (const int) (uniq ndims)
                  (method (uniq dimspecs) size))
             (var (pointer void) (uniq hist))
             (vararray double (uniq xs) ((uniq ndims)))
             (cond
               ((= (uniq ndims) 1)
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new TH1D
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (aref (uniq dimspecs) 0 0)
                                     (aref (uniq dimspecs) 0 1)
                                     (aref (uniq dimspecs) 0 2))))
                (pmethod (typecast (pointer th1d)
                                   (uniq hist))
                         set-directory 0))
               ((= (uniq ndims) 2)
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new TH2D
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (aref (uniq dimspecs) 0 0)
                                     (aref (uniq dimspecs) 0 1)
                                     (aref (uniq dimspecs) 0 2)
                                     (aref (uniq dimspecs) 1 0)
                                     (aref (uniq dimspecs) 1 1)
                                     (aref (uniq dimspecs) 1 2))))
                (pmethod (typecast (pointer th2d)
                                   (uniq hist))
                         set-directory 0))
               ((= (uniq ndims) 3)
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new TH3D
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (aref (uniq dimspecs) 0 0)
                                     (aref (uniq dimspecs) 0 1)
                                     (aref (uniq dimspecs) 0 2)
                                     (aref (uniq dimspecs) 1 0)
                                     (aref (uniq dimspecs) 1 1)
                                     (aref (uniq dimspecs) 1 2)
                                     (aref (uniq dimspecs) 2 0)
                                     (aref (uniq dimspecs) 2 1)
                                     (aref (uniq dimspecs) 2 2))))
                (pmethod (typecast (pointer th3d)
                                   (uniq hist))
                         set-directory 0))
               (t
                (vararray int (uniq nbins) ((uniq ndims)))
                (for (var int (uniq i) 0)
                     (< (uniq i) (uniq ndims))
                     (incf (uniq i))
                     (setf (aref (uniq nbins) (uniq i))
                           (aref (uniq dimspecs) (uniq i) 0)))

                (vararray double (uniq low) ((uniq ndims)))
                (for (var int (uniq i) 0)
                     (< (uniq i) (uniq ndims))
                     (incf (uniq i))
                     (setf (aref (uniq low) (uniq i))
                           (aref (uniq dimspecs) (uniq i) 1)))

                (vararray double (uniq high) ((uniq ndims)))
                (for (var int (uniq i) 0)
                     (< (uniq i) (uniq ndims))
                     (incf (uniq i))
                     (setf (aref (uniq high) (uniq i))
                           (aref (uniq dimspecs) (uniq i) 2)))
                (setf (uniq hist)
                      (typecast (pointer void)
                                (new THnSparseD
                                     (str (uniq hist))
                                     (str (uniq hist))
                                     (uniq ndims)
                                     (uniq nbins)
                                     (uniq low)
                                     (uniq high)))))))
           inits
           `((cond
               ((= (uniq ndims) 1)
                (pmethod (typecast (pointer TH1D)
                                   (uniq hist))
                         sumw2))
               ((= (uniq ndims) 2)
                (pmethod (typecast (pointer TH2D)
                                   (uniq hist))
                         sumw2))
               ((= (uniq ndims) 3)
                (pmethod (typecast (pointer TH3D)
                                   (uniq hist))
                         sumw2))
               (t
                (pmethod (typecast (pointer THnSparseD)
                                   (uniq hist))
                         sumw2)))))
         ((write_histogram (uniq hist)
                           (uniq ndims)
                           (str (eval (cpp-work-path (uniq hist-file))))
                           (uniq names)))
         (let ((result
                (load-object 'sparse-histogram
                             (cpp-work-path (uniq hist-file)))))
           (delete-file (cpp-work-path (uniq hist-file)))
           result)
       ,@(replace-hins body))))

;; Histogram read and write functions

(defcppfun (pointer void) read_histogram
    ((var string filename))
  (let ((hid-t infile
          (h5fopen (method filename c-str)
                   +H5F-ACC-RDONLY+
                   +H5P-DEFAULT+))

        ;; bin specs
        (hid-t binspec_dataset
          (h5dopen2 infile
                    (str "/histogram/bin-specs")
                    +H5P-DEFAULT+))
        (hid-t binspec_datatype
          (h5dget-type binspec_dataset))
        (hid-t binspec_dataspace
          (h5dget-space binspec_dataset))
        ((pointer hsize-t) binspec_dataset_dims
          (new hsize-t))
        ((pointer hsize-t) binspec_chunk_dims
          (new hsize-t))
        (int binspec_chunk_size)
        (int binspec_nrows)
        (int binspec_nfields 5) ; fixed formatting

        ;; data
        (hid-t data_dataset
          (h5dopen2 infile
                    (str "/histogram/data")
                    +H5P-DEFAULT+))
        (hid-t data_datatype
          (h5dget-type data_dataset))
        (hid-t data_dataspace
          (h5dget-space data_dataset))
        ((pointer hsize-t) data_dataset_dims
          (new hsize-t))
        ((pointer hsize-t) data_chunk_dims
          (new hsize-t))
        (int data_chunk_size)
        (int data_nfields)
        (int data_nrows)

        ;; shared
        (hid-t create_plist)
        ;; memspace things
        (hid-t memspace)
        ((pointer hsize-t) memspace_dims
          (new hsize-t))
        ((pointer hsize-t) memspace_maxdims
          (new hsize-t))

        (herr-t stat)
        ((pointer char) buffer)
        ((pointer void) result))

    ;;; binspec reading

    ;; chunking information
    (setf create_plist
          (h5dget-create-plist binspec_dataset))
    (h5pget-chunk create_plist 1 binspec_chunk_dims)
    (setf binspec_chunk_size
          (aref binspec_chunk_dims 0))

    ;; dataset extent
    (setf stat
          (h5sget-simple-extent-dims binspec_dataspace
                                     binspec_dataset_dims
                                     NULL))
    (setf binspec_nrows
          (aref binspec_dataset_dims
                0))
    ;; name string information
    (var (pointer hsize-t) binspec_name_dims
         (new hsize-t))
    (h5tget-array-dims2 (h5tget-member-type binspec_datatype
                                            0)
                        binspec_name_dims)
    ;; Setup buffer:
    (var long binspec_row_size
         (h5tget-size binspec_datatype))
    (var long binspec_buffer_size
         (* binspec_row_size
            binspec_chunk_size))
    (setf buffer
          (new[] char binspec_buffer_size))

    ;; Dimensionality
    (var int ndims
         binspec_nrows)

    ;; loop variables
    (var int row 0)
    (var (pointer int) name_length
         (new[] int ndims))
    (var (pointer int) nbins
         (new[] int ndims))
    (var (pointer double) low
         (new[] double ndims))
    (var (pointer double) high
         (new[] double ndims))
    ;; Get binspec info
    (for (var int chunk_index 0)
         (< (* chunk_index binspec_buffer_size)
            binspec_nrows)
         (incf chunk_index)
         (var (const int) chunk_size
              binspec_chunk_size)
         (var int remaining_rows
              (- binspec_nrows
                 (* chunk_index chunk_size)))
         (if (< remaining_rows chunk_size)
             (setf (aref memspace_dims 0)
                   remaining_rows)
             (setf (aref memspace_dims 0)
                   chunk_size))
         ;; memspace_maxdims
         (var (pointer hsize-t) memspace_maxdims
              (new hsize-t))
         (setf (value memspace_maxdims)
               (value memspace_dims))
         ;; start
         (var (pointer hsize-t) start
              (new hsize-t))
         (setf (value start)
               (* chunk_index
                  chunk_size))
         ;; stride
         (var (pointer hsize-t) stride
              (new hsize-t))
         (setf (value stride)
               1)
         ;; count
         (var (pointer hsize-t) cnt
              (new hsize-t))
         (setf (value cnt)
               1)
         ;; blck
         (var (pointer hsize-t) blck
              (new hsize-t))
         (setf (value blck)
               (value memspace_dims))

         (h5sselect-hyperslab binspec_dataspace
                              :H5S-SELECT-SET
                              start
                              stride
                              cnt
                              blck)
         (setf memspace
               (h5screate-simple 1 memspace_dims memspace_maxdims))
         (h5dread binspec_dataset
                  binspec_datatype
                  memspace
                  binspec_dataspace
                  +H5P-DEFAULT+
                  buffer)
         (for (var int i 0) (< i (value memspace_dims)) (incf i)
              (var long buffer_index
                   (+ (* i binspec_row_size)
                      (* chunk_size chunk_index)))

              ;; name_length
              (setf (aref name_length row)
                    (value
                     (typecast (pointer int)
                               (+ buffer
                                  buffer_index
                                  (H5Tget-member-offset binspec_datatype
                                                        1)))))
              ;; nbins
              (setf (aref nbins row)
                    (value
                     (typecast (pointer int)
                               (+ buffer
                                  buffer_index
                                  (H5Tget-member-offset binspec_datatype
                                                        2)))))
              ;; low
              (setf (aref low row)
                    (value
                     (typecast (pointer double)
                               (+ buffer
                                  buffer_index
                                  (H5Tget-member-offset binspec_datatype
                                                        3)))))

              ;; high
              (setf (aref high row)
                    (value
                     (typecast (pointer double)
                               (+ buffer
                                  buffer_index
                                  (H5Tget-member-offset binspec_datatype
                                                        4)))))
              (incf row))
         ;; Loop cleanup
         (delete memspace_maxdims)
         (delete start)
         (delete stride)
         (delete cnt)
         (delete blck)
         )



    ;; chunking information
    (setf create_plist
          (h5dget-create-plist data_dataset))
    (h5pget-chunk create_plist 1 data_chunk_dims)
    (setf data_chunk_size
          (aref data_chunk_dims 0))

    ;; dataset extent
    (setf stat
          (h5sget-simple-extent-dims data_dataspace
                                     data_dataset_dims
                                     NULL))
    (setf data_nrows
          (aref data_dataset_dims
                0))

    ;; Setup buffer:
    (var string second_field_name
         (h5tget-member-name data_datatype 1))
    (var long data_row_size)
    (var int n_count_vars)
    (if (= second_field_name
           (str "count-error"))
        (setf n_count_vars 2)
        (setf n_count_vars 1))

    (setf data_row_size
          (h5tget-size data_datatype))

    (var long data_buffer_size
         (* data_row_size
            data_chunk_size))

    (delete[] buffer)
    (setf buffer
          (new[] char data_buffer_size))

    ;;; Create histogram result

    (cond
      (;; 1-D
       (= ndims 1)
       (setf result
             (typecast
              (pointer void)
              (new TH1D
                   (str "hist")
                   (str "hist")

                   (aref nbins 0)
                   (aref low 0)
                   (aref high 0))))
       (cond
         ((= n_count_vars 2)
          (var (pointer TH1D) h
               (typecast (pointer TH1D) result))
          (pmethod h sumw2))))
      (;; 2-D
       (= ndims 2)
       (setf result
             (typecast
              (pointer void)
              (new TH2D
                   (str "hist")
                   (str "hist")

                   (aref nbins 0)
                   (aref low 0)
                   (aref high 0)

                   (aref nbins 1)
                   (aref low 1)
                   (aref high 1))))
       (cond
         ((= n_count_vars 2)
          (var (pointer TH2D) h
               (typecast (pointer TH2D) result))
          (pmethod h sumw2))))
      (;; 3-D
       (= ndims 3)
       (setf result
             (typecast
              (pointer void)
              (new TH3D
                   (str "hist")
                   (str "hist")

                   (aref nbins 0)
                   (aref low 0)
                   (aref high 0)

                   (aref nbins 1)
                   (aref low 1)
                   (aref high 1)

                   (aref nbins 2)
                   (aref low 2)
                   (aref high 2))))
       (cond
         ((= n_count_vars 2)
          (var (pointer TH3D) h
               (typecast (pointer TH3D) result))
          (pmethod h sumw2))))
      (;; Sparse
       t
       (setf result
             (typecast
              (pointer void)
              (new THnSparseD
                   (str "hist")
                   (str "hist")
                   ndims
                   nbins
                   low
                   high)))
       (cond
         ((= n_count_vars 2)
          (var (pointer THnSparseD) h
               (typecast (pointer THnSparseD) result))
          (pmethod h sumw2)))))

    ;; Read histogram data:
    (for (var int chunk_index 0)
         ;; (< (* chunk_index data_buffer_size)
         ;;    data_nrows)
         (< (* chunk_index data_chunk_size)
            data_nrows)
         (incf chunk_index)
         (var (const int) chunk_size
              data_chunk_size)
         (var int remaining_rows
              (- data_nrows
                 (* chunk_index chunk_size)))
         (if (< remaining_rows chunk_size)
             (setf (aref memspace_dims 0)
                   remaining_rows)
             (setf (aref memspace_dims 0)
                   chunk_size))
         ;; memspace_maxdims
         (var (pointer hsize-t) memspace_maxdims
              (new hsize-t))
         (setf (value memspace_maxdims)
               (value memspace_dims))
         ;; start
         (var (pointer hsize-t) start
              (new hsize-t))
         (setf (value start)
               (* chunk_index
                  chunk_size))
         ;; stride
         (var (pointer hsize-t) stride
              (new hsize-t))
         (setf (value stride)
               1)
         ;; count
         (var (pointer hsize-t) cnt
              (new hsize-t))
         (setf (value cnt)
               1)
         ;; blck
         (var (pointer hsize-t) blck
              (new hsize-t))
         (setf (value blck)
               (value memspace_dims))

         (h5sselect-hyperslab data_dataspace
                              :H5S-SELECT-SET
                              start
                              stride
                              cnt
                              blck)

         (setf memspace
               (h5screate-simple 1 memspace_dims memspace_maxdims))
         (h5dread data_dataset
                  data_datatype
                  memspace
                  data_dataspace
                  +H5P-DEFAULT+
                  buffer)

         (var (pointer double) count)
         (var (pointer double) xs)
         (for (var int i 0) (< i (value memspace_dims)) (incf i)
              (var long buffer_index
                   (* i data_row_size))
              ;; count
              (setf count
                    (typecast (pointer double)
                              (+ buffer
                                 buffer_index)))

              ;; xs
              (setf buffer_index
                    (+ buffer_index
                       (* n_count_vars (sizeof double))))
              (setf xs
                    (typecast (pointer double)
                              (+ buffer
                                 buffer_index)))

              ;; insertion
              (cond
                ;; 1-D
                ((= ndims 1)
                 (var (pointer TH1D) h
                      (typecast (pointer TH1D) result))
                 (pmethod h
                          Fill
                          (value xs)
                          (value count))
                 (if (= n_count_vars 2)
                     (let ((int bin_index
                             (pmethod (pmethod h
                                               x-axis)
                                      find-bin
                                      (value xs))))
                       (pmethod h
                                set-bin-error
                                bin_index
                                (aref count 1)))))
                ;; 2-D
                ((= ndims 2)
                 (var (pointer TH2D) h
                      (typecast (pointer TH2D) result))
                 (pmethod h
                          Fill
                          (aref xs 0)
                          (aref xs 1)
                          (value count))
                 (if (= n_count_vars 2)
                     (let ((int x_bin_index
                             (pmethod (pmethod h
                                               x-axis)
                                      find-bin
                                      (aref xs 0)))
                           (int y_bin_index
                             (pmethod (pmethod h
                                               y-axis)
                                      find-bin
                                      (aref xs 1)))
                           (int bin_index
                             (pmethod h
                                      bin
                                      x_bin_index
                                      y_bin_index)))
                       (pmethod h
                                set-bin-error
                                bin_index
                                (aref count 1)))))
                ;; 3-D
                ((= ndims 3)
                 (var (pointer TH3D) h
                      (typecast (pointer TH3D) result))
                 (pmethod h
                          Fill
                          (aref xs 0)
                          (aref xs 1)
                          (aref xs 2)
                          (value count))
                 (if (= n_count_vars 2)
                     (let ((int x_bin_index
                             (pmethod (pmethod h
                                               x-axis)
                                      find-bin
                                      (aref xs 0)))
                           (int y_bin_index
                             (pmethod (pmethod h
                                               y-axis)
                                      find-bin
                                      (aref xs 1)))
                           (int z_bin_index
                             (pmethod (pmethod h
                                               z-axis)
                                      find-bin
                                      (aref xs 2)))
                           (int bin_index
                             (pmethod h
                                      bin
                                      x_bin_index
                                      y_bin_index
                                      z_bin_index)))
                       (pmethod h
                                set-bin-error
                                bin_index
                                (aref count 1)))))
                ;; THnSparse
                (t
                 (var (pointer THnSparseD) h
                      (typecast (pointer THnSparseD) result))
                 (pmethod h fill xs (value count))
                 (if (= n_count_vars 2)
                     (pmethod h
                              set-bin-error2
                              (pmethod h
                                       get-bin
                                       xs)
                              (aref count 1))))))
         ;; Cleanup
         (delete memspace_maxdims)
         (delete start)
         (delete stride)
         (delete cnt)
         (delete blck))
    ;; Cleanup:
    (delete binspec_dataset_dims)
    (delete binspec_chunk_dims)
    (delete data_dataset_dims)
    (delete data_chunk_dims)
    (delete memspace_dims)
    (delete memspace_maxdims)
    (delete binspec_name_dims)
    (delete nbins)
    (delete low)
    (delete high)
    (delete buffer)

    ;; Return:
    (return result)))

(defcppfun void write_histogram
    ((var (pointer void) hist)
     ;; May be able to remove in future
     (var int ndims)
     (var string filename)
     (var (pointer string) field_names))
  (var hid-t outfile
       (H5Fcreate (method filename c-str)
                  +H5F-ACC-TRUNC+
                  +H5P-DEFAULT+
                  +H5P-DEFAULT+))

  (var bool cleanup_field_names
       false)

  ;; Compute or set field_names information
  (var (pointer int) name_lengths
       (new[] int ndims))
  (var int max_name_length 0)
  (if (= field_names 0)
      (progn
        (setf cleanup_field_names true)
        (setf field_names
              (new[] string ndims))
        ;; setup field_names
        (for (var int i 0) (< i ndims) (incf i)
             (var stringstream ss)
             (<< ss (str "x") (+ 1 i))
             (setf (aref field_names i)
                   (method ss stringstream.str)))))
  (for (var int i 0) (< i ndims) (incf i)
       (setf (aref name_lengths i)
             (method (aref field_names i)
                     length))
       (if (> (aref name_lengths i)
              max_name_length)
           (setf max_name_length
                 (aref name_lengths i))))

  ;; compute nbins, low and high
  (var (pointer long) nbins
       (new[] long ndims))
  (var (pointer double) low
       (new[] double ndims))
  (var (pointer double) high
       (new[] double ndims))

  (var (pointer (pointer TAxis)) axes
       (new[] (pointer TAxis) ndims))

  (cond
    (;; 1-D
     (= ndims 1)
     (var (pointer TH1D) h
          (typecast (pointer TH1D) hist))
     (setf (aref axes 0)
           (pmethod h x-axis)))
    (;; 2-D
     (= ndims 2)
     (var (pointer TH2D) h
          (typecast (pointer TH2D) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))
     (setf (aref axes 1)
           (pmethod h y-axis)))
    (;; 3-D
     (= ndims 3)
     (var (pointer TH3D) h
          (typecast (pointer TH3D) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))
     (setf (aref axes 1)
           (pmethod h y-axis))
     (setf (aref axes 2)
           (pmethod h z-axis)))
    (;; Sparse
     t
     (var (pointer THnSparseD) h
          (typecast (pointer THnSparseD) hist))
     (for (var int dim 0) (< dim ndims) (incf dim)
          (setf (aref axes dim)
                (pmethod h get-axis
                         dim)))))

  (for (var int dim 0) (< dim ndims) (incf dim)
       (setf (aref nbins dim)
             (pmethod (aref axes dim)
                      axis.nbins))
       (setf (aref low dim)
             (pmethod (aref axes dim)
                      bin-low-edge
                      1))
       (setf (aref high dim)
             (pmethod (aref axes dim)
                      bin-up-edge
                      (aref nbins dim))))

  ;; chunk size (not same as Lisp default, writing all in one chunk)
  (var int chunk_size
       ndims)

  ;; buffer size
  (var long buffer_row_size
       (+ max_name_length
          (* 2
             (sizeof int))
          (* 2
             (sizeof double))))
  (var long buffer_size
       (* buffer_row_size
          chunk_size))

  ;; buffer
  (var (pointer char) buffer
       (new[] char buffer_size))

  ;; HDF5 type
  (var hid-t binspec_datatype
       (h5tcreate +H5T-COMPOUND+
                  buffer_row_size))
  (var (pointer hsize-t) name_type_dims
       (new hsize-t))
  (setf (value name_type_dims)
        max_name_length)
  (var hid-t name_type
       (h5tarray-create2 +H5T-NATIVE-CHAR+
                         1
                         name_type_dims))
  (h5tinsert binspec_datatype
             (str "name")
             0
             name_type)
  (h5tinsert binspec_datatype
             (str "name-length")
             max_name_length
             +H5T-NATIVE-INT+)
  (h5tinsert binspec_datatype
             (str "nbins")
             (+ max_name_length
                (sizeof int))
             +H5T-NATIVE-INT+)
  (h5tinsert binspec_datatype
             (str "low")
             (+ max_name_length
                (* 2 (sizeof int)))
             +H5T-NATIVE-DOUBLE+)
  (h5tinsert binspec_datatype
             (str "high")
             (+ max_name_length
                (* 2 (sizeof int))
                (sizeof double))
             +H5T-NATIVE-DOUBLE+)

  ;; Dataset
  (var hid-t binspec_dataset)

  (var (pointer hsize-t) binspec_chunkdims
       (new hsize-t))
  (setf (value binspec_chunkdims)
        chunk_size)

  (var (pointer hsize-t) binspec_dataset_dims
       (new hsize-t))
  (setf (value binspec_dataset_dims)
        ndims)

  (var (pointer hsize-t) binspec_dataset_maxdims
       (new hsize-t))
  (setf (value binspec_dataset_maxdims)
        +H5S-UNLIMITED+)

  (var hid-t cparms
       (h5pcreate +H5P-DATASET-CREATE+))
  (h5pset-chunk cparms 1 binspec_chunkdims)

  (var hid-t binspec_dataspace
       (h5screate-simple 1
                         binspec_dataset_dims
                         binspec_dataset_maxdims))

  (h5gclose
   (h5gcreate1 outfile (str "/histogram") 0))

  (setf binspec_dataset
        (h5dcreate1 outfile
                    (str "/histogram/bin-specs")
                    binspec_datatype
                    binspec_dataspace
                    cparms))

  (h5sclose binspec_dataspace)

  (var (pointer hsize-t) start
       (new hsize-t))

  (var (pointer hsize-t) stride
       (new hsize-t))

  (var (pointer hsize-t) cnt
       (new hsize-t))

  (var (pointer hsize-t) blck
       (new hsize-t))

  ;; Write bin-specs to file
  (for (var int i 0) (< i ndims) (incf i)
       (var int buffer_index
            (* i
               buffer_row_size))

       ;; name
       (for (var int j 0)
            (< j (method (aref field_names i) length))
            (incf j)
            (setf (aref buffer (+ buffer_index
                                  j))
                  (aref (aref field_names i)
                        j)))

       ;; name-length
       (setf (value
              (typecast (pointer int)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 1))))
             (method (aref field_names i) length))

       ;; nbins
       (setf (value
              (typecast (pointer int)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 2))))
             (aref nbins i))

       ;; low
       (setf (value
              (typecast (pointer double)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 3))))
             (aref low i))

       ;; high
       (setf (value
              (typecast (pointer double)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 4))))
             (aref high i)))

  (setf binspec_dataspace
        (h5dget-space binspec_dataset))

  (setf (value start) 0)
  (setf (value stride) 1)
  (setf (value cnt) 1)
  (setf (value blck) chunk_size)

  ;; memspace variables
  (var hid-t memspace)

  (var (pointer hsize-t) memspace_dims
       (new hsize-t))
  (setf (value memspace_dims)
        chunk_size)

  (var (pointer hsize-t) memspace_maxdims
       (new hsize-t))
  (setf (value memspace_maxdims)
        chunk_size)

  (setf memspace
        (h5screate-simple 1 memspace_dims memspace_maxdims))

  (h5sselect-hyperslab binspec_dataspace
                       :H5S-SELECT-SET
                       start stride cnt blck)
  (h5dwrite binspec_dataset
            binspec_datatype
            memspace
            binspec_dataspace
            +H5P-DEFAULT+
            buffer)
  (h5sclose memspace)
  (h5sclose binspec_dataspace)
  (h5dclose binspec_dataset)
  (delete[] buffer)

  ;; Histogram data output
  (var hid-t data_dataset)
  (var hid-t data_dataspace)


  ;; NOTE: Something is broken in the chunk-write algorithm such that
  ;; only one chunk seems to be written.  Therefore, this setting is
  ;; not correct, and writing should happen all in one chunk.
  ;;
  ;; Default chunk size for Lisp
  ;; (setf chunk_size
  ;;       1000)

  ;; Calculate chunk size:

  ;; Total number of events written
  (var long row 0)

  (var (pointer hsize-t) data_dataset_dims
       (new hsize-t))
  (setf (value data_dataset_dims)
        0)

  (var (pointer hsize-t) data_dataset_maxdims
       (new hsize-t))
  (setf (value data_dataset_maxdims)
        +H5S-UNLIMITED+)

  ;; Number of count fields:
  (var int n_count_vars)
  (cond
    (;; TH1D
     (= ndims 1)
     (var (pointer TH1D) h
          (typecast (pointer TH1D) hist))
     (if (= 0 (pmethod h get-sumw2-n))
         (setf n_count_vars 1)
         (setf n_count_vars 2)))
    (;; TH2D
     (= ndims 2)
     (var (pointer TH2D) h
          (typecast (pointer TH2D) hist))
     (if (= 0 (pmethod h get-sumw2-n))
         (setf n_count_vars 1)
         (setf n_count_vars 2)))
    (;; TH3D
     (= ndims 3)
     (var (pointer TH3D) h
          (typecast (pointer TH3D) hist))
     (if (= 0 (pmethod h get-sumw2-n))
         (setf n_count_vars 1)
         (setf n_count_vars 2)))
    (;; Sparse
     t
     (var (pointer THnSparseD) h
          (typecast (pointer THnSparseD) hist))
     (if (pmethod h get-calculate-errors)
         (setf n_count_vars 2)
         (setf n_count_vars 1))))

  ;; Datatype size
  (var long data_row_size
       (* (sizeof double)
          (+ ndims
             n_count_vars)))

  ;;; Datatype
  (var hid-t data_datatype
       (h5tcreate +H5T-COMPOUND+
                  data_row_size))
  ;; insert members
  (h5tinsert data_datatype (str "count") 0 +H5T-NATIVE-DOUBLE+)
  (if (= n_count_vars 2)
      (h5tinsert data_datatype (str "count-error") (sizeof double)
                 +H5T-NATIVE-DOUBLE+))
  (for (var int i 0) (< i ndims) (incf i)
       (h5tinsert data_datatype
                  (method (aref field_names i)
                          c-str)
                  (* (sizeof double)
                     (+ i n_count_vars))
                  +H5T-NATIVE-DOUBLE+))

  ;; Amount of data in histogram
  (var long npoints)
  (cond
    (;; TH1D
     (= ndims 1)
     (var (pointer TH1D) h
          (typecast (pointer TH1D) hist))
     (setf npoints
           (pmethod h nbinsx)))
    (;; TH2D
     (= ndims 2)
     (var (pointer TH2D) h
          (typecast (pointer TH2D) hist))
     (setf npoints
           (* (pmethod h nbinsx)
              (pmethod h nbinsy))))
    (;; TH3D
     (= ndims 3)
     (var (pointer TH3D) h
          (typecast (pointer TH3D) hist))
     (setf npoints
           (* (pmethod h nbinsx)
              (pmethod h nbinsy)
              (pmethod h nbinsz))))
    (;; Sparse
     t
     (var (pointer THnSparseD) h
          (typecast (pointer THnSparseD) hist))
     (setf npoints
           (pmethod h axis.nbins))))
  ;; Create dataset
  (if (= npoints 0)
      (setf chunk_size 1)
      (setf chunk_size npoints))
  (setf data_dataspace
        (h5screate-simple 1
                          data_dataset_dims
                          data_dataset_maxdims))
  (setf cparms
        (h5pcreate +H5P-DATASET-CREATE+))
  (var (pointer hsize-t) data_chunk_dims
       (new hsize-t))
  (setf (value data_chunk_dims)
        chunk_size)
  (h5pset-chunk cparms 1 data_chunk_dims)
  (setf data_dataset
        (h5dcreate1 outfile
                    (str "/histogram/data")
                    data_datatype
                    data_dataspace
                    cparms))
  (h5sclose data_dataspace)
  ;; Setup buffer
  (setf buffer
        (new[] char (* data_row_size
                       chunk_size)))

  (var (pointer double) count
       (new[] double n_count_vars))
  (var (pointer double) xs
       (new[] double ndims))
  ;; Row index
  (setf row 0)

  ;; Chunk index
  (var int chunk_index -1)

  (for (var int i 0) (< i npoints) (incf i)
       ;; Set count and xs for each type of histogram
       (cond
         (;; TH1D
          (= ndims 1)
          (var (pointer TH1D) h
               (typecast (pointer TH1D) hist))
          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         (+ i 1)))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (pmethod h
                             bin-error
                             (+ i 1))))
          (setf (aref xs 0)
                (pmethod h
                         get-bin-center
                         (+ i 1))))
         ;; 2-D and 3-D histograms are broken due to i not being a
         ;; useful global index via the standard ROOT functions, so I
         ;; have to add special cases for them.
         (;; TH2D
          (= ndims 2)
          (var (pointer TH2D) h
               (typecast (pointer TH2D) hist))
          (var int x_index)
          (var int y_index)

          ;; Set x,y indices:

          (setf x_index
                (+ (mod i
                        (pmethod h nbinsx))
                   1))
          (setf y_index
                (+ (/ i
                      (pmethod h nbinsx))
                   1))

          ;; Set global index:
          (var int globindex
               (pmethod h
                        get-bin
                        x_index
                        y_index))

          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         globindex))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (pmethod h
                             bin-error
                             globindex)))
          (var (pointer TAxis) xaxis
               (pmethod h x-axis))
          (var (pointer TAxis) yaxis
               (pmethod h y-axis))

          (setf (aref xs 0)
                (pmethod xaxis
                         get-bin-center
                         x_index))
          (setf (aref xs 1)
                (pmethod yaxis
                         get-bin-center
                         y_index)))
         (;; TH3D
          (= ndims 3)
          (var (pointer TH3D) h
               (typecast (pointer TH3D) hist))
          (var int x_index)
          (var int y_index)
          (var int z_index)
          ;; Set x,y,z indices:

          (setf x_index
                (+ (mod i
                        (pmethod h nbinsx))
                   1))
          (setf y_index
                (+ (mod (/ i
                           (pmethod h nbinsx))
                        (pmethod h nbinsy))
                   1))
          (setf z_index
                (+ (/ i
                      (* (pmethod h nbinsx)
                         (pmethod h nbinsy)))
                   1))

          ;; Set global index:
          (var int globindex
               (pmethod h
                        get-bin
                        x_index
                        y_index
                        z_index))

          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         globindex))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (pmethod h
                             bin-error
                             globindex)))
          (var (pointer TAxis) xaxis
               (pmethod h x-axis))
          (var (pointer TAxis) yaxis
               (pmethod h y-axis))
          (var (pointer TAxis) zaxis
               (pmethod h z-axis))

          (setf (aref xs 0)
                (pmethod xaxis
                         get-bin-center
                         x_index))
          (setf (aref xs 1)
                (pmethod yaxis
                         get-bin-center
                         y_index))
          (setf (aref xs 2)
                (pmethod zaxis
                         get-bin-center
                         z_index)))
         (;; Sparse
          t
          (var (pointer THnSparseD) h
               (typecast (pointer THnSparseD) hist))
          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         i))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (sqrt
                     (pmethod h
                              get-bin-error2
                              ;; Old:
                              ;; (+ i 1)
                              i
                              ))))
          (var (pointer int) indices
               (new[] int ndims))
          (pmethod h get-bin-content
                   i
                   indices)
          (for (var int axis_index 0) (< axis_index ndims) (incf axis_index)
               (var (pointer TAxis) axis
                    (pmethod h get-axis axis_index))
               (setf (aref xs axis_index)
                     (pmethod axis get-bin-center
                              (aref indices axis_index))))
          (delete[] indices)))
       ;; Fill buffer whenever count or error are not zero
       (var long buf_index
            (* (mod row chunk_size)
               (+ n_count_vars ndims)))

       (var (pointer double) buf
            (typecast (pointer double) buffer))
       (var bool fill_p false)
       (if (not (= 0
                   (aref count 0)))
           (setf fill_p true))
       (if (= n_count_vars 2)
           (if (not (= 0
                       (aref count 1)))
               (setf fill_p true)))
       (if fill_p
           (progn
             (setf (aref buf buf_index)
                   (aref count 0))
             (if (= n_count_vars 2)
                 (setf (aref buf (+ 1 buf_index))
                       (aref count 1)))
             (for (var int j 0) (< j ndims) (incf j)
                  (setf (aref buf (+ buf_index j n_count_vars))
                        (aref xs j)))
             (incf row)))
       ;; When buffer full, write chunk to dataset
       (if (and (not (= row 0))
                (= 0 (mod row chunk_size)))
           (progn
             (incf chunk_index)
             (setf (value data_dataset_dims)
                   (+ (value data_dataset_dims)
                      chunk_size))
             (h5dset-extent data_dataset
                            data_dataset_dims)
             (setf data_dataspace
                   (h5dget-space data_dataset))
             (setf (value memspace_dims)
                   chunk_size)
             (setf (value memspace_maxdims)
                   chunk_size)

             (setf memspace
                   (h5screate-simple 1 memspace_dims memspace_maxdims))

             (setf (value start) (* chunk_size
                                    chunk_index))
             (setf (value stride) 1)
             (setf (value cnt) 1)
             (setf (value blck) chunk_size)

             (h5sselect-hyperslab data_dataspace
                                  :H5S-SELECT-SET
                                  start stride cnt blck)
             (h5dwrite data_dataset
                       data_datatype
                       memspace
                       data_dataspace
                       +H5P-DEFAULT+
                       buffer)
             (h5sclose memspace)
             (h5sclose data_dataspace))))
  ;; Final write if events don't match buffer size
  (if (not (= 0
              (mod row chunk_size)))
      (progn
        (incf chunk_index)
        (var long final_chunk_size
             (mod row chunk_size))
        (setf (value data_dataset_dims)
              (+ (value data_dataset_dims)
                 final_chunk_size))
        (h5dset-extent data_dataset
                       data_dataset_dims)
        (setf data_dataspace
              (h5dget-space data_dataset))
        (setf (value memspace_dims)
              final_chunk_size)
        (setf (value memspace_maxdims)
              final_chunk_size)

        (setf memspace
              (h5screate-simple 1 memspace_dims memspace_maxdims))

        (setf (value start) (* chunk_size
                               chunk_index))

        (setf (value stride) 1)
        (setf (value cnt) 1)
        (setf (value blck) final_chunk_size)

        (h5sselect-hyperslab data_dataspace
                             :H5S-SELECT-SET
                             start stride cnt blck)
        (h5dwrite data_dataset
                  data_datatype
                  memspace
                  data_dataspace
                  +H5P-DEFAULT+
                  buffer)
        (h5sclose memspace)
        (h5sclose data_dataspace)))
  ;; Cleanup
  (h5fclose outfile)
  ;; Cleanup memory:
  (delete[] name_lengths)
  (if cleanup_field_names
      (delete[] field_names))
  (delete[] nbins)
  (delete[] low)
  (delete[] high)
  (delete[] axes)
  (delete[] buffer)
  (delete name_type_dims)
  (delete binspec_chunkdims)
  (delete binspec_dataset_dims)
  (delete binspec_dataset_maxdims)
  (delete start)
  (delete stride)
  (delete cnt)
  (delete blck)
  (delete memspace_dims)
  (delete memspace_maxdims)
  (delete data_dataset_dims)
  (delete data_dataset_maxdims)
  (delete data_chunk_dims)
  (delete[] count)
  (delete[] xs)
  )

;; Same as above but only for THXF types
(defcppfun void write_histogram_float
    ((var (pointer void) hist)
     ;; May be able to remove in future
     (var int ndims)
     (var string filename)
     (var (pointer string) field_names))
  (var hid-t outfile
       (H5Fcreate (method filename c-str)
                  +H5F-ACC-TRUNC+
                  +H5P-DEFAULT+
                  +H5P-DEFAULT+))

  (var bool cleanup_field_names
       false)

  ;; Compute or set field_names information
  (var (pointer int) name_lengths
       (new[] int ndims))
  (var int max_name_length 0)
  (if (= field_names 0)
      (progn
        (setf cleanup_field_names true)
        (setf field_names
              (new[] string ndims))
        ;; setup field_names
        (for (var int i 0) (< i ndims) (incf i)
             (var stringstream ss)
             (<< ss (str "x") (+ 1 i))
             (setf (aref field_names i)
                   (method ss stringstream.str)))))
  (for (var int i 0) (< i ndims) (incf i)
       (setf (aref name_lengths i)
             (method (aref field_names i)
                     length))
       (if (> (aref name_lengths i)
              max_name_length)
           (setf max_name_length
                 (aref name_lengths i))))

  ;; compute nbins, low and high
  (var (pointer long) nbins
       (new[] long ndims))
  (var (pointer double) low
       (new[] double ndims))
  (var (pointer double) high
       (new[] double ndims))

  (var (pointer (pointer TAxis)) axes
       (new[] (pointer TAxis) ndims))

  (cond
    (;; 1-D
     (= ndims 1)
     (var (pointer TH1F) h
          (typecast (pointer TH1F) hist))
     (setf (aref axes 0)
           (pmethod h x-axis)))
    (;; 2-D
     (= ndims 2)
     (var (pointer TH2F) h
          (typecast (pointer TH2F) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))
     (setf (aref axes 1)
           (pmethod h y-axis)))
    (;; 3-D
     (= ndims 3)
     (var (pointer TH3F) h
          (typecast (pointer TH3F) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))
     (setf (aref axes 1)
           (pmethod h y-axis))
     (setf (aref axes 2)
           (pmethod h z-axis)))
    (;; Sparse
     t
     (var (pointer THnSparseF) h
          (typecast (pointer THnSparseF) hist))
     (for (var int dim 0) (< dim ndims) (incf dim)
          (setf (aref axes dim)
                (pmethod h get-axis
                         dim)))))

  (for (var int dim 0) (< dim ndims) (incf dim)
       (setf (aref nbins dim)
             (pmethod (aref axes dim)
                      axis.nbins))
       (setf (aref low dim)
             (pmethod (aref axes dim)
                      bin-low-edge
                      1))
       (setf (aref high dim)
             (pmethod (aref axes dim)
                      bin-up-edge
                      (aref nbins dim))))

  ;; chunk size (not same as Lisp default, writing all in one chunk)
  (var int chunk_size
       ndims)

  ;; buffer size
  (var long buffer_row_size
       (+ max_name_length
          (* 2
             (sizeof int))
          (* 2
             (sizeof double))))
  (var long buffer_size
       (* buffer_row_size
          chunk_size))

  ;; buffer
  (var (pointer char) buffer
       (new[] char buffer_size))

  ;; HDF5 type
  (var hid-t binspec_datatype
       (h5tcreate +H5T-COMPOUND+
                  buffer_row_size))
  (var (pointer hsize-t) name_type_dims
       (new hsize-t))
  (setf (value name_type_dims)
        max_name_length)
  (var hid-t name_type
       (h5tarray-create2 +H5T-NATIVE-CHAR+
                         1
                         name_type_dims))
  (h5tinsert binspec_datatype
             (str "name")
             0
             name_type)
  (h5tinsert binspec_datatype
             (str "name-length")
             max_name_length
             +H5T-NATIVE-INT+)
  (h5tinsert binspec_datatype
             (str "nbins")
             (+ max_name_length
                (sizeof int))
             +H5T-NATIVE-INT+)
  (h5tinsert binspec_datatype
             (str "low")
             (+ max_name_length
                (* 2 (sizeof int)))
             +H5T-NATIVE-DOUBLE+)
  (h5tinsert binspec_datatype
             (str "high")
             (+ max_name_length
                (* 2 (sizeof int))
                (sizeof double))
             +H5T-NATIVE-DOUBLE+)

  ;; Dataset
  (var hid-t binspec_dataset)

  (var (pointer hsize-t) binspec_chunkdims
       (new hsize-t))
  (setf (value binspec_chunkdims)
        chunk_size)

  (var (pointer hsize-t) binspec_dataset_dims
       (new hsize-t))
  (setf (value binspec_dataset_dims)
        ndims)

  (var (pointer hsize-t) binspec_dataset_maxdims
       (new hsize-t))
  (setf (value binspec_dataset_maxdims)
        +H5S-UNLIMITED+)

  (var hid-t cparms
       (h5pcreate +H5P-DATASET-CREATE+))
  (h5pset-chunk cparms 1 binspec_chunkdims)

  (var hid-t binspec_dataspace
       (h5screate-simple 1
                         binspec_dataset_dims
                         binspec_dataset_maxdims))

  (h5gclose
   (h5gcreate1 outfile (str "/histogram") 0))

  (setf binspec_dataset
        (h5dcreate1 outfile
                    (str "/histogram/bin-specs")
                    binspec_datatype
                    binspec_dataspace
                    cparms))

  (h5sclose binspec_dataspace)

  (var (pointer hsize-t) start
       (new hsize-t))

  (var (pointer hsize-t) stride
       (new hsize-t))

  (var (pointer hsize-t) cnt
       (new hsize-t))

  (var (pointer hsize-t) blck
       (new hsize-t))

  ;; Write bin-specs to file
  (for (var int i 0) (< i ndims) (incf i)
       (var int buffer_index
            (* i
               buffer_row_size))

       ;; name
       (for (var int j 0)
            (< j (method (aref field_names i) length))
            (incf j)
            (setf (aref buffer (+ buffer_index
                                  j))
                  (aref (aref field_names i)
                        j)))

       ;; name-length
       (setf (value
              (typecast (pointer int)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 1))))
             (method (aref field_names i) length))

       ;; nbins
       (setf (value
              (typecast (pointer int)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 2))))
             (aref nbins i))

       ;; low
       (setf (value
              (typecast (pointer double)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 3))))
             (aref low i))

       ;; high
       (setf (value
              (typecast (pointer double)
                        (+ buffer
                           buffer_index
                           (h5tget-member-offset binspec_datatype
                                                 4))))
             (aref high i)))

  (setf binspec_dataspace
        (h5dget-space binspec_dataset))

  (setf (value start) 0)
  (setf (value stride) 1)
  (setf (value cnt) 1)
  (setf (value blck) chunk_size)

  ;; memspace variables
  (var hid-t memspace)

  (var (pointer hsize-t) memspace_dims
       (new hsize-t))
  (setf (value memspace_dims)
        chunk_size)

  (var (pointer hsize-t) memspace_maxdims
       (new hsize-t))
  (setf (value memspace_maxdims)
        chunk_size)

  (setf memspace
        (h5screate-simple 1 memspace_dims memspace_maxdims))

  (h5sselect-hyperslab binspec_dataspace
                       :H5S-SELECT-SET
                       start stride cnt blck)
  (h5dwrite binspec_dataset
            binspec_datatype
            memspace
            binspec_dataspace
            +H5P-DEFAULT+
            buffer)
  (h5sclose memspace)
  (h5sclose binspec_dataspace)
  (h5dclose binspec_dataset)
  (delete[] buffer)

  ;; Histogram data output
  (var hid-t data_dataset)
  (var hid-t data_dataspace)


  ;; NOTE: Something is broken in the chunk-write algorithm such that
  ;; only one chunk seems to be written.  Therefore, this setting is
  ;; not correct, and writing should happen all in one chunk.
  ;;
  ;; Default chunk size for Lisp
  ;; (setf chunk_size
  ;;       1000)

  ;; Calculate chunk size:

  ;; Total number of events written
  (var long row 0)

  (var (pointer hsize-t) data_dataset_dims
       (new hsize-t))
  (setf (value data_dataset_dims)
        0)

  (var (pointer hsize-t) data_dataset_maxdims
       (new hsize-t))
  (setf (value data_dataset_maxdims)
        +H5S-UNLIMITED+)

  ;; Number of count fields:
  (var int n_count_vars)
  (cond
    (;; TH1F
     (= ndims 1)
     (var (pointer TH1F) h
          (typecast (pointer TH1F) hist))
     (if (= 0 (pmethod h get-sumw2-n))
         (setf n_count_vars 1)
         (setf n_count_vars 2)))
    (;; TH2F
     (= ndims 2)
     (var (pointer TH2F) h
          (typecast (pointer TH2F) hist))
     (if (= 0 (pmethod h get-sumw2-n))
         (setf n_count_vars 1)
         (setf n_count_vars 2)))
    (;; TH3F
     (= ndims 3)
     (var (pointer TH3F) h
          (typecast (pointer TH3F) hist))
     (if (= 0 (pmethod h get-sumw2-n))
         (setf n_count_vars 1)
         (setf n_count_vars 2)))
    (;; Sparse
     t
     (var (pointer THnSparseF) h
          (typecast (pointer THnSparseF) hist))
     (if (pmethod h get-calculate-errors)
         (setf n_count_vars 2)
         (setf n_count_vars 1))))

  ;; Datatype size
  (var long data_row_size
       (* (sizeof double)
          (+ ndims
             n_count_vars)))

  ;;; Datatype
  (var hid-t data_datatype
       (h5tcreate +H5T-COMPOUND+
                  data_row_size))
  ;; insert members
  (h5tinsert data_datatype (str "count") 0 +H5T-NATIVE-DOUBLE+)
  (if (= n_count_vars 2)
      (h5tinsert data_datatype (str "count-error") (sizeof double)
                 +H5T-NATIVE-DOUBLE+))
  (for (var int i 0) (< i ndims) (incf i)
       (h5tinsert data_datatype
                  (method (aref field_names i)
                          c-str)
                  (* (sizeof double)
                     (+ i n_count_vars))
                  +H5T-NATIVE-DOUBLE+))

  ;; Amount of data in histogram
  (var long npoints)
  (cond
    (;; TH1F
     (= ndims 1)
     (var (pointer TH1F) h
          (typecast (pointer TH1F) hist))
     (setf npoints
           (pmethod h nbinsx)))
    (;; TH2F
     (= ndims 2)
     (var (pointer TH2F) h
          (typecast (pointer TH2F) hist))
     (setf npoints
           (* (pmethod h nbinsx)
              (pmethod h nbinsy))))
    (;; TH3F
     (= ndims 3)
     (var (pointer TH3F) h
          (typecast (pointer TH3F) hist))
     (setf npoints
           (* (pmethod h nbinsx)
              (pmethod h nbinsy)
              (pmethod h nbinsz))))
    (;; Sparse
     t
     (var (pointer THnSparseF) h
          (typecast (pointer THnSparseF) hist))
     (setf npoints
           (pmethod h axis.nbins))))
  ;; Create dataset
  (if (= npoints 0)
      (setf chunk_size 1)
      (setf chunk_size npoints))
  (setf data_dataspace
        (h5screate-simple 1
                          data_dataset_dims
                          data_dataset_maxdims))
  (setf cparms
        (h5pcreate +H5P-DATASET-CREATE+))
  (var (pointer hsize-t) data_chunk_dims
       (new hsize-t))
  (setf (value data_chunk_dims)
        chunk_size)
  (h5pset-chunk cparms 1 data_chunk_dims)
  (setf data_dataset
        (h5dcreate1 outfile
                    (str "/histogram/data")
                    data_datatype
                    data_dataspace
                    cparms))
  (h5sclose data_dataspace)
  ;; Setup buffer
  (setf buffer
        (new[] char (* data_row_size
                       chunk_size)))

  (var (pointer double) count
       (new[] double n_count_vars))
  (var (pointer double) xs
       (new[] double ndims))
  ;; Row index
  (setf row 0)

  ;; Chunk index
  (var int chunk_index -1)

  (for (var int i 0) (< i npoints) (incf i)
       ;; Set count and xs for each type of histogram
       (cond
         (;; TH1F
          (= ndims 1)
          (var (pointer TH1F) h
               (typecast (pointer TH1F) hist))
          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         (+ i 1)))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (pmethod h
                             bin-error
                             (+ i 1))))
          (setf (aref xs 0)
                (pmethod h
                         get-bin-center
                         (+ i 1))))
         ;; 2-D and 3-D histograms are broken due to i not being a
         ;; useful global index via the standard ROOT functions, so I
         ;; have to add special cases for them.
         (;; TH2F
          (= ndims 2)
          (var (pointer TH2F) h
               (typecast (pointer TH2F) hist))
          (var int x_index)
          (var int y_index)

          ;; Set x,y indices:

          (setf x_index
                (+ (mod i
                        (pmethod h nbinsx))
                   1))
          (setf y_index
                (+ (/ i
                      (pmethod h nbinsx))
                   1))

          ;; Set global index:
          (var int globindex
               (pmethod h
                        get-bin
                        x_index
                        y_index))

          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         globindex))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (pmethod h
                             bin-error
                             globindex)))
          (var (pointer TAxis) xaxis
               (pmethod h x-axis))
          (var (pointer TAxis) yaxis
               (pmethod h y-axis))

          (setf (aref xs 0)
                (pmethod xaxis
                         get-bin-center
                         x_index))
          (setf (aref xs 1)
                (pmethod yaxis
                         get-bin-center
                         y_index)))
         (;; TH3F
          (= ndims 3)
          (var (pointer TH3F) h
               (typecast (pointer TH3F) hist))
          (var int x_index)
          (var int y_index)
          (var int z_index)
          ;; Set x,y,z indices:

          (setf x_index
                (+ (mod i
                        (pmethod h nbinsx))
                   1))
          (setf y_index
                (+ (mod (/ i
                           (pmethod h nbinsx))
                        (pmethod h nbinsy))
                   1))
          (setf z_index
                (+ (/ i
                      (* (pmethod h nbinsx)
                         (pmethod h nbinsy)))
                   1))

          ;; Set global index:
          (var int globindex
               (pmethod h
                        get-bin
                        x_index
                        y_index
                        z_index))

          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         globindex))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (pmethod h
                             bin-error
                             globindex)))
          (var (pointer TAxis) xaxis
               (pmethod h x-axis))
          (var (pointer TAxis) yaxis
               (pmethod h y-axis))
          (var (pointer TAxis) zaxis
               (pmethod h z-axis))

          (setf (aref xs 0)
                (pmethod xaxis
                         get-bin-center
                         x_index))
          (setf (aref xs 1)
                (pmethod yaxis
                         get-bin-center
                         y_index))
          (setf (aref xs 2)
                (pmethod zaxis
                         get-bin-center
                         z_index)))
         (;; Sparse
          t
          (var (pointer THnSparseF) h
               (typecast (pointer THnSparseF) hist))
          (setf (aref count 0)
                (pmethod h
                         get-bin-content
                         i))
          (if (= n_count_vars 2)
              (setf (aref count 1)
                    (sqrt
                     (pmethod h
                              get-bin-error2
                              ;; Old:
                              ;; (+ i 1)
                              i
                              ))))
          (var (pointer int) indices
               (new[] int ndims))
          (pmethod h get-bin-content
                   i
                   indices)
          (for (var int axis_index 0) (< axis_index ndims) (incf axis_index)
               (var (pointer TAxis) axis
                    (pmethod h get-axis axis_index))
               (setf (aref xs axis_index)
                     (pmethod axis get-bin-center
                              (aref indices axis_index))))
          (delete[] indices)))
       ;; Fill buffer whenever count or error are not zero
       (var long buf_index
            (* (mod row chunk_size)
               (+ n_count_vars ndims)))

       (var (pointer double) buf
            (typecast (pointer double) buffer))
       (var bool fill_p false)
       (if (not (= 0
                   (aref count 0)))
           (setf fill_p true))
       (if (= n_count_vars 2)
           (if (not (= 0
                       (aref count 1)))
               (setf fill_p true)))
       (if fill_p
           (progn
             (setf (aref buf buf_index)
                   (aref count 0))
             (if (= n_count_vars 2)
                 (setf (aref buf (+ 1 buf_index))
                       (aref count 1)))
             (for (var int j 0) (< j ndims) (incf j)
                  (setf (aref buf (+ buf_index j n_count_vars))
                        (aref xs j)))
             (incf row)))
       ;; When buffer full, write chunk to dataset
       (if (and (not (= row 0))
                (= 0 (mod row chunk_size)))
           (progn
             (incf chunk_index)
             (setf (value data_dataset_dims)
                   (+ (value data_dataset_dims)
                      chunk_size))
             (h5dset-extent data_dataset
                            data_dataset_dims)
             (setf data_dataspace
                   (h5dget-space data_dataset))
             (setf (value memspace_dims)
                   chunk_size)
             (setf (value memspace_maxdims)
                   chunk_size)

             (setf memspace
                   (h5screate-simple 1 memspace_dims memspace_maxdims))

             (setf (value start) (* chunk_size
                                    chunk_index))
             (setf (value stride) 1)
             (setf (value cnt) 1)
             (setf (value blck) chunk_size)

             (h5sselect-hyperslab data_dataspace
                                  :H5S-SELECT-SET
                                  start stride cnt blck)
             (h5dwrite data_dataset
                       data_datatype
                       memspace
                       data_dataspace
                       +H5P-DEFAULT+
                       buffer)
             (h5sclose memspace)
             (h5sclose data_dataspace))))
  ;; Final write if events don't match buffer size
  (if (not (= 0
              (mod row chunk_size)))
      (progn
        (incf chunk_index)
        (var long final_chunk_size
             (mod row chunk_size))
        (setf (value data_dataset_dims)
              (+ (value data_dataset_dims)
                 final_chunk_size))
        (h5dset-extent data_dataset
                       data_dataset_dims)
        (setf data_dataspace
              (h5dget-space data_dataset))
        (setf (value memspace_dims)
              final_chunk_size)
        (setf (value memspace_maxdims)
              final_chunk_size)

        (setf memspace
              (h5screate-simple 1 memspace_dims memspace_maxdims))

        (setf (value start) (* chunk_size
                               chunk_index))

        (setf (value stride) 1)
        (setf (value cnt) 1)
        (setf (value blck) final_chunk_size)

        (h5sselect-hyperslab data_dataspace
                             :H5S-SELECT-SET
                             start stride cnt blck)
        (h5dwrite data_dataset
                  data_datatype
                  memspace
                  data_dataspace
                  +H5P-DEFAULT+
                  buffer)
        (h5sclose memspace)
        (h5sclose data_dataspace)))
  ;; Cleanup
  (h5fclose outfile)
  ;; Cleanup memory:
  (delete[] name_lengths)
  (if cleanup_field_names
      (delete[] field_names))
  (delete[] nbins)
  (delete[] low)
  (delete[] high)
  (delete[] axes)
  (delete[] buffer)
  (delete name_type_dims)
  (delete binspec_chunkdims)
  (delete binspec_dataset_dims)
  (delete binspec_dataset_maxdims)
  (delete start)
  (delete stride)
  (delete cnt)
  (delete blck)
  (delete memspace_dims)
  (delete memspace_maxdims)
  (delete data_dataset_dims)
  (delete data_dataset_maxdims)
  (delete data_chunk_dims)
  (delete[] count)
  (delete[] xs)
  )

;; Utility function for easily accessing the value of a histogram at a
;; given point
(defcppfun double hist_point_ref
    ((var (pointer void) hist)
     (var (pointer (vector double)) point))
  (var (const int) ndims (pmethod point size))
  (vararray (pointer taxis) axes (ndims))
  (vararray int indices (ndims))
  (var int binindex)
  (cond
    ((= ndims 1)
     (var (pointer TH1D) h
          (typecast (pointer TH1D) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))

     (for (var int i 0)
          (< i ndims)
          (incf i)
          (setf (aref indices i)
                (pmethod (aref axes i)
                         find-bin
                         (aref (value point) i))))
     
     (setf binindex
           (pmethod h get-bin
                    (aref indices 0)))
     (return (pmethod h get-bin-content binindex)))
    ((= ndims 2)
     (var (pointer TH2D) h
          (typecast (pointer TH2D) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))
     (setf (aref axes 1)
           (pmethod h y-axis))

     (for (var int i 0)
          (< i ndims)
          (incf i)
          (setf (aref indices i)
                (pmethod (aref axes i)
                         find-bin
                         (aref (value point) i))))

     (setf binindex
           (pmethod h get-bin
                    (aref indices 0)
                    (aref indices 1)))
     (return (pmethod h get-bin-content binindex)))
    ((= ndims 3)
     (var (pointer TH3D) h
          (typecast (pointer TH3D) hist))
     (setf (aref axes 0)
           (pmethod h x-axis))
     (setf (aref axes 1)
           (pmethod h y-axis))
     (setf (aref axes 2)
           (pmethod h z-axis))

     (for (var int i 0)
          (< i ndims)
          (incf i)
          (setf (aref indices i)
                (pmethod (aref axes i)
                         find-bin
                         (aref (value point) i))))
     
     (setf binindex
           (pmethod h get-bin
                    (aref indices 0)
                    (aref indices 1)
                    (aref indices 2)))
     (return (pmethod h get-bin-content binindex)))
    (t
     (var (pointer THnSparseD) h
          (typecast (pointer THnSparseD) hist))
     (for (var int dim 0) (< dim ndims) (incf dim)
          (setf (aref axes dim)
                (pmethod h get-axis
                         dim)))

     (for (var int i 0)
          (< i ndims)
          (incf i)
          (setf (aref indices i)
                (pmethod (aref axes i)
                         find-bin
                         (aref (value point) i))))
     (setf binindex
           (pmethod h get-bin indices))
     
     (return (pmethod h get-bin-content binindex)))))

;; C++ conversion methods

(defmethod cpp-loader ((obj sparse-histogram))
  'read_histogram)

(defmethod cpp-loader ((obj contiguous-histogram))
  'read_histogram)

;; Writing a histogram into a ROOT file
(defgeneric write-histogram-to-rootfile (hist pathname)
  (:documentation "Writes a histogram to a root file."))

;; General method for sparse and contiguous histograms:
(defmethod write-histogram-to-rootfile
    (hist pathname)
  (let* ((ndims (hist-ndims hist))
         (histpath (cpp-work-path "write-histogram/hist.h5"))
         (type-symbol
          (case ndims
            (1 'TH1D)
            (2 'TH2D)
            (3 'TH3D)
            (otherwise 'THnSparseD)))
         (body
          `((function int main ()
                      (var string outfilename
                           (str ,pathname))
                      (var string infilename
                           (str ,histpath))
                      (varcons TFile outfile
                               (str ,pathname)
                               (str "RECREATE"))
                      (var (pointer ,type-symbol) h
                           (typecast (pointer ,type-symbol)
                                     (read_histogram infilename)))
                      (pmethod h write)
                      (method outfile root-close)
                      (return 0)))))
    (save-object hist histpath)
    (exe-fn (cpp-work-path "write-histogram/exe")
            body
            :output *standard-output*)
    ;; cleanup:
    (delete-file (cpp-work-path "write-histogram/exe"))
    (delete-file (cpp-work-path "write-histogram/exe.cc"))
    (delete-file histpath)
    pathname))

;; Returns a THXD histogram given a THXF histogram
(defun read-histogram-from-rootfile (pathname histname axis-names)
  "This function utilizes the TKey framework to investigate the type
information of histograms in a ROOT file.  MAKE SURE AXIS-NAMES
EXACTLY MATCHES THE HISTOGRAM IN QUESTION.

Set float-p to non-NIL if you want to read a floating point
histogram."
  (let* ((histpath (cpp-work-path "rhfr/hist.h5"))
         result)
    (exe-fn (cpp-work-path "rhfr/exe")
            `((function int main ()
                        ;; Open file and get typename
                        (var string infilename (str ,pathname))
                        (varcons tfile infile
                                 (method infilename c-str)
                                 (str "READ"))
                        (var (pointer tkey) tk
                             (method infile get-key
                                     (str ,histname)))
                        (var string tn
                             (pmethod tk get-class-name))

                        ;; Read histogram:
                        (var (pointer void) h)
                        (method infile GetObject
                                (str ,histname)
                                h)

                        ;; Switch on the type name:
                        (cond
                          ((= tn (str "TH1D"))
                           (var (const int) ndims 1)
                           (vararray string fieldnames
                                     (ndims)
                                     (str ,(first axis-names)))
                           (write_histogram h
                                            ndims
                                            (str ,histpath)
                                            fieldnames))
                          ((= tn (str "TH1F"))
                           (var (const int) ndims 1)
                           (vararray string fieldnames
                                     (ndims)
                                     (str ,(first axis-names)))
                           (write_histogram_float h
                                                  ndims
                                                  (str ,histpath)
                                                  fieldnames))
                          ((= tn (str "TH2D"))
                           (var (const int) ndims 2)
                           (vararray string fieldnames
                                     (ndims)
                                     (str ,(first axis-names))
                                     (str ,(second axis-names)))
                           (write_histogram h
                                            ndims
                                            (str ,histpath)
                                            fieldnames))
                          ((= tn (str "TH2F"))
                           (var (const int) ndims 2)
                           (vararray string fieldnames
                                     (ndims)
                                     (str ,(first axis-names))
                                     (str ,(second axis-names)))
                           (write_histogram_float h
                                                  ndims
                                                  (str ,histpath)
                                                  fieldnames))
                          ((= tn (str "TH3D"))
                           (var (const int) ndims 3)
                           (vararray string fieldnames
                                     (ndims)
                                     (str ,(first axis-names))
                                     (str ,(second axis-names))
                                     (str ,(third axis-names)))
                           (write_histogram h
                                            ndims
                                            (str ,histpath)
                                            fieldnames))
                          ((= tn (str "TH3F"))
                           (var (const int) ndims 3)
                           (vararray string fieldnames
                                     (ndims)
                                     (str ,(first axis-names))
                                     (str ,(second axis-names))
                                     (str ,(third axis-names)))
                           (write_histogram_float h
                                                  ndims
                                                  (str ,histpath)
                                                  fieldnames))
                          ((= tn (str "THnSparseT<TArrayD>"))
                           (var (const int) ndims ,(length axis-names))
                           (vararray string fieldnames
                                     (ndims)
                                     ,@(loop
                                          for a in axis-names
                                          collecting
                                            `(str ,a)))
                           (write_histogram h
                                            ndims
                                            (str ,histpath)
                                            fieldnames))
                          ((= tn (str "THnSparseT<TArrayF>"))
                           (var (const int) ndims ,(length axis-names))
                           (vararray string fieldnames
                                     (ndims)
                                     ,@(loop
                                          for a in axis-names
                                          collecting
                                            `(str ,a)))
                           (write_histogram_float h
                                                  ndims
                                                  (str ,histpath)
                                                  fieldnames)))
                        (return 0)))
            :output *standard-output*)
    (setf result (load-object 'sparse-histogram
                              histpath))
    ;; cleanup after debugging
    result))

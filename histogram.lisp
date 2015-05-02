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

              (incf row)))

    ;;; Create histogram result

    ;; (<< cout
    ;;     (aref nbins 0)
    ;;     endl)
    ;; (<< cout
    ;;     (aref low 0)
    ;;     endl)
    ;; (<< cout
    ;;     (aref high 0)
    ;;     endl)

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
                   (aref high 0)))))
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
                   (aref high 1)))))
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
                   (aref high 2)))))
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
                   high)))))

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

    ;; Read histogram data:
    (for (var int chunk_index 0)
         (< (* chunk_index data_buffer_size)
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
                   (+ (* i data_row_size)
                      (* chunk_size chunk_index)))
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
                                (aref count 1))))))))
    (return result)))

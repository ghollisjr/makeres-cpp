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
         (/ (* binspec_nrows
               (+ (* (sizeof char)
                     (value binspec_name_dims))
                  (sizeof int)
                  (sizeof int)
                  (sizeof double)
                  (sizeof double)))
            (sizeof char)))
    (var long binspec_buffer_size
         (* binspec_row_size
            binspec_chunk_size))
    (setf buffer
          (new[] char binspec_buffer_size))
    
    ;; Switch on number of dimensions
    (var int ndims
         binspec_nrows)

    ;; loop variables
    (var int row 0)
    (varcons (vector int) name_length
              ndims)
    (varcons (vector int) nbins
              ndims)
    (varcons (vector double) low
              ndims)
    (varcons (vector double) high
              ndims)
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
         (var (pointer hsize-t) count
              (new hsize-t))
         (setf (value count)
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
                              count
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
              (setf buffer_index
                    (+ buffer_index
                       (* (sizeof char)
                          (value binspec_name_dims))))
              (setf (aref name_length row)
                    (value
                     (typecast (pointer int)
                               (+ buffer
                                  buffer_index))))
              ;; nbins
              (setf buffer_index
                    (+ buffer_index
                       (sizeof int)))
              (var (pointer int) nbins
                   (typecast (pointer int)
                             (+ buffer
                                buffer_index)))
              ;; low
              (setf buffer_index
                    (+ buffer_index
                       (sizeof int)))
              
              ;; high
              (setf buffer_index
                    (+ buffer_index
                       (sizeof double)))

              (incf row)
              )
         
         )
    ))

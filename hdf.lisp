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

(defmacro defhdffun (fname string)
  `(defcpp ,fname (&rest args)
     (format nil
             ,(string-append string
                             "(~{~a~^,~})")
             (mapcar #'cpp args))))

(with-defcheader "hdf5.h"
  (;; Basic Types
   (defcpp size-t ()
     "size_t")
   (defcpp hid-t  ()
     "hid_t")
   (defcpp herr-t ()
     "herr_t")
   (defcpp hsize-t ()
     "hsize_t")
   (defcpp hssize-t ()
     "hssize_t")
   (defcpp htri-t ()
     "htri_t")

;;;; Enumerations

   ;; H5T_CLASS_T
   (defcpp :H5T-NO-CLASS ()
     "H5T_NO_CLASS")
   (defcpp :H5T-INTEGER ()
     "H5T_INTEGER")
   (defcpp :H5T-FLOAT ()
     "H5T_FLOAT")
   (defcpp :H5T-TIME ()
     "H5T_TIME")
   (defcpp :H5T-STRING ()
     "H5T_STRING")
   (defcpp :H5T-BITFIELD ()
     "H5T_BITFIELD")
   (defcpp :H5T-OPAQUE ()
     "H5T_OPAQUE")
   (defcpp :H5T-COMPOUND ()
     "H5T_COMPOUND")
   (defcpp :H5T-REFERENCE ()
     "H5T_REFERENCE")
   (defcpp :H5T-ENUM ()
     "H5T_ENUM")
   (defcpp :H5T-VLEN ()
     "H5T_VLEN")
   (defcpp :H5T-ARRAY ()
     "H5T_ARRAY")

   ;; H5T_DIRECTION_T
   (defcpp :H5T-DIR-DEFAULT ()
     "H5T_DIR_DEFAULT")
   (defcpp :H5T-DIR-ASCEND ()
     "H5T_DIR_ASCEND")
   (defcpp :H5T-DIR-DESCEND ()
     "H5T_DIR_DESCEND")

   ;; H5S_SELOPER_T
   (defcpp :H5S-SELECT-NOOP ()
     "H5S_SELECT_NOOP")
   (defcpp :H5S-SELECT-SET ()
     "H5S_SELECT_SET")

   ;; constants
   (defcpp +H5S-UNLIMITED+ ()
     "H5S_UNLIMITED")

   (defcpp +H5S-ALL+ ()
     "H5S_ALL")
   (defcpp +H5F-ACC-TRUNC+ ()
     "H5F_ACC_TRUNC")
   (defcpp +H5F-ACC-RDONLY+ ()
     "H5F_ACC_RDONLY")
   (defcpp +H5F-ACC-RDWR+ ()
     "H5F_ACC_RDWR")
   (defcpp +H5P-DEFAULT+ ()
     "H5P_DEFAULT")
   (defcpp +H5P-DATASET-CREATE+ ()
     "H5P_DATASET_CREATE")
   (defcpp +H5T-NATIVE-CHAR+ ()
     "H5T_NATIVE_CHAR")
   (defcpp +H5T-NATIVE-UCHAR+ ()
     "H5T_NATIVE_UCHAR")
   (defcpp +H5T-NATIVE-SHORT+ ()
     "H5T_NATIVE_SHORT")
   (defcpp +H5T-NATIVE-USHORT+ ()
     "H5T_NATIVE_USHORT")
   (defcpp +H5T-NATIVE-INT+ ()
     "H5T_NATIVE_INT")
   (defcpp +H5T-NATIVE-UINT+ ()
     "H5T_NATIVE_UINT")
   (defcpp +H5T-NATIVE-LONG+ ()
     "H5T_NATIVE_LONG")
   (defcpp +H5T-NATIVE-ULONG+ ()
     "H5T_NATIVE_ULONG")
   (defcpp +H5T-NATIVE-LLONG+ ()
     "H5T_NATIVE_LLONG")
   (defcpp +H5T-NATIVE-ULLONG+ ()
     "H5T_NATIVE_ULLONG")
   (defcpp +H5T-NATIVE-FLOAT+ ()
     "H5T_NATIVE_FLOAT")
   (defcpp +H5T-NATIVE-DOUBLE+ ()
     "H5T_NATIVE_DOUBLE")
   (defcpp +H5T-COMPOUND+ ()
     "H5T_COMPOUND")

   ;; hdf functions:

   (defhdffun H5Fcreate "H5Fcreate")

   (defhdffun H5Fopen "H5Fopen")

   (defhdffun H5Pcreate "H5Pcreate")

   (defhdffun H5Pset-chunk "H5Pset_chunk")

   (defhdffun H5Pset-deflate "H5Pset_deflate")

   (defhdffun H5Pget-chunk "H5Pget_chunk")

   (defhdffun H5Tarray-create2 "H5Tarray_create2")

   (defhdffun H5Tcreate "H5Tcreate")

   (defhdffun H5Tclose "H5Tclose")

   (defhdffun H5Tinsert "H5Tinsert")

   (defhdffun H5Dcreate1 "H5Dcreate1")

   (defhdffun H5Dopen2 "H5Dopen2")

   (defhdffun H5Dclose "H5Dclose")

   (defhdffun H5Dwrite "H5Dwrite")

   (defhdffun H5Dread "H5Dread")

   (defhdffun H5Dget-space "H5Dget_space")

   (defhdffun H5Dget-type "H5Dget_type")

   (defhdffun H5Dget-create-plist "H5Dget_create_plist")

   (defhdffun H5Dset-extent "H5Dset_extent")

   (defhdffun H5Fclose "H5Fclose")

   (defhdffun H5Screate-simple "H5Screate_simple")

   (defhdffun H5Sclose "H5Sclose")

   (defhdffun H5Sselect-hyperslab "H5Sselect_hyperslab")

   (defhdffun H5Sget-simple-extent-ndims "H5Sget_simple_extent_ndims")

   (defhdffun H5Sget-simple-extent-dims "H5Sget_simple_extent_dims")

   (defhdffun H5Tget-class "H5Tget_class")

   (defhdffun H5Tget-super "H5Tget_super")

   (defhdffun H5Tget-native-type "H5Tget_native_type")

   (defhdffun H5Tget-size "H5Tget_size")

   (defhdffun H5Tget-array-ndims "H5Tget_array_ndims")

   (defhdffun H5Tget-array-dims2 "H5Tget_array_dims2")

   (defhdffun H5Tget-nmembers "H5Tget_nmembers")

   (defhdffun H5Tget-member-type "H5Tget_member_type")

   (defhdffun H5Tget-member-name "H5Tget_member_name")

   (defhdffun H5Tget-member-offset "H5Tget_member_offset")

   (defhdffun H5Tequal "H5Tequal")

;;; Group functions:

   (defhdffun H5Gcreate2 "H5Gcreate2")

   (defhdffun H5Gcreate1 "H5Gcreate1")

   (defhdffun H5Gclose "H5Gclose"))
  :flags "-lhdf5")

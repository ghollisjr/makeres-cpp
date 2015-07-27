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

(defun root2hdf (name root-file-path hdf-file-path)
  "Converts a ROOT file located at root-file-path to an HDF5 file
located at hdf-file-path with root2hdf script"
  (let* ((tmp-path
          (cut-newline (sh mktemp
                           "-d" "-p" ".")))
         (tmpdir
          (make-pathname :directory
                         (list :relative tmp-path)))
         (convpath
          (merge-pathnames "converter"
                           tmpdir)))
    (with-input-from-string (s root-file-path)
      (run "root2hdf"
           (list name
                 convpath)
           :output *standard-output*
           :input s))
    (run convpath
         (list root-file-path
               hdf-file-path))
    (sb-ext:delete-directory tmpdir :recursive t)
    hdf-file-path))

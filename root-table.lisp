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

;;;; This file defines a data type corresponding to ROOT TTrees or
;;;; TChains stored on disk.
;;;;
;;;; This data type concerns not the tabular data, but the metadata
;;;; involving where the data files are located, the TTree/TChain
;;;; name, etc., whatever is needed for generating the C++ code for
;;;; processing the data.
;;;;
;;;; To support the use of these tables, various utilities are also
;;;; present, e.g. read-fields-types uses a ROOT session and the
;;;; MakeClass function to read the names and types of the leaves in a
;;;; TTree already stored to disk.

(defvar *root-type-map*
  (map->hash-table
   '(("Char_t" . "char")
     ("UChar_t" . "unsigned char")
     ("Short_t" . "short")
     ("UShort_t" . "unsigned short")
     ("Int_t" . "int")
     ("UInt_t" . "unsigned int")
     ("Long_t" . "long")
     ("ULong_t" . "unsigned long")
     ("Float_t" . "float")
     ("Double_t" . "double"))
   'equal))

(defvar *root-branch-type-map*
  (map->hash-table
   '(("Char_t" . "B")
     ("UChar_t" . "b")
     ("Short_t" . "S")
     ("UShort_t" . "s")
     ("Int_t" . "I")
     ("UInt_t" . "i")
     ("Float_t" . "F")
     ("Double_t" . "D")
     ("Long_t" . "L")
     ("ULong_t" . "l")
     ("Bool_t" . "O"))
   'equal))

(defun read-fields-types (paths name)
  "Reads the field names and types of a stored TTree. Output is a list
  with elements of the following form:

  (name-symbol type &key length max-length)

  where length and max-length are only provided when the field is an
  array.  Note that at present only 1-D is supported, although it
  should be easy to extend to work with multiple dimensions once the
  system is working."
  (let* ((startdir
          (pwd))
         (workdir
          (cut-newline
           (sh "mktemp"
               "-d" "-p" ".")))
         (paths (mapcar #'namestring
                        (mapcar #'cl-ana.makeres::ensure-absolute-pathname
                                paths)))
         (branches
          (let* ((newline (format nil "~%"))
                 (root-commands
                  (apply #'string-append
                         (append
                          (list "TChain chain(\"" name "\");" newline)
                          (loop
                             for p in paths
                             appending
                               (list "chain.AddFile(\"" p "\");" newline))
                          (list "chain.MakeClass(\"treeclass\");" newline
                                ".q" newline)))))
            (cd workdir)
            (with-input-from-string (stream root-commands)
              (run "root"
                   (list "-l")
                   :input stream))
            (let ((result
                   (pipe (cat "treeclass.h")
                         (awk "BEGIN {in_branches=0} {if(/Declaration of leaf types/) in_branches=1; else if(/List of branches/) in_branches=0; else if(in_branches) print($0)}")
                         (awk "{if($0) print($0)}"))))
              (cd startdir)
              (sb-ext:delete-directory workdir :recursive t)
              result)))
         (rawvars
          (lines
           (pipe (echo branches)
                 (awk "{print($2)}")
                 (sed "-e" "s/;//"))))
         (arraytypes
          (lines
           (pipe (echo branches)
                 (grep "\\[")
                 (awk "{print($1)}"))))
         (atoms
          (lines
           (pipe (echo (unlines rawvars))
                 (grep "-v" "\\["))))
         (atomtypes
          (lines
           (pipe (echo branches)
                 (grep "-v" "\\[")
                 (awk "{print($1)}"))))
         (atom->type
          (let ((result (make-hash-table :test 'equal)))
            (loop
               for atom in atoms
               for type in atomtypes
               do (setf (gethash atom result)
                        type))
            result))
         (rawarrays
          (lines
           (pipe (echo (unlines rawvars))
                 (grep "\\["))))
         (arrays
          (lines
           (pipe (echo (unlines rawarrays))
                 (sed "-e" "s/\\[.*\\]//"))))
         (array->type
          (let ((result (make-hash-table :test 'equal)))
            (loop
               for array in arrays
               for type in arraytypes
               do (setf (gethash array result)
                        type))
            result))
         (arraymaxsizes
          (lines
           (pipe (echo (unlines rawarrays))
                 (grep "-o" "\\[[[:digit:]]\\+\\]")
                 (grep "-o" "[[:digit:]]\\+"))))
         (array->maxsize
          (let ((result (make-hash-table :test 'equal)))
            (loop
               for array in arrays
               for maxsize in arraymaxsizes
               do (setf (gethash array result)
                        maxsize))
            result))
         (rawsizedarrays
          (lines
           (pipe (echo branches)
                 (grep "//\\[.*\\+\\]"))))
         (sizedarrays
          (lines
           (pipe (echo (unlines rawsizedarrays))
                 (awk "{print($2)}")
                 (sed "-e" "s/\\[.*\\];//"))))
         (arraysizes
          (lines
           (pipe (echo (unlines rawsizedarrays))
                 (awk "{print($3)}")
                 (sed "-e" "s|//||")
                 (sed "-e" "s/\\[//")
                 (sed "-e" "s/\\]//"))))
         (array->size
          (let ((result (make-hash-table :test 'equal)))
            (loop
               for array in sizedarrays
               for size in arraysizes
               do (setf (gethash array result)
                        size))
            result)))
    (append
     ;; atoms
     (loop
        for atom being the hash-keys in atom->type
        for type being the hash-values in atom->type
        collecting (list atom type))
     ;; arrays
     (loop
        for array being the hash-keys in array->type
        for type being the hash-values in array->type
        collecting
          (if (gethash array array->size)
              (list array type
                    :length (gethash array array->size)
                    :max-length (gethash array array->maxsize))
              (list array type
                    :length (gethash array array->maxsize)
                    :max-length (gethash array array->maxsize)))))))

(defun read-nrows (paths name)
  "Reads number of rows from TTree(s)"
  (let* ((tmpdir (mktempdir))
         (tmppath (merge-pathnames
                   "exe"
                   (make-pathname :directory
                                  (list :absolute tmpdir)))))
    (let ((result
           (read-from-string
            (with-output-to-string (s)
              (exe-fn tmppath
                      `((function
                         int main ()
                         (varcons TChain chain
                                  (str ,name))
                         ,@(loop
                              for p in paths
                              collecting
                                `(method chain add-file
                                         (str ,p)))
                         (<< cout (method chain entries) endl)))
                      :output s)))))
      (sb-ext:delete-directory tmpdir :recursive t)
      result)))

(defstruct root-table
  paths
  fields-types ; field names and types
  nrows
  name)

(defun cpp-tab-fields-types->src-fields-types (ft)
  "Converts between output spec and input spec"
  (mapcar (lambda (lst)
            (destructuring-bind (field type &key length max-length)
                lst
              (if max-length
                  (list field type max-length)
                  (list field type))))
          ft))

(defmethod save-object ((tab root-table) path)
  (with-open-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (format file "~s~%" tab)))

(defmethod load-object ((type (eql 'root-table)) path)
  (with-open-file (file path
                        :direction :input)
    (read file)))

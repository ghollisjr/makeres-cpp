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

;;;; This file defines two different data types:
;;;;
;;;; 1. root-table       <--> TTree
;;;; 2. root-table-chain <--> TChain
;;;;
;;;; These data types concern not the tabular data, but the metadata
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-str (x)
    "Provides a shell-like treatment of symbols.  Symbols starting
with $ are evaluated, others are returned as downcased strings."
    (cond
      ((symbolp x)
       (let ((package (symbol-package x))
             (str (string x)))
         (if (char= (elt str 0)
                    #\$)
             (intern (subseq str 1)
                     package)
             (string-downcase str))))
      (t x))))

;; Shell utilities:
(defmacro sh (command &rest args)
  (let ((command
         (to-str command))
        (s (gensym)))
    `(with-output-to-string (,s)
       (run ,command (list ,@args)
            :output ,s))))

(defun cd (dir)
  (sb-posix:chdir dir))

(defun pwd ()
  (sb-posix:getcwd))

(defun cut-newline (string)
  (let ((length (length string)))
    (if (not (zerop length))
        (if (char= #\Newline (elt string (1- length)))
            (subseq string 0 (1- length))
            string)
        "")))

(defmacro pipe (&rest commands)
  "Each command should be a list starting with the command string or
symbol to be lowercased followed by the arguments to that command
which will be evaluated.  The output of the previous command is piped
into the input of the next command."
  (let* ((commands
          (mapcar (lambda (command)
                    (cons (to-str (first command))
                          (rest command)))
                  commands))
         (functions
          (mapcar (lambda (command)
                    (alexandria:with-gensyms (stream s)
                      `(lambda (,stream)
                         (with-output-to-string (,s)
                           (run ,(first command) (list ,@(rest command))
                                :input ,stream
                                :output ,s)))))
                  commands)))
    (alexandria:with-gensyms (fns left right stream)
      `(let ((,fns
              (list ,@functions)))
         (reduce (lambda (,left ,right)
                   (with-input-from-string (,stream ,left)
                     (funcall ,right ,stream)))
                 ,fns
                 :initial-value "")))))

(defun read-fields-types (paths name)
  "Reads the field names and types of a stored TTree. Output is a list
  with elements of the following form:

  (name-symbol type &key length max-length)

  where length and max-length are only provided when the field is an
  array.  Note that at present only 1-D is supported, although it
  should be easy to extend to work with multiple dimensions once the
  system is working."
  (flet ((lines (x)
           (remove ""
                   (split-sequence:split-sequence
                    #\Newline
                    (cut-newline x))
                   :test #'string=))
         ;; Merge lines into single input string
         (unlines (x)
           (apply #'string-append (intersperse (format nil "~%") x))))
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
                            (list "chain->MakeClass(\"treeclass\");" newline
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
                      :max-length (gethash array array->maxsize))))))))

(defstruct root-table
  paths
  fields-types ; field names and types
  name)

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

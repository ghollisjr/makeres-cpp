(in-package :makeres-cpp)

(defcpp var (type name &optional value)
  (with-output-to-string (out)
    (format out "~a ~a" (cpp type) (cpp name))
    (when value
      (format out " = ~a" value))
    (format out ";~%")))

;; Returns a type which requires many type-tokens
(defcpp type (&rest type-tokens)
  (with-output-to-string (out)
    (format out "~{~a~^ ~}"
            (mapcar #'cpp type-tokens))))

;; Returns the pointer type for a given type
(defcpp pointer (type)
  (with-output-to-string (out)
    (format out "~a*" (cpp type))))

(defcpp new (type-or-constructor &rest args)
  (with-output-to-string (out)
    (format out "new ~a" (cpp type-or-constructor))
    (if args
        (format out "(~{~a~^,~})"
                (mapcar #'cpp args))
        (format out "()"))))

(defcpp new[] (type size)
  (with-output-to-string (out)
    (format out "new ~a" (cpp type))
    (format out "[~a]"
            (cpp size))))

(defcpp delete (var)
  (with-output-to-string (out)
    (format out "delete ~a;~%" (cpp var))))

(defcpp delete[] (var)
  (with-output-to-string (out)
    (format out "delete[] ~a;~%" (cpp var))))

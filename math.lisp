(in-package :makeres-cpp)

(defcpp + (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^+~})" (mapcar #'cpp forms))))

(defcpp - (&rest forms)
  (with-output-to-string (out)
    (if (single forms)
        (format out "(-~a)" (cpp (first forms)))
        (format out "(~{~a~^-~})" (mapcar #'cpp forms)))))

(defcpp * (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^*~})" (mapcar #'cpp forms))))

;; Note: Not valid for single argument, only works with two or more arguments
(defcpp / (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^/~})" (mapcar #'cpp forms))))

(defcpp expt (x y)
  (with-output-to-string (out)
    (format out "pow(~a,~a)"
            (cpp x)
            (cpp y))))

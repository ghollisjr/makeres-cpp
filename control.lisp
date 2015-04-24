(in-package :makeres-cpp)

(defcpp progn (&body body)
  (with-output-to-string (out)
    (loop
       for expr in body
       do (format out "~a;~%" (cpp expr)))))

(defcpp let (bindings &body body)
  (with-output-to-string (out)
    (format out "{~%")
    (loop
       for binding in bindings
       do (destructuring-bind (type var &optional init) binding
            (format out "~a ~a" (cpp type) (cpp var))
            (when init
              (format out " = ~a" (cpp init)))
            (format out ";~%")))
    (loop
       for expr in body
       do (format out "~a;~%"
                  (cpp expr)))
    (format out "}~%")))

(defcpp cond (&rest test-bodies)
  (with-output-to-string (out)
    (loop
       for test-body in test-bodies
       for entry from 0
       do (let ((opstr (if (zerop entry)
                           "if"
                           "else if")))
            (destructuring-bind (test &rest body) test-body
              (if (not (eq test t))
                  (format out "~a(~a) {~%"
                          opstr (cpp test))
                  (format out "else {~%"))
              (loop
                 for expr in body
                 do (format out "~a;~%" (cpp expr)))
              (format out "}~%")
              (if (eq test t)
                  (return t)))))))

(defcpp if (test expr &optional else)
  (with-output-to-string (out)
    (format out "if(~a) {~%" (cpp test))
    (format out "~a;~%" (cpp expr))
    (format out "}~%")
    (when else
      (format out "else {~%")
      (format out "~a;~%" (cpp expr))
      (format out "}~%"))))

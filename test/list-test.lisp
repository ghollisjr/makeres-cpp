(require 'makeres-cpp)

(in-package :makeres-cpp)

(setf *cpp-print-progress* 1000000)

(set-cpp-work-path (work-path "cpp-work"))

(defun test ()
  (exe "/home/ghollisjr/test/makeres-cpp/list-test"
       ((function int main ()
                  (varcons (vector double) xs 10)
                  (for (var int i 0)
                       (< i 10)
                       (incf i)
                       (setf (aref xs i) i))
                  (write_list
                   xs
                   (str "/home/ghollisjr/test/makeres-cpp/list-test.txt"))
                  (return 0)))
       :output *standard-output*))

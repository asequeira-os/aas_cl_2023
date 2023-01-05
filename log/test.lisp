(in-package :log-test)

(deftest all-tests
  (and (base-test) (error-test)))

(deftest base-test
  (let ((result
         (with-output-to-string (stream)
           (let ((log::*stream* stream))
             (log-info 2 "hello ~A ~A" 123 "foo")))))
    (is (> (length result) 30))))

(defun raisezerodiv ()
  (eval '(/ 2 0)))

(deftest error-test
  (let ((trace
         (with-output-to-string (stream)
           (let ((log::*stream* stream))
             (verify-error error
                 (with-logged-error
                   (raisezerodiv)))))))
    (is (> (length trace) 40))
    (is (search "DIVISION-BY-ZERO" trace))))

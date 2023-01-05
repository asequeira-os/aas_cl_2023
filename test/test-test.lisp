;;test code
(defpackage :aseq-test-test
  (:use :common-lisp ))
(in-package :aseq-test-test)

(aseq-test:deftest all-tests
  (let ((aseq-test::*debug-on-failure* nil)
        (aseq-test::*debug-on-error* nil)
        (aseq-test::*test-dribble* nil)
        (aseq-test::*failed-count* 0)
        (aseq-test::*passed-count* 0)
        (aseq-test::*tests-status* (make-hash-table :test #'eq)))
    (testbad)
    (assert (= 0 aseq-test::*passed-count*))
    (assert (= 1 aseq-test::*failed-count*))
    (assert (eq (gethash 'testbad aseq-test::*tests-status*) :failed))
    (testgood)
    (assert (= 1 aseq-test::*passed-count*))
    (assert (= 1 aseq-test::*failed-count*))
    (assert (eq (gethash 'testgood aseq-test::*tests-status*) :passed))

    (testerror)
    (assert (eq (gethash 'testerror  aseq-test::*tests-status*) :error))
    (test-verify-error-bad-2)
    (assert (eq (gethash 'test-verify-error-bad-2
                         aseq-test::*tests-status*)
                :error))
    (test-verify-error-bad)
    (assert (eq (gethash 'test-verify-error-bad
                         aseq-test::*tests-status*) :error)))
  ;;if we reached here, things are good
  (setf (gethash 'test-verify-error-bad
                 aseq-test::*tests-status*) :passed)
  (setf (gethash 'testerror aseq-test::*tests-status*) :passed)
  (setf (gethash 'testgood aseq-test::*tests-status*) :passed)
  (setf (gethash 'testbad aseq-test::*tests-status*) :passed)
  (setf (gethash 'test-verify-error-bad-2 aseq-test::*tests-status*)
        :passed)
  (test-verify-error)
  (warg+test 10 20)
  (aseq-test:is t))



(defun incr-test-2 (x)
  (aseq-test:is (> 10 (1+ x)))
  (+ x x))

(aseq-test:deftest testgood
  "test succeeds"
  (aseq-test:is (= 2 (+ 1 1))))

(aseq-test:deftest testbad
  "test fails"
  (aseq-test:is (= 2 3)))

(aseq-test:deftest testerror
  "test raises error"
  (error "some error"))

(define-condition test-condition (error) ())

(aseq-test:deftest test-verify-error
  (aseq-test:verify-error test-condition
      (cause-error)))


(defun cause-error ()
  (error 'test-condition))

(aseq-test:deftest test-verify-error-bad
  (aseq-test:verify-error test-condition
      (error "not a good one"))
  (print "after throwing"))

(aseq-test:deftest test-verify-error-bad-2
  (aseq-test:verify-error test-condition
      (+ 1 2 3))
  (print "after throwing"))

(aseq-test:deftest-with-args warg+test (a b)
  (aseq-test:is (< 10 (+ a b))))


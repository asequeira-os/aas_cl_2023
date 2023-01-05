;;my own test framework
;;
;;rationale for getting out of fiveam/eos
;;fiveam/eos creates a object for every 'is' call.
;;other issues(for me) with them is that the tests are not functions
;;implying composition requires non std CL mechanisms.
;;million tests simply caused things to grind with the 'is' objects


(defpackage :aseq-test
  (:use :common-lisp)
  (:export
   #:*debug-on-failure*
   #:*debug-on-error*
   #:*test-dribble*
   #:*test-verbosity*
   #:*passed-count*
   #:*failed-count*
   #:*skip-succeeded*
   #:*multi-host*
   #:is
   #:is-not
   #:deftest
   #:deftest-with-args
   #:test-message
   #:failed-tests
   #:not-run-tests
   #:verify-error))
(in-package :aseq-test)

(defvar *test-dribble* *standard-output*)
(defvar *test-verbosity* 1)
(defvar *debug-on-error* t)
(defvar *debug-on-failure* t)
(defvar *depth* 0)
(defvar *passed-count* 0)
(defvar *failed-count* 0)
(defvar *skip-succeeded* nil)
(defvar *multi-host* nil)

(declaim (fixnum *passed-count* *failed-count* *depth* *test-verbosity*))

(defun test-message (level format &rest args)
  (when (and (<= level *test-verbosity*)
             *test-dribble*)
    (format *test-dribble* "~%")
    (apply #'format *test-dribble* format args)))

(define-condition assertion-fail (error)
  ((failed-form :initarg :failed-form
                :reader asertion-fail-failed-form))
  (:report (lambda (condition stream)
             (format stream "~%failed form ~A"
                     (asertion-fail-failed-form condition)))))

(defmacro is (test-form)
  (let ((form (gensym))
        (form-value (gensym)))
    `(let* ((,form ',test-form)
            (,form-value ,test-form))
       (test-message 10 "~%; ~A => ~A" ,form ,form-value)
       (loop (when ,form-value
               (incf *passed-count*)
               (return))
          (test-message 2  "failed form ~A" ,form)
          (cerror "enter a different form"
                  'assertion-fail
                  :failed-form ,form
                  :allow-other-keys t)
          (setf ,form (read))
          (setf ,form-value ,form)))))

(defmacro is-not (test-form)
  `(is (not ,test-form)))

(defmacro verify-error (error-class test-form &body body)
  `(or (handler-case
           (progn
             (,@test-form)
             nil)
         (,error-class (e)
           (progn
             ,@body
             (incf *passed-count*)
             (test-message 5 "caught expected error ~A" e)
             t)))
       (error "expected error of type ~A not thrown" ',error-class)))

(defvar *tests-status* (make-hash-table :test #'eq))
(defvar *tests-time* (make-hash-table :test #'eq))

;;todo 2 I am still not able to handle test return values
(defmacro deftest-with-args (name args &body body)
  (multiple-value-bind (forms decl doc)
      (util:parse-body body :documentation t)
    (setf (gethash name *tests-status*) nil)
    (let* ((args args)
           (null-args (null args)))
      `(defun ,name ,args
         ,@(when doc `(,doc))
         ,@decl
         (when (and  ,null-args *skip-succeeded*
                     (eql (gethash ',name *tests-status*) :passed))
           (return-from ,name t))
         (unwind-protect
              (progn
                (when (zerop *depth*)
                  (setf *passed-count* 0)
                  (setf *failed-count* 0))
                (incf *depth*)
                (test-message 5 "test ~A" ',name)
                (setf (gethash ',name *tests-status*) :running)
                (handler-bind
                    ((assertion-fail (lambda (e)
                                       (setf (gethash ',name *tests-status*) :failed)
                                       (incf *failed-count*)
                                       (unless *debug-on-failure*
                                         (test-message 1 "~A failed: ~A" ',name e)
                                         (return-from ,name nil))))
                     (error (lambda (e)
                              (setf (gethash ',name *tests-status*) :error)
                              (unless (or *debug-on-error* (typep e 'assertion-fail))
                                (incf *failed-count*)
                                (test-message 1 "~A failed with error: ~A" ',name  e)
                                (return-from ,name nil)))))
                  (let ((start-time (get-internal-run-time)))
                    ,@forms
                    (setf (gethash ',name *tests-status*) :passed)
                    (test-message 10 "~%end test ~A" ',name)
                    (setf (gethash ',name *tests-time*)
                          (/ (- (get-internal-run-time) start-time)
                             internal-time-units-per-second)))
                  t))
           (decf *depth*)
           (when (zerop *depth*)
             (print-stats)))))))

(defmacro deftest (name &body body)
  `(deftest-with-args ,name nil ,@body))

(defun print-stats ()
  (test-message 1 "checks: passed ~:D failed ~:D"
                *passed-count* *failed-count*)
  (let ((pass 0) (fail 0) (err 0) (notrun 0))
    (maphash (lambda (k v)
               (declare (ignorable k))
               (if (null v)
                   (incf notrun)
                   (ecase v
                     (:passed (incf pass))
                     (:failed (incf fail))
                     (:error (incf err))))) *tests-status*)

    (test-message 1 "tests: pass ~A fail ~A error ~A not run ~A"
                  pass fail err notrun))
  *tests-status*)

(defun failed-tests ()
  (list-tests :failed))

(defun not-run-tests ()
  (list-tests nil))

(defun list-tests (status)
  (let ((list (list)))
    (maphash (lambda (k v)
               (if (and (null v) (null status))
                   (push k list)
                   (if (eq status v)
                       (push (list k
                                   (if (eq status :passed)
                                       (floor (gethash k *tests-time*))
                                       -1)) list)))) *tests-status*)
    (if (eq status :passed)
        (sort list #'< :key #'second)
        list)))

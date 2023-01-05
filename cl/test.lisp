(defpackage :aas-cl-test
  (:use :common-lisp :aas-cl :aseq-test :util))

(in-package :aas-cl-test)

(deftest aas-cl-all-tests
  (and (kw-symb-test) (misc-test) (build-string-test)
       (match-readtable-case-test)
       (comparison-test)
       (xor-test)
       (flatten-test)
       (truncate-seq-test)
       (thread-tests)
       (decimals-tests)
       (singleton-tests)))

(deftest kw-symb-test
  (is (eq (keyword-from-symbol 'abc) :abc)))

(deftest misc-test
  (mapcar (lambda (in out)
            (is (equal out (clean-up-spaces in))))
          '(nil "" " " "   " "x" " x" "   x" " x " "xx  yq   z"         )
          '(nil "" ""  ""    "x" "x"  "x"   "x" "xx yq z" ))
  (mapcar (lambda (in out)
            (is (equal out (invert-case in))))
          '(nil "" "x" "X" "XYZ" "Xyz")
          '(nil "" "X" "x" "xyz" "Xyz")
          ))


(deftest build-string-test
  (is
   (and (string= (build-string) "")
        (string= (build-string "x") "x")
        (string= (build-string "xy" "ab") "xyab")
        (string= (build-string "xy" "ab") "xyab")
        (string= (build-string "xy" "ab" "cd") "xyabcd")
        (string= (build-string "xy" "" "ab") "xyab"))))


(deftest match-readtable-case-test
  (is (string= (match-readtable-case "fooX") "FOOX"))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :downcase)
    (is (string= (match-readtable-case "fooXq") "fooxq")))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :upcase)
    (is (string= (match-readtable-case "fooXq") "FOOXQ")))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (is (string= (match-readtable-case "fooXq") "fooXq"))
    (is (string= (match-readtable-case "XXX") "xxx"))
    (is (string= (match-readtable-case "xxx") "XXX")))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (is (string= (match-readtable-case "fooXq") "fooXq"))))

(deftest thread-tests
  (and (threads-1) (threads-2) (thread-local)))

(defvar *fooo* nil)
(defvar *bindings* nil)

(deftest threads-1
  (let* ((fn (lambda ()
               (format t "~A " *fooo*)))
         (output (with-output-to-string (*standard-output*)
                   (let ((*bindings*
                          (list (cons '*fooo*  123)
                                (cons '*standard-input*  *standard-input*)
                                (cons '*standard-output*  *standard-output*)
                                (cons '*error-output*  *error-output*)
                                (cons '*query-io*  *query-io*)
                                (cons '*debug-io*  *debug-io*)
                                (cons '*trace-output*  *trace-output*))))
                     (mp-make-thread "test1" fn *bindings*)
                     (mp-make-thread "test2" fn *bindings*)
                     (sleep 2)))))
    (is (equal "123 123 " output))))

(deftest threads-2
  (let ((i 0)
        (l1 (mp-make-lock "l1"))
        (l2 (mp-make-lock "l2")))
    (is (and l1 l2))
    (is (not (eq l1 l2)))
    (flet ((thread-fn ()
             (mp-with-lock (l1)
               (mp-with-lock (l2)
                 (incf i)))))
      (mp-make-thread "t1" #'thread-fn nil)
      (mp-make-thread "t1" #'thread-fn nil))
    (sleep 1)
    (is (= i 2))))

(defvar *local-1* nil)

(deftest thread-local
  (labels ((fn-2 ()
             (mp-thread-local *local-1* (+ 2 5)
               (is (= 5 *local-1*))))
           (thread-fn ()
             (mp-thread-local *local-1* (+ 2 2)
               (is (= 4 *local-1*))
               (incf *local-1*)
               (fn-2)
               (sleep (random 3)))))
    (loop for j from 1 to 10 do
         (mp-make-thread "t1" #'thread-fn nil)))
  (sleep 5)
  (is (null *local-1*)))

(def-comparison "tc" 'integer > < =)

(deftest comparison-test
  (dolist (op (list #'tc= #'tc>= #'tc<= #'tc< #'tc>))
    (is (funcall op 3)))
  (let ((eq-data '((3 3) (3 3 3) (3 3 3 3))))
    (dolist (eq-data eq-data)
      (is (apply #'tc= eq-data))
      (is (apply #'tc>= eq-data))
      (is (apply #'tc<= eq-data))
      ;;(is-not (apply #'tc/= eq-data))
      (is-not (apply #'tc< eq-data))
      (is-not (apply #'tc> eq-data))))
  (let ((lt '((3 4) (3 4 4 5 6 6) (3 4 4 5 6 6 7))))
    (dolist (lt lt)
      (is (apply #'tc<= lt))
      (is-not (apply #'tc>= lt))
      (let ((gt (reverse lt)))
        (is (apply #'tc>= gt))
        (is-not (apply #'tc<= gt))))))

(deftest truncate-seq-test
  (is (null (truncate-seq nil 2)))
  (is (equalp (truncate-seq #(1 2 3) 2) #(1 2)))
  (is (equalp (truncate-seq #(1 2 3) 3) #(1 2 3)))
  (is (equalp (truncate-seq #(1 2 3) 4) #(1 2 3)))
  (is (equalp (truncate-seq #() 3) #()))
  (is (equalp (truncate-seq #() 0) #()))
  (is (equalp (truncate-seq #(1 2 3) 0) #())))

(deftest flatten-test
  (let ((inv  (vector nil '(1) '(1 2) '(1 2 3) '(1 (2 3))
                      '((1 2) 3) '(1 (2 nil) 3 nil 4) '(nil)))
        (outv (vector nil '(1) '(1 2) '(1 2 3) '(1 2 3)
                      '(1 2 3)   '(1 2 nil 3 nil 4)   '(nil)))
        (nnil (vector nil '(1) '(1 2) '(1 2 3) '(1 2 3)
                      '(1 2 3)   '(1 2 3 4)     nil)))
    (loop for i from 0 to (1- (length inv)) do
         (let ((in (aref inv i))
               (out (aref outv i))
               (nn (aref nnil i)))
           (let ((r (flatten in t)))
             (is (equalp out r)))
           (let ((r (flatten in)))
             (is (equalp nn r)))))))


(defvar *singleton-test-count* 0)

(let ((rs (make-random-state t)))
  (defun test-incrementer ()
    (let ((now (get-universal-time)))
      (declare (ignorable now))
      ;;(format t "instance  at ~A start~%" now)
      (if (zerop (random 2 rs))
          (error "kill me")
          (incf *singleton-test-count*))
      (sleep (random 5 rs))
      (unless (= 1 *singleton-test-count*)
        (print "test-incrementer fail")
        (assert "fail"))
      ;;(format t "instance  at ~A end~%" now)
      (decf *singleton-test-count*))))

(deftest singleton-tests
  (let ((singleton (singleton #'test-incrementer t)))
    (loop for i from 1 to 10 do
         (mp-make-thread "foo" (lambda () (funcall singleton)))
         (sleep (random 2)))
    (sleep 6)
    (is (zerop *singleton-test-count*))))

(deftest xor-test
  (is-not (xor nil nil))
  (is (xor nil t))
  (is (xor t nil ))
  (is-not (xor t t)))

;;decimal tests
(deftest decimals-tests
  (let ((num (/ 41 4)))
    (is (equal "10.25" (decimal->string num)))
    (is (equal num
               (parse-decimal (decimal->string num))))
    (let* ((neg (- 0 num))
           (ans (parse-decimal (decimal->string neg))))
      (is (equal "-10.25" (decimal->string neg)))
      (is (equal neg ans)))))

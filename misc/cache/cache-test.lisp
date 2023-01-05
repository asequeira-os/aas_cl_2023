(in-package :cache-test)

(define-constant MAX-TEST-KEY 100)

(defstruct tdata
  k
  v
  s)

(defmethod cache:cache-key ((object tdata))
  (test-key (tdata-k object)))

(defmethod cache:cache-ttl ((object tdata))
  (random 5))


(defun test-key (int)
  (format nil "~A" int))

(deftest cache-all-tests
  (let ((data (make-test-data))
        (cache (cache:make-aas-cache (/ MAX-TEST-KEY 5))))
    (loop for i from 1 to (* 5 MAX-TEST-KEY) do
         (when (= i MAX-TEST-KEY)
           (sleep 5))
         (cache:cache-get cache (test-key (random  (/ MAX-TEST-KEY 2))))
         (cache:cache-get cache (test-key (random  (* MAX-TEST-KEY 2))))
         (let ((entry (gethash (random MAX-TEST-KEY) data)))
           (is (eq entry
                   (cache:cache-put cache entry)))))
    (is (cache::validate-cache cache t))

    (cache:cache-remove cache  (test-key 5))
    (cache:cache-put cache (gethash 10 data))

    (cache:with-cached-object obj10 (test-key 10) cache
                              (error "should not execute")
                              (is obj10))

    (cache:with-cached-object obj5 (test-key 5) cache
                              (make-test-datum 5)
                              (is obj5))

    (loop for i from 1 to (/ MAX-TEST-KEY 3) do
         (cache:cache-remove cache  (test-key (random  (/ MAX-TEST-KEY 2))))
         (cache:cache-get cache (test-key (random  (* MAX-TEST-KEY 2)))))
    ;;(break)
    (is (cache::validate-cache cache nil))
    t))

(defun make-test-data ()
  (let ((data (make-hash-table :test #'equal)))
    (loop for i from 0 to MAX-TEST-KEY do
         (setf (gethash i data) (make-test-datum i)))
    data))

(defun make-test-datum (i )
  (let ((v (random 200)))
    (make-tdata :k i :v v
                :s (format nil "foo ~A is ~A" i v))))


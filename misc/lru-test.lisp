(in-package :aas-misc-test)

(deftest lru-tests
  (and (lru-test-1) (lru-test-2) (lru-test-3)))

(deftest lru-test-1
  (let ((lru (lru-make 5)))
    (multiple-value-bind (value found)
        (lru-get lru 1)
      (is (null value))
      (is (null found)))
    (loop for i from 1 to 5 do
         (lru-add lru i (* i i)))
    (loop for i from 1 to 5 do
         (multiple-value-bind (value found)
             (lru-get lru i)
           (is found)
           (is (= (* i i) value))))
    (lru-get lru 1)
    (lru-add lru 6 (* 6 6))
    (dolist (i '(1 3 4 5 6))
      (multiple-value-bind (value found)
          (lru-get lru i)
        (is found)
        (is (= (* i i) value))))
    (multiple-value-bind (value found)
        (lru-get lru 2)
      (is (null found))
      (is (null value)))))

(deftest lru-test-2
  (let ((lru (lru-make 5)))
    (loop for i from 1 to 4 do
         (lru-add lru i (* i i)))
    (loop for i from 1 to 20 do
         (lru-add lru 5 (* 5 5)))
    (is (= (lru-get lru 3) (* 3 3)))))

(deftest lru-test-3
  (let ((lru (lru-make 1000)))
    (flet ((thread-a ()
             (loop for i from 1 to 10000 do
                  (lru-add lru i (* i i))))
           (thread-b ()
             (dolist (str '("a" "b" "c" "d" "e"))
               (lru-add lru str (concatenate 'string str " " str)))))
      (let ((threads (loop for i from 1 to 100
                        collect (bt:make-thread (if (zerop (random 2))
                                                    #'thread-a
                                                    #'thread-b)))))
        (loop for i from 1 to 100000 do
             (let ((r (lru-get lru i)))
               (is (or (null r) (= r (* i i))))))
        (dolist (thread threads)
          (bt:join-thread thread))))))

(in-package :sampling-test)

(deftest all-tests
  (reservoir-z-test)
  (reservoir-r-test))

(deftest reservoir-r-test
  (let ((rr (create-reservoir-r 25)))
    (loop
       for i from 1 to 100000 do
         (process rr i))
    (let ((s (coerce (get-sample rr) 'list)))
      (is (every #'identity s))
      (is (not (apply #'< s))) ; could fail - unlucky
      (is (not (some #'null s))))))


(deftest reservoir-z-test
  (let ((rr (create-reservoir-z 25)))
    (loop
       for i from 1 to 10000000 do
         (process rr i))
    (let ((s (coerce (get-sample rr) 'list)))
      (is (every #'identity s))
      (is (not (apply #'< s))) ; could fail - unlucky
      (is (not (some #'null s))))))

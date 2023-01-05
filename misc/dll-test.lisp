(in-package :aas-misc-test)

(deftest dll-tests
  (and (dll-test-1) (dll-test-2)))

(deftest dll-test-1
  (let ((dll (dll-make)))
    (is-not (null dll))
    (is (dll-empty-p dll))
    (loop for i from 1 to 10
         collecting (dll-add-head dll i))
    (is-not (dll-empty-p dll))
    (loop for i from 1 to 10 do
         (is (= i (dll-node-value (dll-remove-tail dll)))))
    (is (dll-empty-p dll))
    (dll-remove-tail dll)
    (is (dll-empty-p dll))
    (loop for i from 1 to 5 do
         (dll-add-head dll i))
    (let ((xnode (dll-add-head dll 'x)))
      (loop for i from 6 to 10 do
           (dll-add-head dll i))
      (is (eq (dll-remove dll xnode) xnode))
      (loop for i from 1 to 10 do
           (is (= i (dll-node-value (dll-remove-tail dll))))))
    (is (dll-empty-p dll))))

(deftest dll-test-2
  (let ((dll (dll-make)))
    (is (dll-empty-p dll))
    (let ((node (dll-add-head dll 123)))
      (is-not (dll-empty-p dll))
      (dll-remove dll node)
      (is (dll-empty-p dll)))))

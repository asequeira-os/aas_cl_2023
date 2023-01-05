(in-package :scratch.test)

(in-suite :scratch.test)

(fiveam:test nearest-binary-search3
  (let* ((list (list 12 45 789 41 35 89 456))
         (vector (coerce (sort list #'<) 'vector)))
    (fiveam:is (= 12 (nearest-binary-search 10 vector)))
    (fiveam:is (= 12 (nearest-binary-search 11 vector)))
    (fiveam:is (= 12 (nearest-binary-search 12 vector)))
    (fiveam:is (= 12 (nearest-binary-search 13 vector)))
    (fiveam:is (= 12 (nearest-binary-search 34 vector)))
    (fiveam:is (= 35 (nearest-binary-search 35 vector)))
    (fiveam:is (= 35 (nearest-binary-search 36 vector)))
    (fiveam:is (= 35 (nearest-binary-search 39 vector)))
    (fiveam:is (= 35 (nearest-binary-search 40 vector)))
    (fiveam:is (= 456 (nearest-binary-search 456 vector)))
    (fiveam:is (= 456 (nearest-binary-search 457 vector)))
    (fiveam:is (= 456 (nearest-binary-search 788 vector)))
    (fiveam:is (= 789 (nearest-binary-search 789 vector)))
    (fiveam:is (= 789 (nearest-binary-search 1000 vector)))))

(fiveam:test consistent-hash-test-1
  (fiveam:is (= 1096121493 (hash-string "foooo")))
  (let* ((ch (algorithm t))
         (nodes-a (list "foobar-a" "foobar-b" "foobar-c"
                        "foobar-d" "xooxah-1" "xooxah-2")))
    (fiveam:is (not (null ch)))
    (dolist (nodename nodes-a)
      (add-node ch nodename))
    ;(print (slot-value ch 'combined-nodes))
    (dolist (nodename nodes-a)
      (let ((1stnode (concatenate 'string nodename "1")))
      (fiveam:is (equal (cdr (get-key-node ch 1stnode)) nodename))))))

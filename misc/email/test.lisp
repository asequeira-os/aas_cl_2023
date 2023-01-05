(in-package :email-test)

(deftest email-tests
  (let ((ivl (list nil "" "a" "a@b"))
        (vl (list "a@b.in" "hjkhjkhkjhhjk@foo.dfds.com")))
    (dolist (v vl)
      (is (validate v)))
    (dolist (iv ivl)
      (is-not (validate iv)))))

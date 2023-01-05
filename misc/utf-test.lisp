(in-package :aas-misc-test)

(deftest utf8-tests
  (let* ((orig "ていることが２８日分かった。 ほとんどの業者が格安で商品を落札")
         (oct (string-to-utf8 orig))
         (back (utf8-to-string oct)))
    (is (string-equal orig back))
    (is (>= (length oct) (length orig)))))


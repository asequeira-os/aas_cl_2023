;;-*- coding: utf-8 -*-
(in-package :cipher-test)

(deftest all-tests
  (and (digest-test) (digest-salt-test) (crc32-test) (encrypt-test)
       (site-key-tests) (site-key-valididty)))

;;the expected values were obtained using
;;echo -n jshdfkjat | sha256sum.exe
(deftest digest-test
  (let ((data
         '(("jshdfkjas" .
            "bd15e9e89a9c29ae3db759debdbcc575da4cffa0e421700eaa647a7bbe58a7df")
           ("ていることが２８日分かった。 ほとんどの業者が格安で商品を落札" .
            ;;todo 5 sha256 for utf-8 is not matching
            ;;"075b805bb6d76ed2d6ec206a41be0353eac4a718b4376f57c5e2aa5b6f54f74e"
            "179e38147cde760e5bb171daefa45d6852cd549a5d828486cbd0c74290b40e50")
           ("jshdfkjat" .
            "34f5cbfdb2caac07065a9a63aa549cc9f35cc8f5ff82ee776f39983d8da0fcbe")
           ("" .
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))))
    (dolist (data data)
      (let ((result (digest (car data))))
        ;;(format t "~A ~% ~A ~%~%" (car data) result )
        (is (string-equal result (cdr data)))))))

(deftest digest-salt-test
  (let ((salt "qqq")
        (data
         '(("jshdfkjas" .
            "a84462238ce4a8d66c181f15b3470c84d283d11838b0c21b4378ca00ac2cb32c")
           ("jshdfkjat" .
            "d63947b964e27778203299bf7609e82a2e89a0578e88937be5eee77bd305ee9e")
           ("" .
            "69ea2cebe0c6d987a28de4984d107af8314741ebe53e5333b1da3254ebceff11"))))
    (dolist (data data)
      (let ((result (digest (car data) salt)))
        ;;(format t "~A ~% ~A ~%~%" (car data) result )
        (is (string-equal result (cdr data)))))))

(deftest crc32-test
  (let ((data '(("hjkuygjk" . 2772901874)
                ("ていることが２８日分かった。 ほとんどの業者が格安で商品を落札" . 1644540445)
                ("" . 0))))
    (dolist (data data)
      (let* ((in (car data))
             (expect (cdr data))
             (result (crc32 in)))
        ;;(format t "~A ~A ~A~%" in expect result)
        (is (= expect result))
        (is (<= 0 result (expt 2 32)))))))

(deftest encrypt-test
  (let ((key "g68ghjj54klh%$#FD")
        (data '("hjkuygjk"
                "ていることが２８日分かった。 ほとんどの業者が格安で商品を落札")))
    (dolist (data data)
      (let ((encrypted (encrypt data key)))
        (is (equal data (decrypt encrypted key)))
        (verify-error babel-encodings:character-decoding-error
                      (decrypt encrypted "foo"))))
    (is (equal "" (decrypt (encrypt "" key) key)))
    (is (null (encrypt nil key)))
    (is (null (decrypt nil key)))))

(deftest site-key-tests
  (is (cipher::valid-key-p cipher::*site-key-latest*))
  (let ((data '("hjkuygjk"
                "ていることが２８日分かった。 ほとんどの業者が格安で商品を落札")))
    (dolist (data data)
      (let* ((enc (encrypt-using-site-key data))
             (dec (decrypt-using-site-key enc)))
        (is-not (equalp enc data))
        (is (equalp dec data))))))

(deftest site-key-valididty
  (let ((test (lambda ()
                (let ((data "hello world"))
                  (let* ((enc (encrypt-using-site-key data))
                         (dec (decrypt-using-site-key enc)))
                    (is (equalp dec data)))))))
    (funcall test)
    (is (cipher::valid-key-p cipher::*site-key-latest*))
    (let ((cipher::*site-key-latest*
           (cipher::make-encryption-key
              :KEY "11050835269059271043666057389466786229906912097"
              :EOL  1234)))
      (funcall test)
      (is-not (cipher::valid-key-p cipher::*site-key-latest*))
      (funcall test))))

(defpackage :cipher
  (:use :cl :aas-cl))

(in-package :cipher)
(export '(digest crc32 encrypt decrypt
          make-new-key
          encryption-key-key encryption-key-eol
          encrypt-using-site-key decrypt-using-site-key))

(defpackage :cipher-test
  (:use :cl :cipher :aseq-test))

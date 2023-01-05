(defpackage :aas-http-common
  (:use :common-lisp :aas-cl :drakma)
  (:shadow #:url-encode))

(in-package :aas-http-common)

(export '(url-encode make-base-url make-full-url))

(defpackage :aas-http-common-test
  (:use :common-lisp :aas-cl :aas-http-common :aseq-test))

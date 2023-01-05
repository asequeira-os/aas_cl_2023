(defpackage :email
  (:use :cl :aas-cl))

(in-package :email)

(export '(load-config send-email validate))

(defpackage :email-test
  (:use :cl :aas-misc :aseq-test :email))

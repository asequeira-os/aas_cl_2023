(defpackage :db-cloud
  (:use :cl :aas-cl :db-base))

(in-package :db-cloud)

(export '(init-cloud init-node cloud-db-ctor get-db-num))

(defpackage :db-cloud-test
  (:use :common-lisp :aas-cl :db-cloud :aseq-test))

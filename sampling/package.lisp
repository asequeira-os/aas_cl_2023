(defpackage :sampling
  (:use :cl ))

(in-package :sampling)
(export '(process get-sample
          create-reservoir-r
          create-reservoir-z))

(defpackage :sampling-test
  (:use :cl :sampling :aseq-test))

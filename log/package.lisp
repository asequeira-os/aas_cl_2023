(defpackage :log
  (:use :cl :aas-cl))

(in-package :log)
(export '(
          log-fail
          log-flush
          log-info
          log-init
          log-rotate
          log-warn
          with-logged-error))

(defpackage :log-test
  (:use :cl :log :aseq-test))

(in-package :log-test)

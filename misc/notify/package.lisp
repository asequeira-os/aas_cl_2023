(defpackage :notify
  (:use :cl :aas-cl))

(in-package :notify)
(export '(define-notifier add-listener remove-listener notify-all))

(defpackage :notify-test
  (:use :cl :notify :aseq-test))
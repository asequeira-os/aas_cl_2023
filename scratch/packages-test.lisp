(in-package :cl-user)

(defpackage :scratch.test
  (:use :cl :scratch :fiveam))

(in-package :scratch.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-modules*)))

(def-suite :scratch.test)


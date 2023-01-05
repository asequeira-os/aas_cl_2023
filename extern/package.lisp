(defpackage :extern
  (:use :cl :aas-cl))

(in-package :extern)

(export '(
          define-enum
          enum-list
          enum-ordinal
          enum-vector-from-db
          enum-vector-to-db
          enum< enum> enum=
          ))

(defpackage :extern-test
  (:use :cl :extern :db-base :aseq-test))


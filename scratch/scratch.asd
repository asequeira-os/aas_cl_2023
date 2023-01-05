;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(defpackage #:scratch.system
  (:use :common-lisp :asdf))

(in-package #:scratch.system)


(defsystem :scratch
  :serial t
  :depends-on (
               :portable-threads
               :lisa
               :hunchentoot
               :cl-who
               :local-time
               :cl-utilities
               :postmodern
               :ironclad
               :arnesi
               :fiveam
               :cl-ppcre
               :alexandria
               :split-sequence
               )
  :components ((:file "packages")
               (:file "sequence-server")
               (:file "caldb")
               (:file "cal-times")
               (:file "server")

               (:file "auth-base")
               (:file "consistent-hashing")
               (:file "custom-iterator")
               (:file "lock")
               (:file "lru-cache")
               (:file "scratch-misc")

               )
  )


(defsystem #:scratch.test
  :serial t
  :depends-on (:scratch :fiveam)
  :components (
               (:file "packages-test")
               (:file "caldb-test")
               (:file "auth-base-test")
               (:file "cal-times-test")
               (:file "consistent-hashing-test")
               (:file "custom-iterator-test")
               (:file "lock-test")
               (:file "lru-cache-test")
               )
  )

(defmethod perform ((op test-op) (system (eql (find-system :scratch))))
  (operate 'load-op '#:scratch.test))

(defmethod perform :after ((op test-op) (system (eql (find-system :scratch))))
  (funcall (read-from-string "util:test") :scratch.test))
;  (util:test :scratch.test))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :scratch))))
  nil)


(defpackage :auth-remote
  (:use :cl :aas-cl :db-base :aas-rpc)
  (:import-from :auth
                auth-token-a
                auth-token-g
                auth-token-h
                auth-token-l
                auth-token-tz))

(in-package :auth-remote)
(export '(auth-verify-remote auto-bind-db))

(defpackage :auth-remote-test
  (:use :cl :auth-remote :aseq-test))



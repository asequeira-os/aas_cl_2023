(defpackage :xed
  (:use :cl :aas-cl :db-base :aas-rpc :cloud :time :extern))

(in-package :xed)
(export '(+xeduler+
          ))

(defpackage :xed-test
  (:use :cl :xed :aseq-test :db-base :aas-rpc :cloud :time :extern :auth-test)
  (:import-from :auth
                login-response-login-token
                )
  (:import-from :auth-test
                test-user-orig-login
                test-user-login)
  (:import-from :xed
                +service+
                calendar-id
                make-alarm-info
                make-calendar
                make-work-shift
                shifts-overlap
                work-shift-id
                ))

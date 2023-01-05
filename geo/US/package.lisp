(defpackage :geo-US
  (:use :common-lisp)
  (:export
   :+US-STATE-CODES+
   :+US-STATE-CODES+MIL+
   :+US-STATE-NAMES+
   :US-state-name
   :US-address
   :get-US-state-timezones
   :get-US-timezones
   ))

(in-package :geo-US)

(defpackage :geo-US-test
  (:use :cl :geo-US :aseq-test)
  (:import-from :geo-US
                *us-state-code-to-name*
                *US-state-timezones*
                create-US-address)
  (:import-from :time
                olsen-data-lookup))
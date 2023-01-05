(defpackage :aas-local-time
  (:nicknames :time)
  (:use :common-lisp :aas-cl :util :assoc-db :aas-rpc))

(defpackage :aas-local-time-test
  (:use :common-lisp :aas-cl :aas-local-time :aseq-test))

(in-package :aas-local-time)

(export '(create-date date add-days create-time
          day-of-week
          local-date-time create-date-time combine-date-time
          date-time-offset days-in-month
          days-seconds-to-date-time
          dto tz tz-name
          dto= dto< dto> dto/= dto>= dto<=
          date-difference invalid-date-time
          load-timezones get-timezone with-timezone +timezone-utc+
          to-timezone
          dto-year dto-month dto-day  dto-hour dto-minute dto-second dto-offset dto-seconds-utc
          date-y date-m date-d date-day-num
          dur
          add-duration subtract-duration dow-of-month start-of-next-month
          dto-from-dt date-from-dto time-from-dto dt-from-dto
          *utc-now* *timezone*
          utc-now
          now
          day-start yesterday tomorrow
          tomorrow-day-start yesterday-day-start
          first-of-month first-of-year
          next-hour prev-hour
          days-in-this-month days-in-month
          cron-set-entry
          make-duration
          ))

;; future candidates
;; days-seconds-to-date-time
;; is-valid-date-time  date-difference
;; date-number-to-date date-equal-p
;; date-number day-of-week
;; add-days start-of-next-month last-dow

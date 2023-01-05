(in-package :geo-US)

;;based on
;;http://en.wikipedia.org/wiki/List_of_U.S._states_by_time_zone
(defparameter *US-state-timezones*
  (make-hash-table :size 100 :test #'equal))

(defun set-US-state-timezones (code &rest timezones)
  (setf (gethash code *US-state-timezones*) timezones))

(defun get-US-state-timezones (code)
  "list of main time zones for the state"
  (values (gethash code *US-state-timezones*)))

(let ((list
       (list
        "US/Alaska Standard Time" "US/Central Standard Time"
        "US/Mountain Standard Time" "US/Pacific Standard Time"
        "US/Eastern Standard Time" "US/Guam"
        "US/Hawaii Standard Time" "US/Samoa" )))
  (defun get-US-timezones ()
    list))


(set-US-state-timezones "AL" "America/Chicago" "America/New York")
(set-US-state-timezones "AK" "America/Anchorage")
(set-US-state-timezones "AS" "Pacific/Pago Pago")
(set-US-state-timezones "AZ" "America/Phoenix")
(set-US-state-timezones "AR" "America/Chicago")
(set-US-state-timezones "CA" "America/Los Angeles")
(set-US-state-timezones "CO" "America/Denver")
(set-US-state-timezones "CT" "America/New York")
(set-US-state-timezones "DE" "America/New York")
(set-US-state-timezones "DC" "America/New York")
(set-US-state-timezones "FM" )
(set-US-state-timezones "FL" "America/New York" "America/Chicago")
(set-US-state-timezones "GA" "America/New York")
(set-US-state-timezones "GU" "Pacific/Guam")
(set-US-state-timezones "HI" "Pacific/Honolulu")
(set-US-state-timezones "ID" "America/Denver" "America/Los Angeles")
(set-US-state-timezones "IL" "America/Chicago")
(set-US-state-timezones "IN" "America/New York" "America/Chicago")
(set-US-state-timezones "IA" "America/Chicago")
(set-US-state-timezones "KS" "America/Chicago" "America/Denver")
(set-US-state-timezones "KY" "America/New York" "America/Chicago")
(set-US-state-timezones "LA" "America/Chicago")
(set-US-state-timezones "ME" "America/New York")
(set-US-state-timezones "MH" )
(set-US-state-timezones "MD" "America/New York")
(set-US-state-timezones "MA" "America/New York")
(set-US-state-timezones "MI" "America/New York" "America/Chicago")
(set-US-state-timezones "MN" "America/Chicago")
(set-US-state-timezones "MS" "America/Chicago")
(set-US-state-timezones "MO" "America/Chicago")
(set-US-state-timezones "MT" "America/Denver")
(set-US-state-timezones "NE" "America/Chicago" "America/Denver")
(set-US-state-timezones "NV" "America/Los Angeles" "America/Denver")
(set-US-state-timezones "NH" "America/New York")
(set-US-state-timezones "NJ" "America/New York")
(set-US-state-timezones "NM" "America/Denver")
(set-US-state-timezones "NY" "America/New York")
(set-US-state-timezones "NC" "America/New York")
(set-US-state-timezones "ND" "America/Chicago" "America/Denver")
(set-US-state-timezones "MP" "Pacific/Guam")
(set-US-state-timezones "OH" "America/New York")
(set-US-state-timezones "OK" "America/Chicago")
(set-US-state-timezones "OR" "America/Los Angeles" "America/Denver")
(set-US-state-timezones "PW" )
(set-US-state-timezones "PA" "America/New York")
(set-US-state-timezones "PR" )
(set-US-state-timezones "RI" "America/New York")
(set-US-state-timezones "SC" "America/New York")
(set-US-state-timezones "SD" "America/Chicago" "America/Denver")
(set-US-state-timezones "TN" "America/Chicago" "America/New York")
(set-US-state-timezones "TX" "America/Chicago" "America/Denver")
(set-US-state-timezones "UT" "America/Denver")
(set-US-state-timezones "VT" "America/New York")
(set-US-state-timezones "VI" )
(set-US-state-timezones "VA" "America/New York")
(set-US-state-timezones "WA" "America/Los Angeles")
(set-US-state-timezones "WV" "America/New York")
(set-US-state-timezones "WI" "America/Chicago")
(set-US-state-timezones "WY" "America/Denver")


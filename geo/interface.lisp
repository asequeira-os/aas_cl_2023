(in-package :geo)

(defvar *country-timezones* (make-hash-table :test #'equal))

(defun get-country-timezone-names (cc)
  (or (gethash cc *country-timezones*)
      (error "unsupported country ~A" cc)))

(defun set-country-zones (cc zones)
  (setf (gethash cc *country-timezones*) zones))

(set-country-zones "US" (geo-US:get-US-timezones))
(set-country-zones "CA"
                   (list "Canada/Pacific Standard Time"
                         "Canada/Mountain Standard Time (No DST)"
                         "Canada/Mountain Standard/Daylight Time"
                         "Canada/Central Standard Time (No DST)"
                         "Canada/Central Standard/Daylight Time"
                         "Canada/Eastern Standard Time (No DST)"
                         "Canada/Eastern Standard/Daylight Time"
                         "Canada/Atlantic Standard Time (No DST)"
                         "Canada/Atlantic Standard/Daylight Time"
                         "Canada/Newfoundland Standard/Daylight Time"
                         ))

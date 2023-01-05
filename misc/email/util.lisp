(in-package :email)

(defun validate (email-address)
  (and (stringp email-address)
       (< 5 (length email-address))
       (find #\@ email-address)
       t))

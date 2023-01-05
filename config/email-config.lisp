;;email sending (smtp) configuration
(in-package :email)

;;this should be some localhost in production?
(setf *smtp-host* "smtp.aol.com")
;;this should be some support email in production
(setf *envelop-sender-email* "zzz@zzz.com")
;;next line is for testing only
(setf *smtp-to-test-address* "qqqqqq@qqqqqqq.com")
;;set this to nil to enable actual sending
(setf *smtp-do-not-send* t)

(setf *smtp-ssl-options* :starttls)
;;this should not be needed if I have a local smtp server setup?
(setf *smtp-auth* '(:login "zzzzzz" "uuuuuuu"))




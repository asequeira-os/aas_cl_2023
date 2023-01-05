(in-package :email)

;;following worked
;; (cl-smtp:send-email "smtp.aol.com" "ouqt555@aim.com" "asequeir@yahoo.com" "test smpt lib" "hello message" :ssl :starttls :authentication '(:login "ouqt555" "******"))

(defvar *smtp-host* )
(defvar *envelop-sender-email* )
(defvar *smtp-ssl-options* )
(defvar *smtp-auth* )
(defvar *smtp-to-test-address* )

(defvar *smtp-do-not-send* t)
(defvar *smtp-log-emails* t)

(defun load-config ()
  (setf *smtp-host* nil
        *envelop-sender-email* nil
        *smtp-ssl-options* nil
        *smtp-auth* nil)
  (or (and (aas-build::load-config "email-config" t)
           *smtp-host*  *envelop-sender-email*)
      (error "bad email-config")))

(defun send-email (to subject message html-message
                   &optional (from *envelop-sender-email*))
  (when *smtp-to-test-address*
    (setf to *smtp-to-test-address*))
  (let ((fn (lambda ()
              (log:with-logged-error
                (cl-smtp:send-email *smtp-host* from to subject message
                                    :ssl *smtp-ssl-options*
                                    :authentication *smtp-auth*
                                    :html-message html-message)))))
    (when *smtp-log-emails*
      (log:log-info 1 "EMAIL: from: ~A to: ~A subject: ~A body: ~A"
                from to subject message))
    (if *smtp-do-not-send*
        (log:log-info 2 "EMAIL: not actually sending email")
        (mp-make-thread "sendemail" fn))))

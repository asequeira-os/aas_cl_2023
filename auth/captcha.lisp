(in-package :auth)

(def-rpc-struct recaptcha
    (challenge  nil :type string)
  (response  nil :type string))

(def-rpc-struct captcha-data
    (vendor nil :type string)
  (recaptcha nil :type (or null recaptcha)))

(cloud:define-error 40103 fail-captcha)

(defmethod captcha-verify :around (data)
  (multiple-value-bind (success error)
      (call-next-method data)
    (if success
        t
        (cloud:raise-error 'fail-captcha error))))

(defmethod captcha-verify ((captcha-data captcha-data))
  (unless captcha-data
    (error "captcha-data missing"))
  (let ((vendor (captcha-data-vendor captcha-data)))
    (with-hashed-identity (:test #'equal)
      (case vendor
        ("recaptcha" (captcha-verify (captcha-data-recaptcha captcha-data)))
        ("test" (values (or (util:development-p)
                            (util:test-p))
                        "test not allowed"))
        (t (error "captcha vendor ~A not supported" vendor))))))

(defmethod captcha-verify ((captcha null))
  (error "captcha-data missing"))

;;recaptcha implementation follows
(defmethod captcha-verify ((recaptcha recaptcha))
  (recaptcha-verify (recaptcha-challenge recaptcha)
                    (recaptcha-response recaptcha)
                    aas-rpc:*client-ip*))

(define-constant +recaptcha-verify-url+ "http://www.google.com/recaptcha/api/verify")

(defvar *recaptcha-private-key*)
(defvar *recaptcha-public-key*)

(defun recaptcha-verify (challenge-field response-field remote-ip
                         &optional (private-key *recaptcha-private-key*))
  "return true if success. second values shows error from recpatcha on failure"
  (let ((response
         (split-sequence:split-sequence
          #\Newline
          (drakma:http-request +recaptcha-verify-url+
                               :method :post
                               :parameters `(("privatekey" . ,private-key)
                                             ("remoteip" . ,remote-ip)
                                             ("challenge" . ,challenge-field)
                                             ("response" . ,response-field))))))
    (values (string= (first response) "true")
            (second response))))

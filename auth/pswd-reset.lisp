(in-package :auth)

(def-db-table pswd-reset
    "temp tokens for password reset"
  +auth+
  ((userid user-id)
   (rsttkn string)
   (created aas-local-time:dto)
   (orig-login string))
  (((userid id db)) ((userid id id)))
  nil)

(def-rpc string reset-password-email
    (:post-only t :anonymous t :application +auth+)
    (login string)
  (if (cloud:is-my-key login)
      (reset-password-email-internal :login login :orig-login nil)
      (call-remote-anon (cloud:get-key-host login)
                        #'reset-password-email
                        login)))

(def-rpc string reset-password-email-internal
    (:post-only t :host-to-host-only t :anonymous t :application +auth+)
    (login string orig-login string)
  (if orig-login
      (cloud:with-db-for-key (*db* orig-login)
        (let ((row (db-user_base-select-login login)))
          (unless row
            (error "user ~A not found" login))
          (let ((i18n:*locale* (db-table-user-base-row-lc row)))
            (reset-password-generate-token
             login orig-login (db-table-user-base-row-userid row)))))
      (cloud:with-db-for-key (*db* login)
        (let ((pointer (db-user_pointer-select-pk login)))
          (if pointer
              (let ((orig-login
                     (db-table-user-pointer-row-orig-login pointer)))
                (call-remote-anon (cloud:get-key-host orig-login)
                                  #'reset-password-email-internal
                                  login orig-login))
              (reset-password-email-internal :login login :orig-login login))))))

(defvar *password-reset-request-min-gap*
  (aas-local-time:make-duration 0 10 0))

(cloud:define-error 40104 fail-password-reset-min-gap)


(defun reset-password-generate-token (login orig-login userid)
  (let ((old-row (db-pswd_reset-select-pk (user-db-num userid) (user-id-num userid))))
    (when old-row
      (let* ((ts (db-table-pswd-reset-row-created old-row))
             (age (aas-local-time:date-difference (aas-local-time:utc-now) ts)))
        (if (aas-local-time:duration< age *password-reset-request-min-gap*)
            (cloud:raise-error 'fail-password-reset-min-gap)
            (db-base:db-delete-row old-row)))))
  (let ((rsttkn (write-to-string (aas-cl:aas-random 1000000000) :base 26)))
    (db-insert-row
     (make-db-table-pswd-reset-row userid rsttkn
                                   (aas-local-time:utc-now) orig-login))
    (let ((message (format nil
                           (i18n:get-text 'reset-password-email-body)
                           rsttkn)))
      (email:send-email login (i18n:get-text 'reset-password-email-subject)
                        message message))
    "OK"))

(def-rpc string reset-password
    (:post-only t :anonymous t :application +auth+)
    (login string reset-token string new-password string)
  (if (cloud:is-my-key login)
      (reset-password-internal :login login :orig-login nil
                               :reset-token reset-token
                               :new-password new-password)
      (call-remote-anon (cloud:get-key-host login)
                        #'reset-password
                        login reset-token new-password)))

(def-rpc string reset-password-internal
    (:post-only t :host-to-host-only t :anonymous t :application +auth+)
    (login string orig-login string reset-token string new-password string)
  (if orig-login
      (cloud:with-db-for-key (*db* orig-login)
        (let ((row (db-user_base-select-login login)))
          (unless row
            (error "user ~A not found" login))
          (let ((userid (db-table-user-base-row-userid row)))
            (let ((tknrow (db-pswd_reset-select-pk
                           (user-db-num userid) (user-id-num userid))))
              (unless tknrow
                (error "reset information not found"))
              (let ((dbtkn (db-table-pswd-reset-row-rsttkn tknrow)))
                (unless (string= dbtkn reset-token)
                  (error "reset code mismatch"))
                (change-password-impl userid new-password)
                "OK")))))
      (cloud:with-db-for-key (*db* login)
        (let ((pointer (db-user_pointer-select-pk login)))
          (if pointer
              (let ((orig-login
                     (db-table-user-pointer-row-orig-login pointer)))
                (call-remote-anon (cloud:get-key-host orig-login)
                                  #'reset-password-internal
                                  login orig-login reset-token new-password))
              (reset-password-internal :login login :orig-login login
                                       :reset-token reset-token :new-password new-password))))))


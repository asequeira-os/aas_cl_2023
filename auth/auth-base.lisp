(in-package :auth)

(def-db-table user-base
    "user login table (main)"
  +auth+
  ((userid user-id)
   (guid string)
   (login string)
   (orig-login (or null string))
   (ext-db-key integer)
   (created aas-local-time:dto)
   (lc i18n:locale)
   (tz aas-local-time:tz)
   (realm enum-user-realm)
   (active boolean)
   (id-confirmed boolean)
   (deleted boolean))
  (((userid id db) :ascending)((userid id id) :ascending))
  ((:unique login (login)))
  (:vdb-sequence (userid id db) (userid id id)))

(def-db-table user-pointer
    "user in another db"
  +auth+
  ((login string) (orig-login string) (deleted boolean) (userid user-id))
  (login)
  ((:unique byuserid ((userid id db)) ((userid id id)))))

(def-db-table user-password
    "user password table"
  +auth+
  ((userid user-id) (salt string) (hash string))
  (((userid id db) :ascending) ((userid id id) :ascending))
  nil)

(def-db-table user-status
    "user status situation entries when active is false in user-base table"
  +auth+
  ((userid user-id) (status symbol))
  (((userid id db)) ((userid id id)) status)
  nil)

(def-db-table user-login-guid
    "guid user logins"
  +auth+
  ((userid user-id)
   (guid string)
   (created time:dto))
  (((userid id db)) ((userid id id)))
  nil)

(defun make-password-hash-salt ()
  (cipher:encryption-key-key (cipher:make-new-key)))

(log::register-field 'auth-user
                     (lambda ()
                       (if *auth-user*
                           (db-table-user-base-row-login *auth-user*)
                           "")))
(cloud:define-error 40100 id-in-use)
(cloud:define-error 40101 invalid-login-id)
(cloud:define-error 40102 fail-password-rules)

(def-rpc-with-proxy user-id create-password-user-login
    (:post-only t :anonymous t :application +auth+)
    (login string password string locale string captcha captcha-data tz tz)
    (key-to-vdb (util:trim login))
  (i18n:with-locale locale
    (captcha-verify captcha)
    (unless (static-validate-password password)
      (cloud:raise-error 'fail-password-rules ))
    (create-password/guid-user-login-impl login password nil tz)))

;;todo 1 need to check auth is a paying user
(def-rpc-with-proxy user-id create-guid-user-login
    (:post-only t :application +auth+)
    (login string)
    (key-to-vdb (util:trim login))
  (create-password/guid-user-login-impl login nil t  time:*timezone*)
  )

(defun create-password/guid-user-login-impl (login password guid-p tz)
  (assert (aas-cl:xor password guid-p)  nil
          "have to supply either password or guid-p")
  (let ((login (util:trim login)))
    (unless (cloud:is-my-key login)
      (error "wrong host for ~A" login))
    (unless (or guid-p (static-validate-login login))
      (cloud:raise-error 'invalid-login-id login))
    (cloud:with-db-for-key (*db* login)
      (db-with-transaction ('create-password-user-login-impl)
        (when (or (db-user_pointer-select-pk login)
                  (db-user_base-select-login login))
          (cloud:raise-error 'id-in-use login))
        (let* ((vdb (key-to-vdb login))
               (user-base-row
                (db-insert-row
                 (make-db-table-user-base-row
                  (make-user-id :id (make-db-id :db vdb))
                  (aas-cl:guid-string)
                  login
                  nil
                  (cloud:ext-db-key login)
                  (aas-local-time:utc-now)
                  i18n:*locale* tz
                  (if guid-p +enum-user-realm-no-password+ +enum-user-realm-password+ )
                  guid-p ;; guid user is active on creation
                  nil nil))))
          (when user-base-row
            (let ((userid (db-table-user-base-row-userid user-base-row))
                  (salt (make-password-hash-salt)))
              (if guid-p
                  (db-insert-row (make-db-table-user-login-guid-row
                                  userid (aas-cl:guid-string) (time:utc-now)))
                  (db-insert-row (make-db-table-user-password-row
                                  userid salt (cipher:digest password salt))))
              (db-insert-row (make-db-table-user-status-row userid 'email-unconfirmed))
              (unless guid-p
                (send-confirmation-link user-base-row))
              userid)))))))

(notify:define-notifier  user-delete-notification)

(def-rpc-with-proxy string delete-user-pointer
    (:post-only t :host-to-host-only t :anonymous t :application +auth+)
    (login string)
    (key-to-vdb login)
  (db-with-transaction ('delete-user-pointer)
    (let ((row (db-user_pointer-select-pk login)))
      (if row
          (db-base:db-delete-row row)
          (error "user pointer entry not found"))
      "OK")))

(def-rpc-with-proxy string insert-user-pointer
    (:post-only t :host-to-host-only t :application +auth+)
    (login string orig-login string)
    (key-to-vdb login)
  (or orig-login
      (error "user pointer entry needs orig-login"))
  (db-with-transaction ('insert-user-pointer)
    (when (or (db-user_pointer-select-pk login)
              (db-user_base-select-login login))
      (cloud:raise-error 'id-in-use login))
    (db-base:db-insert-row (make-db-table-user-pointer-row login orig-login nil (auth-token-user-id))))
  "OK")

(def-rpc string verify-auth (:post-only t :application +auth+)
    ()
  "does nothing, but verifies auth token as side effect"
  "OK")

(def-rpc-with-proxy string change-password (:post-only t :application +auth+)
    (login string password string new-password string)
    (auth-token-db-num)
  (or (cloud:is-my-vdb (auth-token-d *auth-token*))
      (error "bug not my db for authenticated user?"))
  (unless (static-validate-password new-password)
    (cloud:raise-error 'fail-password-rules ))
  (unless (password-user-login :login login :password password)
    (error "password invalid"))
  (change-password-impl (auth-token-user-id) new-password)
  "OK")

(defun change-password-impl (userid new-password)
  (db-with-transaction ('change-password)
    (let ((user-base (db-user_base-select-pk (user-db-num userid) (user-id-num userid)))
          (row (db-user_password-select-pk (user-db-num userid) (user-id-num userid))))
      (unless user-base
        (error "bug - no user-base record"))
      (let ((realm (db-table-user-base-row-realm user-base)))
        (unless (or row (eq realm +enum-user-realm-no-password+))
          (error "bug - password row not found"))
        (when row
          (db-base:db-delete-row row))
        (when (eq realm +enum-user-realm-no-password+)
          (setf (db-table-user-base-row-realm user-base) +enum-user-realm-password+)
          (db-update-row user-base))))
    (let ((salt (make-password-hash-salt)))
      (db-insert-row (make-db-table-user-password-row
                      userid salt (cipher:digest new-password salt))))))

(defun verify-my-user ()
  (or (cloud:is-my-vdb (auth-token-d *auth-token*))
      (error "bug not my db for authenticated user?")))

(def-rpc-with-proxy string change-login (:post-only t :application +auth+)
    (login string new-login string password string)
    (auth-token-db-num)
  (verify-my-user)
  (or (string-equal (db-table-user-base-row-login *auth-user*)
                    login)
      (error "login does not match"))
  (when (equal new-login login)
    (error "invalid change-login request old:~A new:~A"
           login new-login))
  (unless (password-user-login :login login :password password)
    (error "password invalid"))
  (change-login-impl login new-login nil))

(defun change-login-impl (login new-login delete)
  (unless (eq (db-table-user-base-row-realm *auth-user*)
              +enum-user-realm-password+)
    (error "change login not allowed for password less user"))
  (let ((orig-login (or (db-table-user-base-row-orig-login *auth-user*)
                        login)))
    ;;delete old user pointer if needed
    (unless (or (null orig-login) (string-equal orig-login login))
      (delete-user-pointer :login login))
    ;;insert new user pointer if needed
    (unless (string-equal orig-login new-login)
      (insert-user-pointer :login new-login :orig-login orig-login))
    ;;delete and recreate user base with new login
    (db-with-transaction ('change-login)
      (setf (db-table-user-base-row-orig-login *auth-user*)
            orig-login)
      (setf (db-table-user-base-row-login *auth-user*) new-login)
      (setf (db-table-user-base-row-active *auth-user*) nil)
      (setf (db-table-user-base-row-guid *auth-user*)
            (aas-cl:guid-string))
      (db-insert-row (make-db-table-user-status-row
                      (db-table-user-base-row-userid *auth-user*)
                      (if delete 'deleted 'email-unconfirmed)))
      (db-base:db-update-row *auth-user*)
      (unless delete
        (send-confirmation-link *auth-user*)))
    "OK"))

(defun send-confirmation-link (user-base-row)
  (let ((login (db-table-user-base-row-login user-base-row))
        (guid (db-table-user-base-row-guid user-base-row)))
    (let ((message (format nil
                           (i18n:get-text 'confirm-email-body)
                           (aas-http-client:filled-http-link "https"
                                                             www-ui-config:*host*
                                                             www-ui-config:*email-confirm-path*
                                                             login guid))))
      (email:send-email login (i18n:get-text 'confirm-email-subject)
                        message message))))

(def-rpc-with-proxy string auth-user-guid-for-test
    (:anonymous t :application +auth+)
    (user-id user-id)
    (db-id-db (user-id-id user-id))
  (unless (or (util:test-p) (util:development-p))
    (error "test only api. security risk"))
  (let ((row (db-user_base-select-pk *vdb* (user-id-num user-id))))
    (unless row
      (error "no such user ~A" user-id))
    (if (eq +enum-user-realm-password+ (db-table-user-base-row-realm row))
        (db-table-user-base-row-guid row)
        (let ((guid-row (db-user_login_guid-select-pk
                         (user-db-num user-id) (user-id-num user-id))))
          (unless guid-row
            (error "bug no guid row"))
          (db-table-user-login-guid-row-guid guid-row)))))


(def-rpc string confirm-user-email
    (:post-only t :anonymous t :application +auth+)
    (login string guid string)
  (unless (and login guid)
    (error "missing input"))
  (confirm-user-email-internal :login login :guid guid))

(def-rpc-with-proxy string confirm-user-email-internal
    (:post-only t :host-to-host-only t :anonymous t :application +auth+)
    (login string orig-login (or null string) guid string)
    (key-to-vdb (or orig-login login))
  (let ((user-pointer (db-user_pointer-select-pk login)))
    (let* ((orig-login
            (or orig-login
                (and user-pointer
                     (db-table-user-pointer-row-orig-login user-pointer)))))
      (if (cloud:is-my-key (or orig-login login))
          (cloud:with-db-for-key (*db* (or  orig-login login))
            (db-with-transaction ('confirm-user-email-internal)
              (let ((user-base (db-user_base-select-login login)))
                (unless user-base
                  (error "user not found"))
                (unless (equal guid
                               (db-table-user-base-row-guid user-base))
                  (error "invalid guid"))
                (let* ((userid (db-table-user-base-row-userid user-base))
                       (user-status (db-user_status-select-pk
                                     (user-db-num userid) (user-id-num userid)
                                     'email-unconfirmed)))
                  (unless user-status
                    (error "user status missing"))
                  (db-base:db-delete-row user-status))
                (setf (db-table-user-base-row-guid user-base)
                      (aas-cl:guid-string))
                (setf (db-table-user-base-row-active user-base) t)
                (setf (db-table-user-base-row-id-confirmed user-base) t)
                (db-base:db-update-row user-base)
                "OK")))
          (confirm-user-email-internal
           :login login :orig-login orig-login :guid guid)
          ))))

(def-rpc-with-proxy string delete-login
    (:allow-inactive t :post-only t :application +auth+)
    (login string)
    (auth-token-db-num)
  (or (string-equal (db-table-user-base-row-login *auth-user*)
                    login)
      (error "login does not match (we don't have admin facility yet)"))
  (change-login-impl login (aas-cl:guid-string) t))

(defun static-validate-login (login)
  (email:validate login))

(defun static-validate-password (password)
  (and password
       (stringp password)
       (< 5 (length password))
       t))

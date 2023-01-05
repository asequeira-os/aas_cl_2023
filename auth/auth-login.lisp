(in-package :auth)

(defparameter *user-login-token-ttl-seconds* (* 8 60 60))

(def-rpc-struct auth-token
    (u -1 :type integer) ;;userid
  (d -1 :type integer)   ;;auth db num
  (k -1 :type integer) ;;use this for db data distribution outside the auth app
  (g nil :type string) ;;user guid
  (t nil :type integer) ;;token ttl
  (l nil :type i18n:locale) ;;locale key
  (tz nil :type time:tz) ;;timezone
  (a nil :type boolean) ;;active
  (h nil :type string))

(setf aas-rpc::*auth-token-type* 'auth-token)

(defmethod cloud:get-auth-token-hash-key ((auth-token auth-token))
  (auth-token-k auth-token))

(defmethod cache:cache-key ((auth-token auth-token))
  (format nil "~A-~A"
          (auth-token-u auth-token) (auth-token-d auth-token)))

(defmethod cache:cache-ttl ((auth-token auth-token))
  (/ *user-login-token-ttl-seconds* 2))

(defun auth-token-user (&optional (token *auth-token*))
  "returns values db-num userid"
  (values (auth-token-d token) (auth-token-u token)))

(defun auth-token-db-num (&optional (token *auth-token*))
  (auth-token-d token))

(defun auth-token-ext-db-key (&optional (token *auth-token*))
  (auth-token-k token))

;;at login use the db num by calling get-db-num on the db used to verify the login
;;but do not store the number in the db user table [that way we can move data around easily]

(def-rpc-struct login-response
    (status "FAIL" :type string)
  (auth-host nil :type string)
  (login-token nil :type auth-token))


(def-rpc login-response password-user-login
    (:post-only nil :anonymous t :application +auth+)
    (login string password string)
  (password-user-login-internal :login login :password password
                                :orig-login nil :allow-inactive nil))

;;for use for the impatient paying user, so we don't loose them
(def-rpc login-response password-user-login-temp
    (:post-only nil :anonymous t :application +auth+)
    (login string password string)
  (password-user-login-internal :login login :password password
                                :orig-login nil :allow-inactive t))

(def-rpc login-response password-user-login-internal
    (:post-only t :host-to-host-only t :anonymous t :application +auth+)
    (login string password string
           orig-login (or null string) allow-inactive boolean)
  (if (null orig-login)
      (if (cloud:is-my-key login)
          (password-user-login-local login orig-login password allow-inactive)
          (let ((host (cloud:get-key-host login)))
            (aas-rpc:call-remote-trust host #'password-user-login-internal
                                       login password orig-login allow-inactive)))
      (if (cloud:is-my-key orig-login)
          (password-user-login-local login orig-login password allow-inactive)
          (error "call to wrong host login:~A orig-login:~A"
                 login orig-login))))

(define-error 40106 invalid-password)
(define-error 40107 invalid-login)

(defparameter *inactive-initial-grace* (time:make-duration 0 0 1)) ;; 1 hour

(defun password-user-login-local (login orig-login password allow-inactive)
  (cloud:with-db-for-key (*db* (or orig-login login))
    (let ((user-base (db-user_base-select-login login)))
      (when user-base
        ;;todo 1 need a transition startegy from guid to pswd type user
        (unless (eq +enum-user-realm-password+ (db-table-user-base-row-realm user-base))
          (error "not a password user")))
      (if user-base
          (password-user-login-exec user-base login password nil allow-inactive)
          (let ((user-pointer (db-user_pointer-select-pk login)))
            (if user-pointer
                (let* ((orig-login
                        (db-table-user-pointer-row-orig-login user-pointer))
                       (host (cloud:get-key-host orig-login)))
                  (aas-rpc:call-remote-trust host #'password-user-login-internal
                                             login password orig-login allow-inactive))
                (raise-error 'invalid-login login)))))))

(defun password-user-login-exec (user-base login password guid allow-inactive)
  (let* ((userid (db-table-user-base-row-userid user-base))
         (guid-p (eq (db-table-user-base-row-realm user-base)
                     +enum-user-realm-no-password+))
         (deleted (db-table-user-base-row-deleted user-base))
         (active (db-table-user-base-row-active user-base)))
    (unless active
      (if allow-inactive
          (let* ((created (db-table-user-base-row-created user-base))
                 (age (time:date-difference time:*utc-now* created)))
            (when (time:duration< *inactive-initial-grace* age)
              (error "inactive user is not allowed to login")))
          (error "inactive user is not allowed to login")))
    (when deleted
      (error "deleted user is not allowed to login"))
    (if guid-p
        (let ((guid-row (db-user_login_guid-select-pk
                         (user-db-num userid) (user-id-num userid))))
          (if (equal guid (db-table-user-login-guid-row-guid guid-row))
              (login-response user-base)
              (raise-error 'invalid-password login)))
        (let* ((user-password-row (db-user_password-select-pk
                                   (user-db-num userid) (user-id-num userid)))
               (salt (db-table-user-password-row-salt user-password-row))
               (hash (db-table-user-password-row-hash user-password-row))
               (hash-check (cipher:digest password salt)))
          (if (equal hash hash-check)
              (login-response user-base)
              (raise-error 'invalid-password login))))))

;;guid user can be having no pointer entry
(def-rpc-with-proxy login-response guid-user-login
    (:post-only t :anonymous t :application +auth+)
    (login string guid string)
    (key-to-vdb (util:trim login))
  (let ((user-base (db-user_base-select-login login)))
    (if user-base
        (password-user-login-exec user-base login nil guid nil)
        (raise-error 'invalid-login login))))

(defun login-response (user-base)
  (let* ((auth-host (host:my-hostname))
         (userid (db-table-user-base-row-userid user-base))
         (auth-token
          (make-auth-token :u (user-id-num userid)
                           :d (user-db-num userid)
                           :k (db-table-user-base-row-ext-db-key user-base)
                           :g (db-table-user-base-row-guid user-base)
                           :h auth-host
                           :l (db-table-user-base-row-lc user-base)
                           :tz (db-table-user-base-row-tz user-base)
                           :a (db-table-user-base-row-active user-base)
                           :t (compute-auth-token-ttl))))
    (make-login-response
     :status "OK" :auth-host auth-host :login-token auth-token)))

(defun compute-auth-token-ttl ()
  (+ *user-login-token-ttl-seconds*
     (get-universal-time)))

(cloud:define-error 40105 token-expired)

(defun auth-token-verify (auth-token)
  (cloud:with-application +auth+
    (unless auth-token
      (error "auth token is null"))
    (unless (equal (host:my-hostname) (auth-token-h auth-token))
      (error "token issued by ~A [my hostname is ~A]"
             (host:my-hostname) (auth-token-h auth-token)))
    (let ((db-num (auth-token-d auth-token)))
      (when (< (auth-token-t auth-token) (get-universal-time))
        (cloud:raise-error 'token-expired))
      (unless (>= db-num 0)
        (error "invalid auth token"))
      (unless (cloud:is-my-vdb db-num)
        (error "not my token"))
      (cloud::with-db-node (*db* (cloud:get-db-node db-num) )
        (let ((db-row (db-user_base-select-pk db-num (auth-token-u auth-token))))
          (unless db-row
            (error "invalid token"))
          (unless (eq (auth-token-a auth-token)
                      (db-table-user-base-row-active db-row))
            (cloud:raise-error 'token-expired))
          (unless (string-equal (db-table-user-base-row-guid db-row)
                                (auth-token-g auth-token))
            (error "invalid token"))
          (values db-row
                  (auth-user-db-key db-row)
                  (db-table-user-base-row-lc db-row)
                  (db-table-user-base-row-tz db-row)))))))

(defun auth-user-db-key (auth-user)
  (or (db-table-user-base-row-orig-login auth-user)
      (db-table-user-base-row-login auth-user)))

(defmethod aas-rpc:app-rpc-handler :around
    ((app (eql +auth+)) auth-token rpc-meta function args)
  (declare (ignorable rpc-meta function args))
  (cloud:with-application app
    (if auth-token
        ;;auth rpc
        (if (equal (host:my-hostname) (auth-token-h auth-token))
            ;;my token
            (multiple-value-bind
                  (*auth-user* auth-db-key i18n:*locale* time:*timezone*)
                (auth-token-verify auth-token)
              (cloud:with-db-for-key (*db* auth-db-key)
                (call-next-method)))
            ;;another host token
            (if (verify-auth-token-internal :auth-token auth-token)
                (let ((*auth-user* nil) ;;remote user, can't bind db row
                      (i18n:*locale* (auth-token-l auth-token))
                      (time:*timezone* (auth-token-tz auth-token)))
                  ;;can't bind any default db
                  (call-next-method))
                (error "auth token verification failed")))
        ;;anon rpc
        (call-next-method))))

(def-rpc-with-proxy boolean verify-auth-token-internal
    (:host-to-host-only t :anonymous t :application +auth+)
    (auth-token auth-token)
    (auth-token-db-num auth-token)
  (or (auth-token-verify auth-token)
      (error "failed auth token verification")))



(in-package :auth-test)

(defstruct test-user
  (orig-login nil :type string)
  (password nil :type string)
  (ext-key -1 :type integer)
  (user-id nil :type (or null auth:user-id))
  (confirmed nil :type boolean)
  (login nil :type string))


(defun login-test-user (test-user)
  (let ((confirmed (test-user-confirmed test-user)))
    (aas-rpc:call-remote-anon "localhost"
                              (if confirmed
                                  #'auth:password-user-login
                                  #'auth:password-user-login-temp)
                              (if confirmed
                                  (test-user-login test-user)
                                  (test-user-orig-login test-user))
                              (test-user-password test-user))))


(defvar *test-context* nil)
(defvar *test-logins* )
(defvar *test-confirmed-users* nil)
(defvar *test-unconfirmed-users* nil)
(defvar *test-count* nil)

;;todo 5 count var needs gensym
(defmacro test-with-users ((count) &body body)
  (let ((body-fn (gensym "body-fn")))
    `(flet ((,body-fn () ,@body))
       (or (util:test-p) (util:development-p)
           (error "test/dev mode not set"))
       (if (and *test-context* (<= ,count *test-count*))
           (,body-fn)
           (let ((*test-context* t)
                 (*test-count* ,count)
                 (*test-confirmed-users* (cloud:with-application auth:+auth+
                                           (test-confirm-users
                                            (test-create-users ,count))))
                 (*test-unconfirmed-users* (cloud:with-application auth:+auth+
                                             (test-create-users ,count)))
                 (*test-logins* (make-hash-table :test #'equal)))
             (unwind-protect
                  (,body-fn)
               (delete-test-users *test-confirmed-users* t)
               (delete-test-users *test-unconfirmed-users* nil)))))))

(defun test-users (&optional (confirmed t) )
  (unless *test-context*
    (error "test-for-all-users call valid only inside *test-context*"))
  (if confirmed
      *test-confirmed-users*
      *test-unconfirmed-users*))

(defmacro test-with-user (test-user &body body)
  (let ((tu (gensym "test-user"))
        (login (gensym "login")))
    `(let* ((,tu ,test-user)
            (,login (test-login-for-user ,tu)))
       (let* ((*auth-token* (login-response-login-token ,login))
              (time:*timezone* (auth-token-tz *auth-token*))
              (aas-local-time:*utc-now* (aas-local-time:utc-now))
              (i18n:*locale* (auth-token-l *auth-token*)))
         ,@body))))

(defun test-get-user (user-number &optional (confirmed t))
  (nth user-number (if confirmed
                       *test-confirmed-users*
                       *test-unconfirmed-users*)))

(defun test-login-for-user (test-user)
  (or (util:test-p) (util:development-p)
      (error "test/dev mode not set"))
  (let* ((id (test-user-user-id test-user))
         (login-resp (when id (gethash id *test-logins*))))
    (if login-resp
        login-resp
        (progn
          (let* ((login-resp (login-test-user test-user))
                 (token (login-response-login-token login-resp))
                 (user-id (get-auth-user-id token)))
            (setf (gethash user-id *test-logins*) login-resp))))))

(defvar *test-user-counter* 1)

(defun create-test-data (count)
  (let ((list (list)))
    (loop for i from 1 to count do
         (let ((base (get-universal-time)))
           (let ((un (format nil "u~A-~A-~A@test.local.mmm"
                             base (incf *test-user-counter*) i))
                 (up (format nil "pswd~A%$^" base))
                 (unn (format nil "u~A-~A~A-2@test.local.mmm"
                              base (incf *test-user-counter*) i)))
             (push (make-test-user :orig-login un
                                   :password up
                                   :ext-key (cloud:ext-db-key un)
                                   :login unn) list))))
    list))

(defun test-create-users (count)
  (let ((users (create-test-data count)))
    (dolist (user users)
      (setf (test-user-user-id user) (create-test-user user)))
    users))

(defun test-confirm-users (users)
  (dolist (user users)
    (let ((login1 (test-user-orig-login user))
          (password (test-user-password user))
          (login2 (test-user-login user)))
      (let ((guid (aas-rpc:call-remote-anon "localhost"
                                            #'auth:auth-user-guid-for-test
                                            (test-user-user-id user))))
        (auth:confirm-user-email :login login1 :guid guid)
        (let ((resp (auth:password-user-login :login login1 :password password)))
          (aas-rpc:call-remote-impersonate
           "localhost" #'auth:change-login (login-response-login-token resp)
           login1 login2 password)
          (let ((guid2 (aas-rpc:call-remote-anon
                        "localhost" #'auth:auth-user-guid-for-test
                        (test-user-user-id user))))
            (auth:confirm-user-email :login login2 :guid guid2)
            (setf (test-user-confirmed user) t))))))
  users)

(defun create-test-user (user)
  (aas-rpc:call-remote-anon "localhost" #'auth:create-password-user-login
                            (test-user-orig-login user)
                            (test-user-password user)
                            "en-US"
                            (make-captcha-data  :vendor "test")
                            (aas-local-time:get-timezone "US/Pacific Standard Time")))

(defun delete-test-users (users confirmed)
  (dolist (user users)
    (let ((login (if confirmed
                     (test-user-login user)
                     (test-user-orig-login user)))
          )
      (let ((resp   (test-login-for-user user) ))
        (aas-rpc:call-remote-impersonate
         "localhost" #'delete-login
         (login-response-login-token resp) login)))))


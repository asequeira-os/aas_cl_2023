(in-package :auth)

(defun auth-token-user-id ()
  (multiple-value-bind (user-db-num userid)
      (auth:auth-token-user)
    (make-user-id :id (make-db-id :db user-db-num :id userid))))

;;does not always provide orig-login
(def-rpc-with-proxy user find-user
    (:post-only t :host-to-host-only t :anonymous t :application +auth+)
    (login string orig-login (or null string))
    (key-to-vdb (or orig-login login))
  (if (null orig-login)
      (if (cloud:is-my-key login)
          (find-user-local login orig-login)
          (let ((host (cloud:get-key-host login)))
            (aas-rpc:call-remote-trust host #'find-user login orig-login )))
      (if (cloud:is-my-key orig-login)
          (find-user-local login orig-login)
          (error "call to wrong host login:~A orig-login:~A"
                 login orig-login))))

(defun find-user-local (login orig-login)
  (cloud:with-db-for-key (*db* (or orig-login login))
    (let ((user-base (db-user_base-select-login login)))
      (if user-base
          (user-from-user-base-row user-base)
          (let ((user-pointer (db-user_pointer-select-pk login)))
            (if user-pointer
                (let* ((orig-login
                        (db-table-user-pointer-row-orig-login user-pointer))
                       (host (cloud:get-key-host orig-login)))
                  (aas-rpc:call-remote-trust host #'find-user login orig-login))
                (raise-error 'invalid-login login)))))))

(defun user-from-user-base-row (user-base)
  (let* ((user-id (db-table-user-base-row-userid user-base))
         (deleted (db-table-user-base-row-deleted user-base))
         (orig-login (db-table-user-base-row-orig-login user-base))
         (active (db-table-user-base-row-active user-base)))
    (unless active
      (error "inactive user "))
    (when deleted
      (error "deleted user "))
    (make-user :orig-login orig-login
               :userid user-id
               :ext-key (db-table-user-base-row-ext-db-key user-base)
               :login (db-table-user-base-row-login user-base))))

(def-rpc (vector user)  get-user
    (:post-only t :host-to-host-only t :application +auth+)
    (list (vector user-id))
  (let ((grouped-list (aas-misc:group-by list
                                         :key #'user-db-num
                                         :test #'equal)))
    ;;list elm is list, car is db num, cdr is list of stub2
    (coerce (flatten (mapcar #'get-user-impl grouped-list)) 'vector)))

(defun get-user-impl (list)
  (let ((db-num (car list))
        (list (cdr list)))
    (coerce (get-user-local :db-num db-num :list (coerce list 'vector))
            'list)))

(def-rpc-with-proxy (vector user) get-user-local
    (:host-to-host-only t :application +auth+)
    (db-num integer list (vector user-id))
    db-num
  (coerce (mapcar (lambda (user-id)
                    (assert (= (user-db-num user-id) db-num))
                    (let ((row (db-user_base-select-pk db-num (user-id-num user-id) )))
                      (user-from-user-base-row row))) (coerce list 'list))
          'vector))

(defun verify-self (userid)
  (unless (equalp (auth-token-user-id) userid)
    (raise-error 'acl-perm-denied)))

(defun get-auth-user-id (&optional (token *auth-token*))
  (multiple-value-bind (auth-db-num userid)
      (auth:auth-token-user token)
    (make-user-id :id (make-db-id :db auth-db-num :id userid))))


(defun auth-to-ext-vdb (&optional (token *auth-token*) (app cloud:*application*))
  (assert (not (eq app +auth+)) nil "why looking for vdb for auth app?")
  (key-to-vdb (auth-token-ext-db-key token) app))

(defun user-to-ext-vdb (user &optional (app cloud:*application*))
  (assert (not (eq app +auth+)) nil "why looking for vdb for auth app?")
  (key-to-vdb (user-ext-key user)))
(in-package :auth-test)

(defparameter *test-users* nil)

(defun get-test-users (cached)
  (unless (and cached *test-users*)
    (create-test-users nil)
    (confirm-test-users nil))
  *test-users*)

(deftest all-tests
  (cloud:with-application +auth+
    (guid-users-test)
    (is (and (create-test-users t)
             (login-user-tests t )
             (delete-users-test))))
  (setf *test-users* nil))

(defun create-test-users (test)
  (let ((aseq-test:*test-dribble* nil))
    (setf *test-users* nil)
    (cloud:with-application +auth+
      (or (util:test-p) (util:development-p)
          (error "test/dev mode not set"))
      (setf *test-users* (create-test-data 3))
      (create-user-tests test)))
  *test-users*)

(defun confirm-test-users (test)
  (let ((aseq-test:*test-dribble* nil))
    (cloud:with-application +auth+
      (login-user-tests test))
    *test-users*))

(defun create-user-tests (test)
  (dolist (td *test-users*)
    (let ((userid (create-test-user td)))
      (setf (test-user-user-id td) userid)
      (is (>= (user-db-num userid) 0))
      (is (> (user-id-num userid) 0))))
  (when test
    (dolist (td *test-users*)
      (verify-error error (create-test-user td)))))

(defun login-inactive-user (test-user)
  (aas-rpc:call-remote-anon "localhost" #'password-user-login-temp
                            (test-user-orig-login test-user)
                            (test-user-password test-user)))


(defun login-user-tests (test)
  (let ((profile-non-exists-test 0))
    ;;users not yet activated
    (when test
      (dolist (td *test-users*)
        (verify-error error (password-user-login
                             :login (test-user-orig-login td) :password "fooobar"))
        (verify-error error (find-user
                             :login (test-user-orig-login td)))))
    (dolist (td *test-users*)
      (let ((login1 (test-user-orig-login td))
            (password (test-user-password td))
            (login2 (test-user-login td)))
        (when test
          (is (aas-rpc:call-remote-anon "localhost"
                                        #'password-user-login-temp
                                        login1 password))
          (verify-error error (password-user-login
                               :login login1 :password password)))
        (let ((guid (aas-rpc:call-remote-anon "localhost"
                                              #'auth-user-guid-for-test
                                              (test-user-user-id td))))
          (is guid) ;;make sure we are getting confirmation data
          (is (equal "OK"
                     (confirm-user-email
                      :login login1
                      :guid guid)))
          (check-user nil td (find-user :login login1))
          (let* ((resp (password-user-login
                        :login login1 :password password))
                 (db-num (auth-token-d
                          (login-response-login-token resp))))
            (is (typep resp 'login-response))
            (is (= (auth-token-k (login-response-login-token resp))
                   (test-user-ext-key td)))
            (aas-rpc:call-remote-impersonate "localhost" #'change-login
                                             (login-response-login-token resp)
                                             login1 login2 password)
            (when test
              (verify-error error (find-user :login login1))
              (verify-error error (password-user-login
                                   :login login1 :password password))
              (verify-error error (password-user-login
                                   :login login2 :password password)))
            (let ((guid2 (aas-rpc:call-remote-anon "localhost"
                                                   #'auth-user-guid-for-test
                                                   (test-user-user-id td))))
              (is guid2)
              (is-not (equal guid guid2))
              (is (equal "OK"
                         (confirm-user-email
                          :login login2
                          :guid guid2)))
              (check-user t td (find-user :login login2))
              (let*  ((resp (password-user-login
                             :login login2 :password password))
                      (db-num-2 (auth-token-d (login-response-login-token resp))))
                (is resp)
                (is (= db-num db-num-2))
                (when test
                  (user-profile-tests resp (= profile-non-exists-test 1)))
                (incf profile-non-exists-test)
                (is (= (auth-token-k (login-response-login-token resp))
                       (test-user-ext-key td))))
              (when test
                (test-change-password login2 password "aaaaaa")
                (test-change-password login2 "aaaaaa" password))))))))
  t)

(deftest delete-users-test
  (dolist (td *test-users*)
    (let ((login (test-user-login td))
          (password (test-user-password td)))
      (let ((resp (password-user-login
                   :login login :password password)))
        ;;(is (equal (db-table-user-base-row-orig-login resp) (test-user-orig-login td)))
        (aas-rpc:call-remote-impersonate "localhost" #'delete-login
                                         (login-response-login-token resp)
                                         login)
        (verify-error error (password-user-login
                             :login login :password password))))))

(defun test-change-password (login password new-password)
  (let  ((resp (password-user-login
                :login login :password password)))
    (is resp)
    (verify-error error
        (aas-rpc:call-remote-impersonate
         "localhost" #'change-password
         (login-response-login-token resp)
         login "fooo" "aaaaaa"))
    (is (aas-rpc:call-remote-impersonate
         "localhost" #'change-password
         (login-response-login-token resp)
         login password new-password))
    (verify-error error (password-user-login
                         :login login :password password))
    (is (password-user-login
         :login login :password new-password))))

(defun check-user (login-changed testuser user)
  (is testuser)
  (is user)
  (is (equal (test-user-ext-key testuser) (user-ext-key user)))
  (is-not (zerop (user-id-num user)))
  (let ((ol2 (user-orig-login user)))
    (if login-changed
        (progn
          (is (equal (test-user-login testuser ) (user-login user)))
          (is (equal (test-user-orig-login testuser ) ol2)))
        (progn
          (is (null (user-orig-login user)))
          (is (equal (test-user-orig-login testuser) (user-login user)))))))






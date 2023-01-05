(in-package :auth-test)

(defun user-profile-tests (login-resp test-non-exist)
  (let ((token
         (login-response-login-token login-resp)))
    (let ((user-profile (ctor-test-profile)))
      (when test-non-exist
        (verify-error error
            (aas-rpc:call-remote-impersonate
             "localhost" #'auth:update-user-profile token
             user-profile)))
      (let ((noprof (aas-rpc:call-remote-impersonate
                     "localhost" #'auth:get-user-profile token)))
        (is (null noprof)))
      (is (equalp user-profile
                  (aas-rpc:call-remote-impersonate
                   "localhost" #'auth:create-user-profile token
                   user-profile)))
      (when test-non-exist
        (verify-error error
            (aas-rpc:call-remote-impersonate
             "localhost" #'auth:create-user-profile token
             user-profile)))
      (setf (user-profile-second-name user-profile)
            (format nil "siek~A" (random 1024)))
      (is (equalp user-profile
                  (aas-rpc:call-remote-impersonate
                   "localhost" #'auth:update-user-profile token
                   user-profile)))
      (is (equalp user-profile
                  (aas-rpc:call-remote-impersonate
                   "localhost" #'auth:get-user-profile token))))))

(defun ctor-test-profile ()
  (let ((r (random 1024)))
    (make-user-profile :first-name (format nil "raonny~A" r)
                       :second-name nil
                       :last-name (format nil "balo~A" r))))



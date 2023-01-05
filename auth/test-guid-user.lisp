(in-package :auth-test)

(deftest guid-users-test
  (cloud:with-application auth:+auth+
    (auth-test:test-with-users (3)
      (let ((guid-test-users (create-test-data 2)))
        (verify-error error
            (aas-rpc:call-remote-anon "localhost" #'auth:create-guid-user-login
                                      (test-user-login (first guid-test-users))))
        (auth-test:test-with-user (auth-test:test-get-user 0)
          (dolist (guid-test-user guid-test-users)
            (let ((guid-userid (auth:create-guid-user-login
                                :login (test-user-login guid-test-user))))
              (is guid-userid)
              (let ((guid (auth:auth-user-guid-for-test :user-id guid-userid)))
                (verify-error error
                    (auth:guid-user-login :login (test-user-login guid-test-user)
                                          :guid "fooooo"))
                (let ((login-resp (auth:guid-user-login :login (test-user-login guid-test-user)
                                                        :guid guid)))
                  (is login-resp)
                  (is (equalp (aas-rpc:call-remote-impersonate
                               "localhost" #'auth:verify-auth
                               (login-response-login-token login-resp))
                              "OK")))))))))))

;;todo 2 not enough testing in guid user
;;above creates user and logs him in
;;expect more to be tested when used in xed entry creation

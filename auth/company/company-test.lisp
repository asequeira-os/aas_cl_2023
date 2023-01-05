(in-package :auth-test)

(deftest companies-test
  (test-with-users (3)
    (cloud:with-application auth:+auth+
      (company-test-1 (test-get-user 0) (test-get-user 1)))))

(defun company-test-1 (user member)
  (let* ((login-resp (auth-test:test-login-for-user user))
         (host (login-response-auth-host login-resp))
         (token (login-response-login-token login-resp)))
    (let ((company (call-remote-impersonate
                    host #'auth:company-create token
                    "my comp foo")))
      (is company)
      (let ((id (company-id company)))
        (is (< 0 (cloud:db-id-id id)))
        (is (<= 0 (cloud:db-id-db id))))
      (call-remote-impersonate
       host #'auth:company-add-user token
       company (test-user-user-id member))
      (test-with-user member
        (company-member-check :company company))
      (verify-error error
          (test-with-user user
            (company-member-check :company company))))))



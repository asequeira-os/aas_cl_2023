(defpackage :auth
  (:use :cl :aas-cl :db-base :aas-rpc :cloud :aas-local-time)
  (:import-from :aas-rpc *auth-user* *auth-token*))

(in-package :auth)
(export '(*user-login-token-ttl-seconds* +auth+
          auth-token-user
          auth-token-user-id
          auth-token-ext-db-key
          auth-to-ext-vdb
          user-to-ext-vdb
          auth-token-db-num

          ;;ext stuff
          user-id
          user-db-num
          user-id-num
          user

          ;;acl stuff
          obj-db-id
          right-object-type
          +admin-right+
          acl-user-check-right
          acl-subj-check-right
          acl-user-add-right
          acl-group-add-right
          acl-user-direct-groups
          acl-group-add-user
          ))

(defpackage :auth-test
  (:use :cl :auth :aseq-test)
  (:import-from :aas-rpc
                call-remote-impersonate call-remote-trust)
  (:import-from :auth
                *auth-token* make-captcha-data
                auth-token-tz auth-token-l auth-token-d auth-token-k
                user-ext-key user-orig-login user-login
                login-response login-response-auth-host login-response-login-token
                get-auth-user-id
                group-create group-rename
                make-group group-id
                company-id company-member-check
                make-user-profile user-profile-second-name

                ))

(in-package :auth-test)
(export '(
          test-get-user
          test-login-for-user
          test-users
          test-with-user
          test-with-users
          ))

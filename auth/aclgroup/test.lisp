(in-package :auth-test)

(defvar *user-groups* (make-hash-table :test #'equalp))

(defvar *u1* )
(defvar *u2* )
(defvar *u3* )
(defvar *prev-group* nil)

(deftest aclall
  (auth-test:test-with-users (3)
    (let ((*u1* (test-get-user 0))
          (*u2* (test-get-user 1))
          (*u3* (test-get-user 2)))
      (let* ((g1 (aclforuser *u1* *u2* *u3*))
             (g2 (aclforuser *u2* *u1* *u3*))
             (g3 (aclforuser *u3* *u2* *u1*))
             (g2b (aclforuser *u2* *u1* *u3*)))
        (is g2b)
        (acl-group-membership-test *u3* 3)
        (acl-group-membership-test *u2* 2)
        (acl-group-add-child-test *u1* g1 g2 g3)
        ;;(acl-group-add-child-test *u2* g2 g1 g3)
        ))))

(defun aclforuser (user ux uy)
  (let* ((login-resp (auth-test:test-login-for-user user))
         (host (login-response-auth-host login-resp)))
    (let ((group (make-user-group login-resp host )))
      (is group)
      (is (< 0 (cloud:db-id-id (group-id group))))
      (group-member-tests  group (login-response-login-token login-resp))
      (is (group-add-user-test-fn login-resp host group ux))
      (is (group-add-user-test-fn login-resp host group uy))
      (verify-error error (group-add-user-test-fn login-resp host group ux))
      (acluserslist login-resp host group ux uy)
      (let ((g2 (group-rename-test-fn login-resp host group)))
        (is g2)
        (is (equalp (group-id g2) (group-id group)))
        (setf (gethash user *user-groups*) g2)
        (when *prev-group*
          (verify-error error (group-rename-test-fn login-resp host *prev-group*))
          ;;gets acl-perm-denied error
          (verify-error error (group-add-user-test-fn login-resp host *prev-group* ux))
          )
        (setf *prev-group* g2)
        ;;  (break)
        group))))

(defun make-user-group (login-resp host )
  (let* ((name "group foo")
         (group (call-remote-impersonate
                 host #'group-create
                 (login-response-login-token login-resp)
                 name)))
    group))

(defun group-rename-test-fn (login-resp host  group)
  (call-remote-impersonate
   host #'group-rename
   (login-response-login-token login-resp)
   (make-group :id (group-id group) :name "group bar")))

(defun group-add-user-test-fn (login-resp host group u2)
  (call-remote-impersonate
   host #'auth:group-add-user
   (login-response-login-token login-resp)
   group (test-user-login u2)))

(defun group-add-right-test-fn (auth-token obj right group)
  (call-remote-impersonate
   "localhost" #'auth:group-add-right
   auth-token
   (right-object-type obj) (obj-db-id obj)
   group right))

(defun subj-check-right (auth-token sub right obj)
  (call-remote-impersonate
   "localhost" #'auth:acl-subj-check-right
   auth-token
   sub right obj ))

;;todo 1 no tests yet for subj to obj rights addition and tests
;;it requires the logged in user have admin right to obj

(defun group-member-tests (group auth-token)
  (let* ((typex "xTbgfrq")
         (typey "xTbHYTFq")
         ;;(right "myright1")
         (sub1 (cloud:make-db-id-type :type typex
                                      :dbid (cloud:make-db-id :db 9999991 :id 456678)))
         (sub2 (cloud:make-db-id-type :type typex
                                      :dbid (cloud:make-db-id :db 9999991 :id 456679)))
         (sub3 (cloud:make-db-id-type :type typey
                                      :dbid (cloud:make-db-id :db 9999991 :id 456678)))
         (sub4 (cloud:make-db-id-type :type typex
                                      :dbid (cloud:make-db-id :db 9999992 :id 456678))))
    (is (group-add-subject-test-fn auth-token group sub1))
    (is (group-add-subject-test-fn auth-token group sub2))
    (is (group-add-subject-test-fn auth-token group sub3))
    ;;(group-add-right-test-fn auth-token sub2 right group);;sub2 double duty as obj here
    (verify-error error (group-add-subject-test-fn auth-token group sub2))
    ;;(is (subj-check-right auth-token sub1 right sub2))
    (is (group-remove-subject-test-fn auth-token group sub1))
    (is (group-add-subject-test-fn auth-token group sub4))
    (let ((gl (call-remote-impersonate
               "localhost" #'auth:group-list-for-subj auth-token
               sub2)))
      (is (= 1 (length gl))))
    (is (group-remove-subject-test-fn auth-token group sub2))
    (let ((gl (call-remote-impersonate
               "localhost" #'auth:group-list-for-subj auth-token
               sub2)))
      (is (zerop (length gl))))
    (is (group-remove-subject-test-fn auth-token group sub3))
    (is (group-remove-subject-test-fn auth-token group sub4))))


(defun group-add-subject-test-fn (auth-token group sub)
  (call-remote-impersonate
   "localhost" #'auth:group-add-subject auth-token
   group sub))

(defun group-remove-subject-test-fn (auth-token group sub)
  (call-remote-impersonate
   "localhost" #'auth:group-remove-subject auth-token
   group sub))

(defun acluserslist (login-resp host group ux uy)
  (let ((ulist (call-remote-impersonate
                host #'auth:group-list-users
                (login-response-login-token login-resp) group)))
    (is (find (test-user-orig-login ux) ulist :test #'equal
              :key #'user-orig-login))
    (is (find (test-user-orig-login uy) ulist :test #'equal
              :key #'user-orig-login))
    (is (= 2 (length ulist)))
    (let ((urm (aref ulist 0)))
      (is (call-remote-impersonate
           host #'auth:group-remove-user
           (login-response-login-token login-resp) group urm))
      (is (= 1 (length (call-remote-impersonate
                        host #'auth:group-list-users
                        (login-response-login-token login-resp) group))))
      (call-remote-impersonate
       host #'auth:group-add-user
       (login-response-login-token login-resp)
       group (user-login urm)))))

(defun acl-group-add-child-test (user g1 g2 g3)
  (let* ((login-resp (auth-test:test-login-for-user user))
         (host "localhost"))
    (is (call-remote-impersonate
         host #'auth:group-add-child
         (login-response-login-token login-resp) g1 g2))
    (is (verify-parent-count g1 0))
    (is (verify-parent-count g2 1))
    (is (call-remote-impersonate
         host #'auth:group-add-child
         (login-response-login-token login-resp) g1 g3))
    (is (verify-parent-count g3 1))
    (verify-error error
        (call-remote-impersonate
         host #'auth:group-add-child
         (login-response-login-token login-resp) g2 g3))
    (is (verify-parent-count g2 1))
    (is (verify-parent-count g3 1))
    (is (call-remote-impersonate
         host #'auth:group-remove-child
         (login-response-login-token login-resp) g1 g3))
    (is (verify-parent-count g3 0))
    (is (call-remote-impersonate
         host #'auth:group-delete
         (login-response-login-token login-resp) g1))
    (verify-error error
        (call-remote-impersonate
         host #'auth:group-add-child
         (login-response-login-token login-resp) g1 g3))))

(defun verify-parent-count (group count)
  (cloud:with-application +auth+
    (call-remote-trust
     (if aseq-test:*multi-host*
         (error "todo 4 need multi host test fix hardcoded name")
         (host:my-hostname))
     #'auth:test-group-verify-parent-count group count)))


(defun acl-group-membership-test (user expected-count)
  (let* ((login-resp (auth-test:test-login-for-user user))
         (host "localhost"))
    (let ((glist (call-remote-impersonate
                  host #'auth:acl-user-effective-membership
                  (login-response-login-token login-resp))))
      ;;todo 2 acl-group-membership-test is weak
      (is (= expected-count (length glist))))))

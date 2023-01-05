(in-package :fin-cc-test)

(deftest all-tests
  (auth-test:test-with-users  (3)
    (and (basics)
         (cc-vendor-list-test)
         (profiles)
         (inactive-profile-test)
         (luhn-test)
         (authorize-net-tests))))


(defvar *test-cards* (list "370000000000002" "6011000000000012"
                           "4007000000027" "4012888818888"
                           "3088000000000017" "38000000000006"))

;;from
;;http://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm
(defvar *paypal-test-numbers*
  '("378282246310005" "371449635398431" "378734493671000" "5610591081018250"
    "30569309025904" "38520000023237" "6011111111111117" "6011000990139424"
    "3530111333300000" "3566002020360505" "5555555555554444" "5105105105105100"
    "4111111111111111" "4012888888881881" "4222222222222" "5019717010103742"
    "6331101999990016"))

(deftest basics
  (verify-error error
      (fin-cc:enable-authorize-net-prod nil nil))
  (verify-error error
      (enable-authorize-net-dev nil nil))
  (verify-error error
      (validate-cc-num "foo" "12345" 3 2011 "123"))
  (verify-error error
      (validate-cc-num *visa* "1x2345" 3 2011 "123"))
  (verify-error error
      (validate-cc-num *visa* "" 3 2011 "123"))
  (verify-error error
      (validate-cc-num *visa* "12345" 13 2011 "123"))
  (verify-error error
      (validate-cc-num *visa* "1x2345" 3 1999 "123"))
  t)

(deftest cc-vendor-list-test
  (let ((list (aas-rpc:call-remote-anon
               "localhost" #'cc-vendors-list)))
    (is (equal (length list) (hash-table-count *cc-vendors*)))
    (loop for i from 0 to (1- (length list)) do
         (let ((vendor (aref list i)))
           (is (not (null (cc-vendor-name vendor))))
           (is (gethash (cc-vendor-key vendor)
                        *cc-vendors*))))))

(deftest profiles
  (mapc #'profile-cc-user (auth-test:test-users t) *test-cards*))

(defun profile-cc-user (user cc-num)
  (is (luhn cc-num))
  (let* ((login-resp (auth-test:test-login-for-user user))
         (token (auth::login-response-login-token login-resp))
         (cc-count (+ 2 (random 2)))
         (in-vector (make-array cc-count)))
    (let ((empty (aas-rpc::call-remote-impersonate
                  "localhost" #'get-user-cc-list
                  token t)))
      (is empty)
      (is (zerop (length empty))))
    (loop for i from 1 to cc-count do
         (let* ((virgin-profile (make-test-cc-profile token cc-num))
                (resp (aas-rpc::call-remote-impersonate
                       "localhost" #'create-cc-user-profile
                       token
                       virgin-profile
                       "123" ;; ccv
                       ))
                (db-profile (cc-profile-resp-cc-profile resp))
                (profile-id (cc-profile-dbid db-profile)))
           (is (cc-profile-resp-accepted resp))
           (is (not (zerop (cloud:db-id-id profile-id))))
           (setf (cc-profile-dbid virgin-profile) profile-id)
           (is (not (util:empty-string-p (cc-profile-cc-num-enc db-profile))))
           (setf (cc-profile-cc-num-enc virgin-profile)
                 (cc-profile-cc-num-enc db-profile))
           (is (equalp virgin-profile db-profile))
           (setf (aref in-vector (1- i)) virgin-profile)))
    (let ((vector (aas-rpc::call-remote-impersonate
                   "localhost" #'get-user-cc-list
                   token t)))
      (is (= cc-count (length vector)))
      (setf vector (sort vector #'< :key (lambda (cc-profile)
                                           (cloud:db-id-id
                                            (cc-profile-dbid cc-profile)))))
      (is (equalp in-vector vector)))
    (let ((dp (aref in-vector 1)))
      (let ((resp (aas-rpc::call-remote-impersonate
                   "localhost" #'delete-cc-user-profile
                   token dp)))
        (is resp)
        (let ((all (aas-rpc::call-remote-impersonate
                    "localhost" #'get-user-cc-list
                    token nil))
              (active (aas-rpc::call-remote-impersonate
                       "localhost" #'get-user-cc-list
                       token t)))
          (is (equalp in-vector all))
          (is (= (length active) (1- (length in-vector)))))))
    (let ((up (aref in-vector 0)))
      (setf (cc-profile-first-name up) "updatedname")
      (let* ((resp (aas-rpc::call-remote-impersonate
                    "localhost" #'update-cc-user-profile
                    token up "123" ))
             (db-profile (cc-profile-resp-cc-profile resp)))
        (is (cc-profile-resp-accepted resp))
        ;; (is (> (cc-profile-id db-profile)
        ;;        (cc-profile-id up)))
        (setf (cc-profile-first-name up)
              (cc-profile-first-name up))
        (setf (cc-profile-dbid up) (cc-profile-dbid db-profile))
        (is (equalp up db-profile))))))




(defun random-string (prefix)
  (format nil "~A~A" prefix (random 1024)))

(defun make-test-cc-profile (token cc-num)
  (let ((cc-vdb (auth:auth-to-ext-vdb token +fin-cc+)))
    (make-cc-profile
     :dbid (cloud:make-db-id :db cc-vdb :id nil)
     :first-name (random-string "giveaname")
     :last-name (random-string "cosby")
     :address (random-string "roflane") :city "plano" :state "TX"
     :zip "75086" :country "US"
     :phone  (random-string "1245")
     :cc-vendor *visa*
     :cc-num cc-num
     :cc-exp-mm (1+ (random 10))
     :cc-exp-year (+ (1+ (aas-local-time::dto-year (aas-local-time:utc-now)))
                     (random 10)))))

(deftest inactive-profile-test
  (let* ((users (auth-test:test-users nil))
         (login-resp (auth-test:test-login-for-user (first users)))
         (token (login-response-login-token login-resp))
         (virgin-profile (make-test-cc-profile token (first *paypal-test-numbers*)))
         (resp (aas-rpc:call-remote-impersonate
                "localhost" #'create-cc-user-profile
                token virgin-profile "123")))
    (is resp)
    (is (equal "This transaction has been approved."
               (cc-profile-resp-message resp)))))


(deftest luhn-test
  (dolist (cc *paypal-test-numbers*)
    (is (luhn cc))
    (is-not (luhn (concatenate 'string "1" cc)))))



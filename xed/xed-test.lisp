(in-package :xed-test)

(deftest all-tests
  (cloud:with-application xed:+xeduler+
    (recur-tests)
    (auth-test:test-with-users (3)
      (let ((inactives (auth-test:test-users nil)))
        (run-inactive-tests inactives)
        (all-tests-user (auth-test:test-get-user 0) (auth-test:test-get-user 1)
                        (auth-test:test-get-user 2))))))

;;todo 1 no tests yet entry making in service and customer cal
(defun all-tests-user (test-admin-user test-u-serve test-u-cust)
  (auth-test:test-with-user test-admin-user
    (let ((u-serve (auth:find-user :login (auth-test::test-user-login test-u-serve)))
          (calendar
           (xed:create-calendar :calendar (make-calendar
                                           :id nil :name "fooo"
                                           :servicing t
                                           :tz (aas-local-time:get-timezone "US/Pacific Standard Time")))))
      (is calendar)
      (is (xed:add-service-user :calendar calendar :user u-serve))
      (is (= 1 (length (xed:list-service-users :calendar calendar))))
      (is-not (xed:serviced-cals-list))
      (verify-error error (auth:acl-user-check-right calendar +service+))
      (auth-test:test-with-user test-u-serve
        (is (auth:acl-user-check-right calendar +service+)))
      (shifts-crud-test calendar test-admin-user test-u-serve)
      (holidays-test calendar test-admin-user test-u-serve)
      (alarm-info-default calendar)
      (let ((login-resp (auth-test:test-login-for-user test-admin-user)))
        (verify-error error
            (aas-rpc:call-remote-impersonate
             "localhost" #'auth::get-user-host
             (login-response-login-token login-resp)))
        (let ((el (aas-rpc:call-remote-impersonate
                   "localhost" #'anon-app:get-enum-list
                   (login-response-login-token login-resp)
                   "xed:week-of-month")))
          (is (= 5 (length el))))
        (is (equal (host:my-hostname)
                   (aas-rpc:call-remote-impersonate
                    "localhost" #'xed::get-user-host
                    (login-response-login-token login-resp)))))
      (auth-test:test-with-user test-u-serve
        (let ((serviced-cals (xed:serviced-cals-list)))
          (is serviced-cals)
          (is (= 1 (length serviced-cals)))
          (is (equalp (aref serviced-cals 0) (calendar-id calendar)))))
      (is (xed:remove-service-user :calendar calendar :user u-serve))
      (is (zerop (length (xed:list-service-users :calendar calendar))))
      (let ((cust-cal (non-service-cal-tests calendar test-u-cust test-u-serve)))
        (is cust-cal)
        ;;todo 1 add customer and service calendar entry creation tests here
        )
      )))

(defun non-service-cal-tests (service-cal test-u-cust test-u-serve)
  (let* ((cust-login (test-user-login test-u-cust))
         (u-cust (auth:find-user :login cust-login))
         (tz (aas-local-time:get-timezone "US/Pacific Standard Time")))
    (auth-test:test-with-user test-u-serve
      (is (null (xed:get-user-calendar)))
      (verify-error error (xed:add-service-user :calendar service-cal :user u-cust))
      (let ((cust-cal (auth-test:test-with-user test-u-cust
                        (xed:create-calendar :calendar (make-calendar
                                                        :id nil :name "fooo"
                                                        :servicing nil
                                                        :tz tz)))))
        (verify-error error (xed:add-work-shift :cal-id (calendar-id cust-cal)
                                                :shift (create-shift 2 9 00 18 20)))
        (verify-error error (xed:upsert-holiday :cal-id (calendar-id cust-cal)
                                                :hd (time:create-date 2015 3 15)))
        (auth-test:test-with-user test-u-cust
          (alarm-info-default cust-cal)
          (let ((db-cal (xed:get-user-calendar))
                (db-cal-2 (xed:get-calendar-by-login :login cust-login)))
            (is db-cal)
            (is db-cal-2)
            (let ((db-id (calendar-id db-cal)))
              (is db-id)
              (is (cloud:db-id-id db-id))
              (setf (calendar-id cust-cal) db-id))
            (is (equalp cust-cal db-cal)))
          cust-cal ;;return value
          )))))

(defun shifts-crud-test (cal u-admin u-serve)
  (let ((shifts (list
                 (create-shift 0 8 30 16 20)
                 (create-shift 2 9 00 18 20)
                 (create-shift 0 18 30 0 0)))
        (overlap (create-shift 0 16 20 18 45)))
    (is (shifts-overlap (third shifts) overlap))
    (is (shifts-overlap overlap (third shifts)))
    (auth-test:test-with-user u-admin
      (verify-error error
          (xed:add-work-shift :cal-id (calendar-id cal)
                              :shift overlap)))
    (auth-test:test-with-user u-serve
      (dolist (shift shifts)
        (let ((rs (xed:add-work-shift :cal-id (calendar-id cal) :shift shift)))
          (is (not (zerop (cloud:db-id-id (work-shift-id rs)))))))
      (let ((rl (xed:list-work-shift :cal-id (calendar-id cal))))
        (is (= 3 (length rl)))
        (verify-error error
            (xed:add-work-shift :cal-id (calendar-id cal)
                                :shift overlap))
        (is (xed:delete-work-shift :cal-id (calendar-id cal) :shift (aref rl 1)))
        (verify-error error
            (xed:delete-work-shift :cal-id (calendar-id cal) :shift (aref rl 1)))))))

(defun create-shift (dow start-hour start-minute end-hour end-minute)
  (make-work-shift :id nil :dow dow
                   :start-hour start-hour :start-minute start-minute
                   :end-hour end-hour :end-minute end-minute))

;;this is really to test unconfirmed auth
;;but I want it to be done after some other app [such as xed] is in the picture

(aas-rpc:def-rpc string test-auth-inactive
    (:allow-inactive t :application aas-rpc-test::+test-app+)
    (r1 string)
  (string-upcase r1))

(defun run-inactive-tests (users)
  (let ((user (first users)))
    (let ((login-resp (auth-test:test-login-for-user user)))
      (let ((uc (aas-rpc:call-remote-impersonate
                 "localhost" #'test-auth-inactive
                 (login-response-login-token login-resp)
                 "foo23gh")))
        (is (equal uc (string-upcase "foo23gh"))))
      (verify-error error
          (aas-rpc:call-remote-impersonate
           "localhost" #'xed:create-calendar
           (login-response-login-token login-resp)
           (make-calendar
            :id nil :name "fooo"
            :tz (aas-local-time:get-timezone "US/Pacific Standard Time")))
        (let ((e aseq-test::e))
          (is (equal "inactive login not allowed"
                     (aas-rpc::json-rpc-2-error-message (aas-rpc::rpc-error-cause e)))))))))

(defun alarm-info-default (cal)
  (let* ((ad1 (aas-rpc:call-remote-impersonate
               "localhost" #'xed:set-alarm-info-default
               aas-rpc::*auth-token*
               cal (make-alarm-info :dur1 nil
                                    :dur2 nil)))
         (ad2 (aas-rpc:call-remote-impersonate
               "localhost" #'xed:set-alarm-info-default
               aas-rpc::*auth-token*
               cal (make-alarm-info :dur1 (time:make-duration 0 30 0)
                                    :dur2 nil)))
         (ad3 (aas-rpc:call-remote-impersonate
               "localhost" #'xed:set-alarm-info-default
               aas-rpc::*auth-token*
               cal (make-alarm-info :dur1 nil
                                    :dur2 (time:make-duration 0 30 0))))
         (ad4 (aas-rpc:call-remote-impersonate
               "localhost" #'xed:set-alarm-info-default
               aas-rpc::*auth-token*
               cal (make-alarm-info :dur1 (time:make-duration 0 0 0 1)
                                    :dur2 (time:make-duration 0 30 0))))
         (ad5 (aas-rpc:call-remote-impersonate
               "localhost" #'xed:get-alarm-info-default
               aas-rpc::*auth-token*
               cal )))
    (is ad1)
    (is ad2)
    (is ad3)
    (is ad4)
    (is-not (equalp ad3 ad4))
    (is (equalp ad4 ad5))))


(defun holidays-test (cal u-admin u-serve)
  (auth-test:test-with-user u-admin
    (verify-error error
        (xed:upsert-holiday :cal-id (calendar-id cal)
                            :hd (time:create-date 2015 3 15)))
    (let ((d1 (time:create-date 2019 2 14))
          (d2 (time:create-date 2021 12 14))
          (d3 (time:create-date 2019 3 14)))
      (auth-test:test-with-user u-serve
        (is (xed:upsert-holiday :cal-id (calendar-id cal) :hd d1))
        (is (xed:upsert-holiday :cal-id (calendar-id cal) :hd d2))
        (is (xed:upsert-holiday :cal-id (calendar-id cal) :hd d1))
        (is (xed:upsert-holiday :cal-id (calendar-id cal) :hd d3))
        (let ((list (xed:list-holiday :cal-id (calendar-id cal) :year 2019)))
          (is (= 2 (length list)))
          (is (equalp (aref list 0) d1))
          (is (equalp (aref list 1) d3)))
        (let ((list (xed:list-holiday :cal-id (calendar-id cal) :year 2022)))
          (is (null list)))))))


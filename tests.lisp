(in-package :cl-user)

(aseq-test:deftest fast-dummy-test
  (aseq-test:is t))

(aseq-test:deftest aas-all-tests
  (let ((time 0))
    (aas-cl:time-ex (time)
      (and (aas-cl-test::aas-cl-all-tests)
           (cl-user::fast-dummy-test)
           (log-test::all-tests)
           (basic-stats-test::all-tests)
           (sampling-test::all-tests)
           (aseq-test-test::all-tests)
           (assoc-db::make-db-test)
           (norvig-unify::norvig-unify-tests-1)
           (aas-misc::all-tests)
           (email-test::email-tests)
           (i18n-test::all-tests)
           (cache-test::cache-all-tests)
           (cipher-test::all-tests)
           (notify-test::all-tests)
           (aas-http-client-test::all-tests)
           (aas-rpc-test::all-tests)
           (extern-test::enum-tests)
           (db-base-test::all-tests)
           (aas-local-time-test::all-tests)
           (aas-graph::all-tests)
           (cloud-test::all-tests)
           (db-cloud-test::all-tests)
           (anon-app-test::all-tests)
           (auth-test::all-tests)
           (auth-test::aclall)
           (auth-test::companies-test)
           (xed-test::all-tests)
           (fin-cc-test::all-tests)
           (geo-US-test::all-tests)))
    (aseq-test:test-message 2 "total time: ~A" time)
    (main:console-trace (format nil "total time ~A seconds" time))
    (assert (< 70 time 150))))


;;arbitrary subset for my quick verification
;;will change as the wind blows
(aseq-test:deftest aas-tests-subset
  (aas-local-time:load-timezones)
  (auth-test:test-with-users (10)
    (and
     ;;(fin-cc-test::all-tests)
     (aas-rpc-test::all-tests)
     (db-base-test::all-tests)
     (cloud-test::all-tests)
     (db-cloud-test::all-tests)
     (auth-test::all-tests)
     (auth-test::aclall)
     (auth-test::companies-test)
     (xed-test::all-tests)
     ;;(aas-local-time-test::all-tests)
     )))
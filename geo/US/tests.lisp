(in-package :geo-US-test)

(aseq-test:deftest all-tests
  (and (us-states-test) (us-timezones-test) (ensure-zones-are-valid)
       (serialize-test)))

(aseq-test:deftest us-states-test
  (aseq-test:is (= 59 (length +US-STATE-CODES+)))
  (aseq-test:is (= (length +US-STATE-CODES+MIL+)
                   (length +US-STATE-NAMES+)
                   (hash-table-count *us-state-code-to-name*)))
  (aseq-test:is (string-equal "California" (US-state-name "CA")))
  (aseq-test:is (string-equal "District of Columbia" (US-state-name "DC")))
  (aseq-test:is (null (US-state-name "Ca")))
  (aseq-test:is (null (US-state-name nil)))
  (aseq-test:is (null (US-state-name "")))
  (aseq-test:is (null (US-state-name "CA "))))

(aseq-test:deftest us-timezones-test
  (aseq-test:is (= 8 (length (get-US-timezones))))
  (aseq-test:is (null (get-US-state-timezones "XX")))
  (aseq-test:is (equalp (get-US-state-timezones "CA")
                        (list "America/Los Angeles"))))


(aseq-test:deftest ensure-zones-are-valid
  (aas-local-time:load-timezones)
  (maphash (lambda (state-code zones)
             (aseq-test:is (= 2 (length state-code)))
             (dolist (zone zones)
               (aseq-test:is
                (not (null (olsen-data-lookup zone))))))
           *US-state-timezones*))

(deftest serialize-test
  (let ((adr1 (create-US-address
               "first st" "apt 123" "los angeles" "CA" "95503")))
    (is adr1)
    (let ((json (with-output-to-string (stream)
                  (aas-rpc:sout-object aas-rpc:+json-format+ stream
                                       'us-address adr1))))
      (is (stringp json))
      (is (< 20 (length json)))
      (with-input-from-string (in json)
        (let ((adr2 (funcall (aas-rpc::get-deserializer 'geo-us:us-address)
                             aas-rpc:+json-format+ in 'geo-us:us-address )))
          (is (equalp adr1 adr2)))
        )
      t)))


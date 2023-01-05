(in-package :anon-app-test)

(deftest all-tests
  (cloud:with-application +anon-app+
    (is (and (countries-test)))))

(deftest countries-test
  (let ((cc-list (aas-rpc:call-remote-anon
                  "localhost" #'anon-app:get-countries)))
    (is cc-list)
    (is (> (length cc-list) 3))
    ;;todo 1 need to test for timezones for all defined countries
    (let ((US (find "US" cc-list :key #'country-iso2 :test #'equal)))
      (is US)
      (is (country-zones-test US)))))

(defun country-zones-test (country)
  (let ((cc (country-iso2 country)))
    (let ((tzs (aas-rpc:call-remote-anon
                "localhost" #'anon-app:get-country-timezones cc)))
      (is tzs)
      (when (equalp cc "US")
        (is (< 5 (length tzs))))
      (is (find "US/Pacific Standard Time" tzs
                :test #'equal :key #'tz-name))
      (loop for tz across tzs do
           (is tz)
           (is (typep tz 'time:tz)))))
  t)

;; (is (find "CA" cc-list :key #'country-iso2 :test #'equal))
;;   (verify-error error
;;                 (aas-rpc:call-remote-anon
;;                  "localhost" #'anon-app:get-country-timezones "CA"))))


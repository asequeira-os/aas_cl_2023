(in-package :xed-test)


(deftest recur-tests
  (with-timezone "US/Eastern Standard Time"
    (dolist (year (list 2013 2016 2028))
      (dolist (month (list 1 2 3 11 12))
        (dolist (day (list 1 2 28 (time:days-in-month year month)))
          (let* ((start (time::make-date-time 0 0 0 day month year 0))
                 (end (add-duration start (make-duration 0 0 0 12 3 2))))
            (let ((r-list (create-test-recurs)))
              (is r-list)
              (dolist (r-e r-list)
                (let ((r (first r-e))
                      (expect-e (second r-e)))
                  (if expect-e
                      (verify-error error
                          (test-one-recur r start end))
                      (test-one-recur r start end)))))))))))

(defun  test-one-recur (r start end)
  (roundtrip-recur r)
  (let ((r (aas-rpc::deserialize-after r))) ;;for computations side effect
    (is r)
    (multiple-value-bind (r-list exhaust)
        (xed::recur-compute-in-range r start end)
      (is (or r-list exhaust)))))


(defun create-test-recurs ()
  (let ((list ))
    (dolist (dur (list 5 60 360))
      (dolist (rc (list nil 1 2 10 100))
        (dolist (end (list nil (create-date-time 2013 2 20 15 42 3)
                           (create-date-time 2016 3 1 15 42 3)))
          (dolist (freq (list nil +enum-recur-freq-daily+ +enum-recur-freq-weekly+
                              +enum-recur-freq-monthly+ +enum-recur-freq-yearly+)
                   ;;xed:+enum-recur-freq-list+
                   )
            (time:with-timezone aas-local-time-test::+new-york-test-name+
              (push (create-daily-test-recur freq end rc dur) list))) )))
    (nreverse list)))

(defun create-daily-test-recur (freq end rc dur)
  (let* ((start-dt (create-date-time 2012 3 15 20 15 0 ))
         (recur (xed::make-recur :tz *timezone*
                                 :start start-dt  :start-dto (dto-from-dt start-dt)
                                 :freq freq :interval nil
                                 :dow-v nil :dow-v-db nil
                                 :dow nil :week-num nil
                                 :end end :end-dto nil :max-repeat rc :dur dur)))
    (list recur
          (or (and (null freq) rc)))))

(defun roundtrip-recur (recur)
  (let ((json (with-output-to-string (s)
                (aas-rpc:sout-object aas-rpc:+json-format+ s 'xed::recur recur))))
    (is json)
    (let ((rd (with-input-from-string (in json)
                (funcall (aas-rpc::get-deserializer 'xed::recur)
                         aas-rpc:+json-format+ in 'xed::recur))))
      (is rd)
      rd)))

;;(recur-tests)

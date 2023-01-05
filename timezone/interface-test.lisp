(in-package :aas-local-time-test)

(deftest interface-tests
  (and (interface-tests-1) (interface-tests-2)
       (interface-tests-3)
       (dow-of-month-test)))

(deftest interface-tests-1
  (is (load-timezones))
  (is (< 300 (hash-table-count aas-local-time::*timezones*) 500))
  (is (< 300 (length (time::timezone-names)) 500))
  (let ((dt (create-date-time 2012 2 1 2 3 1))
        (dto (date-time-offset 2012 2 1 2 3 1 -3600)))
    (with-timezone +new-york-test-name+
      (is (not (null aas-local-time::*timezone*)))
      (let* ((latz (aas-local-time::olsen-data-lookup "America/Los Angeles"))
             (nytz (aas-local-time::olsen-data-lookup "America/New York")))
        (is (eq nytz (time::tz-data aas-local-time::*timezone*)))
        (is-not (eq latz (time::tz-data aas-local-time::*timezone*)))
        (is (typep latz 'aas-local-time::zone-tr-data))
        (is (dto= (dto-to-dto dto latz) dto))
        (is (typep (dt-to-dto dt latz) 'aas-local-time::dto))
        (is (dto= (dto-to-dto dto latz) dto))
        (is (dto> (dt-to-dto dt latz) (dt-to-dto dt nytz)))
        (is-not (= (dto-hour dto)
                   (dto-hour (dto-to-dto dto nytz))))
        (is-not (= (dto-hour (dto-to-dto dto latz))
                   (dto-hour (dto-to-dto dto nytz))))))))

;;todo 1 this test is shaky
(deftest interface-tests-2
  (let* ((latz (get-timezone +los-angeles-test-name+))
         (nytz (get-timezone +new-york-test-name+)))
    (let* ((utcnow (utc-now))
           (latz-now (now latz))
           (hour-conv (* (if (> 0 (dto-offset latz-now)) -1 1)
                         3600))
           (nytz-now (now nytz)))
      ;;(format t "~%~A ~A ~A" utcnow latz-now nytz-now)
      ;;this could fail if we are unlucky
      (is (dto= utcnow latz-now nytz-now))
      ;;todo 0 this test fails since I am now using utc machines
      ;;assuming machine is set to PST
      (with-slots (year month day hour minute offset) latz-now
        (multiple-value-bind (s m h d mm y dow dst xoffset)
            (decode-universal-time (get-universal-time)
                                   (/ offset hour-conv))
          (declare (ignorable s dst dow))
          (is (= year y))
          (is (= month mm))
          (is (= day d))
          (is (= hour h))
          (is (= minute m))
          (is (= offset (* (+ xoffset (if dst -1 0))
                           -3600))))))))

(deftest interface-tests-3
  (with-timezone +new-york-test-name+
    (let* ((now (now))
           (day-start (day-start now))
           (first-of-month (first-of-month now))
           (first-of-year (first-of-year now))
           (maxday (days-in-this-month now)))
      (is (<= maxday 31))
      ;;next check fails on the first day of every year
      (is (dto<= first-of-year first-of-month day-start now))
      (is (dto< day-start now)))))


;;2012 mar 1 is a thursday
;;=> 2012/3/1 dow 3
(deftest dow-of-month-test
  (let ((days (list 5 6 7 1 8 15 22 29 30 31 25))
        (doms (list
               (dow-of-month 0 0 3 2012) ;;should be 5th
               (dow-of-month 1 0 3 2012) ;;should be 6th
               (dow-of-month 2 0 3 2012) ;;should be 7th
               (dow-of-month 3 0 3 2012) ;;should be 1st
               (dow-of-month 3 1 3 2012) ;;should be 8
               (dow-of-month 3 2 3 2012) ;;should be 15
               (dow-of-month 3 3 3 2012) ;;should be 22
               (dow-of-month 3 4 3 2012) ;;should be 29
               (dow-of-month 4 4 3 2012) ;;should be 30
               (dow-of-month 5 4 3 2012) ;;should be 31
               (dow-of-month 6 4 3 2012) ;;should be 25
               )))
    (mapcar (lambda (day dom)
              (is (= day (date-d dom)))) days doms)))

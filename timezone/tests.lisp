(in-package :aas-local-time-test)

(deftest all-tests
  (load-timezones)
  (and (all-date-tests)
       (interface-tests)
       (all-date-time-tests)
       (date-date-time-compatibility)
       (local-tests)
       (dt-to-dto-tests)
       (dto-to-dto-tests)
       (bh-amb-test)
       (all-rule-parse-tests)
       (yesterday-tomorrow-test)
       (next-prev-hour-tests)
       (arithmetic-tests)))

(deftest date-date-time-compatibility
  (loop for year in '(2013 2016 2019) do
       (loop for month from 1 to 12 do
            (let ((max-day (days-in-month year month)))
              (loop for day from 1 to max-day do
                   (let* ((date (create-date year month day nil))
                          (date-time (make-date-time 0 0 0 day month year)))
                     (is (= (day-of-week date)
                            (dto-day-of-week date-time)))
                     (is (= (slot-value date 'day-num)
                            (slot-value date-time 'days-utc)))))))))

(deftest dt-to-dto-tests
  (mapcar
   (lambda (zone-name stdoffset dstoffset)
     ;;(format t "~%~A" zone-name)
     (let ((zone (gethash zone-name *timezones*)))
       (let* ((date (create-date 2013 2 10))
              (time (create-time 20 12 15))
              (dt (combine-date-time date time))
              (dto (multiple-value-list (dt-to-dto dt zone))))
         (is (= stdoffset (slot-value (first dto) 'offset))))
       (if (/= stdoffset dstoffset)
           (verify-error
               invalid-date-time
               (let* ((date (create-date 2012 3 11))
                      (time (create-time   2  12  15))
                      (dt (combine-date-time date time))
                      (dto (multiple-value-list (dt-to-dto dt zone))))
                 (declare (ignorable dto)))))
       (let* ((date (create-date 2013 3 14))
              (time (create-time  3  12  15))
              (dt (combine-date-time date time))
              (dto (multiple-value-list (dt-to-dto dt zone))))
         (is (= dstoffset (slot-value (first dto) 'offset))))
       (let* ((date (create-date 2013 11 7))
              (time (create-time  1  12  15))
              (dt (combine-date-time date time))
              (dto (multiple-value-list (dt-to-dto dt zone))))
         (is (= stdoffset (slot-value (first dto) 'offset))))
       (let* ((date (create-date 2013 11 7))
              (time (create-time  5  12  15))
              (dt (combine-date-time date time))
              (dto (multiple-value-list (dt-to-dto dt zone))))
         (is (= stdoffset (slot-value (first dto) 'offset))))
       (let* ((date (create-date 2013 12 31))
              (time (create-time  5  12  15))
              (dt (combine-date-time date time))
              (dto (multiple-value-list (dt-to-dto dt zone))))
         (is (= stdoffset (slot-value (first dto) 'offset))))))
   (list  "America/Los Angeles" "America/New York" "Asia/Kolkata")
   (list -28800 -18000 19800)
   (list -25200 -14400 19800)))

(deftest dto-to-dto-tests
  (let ((zone1 (gethash "America/New York" *timezones*))
        (zone2 (gethash "America/Los Angeles" *timezones*))
        (zone-utc (gethash "Etc/UTC" *timezones*))
        (year 2013)
        (months '(1 3 4 11 12))
        (days '(2 7 14 28))
        (invalid 0)
        (hours '(0 1 2 3 5)))
    (is
     (and (not (null zone1)) (not (null zone2)) (not (null zone-utc))
          (not (eq zone1 zone2)) (not (eq zone2 zone-utc))
          (not (eq zone-utc zone1))))
    (dolist (month months)
      (dolist (day days)
        (dolist (hour hours)
          (handler-case
              (let* ((date (create-date year month day))
                     (time (create-time  hour  55  45))
                     (dt (combine-date-time date time))
                     (dto (dt-to-dto dt zone1))
                     (dto2 (dto-to-dto dto zone2))
                     (dt2 (dto-to-dt dto2))
                     (dtutc (dto-to-dto dto zone-utc))
                     (dtutc2 (dto-to-dto dto2 zone-utc)))
                ;;(format t "~%~A -> ~A ->~& ~A -> ~A -> ~& ~A ~A"
                ;;        dt dto dto2 dt2 dtutc dtutc2)
                (is (local-date-time< dt2 dt))
                (is (and (dto= dto dto2 dtutc dtutc2)
                         (= (dto-hour dtutc) (dto-hour dtutc2))
                         (= 0 (dto-offset dtutc) (dto-offset dtutc2))))
                (let ((seconds-diff (mod-seconds-diff (total-seconds dt)
                                                      (total-seconds dt2))))
                  ;;todo 3 improve test verification constraints
                  (is (or (= (* 2 60 60) seconds-diff)
                          (= (* 3 60 60) seconds-diff)
                          (= (* 4 60 60) seconds-diff)))
                  ))
            (invalid-date-time ()
              (incf invalid))))))
    (is (= 0 invalid))))

(deftest bh-amb-test
  "test for clean, blackhole , and ambiguous local times"
  (with-timezone +new-york-test-name+
    (multiple-value-bind (dto amb)
        (dto-from-dt (create-date-time 2011 3 1 2 30 0))
      (is dto)
      (is-not amb))
    (verify-error invalid-date-time
        (dto-from-dt (create-date-time 2011 3 13 2 30 0)))
    (multiple-value-bind (dto amb)
        (dto-from-dt (create-date-time 2011 11 6 1 30 0))
      (is dto)
      (is amb))))

(deftest yesterday-tomorrow-test
  (dolist (year (list 2012 2015))
    (dolist (hour (list 0 1 2 3 4 22 23))
      (dolist (minute (list 0 1 30 59))
        (dolist (second (list 0 1 30 59))
          (with-timezone +new-york-test-name+
            (let* ((dto (dto-from-dt (create-date-time year 1 1 hour minute second)))
                   (n 800)
                   (forward (make-array n))
                   (reverse (make-array n)))
              (loop for i from 0 to (1- n) do
                   (setf (aref forward i) (copy-structure dto))
                   (setf dto (tomorrow dto)))
              (loop for i from (1- n) downto 0 do
                   (setf dto (yesterday dto))
                   (setf (aref reverse i) (copy-structure dto)))
              (is (equalp forward reverse)))))))))

(deftest next-prev-hour-tests
  (with-timezone +new-york-test-name+
    (let ((dto (dto-from-dt (create-date-time 2011 4 2 4 5 6))))
      (dotimes (i 24)
        (setf dto (next-hour dto)))
      (is (equalp dto (dto-from-dt (create-date-time 2011 4 3 4 5 6)))))
    (dolist (hour (list 0 1 2 3 4 22 23))
      (dolist (minute (list 0 1 30 59))
        (dolist (second (list 0 1 30 59))
          (dolist (year (list 2012 2015))
            (dolist (month (list 1 3 11 12))
              (loop for day from 1 to 31 do
                   (let ((dto
                          (ignore-errors
                            (dto-from-dt
                             (create-date-time year month day hour minute second)))))
                     (when dto
                       (let* ((n (next-hour dto))
                              (p (prev-hour dto)))
                         (is-not (equalp dto n))
                         (is-not (equalp dto p))
                         (is-not (equalp p n))
                         (is (equalp dto (prev-hour n)))
                         (is (equalp dto (next-hour p))))))))))))))

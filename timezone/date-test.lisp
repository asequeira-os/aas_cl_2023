(in-package :aas-local-time-test)

(define-constant +new-york-test-name+ "US/Eastern Standard Time")
(define-constant +los-angeles-test-name+ "US/Pacific Standard Time")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'aas-local-time::*serialize*)
  (import 'aas-local-time::*timezones*)
  (import 'aas-local-time::date)
  (import 'aas-local-time::date-y)
  (import 'aas-local-time::date-m)
  (import 'aas-local-time::date-d)
  (import 'aas-local-time::date-number)
  (import 'aas-local-time::date-number-to-date)
  (import 'aas-local-time::time)
  (import 'aas-local-time::tz-time)
  (import 'aas-local-time::tz-time-h)
  (import 'aas-local-time::tz-time-m)
  (import 'aas-local-time::tz-time-s)
  (import 'aas-local-time::day-on-fixed-day)
  (import 'aas-local-time::day-on-last-dow)
  (import 'aas-local-time::day-on-dow-relative)
  (import 'aas-local-time::day-on-dow>=)
  (import 'aas-local-time::day-on-dow<=)
  (import 'aas-local-time::day-on)
  (import 'aas-local-time::at)
  (import 'aas-local-time::zone-rules/save-local-std)
  (import 'aas-local-time::zone-rules/save-rules)
  (import 'aas-local-time::rules)
  (import 'aas-local-time::zone-rules/save-save)
  (import 'aas-local-time::tz-time-utc)
  (import 'aas-local-time::tz-time-local-std)
  (import 'aas-local-time::tz-time-local-wall)
  (import 'aas-local-time::days-utc)
  (import 'aas-local-time::dto)
  (import 'aas-local-time::day-num)
  (import 'aas-local-time::dow)
  (import 'aas-local-time::hour)
  (import 'aas-local-time::hours)
  (import 'aas-local-time::minute)
  (import 'aas-local-time::minutes)
  (import 'aas-local-time::second)
  (import 'aas-local-time::seconds)
  (import 'aas-local-time::offset)
  (import 'aas-local-time::day)
  (import 'aas-local-time::days)
  (import 'aas-local-time::month)
  (import 'aas-local-time::year)
  (import 'aas-local-time::dhms-to-total-seconds)
  (import 'aas-local-time::dur-months)
  (import 'aas-local-time::add-duration)
  (import 'aas-local-time::make-duration)
  (import 'aas-local-time::add-seconds)
  (import 'aas-local-time::dt-to-dto)
  (import 'aas-local-time::dto-to-dt)
  (import 'aas-local-time::dto-to-dto)
  (import 'aas-local-time::dto-offset)
  (import 'aas-local-time::mod-seconds-diff)
  (import 'aas-local-time::total-seconds)
  (import 'aas-local-time::is-valid-date-time)
  (import 'aas-local-time::dur-hours)
  (import 'aas-local-time::dur-days)
  (import 'aas-local-time::dto-days-utc)
  (import 'aas-local-time::dto-seconds-utc)
  (import 'aas-local-time::break-seconds)
  (import 'aas-local-time::dto-day-of-week)
  (import 'aas-local-time::make-date-time)
  (import 'aas-local-time::dto-day-of-week)
  (import 'aas-local-time::last-dow)
  (import 'aas-local-time::dow>=)
  (import 'aas-local-time::dow<=)
  (import 'aas-local-time::+EPOCH-YEAR+)
  (import 'aas-local-time::+MAX-YEAR+)
  (import 'aas-local-time::rule-parse-year-type)
  (import 'aas-local-time::dow-abbreviations)
  (import 'aas-local-time::dow-full-names)
  (import 'aas-local-time::rule-parse-from-year)
  (import 'aas-local-time::rule-parse-to-year)
  (import 'aas-local-time::rule-check-year-type)
  (import 'aas-local-time::rule-parse-month-in)
  (import 'aas-local-time::rule-parse-day-on)
  (import 'aas-local-time::rule-parse-save)
  (import 'aas-local-time::rule-parse-at)
  (import 'aas-local-time::zone-parse-gmtoff)
  (import 'aas-local-time::empty-string-p)
  (import 'aas-local-time::zone-parse-untilyear-month-day-time)
  (import 'aas-local-time::month-abbreviations)
  (import 'aas-local-time::zone-parse-rules/save))

(defun is-leap-year-test (year)
  (aas-local-time::is-leap-year-% year))

(defun test-day-of-week (year month day)
  (let* ((a (floor (- 14 month) 12))
         (y (- year  a))
         (m (+ month (* 12 a) -2)))
    (mod (- (+ day y (floor y 4) (floor y 400) (floor (* 31 m) 12))
            (floor y 100) 1) 7)))

(deftest all-date-tests
  (and (basics-test)
       (make-day-test-1)
       (make-day-test-2)
       (make-day-test-3)
       (make-day-test-4)
       (date-comparison-test)
       (day-of-week-test-1)))

(deftest basics-test
  (is (= aas-local-time::+epoch-universal-time+ 3160857600))
  (is (= aas-local-time::+compat-offset+ 3958))
  (is (= aas-local-time::+seconds-in-day+ 86400))
  (is (equalp aas-local-time::+days-in-month+
              #(31 28 31 30 31 30 31 31 30 31 30 31)))
  (is (equalp aas-local-time::+days-in-month-leap+
              #(31 29 31 30 31 30 31 31 30 31 30 31))))

(deftest make-day-test-1
  (let ((num-day (- 0 aas-local-time::+compat-offset+))
        (year 2000))
    (do () ((> year 2110))
      (loop for month in '(3 4 5 6 7 8 9 10 11 12 1 2) do
           (let* ((maxday (days-in-month year month)))
             (when (= 1 month)
               (incf year))
             (loop for day from 1 to maxday do
                  (let* ((date (create-date year month day))
                         (date-num-day (slot-value date 'day-num))
                         (date2 (date-number-to-date date-num-day)))
                    (is (and (= date-num-day num-day)
                             (= (test-day-of-week year month day)
                                (slot-value date 'dow))
                             (date= date date2)))
                    (incf num-day))))))))

#+ccl
(define-constant +min-test-year+ 1920)
#+sbcl
(define-constant +min-test-year+ 1900)

(deftest make-day-test-2
  (let ((num-day (1- (- 0 aas-local-time::+compat-offset+)))
        (year 2000))
    (do () ((< year (1+ +min-test-year+)))
      (loop for month in '(2 1 12 11 10 9 8 7 6 5 4 3) do
           (let* ((maxday (days-in-month year month)))
             (when (= month 12)
               (decf year))
             (loop for day downfrom maxday to 1 do
                  (let* ((date (create-date year month day))
                         (date-num-day (slot-value date 'day-num))
                         (date2 (date-number-to-date date-num-day)))
                    (is (and (= date-num-day num-day)
                             (= (test-day-of-week year month day)
                                (slot-value date 'dow))
                             (date= date date2)))
                    (decf num-day))))))))

(deftest make-day-test-3
  (loop for month in '(1 2 3 4 9 10 11 12) do
       (loop for year in '(1991 1994 1996 2000 2001 2003 2004 2008) do
            (let* ((maxday (days-in-month year month)))
              (loop for day downfrom maxday to 1 do
                   (loop for td from 1 to 10 do
                        (let* ((limit (* 10 365))
                               (num (random limit))
                               (d1 (create-date year month day))
                               (d2 (add-days d1 num))
                               (d3 (add-days d1 (- 0 num))))
                          (is (= (slot-value d2 'day-num)
                                 (+ (slot-value d1 'day-num)
                                    num)))
                          (is (= (slot-value d3 'day-num)
                                 (- (slot-value d1 'day-num)
                                    num))))))))))

(deftest make-day-test-4
  (is (date= (create-date 2011 3 27) (last-dow 6 3 2011)))
  (is (date= (create-date 2000 1 31) (last-dow 0 1 2000)))
  (is (date= (create-date 2000 2 27) (last-dow 6 2 2000)))
  (is (date= (create-date 2000 3 25) (last-dow 5 3 2000)))
  (loop for year in '(2000 2001 2007 2008 2013) do
       (loop for month from 1 to 12 do
            (loop for dow from 0 to 6 do
                 (progn
                   (day-test-last-dow dow month year)
                   (day-test-dow>=<= dow month year))))))


(defun day-test-last-dow (dow month year)
  (let* ((ldow (last-dow dow month year))
         (next-month?
          (date-number-to-date (+ 7 (date-number ldow)))))
    (is (= dow (day-of-week ldow)))
    (is (not (= (time::date-m next-month? ) month)))
    (is (and (= month (time::date-m ldow ))
             (= year (time::date-y ldow ))))))

(defun day-test-dow>=<= (dow month year)
  (let* ((maxday (days-in-month year month)))
    (loop for day from 1 to maxday do
         (progn
           (day-test-dow>= dow day month year)
           (day-test-dow<= dow day month year)))))

(defun day-test-dow>= (dow day month year)
  (let* ((minday (create-date year month day))
         (testdate (dow>= dow day month year))
         (test-dow (day-of-week testdate))
         (num-diff (- (date-number testdate) (date-number minday))))
    (is (= dow test-dow))
    (is (<= 0 num-diff 6))))

(defun day-test-dow<= (dow day month year)
  (let* ((maxday (create-date year month day))
         (testdate (dow<= dow day month year))
         (test-dow (day-of-week testdate))
         (num-diff (-  (date-number maxday) (date-number testdate))))
    (is (= dow test-dow))
    (is (<= 0 num-diff 6))))

(deftest day-of-week-test-1
  (let ((years '(1945 1973 2010 2011 2013 1980 1996 2000 2012 2100)))
    (dolist (year years)
      (day-of-week-test year))))

(defun day-of-week-test (year)
  (let* ((months '(1 2 3 6 7 8 11 12)))
    (dolist (month months)
      (loop for day from 1 to (days-in-month year month)
         do (is
             (= (day-of-week (create-date year month day))
                (seventh
                 (multiple-value-list
                  (decode-universal-time
                   (encode-universal-time 0 0 0 day month year) 0)))))))))


(deftest date-comparison-test
  (let ((y 2010)
        (m 11)
        (d 3))
    (let ((d1 (create-date y m d))
          (d2 (create-date (1+ y) m d))
          (d3 (create-date (1- y) m d)))
      (is (date= d1))
      (is (date< d3 d1 d2))
      (is-not (date> d3 d1 d2)))))

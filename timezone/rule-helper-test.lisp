(in-package :aas-local-time-test)

(aseq-test:deftest all-rule-parse-tests
  (and (rule-parse-test-1)
       (rule-parse-test-2)
       (rule-parse-test-3)
       (rule-parse-test-4)
       (zone-parse-test-1)
       (zone-parse-test-2)
       (zone-parse-test-3)))

(aseq-test:deftest rule-parse-test-1
  (aseq-test:is (= 2002 (rule-parse-from-year "2002")))
  (aseq-test:is (= 2055 (rule-parse-from-year "2055")))
  (aseq-test:is (> +epoch-year+ (rule-parse-from-year "1986")))
  (aseq-test:is (> +epoch-year+ (rule-parse-from-year "minimum")))
  (aseq-test:is (< +max-year+ (rule-parse-from-year "maximum")))
  (aseq-test:is (< +max-year+ (rule-parse-from-year "2200")))
  (aseq-test:is (> +epoch-year+ (rule-parse-from-year "min")))
  (aseq-test:is (< +max-year+ (rule-parse-from-year "max")))
  (aseq-test:is (= 2015 (rule-parse-to-year "only" 2015)))
  (aseq-test:is (= 2030 (rule-parse-to-year "2030" 2015))))

(aseq-test:deftest rule-parse-test-2
  (aseq-test:is (eq (rule-parse-year-type  "nonpres")
                    :non-us-presidential-election-year))
  (aseq-test:is (eq (rule-parse-year-type  "nonuspres")
                    :non-us-presidential-election-year))
  (aseq-test:is (eq (rule-parse-year-type  "uspres")
                    :us-presidential-election-year))
  (aseq-test:is (eq (rule-parse-year-type "-") :all-years))
  (aseq-test:is (eq (rule-parse-year-type "odd") :odd-years))
  (aseq-test:is (eq (rule-parse-year-type "even") :even-years))
  (aseq-test:is (eq (rule-parse-year-type "Odd") :odd-years))
  (aseq-test:is (eq (rule-parse-year-type "Even") :even-years))
  (aseq-test:verify-error error (rule-parse-year-type "evexn"))
  (aseq-test:is (rule-check-year-type :all-years 123))
  (aseq-test:is (rule-check-year-type :even-years 2012))
  (aseq-test:is (rule-check-year-type :odd-years 2013))
  (aseq-test:verify-error error (rule-check-year-type
                                 :non-us-presidential-election-year 2013))
  (aseq-test:verify-error error (rule-check-year-type
                                 :us-presidential-election-year 2013))
  (aseq-test:is (= 1 (rule-parse-month-in "Jan")))
  (aseq-test:is (= 1 (rule-parse-month-in "jan")))
  (aseq-test:is (= 12 (rule-parse-month-in "Dec")))
  (aseq-test:is (= 1 (rule-parse-month-in "January")))
  (aseq-test:is (= 1 (rule-parse-month-in "january")))
  (aseq-test:is (= 12 (rule-parse-month-in "December"))))

(aseq-test:deftest rule-parse-test-3
  (let ((sfx-v (vector "" "w" "s" "u" "g" "z"))
        (type-v (vector +enum-time-type-wall+ +enum-time-type-wall+
                        +enum-time-type-std+ +enum-time-type-utc+
                        +enum-time-type-utc+ +enum-time-type-utc+)))
    (do ((sfx-i 0 (incf sfx-i))) ((= sfx-i (length sfx-v)))
      (let ((sfx (aref sfx-v sfx-i))
            (type (aref type-v sfx-i))
            (td (vector "-" "0" "2" "2:25" "22:35:49"))
            (hours   (vector 0 0 2 2  22))
            (minutes (vector 0 0 0 25 35))
            (seconds (vector 0 0 0 0  49))
            (save-v (vector 0 0 7200 8700 81349)))
        (do ((i 0 (incf i))) ((= i (length td)))
          (let ((tz-time (rule-parse-at  (concatenate 'string (aref td i) sfx)))
                (save (rule-parse-save (aref td i))))
            (aseq-test:is (= (aref hours i) (tz-time-h tz-time)))
            (aseq-test:is (= (aref minutes i) (tz-time-m tz-time)))
            (aseq-test:is (= (aref seconds i) (tz-time-s tz-time)))
            (aseq-test:is (eq (time::tz-time-usw tz-time) type))
            (aseq-test:is (= save (aref save-v i)))))))))

(aseq-test:deftest rule-parse-test-4
  (let ((dn-l (list "1" "2" "28" "29" "30" "31"))
        (class (find-class 'day-on-fixed-day))
        (class>= (find-class 'day-on-dow>=))
        (class<= (find-class 'day-on-dow<=)))
    (dolist (dn dn-l)
      (let ((i 0)
            (day-on (rule-parse-day-on dn))
            (dni (parse-integer dn)))
        (aseq-test:is (eq (class-of day-on) class))
        (aseq-test:is (= (slot-value day-on 'day) dni))
        (flet ((testxxx (dow)
                 (let ((day-on>= (rule-parse-day-on
                                  (concatenate 'string dow ">=" dn)))
                       (day-on<= (rule-parse-day-on
                                  (concatenate 'string dow "<=" dn))))
                   (aseq-test:is (eq (class-of day-on>=) class>=))
                   (aseq-test:is (eq (class-of day-on<=) class<=))
                   (aseq-test:is (= i
                                    (slot-value day-on>= 'dow)
                                    (slot-value day-on<= 'dow))))))
          (dolist (dow dow-abbreviations)
            (testxxx dow)
            (incf i))
          (setf i 0)
          (dolist (dow dow-full-names)
            (testxxx dow)
            (incf i))))))
  (let ((class (find-class 'day-on-last-dow))
        (i 0))
    (flet ((test-one-dow (dow)
             (let ((day-on (rule-parse-day-on (concatenate 'string "last" dow))))
               (aseq-test:is (eq (class-of day-on) class))
               (aseq-test:is (= i (slot-value day-on 'dow))))))
      (dolist (dow dow-abbreviations)
        (test-one-dow dow)
        (incf i))
      (setf i 0)
      (dolist (dow dow-full-names)
        (test-one-dow dow)
        (incf i)))))


(aseq-test:deftest zone-parse-test-1
  (let ((td (vector "0" "2" "2:25" "22:35:49" "25:10:15"))
        (gmtoff-v (vector 0 7200 8700 81349 90615)))
    (do ((i 0 (incf i))) ((= i (length td)))
      (let ((gmtoff (zone-parse-gmtoff (aref td i)))
            (gmtoff- (zone-parse-gmtoff (concatenate 'string "-" (aref td i))))
            (expected (aref gmtoff-v i)))
        (aseq-test:is (= expected gmtoff))
        (aseq-test:is (= expected (* -1 gmtoff-)))))))

(aseq-test:deftest zone-parse-test-2
  (aseq-test:is (empty-string-p nil))
  (aseq-test:is (empty-string-p ""))
  (aseq-test:is (not (empty-string-p "x")))
  (aseq-test:is (not (empty-string-p "xy")))
  (let ((years (list nil "2012" "2040"))
        (in-l (list nil "Jan" "dec"))
        (on-l (list nil "5" "lastSun"))
        (at-l (list nil "2" "2u")))
    (dolist (year years)
      (dolist (in in-l)
        (dolist (on on-l)
          (dolist (at at-l)
            (test-zone-parse-until year in on at)))))))

(defun test-zone-parse-until (year in on at)
  (let ((zpu (zone-parse-untilyear-month-day-time year in on at)))
    (if (null year)
        (aseq-test:is (null zpu))
        (progn
          (aseq-test:is (= (parse-integer year)
                           (slot-value zpu 'year)))
          (if (null in)
              (aseq-test:is (= 1 (slot-value zpu 'month)))
              (progn
                (aseq-test:is
                 (= (1+ (position in month-abbreviations :test #'string-equal))
                    (slot-value zpu 'month)))
                (if (null on)
                    (progn
                      (aseq-test:is (typep (slot-value zpu 'day-on) 'day-on-fixed-day))
                      (aseq-test:is (= 1 (slot-value (slot-value zpu 'day-on) 'day))))
                    (progn
                      (when (string-equal "5" on)
                        (aseq-test:is (typep (slot-value zpu 'day-on) 'day-on-fixed-day))
                        (aseq-test:is (= 5 (slot-value (slot-value zpu 'day-on) 'day))))
                      (when (string-equal "lastSun" on)
                        (aseq-test:is (typep (slot-value zpu 'day-on) 'day-on-last-dow))
                        (aseq-test:is (= 6 (slot-value (slot-value zpu 'day-on) 'dow))))
                      (let ((ato (slot-value zpu 'at)))
                        (if (null at)
                            (progn
                              (aseq-test:is (eq time::+enum-time-type-wall+
                                                (time::tz-time-usw ato)))
                              (aseq-test:is (= 0 (time::tz-time-h ato )
                                               (time::tz-time-m ato )
                                               (time::tz-time-s ato ))))
                            (progn
                              (aseq-test:is (= 2 (time::tz-time-h ato )))
                              (when (string-equal "2" at)
                                (aseq-test:is (eq time::+enum-time-type-wall+
                                                  (time::tz-time-usw ato))))
                              (when (string-equal "2u" at)
                                (aseq-test:is (eq time::+enum-time-type-utc+
                                                  (time::tz-time-usw ato)))))))))))))))

(aseq-test:deftest zone-parse-test-3
  (let ((tdl (list "-" "foorule" "2:34"))
        (i 0))
    (dolist (td tdl)
      (let ((r/s (zone-parse-rules/save td)))
        (case i
          (0
           (aseq-test:is (typep r/s 'zone-rules/save-local-std)))
          (1
           (aseq-test:is (typep r/s 'zone-rules/save-rules))
           (aseq-test:is (string-equal "foorule"
                                       (slot-value r/s 'rules))))
          (2
           (aseq-test:is (typep r/s 'zone-rules/save-save))
           (aseq-test:is (= 9240 (slot-value r/s 'seconds))))))
      (incf i))))


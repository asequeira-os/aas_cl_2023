(in-package :aas-local-time-test)

(aseq-test:deftest local-tests
  (and (local-ctor-test)
       (local-add-seconds-test)
       (dto-comparison-tests)
       (json-tests)
       (dto-to-dt-test)))


(defun local-test-base ()
  (let* ((d1 (create-date 2010 03 15))
         (t1 (create-time  14 13 12))
         (dt1 (combine-date-time d1 t1)))
    dt1))

(aseq-test:deftest local-ctor-test
  (let* ((dt1 (local-test-base)))
    (with-slots (date time) dt1
      (let ((year (date-y date))
            (month (date-m date))
            (day (date-d date)))
        (aseq-test:is (= year 2010))
        (aseq-test:is (= month 3))
        (aseq-test:is (= day 15)))
      (let ((hour (tz-time-h time))
            (minute (tz-time-m time))
            (second (tz-time-s time)))
        (aseq-test:is (= hour 14))
        (aseq-test:is (= minute 13))
        (aseq-test:is (= second 12))))))

(aseq-test:deftest local-add-seconds-test
  (let* ((2h (* 2 60 60))
         (15h (* 15 60 60))
         (15h+3s (+ 15h 3))
         (dt1 (local-test-base))
         (dt2 (add-seconds dt1 2h))
         (dt3 (add-seconds dt1 (- 2h)))
         (dt4 (add-seconds dt1 15h))
         (dt5 (add-seconds dt1 (- 15h)))
         (dt6 (add-seconds dt1 15h+3s))
         (dt7 (add-seconds dt1 (- 15h+3s))))
    (test-print "~A" dt1
                "#<2010/03/15># #<14:13:12w>#")
    (test-print "~A" dt2
                "#<2010/03/15># #<16:13:12w>#")
    (test-print "~A" dt3
                "#<2010/03/15># #<12:13:12w>#")
    (test-print "~A" dt4
                "#<2010/03/16># #<05:13:12w>#")
    (test-print "~A" dt5
                "#<2010/03/14># #<23:13:12w>#")
    (test-print "~A" dt6
                "#<2010/03/16># #<05:13:15w>#")
    (test-print "~A" dt7
                "#<2010/03/14># #<23:13:09w>#")
    ;;next line should not throw error
    (with-output-to-string (ss)
      (format ss "~S" dt7))
    (let ((*serialize* 1))
      (aseq-test:verify-error error
          (with-standard-io-syntax
            (test-print "~S" dt7
                        "#<2010/03/14># #<23:13:09w>#"))))))

(aseq-test:deftest dto-to-dt-test
  (let* ((dto (make-date-time 59 58 23 29 2 2012))
         (dt (dto-to-dt dto)))
    (with-slots (date time) dt
      (let ((year (date-y date))
            (month (date-m date))
            (day (date-d date)))
        (let ((hour (tz-time-h time))
              (minute (tz-time-m time))
              (second (tz-time-s time)))
          (aseq-test:is
           (and (= 2012 year) (= month 2) (= day 29)
                (= 23 hour) (= 58 minute) (= 59 second))))))))

(aseq-test:deftest dto-comparison-tests
  (let* ((2h (* 2 60 60))
         (23h (* 23 60 60))
         (dt5 (local-test-base))
         (dt4 (add-seconds dt5  (- 23h)))
         (dt3 (add-seconds dt4  (- 2h)))
         (dt2 (add-seconds dt3  (- 2h)))
         (dt1 (add-seconds dt2  (- 1)))
         (dt6 (add-seconds dt5 2h))
         (dt7 (add-seconds dt6 23h))
         (dt8 (add-seconds dt7 2h))
         (dt9 (add-seconds dt8 1)))
    (aseq-test:is
     (and
      (local-date-time< dt1 dt2 dt3 dt4 dt5 dt6 dt7 dt8 dt9)
      (local-date-time< dt1)
      (local-date-time< dt1 dt2)
      (local-date-time> dt9 dt6 dt5 dt3)
      (local-date-time<= dt1 dt2 dt3 dt4 dt5 dt6 dt7 dt8 dt9)
      (local-date-time<= dt1)
      (local-date-time<= dt1 dt2)
      (local-date-time>= dt9 dt6 dt5 dt3)
      (local-date-time= dt1)
      (local-date-time= dt1 dt1)
      ;;(local-date-time/= dt1)
      ;;(local-date-time/= dt1 dt2)
      ))

    (aseq-test:is-not
     (or
      (local-date-time> dt1 dt2 dt3 dt4 dt5 dt6 dt7 dt8 dt9)
      (local-date-time> dt1 dt2)
      (local-date-time< dt9 dt6 dt5 dt3)
      (local-date-time>= dt1 dt2 dt3 dt4 dt5 dt6 dt7 dt8 dt9)
      (local-date-time>= dt1 dt2)
      (local-date-time<= dt9 dt6 dt5 dt3)
      ;;(local-date-time/= dt1 dt1)
      (local-date-time= dt1 dt2)))))

(deftest json-tests
  (let ((tt (create-time 22 54 30 +enum-time-type-std+))
        (d (create-date 2019 3 23)))
    (let ((dt (combine-date-time d tt)))
      (let ((js (with-output-to-string (s)
                  (aas-rpc::sout-object aas-rpc::+json-format+ s 'time::local-date-time
                                        dt))))
        ;;(print js)
        (let ((us (with-input-from-string (s js)
                    (funcall (aas-rpc::get-deserializer 'time::local-date-time)
                             aas-rpc::+json-format+ s 'time::local-date-time))))
          (is (equalp dt us)))))))

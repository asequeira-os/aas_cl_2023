(in-package :aas-local-time-test)

(aseq-test:deftest arithmetic-tests
  (and (dur-add-test)))

(aseq-test:deftest dur-add-test
  (let ((invalid 0))
    (handler-case
        (with-timezone +los-angeles-test-name+
          (dolist (year '(2013 2018 2020))
            (dolist (month '(1 3 6 11 12))
              (dolist (day '(1 7 14 28))
                (dolist (hour '(0 1 2 3 22 23))
                  (dolist (minute '(0 30 59))
                    (dolist (second '(0 30 59))
                      (rel-dur-add-test year month day
                                        hour minute second)
                      (abs-dur-add-test year month day
                                        hour minute second))))))))
      (invalid-date-time () (incf invalid)))
    (aseq-test:is (= 1 invalid))))

(defun rel-dur-add-test (year month day hour minute second)
  (let* ((dto (create-date-time year month day hour minute second))
         (dtotz (dt-to-dto dto (time::tz-data *timezone*))))
    (dolist (dyear '(0 1 4 20))
      (dolist (dmonth '(0 1 11 12 13 24 25))
        (dolist (dday '(0 1 30 60 370))
          (let* ((dur (make-duration 0 0 0 dday dmonth dyear))
                 (dto2 (add-duration dtotz dur))
                 (dd (date-difference dto2 dtotz))
                 (orig (subtract-duration dto2 dur))
                 )
            (is (dto= dtotz orig))
            (if (= 0 dday dmonth dyear)
                (progn
                  (aseq-test:is (= 0 (dur-days dd)
                                   (dur-months dd)))
                  (aseq-test:is (dto= dtotz dto2)))
                (progn
                  (aseq-test:is (< 0 (+ (dur-days dd)
                                        (dur-months dd))))
                  (aseq-test:is (dto< dtotz dto2))))))))))

(defun abs-dur-add-test (year month day hour minute second)
  (let* ((dto (create-date-time year month day hour minute second))
         (dtotz (dt-to-dto dto (time::tz-data *timezone*))))
    (dolist (dseconds '(0 1 59))
      (dolist (dminutes '(0 1 59))
        (dolist (dhours '(0 1 23 48))
          (dolist (ddays '(0 1 33))
            (let* ((dur (make-duration dseconds dminutes dhours
                                       ddays 0 0))
                   (dto2 (add-duration dtotz dur))
                   (dto3 (subtract-duration dtotz dur))
                   (dd (date-difference dto2 dtotz)))
              (declare (ignorable dd))
              (is (equalp dtotz (add-duration dto3 dur)))
              (with-slots (seconds minutes hours days  months years) dd
                (aseq-test:is (= (dhms-to-total-seconds
                                  days hours minutes seconds)
                                 (dhms-to-total-seconds
                                  ddays dhours dminutes dseconds))))

              (if (= 0 dseconds dminutes dhours ddays)
                  (aseq-test:is (dto= dtotz dto2))
                  (aseq-test:is (dto< dtotz dto2))))))))))

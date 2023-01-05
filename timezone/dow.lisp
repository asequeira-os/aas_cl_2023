(in-package :aas-local-time)

(defun dow-of-month (dow week-num month year)
  "get a specific day of  week for a month
e.g. wednesday of 3rd week of  May 2015
dow is 0 to 6 as usual
week-num 0 to 4, 4 meaning last (could  end up as 4th week)"
  (let* ((fom (create-date year month 1 ))
         (fdow (day-of-week fom))
         (diff (- dow fdow))
         (days (+ (* week-num 7) diff))
         (guess (add-days fom days)))
    (if (= (date-m guess) month)
        guess
        (if (date< guess fom)
            (add-days guess 7)
            (add-days guess -7)))))



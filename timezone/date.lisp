(in-package :aas-local-time)

(defstruct date
  (y nil   :type (integer 1800 2200))
  (m nil  :type (integer 1 12))
  (d nil  :type (integer 1 31))
  (leap nil :type boolean )
  (day-num nil  :type (or null integer))
  (dow  nil :type (integer 0 6)))

(defmethod sout-object (ser-type stream type (tt date))
  (declare (ignorable type))
  (aas-rpc:sout-object ser-type stream 'string
                       (date-format-string tt)))

(defun date-format-string (tt)
  (format nil "~4,'0D/~2,'0D/~2,'0D"
          (date-y tt)(date-m tt)(date-d tt)))

(aas-rpc:set-deserializer
 'date
 (lambda (ser-type stream obj-type)
   (let ((ts (aas-rpc::deserialize-string ser-type stream obj-type)))
     (date-parse-string ts))))

(defun date-parse-string (ts)
  (let ((tsar (split-sequence:split-sequence #\/ ts)))
    (unless (= 3 (length tsar))
      (error "bad date string: ~A" ts))
    (let* ((y (parse-integer (first tsar)))
           (m (parse-integer (second tsar)))
           (d (parse-integer (third tsar)))
           )
      (create-date y m d t))))

;;specially chosen epoch year that is a multiple of 400
;;epoch (that is day zero) is March 1st, 2000
(define-constant +xxx-epoch-year+ 2000) ;;NO change allowed - set in stone
(define-constant +xxx-epoch-month+ 3)   ;;NO change allowed - set in stone


;;Mar. 1, 2000 was Wednesday => 2
(define-constant +xxx+epoch-day-of-week+
    (seventh (multiple-value-list
              (decode-universal-time
               (encode-universal-time  0 0 0 1 +xxx-epoch-month+ +xxx-epoch-year+) 0))))

(assert (= +xxx+epoch-day-of-week+ 2))
(assert (< +xxx-epoch-year+ +epoch-year+))

(define-constant +compat-offset+
    (- (+ (* 365 (- +epoch-year+ +xxx-epoch-year+))
          (floor (/ (-  +epoch-year+ +xxx-epoch-year+) 4)))
       (+ 31 28)) )

;;we treat march as month zero
(define-constant +xxx-days-in-month+
    (vector 31 30 31 30 31 31 30 31 30 31 31))

;;the begining day number of each month
(defvar +yyy-month-to-day-num+
  (make-array 12 :adjustable nil :element-type '(integer 0 366)))

(let ((offset 0))
  (loop for x from 0 to 10
     do (progn
          (setf (aref +yyy-month-to-day-num+ x) offset)
          (incf offset (aref +xxx-days-in-month+ x))))
  (setf (aref +yyy-month-to-day-num+ 11) offset))


;;map from any day 0 - 365 to a xmonth
(defvar +xxx-days-to-month+
  (make-array 366 :adjustable nil :element-type '(integer 0 11)))

(let ((offset 0))
  (loop for month from 0 to 10 do
       (loop for day from 1 to (aref +xxx-days-in-month+ month) do
            (progn
              (setf (aref +xxx-days-to-month+ offset) month)
              (incf offset))))
  (loop for febday from offset to 365 do
       (setf  (aref +xxx-days-to-month+ febday) 11)))

(defgeneric day-of-week (obj) )

(defmethod day-of-week ((obj date))
  (date-dow obj))

(defun date-number (date)
  (date-day-num date))

(defmethod db-base:get-struct-columns ((symbol (eql 'date)))
  '((day-num integer)))

(defun make-date-from-db (&key day-num)
  (date-number-to-date day-num))

;;does not work - cause index column is typed as integer for the day-num
;; (db-base:set-where-arg-trasnformer 'date 'date-to-db%)
;; (defun date-to-db% (date)
;;   (date-day-num date))

(defun date<% (d1 d2)
  (< (slot-value d1 'day-num) (slot-value d2 'day-num)))
(defun date=% (d1 d2)
  (= (slot-value d1 'day-num) (slot-value d2 'day-num)))
(defun date>% (d1 d2)
  (> (slot-value d1 'day-num) (slot-value d2 'day-num)))

(def-comparison "date" 'date date>% date<% date=%)

;;todo 5 tests pending for this print-object method
(defmethod print-object ((date date) stream)
  (if (and (not *print-readably*) (not *print-escape*))
      (with-slots (y m d) date
        (format stream "#<~D/~2,'0D/~2,'0D>#" y m d))
      (call-next-method)))

(defun is-leap-year-% (year)
  (declare (type fixnum year))
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun create-date (year month day &optional (validate t))
  (when validate
    (let* ((leap (is-leap-year-% year))
           (max-days (if leap
                         +days-in-month-leap+
                         +days-in-month+)))
      (and (or (< 1800 year 2200) (error "year out of range"))
           (or (< 0 month 13) (error "invalid month"))
           (or (< 0 day 32) (error "invalid day"))
           (or (<= day (aref max-days (1- month)))
               (error "day out of range")))))
  (make-date-% year month day))

(defun make-date-% (year month day)
  (declare (type (integer 1800 2300) year)
           (type (integer 1 12) month)
           (type (integer 1 31) day))
  (let* ((mn (mod (- month 3) 12))
         (num-years (+ (- year +xxx-epoch-year+)
                       (if (< month 3)
                           -1
                           0)))
         (leaps (- (+ (floor num-years 4) (floor num-years 400))
                   (floor num-years 100)))
         (day-num (+ (* num-years 365)
                     leaps
                     (if (zerop mn)
                         0
                         (aref +yyy-month-to-day-num+ mn))
                     (1- day)))
         (dow
          (mod (+ +xxx+epoch-day-of-week+ day-num) 7)))
    (declare (type (integer -500 500) num-years)
             (type (integer -12 12) mn)
             (type (integer -150 150) leaps)
             (type (integer -200000 200000) day-num)
             (type (integer 0 6) dow))
    (make-date :y year :m month :d day
               :leap (is-leap-year-% year)
               :day-num (- day-num +compat-offset+ ) :dow dow)))


(define-constant +epoch-universal-time+
    (encode-universal-time 0 0 0 1 +xxx-epoch-month+ +xxx-epoch-year+))

(defun make-date-%2 (year month day)
  "not used. looks slower than make-date-%"
  (declare (type (integer 1800 2300) year)
           (type (integer 1 12) month)
           (type (integer 1 31) day))
  (let* ((ut (encode-universal-time 0 0 0 day month year))
         (ut2 (- ut +epoch-universal-time+))
         (day-num (floor ut2 +seconds-in-day+))
         (dow
          (mod (+ +xxx+epoch-day-of-week+ day-num) 7)))
    (make-instance 'date :year year :month month :day day
                   :leap (is-leap-year-% year)
                   :day-num (- day-num +compat-offset+) :dow dow)))

(defun date-number-to-date (day-num)
  (let ((ut (+ +epoch-universal-time+
               (* +seconds-in-day+ (+ day-num +compat-offset+)))))
    (multiple-value-bind
          (second minute hour day month year)
        (decode-universal-time ut 0)
      (declare (ignorable second minute hour))
      (create-date year month day))))

(defun date-equal-p (date1 date2)
  (= (slot-value date1 'day-num)
     (slot-value date2 'day-num)))

(defun add-days (date days)
  (date-number-to-date
   (+ days (slot-value date 'day-num))))

(defun start-of-next-month (year month)
  (declare (type (integer 1 12) month)
           (type (integer 1500 2500) year))
  (if (= 12 month)
      (create-date (1+ year) 1 1)
      (create-date year (1+ month) 1)))

(defun last-dow (dow month year)
  "get last day of the week"
  (declare (type (integer 0 6) dow)
           (type (integer 1 12) month))
  (let* ((next-month (start-of-next-month year month))
         (dow-next-month (day-of-week next-month))
         (diff (mod (- dow-next-month dow) 7)))
    (if (zerop diff)
        (date-number-to-date (- (date-number next-month) 7))
        (date-number-to-date (- (date-number next-month) diff)))))

(defun dow>= (dow day month year)
  (let* ((mindate (create-date year month day))
         (min-dow (day-of-week mindate))
         (diff (mod (- dow min-dow) 7)))
    (date-number-to-date (+ (date-number mindate) diff))))

(defun dow<= (dow day month year)
  (let* ((maxdate (create-date year month day))
         (max-dow (day-of-week maxdate))
         (diff (mod (- max-dow dow) 7)))
    (date-number-to-date (- (date-number maxdate) diff))))
(in-package :aas-local-time)

(defvar *leap-years* nil
  "bitvector with leap year bits (epoch offset is zero) set")

(defvar *start-of-year-day* nil
  "day offset for the first day of each year")

(defvar *start-of-month-day*
  (make-array 12 )
  "day offset for the first of each month within a year")

(defvar *start-of-month-day-leap*
  (make-array 12 )
  "day offset for the first of each month within a leap year")

(defvar *day-to-month*
  (make-array 365 )
  "the month of any given day of year")

(defvar *day-to-month-leap*
  (make-array 366 )
  "the month of any given day of a leap year")

;;day of week numbers conform to CLHS 25.1.4.1 Decoded Time
;;0 means Monday, 1 means Tuesday, and so on; 6 means Sunday
(define-constant +epoch-day-of-week+
    (seventh (multiple-value-list
              (decode-universal-time
               (encode-universal-time  0 0 0 1 1 +epoch-year+) 0))))

(defstruct dto
  (days-utc nil :type (integer 0 50000) :read-only t)
  (seconds-utc nil :type (integer 0 86401) :read-only t)
  (year nil :type (integer 0 2200) :read-only t)
  (month nil :type (integer 1 12) :read-only t)
  (day nil :type (integer 1 31) :read-only t)
  (hour nil :type (integer 0 23) :read-only t)
  (minute nil :type (integer 0 59) :read-only t)
  (second nil :type (integer 0 59) :read-only t)
  (day-of-week nil :type (integer 0 6) :read-only t)
  (offset nil :type (integer -93600 93600) :read-only t))

;;db storage for dto
(defmethod db-base:get-struct-columns ((symbol (eql 'dto)))
  '((dbdt integer) ;;synthetic column
    (offset integer)))

(def-rpc-struct dur
    (seconds nil :type (or null integer) :read-only t)
  (minutes nil :type (or null integer) :read-only t)
  (hours nil :type (or null integer) :read-only t)
  (days nil :type (or null integer) :read-only t)
  (months nil :type (or null integer) :read-only t)
  (years nil :type (or null integer) :read-only t)
  (total-seconds nil :skip-rpc :type (or null integer) :read-only t)
  )

(defmethod aas-rpc:deserialize-after ((dur dur))
  (make-duration (dur-seconds dur) (dur-minutes dur) (dur-hours dur)
                 (dur-days dur) (dur-months dur) (dur-years dur)))

(defmethod db-base:get-struct-columns ((symbol (eql 'dur)))
  '((seconds  (or null integer) )
    (minutes  (or null integer) )
    (hours  (or null integer) )
    (days  (or null integer) )
    (months  (or null integer) )
    (years  (or null integer) )
    (total-seconds  (or null integer))))

(defun make-dur-from-db (&key seconds  minutes hours days months years total-seconds)
  (make-dur :seconds seconds  :minutes minutes :hours hours
            :days days :months months :years years :total-seconds total-seconds))

(defconstant +db-seconds-shift+ 100000) ;; > max seconds in a day

(defun dto-dbdt (dto)
  (+ (* (dto-days-utc dto) +db-seconds-shift+)
     (dto-seconds-utc dto)))

(defun make-dto-from-db (&key dbdt offset)
  (multiple-value-bind (days-utc seconds-utc)
      (floor dbdt +db-seconds-shift+)
    (days-seconds-to-date-time days-utc seconds-utc offset)))

(defmethod make-load-form ((self dto) &optional environment)
  (declare (ignorable environment))
  `(make-dto
    :days-utc ',(dto-days-utc self)
    :seconds-utc ',(dto-seconds-utc self)
    :year ',(dto-year self)
    :month ',(dto-month self)
    :day ',(dto-day self)
    :hour ',(dto-hour self)
    :minute ',(dto-minute self)
    :second ',(dto-second self)
    :day-of-week ',(dto-day-of-week self)
    :offset ',(dto-offset self)))


(defmethod day-of-week ((obj dto))
  (dto-day-of-week obj))

(defun utc-now ()
  (multiple-value-bind (second minute hour date month year x y offset)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignorable x y))
    (let ((dto
           (make-date-time second minute hour
                           date month year (* (- offset) 60 60))))
      (days-seconds-to-date-time (slot-value dto 'days-utc)
                                 (slot-value dto 'seconds-utc)
                                 0))))

;;some validations(document assumptions)

(defun valid-ts-component (tc start end)
  (and (integerp tc)
       (<= start tc end)))

(defun valid-second (second)
  (valid-ts-component second 0 59))

(defun valid-minute (minute)
  (valid-ts-component minute 0 59))

(defun valid-hour (hour)
  (valid-ts-component hour 0 23))

(defun valid-day (day)
  (valid-ts-component day 1 31))

(defun valid-month (month)
  (valid-ts-component month 1 12))

;;todo Z  need to remove min year restriction
;;this is not important to do item, since for date only (no time)
;;I will not have this restriction
(defun valid-year (year)
  (valid-ts-component year +epoch-year+ +max-year+))

(let ((vector-size (1+ (- +max-year+ +epoch-year+))))
  (setf *leap-years*
        (make-array vector-size :element-type 'bit :initial-element 0))
  (setf *start-of-year-day*
        (make-array vector-size :element-type 'integer :initial-element 0))
  (do ((i 0 (incf i))
       (start-of-year-day 0)
       (year  +epoch-year+ (incf year)))
      ((> year +max-year+))
    (let ((is-leap (is-leap-year-% year)))
      (setf (aref *leap-years* i)
            (if is-leap 1 0))
      (setf (aref *start-of-year-day* i) start-of-year-day)
      (incf start-of-year-day
            (if is-leap 366 365)))))

(do ((i 0 (incf i))
     (offset 0)
     (offset-leap 0))
    ((= i 12))
  (setf (aref *start-of-month-day* i) offset)
  (setf (aref *start-of-month-day-leap* i) offset-leap)
  (incf offset (aref +days-in-month+ i))
  (incf offset-leap (aref +days-in-month-leap+ i)))

(let ((normal-month 1)
      (leap-month 1))
  (dotimes (day 365)
    (if (= 12 normal-month)
        (setf (aref *day-to-month* day) normal-month)
        (if (< day (aref *start-of-month-day* normal-month))
            (setf (aref *day-to-month* day) normal-month)
            (progn
              (incf normal-month)
              (setf (aref *day-to-month* day) normal-month))))
    (if (= 12 leap-month)
        (setf (aref *day-to-month-leap*  day) leap-month)
        (if (< day (aref *start-of-month-day-leap* leap-month))
            (setf (aref *day-to-month-leap* day) leap-month)
            (progn
              (incf leap-month)
              (setf (aref *day-to-month-leap* day) leap-month)))))
  (setf (aref *day-to-month-leap* 365) 12))

(defun days-in-month (year month)
  (if (is-leap-year year)
      (aref +days-in-month-leap+ (1- month))
      (aref +days-in-month+ (1- month))))

(defun is-leap-year (year)
  (declare (type fixnum year))
  (if (<= +epoch-year+ year +max-year+)
      (= 1 (aref *leap-years* (- year +epoch-year+)))
      (is-leap-year-% year)))

(defun is-valid-date-time (second minute hour day month year)
  (let* ((basic (and (valid-second second)
                     (valid-minute minute)
                     (valid-hour hour)
                     (valid-day day)
                     (valid-month month)
                     (valid-year year)))
         (leap (and basic (is-leap-year year)))
         (days-in-month (if leap
                            +days-in-month-leap+
                            +days-in-month+)))
    (and basic
         (<= day (aref days-in-month (1- month))))))

;;we could have positive or negative offset
;;offset is in seconds
;;edge cases of more than 24 hours in some places are correclty handled
(defun offset-to-utc (day seconds offset)
  (let* ((offset-seconds (* offset -1))
         (seconds (+ seconds offset-seconds)))
    (multiple-value-bind (day-offset seconds)
        (floor seconds +seconds-in-day+)
      (values (+ day day-offset) seconds))))

(defun valid-offset (offset)
  (and (integerp offset)
       (< (* -26 60 60) offset (* 26 60 60))))

;;offset + for India, - for US
(defun make-date-time (second minute hour day month year &optional (offset 0))
  "Date time components that are same as CL standard decoded time. offset meaning is  simplified. It is whole number of minutes (+/-). Defaults to 0, meaning UTC"
  (or (and (is-valid-date-time second minute hour day month year)
           (valid-offset offset))
      (error "invalid date time"))
  (let* ((is-leap (is-leap-year year))
         (my-day (+ (aref *start-of-year-day* (- year +epoch-year+))
                    (if (> month 1)
                        (aref (if is-leap
                                  *start-of-month-day-leap*
                                  *start-of-month-day*)
                              (- month 1))
                        0)
                    (1- day)))
         (day-of-week
          (mod (+ +epoch-day-of-week+  my-day) 7))
         (my-seconds (+ (* hour +seconds-in-hour+)
                        (* minute 60)
                        second)))
    ;;db storage needs to store my-day my-seconds offset only
    (multiple-value-bind (my-day my-seconds)
        (offset-to-utc my-day my-seconds offset)
      (make-dto :days-utc my-day :seconds-utc my-seconds
                :year year :month month :day day
                :hour hour :minute minute :second second
                :day-of-week day-of-week :offset offset))))

(defmethod print-object ((dto dto) stream)
  (with-slots (year month day hour minute second offset) dto
    (let ((offset-minutes (truncate (/ offset 60))))
      (if (and *serialize* *print-readably* *print-escape*)
          (format stream "(make-date-time ~S ~S ~S ~S ~S ~S ~S)"
                  second minute hour day month year offset)
          (if (and (not *print-readably*) (not *print-escape*))
              (format stream
                      "#<~D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D~C~2,'0D:~2,'0D>#"
                      year month day hour minute second
                      (if (< offset-minutes 0) #\- #\+)
                      (floor (abs offset-minutes) 60)
                      (rem (abs offset-minutes) 60))
              (call-next-method))))))


(defun days-to-year (day)
  (let ((index (floor (/ day 365))))
    (util:while (> (aref *start-of-year-day* index) day)
      (decf index))
    (values (+ +epoch-year+ index) index)))

(defun days-to-month-day (days year)
  (let* ((leap (is-leap-year year))
         (month (aref (if leap
                          *day-to-month-leap*
                          *day-to-month*) days))
         (day (- days (aref (if leap
                                *start-of-month-day-leap*
                                *start-of-month-day*) (1- month)))))
    (values month (1+ day))))

(defun days-to-date (days)
  (multiple-value-bind (year index)
      (days-to-year days)
    (let ((days-left (- days (aref *start-of-year-day* index))))
      (multiple-value-bind (month day)
          (days-to-month-day days-left year)
        (values year month day)))))

(defun break-seconds (total-seconds)
  (multiple-value-bind (minutes seconds)
      (floor total-seconds 60)
    (multiple-value-bind (hours minutes)
        (floor minutes 60)
      (multiple-value-bind (days hours)
          (floor hours 24)
        (values seconds minutes hours days)))))

(defun days-seconds-to-date-time (days-utc seconds-utc &optional (offset 0))
  (multiple-value-bind (days seconds)
      (offset-to-utc days-utc seconds-utc (- 0 offset))
    (multiple-value-bind (year month day)
        (days-to-date days)
      (multiple-value-bind (second minute hour)
          (break-seconds seconds)
        (let ((day-of-week
               (mod (+ +epoch-day-of-week+  days) 7)))

          (make-dto :days-utc days-utc :seconds-utc seconds-utc
                    :year year :month month :day day
                    :hour hour :minute minute :second second
                    :day-of-week day-of-week :offset offset))))))

;;fuzzy (not absolute) duration object
;;normalizes seconds minutes hours and days as one group
;;normalizes months and years as second part
;;this means seconds, minutes and hours stay within normal values
;;months stay within normal values
;;days and years take on arbitrary values
(defun make-duration (seconds minutes hours
                      &optional (days 0) (months 0) (years 0))
  (let* ((total-seconds (+ seconds
                           (* minutes 60)
                           (* hours +seconds-in-hour+))))
    (multiple-value-bind (seconds minutes hours second-days)
        (break-seconds total-seconds)
      (multiple-value-bind (total-days total-months total-years)
          (values (+ days second-days)
                  (mod months 12)
                  (+ years  (floor months 12)))
        (let ((grand-seconds
               (if (and (zerop total-months) (zerop total-years))
                   (dhms-to-total-seconds total-days hours minutes seconds)
                   nil)))
          (make-dur :seconds seconds :minutes minutes :hours hours
                    :days total-days :months total-months :years total-years
                    :total-seconds grand-seconds))))))

(defun dto<% (arg1 arg2)
  (let ((day1 (dto-days-utc arg1))
        (day2 (dto-days-utc arg2)))
    (or (< day1 day2)
        (and (= day1 day2)
             (< (dto-seconds-utc arg1)
                (dto-seconds-utc arg2))))))

(defun dto=% (arg1 arg2)
  (and (= (dto-days-utc arg1) (dto-days-utc arg2))
       (= (dto-seconds-utc arg1) (dto-seconds-utc arg2))))

(defun dto>% (arg1 arg2)
  (dto<% arg2 arg1))


(aas-cl:def-comparison "dto" 'dto dto>% dto<% dto=%)


(defun dhms-to-total-seconds (days hours minutes seconds)
  (+  (* (+ (* (+ (* days 24) hours) 60) minutes) 60) seconds))
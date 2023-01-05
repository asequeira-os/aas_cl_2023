(in-package :aas-local-time)

(defun create-date-time (year month day hour minute second &optional (type +enum-time-type-wall+))
  (make-local-date-time :date (create-date year month day)
                        :time (create-time hour minute second type)))

(defun combine-date-time (date time)
  (make-local-date-time :date date :time time))

(defun date-time-offset (year month day hour minute second offset)
  (make-date-time second minute hour day month year offset))

(defmacro with-timezone (name &body body)
  `(let ((*timezone*
          (get-timezone ,name)))
     (progn
       (when (null *timezone*)
         (error "bad timezone ~A" ,name)) ;;todo 3 double eval fix
       ,@body)))

(defun olsen-data-lookup (name)
  (or (gethash name *timezones*)
      (gethash name *links*)))

(defun now (&optional (zone *timezone*))
  (dto-to-dto (utc-now) (tz-data zone)))

(defmethod day-start ((dto dto) &optional (zone *timezone*))
  (with-slots (year month day) dto
    (dt-to-dto (create-date-time year month day 0 0 0) (tz-data zone))))

(defmethod tomorrow ((dto dto) &optional (zone *timezone*))
  (add-duration dto (make-duration 0 0 24) zone))

(defmethod yesterday ((dto dto) &optional (zone *timezone*))
  (add-duration dto (make-duration 0 0 -24) zone))

(defmethod tomorrow-day-start ((dto dto) &optional (zone *timezone*))
  (day-start (tomorrow dto zone) zone))

(defmethod yesterday-day-start ((dto dto) &optional (zone *timezone*))
  (day-start (yesterday dto zone) zone))

(defmethod first-of-month ((dto dto) &optional (zone *timezone*))
  (with-slots (year month) dto
    (dt-to-dto (create-date-time year month 1 0 0 0) (tz-data zone))))

(defmethod first-of-year ((dto dto) &optional (zone *timezone*))
  (with-slots (year) dto
    (dt-to-dto (create-date-time year 1 1 0 0 0) (tz-data zone))))

(defmethod days-in-this-month ((dto dto))
  (with-slots (year month) dto
    (days-in-month year month)))

(defmethod days-in-this-month ((date date))
  (days-in-month (date-y date) (date-m date)))

(defmethod next-hour ((dto dto) &optional (zone *timezone*))
  (add-duration dto (make-duration 0 0 1) zone))

(defmethod hour-start ((dto dto) &optional (zone *timezone*))
  (dt-to-dto (create-date-time (dto-year dto) (dto-month dto) (dto-day dto)
                               (dto-hour dto)  0 0) (tz-data zone)))

(defmethod prev-hour ((dto dto) &optional (zone *timezone*))
  (add-duration dto (make-duration 0 0 -1) zone))





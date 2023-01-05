(in-package :aas-local-time)

(defun get-tr-data-point-offset (zone dt)
  (with-slots (max-year years) zone
    (let ((year (date-y (slot-value dt 'date) )))
      ;;(break)
      (if (> year max-year)
          (- max-year +epoch-year+)
          (- year +epoch-year+)))))

;;todo 5 add a means of indicating the erroneous value
(define-condition invalid-date-time (error)
  ((reason :initarg :reason
           :reader invalid-date-time-reason))
  (:report (lambda (condition stream)
             (format stream "~%invalid date/time ~A"
                     (invalid-date-time-reason condition)))))

(defmethod dt-to-dto (dt (zone zone-tr-data) )
  (if (null dt)
      nil
      (error "unhandled date time type data ~A" dt)))

(defmethod dt-to-dto ((dt local-date-time) (zone zone-tr-data) )
  (with-slots (date time) dt
    (let ((year (date-y date))
          (month (date-m date))
          (day (date-d date)))
      (let ((hour (tz-time-h time))
            (minute (tz-time-m time))
            (second (tz-time-s time)))
        (let* ((index (get-tr-data-point-offset zone dt))
               (years (slot-value zone 'years))
               (tdp-list (aref years index))
               (first-tdp (first tdp-list)))
          (if (local-date-time< dt (slot-value first-tdp 'start ))
              (let* ((tdp (car (last (aref years (1- index)))))
                     (tr (tr-data-point-tr tdp)))
                (values (make-date-time second minute hour
                                        day month year
                                        (+ (transition-offset tr)
                                           (transition-save tr)))
                        nil))
              (let ((prev-tdp nil))
                (dolist (tdp tdp-list)
                  (with-slots (tr amb-bh start end) tdp
                    (if (local-date-time< dt start)
                        (with-slots (tr) prev-tdp
                          (return-from dt-to-dto
                            (values (make-date-time second minute hour
                                                    day month year
                                                    (+ (transition-offset tr)
                                                       (transition-save tr)))
                                    nil)))
                        (if (and (local-date-time>= dt start)
                                 (local-date-time< dt end))
                            (if (eq amb-bh +blackhole-tr+)
                                (error 'invalid-date-time :reason amb-bh)
                                (return-from dt-to-dto
                                  (values (make-date-time second minute hour
                                                          day month year
                                                          (+ (transition-offset tr)
                                                             (transition-save tr)))
                                          amb-bh)))
                            (setf prev-tdp tdp)))))
                (with-slots (tr) prev-tdp
                  (values (make-date-time second minute hour
                                          day month year
                                          (+ (transition-offset tr)
                                             (transition-save tr)))
                          nil)))))))))

(defmethod dto-to-dto ((dto dto) (zone zone-tr-data))
  (with-slots (years max-year) zone
    (with-slots (days-utc seconds-utc year month day hour minute second
                          day-of-week  offset) dto
      (let* ((index (if (> year max-year)
                        (- max-year +epoch-year+)
                        (- year +epoch-year+)))
             (tdp-list (aref years index))
             (first-tdp (first tdp-list)))
        (with-slots (tr amb-bh start end) first-tdp
          (with-slots (date-time  offset save) tr
            (if (dto< dto date-time)
                (let ((tdp (car (last (aref years (1- index))))))
                  (with-slots (tr amb-bh start end) tdp
                    (return-from dto-to-dto
                      (days-seconds-to-date-time
                       days-utc seconds-utc (+ (transition-offset tr)
                                               (transition-save tr)))))))))
        (let ((prev-tdp first-tdp))
          (dolist (tdp (rest tdp-list))
            (with-slots (tr) tdp
              (with-slots (date-time) tr
                (if (dto< dto  date-time)
                    (with-slots (tr) prev-tdp
                      (return-from dto-to-dto
                        (days-seconds-to-date-time
                         days-utc seconds-utc (+ (transition-offset tr)
                                                 (transition-save tr)))))
                    (setf prev-tdp tdp)))))
          (with-slots (tr) prev-tdp
            (with-slots (date-time) tr
              (return-from dto-to-dto
                (days-seconds-to-date-time
                 days-utc seconds-utc (+ (transition-offset tr)
                                         (transition-save tr)))))))))))


(defun date-from-dto (dto)
  (and dto (create-date (dto-year dto) (dto-month dto) (dto-day dto) nil)))

(defun time-from-dto (dto)
  (and dto (create-time (dto-hour dto) (dto-minute dto) (dto-second dto) +enum-time-type-wall+)))

(defun dt-from-dto (dto)
  (and dto (combine-date-time (date-from-dto dto) (time-from-dto dto))))

(defun dto-from-dt (dt &optional (tz *timezone*))
  (unless tz
    (error "timezone not set"))
  (dt-to-dto dt (tz-data tz)))

(in-package :aas-local-time)

(defstruct local-date-time
  (date nil :type date)
  (time nil :type tz-time))

(defmethod sout-object (ser-type stream type (dt local-date-time))
  (declare (ignorable type))
  (let ((d (local-date-time-date dt))
        (tt (local-date-time-time dt)))
    (aas-rpc:sout-object ser-type stream 'string
                         (format nil "~AT~A"
                                 (date-format-string d)
                                 (tz-time-format-string tt)))))

(aas-rpc:set-deserializer
 'local-date-time
 (lambda (ser-type stream obj-type)
   (let ((ts (aas-rpc::deserialize-string ser-type stream obj-type)))
     (let ((tsar (split-sequence:split-sequence #\T ts)))
       (create-local-date-time (date-parse-string (first tsar))
                               (tz-time-parse-string (second tsar)))))))


(defun create-local-date-time (date time)
  (make-local-date-time :date date :time time))

(defmethod dto-to-dt ((dto dto))
  (with-slots (year  month day  hour  minute  second)
      dto
    (create-local-date-time
     (create-date year month day)
     (make-tz-time :h hour :m minute :s second :usw +enum-time-type-wall+))))

(defmethod total-seconds ((date-time local-date-time))
  (with-slots (h m s)
      (slot-value date-time 'time)
    (+ s (* m 60) (* h 60 60))))

(defun mod-seconds-diff (s1 s2)
  (if (< s1 s2)
      (- (+ s1 +seconds-in-day+) s2)
      (- s1 s2)))

(defmethod add-seconds ((date-time local-date-time) seconds)
  (with-slots (h m s)
      (slot-value date-time 'time)
    (let ((total-seconds (+ seconds
                            s
                            (* m 60)
                            (* h 60 60))))
      (multiple-value-bind
            (seconds minutes hours days)
          (break-seconds total-seconds)
        (create-local-date-time
         (add-days (slot-value date-time 'date) days)
         (make-tz-time :h hours :m minutes :s seconds :usw +enum-time-type-wall+))))))

(defmethod print-object ((time tz-time) stream)
  (with-slots (h m s usw) time
    (let ((type (enum-time-type-letter usw)))
      (if (and (not *print-readably*) (not *print-escape*))
          (format stream "#<~2,'0D:~2,'0D:~2,'0D~C>#" h m s type)
          (call-next-method)))))

(defmethod print-object ((date-time local-date-time) stream)
  (if (and (not *print-readably*) (not *print-escape*))
      (format stream "~A ~A" (slot-value date-time 'date)
              (slot-value date-time 'time))
      (if *print-readably*
          (error "no readable printer for local-date-time yet")
          (call-next-method))))

(defun local-date-time<% (dt1 dt2)
  (with-slots (date time) dt1
    (let ((d1 date)
          (t1 time))
      (with-slots (date time) dt2
        (or (date<% d1 date)
            (and (date=% d1 date)
                 (tz-time<% t1 time)))))))

(defun local-date-time>% (dt1 dt2)
  (with-slots (date time) dt1
    (let ((d1 date)
          (t1 time))
      (with-slots (date time) dt2
        (or (date>% d1 date)
            (and (date=% d1 date)
                 (tz-time>% t1 time)))))))

(defun local-date-time=% (dt1 dt2)
  (and (not (local-date-time<% dt1 dt2))
       (not (local-date-time>% dt1 dt2))))


(def-comparison "local-date-time" 'local-date-time
  local-date-time>% local-date-time<% local-date-time=%)

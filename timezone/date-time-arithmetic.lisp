(in-package :aas-local-time)

(defmethod subtract-duration ((dto dto) (duration dur)
                              &optional(tz *timezone*))
  (let ((neg-dur (make-duration (* -1 (dur-seconds duration))
                                (* -1 (dur-minutes duration))
                                (* -1 (dur-hours duration))
                                (* -1 (dur-days duration))
                                (* -1 (dur-months duration))
                                (* -1 (dur-years duration)))))
    (add-duration dto neg-dur tz)))

(defmethod add-duration ((date-time dto) (duration dur)
                         &optional(tz *timezone*))
  (if (= 0 (dur-months duration) (dur-years duration))
      (add-abs-duration date-time duration (tz-data tz))
      (add-relative-duration date-time duration (tz-data tz))))

(defun add-abs-duration (date-time duration zone-tr-data)
  (or (= 0 (dur-months duration) (dur-years duration))
      (error "invalid call"))
  (let ((seconds (+
                  (dto-seconds-utc date-time)
                  (dur-seconds duration)
                  (* (dur-minutes duration) 60)
                  (* (dur-hours duration) +seconds-in-hour+))))
    (multiple-value-bind (days seconds)
        (floor seconds +seconds-in-day+)
      (let ((days (+
                   (dto-days-utc date-time)
                   days
                   (dur-days duration))))
        (let ((utc-dto (days-seconds-to-date-time days seconds 0)))
          (dto-to-dto utc-dto zone-tr-data))))))

(defun add-relative-duration (date-time duration zone-tr-data)
  (with-slots (seconds minutes hours days months years) duration
    (unless (= 0 seconds minutes hours)
      (error "can not add months/years with any of ~A"
             "days, seconds, minutes or hours"))
    ;;positive we add year, months, then days
    ;;negative we remove days , then months, then years
    ;;this really just ensuring an arbitrary canonical order
    ;;so add-duration and subtract-duration are consistent
    (let* ((order (or (minusp days)
                      (and (zerop days) (minusp months))
                      (and (zerop days) (zerop months) (minusp years))))
           (time (time-from-dto date-time))
           (date (if order
                     (add-days (date-from-dto date-time) days) ;;days added first
                     (date-from-dto date-time))))
      (with-slots (y m d) date
        (multiple-value-bind (add-year nmonth)
            (floor (+ (1- m) months) 12)
          (incf nmonth)
          (let ((nyear (+ y add-year years)))
            (assert (<= 1 nmonth nyear))
            (let ((adjusted-date (create-adjusted-date nyear nmonth d)))
              (let ((ndate (if order
                               adjusted-date ;;days already added
                               (add-days adjusted-date days))))
                (dt-to-dto (combine-date-time ndate time) zone-tr-data)))))))))

(defun create-adjusted-date (y m d)
  (assert (<= d 31))
  (let ((maxd (days-in-month y m)))
    (if (<= d maxd)
        (create-date y m d t)
        (let ((diff (- maxd d)))
          (add-days (create-date y m maxd) diff)))))


(defun date-difference (date-time-1 date-time-2)
  "computes the exact difference (no fuzzy logic)"
  (let ((seconds
         (- (dto-seconds-utc date-time-1) (dto-seconds-utc date-time-2))))
    (multiple-value-bind (days seconds)
        (floor seconds +seconds-in-day+)
      (make-duration
       seconds 0 0
       (+ (- (dto-days-utc date-time-1) (dto-days-utc date-time-2))
          days) 0 0))))

(defun dur<% (arg1 arg2)
  (let ((s1 (dur-total-seconds arg1))
        (s2 (dur-total-seconds arg2)))
    (when (or (null s1) (null s2))
      (error "can not compare durations with months or years"))
    (< s1 s2)))

(defun dur=% (arg1 arg2)
  (equalp arg1 arg2))

(defun dur>% (arg1 arg2)
  (dur<% arg2 arg1))

(aas-cl:def-comparison "duration" 'dur dur>% dur<% dur=%)
;;todo 2 need unit tests for duration comparisons


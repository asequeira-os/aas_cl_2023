(in-package :xed)

;;compatibility check with CL dow since it's in use in time:dto
(assert (zerop (enum-ordinal +enum-weekdays-mon+)))
(assert (= 1 (enum-ordinal +enum-weekdays-tue+)))

;;this is the recurrence definition of a meeting or an alert
(def-rpc-struct recur
    (tz nil :type (or null tz))
  (start nil :type local-date-time)
  (start-dto nil :skip-rpc :type (or null dto)) ;;null for incoming
  (freq nil :type (or null enum-recur-freq))
  (interval nil :type (or null integer)) ;;for daily, weekly, monthly, yearly
  (dow-v nil :type (or null (vector enum-weekdays)));;for weekly
  (dow-v-db nil :skip-rpc)
  (week-num nil :type (or null week-of-month)) ;;for monthly by dow (unless by same day)
  (dow nil :type (or null enum-weekdays)) ;;monthly
  (end nil :type (or null time::local-date-time)) ;;user entered untouchable time
  (end-dto nil :skip-rpc :type (or null dto)) ;;user entered untouchable time in tz
  (max-repeat nil :type (or null integer))
  (last-start-dto nil :skip-rpc :type (or null dto))
  (dur nil :type integer) ;;duration in minutes
  (dead-end nil :skip-rpc :type (or null dto))
  )

(defmethod db-base:get-struct-columns ((symbol (eql 'recur)))
  '(
    (tz tz)
    (start-dto dto)
    (freq (or null enum-recur-freq))
    (interval integer)
    (dow-v-db (or null string))
    (dow (or null enum-weekdays))
    (week-num (or null integer))
    (end-dto (or null dto))
    (max-repeat (or null integer))
    (last-start-dto dto)
    (dur integer)
    (dead-end dto)))

(defun make-recur-from-db (&key tz start-dto  freq interval
                           dow-v-db  dow week-num
                           end-dto  max-repeat last-start-dto dur dead-end)
  (make-recur :tz tz :start (dt-from-dto start-dto) :start-dto start-dto
              :freq freq :interval interval
              :dow-v (enum-vector-from-db 'enum-weekdays dow-v-db) :dow-v-db dow-v-db
              :dow dow :week-num week-num
              :end (dt-from-dto end-dto) :end-dto end-dto :max-repeat max-repeat
              :last-start-dto last-start-dto :dur dur :dead-end dead-end))

(defmethod aas-rpc:deserialize-after ((recur recur))
  (unless (recur-tz recur)
    (setf (recur-tz recur) time:*timezone*))
  (with-timezone (recur-tz recur)
    (let ((start (recur-start recur))
          (end (recur-end recur)))
      (setf (recur-start-dto recur) (dto-from-dt start))
      (when end
        (setf (recur-end-dto recur) (dto-from-dt end)))
      (let ((last-start (recur-compute-last-start recur)))
        (setf (recur-last-start-dto recur) last-start)
        (setf (recur-dead-end recur) (compute-meeting-end last-start (recur-dur recur))))
      (when (eq (recur-freq recur) +enum-recur-freq-weekly+)
        (when (or (null (recur-dow-v recur)) (zerop (length (recur-dow-v recur))))
          (setf (recur-dow-v recur)
                (make-array '(1) :element-type 'enum-weekdays
                            :initial-contents
                            (list (ord-to-weekday (day-of-week (recur-start-dto recur))))))))
      (setf (recur-dow-v recur) (sort (recur-dow-v recur) #'enum<))
      (setf (recur-dow-v-db recur) (enum-vector-to-db 'enum-weekdays (recur-dow-v recur)))
      (validate-recur recur))))

(defun validate-recur (r)
  (assert (not (null (recur-start-dto r))))
  (assert (not (null (recur-start r))))
  (assert (or (null (recur-interval r))(< 0 (recur-interval r))))
  (let ((max-repeat (recur-max-repeat r)))
    (if max-repeat
        (progn
          (assert (< 0 (recur-max-repeat r)))
          (assert (recur-freq r)))))
  (when (recur-dow-v r)
    (assert (recur-dow-v-db r)))
  (when (recur-end r)
    (assert (recur-end-dto r)))
  r)

(defun compute-next-recur-dto  (r now)
  (let ((candidate
         (with-timezone (recur-tz r)
           (extern::rt-case ((recur-freq r) :test #'eq)
             (+enum-recur-freq-daily+ (compute-next-recur-daily r now))
             (+enum-recur-freq-weekly+ (compute-next-recur-weekly r now))
             (+enum-recur-freq-monthly+ (compute-next-recur-monthly r now))
             (+enum-recur-freq-yearly+ (compute-next-recur-yearly r now))
             (nil nil)
             (t (error "unsupported recurrence frequency ~A" (recur-freq r)))))))
    (when (and candidate
               (dto<= candidate (recur-last-start-dto r) ))
      candidate)))

(defun recur-compute-last-start (r)
  (or (if (recur-max-repeat r)
          (let ((c-end
                 (with-timezone (recur-tz r) ;;compute for max repeat case
                   (extern::rt-case ((recur-freq r) :test #'eq)
                     (+enum-recur-freq-daily+ (recur-compute-last-start-daily r ))
                     (+enum-recur-freq-weekly+ (recur-compute-last-start-weekly r ))
                     (+enum-recur-freq-monthly+ (recur-compute-last-start-monthly r ))
                     (+enum-recur-freq-yearly+ (recur-compute-last-start-yearly r ))
                     (nil (error "please set recur frequency when setting max repeats"))
                     (t (error "unsupported recurrence frequency ~A" (recur-freq r)))
                     ))))
            (earlier-dto (recur-end-dto r) c-end))
          (if (recur-freq r) ;;max-repeat is nil
              (recur-end-dto r)  ;;freq is set => repetition stopped by end if any
              (recur-start-dto r)) ;;no repeat, no freq => one shot
          )
      +max-dto+))

;;expected to be called when end has already been computed and set
(defun recur-compute-in-range (r start-dto end-dto)
  "calculate all recurrences within range
start inclusive, end exclusive"
  (assert (and start-dto end-dto))
  (assert (dto< start-dto end-dto))
  (let* ((last-start-dto (or (recur-last-start-dto r) +max-dto+))
         (exhausted (dto<= last-start-dto end-dto)))
    (when (dto<= last-start-dto start-dto)
      (return-from recur-compute-in-range (values nil exhausted)))
    (let ((last-recur (add-duration  start-dto (make-duration -1 0 0)))
          (list nil))
      (util:while (dto< last-recur end-dto)
        (let ((candidate (compute-next-recur-dto r last-recur)))
          (if (and candidate (dto< candidate end-dto))
              (progn
                (setf last-recur candidate)
                (push candidate list))
              (return-from recur-compute-in-range
                (values (nreverse list) exhausted))))))))

;;implementation details
(defun compute-next-recur-daily (r now)
  (let* ((time (time-from-dto (recur-start-dto r)))
         (date (date-from-dto now))
         (base (dto-from-dt (combine-date-time date time)))
         (interval (or (recur-interval r) 1)))
    (if (= 1 interval)
        (if (dto< now base)
            base
            (dto-from-dt (combine-date-time (add-days date 1) time)))
        (dto-from-dt (combine-date-time (add-days date interval) time)))))


(defun compute-next-recur-weekly (r now)
  (let* ((date (date-from-dto now))
         (time (time-from-dto (recur-start-dto r)))
         (dow-v (recur-dow-v r))
         (interval (or (recur-interval r) 1))
         (b-dow-num (day-of-week now))
         (b-dow (make-enum-weekdays-from-db :elm b-dow-num)))
    (loop for dow across dow-v do
         (when (enum< b-dow dow)
           (let ((diff-days (- (extern::enum-elm-ord (extern::enum-element dow))
                               b-dow-num)))
             (return-from compute-next-recur-weekly
               (dto-from-dt (combine-date-time (add-days date diff-days) time))))))
    (let ((dow-diff (- b-dow-num
                       (extern::enum-elm-ord (extern::enum-element (aref dow-v 0))))))
      (let ((diff-days (- (* 7 interval) dow-diff)))
        (dto-from-dt (combine-date-time (add-days date diff-days) time))))))

;;monthly repeat process
;; interval,
;; same date or
;;   on 1st,2nd,3rd,4th, last
;;      sun,...,sat
(defun compute-next-recur-monthly (r now)
  (let* ( (now-m (dto-month now))
         (now-y (dto-year now))
          (time (time-from-dto (recur-start-dto r)))
          (dow (recur-dow r)))
    (if (null dow) ;;on same day
        (compute-next-recur-monthly-day r now)
        (let* ((guess-date (dow-of-month (enum-ordinal dow)
                                         (enum-ordinal (recur-week-num r))
                                         now-m now-y) )
               (guess-dto (dto-from-dt (combine-date-time guess-date time))))
          (if (dto< now guess-dto)
              guess-dto
              (let* ((next-month (start-of-next-month now-y now-m))
                     (guess-date2 (dow-of-month (enum-ordinal dow)
                                                (enum-ordinal (recur-week-num r))
                                                (date-m  next-month)
                                                (date-y next-month))))
                (dto-from-dt (combine-date-time guess-date2 time))))))))

(defun compute-next-recur-monthly-day (r now)
  (let* ((day (dto-day (recur-start-dto r) ))
         (now-m (dto-month now))
         (now-y (dto-year now))
         (time (time-from-dto (recur-start-dto r))))
    (let* ((guess-date (create-date now-y now-m
                                    (min day (days-in-month now-y now-m))))
           (guess-dto (dto-from-dt (combine-date-time guess-date time))))
      (if (dto< now guess-dto)
          guess-dto
          (let* ((next-month (start-of-next-month now-y now-m))
                 (y (date-y next-month))
                 (m (date-m next-month))
                 (guess-date2 (create-date y m (min day (days-in-month y m))))
                 (guess-dto (dto-from-dt (combine-date-time guess-date2 time))))
            guess-dto)))))

(defun compute-next-recur-yearly (r now)
  (let* ((day (dto-day (recur-start-dto r) ))
         (m (dto-month (recur-start-dto r)))
         (now-y (dto-year now))
         (time (time-from-dto (recur-start-dto r))))
    (let* ((guess-date (create-date now-y m
                                    (min day (days-in-month now-y m))))
           (guess-dto (dto-from-dt (combine-date-time guess-date time))))
      (if (dto< now guess-dto)
          guess-dto
          (let* ((y2 (1+ now-y))
                 (date (create-date y2  m (min day (days-in-month y2 m)))))
            (dto-from-dt (combine-date-time date time)))))))

;;nil is considered infinity
(defun earlier-dto (dto1 dto2)
  (if (and dto1 dto2)
      (if (dto< dto1 dto2)
          dto1
          dto2)
      (if dto1
          dto1
          dto2)))

(defun recur-compute-last-start-daily (r)
  (let* ((start (recur-start-dto r))
         (date (date-from-dto start))
         (interval (or (recur-interval r) 1))
         (repeats (recur-max-repeat r))
         (total-days (* repeats interval))
         (end-date (add-days date total-days)))
    ;;todo 2 deal with amb/bh
    (dto-from-dt (combine-date-time end-date (time-from-dto start)))))

(defun recur-compute-last-start-weekly (r)
  (let* ((start (recur-start-dto r))
         (date (date-from-dto start))
         (interval (or (recur-interval r) 1))
         (repeats (recur-max-repeat r))
         (total-days (* repeats 7 interval))
         (end-date (add-days date total-days)))
    ;;todo 2 deal with amb/bh
    (dto-from-dt (combine-date-time end-date (time-from-dto start)))))

(defun recur-compute-last-start-monthly (r)
  (let* ((start (recur-start-dto r))
         (interval (or (recur-interval r) 1))
         (repeats (recur-max-repeat r))
         (total-months (* repeats interval)))
    (let ((dur (make-duration 0 0 0 0 total-months)))
      (add-duration start dur))))

(defun recur-compute-last-start-yearly (r)
  (let* ((start (recur-start-dto r))
         (interval (or (recur-interval r) 1))
         (repeats (recur-max-repeat r))
         (total-years (* repeats interval)))
    (if (> total-years 50)
        nil ;;too long
        (let ((dur (make-duration 0 0 0 0 0 total-years)))
          (add-duration start dur)))))

(defun compute-meeting-end (start minutes)
  (add-duration start (make-duration 0 minutes 0)))

;;alarm defaults

;;if both are null no alarming by default
(def-rpc-struct alarm-info
    (dur1 nil :type (or null dur))
  (dur2 nil :type (or null dur)))

(defmethod db-base:get-struct-columns ((symbol (eql 'alarm-info)))
  '((dur1 (or null time:dur))
    (dur2 (or null time:dur))))

(defun make-alarm-info-from-db (&key dur1 dur2)
  (make-alarm-info :dur1 dur1 :dur2 dur2))

;;general description struct
;;todo 3 I could add attachments to this if I develop a file storage
;;todo 3 add a timestamp field , make concept of updating timestamsp automatically
;;if fiedls rae seen different at the db layer
(def-rpc-struct description
    (title nil :type (or null string))
  (notes nil :type (or null string)))

(defmethod db-base:get-struct-columns ((symbol (eql 'description)))
  '((title (or null string))
    (notes (or null string))))

(defun make-description-from-db (&key title notes)
  (make-description :title title :notes notes))


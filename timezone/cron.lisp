(in-package :aas-local-time)

(defstruct cron-entry
  (key nil :type symbol)
  (fn )
  (minute nil :type '(or null (integer 0 59)))
  (hour nil :type '(or nil (integer 0 23)))
  (day nil :type '(or nil (integer 1 31)))
  (month nil :type '(or nil (integer 1 12)))
  ;;todo: 1 verify dow matches my stuff
  (dow nil :type '(or nil (integer 0 6))))

(defstruct cron-entry-at
  (entry nil :type cron-entry)
  (at nil :type dto))

(defstruct cron-entries
  (lock (mp-make-lock))
  (min-at nil :type (or null dto))
  (list))

(defvar *cron-entries* (make-cron-entries))

(defun cron-set-entry (symbol function minute hour
                       &optional day month dow)
  (cron-set-entry% (make-cron-entry :key symbol :fn function :minute minute
                                    :hour hour :day day :month month :dow dow)))

(defun cron-set-entry% (entry )
  (let ((entry-at (make-cron-entry-at :entry entry
                                      :at (cron-next-at entry (utc-now)))))
    (mp-with-lock ((cron-entries-lock *cron-entries*))
      (setf (cron-entries-list *cron-entries*)
            (sort (list* entry-at
                         (remove (cron-entry-key entry)
                                 (cron-entries-list *cron-entries*)
                                 :key (lambda (ea)
                                        (cron-entry-key (cron-entry-at-entry ea)))
                                 :test #'eq))
                  #'dto<
                  :key #'cron-entry-at-at))
      (setf (cron-entries-min-at *cron-entries*)
            (cron-entry-at-at (first (cron-entries-list *cron-entries*)))))))

(defun cron-next-at (entry dto)
  (with-timezone +timezone-utc+
    (let ((dow (cron-entry-dow entry))
          (month (cron-entry-month entry))
          (day (cron-entry-day entry)))
      (when (or dow month day)
        (error "dow/month/day cron computations are not yet supported"))
      (let ((hour (cron-entry-hour entry))
            (minute (cron-entry-minute entry)))
        (if (not (null hour))
            (cron-next-at-hour dto hour minute)
            (cron-next-at-minute dto minute))))))

(defun cron-next-at-hour (dto hour minute)
  (let ((first-guess (make-date-time 0 minute hour (dto-day dto) (dto-month dto)
                                     (dto-year dto) (dto-offset dto))))
    (if (or (> hour (dto-hour dto))
            (and (= hour (dto-hour dto))
                 (> minute (dto-minute dto))))
        first-guess
        (add-duration first-guess (make-duration 0 0 24) +timezone-utc+))))

(defun cron-next-at-minute (dto minute)
  (let ((first-guess (make-date-time 0 minute (dto-hour dto) (dto-day dto)
                                     (dto-month dto) (dto-year dto) 0)))
    (if (> minute (dto-minute dto))
        first-guess
        (add-duration first-guess (make-duration 0 0 1) +timezone-utc+))))

(defun cron-process-entries ()
  (let ((now (utc-now))
        (min-at (cron-entries-min-at *cron-entries*)))
    (when (and min-at (dto>= now min-at))
      (let ((jobs (list)))
        (mp-with-lock ((cron-entries-lock *cron-entries*))
          (dolist (job (cron-entries-list *cron-entries*))
            (when (dto>= now (cron-entry-at-at job))
              (push job jobs))))
        (dolist (job jobs)
          (cron-set-entry% (cron-entry-at-entry job))
          (log:with-logged-error
            (funcall (cron-entry-fn (cron-entry-at-entry job)))))))))

(defvar *cron-thread*
  (mp-make-thread "cron"
                  (lambda ()
                    (while t
                      (sleep 60)
                      (cron-process-entries)))
                  (list (cons '*cron-entries* *cron-entries*))))



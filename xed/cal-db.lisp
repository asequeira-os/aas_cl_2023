(in-package :xed)

(def-db-table calendars
    "master list of calendars"
  +xeduler+
  ((c calendar)
   (uid (or null auth:user-id))
   (sg (or null db-id))) ;;servicing group for paying calendar
  (((c id db)) ((c id id)))
  (;;postgres allows unique index on null able columns
   (:unique byuser ((uid id db) :ascending) ((uid id id) :ascending)) )
  (:vdb-sequence (c id db) (c id id)))

(def-db-table serviced-cals
    "list of serviced calendars by a user"
  +xeduler+
  ((uid auth:user-id)
   (cid db-id))
  (((cid db)) ((cid id)) ((uid id db)) ((uid id id)))
  ((:not-unique byuser ((uid id db) :ascending) ((uid id id) :ascending)))
  ())

(def-db-table cal-shift
    "current valid shift for any paying calendar"
  +xeduler+
  ((cid db-id)
   (shift work-shift))
  (((shift id db) :ascending) ((shift id id) :ascending))
  ((:not-unique bycal ((cid db) :ascending) ((cid id) :ascending)))
  (:vdb-sequence (shift id db) (shift id id)))

(def-db-table cal-holidays
    "list of holidays for a calendar"
  +xeduler+
  ((cid db-id)
   (hd date))
  (((cid db) :ascending) ((cid id) :ascending) ((hd day-num) :ascending))
  nil
  nil
  ((range (and (= cdb (cid db))
               (= cid (cid id))
               (<= start (hd day-num))
               (> end (hd day-num))))))

(define-constant +service+ "xed:service")

;;todo 1 'servicing' flag if set needs to ensure user has cc info
(def-rpc-with-proxy calendar create-calendar (:application +xeduler+)
    (calendar calendar)
    (auth:auth-to-ext-vdb)
  (db-with-transaction ('create-calendar)
    (assert (null (calendar-id calendar)))
    (let ((servicing (calendar-servicing calendar)))
      (let ((group
             (when servicing
               (auth::group-create
                :name (format nil "calendar servicing users ~A"
                              (calendar-name calendar))))))
        (setf (calendar-id calendar)
              (make-db-id :db *vdb* :id nil))
        (let ((row (db-insert-row
                    (make-db-table-calendars-row calendar
                                                 (unless servicing
                                                   (auth:auth-token-user-id))
                                                 (when group
                                                   (auth::group-id group))))))
          (let ((calendar (db-table-calendars-row-c row)))
            (auth:acl-user-add-right calendar auth:+admin-right+ t)
            (if servicing
                (auth:acl-group-add-right calendar +service+ group)
                (auth:acl-user-add-right calendar +service+ t))
            calendar))))))

(defun calendar-master-rec (id)
  (db-calendars-select-pk (db-id-db id) (db-id-id id)))

(defun calendar-get (id)
  (let ((row (calendar-master-rec id)))
    (when row
      (db-table-calendars-row-c row))))

(defun check-calendar-service-right (id)
  (let ((calendar (calendar-get id)))
    (assert calendar)
    (assert (calendar-servicing calendar) nil "not a servicing calendar")
    (auth:acl-user-check-right calendar xed::+service+)))

(def-rpc-with-proxy boolean serviced-cals-upsert
    (:application +xeduler+ :host-to-host-only t)
    (cal-id db-id user auth:user)
    (auth:user-to-ext-vdb user)
  (let ((user-id (auth:user-id user)))
    (let ((row (db-insert-row (make-db-table-serviced-cals-row user-id cal-id))))
      (assert row)
      t)))

(def-rpc-with-proxy boolean serviced-cals-delete
    (:application +xeduler+ :host-to-host-only t)
    (cal-id db-id user auth:user)
    (auth:user-to-ext-vdb user)
  (let ((user-id (auth:user-id user)))
    (let ((row (db-serviced_cals-select-pk (db-id-db cal-id) (db-id-id cal-id)
                                           (auth:user-db-num user-id)
                                           (auth:user-id-num user-id))))
      (assert row)
      (and row
           (db-delete-row row)))))

(def-rpc-with-proxy boolean add-service-user (:application +xeduler+)
    (calendar calendar user auth:user)
    (db-id-db (calendar-id calendar))
  (auth:acl-user-check-right calendar auth:+admin-right+)
  (let ((master (calendar-master-rec (calendar-id calendar))))
    (assert (db-table-calendars-row-sg master))
    (and (auth:acl-group-add-user
          (auth::make-group :id (db-table-calendars-row-sg master) :name "fake")
          (auth::user-login user))
         (serviced-cals-upsert :cal-id (calendar-id calendar)
                               :user user)
         t)))

;;returns list of calendar ids
;;see find-customer-calendar rpc
(def-rpc-with-proxy (vector db-id) serviced-cals-list
    (:application +xeduler+)
    ()
    (auth:auth-to-ext-vdb)
  (multiple-value-bind (udb uid)
      (auth:auth-token-user)
    (let ((rows (db-serviced_cals-select-byuser udb uid)))
      (when rows
        (coerce (mapcar #'db-table-serviced-cals-row-cid rows) 'vector)))))

(def-rpc-with-proxy (or null calendar) get-user-calendar
    (:application +xeduler+)
    ()
    (auth:auth-to-ext-vdb)
  (multiple-value-bind (udb uid)
      (auth:auth-token-user)
    (get-calendar-by-user-db-id udb uid)))

(def-rpc (or null calendar) get-calendar-by-login
    (:application +xeduler+ :host-to-host-only t)
    (login string)
  (let ((user (auth:find-user :login login)))
    (when user
      (get-calendar-by-user :user user))))

(def-rpc-with-proxy (or null calendar) get-calendar-by-user
    (:application +xeduler+ :host-to-host-only t)
    (user auth:user)
    (auth:user-to-ext-vdb user)
  (get-calendar-by-user-db-id (auth:user-db-num user) (auth:user-id-num user)))

(defun get-calendar-by-user-db-id (db id)
  ;;you need to be in the right db before calling this
  (let ((row (db-calendars-select-byuser db id)))
    (when row
      (db-table-calendars-row-c row))))

(def-rpc-with-proxy boolean list-service-users (:application +xeduler+)
    (calendar calendar)
    (db-id-db (calendar-id calendar))
  (auth:acl-user-check-right calendar auth:+admin-right+)
  (let ((master (calendar-master-rec (calendar-id calendar))))
    (auth::group-list-users :group (auth::make-group :id (db-table-calendars-row-sg master) :name "fake") :offset 0 :maxrows 100)))

(def-rpc-with-proxy boolean remove-service-user (:application +xeduler+)
    (calendar calendar user auth:user)
    (db-id-db (calendar-id calendar))
  (auth:acl-user-check-right calendar auth:+admin-right+)
  (let ((master (calendar-master-rec (calendar-id calendar))))
    (auth::group-remove-user :group (auth::make-group :id (db-table-calendars-row-sg master) :name "fake") :user user )
    (serviced-cals-delete :cal-id (calendar-id calendar)
                          :user user)
    t))

(def-rpc-with-proxy work-shift add-work-shift (:application +xeduler+)
    (cal-id db-id shift work-shift)
    (db-id-db cal-id)
  (check-calendar-service-right cal-id)
  (let ((master (calendar-master-rec cal-id)))
    (unless master
      (error "invalid calendar id"))
    (let ((calendar (db-table-calendars-row-c master)))
      (auth:acl-user-check-right calendar xed::+service+)
      (assert (null (work-shift-id shift)))
      (let ((list (list-work-shift :cal-id cal-id)))
        (apply #'shift-check-overlap
               (sort (list* shift (coerce list 'list)) #'work-shift< ))
        (setf (work-shift-id shift) (make-db-id :db *vdb*))
        (let ((row (db-insert-row (make-db-table-cal-shift-row cal-id shift))))
          (db-table-cal-shift-row-shift row))))))

(def-rpc-with-proxy (vector work-shift) list-work-shift (:application +xeduler+)
    (cal-id db-id)
    (db-id-db cal-id)
  (let ((master (calendar-master-rec cal-id)))
    (unless master
      (error "invalid calendar id"))
    (let ((calendar (db-table-calendars-row-c master)))
      (auth:acl-user-check-right calendar xed::+service+)
      (let ((rows (db-cal_shift-select-bycal *vdb* (db-id-id cal-id))))
        (when rows
          (coerce (sort (mapcar #'db-table-cal-shift-row-shift rows)
                        #'work-shift<)
                  'vector))))))

(def-rpc-with-proxy (vector work-shift) delete-work-shift (:application +xeduler+)
    (cal-id db-id shift work-shift)
    (db-id-db cal-id)
  (check-calendar-service-right cal-id)
  (let ((shifts (list-work-shift :cal-id cal-id)))
    (unless (and (<= 1 (length shifts))
                 (find shift shifts :test #'equalp))
      (error "invalid shift"))
    (let ((id (work-shift-id shift)))
      (db-with-transaction ('delete-work-shift)
        (db-delete-row (db-cal_shift-select-pk (db-id-db id) (db-id-id id)))
        t))))
;;no update-work-shift - use delete followed by add

(def-rpc-with-proxy boolean upsert-holiday (:application +xeduler+)
    (cal-id db-id hd date)
    (db-id-db cal-id)
  (check-calendar-service-right cal-id)
  (let ((old-row (db-cal_holidays-select-pk *vdb* (db-id-id cal-id) (date-day-num hd))))
    (when old-row
      (db-delete-row old-row))
    (db-insert-row (make-db-table-cal-holidays-row cal-id hd))
    t))

(def-rpc-with-proxy boolean delete-holiday (:application +xeduler+)
    (cal-id db-id hd date)
    (db-id-db cal-id)
  (check-calendar-service-right cal-id)
  (let ((old-row (db-cal_holidays-select-pk *vdb* (db-id-id cal-id) (date-day-num hd))))
    (when old-row
      (db-delete-row old-row))))

(def-rpc-with-proxy (vector date) list-holiday (:application +xeduler+)
    (cal-id db-id year integer)
    (db-id-db cal-id)
  (check-calendar-service-right cal-id)
  (let ((start (create-date year 1 1))
        (end (create-date (1+ year) 1 1)))
    (let ((rows (db-cal_holidays-select-range *vdb* (db-id-id cal-id) (date-day-num start) (date-day-num end))))
      (when rows
        (coerce (mapcar #'db-table-cal-holidays-row-hd rows)
                'vector)))))


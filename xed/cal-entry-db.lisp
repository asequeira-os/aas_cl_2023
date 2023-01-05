(in-package :xed)

;;appt => appointment
;;pcid => primary calendar id
;;cid => calendar id
;;aid => appointment id

;;one record in each participating calendar's db
;;the recur and description objects will be identical copies
(def-db-table appt-master
    "all appointment definitions"
  +xeduler+
  ((aid db-id)   ;;the db here will match the db in pcid
   (pcid db-id) ;;primary calendar id for this meeting
   (cid db-id)  ;;calendar id (the db here is the vdb)
   (r recur)
   (d description)
   (status enum-participation))
  (((cid db)) ((cid id))  ((aid db)) ((aid id)))
  (
   ;; (:not-unique bycalrecur ((cid db) :ascending) ((cid id) :ascending)
   ;;              ((r start-dto dbdt) :descending)
   ;;              ((r dead-end dbdt) :ascending))
   ;;(:not-unique bycal ((cid db)) ((cid id)) ((aid db)) ((aid id)))
   )
  (:vdb-sequence (aid db) (aid id))
  ((range (and (= cdb (cid db))
               (= cid (cid id))
               (> end (r start-dto dbdt))
               (< start (r dead-end dbdt))))))

;;one record each for every involved calendar in the primary calendar db
(def-db-table appt-calendars
    "all calendars involved in a appt"
  +xeduler+
  ((aid db-id)
   (cid db-id))
  (((aid db)) ((aid id)) ((cid db)) ((cid id)))
  ((:not-unique bycal ((cid db)) ((cid id))))
  nil
  nil)

;;recur next entries should be done only if there are alarms
(def-db-table recur-next
    "immediate recur instances"
  +xeduler+
  ((cid db-id) ;;calendar id
   (aid db-id) ;;appt id
   (seq integer)
   (start dto)
   (end dto))
  (((cid db)) ((cid id)) ((aid db)) ((aid id))) ;;seq is not here
  ((:not-unique bystart ((start dbdt) :descending)))
  nil
  ((range (<= start (start dbdt))))
  )

(def-db-table alarm-default
    "default set ups for alarms"
  +xeduler+
  ((cid db-id)
   (def alarm-info)
   )
  (((cid db)) ((cid id)))
  nil)

(def-db-table alarm-master
    "alarms definitions"
  +xeduler+
  ((cid db-id) ;;calendar id
   (aid db-id) ;;appt id
   (seq integer)
   (ai alarm-info)
   )
  (((cid db)) ((cid id)) ((aid db)) ((aid id)) seq)
  nil
  nil
  nil)

(def-db-table alarms
    "active alarms"
  +xeduler+
  ((cid db-id) ;;calendar id
   (aid db-id) ;;appt id
   (seq integer)
   (at dto)
   (fired boolean)
   )
  (((cid db)) ((cid id)) ((aid db)) ((aid id)) seq)
  ((:not-unique byat  ((at dbdt) :ascending :range) ))
  nil
  nil)

;;for servicing calendar this will be taken as default to be set into
;;any non servicing calendar created as a result of creating an appt in this
(def-rpc-with-proxy alarm-info set-alarm-info-default (:application +xeduler+)
    (calendar calendar alarm-info alarm-info)
    (db-id-db (calendar-id calendar))
  (auth:acl-user-check-right calendar auth:+admin-right+)
  (let*  ((cal-id (calendar-id calendar))
          (exist (db-alarm_default-select-pk (db-id-db cal-id) (db-id-id cal-id))))
    (when exist
      (db-delete-row exist))
    (db-insert-row (make-db-table-alarm-default-row cal-id alarm-info))
    alarm-info))

(def-rpc-with-proxy (or null alarm-info) get-alarm-info-default (:application +xeduler+)
    (calendar calendar)
    (db-id-db (calendar-id calendar))
  (let*  ((cal-id (calendar-id calendar))
          (exist (db-alarm_default-select-pk (db-id-db cal-id) (db-id-id cal-id))))
    (and exist
         (db-table-alarm-default-row-def exist))))

;; (defun fooo ()
;;   (let* ((table *db-table-appt-master*)
;;          (db-base::*vdb-sequencer* (let ((counter 0))
;;                                      (lambda (table vdb)
;;                                        (declare (ignorable table vdb))
;;                                        (incf counter)))))
;;     (db-with-conn (*db* (db-base-test::test-spec 1) #'db-base-test::test-db-ctor)
;;       (db-with-transaction ('xxx)
;;         (let ((db-base::*drop-table-ignore-no-exist* t))
;;           (db-base::drop-table table ))
;;         (db-base::create-table table)
;;         (db-recur_master-select-range 1 2 3 1)))))

;; (fooo)


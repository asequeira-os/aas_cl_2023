(in-package :aas-local-time)

(defvar *timezone-numeric-ids* (make-hash-table :test #'equal))
(defvar *timezone-numeric-ids-reverse* (make-hash-table :test #'eql))
(defvar *xed-zones* (make-hash-table :test #'eql))

(defun set-tz-id (name id)
  "name should be a valid olsen tz name (either Zone or Link).
id is the id to be associated with it for perpetuity"
  (let ((oldid (gethash name *timezone-numeric-ids*)))
    (when (and oldid (not (= id oldid)))
      (error "changing ~A from ~A to ~A not allowed" name oldid id)))
  (let ((oldname (gethash id *timezone-numeric-ids-reverse*)))
    (when (and oldname (not (equalp oldname name)))
      (error "~A is used by ~A"
             id (gethash id *timezone-numeric-ids-reverse*))))
  (setf (gethash name *timezone-numeric-ids*) id)
  (setf (gethash id *timezone-numeric-ids-reverse*) name))

(defun tz-name-to-id (name)
  (gethash name *timezone-numeric-ids*))

(defun tz-id-to-name (id)
  (gethash id *timezone-numeric-ids-reverse*))

(def-rpc-struct tz
    (id nil :type integer)
  (name nil :type string)
  (data :skip-rpc nil :type (or null zone-tr-data)))

(defun map-xed-zones ()
  (maphash (lambda (id name)
             (let ((tr-data (olsen-data-lookup name)))
               (unless tr-data
                 (error "~A is not valid zone/link name" name))
               (setf (gethash id *xed-zones*)
                     (make-tz :id id :name name :data tr-data))))
           *timezone-numeric-ids-reverse*)
  t)

(defmethod get-timezone ((tz tz))
  tz)

(defmethod get-timezone ((name string))
  (gethash (gethash name *timezone-numeric-ids*) *xed-zones*))

;;rpc support for zone
(defmethod deserialize-after ((object tz))
  (let ((tz2 (gethash (tz-id object) *xed-zones*)))
    (unless tz2
      (error "invalid timezone object ~A" object))
    (unless (equal (tz-name object) (tz-name tz2))
      (error "timezone name mismatch"))
    tz2))

;;db storage for zone
(defmethod db-base:get-struct-columns ((symbol (eql 'tz)))
  '((id integer)
    ))

(defun make-tz-from-db (&key id)
  (gethash id *xed-zones*))


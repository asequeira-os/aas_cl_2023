(in-package :fin-cc)

(def-db-table cc-user
    "cc user profile"
  +fin-cc+
  ((auth-db-num integer)
   (userid integer)
   (confirmed boolean)
   (active boolean)
   (profile cc-profile))
  (((profile dbid db)) ((profile dbid id)))
  ((:not-unique user-id auth-db-num userid))
  (:vdb-sequence  (profile dbid db) (profile dbid id)))

(def-rpc-struct cc-profile-resp
    (accepted nil :type boolean)
  (message nil :type string)
  (cc-profile  nil :type cc-profile))

;;todo 3 cc profile country field needs to be structured?
(def-rpc-with-proxy cc-profile-resp create-cc-user-profile
    (:allow-inactive t :application +fin-cc+)
    (cc-profile cc-profile ccv string)
    (auth:auth-to-ext-vdb)
  (multiple-value-bind (auth-db-num userid)
      (auth:auth-token-user)
    (setf (cc-profile-dbid cc-profile) (make-db-id :db *vdb*))
    (validate-cc-profile-fields cc-profile ccv)
    (let ((row (make-db-table-cc-user-row auth-db-num userid nil nil cc-profile)))
      (db-with-transaction ('create-cc-user-profile)
        (let* ((db-row (db-insert-row row))
               (cc-profile (db-table-cc-user-row-profile db-row))
               (gw-tx-response (authorize-cc  cc-profile ccv 1)))
          (when (gw-tx-response-approved gw-tx-response)
            (assert (gw-tx-response-approved (void-cc gw-tx-response)))
            (setf (db-table-cc-user-row-active db-row) t)
            (setf (db-table-cc-user-row-confirmed db-row) t)
            (db-update-row db-row))
          (make-cc-profile-resp
           :accepted (gw-tx-response-approved gw-tx-response)
           :message (gw-tx-response-reason-text gw-tx-response)
           :cc-profile cc-profile))))))

(def-rpc-with-proxy (vector cc-profile) get-user-cc-list (:application +fin-cc+)
    (active-only boolean)
    (auth:auth-to-ext-vdb)
  (multiple-value-bind (auth-db-num userid)
      (auth:auth-token-user)
    (let ((db-rows (db-cc_user-select-user-id auth-db-num userid)))
      (sort (coerce
             (mapcar (lambda (row)
                       (db-table-cc-user-row-profile row))
                     (remove-if (lambda (row)
                                  (when active-only
                                    (not (db-table-cc-user-row-active row))))
                                db-rows))
             'vector)
            #'< :key (lambda (cc-profile)
                       (db-id-id (cc-profile-dbid cc-profile)))))))

(def-rpc-with-proxy cc-profile-resp update-cc-user-profile (:application +fin-cc+)
    (cc-profile cc-profile ccv string)
    (auth:auth-to-ext-vdb)
  (db-with-transaction ('update-cc-user-profile)
    (delete-cc-user-profile :cc-profile cc-profile)
    (setf (cc-profile-dbid cc-profile) (make-db-id :db *vdb*))
    (create-cc-user-profile :cc-profile cc-profile :ccv ccv)))

(def-rpc-with-proxy cc-profile delete-cc-user-profile (:application +fin-cc+)
    (cc-profile cc-profile)
    (auth:auth-to-ext-vdb)
  ;;just mark deleted
  (multiple-value-bind (auth-db-num userid)
      (auth:auth-token-user)
    (let ((dbid (cc-profile-dbid cc-profile)))
      (let ((cc-user (db-cc_user-select-pk (db-id-db dbid) (db-id-id dbid))))
        (unless cc-user
          (error "no cc profile record"))
        (unless (and (= auth-db-num (db-table-cc-user-row-auth-db-num cc-user))
                     (= userid (db-table-cc-user-row-userid cc-user)))
          (error "authentication mismatch"))
        (setf (db-table-cc-user-row-active cc-user) nil)
        (db-table-cc-user-row-profile (db-update-row cc-user))))))


(defun validate-cc-profile-fields (cc-profile ccv)
  (declare (ignorable ccv))
  (validate-cc-info cc-profile ccv)
  t)

(defun validate-cc-info (profile ccv)
  "validate individual fields locally"
  (or
   (validate-cc-num (cc-profile-cc-vendor profile)
                    (cc-profile-cc-num profile)
                    (cc-profile-cc-exp-mm profile)
                    (cc-profile-cc-exp-year profile)
                    ccv)
   (error "invalid credit card info")))

(def-db-table gw-tx
    "cc transaction request/response"
  +fin-cc+
  ((auth-db-num integer)
   (userid integer)
   (r gw-tx-response)
   (voided boolean))
  (((r dbid db) :ascending) ((r dbid id) :ascending))
  ((:not-unique user-id auth-db-num userid))
  (:vdb-sequence (r dbid db)  (r dbid id)))

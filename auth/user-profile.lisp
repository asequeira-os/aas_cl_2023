(in-package :auth)

(def-db-table user-db-profile
    "user basic info"
  +auth+
  ((userid user-id)
   (profile user-profile))
  (((userid id db) :ascending) ((userid id id) :ascending))
  nil)

(def-db-table US-address
    "US address"
  +auth+
  ((userid user-id)
   ;;todo 2 have address type
   (adrs geo-US:US-address))
  (((userid id db) :ascending) ((userid id id) :ascending))
  nil)

(defun validate-user-profile (user-profile)
  "todo 1 validate user profile"
  user-profile)

(def-rpc-with-proxy user-profile create-user-profile
    (:post-only nil :application +auth+)
    (user-profile user-profile)
    (auth-token-db-num)
  (db-with-transaction ('create-user-profile)
    (let ((userid (auth-token-user-id)))
      (when (db-user_db_profile-select-pk (user-db-num userid) (user-id-num userid))
        (error "profile already exists"))
      (let ((user-profile (validate-user-profile user-profile)))
        (db-insert-row (make-db-table-user-db-profile-row userid user-profile))
        user-profile))))

(def-rpc-with-proxy user-profile update-user-profile
    (:post-only nil :application +auth+)
    (user-profile user-profile)
    (auth-token-db-num)
  (db-with-transaction ('update-user-profile)
    (let ((userid (auth-token-user-id)))
      (let ((db-row (db-user_db_profile-select-pk
                     (user-db-num userid) (user-id-num userid))))
        (unless db-row
          (error "profile does not exist"))
        (let ((user-profile (validate-user-profile user-profile)))
          (setf (db-table-user-db-profile-row-profile db-row) user-profile)
          (db-update-row db-row)
          user-profile)))))

(def-rpc-with-proxy (or null user-profile) get-user-profile
    (:post-only nil :application +auth+)
    ()
    (auth-token-db-num)
  (let ((userid (auth-token-user-id)))
    (let ((db-row (db-user_db_profile-select-pk
                   (user-db-num userid) (user-id-num userid))))
      (if db-row
          (db-table-user-db-profile-row-profile db-row)
          nil))))

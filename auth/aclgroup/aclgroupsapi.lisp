(in-package :auth)

;;todo 1 only paying users should be able to create groups
(def-rpc-with-proxy group group-create (:application +auth+)
    (name string)
    (auth-token-db-num)
  (group-create-impl :group-db-num *vdb* :name name))

(def-rpc-with-proxy group group-create-impl
    (:host-to-host-only t :application +auth+)
    (group-db-num integer name string)
    group-db-num ;;todo 3 auth group separation point
  (assert (cloud:verify-db-num-conn group-db-num *db*))
  (assert (not (util:empty-string-p name)))
  (db-with-transaction ('group-create)
    (let ((row (db-insert-row
                (make-db-table-aclgroup-row
                 nil group-db-num name 0
                 (get-auth-user-id) nil))))
      (let* ((groupid (db-table-aclgroup-row-groupid row))
             (group (make-group :id (make-db-id :db group-db-num :id groupid)
                                :name name)))
        (acl-user-add-right group +admin-right+ t)
        group))))

(def-rpc-with-proxy boolean group-delete (:application +auth+)
    (group group)
    (db-id-db (group-id group))
  (verify-group-admin group)
  (let ((row (select-group-master-rec group)))
    (when row
      (setf (db-table-aclgroup-row-deleted row) t)
      (db-update-row row)
      t)))

(def-rpc-with-proxy group group-rename (:application +auth+)
    (group group)
    (db-id-db (group-id group))
  (verify-group-admin group)
  (assert (not (util:empty-string-p (group-name group))))
  (let ((row (select-group-master-rec group)))
    (setf (db-table-aclgroup-row-name row) (group-name group))
    (db-update-row row))
  group)

(defun acl-group-add-user (group user-login)
  (group-add-user :group group :user-login user-login))

(def-rpc-with-proxy boolean group-add-user (:application +auth+)
    (group group user-login string)
    (db-id-db (group-id group))
  (let ((user (find-user :login user-login :orig-login nil)))
    (unless user
      (error "user ~A not found" user-login))
    (group-add-subject :group group :subj (dbidtype-from-userid (user-userid user)))))

(def-rpc-with-proxy boolean group-add-subject (:application +auth+)
    (group group subj db-id-type)
    (db-id-db (group-id group))
  (verify-group-admin group)
  (verify-live-group group)
  (let* ((subjtype (db-id-type-type subj))
         (subjdbid (db-id-type-dbid subj))
         (rows (db-aclgroupmember-select-pk
                *vdb* (db-id-id (group-id group))
                subjtype (db-id-db subjdbid) (db-id-id subjdbid)
                *vdb* nil)))
    (when rows
      (error "already exists")))
  (insert-aclgroupmember :group group :subj subj :memberdb t)
  (insert-aclgroupmember :group group :subj subj :memberdb nil)
  t)

(def-rpc-with-proxy boolean insert-aclgroupmember
    (:post-only t :host-to-host-only t  :application +auth+)
    (group group subj db-id-type memberdb boolean)
    (if memberdb
        (key-to-vdb (db-id-db (db-id-type-dbid subj)))
        (db-id-db (group-id group)))
  (db-insert-row (make-db-table-aclgroupmember-row
                  (group-id group)
                  subj
                  *vdb* memberdb))
  t)

(defun verify-group-admin (group)
  (unless (acl-user-check-right group +admin-right+)
    (raise-error 'acl-perm-denied)))

(def-rpc-with-proxy boolean group-remove-user
    (:application +auth+)
    (group group user user)
    (db-id-db (group-id group))
  (group-remove-subject :group group :subj (dbidtype-from-userid (user-userid user))))

(def-rpc-with-proxy boolean group-remove-subject (:application +auth+)
    (group group subj db-id-type)
    (db-id-db (group-id group))
  (verify-group-admin group)
  (group-remove-subj-in-db :group group :subj subj :memberdb nil)
  (group-remove-subj-in-db :group group :subj subj :memberdb t)
  t)


(def-rpc-with-proxy  boolean group-remove-subj-in-db
    (:host-to-host-only t :application +auth+)
    (group group subj db-id-type memberdb boolean)
    (if memberdb
        (key-to-vdb (db-id-db (db-id-type-dbid subj)))
        (db-id-db (group-id group)))
  (let* ((group-db-num (db-id-db (group-id group)))
         (subj-db-num (db-id-db (db-id-type-dbid subj))))
    (let ((row (db-aclgroupmember-select-pk
                group-db-num (db-id-id (group-id group))
                (db-id-type-type subj) subj-db-num (db-id-id (db-id-type-dbid subj))
                *vdb* memberdb)))
      (unless row
        (error "no aclgroupmember record found"))
      (db-delete-row row)
      t)))

(def-rpc-with-proxy (vector user) group-list-users  (:application +auth+)
    (group group offset integer maxrows integer)
    (db-id-db (group-id group))
  (verify-group-admin group)
  (let* ((rows (db-aclgroupmember-select-bygroup-subjtype
                *vdb* *vdb* (db-id-id (group-id group))
                (right-object-type (auth-token-user-id)) nil))
         ;;todo 2 group-list-users pagination support
         (stub-list
          (mapcar (lambda (row)
                    (make-user-id
                     :id (db-id-type-dbid (db-table-aclgroupmember-row-subj row))))
                  rows)))
    ;;todo 9 modify rpc framework to do automatic list to vector [not feasible]
    (get-user :list (coerce stub-list 'vector))))

;;todo: 1 pending rpc to list subjects in group see group-list-users

(def-rpc-with-proxy (vector group) group-list-for-subj  (:application +auth+)
    (subj db-id-type)
    (key-to-vdb (db-id-db (db-id-type-dbid subj)))
  (let ((rows (db-aclgroupmember-select-bysubj
               *vdb* (db-id-type-type subj) (db-id-db (db-id-type-dbid subj))
               (db-id-id (db-id-type-dbid subj)) t)))
    (coerce (mapcar (lambda (row)
                      (make-group :id (db-table-aclgroupmember-row-groupid row)))
                    rows)
            'vector)))

(def-rpc-with-proxy boolean group-add-child
    (:application +auth+)
    (pgroup group cgroup group)
    (db-id-db (group-id pgroup))
  (let ((pdb-num (db-id-db (group-id pgroup)))
        (cdb-num (db-id-db (group-id cgroup))))
    (db-with-transaction ('group-add-child)
      (verify-group-admin pgroup)
      (and (insert-group-child :db-num pdb-num :pgroup pgroup :cgroup cgroup :cdb nil)
           (group-change-parent-count :group cgroup :change 1)
           (insert-group-child :db-num  cdb-num :pgroup pgroup :cgroup cgroup :cdb t)))))

(def-rpc-with-proxy boolean group-change-parent-count
    (:host-to-host-only t :application +auth+)
    (group group change integer)
    (db-id-db (group-id group))
  ;;(verify-group-admin group)
  (unless (or (= 1 change) (= -1 change))
    (error "invalid change count ~A" change))
  (db-with-transaction ('group-change-parent-count)
    (let ((row (select-group-master-rec group)))
      (incf (db-table-aclgroup-row-parents row) change)
      (db-update-row row)
      t)))

(def-rpc-with-proxy boolean insert-group-child
    (:host-to-host-only t :application +auth+)
    (db-num integer pgroup group cgroup group cdb boolean)
    db-num
  (verify-live-group (if cdb cgroup pgroup))
  (db-insert-row (make-db-table-aclgroupchild-row
                  (group-id pgroup) (group-id cgroup) db-num cdb))
  t)

(defun verify-live-group (group)
  (let ((row (select-group-master-rec group)))
    (assert (not (db-table-aclgroup-row-deleted row)))))

(def-rpc boolean group-remove-child
    (:application +auth+)
    (pgroup group cgroup group)
  (verify-group-admin pgroup)
  (let ((pdb-num (db-id-db (group-id pgroup)))
        (cdb-num (db-id-db (group-id cgroup))))
    (and (delete-group-child :db-num pdb-num :pgroup pgroup :cgroup cgroup :cdb nil)
         (group-change-parent-count :group cgroup :change -1)
         (delete-group-child :db-num  cdb-num :pgroup pgroup :cgroup cgroup :cdb t)
         )))

(def-rpc-with-proxy boolean delete-group-child
    (:host-to-host-only t :application +auth+)
    (db-num integer pgroup group cgroup group cdb boolean)
    db-num
  (let ((row (db-aclgroupchild-select-pk
              (db-id-db (group-id pgroup)) (db-id-id (group-id pgroup))
              (db-id-db (group-id cgroup)) (db-id-id (group-id cgroup))
              db-num cdb)))
    (when row
      (db-delete-row row))))

(defun select-group-master-rec (group)
  (let* ((groupid (group-id group))
         (row (db-aclgroup-select-pk (db-id-db groupid) (db-id-id groupid))))
    (assert (= (db-id-db (group-id group)) (db-table-aclgroup-row-db-num row)))
    (unless row
      (error "group not found"))
    row))

(def-rpc-with-proxy boolean test-group-verify-parent-count
    (:host-to-host-only t :anonymous t :application +auth+)
    (group group count integer)
    (db-id-db (group-id group))
  (let ((row (select-group-master-rec group)))
    (= count (db-table-aclgroup-row-parents row))))

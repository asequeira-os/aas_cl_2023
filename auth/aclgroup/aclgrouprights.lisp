(in-package :auth)

(def-rpc-with-proxy boolean group-add-right
    (:application +auth+)
    (objtype string obj db-id group group right string)
    (db-id-db (group-id group))
  ;;need admin right on obj to grant access from group
  (acl-user-check-right-impl objtype obj +admin-right+)
  (db-insert-row (make-db-table-group-rights-row nil objtype obj
                                                 (group-id group) right
                                                 *vdb*))
  (group-add-right-objdb :objtype objtype :obj obj :group group :right right)
  t)

(def-rpc-with-proxy boolean group-add-right-objdb
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id group group right string)
    (key-to-vdb (db-id-db obj))
  (db-insert-row (make-db-table-group-rights-row
                  nil objtype obj (group-id group) right
                  *vdb*)))

(def-rpc-with-proxy boolean group-remove-right
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id group group right string)
    (db-id-db (group-id group))
  (acl-user-check-right-impl objtype obj +admin-right+)
  (let ((rows (db-group_rights-select-bygroupobj
               *vdb* (db-id-id (group-id group))
               (db-id-db obj) (db-id-id obj))))
    (dolist (row rows)
      (when (equal (db-table-group-rights-row-priv row) right)
        (db-delete-row row)
        (group-remove-right-objdb  :objtype objtype :obj obj :group group :right right)
        (return-from group-remove-right t)))))

(def-rpc-with-proxy boolean group-remove-right-objdb
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id group group right string)
    (key-to-vdb (db-id-db obj))
  (let ((rows (db-group_rights-select-bygroupobj
               (db-id-db (group-id group)) (db-id-id (group-id group))
               (db-id-db obj) (db-id-id obj))))
    (dolist (row rows)
      (when (equal (db-table-group-rights-row-priv row) right)
        (db-delete-row row)
        (return-from group-remove-right-objdb t)))))

;;check if any of the groups have the requested right to the object
(def-rpc boolean groups-has-right
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id groups (vector group) right string)
  (let ((groups-by-db (aas-misc:group-by groups
                                         :key (lambda (g)
                                                (db-id-db (group-id g)))
                                         :test #'equal)))
    ;;list elm is list, car is db num, cdr is list of groups
    (dolist (db-and-groups groups-by-db)
      (let ((group-db-num (car db-and-groups))
            (groups (cdr db-and-groups)))
        (when (groups-has-right-impl :objtype objtype :obj obj
                                     :groups (coerce groups 'vector)
                                     :group-db-num group-db-num :right right)
          (return-from groups-has-right t))))
    nil))


(def-rpc-with-proxy boolean groups-has-right-impl
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id groups (vector group) group-db-num integer right string)
    group-db-num
  (loop for group across groups do
     ;;(format t "before check ~A = ~A" group-db-num (db-id-db (group-id group)))
       (unless (= group-db-num (db-id-db (group-id group)))
         ;;(break)
         (error "groups are not from expected db ~A" group-db-num))
     ;;(print "after check")
       (let ((rows (db-group_rights-select-bygroupobj
                    (db-id-db (group-id group)) (db-id-id (group-id group))
                    (db-id-db obj) (db-id-id obj))))
         (when rows
           (when (find right rows
                       :key #'db-table-group-rights-row-priv :test #'equal)
             (return-from groups-has-right-impl t)))))
  nil)

(def-rpc boolean user-add-right
    (:application +auth+)
    (objtype string obj db-id user user right string)
  (user-add-right-impl :objtype objtype :obj obj
                       :user-id (user-userid user)
                       :right right :seed nil))

(def-rpc-with-proxy boolean user-add-right-impl
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id user-id user-id
             right string seed boolean)
    (user-db-num user-id)
  (unless seed
    (acl-user-check-right-impl objtype obj +admin-right+))
  (db-insert-row (make-db-table-user-rights-row nil objtype obj
                                                user-id right
                                                *vdb*))
  (user-add-right-objdb :objtype objtype :obj obj
                        :user-id user-id :right right))

(def-rpc-with-proxy boolean user-add-right-objdb
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id user-id user-id right string)
    (key-to-vdb (db-id-db obj))
  (db-insert-row (make-db-table-user-rights-row nil objtype obj
                                                user-id right
                                                *vdb*)))

(def-rpc boolean user-remove-right
    (:application +auth+)
    (objtype string obj db-id user user right string)
  (user-remove-right-impl :objtype objtype :obj obj
                          :user-id (user-userid user)
                          :right right ))

(def-rpc-with-proxy boolean user-remove-right-impl
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id user-id user-id right string)
    (user-db-num user-id)
  (acl-user-check-right-impl objtype obj +admin-right+)
  (let ((rows (db-user_rights-select-byuserobj
               *vdb* (user-id-num user-id)
               (db-id-db obj) (db-id-id obj))))
    (dolist (row rows)
      (when (equal (db-table-user-rights-row-priv row) right)
        (db-delete-row row)
        (user-remove-right-objdb  :objtype objtype
                                  :user-id user-id
                                  :right right)
        (return-from user-remove-right-impl t)))))

(def-rpc-with-proxy boolean user-remove-right-objdb
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id user-id user-id right string)
    (key-to-vdb (db-id-db obj))
  (let ((rows (db-user_rights-select-byuserobj
               (user-db-num user-id) (user-id-num user-id)
               (db-id-db obj) (db-id-id obj))))
    (dolist (row rows)
      (when (equal (db-table-user-rights-row-priv row) right)
        (db-delete-row row)
        (return-from user-remove-right-objdb t)))))

(def-rpc boolean user-has-right
    (:application +auth+)
    (objtype string obj db-id user-id user-id right string)
  (user-has-right-impl :objtype objtype :obj obj
                       :user-id user-id
                       :right right ))

(def-rpc-with-proxy boolean user-has-right-impl
    (:host-to-host-only t :application +auth+)
    (objtype string obj db-id user-id user-id right string)
    (user-db-num user-id)
  (let ((rows (db-user_rights-select-byuserobj
               *vdb* (user-id-num user-id)
               (db-id-db obj) (db-id-id obj))))
    (and rows
         (find right rows
               :key #'db-table-user-rights-row-priv :test #'equal))))

(defun acl-user-check-right (obj right)
  (let ((objtype (right-object-type obj))
        (obj-db-id (obj-db-id obj)))
    (acl-user-check-right-impl objtype obj-db-id right)
    t))

(defun acl-user-check-right-impl (objtype obj-db-id right)
  (unless
      (or (user-has-right :objtype objtype :obj obj-db-id
                          :user-id (auth-token-user-id)
                          :right right )
          (let ((groups (acl-user-effective-membership)))
            (and groups (not (zerop (length groups)))
                 (groups-has-right :objtype objtype :obj obj-db-id
                                   :groups groups :right right))))
    (raise-error 'acl-perm-denied)))

(def-rpc-with-proxy boolean  acl-subj-check-right (:application +auth+)
    (subj db-id-type right string obj db-id-type)
    (key-to-vdb (db-id-db (db-id-type-dbid subj))) ;;pin host, may help caching later
  (let ((groups (acl-subj-effective-membership :subj subj)))
    (unless (and groups (not (zerop (length groups)))
                 (groups-has-right :objtype (db-id-type-type obj)
                                   :obj (db-id-type-dbid obj)
                                   :groups groups :right right))
      (raise-error 'acl-perm-denied))))

(defun acl-user-add-right (obj right seed)
  (user-add-right-impl :objtype (right-object-type obj) :obj (obj-db-id obj)
                       :user-id (auth-token-user-id)
                       :right right :seed seed))

(defun acl-group-add-right (obj right group)
  (group-add-right :objtype (right-object-type obj) :obj (obj-db-id obj)
                   :group group :right right))




(in-package :auth)

(define-constant +company-right+ "auth::company")

(def-db-table companies
    "company master"
  +auth+
  ((id db-id)
   (name string)
   (group db-id)
   (creator user-id)
   (deleted boolean)
   )
  (((id db)) ((id id)))
  ()
  (:vdb-sequence (id db) (id id)))

(def-rpc-struct company
    (id nil :type db-id)
  (name nil :type string))

(defmethod obj-db-id ((company company))
  (company-id company))

;;todo 1 allow only paying customer to create company
;;allow only one company per customer
(def-rpc-with-proxy  company company-create
    (:application +auth+)
    (name string)
    (auth-token-db-num)
  (db-with-transaction ('create-company)
    (let ((group (group-create :name (concatenate 'string "company " name))))
      (let ((row (make-db-table-companies-row
                  (make-db-id :db *vdb*) name (group-id group) (get-auth-user-id) nil)))
        (let* ((db-row (db-insert-row row))
               (company (make-company :id (db-table-companies-row-id db-row) :name name)))
          (group-add-right :objtype (right-object-type company) :obj (company-id company)
                           :group group :right +company-right+ )
          company)))))

(def-rpc-with-proxy boolean company-add-user
    (:application +auth+)
    (company company user-id user-id)
    (db-id-db (company-id company))
  (let ((row (company-master-rec company)))
    (group-add-subject :group (make-group :id (db-table-companies-row-group row))
                       :subj (dbidtype-from-userid user-id))))


(defun company-master-rec (company)
  (let ((id (company-id company)))
    (db-companies-select-pk (db-id-db id) (db-id-id id))))

(def-rpc-with-proxy boolean company-member-check
    (:application +auth+)
    (company company)
    (db-id-db (company-id company))
  (acl-user-check-right company auth::+company-right+))



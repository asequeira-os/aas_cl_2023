;;todo 2 fix up comments in this file
;;they may not be true since the VDB work
(in-package :db-cloud)

(defun postgres-db-spec (prefix db-num)
  (unless (stringp prefix)
    (error "please configure a db name prefix for app"))
  (db-base::make-postgres-conn-spec
   :db-name  (format nil "~A~3,'0D" prefix db-num)
   :user  "antony" ;;todo 2 prod need secure config for db credentials
   :password  "ggt67yh63jkl"
   :host  "localhost"))

;;one instances per (app, physical db number) combination
(defstruct (cloud-db-node)
  (prefix nil :type string)
  (pdb nil :type (integer 0 31000)))

;;one instances per connection
(defstruct (cloud-db (:include db-base::db-base))
  (node nil :type cloud-db-node)
  )

;;get-db-num is dead . no more node to db-num lookup allowed
;; (defgeneric get-db-num (db))

;; (defmethod get-db-num ((db cloud-db))
;;   (cloud-db-node-db-num (cloud-db-node db)))

;; (defmethod get-db-num ((db cloud-db-node))
;;   (cloud-db-node-db-num db))

(defun cloud-db-ctor-base (node)
  (let* ((db-num (cloud-db-node-pdb node))
         (spec (postgres-db-spec (cloud-db-node-prefix node) db-num)))
    (let ((db-ob (make-cloud-db
                  :node node
                  :disconnector (lambda (db)
                                  (db-disconnect (cloud-db-conn db)))
                  :conn-spec spec
                  :conn (db-connect +ident-postgres+ spec))))
      db-ob)))

(defun cloud-db-ctor (node)
  (let ((db-ob (cloud-db-ctor-base node)))
    (let ((row (db-db_num-select-pk (cloud-db-node-pdb node) db-ob)))
      (unless row
        (error "db number mismatch between db config and actual data")))
    db-ob))


;;this is a important table
;;cloud application will add this to every app schema
;;this is pdb num
(db-base:def-db-table db-num
    "db number"
  "dummy-app"
  ((db-num integer))
  (db-num)
  nil
  nil)

(db-base:def-db-table sequence
    "vdb sequences"
  "dummy-app"
  ((db-num integer)
   (tbl string)
   (seq integer)
   )
  (db-num tbl)
  nil
  nil)

;;db layer does not know which dbs are service by this host
;;hence this is a separate function from init-cloud
(defun init-node (schema prefix i)
  (let ((db-node (make-cloud-db-node
                  :prefix prefix
                  :pdb i)))
    (db-base:db-with-conn
        (db db-node #'cloud-db-ctor-base)
      ;;todo 2 prod REMOVE NEXT STMT BEFORE PRODUCTION
      (let ((db-base::*drop-table-ignore-no-exist* t))
        (db-base::drop-all-tables schema db))
      (db-base::create-all-tables schema db)
      (db-insert-row (make-db-table-db-num-row i) db)
      )
    db-node))


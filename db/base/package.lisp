(defpackage :db-base
  (:use :common-lisp :aas-cl :iter :aas-misc :aseq-test)
  (:export   *db*
             +ident-postgres+
             db-clear-pool
             db-col-type-integer
             db-connect
             db-create-table
             db-delete
             db-delete-row
             db-disconnect
             db-drop-table
             db-index-ascending
             db-index-descending
             db-insert
             db-insert-row
             db-select
             db-select-single
             db-update-row
             db-with-conn
             db-with-transaction
             def-db-table
             get-struct-columns
             iterate-tables))

(in-package :db-base)

(defpackage :db-base-test
  (:use :common-lisp :aas-cl :db-base :aseq-test)
  (:import-from :db-base
                *drop-table-ignore-no-exist*
                *vdb-sequencer*
                +ident-postgres+
                create-all-tables
                create-table
                db-base-conn
                db-clear-pool
                db-from-pool
                db-table-doc
                db-table-sequence-col
                db-to-pool
                db-type-to-lisp
                drop-all-tables
                drop-table
                ident-lisp-to-db
                make-db-base
                make-postgres-conn-spec))



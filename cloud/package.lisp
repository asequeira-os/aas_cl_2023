(defpackage :cloud
  (:use :cl :aas-cl)
  (:import-from :aas-rpc
                *accept-language*
                *auth-token*
                *auth-user*
                add-trusted-host
                call-remote-anon
                call-remote-impersonate
                call-remote-trust
                call-remote-trust-impersonate
                parse-rpc-option)

  (:import-from :db-base
                *table-id-counter*
                *vdb-sequencer*
                add-table-to-schema
                db-table
                db-table-id
                db-table-name
                db-table-vdb-seq-cols
                iterate-tables )

  (:import-from  :db-cloud
                 *db-table-db-num*
                 *db-table-sequence*
                 cloud-db-node
                 cloud-db-node-pdb
                 db-sequence-select-pk
                 db-table-sequence-row-seq
                 make-db-table-sequence-row )  )

(in-package :cloud)

(export '(
          *application*
          *vdb*
          +test-app+
          application
          db-id
          db-id-db
          db-id-id
          db-id-type
          db-id-type-dbid
          db-id-type-type
          def-rpc-with-proxy
          define-app
          define-error
          define-rights
          ext-db-key
          get-auth-token-hash-key
          get-key-host
          get-xed-error-details
          init get-db-node
          is-my-key
          is-my-vdb
          key-to-vdb
          make-db-id
          make-db-id-type
          raise-error
          rights-and
          rights-or
          set-application-db-count
          verify-db-num-conn
          with-application
          with-db-for-key
          xed-error))

(defpackage :cloud-test
  (:use :cl :cloud :aseq-test)
  (:import-from :cloud
                *error-symbols*
                +test-app+
                distribute-vdb-over-pdb
                get-vdb-host
                rights-to-integer
                set-db-name-prefix
                set-pdb-host-range))

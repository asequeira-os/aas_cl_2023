(in-package :db-base-test)

(deftest all-tests
  (unwind-protect
       (and (sequence-test)
            (fk-tests)
            (range-tests)
            (pool-tests)
            (db-table-all-tests)
            (table-index-sql-test)
            (insert-delete-test)
            (symbol-column-test)
            (sub-struct-test)
            (bit-and-boolean-column-test)
            (nulls-test)
            (sub-null-test)
            (decimals-test)
            (with-conn-tests-1)
            (with-conn-tests-2)
            (with-conn-tests-3))
    (test-cleanup)))

(def-db-table fk-1
    "test basic fk"
  *dummy-test-app*
  ((mk (or null integer))
   (fk bc))
  (mk)
  nil
  (:auto-increment)
  )

(deftest fk-tests
  (let* ((table *db-table-fk-1*))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('fk-tests)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table)
        (db-insert-row
         (make-db-table-fk-1-row nil (dc-from-key "fka1")))
        (let ((res (db-fk_1-select-pk 1)))
          (is res)
          (is (typep (db-table-fk-1-row-fk res) 'dc)))))))


;; subnullstruct
(def-db-table subnull-test
    "test table for with a  null sub struct"
  *dummy-test-app*
  ((seq-id (or null integer))
   (foo-bar string)
   (subnull (or null subnullstruct))
   (subnonull subnullstruct))
  (seq-id)
  nil
  (:auto-increment))

(deftest sub-null-test
  (let* ((table *db-table-subnull-test*))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('fk-tests)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table)
        (let ((r1 (db-insert-row
                   (make-db-table-subnull-test-row
                    nil "foo1" nil
                    (make-subnullstruct :intcol 42 :scol "bar1"))))
              (r2 (db-insert-row
                   (make-db-table-subnull-test-row
                    nil "foo2"
                    (make-subnullstruct :intcol 52 :scol "bar2a")
                    (make-subnullstruct :intcol 62 :scol "bar2b")))))
          (let ((r1db (db-subnull_test-select-pk (db-table-subnull-test-row-seq-id r1)))
                (r2db (db-subnull_test-select-pk (db-table-subnull-test-row-seq-id r2))))
            (is (equalp r1 r1db))
            (is (equalp r2 r2db))))))))

(def-db-table rng-test
    "test index used for range"
  *dummy-test-app*
  ((id ss-vdb)
   (rt integer)
   (b boolean)
   (myi (or null integer)))
  (((id v1) :ascending) ((id s1) :ascending))
  ((:not-unique rtind ((id v1) :ascending) ((id s1) :ascending :range)))
  (:vdb-sequence (id v1) (id s1))
  ((rtsilly (or (= b b)  (= s1 (id s1)) (= s2 (id s1))
                (and (>= rt1 rt) (< rt2 rt))))
   (rtfun (and (>= s1 (id s1)) (or (= myi myi) (< mymax myi ))))))

(deftest range-tests
  (let* ((table *db-table-rng-test*)
         (*vdb-sequencer* (let ((counter 0))
                            (lambda (table vdb)
                              (declare (ignorable table vdb))
                              (incf counter)))))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('range-tests)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table)
        (let* ((rlist
                (loop for i from 1 to 10
                   for myi from 5 to 15
                   collect (make-db-table-rng-test-row (make-ss-vdb :v1 1) i t myi))))
          (let ((dblist (mapcar #'db-insert-row rlist)))
            (is (= 10 (length dblist)))
            (let ((rsingle (db-rng_test-select-pk 1 5))
                  (rmulti (db-rng_test-select-range-rtind 1 3 6 )))
              (let ((idsingle (db-table-rng-test-row-id rsingle)))
                (is (= 5 (ss-vdb-s1 idsingle)))
                (is (= 1 (ss-vdb-v1 idsingle))))
              (loop for i from 3 to 5
                 for j from 0 to 2 do
                   (let ((row (nth j rmulti)))
                     (is (= i (ss-vdb-s1 (db-table-rng-test-row-id row)))))))
            (let ((rtfunrows (db-rng_test-select-rtfun 5 6 14)))
              (is (= 1 (length rtfunrows))))))))))




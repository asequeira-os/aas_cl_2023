(in-package :db-cloud-test)

(db-base:def-db-table test-sequencer
    "unit test table for db sequencer"
  db-base-test::*dummy-test-app*
  ((seqcol (or null integer))
   (bar  string))
  (seqcol)
  nil
  (:auto-increment))

(db-base::add-table-to-schema db-cloud::*db-table-db-num* db-base-test::*dummy-test-app*)

(defun itoa (i)
  (with-output-to-string (s)
    (format s "foo ~A" i)))

;;nothing much to test
(deftest all-tests
  (let ((db0 (cloud-db-ctor (init-node db-base-test::*dummy-test-app* "test" 0)))
        (db1 (cloud-db-ctor (init-node db-base-test::*dummy-test-app* "test" 1))))
    (unwind-protect
         (progn
           (is db0)
           (is db1)
           (let* ((row-ob  (db-base:db-insert-row
                            (make-db-table-test-sequencer-row nil "foo")
                            db0))
                  (start-seq (db-table-test-sequencer-row-seqcol row-ob)))
             (loop for i from (1+ start-seq) to (+ 10 start-seq) do
                  (db-base:db-insert-row
                   (make-db-table-test-sequencer-row nil (itoa i)) db0))
             (let ((test-seq (+ 4 start-seq)))
               (is (equal (itoa test-seq)
                          (db-table-test-sequencer-row-bar
                           (db-test_sequencer-select-pk test-seq db0)))))))

      (dolist (db (list db0 db1))
        (funcall (db-cloud::cloud-db-disconnector db) db)))))


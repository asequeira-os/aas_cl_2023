(in-package :db-base-test)

(defvar *dummy-test-app* "dummy-test-app-schema")

(defparameter *test-db-vendor* +ident-postgres+)

(defparameter +max-test-db-num+ 3)

(defparameter *test-data-dir* (aas-build:get-env "LISP_TEMP_DIR"))

(defun test-db-ctor (spec)
  (make-db-base
   :disconnector (lambda (db)
                   (db-disconnect (db-base-conn db)))
   :conn-spec spec
   :conn (db-connect *test-db-vendor* spec)))


(defun test-spec (i)
  (assert (<= 0 i (1- +max-test-db-num+)))
  (if (eql *test-db-vendor* +ident-postgres+)
      (make-postgres-conn-spec
       :db-name  (format nil "test~3,'0D" i)
       :user  "antony"
       :password  "ggt67yh63jkl"
       :host  "localhost")
      (error "unknown db vendor ~A" *test-db-vendor*)))

(defun test-cleanup ()
  (db-clear-pool)
  (loop for i from 0 to (1- +max-test-db-num+) do
       (unwind-protect
            (let ((*drop-table-ignore-no-exist* t))
              (db-with-conn (*db* (test-spec i) #'test-db-ctor)
                (drop-all-tables *dummy-test-app* *db*))))))

(deftest pool-tests
  (let ((spec1 (test-spec 1))
        (spec2 (test-spec 2)))
    (let ((db-1-1 (db-from-pool spec1 #'test-db-ctor))
          (db-2-1 (db-from-pool spec2 #'test-db-ctor))
          (db-1-2 (db-from-pool spec1 #'test-db-ctor))
          (db-2-2 (db-from-pool spec2 #'test-db-ctor)))
      (is-not (or (eq db-1-1 db-1-2)
                  (eq db-2-1 db-2-2)
                  (eq db-1-1 db-2-1)))
      (db-to-pool db-1-1)
      (db-to-pool db-2-1)
      (let ((db-1-3 (db-from-pool spec1 #'test-db-ctor))
            (db-2-3 (db-from-pool spec2 #'test-db-ctor)))
        (is (eq db-1-1 db-1-3))
        (is (eq db-2-1 db-2-3))
        (db-to-pool db-1-2)
        (db-to-pool db-2-2)
        (db-to-pool db-1-3)
        (db-to-pool db-2-3)
        (db-clear-pool)
        (let ((db-1-4 (db-from-pool spec1 #'test-db-ctor)))
          (is-not (eq db-1-4 db-1-3))
          (db-to-pool db-1-4)))))
  (db-clear-pool))

(deftest with-conn-tests-1
  (db-with-conn (db1 (test-spec 1) #'test-db-ctor)
    (db-with-conn (db2 (test-spec 2) #'test-db-ctor)
      (db-with-conn (db3 (test-spec 1) #'test-db-ctor)
        (is (eq db1 db3))
        (is-not (eq db1 db2))
        (db-with-transaction ('with-conn-tests-1 db1)
          (is (= 175 (db-select-single db3 "select 175"))))))))

(deftest with-conn-tests-2
  (let ((saved nil))
    (db-with-conn (db1 (test-spec 1) #'test-db-ctor)
      (is db1)
      (ignore-errors
        (db-with-conn (db2 (test-spec 2) #'test-db-ctor)
          (is db2)
          (setf saved db2)
          (error "should not see this")))
      (db-with-conn (db2-2 (test-spec 2) #'test-db-ctor)
        (is (eq saved db2-2))))))

(deftest with-conn-tests-3
  (let ((t1 nil))
    (mp-make-thread "test-db"
                    (lambda ()
                      (db-with-conn (db1 (test-spec 1) #'test-db-ctor)
                        (setf t1 db1)
                        (sleep 3))) nil)
    (sleep 2)
    (db-with-conn (db1 (test-spec 1) #'test-db-ctor)
      (is-not (eq db1 t1)))
    (sleep 3)))

(def-db-table test-tablex
    "unit test table for db core"
  *dummy-test-app*
  ((fooo  integer) (qux string) (bar  string))
  ((fooo  :ascending) bar)
  ((:unique foobar (fooo :ascending)
            (bar :descending))
   (:not-unique quxindex (qux  :ascending))))

(deftest table-index-sql-test
  (let ((table *db-table-test-tablex*))
    (is (equal "unit test table for db core"
               (db-table-doc table)))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (let ((*drop-table-ignore-no-exist* t))
        (drop-table table))
      (create-table table)
      (drop-table  table))))


(deftest insert-delete-test
  (let ((table *db-table-test-tablex*))
    (is (null (db-table-sequence-col table)))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (create-table table)
      (db-insert-row (make-db-table-test-tablex-row 11 "qqq" "foo"))
      (db-insert-row (make-db-table-test-tablex-row 12 "qqq" "foo"))
      (let ((rows (db-test_tablex-select-quxindex "qqq")))
        (is (= 2 (length rows)))
        (is (= (db-table-test-tablex-row-fooo (first rows)) 11))
        (is (= (db-table-test-tablex-row-fooo (second rows)) 12)))
      (let ((row-ob (db-test_tablex-select-pk 11 "foo")))
        (is row-ob)
        (is (= (db-table-test-tablex-row-fooo row-ob) 11))
        (is (equal "qqq" (db-table-test-tablex-row-qux row-ob))))
      (let ((db-row
             (db-select *db* "select * from test_tablex where fooo = $1" 11)))
        (is (equal db-row '((11 "qqq" "foo")))))
      (db-delete-row (make-db-table-test-tablex-row 11 "qqq" "foo"))
      (let ((db-row
             (db-select *db* "select * from test_tablex  where fooo = $1 " 11)))
        (is (null db-row)))
      (let ((db-row
             (db-select *db* "select * from test_tablex  where fooo = $1 " 12)))
        (is-not (null db-row)))
      (db-insert-row (make-db-table-test-tablex-row 11 "qqq" "foo"))
      (let ((db-row
             (db-select *db* "select * from test_tablex where fooo = $1" 11)))
        (is (equal db-row '((11 "qqq" "foo")))))
      (db-delete-row (make-db-table-test-tablex-row 11 "qqq" "foo"))
      (let ((db-row
             (db-select *db* "select * from test_tablex  where fooo = $1 " 11)))
        (is (null db-row)))
      (db-insert-row (make-db-table-test-tablex-row 15 "aaa" "bbb"))
      (db-insert-row (make-db-table-test-tablex-row 16 "ccc" "ddd") )
      (let ((struct (db-test_tablex-select-pk 15 "bbb")))
        (is (= (db-table-test-tablex-row-fooo struct) 15))
        (is (equal (db-table-test-tablex-row-qux struct) "aaa"))
        (is (equal (db-table-test-tablex-row-bar struct) "bbb"))
        (db-delete-row struct)
        (is (= (db-table-test-tablex-row-fooo
                (db-test_tablex-select-pk 16 "ddd" *db*))
               16))
        (is (null (db-test_tablex-select-pk 15 "bbb")))
        (db-insert-row struct)
        (is (= (db-table-test-tablex-row-fooo
                (db-test_tablex-select-pk 15 "bbb"))
               15)))
      (db-test_tablex-select-pk 15 "ccc")
      (drop-table  table))))

(def-db-table sequence-test
    "test table for sequence generation"
  *dummy-test-app*
  ((seq-id (or null integer))
   (foo-bar string))
  (seq-id)
  nil
  (:auto-increment))

(deftest sequence-test
  (test-cleanup)
  (is (db-table-sequence-col *db-table-sequence-test*))
  (unwind-protect
       (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
         (let ((*drop-table-ignore-no-exist* t))
           (drop-all-tables *dummy-test-app* *db*))
         (create-all-tables *dummy-test-app* *db*)
         (let ((resp
                (db-insert-row (make-db-table-sequence-test-row nil "qqq"))))
           (is (= 1 (db-table-sequence-test-row-seq-id resp)))
           (db-insert-row (make-db-table-sequence-test-row nil "yyy"))
           (db-insert-row (make-db-table-sequence-test-row nil "zzz")))
         (let ((resp (db-sequence_test-select-pk 2)))
           (is (equal "yyy" (db-table-sequence-test-row-foo-bar resp)))))))


(def-db-table test-symbol-table-1
    "unit tests symbol column type"
  *dummy-test-app*
  ((intcol1 (or null integer))
   (sc symbol)
   (str string))
  (intcol1)
  ((:not-unique sc1 sc))
  (:auto-increment))

(deftest symbol-column-test
  (let ((table *db-table-test-symbol-table-1*))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('symbol-column-test)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table )
        (db-insert-row
         (make-db-table-test-symbol-table-1-row nil 'abcd "abCDef"))
        (db-insert-row
         (make-db-table-test-symbol-table-1-row nil 'xxx "Xyz"))
        (let ((row (db-test_symbol_table_1-select-pk 1)))
          (is row)
          (is (eq 'abcd (db-table-test-symbol-table-1-row-sc row )))
          (is (string= "abCDef" (db-table-test-symbol-table-1-row-str row))))
        (drop-table table )
        )))
  )

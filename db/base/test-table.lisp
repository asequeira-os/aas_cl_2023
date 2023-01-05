(in-package :db-base-test)



(deftest db-table-all-tests
  (and (ident-tests) (db-type-map-tests) (single-char-test)
       (test-vdb-sequencer)))

(deftest ident-tests
  (is (equal (ident-lisp-to-db nil) ""))
  (is (equal (ident-lisp-to-db 'xed) "xed"))
  (is (equal (ident-lisp-to-db 'www-xed) "www_xed"))
  (is (equal (ident-lisp-to-db '|Foo|) "foo"))
  (is (equal (ident-lisp-to-db "abcD") "abcd")))

(deftest db-type-map-tests
  (is (equal (db-type-to-lisp "varchar(14) NULL") '(OR NULL string)))
  (is (equal (db-type-to-lisp "varchar (32)") 'string )))
;;  (loop for (db-type arg-type) on rpc-args by #'cddr do

(def-db-table test-table-15
    "unit test table 1 for new def table macro"
  *dummy-test-app*
  ;;columns
  ((intcol1 integer)
   (strcol1 string)
   (strcol2 :db-type "varchar(20)")
   (strcol3 (or null string))
   (st test-table-substruct)
   (strcol4 :db-type "varchar(20) NULL"))
  ((intcol1 :ascending)
   ((st intcol1) ))
  ((:unique int1str1 intcol1 (strcol1 :descending))
   (:not-unique deep1 ((st ss intcol))))
  )

(def-db-table test-sequence-table
    "unit test table with auto sequence"
  *dummy-test-app*
  ;;columns
  ((intcol1 (or null integer))
   (strcol1 string))
  ((intcol1 :ascending)   )
  nil
  (:auto-increment))

(def-db-table test-sequence-table-2
    "unit test table with auto sequence"
  *dummy-test-app*
  ;;columns
  ((intcol1 integer)
   (st test-table-substruct)
   (strcol1 string))
  (((st intcol1) :ascending))
  nil
  (:auto-increment))

;;test struct type table columns be initialized from such struct
;;directly, instead of db stored columns
(deftest sub-struct-test
  (let ((table *db-table-test-table-15*))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('symbol-column-test)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table)
        (db-insert-row
         ;;following should compile without warnings or error
         (make-db-table-test-table-15-row
          1 "foo in strcol1" "bar in strcol2"
          nil
          (make-test-table-substruct
           :intcol1 20 :strcol1 "sub strcol1"
           :strcol2 "sub strcol2"
           :ss (make-sub-substruct :intcol 42)
           :strcol3 "sub strcol3")
          "qq for strcol4"))
        (let ((row (db-test_table_15-select-pk 1 20)))
          (is row)
          (is (string= (db-table-test-table-15-row-strcol1 row)
                       "foo in strcol1")))))))

(def-db-table test-bit
    "boolean flag test"
  *dummy-test-app*
  ((intcol1 (or null integer))
   ;;(bitcol bit)
   (boolcol boolean))
  (intcol1)
  nil
  (:auto-increment))

(deftest bit-and-boolean-column-test
  (let* ((table *db-table-test-bit*)
         ;;(bits  #*101100101001)
         (bools (make-array 20 :element-type 'boolean)))
    (loop for i from 0 to (1- (length bools)) do
         (setf (aref bools i) (= 1 (random 2))))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('bit-column-test)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table)
        (loop for i from 0 to (- (length bools) 1) do
             (db-insert-row
              (make-db-table-test-bit-row
               (1+ i) (aref bools i))))
        (loop for i from 0 to (- (length bools) 1) do
             (let ((row (db-test_bit-select-pk (1+ i))))
               ;;(is (= (aref bits i) (db-table-test-bit-row-bitcol row)))
               (if (aref bools i)
                   (is (db-table-test-bit-row-boolcol row))
                   (is-not (db-table-test-bit-row-boolcol row)))))))))


(def-db-table test-nulls
    "test nulls in varchar"
  *dummy-test-app*
  ((pkid (or null integer))
   (nonnull string)
   (oknull (or null string)))
  (pkid)
  nil
  (:auto-increment))

(deftest nulls-test
  (let* ((table *db-table-test-nulls*))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('nulls-test)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table ))
        (create-table table)
        (let ((row (db-insert-row
                    (make-db-table-test-nulls-row nil "fooo" nil))))
          (is row)
          (is (> (db-table-test-nulls-row-pkid row) 0))
          (let ((selected (db-test_nulls-select-pk
                           (db-table-test-nulls-row-pkid row))))
            (is (null (db-table-test-nulls-row-oknull selected)))))
        (let ((row (db-insert-row
                    (make-db-table-test-nulls-row nil "fooo" "bar"))))
          (is row)
          (is (> (db-table-test-nulls-row-pkid row) 0))
          (let ((selected (db-test_nulls-select-pk
                           (db-table-test-nulls-row-pkid row))))
            (is (equal "bar" (db-table-test-nulls-row-oknull selected)))))
        (verify-error error
            (db-insert-row
             (make-db-table-test-nulls-row nil nil "bar")))))))

(def-db-table test-decimal
    "test decimal arithmetic types"
  *dummy-test-app*
  ((pkid (or null integer))
   (nonnull decimal)
   (fixedprec :db-type "numeric(10,2) null")
   (oknull (or null decimal)))
  (pkid)
  nil
  (:auto-increment))

(deftest decimals-test
  (let* ((table *db-table-test-decimal*)
         (oknull #(123456789/1000000 nil 9876543210654321/100000000
                   nil 12345/100))
         (fixed-in  #(1/10 12/10 123/100 1234/100 1234/1000))
         (fixed-out #(1/10 12/10 123/100 1234/100 123/100)))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('decimals-test)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table )
          (create-table table)
          (loop for i from 0 to (1- (length fixed-in)) do
               (let ((row (db-insert-row
                           (make-db-table-test-decimal-row
                            nil (/ 123 100)
                            (aref fixed-in i)  (aref oknull i)))))
                 (is row)
                 (is (> (db-table-test-decimal-row-pkid row) 0))
                 (let ((selected (db-test_decimal-select-pk
                                  (db-table-test-decimal-row-pkid row))))
                   (is (eql (db-table-test-decimal-row-oknull selected)
                            (aref oknull i)))
                   (is (eql (db-table-test-decimal-row-fixedprec selected)
                            (aref fixed-out i)))))))))))

(deftest single-char-test
  (let* ((table *db-table-test-char*)
         (data (list #\3 #\A #\X #\x)))
    (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
      (db-with-transaction ('single-char-test)
        (let ((*drop-table-ignore-no-exist* t))
          (drop-table table )
          (create-table table)
          (let ((row (db-insert-row
                      (make-db-table-test-char-row
                       nil #\3))))
            (is row)
            (is (< 0 (db-table-test-char-row-pkid row)))
            (let ((start-id (db-table-test-char-row-pkid row)))
              (dolist (td data)
                (db-insert-row (make-db-table-test-char-row nil td)))
              (dolist (td data)
                (let ((row (db-test_char-select-pk (incf start-id))))
                  (is row)
                  (is (eql td (db-table-test-char-row-c row))))))))))))



(let ((vdb-counts (make-hash-table :test #'equal)))
  (defun dummy-vdb-sequencer (table vdb &optional reset)
    (when reset
      (setf vdb-counts (make-hash-table :test #'equal)))
    (or (gethash vdb vdb-counts)
        (setf (gethash vdb vdb-counts) (make-hash-table :test #'equal)))
    (let ((count-hash (gethash vdb vdb-counts)))
      (let ((count (gethash table count-hash)))
        (if count
            (incf (gethash table count-hash))
            (setf (gethash table count-hash) 1))))))

(def-db-table test-vdb-seq
    "test custom sequence"
  *dummy-test-app*
  ((vs ss-vdb)
   (int2 (or null integer))
   (foo string))
  (((vs v1) :ascending) ((vs s1) :ascending))
  nil
  (:vdb-sequence (vs v1) (vs s1)))

(deftest test-vdb-sequencer
  (let ((dk1 "fooo")
        (dk2 "bar"))
    (is (= 1 (dummy-vdb-sequencer dk1 22 t)))
    (is (= 2 (dummy-vdb-sequencer dk1 22)))
    (is (= 1 (dummy-vdb-sequencer dk2 22)))
    (is (= 1 (dummy-vdb-sequencer dk1 23)))
    (is (= 2 (dummy-vdb-sequencer dk2 22))))
  (let ((*vdb-sequencer* #'dummy-vdb-sequencer))
    (let ((table *db-table-test-vdb-seq*))
      (db-with-conn (*db* (test-spec 1) #'test-db-ctor)
        (db-with-transaction ('test-vdb-sequencer)
          (let ((*drop-table-ignore-no-exist* t))
            (drop-table table )
            (create-table table))
          (let ((row (make-db-table-test-vdb-seq-row
                      (make-ss-vdb :v1 22 :s1 nil) 567 "fooo")))
            (let ((row (db-insert-row row)))
              (is (= 1 (ss-vdb-s1 (db-table-test-vdb-seq-row-vs row))))
              (let ((dbrow (db-test_vdb_seq-select-pk 22 1)))
                (is (equalp row dbrow))))))))))



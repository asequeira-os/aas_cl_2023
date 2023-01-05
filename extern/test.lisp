(in-package :extern-test)

(deftest enum-tests
  (is (db-enum-test))
  (let ((v (vector +enum-drink-pepsi+ +enum-drink-tea+
                   +enum-drink-coke+ +enum-drink-coffee+ )))
    (let ((vs (sort (copy-seq v) #'enum<))
          (vs2 (reverse (sort (copy-seq v) #'enum>)))
          (prev -1))
      (is vs)

      (loop for drink across vs do
           (is (< prev (extern::enum-elm-ord (enum-drink-elm drink))))
           (setf prev (extern::enum-elm-ord (enum-drink-elm drink))))
      (is (equalp vs vs2))
      ))
  (let* ((json (with-output-to-string (op)
                 (aas-rpc:sout-object aas-rpc:+json-format+ op 'enum-drink
                                      +enum-drink-tea+)))
         (obj (with-input-from-string (ip json)
                (funcall (aas-rpc::get-deserializer 'enum-drink)
                         aas-rpc:+json-format+ ip 'enum-drink))))
    (is (equal json "\"tea\""))
    (is (eq obj +enum-drink-tea+)))

  (let ((vdb (extern:enum-vector-from-db 'enum-drink "1 3 0")))
    (is (every #'eq vdb
               (vector +enum-drink-coffee+ +enum-drink-pepsi+ +enum-drink-tea+)))

    (is (equalp "1 3 0" (extern:enum-vector-to-db 'enum-drink vdb)))))

(db-base:def-db-table enumtest-1
    "test basic enum"
  db-base-test::*dummy-test-app*
  ((i1 (or null integer))
   (et (or null enum-drink))
   (xx string))
  (i1)
  ((:not-unique allrows ((xx)) ))
  (:auto-increment))

(deftest db-enum-test
  (db-with-conn (*db* (db-base-test::test-spec 1) #'db-base-test::test-db-ctor)
    (let ((table *db-table-enumtest-1*))
      (let ((db-base::*drop-table-ignore-no-exist* t))
        (db-base::drop-table table))
      (db-base::create-table table)
      (db-insert-row (make-db-table-enumtest-1-row nil +enum-drink-coke+ "x") )
      (db-insert-row (make-db-table-enumtest-1-row nil nil "x") ) ;; null enum test
      (db-insert-row (make-db-table-enumtest-1-row nil +enum-drink-pepsi+ "x") ))
    (let ((rows (db-enumtest_1-select-allrows "x")))
      (is rows)
      (is (= 3 (length rows)))
      (mapc (lambda (row exp)
              (is row)
              (is (eq (db-table-enumtest-1-row-et row) exp)))
            rows (list +enum-drink-coke+ nil +enum-drink-pepsi+)))))

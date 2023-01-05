(in-package :db-base)

(define-constant +ident-postgres+ 'postgres)

(defstruct postgres-conn-spec
  (host nil :type string)
  (user nil :type string)
  (password nil :type string)
  (db-name nil :type string))

(defstruct postgres-conn
  (spec nil :type postgres-conn-spec)
  (conn nil :type cl-postgres:database-connection)
  (transaction-name nil :type (or null symbol))
  (affected-rows nil)
  (transaction-depth 0)
  (statement-counter 0)
  (statements (make-hash-table :test #'eql)))

(defun nulls-to-postgres (cols)
  (mapcar (lambda (col)
            (if col
                col
                :null)) cols))

(defun postgres-to-null (value)
  (if (eql value :null)
      nil
      value))

(cl-postgres:def-row-reader xed-row-reader (fields)
  (loop :while (cl-postgres:next-row)
        :collect (loop :for field :across fields
                       :collect (postgres-to-null
                                 (cl-postgres:next-field field)))))

(defmethod db-begin-transaction-impl ((conn postgres-conn) name)
  (let ((transaction-name (postgres-conn-transaction-name conn)))
    (if transaction-name
        (cl-postgres:exec-query (postgres-conn-conn conn)
                                (format nil "savepoint X_~A"
                                        (ident-lisp-to-db name)))
        (progn
          (assert (zerop (postgres-conn-transaction-depth conn)))
          (cl-postgres:exec-query (postgres-conn-conn conn) "BEGIN")
          (setf (postgres-conn-transaction-name conn) name)))
    (setf (postgres-conn-affected-rows conn) nil)
    (incf (postgres-conn-transaction-depth conn))))

(defmethod db-commit-transaction-impl ((conn postgres-conn) name)
  (setf (postgres-conn-affected-rows conn) nil)
  (decf (postgres-conn-transaction-depth conn))
  (if (zerop (postgres-conn-transaction-depth conn))
      (progn
        (cl-postgres:exec-query (postgres-conn-conn conn) "COMMIT")
        (setf (postgres-conn-transaction-name conn) nil))
      (cl-postgres:exec-query (postgres-conn-conn conn)
                              (format nil "release savepoint X_~A"
                                      (ident-lisp-to-db name)))))


(defmethod db-rollback-transaction-impl ((conn postgres-conn) name)
  (setf (postgres-conn-affected-rows conn) nil)
  (decf (postgres-conn-transaction-depth conn))
  (if (zerop (postgres-conn-transaction-depth conn))
      (progn
        (cl-postgres:exec-query (postgres-conn-conn conn) "ROLLBACK")
        (setf (postgres-conn-transaction-name conn) nil))
      (cl-postgres:exec-query (postgres-conn-conn conn)
                              (format nil "rollback to savepoint X_~A"
                                      (ident-lisp-to-db name)))))


(defmethod db-select-impl ((conn postgres-conn) sql &rest parameters)
  (let ((stmt-name
         (postgres-prepare-statement conn sql)))
    (multiple-value-bind
          (rows affected-rows)
        (cl-postgres:exec-prepared (postgres-conn-conn conn) stmt-name
                                   (nulls-to-postgres parameters)
                                   #'xed-row-reader)
      (setf (postgres-conn-affected-rows conn) affected-rows)
      rows)))

(defmethod db-select-single-impl
    ((conn postgres-conn) sql &rest parameters)
  (let ((rows (apply #'db-select-impl conn sql parameters)))
    (when rows
      (first (first rows)))))

(defmethod db-modified-row-count-impl ((conn postgres-conn))
  (postgres-conn-affected-rows conn))

(defmethod db-insert-impl ((conn postgres-conn) sql &rest parameters)
  (let ((stmt-name
         (postgres-prepare-statement conn sql)))
    (multiple-value-bind
          (rows affected-rows)
        (cl-postgres:exec-prepared (postgres-conn-conn conn) stmt-name
                                   (nulls-to-postgres parameters)
                                   #'xed-row-reader)
      (setf (postgres-conn-affected-rows conn) affected-rows)
      (when rows
        (assert (= 1 (length rows)))
        (first (first rows))))))

(defmethod db-delete-impl ((conn postgres-conn) sql &rest parameters)
  (let ((stmt-name
         (postgres-prepare-statement conn sql)))
    (multiple-value-bind
          (ignore affected-rows)
        (cl-postgres:exec-prepared (postgres-conn-conn conn) stmt-name
                                   (nulls-to-postgres parameters)
                                   #'cl-postgres:ignore-row-reader)
      (declare (ignorable ignore))
      (setf (postgres-conn-affected-rows conn) affected-rows))))

(defmethod db-ddl-impl ((conn postgres-conn) sql)
  (handler-bind
      ((cl-postgres:postgresql-warning
        (lambda (pw)
          (let ((msg (format nil "~A" pw)))
            (when (or (search "PRIMARY KEY will create implicit index"
                              msg)
                      (search "CREATE TABLE will create implicit sequence"
                              msg))
              (muffle-warning))))))
    (setf (postgres-conn-affected-rows conn) nil)
    (cl-postgres:exec-query (postgres-conn-conn conn) sql)))


(defmethod db-disconnect ((conn postgres-conn))
  (cl-postgres:close-database (postgres-conn-conn conn)))

(defmethod db-connect ((db-type (eql +ident-postgres+)) spec)
  (check-type spec postgres-conn-spec)
  (let ((conn (cl-postgres:open-database
               (postgres-conn-spec-db-name spec)
               (postgres-conn-spec-user spec)
               (postgres-conn-spec-password spec)
               (postgres-conn-spec-host spec))))
    (make-postgres-conn :spec spec :conn conn)))

(defun postgres-prepare-statement (conn sql)
  (let ((statements (postgres-conn-statements conn)))
    (let ((stmt-name (gethash sql statements)))
      (if stmt-name
          stmt-name
          (let ((stmt-name
                 (format nil "stmt~A"
                         (incf (postgres-conn-statement-counter conn)))))
            (cl-postgres:prepare-query (postgres-conn-conn conn)
                                       stmt-name sql)
            (setf (postgres-conn-affected-rows conn) nil)
            (setf (gethash sql statements) stmt-name)
            stmt-name)))))


(in-package :db-base)

(defvar *db-pool-lock* (mp-make-lock "db-pool"))
(defvar *db-pool* (make-hash-table :test #'equalp))
(defvar *db-conn-to-queue* (make-hash-table :test #'eq))
(defvar *db-thread-local* nil)
(defvar *db-trace-level* 1)

(defvar *db* nil
  "the default db object")

(define-constant db-col-type-integer 'integer)

(define-constant db-index-ascending 'ascending)
(define-constant db-index-descending 'descending)

(defstruct db-base
  (disconnector)
  (conn-spec)
  (conn))

(defun db-trace (level msgformat &rest args)
  (when (<= level *db-trace-level*)
    (apply #'log:log-info 1 msgformat args)))

(defun db-from-pool (conn-spec db-ctor)
  (mp-with-lock (*db-pool-lock*)
    (let ((conn-queue (gethash conn-spec *db-pool*)))
      (unless conn-queue
        (setf conn-queue  (make-queue))
        (setf (gethash conn-spec *db-pool*) conn-queue))
      (or (dequeue conn-queue)
          (let ((db-ob (funcall db-ctor conn-spec)))
            (setf (gethash db-ob *db-conn-to-queue*) conn-queue)
            (db-trace 2 "new db conn ~A~%" conn-spec)
            db-ob)))))


(defun db-to-pool (db-ob)
  (mp-with-lock (*db-pool-lock*)
    (enqueue (gethash db-ob *db-conn-to-queue*) db-ob)))

;;when emptying the pool:
;;close all connections in each db spec queue
;;I empty each conn spec queue
;;but I leave the queue alive since there may be a conn given out
;;this should be  minor memory leak only, since I don't expect
;;arbitrary large number of distinct conn specs
(defun db-clear-pool ()
  (mp-with-lock (*db-pool-lock*)
    (maphash (lambda (db-spec queue)
               (declare (ignorable db-spec))
               (iterate-queue queue
                              (lambda (db-ob)
                                (funcall (db-base-disconnector db-ob) db-ob)
                                (remhash db-ob *db-conn-to-queue*)))
               (empty-queue queue))
             *db-pool*)))

;;todo 5 might need to handle declarations
(defmacro db-with-conn ((var spec db-ctor) &body body)
  (let ((spec-gs (gensym "db-spec"))
        (conn-gs (gensym "db-conn")))
    `(let ((,spec-gs ,spec))
       (let* ((*db-thread-local* (or *db-thread-local*
                                     (make-hash-table :test #'equalp)))
              (,conn-gs (gethash ,spec-gs *db-thread-local*)))
         (if ,conn-gs
             (let ((,var ,conn-gs))
               ,@body)
             (progn
               (let* ((,conn-gs (db-from-pool ,spec-gs ,db-ctor))
                      (,var ,conn-gs))
                 (setf (gethash ,spec-gs *db-thread-local*) ,conn-gs)
                 (unwind-protect
                      (progn
                        ,@body)
                   (remhash ,spec-gs *db-thread-local*)
                   (db-to-pool ,conn-gs)))))))))

(defmacro db-with-transaction ((tx-name &optional (db-ob '*db*)) &body body)
  (let ((db-ok (gensym "txflag"))
        (tx-name-gs (gensym "tx-name"))
        (conn-gs (gensym "db")))
    `(let ((,db-ok nil)
           (,tx-name-gs ,tx-name)
           (,conn-gs ,db-ob))
       (db-begin-transaction ,tx-name-gs ,conn-gs)
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,db-ok t))
         (if ,db-ok
             (db-commit-transaction ,tx-name-gs ,conn-gs)
             (db-rollback-transaction ,tx-name-gs ,conn-gs))))))


(defun db-begin-transaction (name db-ob)
 (db-begin-transaction-impl (db-base-conn db-ob) name))

(defun db-commit-transaction (name db-ob)
  (db-commit-transaction-impl (db-base-conn db-ob) name))

(defun db-rollback-transaction (name db-ob)
  (db-rollback-transaction-impl (db-base-conn db-ob) name))

(defun db-select (db-ob sql &rest parameters)
  (apply #'db-select-impl (db-base-conn db-ob) sql parameters))

(defun db-select-single (db-ob sql &rest parameters)
  (apply #'db-select-single-impl (db-base-conn db-ob) sql parameters))

(defun db-modified-row-count (db-ob)
  (db-modified-row-count-impl (db-base-conn db-ob)))

(defgeneric ident-lisp-to-db (symbol/string))

(defmethod ident-lisp-to-db ((symbol/string null))
  "")

(defmethod ident-lisp-to-db (symbol/string)
  (substitute #\_ #\/
              (substitute #\_ #\-
                          (string-downcase (if (symbolp symbol/string)
                                               (symbol-name symbol/string)
                                               symbol/string)))))

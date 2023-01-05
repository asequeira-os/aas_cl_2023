;;key value storage
;;

(defpackage :assoc-db
  (:use :common-lisp )
  (:export :make-db
           :set-db
           :get-db
           :get-db-keys
           :get-db-values
   ))

(in-package :assoc-db)


;;backend for using assoc list
(defun make-assoc-db (test)
  (list nil test))

(defun set-assoc-db (db key value)
  (let ((cons (assoc key (first db) :test (second db))))
    (if cons
        (setf (cdr cons) value)
        (setf (first db) (acons key value (first db))))))

(defun get-assoc-db (db key )
  (let ((cons (assoc key (first db)  :test (second db))))
    ;;(let ((cons (assoc key (first db)  )))
    (if cons
        (values (cdr cons) t)
        (values nil nil))))

(defun get-assoc-db-keys (db)
  (let ((keys nil))
    (mapcar (lambda (k) (push (car k) keys)) (first db))
    keys))

(defun get-assoc-db-values (db)
  (let ((keys nil))
    (mapcar (lambda (k) (push (cdr k) keys)) (first db))
    keys))


;;interface
;;using assoc backend
(defun make-db (&key (test #'string-equal))
  (make-assoc-db test))

(defun set-db (db key value)
  (set-assoc-db db key value))

(defun get-db (db key)
  (get-assoc-db db key))

(defun get-db-keys (db)
  (get-assoc-db-keys db))

(defun get-db-values (db)
  (get-assoc-db-values db))

;;;tests
;;for now in the same file
(aseq-test:deftest make-db-test
  (let ((db (make-db :test #'equalp)))
    (aseq-test:is (not (null db)))
    (multiple-value-bind (value found-key)
        (get-db db 'foo)
      (aseq-test:is (or (null value) (null found-key)))
      (let ((klist (list 'a   'x   "ee" 567 db 'foo))
            (vlist (list "av" 'foo "ee" db  'm nil)))
        (mapcar (lambda (k v)
                  (set-db db k v))
                klist vlist)
        (mapcar (lambda (k v)
                  (multiple-value-bind (value found-key)
                      (get-db db k)
                    (aseq-test:is (and found-key (eq v value)))))
                klist vlist)
        (aseq-test:is (equal klist (get-db-keys db)))
        (aseq-test:is (equal vlist (get-db-values db)))))))

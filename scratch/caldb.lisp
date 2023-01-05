(in-package :scratch)

;; calendar main table
;;i have to create table something like
;; calkey int
;; ldb    int
;; creator int
;; create_ts timestamp
;; title (string 240)
;; note  (string 240) ?
;; nextfire ??? (we need a local time storage)
;; duration (we need a local-time duration storage)


;; primary (compund) key -> ldb,int
;; ldb used is the same for this calkey as well as the creator
;; so the creator info would be in ldb with the
;; compound key 'ldb,creator'
;; nextfire - needs to be key so we can list thigs to fire



;;local time storage
;;we care only about day and seconds
;;our epoch will be Mar 1 2000 UTC to match the lib
;;but won't rely on that, by explicitly computing the diff
;;so even if the library starts using a different epoch,
;we'll be safe


;; just saving stuff for the night
;; local time object include timeozeninfo so the day and seconds theye
;; retrn are for taht timeozne
;; get india tz
;; (local-time:define-timezone *aaa* #p"/usr/share/zoneinfo/Asia/Calcutta" :load t)
;; adjust default timeozne (US PST) epoch to india
;; (local-time:local-time-adjust (local-time:encode-local-time 0 0 0 0 1 3 2000 ) *aaa*)

;; (local-time:local-time-day (local-time:encode-local-time 0 0 0 0 1 3 2000 *aaa*))


;;this will hold dbs that are in use
;; on a per request basis, so you are
;;expected to bind it on a per request basis

;;we need to track all retrived db connections and
;;return them back to pool by caling diconnect
;;
;;for now we simply connect to the test db
;; and ignore connection leaks
;;

(defvar *db-manager* nil "represents set of connections")

(defmacro with-db-manager (&body body)
  (cl-utilities:with-unique-names (done)
    `(let (,done)
       (flet ((body-fn () ,@body))
         (if *db-manager*
             (body-fn)
             (let ((*db-manager* (make-hash-table :test #'equal)))
               (unwind-protect
                    (prog1
                        (body-fn)
                      (setq ,done t))
                 (cleanup-db-connections ,done))))))))


(defun get-db (ldb)
  "get the connection for the logical db"
  (let ((conn (gethash ldb *db-manager*)))
    (unless conn
      (setf conn (setf (gethash ldb *db-manager*)
            (get-db-raw ldb)))
      (db-start-transaction-raw conn))
    conn))


(defun cleanup-db-connections (commit )
  (when *db-manager*
    (maphash #'(lambda (ldb conn)
                 (declare (ignore ldb))
                 (db-finish-transaction-raw conn commit)
                 (disconnect-db-raw conn))
             *db-manager*)))

(defun disconnect-db-raw (conn)
  (postmodern:disconnect conn))

(defun get-db-raw (ldb)
  "this is supposed to get the physical conn for a logical db.
for now it is hardocded to in a silly way"
  (postmodern:connect "test" "antony" "" "127.0.0.1" :pooled-p t))

(defun db-start-transaction-raw (conn)
  (let ((postmodern:*database* conn))
    (postmodern:execute "BEGIN")))

(defun db-finish-transaction-raw (conn commit)
  (let ((postmodern:*database* conn))
  (if commit
      (postmodern:execute "COMMIT")
      (postmodern:execute "ABORT"))))



#|

\(clsql:def-view-class calm ()
  ((calid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :calid)
   (ldb
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :ldb)
   (creator
    :db-constraints :not-null
    :type integer
    :initarg :ldb))
;;.........
  (:base-table calm))

\(defun create-schema (db)
  (clsql:create-view-from-class 'calm :database db :transactions t))
|#


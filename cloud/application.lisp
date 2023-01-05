(in-package :cloud)

(define-constant +MAX-PDB-COUNT+ 31000)

(deftype pdb-number ()
  `(integer 0 ,+MAX-PDB-COUNT+))

(defstruct application
  (id nil :type integer)
  (name nil :type string)
  (inited nil :type boolean)
  (db-name-prefix nil :type (or null string))
  (pdb-count 0 :type pdb-number)
  (vdb-count 0 :type fixnum))

(defstruct vdb-sequence
  (table nil :type db-table)
  (vdb nil :type integer)
  (val 0 :type integer)
  (inc 0 :type integer))

(defstruct app-db-meta
  (vdb-pdb-map  nil :type (or null vector)) ;;vector of length vdb-count
  (vdb-disabled nil :type (or null vector)) ;;bit vector - set if disabled
  (pdb-host-map nil :type (or null vector)) ;;vector of length pdb-count
  (pdb-nodes nil :type (or null vector)) ;;vector of length pdb-count
  (pdb-seq-locks nil :type (or null vector));;sequence locks
  (sequencers nil :type (or null vector)))

(defvar *app-db-meta* (make-array 200 :element-type 'app-db-meta :adjustable nil))

(defgeneric get-auth-token-hash-key (token))

(defvar *application-id-lookup* (make-hash-table :test #'equal))
(defvar *application-id-counter* -1)

(defvar *application* )

(defgeneric init (app))

(defun fix-app-id (name)
  (assert (< *application-id-counter* (length *app-db-meta*)))
  (let ((num (gethash name *application-id-lookup*)))
    (if num
        num
        (setf (gethash name *application-id-lookup*)
              (incf *application-id-counter*)))))

(defmacro define-app (name-symbol vdb-count)
  (let ((name (string-downcase (symbol-name name-symbol)))
        )
    (or (numberp vdb-count) (error "please specify vdb count"))
    (or (and (stringp name)
             (not (util:empty-string-p name)))
        (error "app name has to be a simple string"))
    (let ((app-gs (gensym "app"))
          (rpc-symbol (aas-cl:build-symbol-in-package
                       (symbol-package name-symbol) ;;todo 9 should be ok
                       "get-user-host")))
      `(progn
         (let ((,app-gs (make-application :id (fix-app-id ,name)
                                          :name ,name :vdb-count ,vdb-count)))
           (aas-rpc:def-rpc string ,rpc-symbol
               (:application ,app-gs :export-rpc-function nil)
               ()
             ;;following is a bit kludgy
             ;;we can't allow this in auth since auth host is assigned by orig-login
             (when (equalp "auth" (application-name cloud:*application*))
               (error "should not call get-user-host for auth"))
             (get-key-host (get-auth-token-hash-key *auth-token*)))
           (add-table-to-schema *db-table-db-num* ,app-gs)
           (add-table-to-schema *db-table-sequence* ,app-gs)
           ,app-gs)))))

(defun set-application-db-count (app count)
  "set the physical db count for this app"
  (let ((vdb-count (application-vdb-count app)))
    (assert (<= 0 count +MAX-PDB-COUNT+))
    (assert (<= count vdb-count))
    (if (zerop vdb-count)
        (assert (zerop count)) ;;db less app
        (if (zerop (application-pdb-count app))
            (progn ;; first time call
              (let ((app-db-meta (make-app-db-meta)))
                (setf (aref *app-db-meta* (application-id app)) app-db-meta)
                (setf (application-pdb-count app) count)
                (setf (app-db-meta-vdb-pdb-map app-db-meta)
                      (make-array vdb-count :element-type 'pdb-number
                                  :adjustable nil :initial-element 0))
                (setf (app-db-meta-vdb-disabled app-db-meta)
                      (make-array vdb-count :element-type 'bit :initial-element 0
                                  :adjustable nil))
                (setf (app-db-meta-pdb-host-map app-db-meta)
                      (make-array count :element-type 'string
                                  :adjustable nil :initial-element 0))
                (setf (app-db-meta-pdb-seq-locks app-db-meta)
                      (make-array count :adjustable nil :initial-element nil))
                (setf (app-db-meta-pdb-nodes app-db-meta)
                      (make-array count :element-type '(or nil cloud-db-node)
                                  :adjustable nil :initial-element nil))))
            (or (= count (application-pdb-count app))
                (error "db count can not change")))))
  app)

(defun set-db-name-prefix (prefix app)
  (setf (application-db-name-prefix app) prefix))

(defun get-app-db-meta (app)
  (aref *app-db-meta* (application-id app)))

(defun set-pdb-host (pdb host &optional (app *application*))
  (add-trusted-host host)
  (setf (aref (app-db-meta-pdb-host-map (get-app-db-meta app)) pdb) host))

(defun set-pdb-host-range (db-start db-end host &optional (app *application*))
  (loop for db from db-start to db-end do
       (set-pdb-host db host app)))

(defun get-pdb-host (pdb  &optional (app *application*))
  (aref (app-db-meta-pdb-host-map (get-app-db-meta app)) pdb))

(defun get-vdb-pdb (vdb  &optional (app *application*))
  (aref (app-db-meta-vdb-pdb-map (get-app-db-meta app)) vdb))

(defun get-vdb-host (vdb  &optional (app *application*))
  (get-pdb-host (get-vdb-pdb vdb app) app))

(defun set-vdb-pdb (vdb pdb &optional (app *application*))
  "custom mapping for the ultimate flexibility"
  (assert (< -1 pdb (application-pdb-count app)))
  (assert (< -1 vdb (application-vdb-count app)))
  (setf (aref (app-db-meta-vdb-pdb-map (get-app-db-meta app)) vdb) pdb))

(defun set-vdb-range-to-pdb (vdb-start vdb-end pdb &optional (app *application*))
  "range distribution for the not so lazy"
  (loop for vdb from vdb-start to vdb-end do
       (set-vdb-pdb vdb pdb app)))

(defun distribute-vdb-over-pdb (&optional (app *application*))
  "even distribution for the lazy"
  (let ((pdb-count (application-pdb-count app)))
    (loop for vdb from 0 to (1- (application-vdb-count app)) do
         (set-vdb-pdb vdb (mod vdb pdb-count) app))))

;;hoping this will be useful in moving individual vdbs
(defun disable-vdb (vdb &optional (app *application*))
  "call this to disable individual vdb"
  (let* ((app-db-meta (get-app-db-meta app))
         (bit-vector (app-db-meta-vdb-disabled app-db-meta)))
    (setf (aref bit-vector vdb) 1)))

(defmacro with-application (application &body body)
  `(let ((*application* ,application)
         (*vdb-sequencer* #'generate-sequence))
     (or (null *application*)
         (application-inited *application*)
         (error "application ~A is not inited/setup" *application*))
     ,@body))

(defmethod init (app)
  (let ((*application* app)
        (app-db-meta (get-app-db-meta app)))
    (unless (zerop (application-vdb-count app))
      (assert (not (null (application-db-name-prefix app))))
      (let ((pdb-count (application-pdb-count app)))
        (setf (app-db-meta-sequencers (get-app-db-meta app))
              (make-array pdb-count :adjustable nil :fill-pointer nil))
        (loop for i from 0 to (1- pdb-count) do
             (when (is-my-pdb i)
               (setf (aref (app-db-meta-pdb-seq-locks app-db-meta) i)
                     (mp-make-lock (format nil "~A pdb ~A" (application-name app) i)))
               (setf (aref (app-db-meta-pdb-nodes app-db-meta) i)
                     (db-cloud:init-node app (application-db-name-prefix app) i))
               (init-sequences i app)))
        (db-base:db-clear-pool)))
    (setf (application-inited app) t)))


(defun is-my-db-node (pdb-node &optional (app *application*))
  (is-my-pdb (cloud-db-node-pdb pdb-node) app))

(defun is-my-pdb (pdb &optional (app *application*))
  (equal (host:my-hostname) (get-pdb-host pdb app)))

(defun is-my-vdb (vdb &optional (app *application*))
  (is-my-pdb (get-vdb-pdb vdb app) app))

(defun key-to-vdb (key  &optional (app *application*))
  (mod (key-to-number key) (application-vdb-count app)))

(defun key-to-pdb (key  &optional (app *application*))
  (get-vdb-pdb (key-to-vdb key app) app))

(defun get-db-node (vdb &optional (app *application*))
  (let ((app-db-meta (get-app-db-meta app)))
    (when (= 1 (aref (app-db-meta-vdb-disabled app-db-meta) vdb))
      (error "vdb ~A is offline for maintenance" vdb))
    ;;this is the one and only place to get to the db node
    (aref (app-db-meta-pdb-nodes app-db-meta) (get-vdb-pdb vdb app))))

(defun key-to-db-node (key &optional (app *application*))
  (get-db-node (key-to-vdb key app) app))

(let ((2p31 (expt 2 31)))
  (defun ext-db-key (string)
    ;;better than trying to deal with 2p32 unsigned in postgres
    (mod (cipher:crc32 string) 2p31)))

(defun key-to-number (key)
  (if (integerp key)
      key
      (if (stringp key)
          (ext-db-key key)
          (error "don't know to make key to number for ~A" key))))

(defun is-my-key (key &optional (app *application*))
  (is-my-pdb (key-to-pdb key app) app))

(defun get-key-host (key &optional (app *application*))
  (get-pdb-host (key-to-pdb key app) app))

(defun verify-db-num-conn (vdb conn)
  (let* ((pdb (get-vdb-pdb vdb))
         (node (cloud-db-node conn))
         (node-pdb (cloud-db-node-pdb node)))
    (unless (= pdb node-pdb)
      (break))
    (assert (= pdb node-pdb))
    t))

(defmacro with-db-node ((var node &optional (app '*application*))
                        &body body)
  (let ((db-node (gensym "node"))
        (app-gs (gensym "app")))
    `(let ((,db-node ,node)
           (,app-gs ,app))
       (unless (is-my-db-node ,db-node ,app-gs)
         (error "~A is not my db" ,db-node))
       (db-base:db-with-conn (,var ,db-node #'db-cloud:cloud-db-ctor)
         ,@body))))

(defmacro with-db-for-key ((var key &optional (app '*application*)) &body body)
  (let ((key-gs (gensym))
        (app-gs (gensym))
        (db-node (gensym)))
    `(let* ((,key-gs ,key)
            (,app-gs ,app)
            (,db-node (key-to-db-node ,key-gs ,app-gs)))
       (with-db-node (,var ,db-node ,app-gs) ,@body))))

(defun init-sequences (pdb app)
  (when (is-my-pdb pdb app)
    (with-db-node (db-base:*db* (aref (app-db-meta-pdb-nodes (get-app-db-meta app)) pdb) app)
      (let* ((vdb-count (application-vdb-count app))
             (vdb-vector (make-array vdb-count :adjustable nil :fill-pointer nil)))
        (setf (aref (app-db-meta-sequencers (get-app-db-meta app)) pdb) vdb-vector)
        (loop for vdb from 0 to (1- vdb-count) do
             (when (is-my-vdb vdb app)
               (init-vdb-sequences app vdb vdb-vector)))))))

(defun init-vdb-sequences (app vdb vdb-vector)
  (let ((table-vector (make-array (1+ *table-id-counter*) ;;hacky
                                  :element-type 'vdb-sequence
                                  :adjustable nil :fill-pointer nil)))
    (setf (aref vdb-vector vdb) table-vector)
    (iterate-tables
     app
     (lambda (table)
       (when (db-table-vdb-seq-cols table)
         (let ((vdb-sequence (make-vdb-sequence :table table :vdb vdb)))
           (load-vdb-sequence vdb-sequence)
           (setf (aref table-vector (db-table-id table)) vdb-sequence)))))))

(defun load-vdb-sequence (vdb-sequence)
  (let ((row (db-sequence-select-pk
              (vdb-sequence-vdb vdb-sequence)
              (db-table-name (vdb-sequence-table vdb-sequence)))))
    (if row
        (load-vdb-sequence-from-row vdb-sequence row)
        (insert-initial-vdb-sequence vdb-sequence))))

(defun insert-initial-vdb-sequence (vdb-sequence)
  (let ((row (make-db-table-sequence-row
              (vdb-sequence-vdb vdb-sequence)
              (db-table-name (vdb-sequence-table vdb-sequence))
              0)))
    (db-base:db-insert-row row)))

(define-constant +VDB-SEQUENCE-UPDATE-THRESHOLD+ 100)

(defun load-vdb-sequence-from-row (vdb-sequence row)
  (incf (db-table-sequence-row-seq row) +VDB-SEQUENCE-UPDATE-THRESHOLD+)
  (db-base:db-update-row row)
  (setf (vdb-sequence-val vdb-sequence) (db-table-sequence-row-seq row)))

(defun update-vdb-sequence (mutex vdb-sequence)
  (mp-with-lock (mutex)
    (incf (vdb-sequence-val vdb-sequence))
    (incf (vdb-sequence-inc vdb-sequence))
    (when (<= +VDB-SEQUENCE-UPDATE-THRESHOLD+ (vdb-sequence-inc vdb-sequence))
      (setf (vdb-sequence-inc vdb-sequence) 0)
      (let ((row (db-sequence-select-pk
                  (vdb-sequence-vdb vdb-sequence)
                  (db-table-name (vdb-sequence-table vdb-sequence)))))
        (assert row nil "missing sequence data in db")
        (setf (db-table-sequence-row-seq row)
              (vdb-sequence-val vdb-sequence))
        (db-base:db-update-row row)))))

(defun generate-sequence (table vdb)
  (let* ((db db-base:*db*)
         (node (cloud-db-node db))
         (pdb (cloud-db-node-pdb node))
         (app *application*)
         (table-id (db-table-id table))
         (app-db-meta (get-app-db-meta app)))
    (let ((vdb-sequence (aref
                         (aref
                          (aref (app-db-meta-sequencers app-db-meta) pdb)
                          vdb)
                         table-id)))
      (assert (is-my-vdb vdb) nil "bad vdb")
      (update-vdb-sequence (aref (app-db-meta-pdb-seq-locks app-db-meta) pdb)
                           vdb-sequence)
      (vdb-sequence-val vdb-sequence))))

(defvar *vdb* )

(defmacro def-rpc-with-proxy (return-type name options rpc-args db-num-form &body body)
  (multiple-value-bind (forms decl doc)
      (util:parse-body body :documentation t)
    (let* ((argnames)
           (host-to-host (parse-rpc-option :host-to-host-only options))
           (anonymous (parse-rpc-option :anonymous options))
           (application (parse-rpc-option :application options))
           (proxy-func (rpc-proxy-function host-to-host anonymous)))
      (loop for (arg ) on rpc-args by #'cddr do
           (push arg argnames))
      (setf argnames (nreverse argnames))
      (let ((db-num (gensym "dbnum"))
            (db-node (gensym "dbnode")))
        `(aas-rpc:def-rpc ,return-type ,name ,options ,rpc-args
           ,@(when doc `(,doc))
           ,@decl
           (if (eq *application* ,application)
               (let* ((,db-num ,db-num-form)
                      (cloud:*vdb* ,db-num)
                      (,db-node (cloud:get-db-node ,db-num)))
                 (if  ,db-node
                      (with-db-node (db-base:*db* ,db-node)
                        ,@forms)
                      (,proxy-func (get-vdb-host ,db-num)
                                   #',name
                                   ,@(unless anonymous
                                             (list '*auth-token*))
                                   ,@argnames)
                      ))
               (with-application ,application
                 (,proxy-func (get-vdb-host 0) ;;todo 2 pick random seed host
                              #',name
                              ,@(unless anonymous
                                        (list '*auth-token*))
                              ,@argnames)))
           )))))

(defun rpc-proxy-function (host-to-host anonymous)
  (if (and host-to-host (not anonymous))
      'call-remote-trust-impersonate
      (if (and host-to-host anonymous)
          'call-remote-trust
          (if (and (not host-to-host) anonymous)
              'call-remote-anon
              (if (and (not host-to-host) (not anonymous))
                  'call-remote-impersonate
                  (error "bug in rpc-proxy-function"))))))


;;around method to default locale from http header Accept-Language
(defmethod aas-rpc::call-rpc-function :around (package name argassoc)
  (declare (ignorable package name argassoc))
  (let ((aas-local-time:*utc-now* (aas-local-time:utc-now)))
    (if (boundp 'i18n:*locale*)
        (call-next-method)
        (i18n:with-locale (locale-from-http-header)
          (call-next-method)))))

(defun locale-from-http-header ()
  (if (and (boundp '*accept-language*)
           (= 5 (length *accept-language*)))
      (let* ((parts (split-sequence:split-sequence #\- *accept-language*))
             (lcstr (concatenate 'string
                                 (string-downcase (first parts)) "-"
                                 (string-upcase (second parts)))))
        (if (i18n:supported-locale-p lcstr)
            lcstr
            "en-US"))
      "en-US"))

(aas-rpc:def-rpc-struct db-id
    (db -1 :type integer)
  (id nil :type (or null integer)))

(defmethod db-base:get-struct-columns ((symbol (eql 'db-id)))
  '((db integer)
    (id (or null integer))))

(defun make-db-id-from-db (&key db id)
  (make-db-id :db db :id id))

(aas-rpc:def-rpc-struct db-id-type
    (type nil :type string)
  (dbid nil :type db-id))

(defmethod db-base:get-struct-columns ((symbol (eql 'db-id-type)))
  '((type string)
    (dbid db-id)))

(defun make-db-id-type-from-db (&key type dbid)
  (make-db-id-type :type type :dbid dbid))

(defvar +test-app+ (define-app test 10))


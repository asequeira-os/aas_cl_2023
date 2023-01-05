(in-package :db-base-test)

(defstruct sub-substruct
  (intcol 0 :type integer))

(defmethod get-struct-columns ((symbol (eql 'sub-substruct)))
  '((intcol integer)))

(defun make-sub-substruct-from-db (&key intcol)
  (make-sub-substruct :intcol intcol))

(defstruct test-table-substruct
  (intcol1 0 :type integer)
  (strcol1 nil :type (or null string))
  (strcol2)
  (ss nil :type (or null sub-substruct))
  (strcol3))

(defmethod get-struct-columns ((symbol (eql 'test-table-substruct )))
  '((intcol1 (or null integer))
    (strcol2 string)
    (ss sub-substruct)))

(defun make-test-table-substruct-from-db (&key intcol1 strcol2 ss)
  (make-test-table-substruct :intcol1 intcol1 :strcol1 "foo"
                             :strcol2 strcol2 :ss ss
                             :strcol3 "bar"))

;;sub null test meta
(defstruct subnullstruct
  (intcol 0 :type integer)
  (scol nil :type string))

(defmethod get-struct-columns ((symbol (eql 'subnullstruct)))
  '((intcol integer)
    (scol string)))

(defun make-subnullstruct-from-db (&key intcol scol)
  (make-subnullstruct :intcol intcol :scol scol))


;;; clos class storage test prep
(defclass bc ()
  ((bc-key :initarg :bc-key)))

(defclass dc (bc)
  ((dc1 :initarg :dc1)))

(defun dc-from-key (key)
  (make-instance 'dc :bc-key key :dc1 (format nil "dc1 from ~A" key)))

(defmethod db-base:get-struct-columns ((symbol (eql 'bc)))
  `((bc-key string)))

(defun make-bc-from-db (&key bc-key)
  (dc-from-key bc-key))

;;single character storage

(def-db-table test-char
    "single character column test table"
  *dummy-test-app*
  ((pkid (or null integer))
   (c standard-char)
   )
  (pkid)
  nil
  (:auto-increment))

;;vdb sequence
(defstruct ss-vdb
  (v1 nil :type integer)
  (s1 nil :type (or null integer)))

(defmethod db-base:get-struct-columns ((symbol (eql 'ss-vdb)))
  '((v1 integer)
    (s1 integer)))

(defun make-ss-vdb-from-db (&key v1 s1)
  (make-ss-vdb :v1 v1 :s1 s1))




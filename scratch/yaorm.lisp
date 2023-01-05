;ideas for yet another ORM
;have a base class for all db field types
;have a slot to indicate db field name
;have a slot to indicate db null allowed or not
;have a derived class for each db field type
; eg varchar integer
;this will have a slot for holding the value
;we don't need anyhing special for null cause nil will do
;
;have a base class for all tables
;have derived class for each table
;derived class one db field slot for every column in that table
;
;db fields will also be useful for index definitions
;
;every table class needs an identifying fields list
;use this as a unique index but not as primary key (clustered index causes issues)
;
;for insert call delete followed by insert
;insert sets values for all columns
;db table class for each table will have a pair of prepared statements
;one for deltee and one for insert, no updates allowed
;
;instantiate any db class by setting identifying field values
;then either retrieve from db or set values from application
;
;update is done by setting key fields, retrieving from db
;modifying fields, calling delete followed by insert
;
;an insatnce once created with key fields does not allow any hnageto the key fields
;have dirty/clean control using accessors (base class methods cna set dirty)
;
;parititoning
; two possibilities - in the base db class or a speicifc mixin,
;maybe with base db class, 
;
;be able to generate all the tables based on a subset sql DDL
;
;turn around and generate table creation ddl for whatever rdbms i need to support
;create sql for the delete and insert statements 

(defclass db-table () () )


(defun field-spec-sql (foo)
  (let ((list nil))
    (dolist (field-info foo)
      (push field-info list))
    (nreverse list)))

  ;(let ((list nil)) (dolist (field-info '((id (:key pk)) (foo :bar bar)) ) (push field-info list)) (nreverse list)))

(defmacro define-table (table (fields &rest field-info) (indexes &rest index-info))
  `(print '(create table ',table
            ,(field-spec-sql field-info))))
                                   

;(macroexpand-1 '

 (define-table calendar-master 
    (fields (id (:key pk) (:type integer))
            (ldb (:key pk) (:type integer))
            (creator (:key creator-k) (:type integer))
            (creator-ldb (:key creator-k) (:type integer)))
  (indexes (primary :primary)
           (creator :non-unique)))

; )
;expected expansion - alist of two strings
;; ("create table calendar_master (
;;   id integer,
;;   ldb integer,
;;   creator integer,
;;   creator-ldb integer,

;;   constraint pk  primary key (id,ldb) );",
;;   "create index calendar_master_creator_k on calendar_master
;;    (creator, creator-ldb);")

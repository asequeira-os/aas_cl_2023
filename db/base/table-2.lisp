(in-package :db-base)

;;todo 2 util funcs below should go elsewhere
(defun make-stack (initial)
  (list (copy-seq (coerce initial 'list))))

(defun pop-stack (stack)
  (pop (car stack)))

(defun push-stack (stack elm)
  (push elm (car stack)))

(defun list-join (list joiner &key (key #'identity))
  (let (res)
    (dolist (elt list)
      (push (funcall key elt) res)
      (push joiner res))
    (pop res)
    (nreverse res)))

(defun list-to-string (list &optional converter)
  "all elements of list should to be strings"
  (if converter
      (list-to-string (mapcar converter list))
      (apply #'concatenate 'string list)))

;;;;end of util funcs

(defvar *sql-parameter-number*)

(defvar *db-tables* (make-hash-table :test #'eql)
  "all defined table objects")

(defstruct table-column
  (name nil :type (or null string))
  (db-type)
  (ident nil :type symbol)
  (allow-null nil :type boolean)
  (type)
  (num nil :type (or null integer))
  (documentation nil :type (or null string))
  (reader)
  (writer))

(defmethod make-load-form ((self table-column) &optional environment)
  (declare (ignorable environment))
  `(make-table-column :name ',(table-column-name self)
                      :db-type ',(table-column-db-type self)
                      :ident ',(table-column-ident self)
                      :allow-null ',(table-column-allow-null self)
                      :type ',(table-column-type self)
                      :documentation ',(table-column-documentation self)
                      :num ',(table-column-num self)))

(defvar *schema-tables* (make-hash-table :test #'eq))

(defstruct db-table
  (doc :read-only :type string)
  (name :read-only :type string)
  (id :read-only :type integer)
  (schemas :read-only)
  (columns :read-only) ;;tree
  (columns-list :read-only);;flat list
  (row-binder :read-only)
  (primary-key :read-only)
  (indices :read-only)
  (sequence-col :read-only)
  (vdb-seq-cols :read-only)
  (row-type :read-only)
  )

(defvar *table-id-counter* -1)
(defvar *table-id-lookup* (make-hash-table :test #'eql))
(defvar *allow-null* nil)

(defun add-table-to-schema (table schema)
  (setf (gethash schema (db-table-schemas table)) schema)
  (let ((tables (gethash schema *schema-tables*)))
    (if (not tables)
        (progn
          (setf (gethash schema *schema-tables*) (make-hash-table :test #'equal))
          (add-table-to-schema table schema))
        (setf (gethash (db-table-name table) tables) table))))

(defun fix-table-id (table-symb)
  (let ((num (gethash table-symb *table-id-lookup*)))
    (if num
        num
        (setf (gethash table-symb *table-id-lookup*)
              (incf *table-id-counter*)))))

(defun pref+ident-to-db (prefix ident)
  (if (or (null prefix) (zerop (length prefix)))
      (ident-lisp-to-db ident)
      (concatenate 'string prefix "_" (ident-lisp-to-db ident))))

(defun lisp-primitive-to-db (lisp)
  (ecase lisp
    (boolean "boolean")
    (standard-char "CHARACTER")
    (decimal "NUMERIC")
    (string "VARCHAR")
    (symbol "VARCHAR")
    (integer "INTEGER")))

(defun db-primitive-p (type)
  (case type
    (boolean t)
    (standard-char t)
    (decimal t)
    (string t)
    (symbol t)
    (integer t)
    (otherwise nil)))

(defun db-type-to-lisp (db-type)
  ;;for varchar don't care about length on lisp side
  (if (alexandria:starts-with-subseq "varchar" db-type :test #'string-equal)
      (if (search "NULL" db-type :test #'string-equal)
          '(or null string)
          'string)
      (if (alexandria:starts-with-subseq "numeric" db-type :test #'string-equal)
          (if (search "NULL" db-type :test #'string-equal)
              '(or null decimal)
              'decimal)
          (error "unhandled db-type ~A" db-type))))

(defgeneric get-struct-columns (symbol))

(defparameter *nulled-cols* (make-hash-table :test #'eq))

(defun get-nullable-struct-columns (symbol)
  (if *allow-null*
      (if (gethash symbol *nulled-cols*)
          (gethash symbol *nulled-cols*)
          (let ((nulled-cols (create-nulled-cols (get-struct-columns symbol))))
            (setf (gethash symbol *nulled-cols*) nulled-cols)
            nulled-cols
            ))
      (get-struct-columns symbol)))

(defun create-nulled-cols (cols)
  (let ((ncl nil))
    (dolist (col cols)
      (let ((col-name (first col))
            (col-type (second col)))
        (push (list col-name (create-nulled-col col-type)) ncl)))
    (nreverse ncl)))

(defun create-nulled-col (col-type)
  (etypecase col-type
    (symbol (list 'or 'null col-type))
    (cons (if (eq 'null (second col-type) )
              col-type
              (list 'or 'null col-type)))))

(defvar *column-num*) ;;internal use

(defun  make-compound-column (prefix column col-type documentation)
  (ecase (first col-type)
    (or (assert (eql 'null (second col-type)))
        (if (db-primitive-p (third col-type))
            (make-table-column
             :name (pref+ident-to-db prefix column)
             :ident column
             :allow-null t
             :type col-type
             :num (incf *column-num*)
             :documentation documentation
             :db-type (concatenate 'string
                                   (lisp-primitive-to-db (third col-type))
                                   " NULL"))
            (let ((*allow-null* t))
              (make-symbol-columns prefix column (third col-type) documentation))))))

(defun make-symbol-columns (prefix column col-type documentation)
  (case col-type
    ((integer string boolean standard-char symbol decimal)
     (make-table-column
      :name (pref+ident-to-db prefix column)
      :ident column
      :allow-null *allow-null*
      :type col-type
      :num (incf *column-num*)
      :documentation documentation
      :db-type (concatenate 'string
                            (lisp-primitive-to-db col-type)
                            (if *allow-null* " NULL " " NOT NULL "))))
    (otherwise
     (let ((subprefix (pref+ident-to-db prefix column)))
       (make-table-column
        :name nil;;(pref+ident-to-db prefix column)
        :ident column
        :allow-null *allow-null*
        :type col-type
        :num nil
        :documentation documentation
        :db-type (make-table-columns
                  subprefix
                  (get-nullable-struct-columns col-type)))))))

(defun make-table-columns (prefix columns)
  "takes column definition from table def macro to
generate sql column definitions"
  (let ((db-columns (list)))
    (dolist (column columns)
      (let ((col-type (rest column))
            (column (first column)))
        (case (first col-type)
          (:db-type
           (push (make-table-column
                  :name (pref+ident-to-db prefix column)
                  :ident column
                  :allow-null *allow-null*
                  :db-type (second col-type)
                  :documentation (third col-type)
                  :num (incf *column-num*)
                  :type (db-type-to-lisp (second col-type))) db-columns))
          (otherwise (etypecase (first col-type)
                       (cons (push (make-compound-column
                                    prefix column (first col-type) (second col-type))
                                   db-columns))
                       (symbol (push (make-symbol-columns
                                      prefix column
                                      (first col-type) (second col-type))
                                     db-columns)))))))
    (let ((result (nreverse db-columns)))
      result)))

(defun flatten-db-columns (columns)
  (alexandria:flatten
   (mapcar (lambda (col)
             (let ((tcdt (table-column-db-type col)))
               (etypecase tcdt
                 (list (flatten-db-columns tcdt))
                 (atom col))))
           columns)))

(defun slots-from-columns (columns)
  (mapcar (lambda (col)
            (list (table-column-ident col) nil
                  :type (col-to-row-struct-slot col)))
          columns))

;;slot definition for the synthesized row struct
(defun col-to-row-struct-slot (col)
  (let ((base (table-column-type col)))
    (etypecase base
      (cons base)
      (symbol (if (table-column-allow-null col) ;;not handling integer range case?
                  (list 'or 'null base)
                  base)))))

(defun symbol-type-ctor (symbol)
  (let ((name (format nil "make-~A-from-db" (symbol-name symbol))))
    (let ((*package* (symbol-package symbol)))
      (build-symbol name))))

(defun odd-list-elements (list)
  (let ((count 0)
        (out nil))
    (dolist (elm list)
      (incf count)
      (if (oddp count)
          (push elm out)))
    (nreverse out)))

(defun even-list-elements (list)
  (let ((count 0)
        (out nil))
    (dolist (elm list)
      (incf count)
      (if (evenp count)
          (push elm out)))
    (nreverse out)))

(defun null-check-wrap (fn &rest args)
  (let* ((p-v-list (first args))
         (p-list (odd-list-elements p-v-list))
         (v-list (even-list-elements p-v-list)))
    ;;(break)
    `(let ((v-eval-list (list ,@v-list)))
       (if (every #'null v-eval-list)
           nil
           (apply #',fn (alexandria:flatten (pairlis (list ,@p-list) v-eval-list)))))))

(defun generate-struct-binder (ctor-args column)
  (let ((ctor (symbol-type-ctor (table-column-type column)))
        (code (list)))
    (dolist (col (table-column-db-type column))
      (let ((tcdt (table-column-db-type col)))
        (etypecase tcdt
          (atom (push (keyword-from-symbol (table-column-ident col)) code)
                (push (pop-stack ctor-args) code))
          (list (push (keyword-from-symbol (table-column-ident col)) code)
                (push (generate-struct-binder ctor-args col) code)))))
    (if *allow-null*
        `,(null-check-wrap ctor (nreverse code))
        `(,ctor ,@(nreverse code)))
    ))

(defun generate-binder (ctor ctor-name ctor-args columns-tree deep-bind)
  (let ((ctor-args-copy-2 (copy-seq ctor-args))
        (ctor-args (make-stack ctor-args))
        (make-row-args (list))
        (make-row-code (list))
        (code (list)))
    (dolist (column columns-tree)
      (let ((tcdt (table-column-db-type column))
            (*allow-null* (table-column-allow-null column)))
        (push (table-column-ident column) make-row-args)
        (push (keyword-from-symbol (table-column-ident column)) make-row-code)
        (push (table-column-ident column) make-row-code)
        (etypecase tcdt
          (atom ;;(break)
           (push (keyword-from-symbol (table-column-ident column)) code)
           (let ((arg (pop-stack ctor-args))
                 (db-type (table-column-type column)))
             (push (postgres-to-lisp db-type arg) code)))
          (list
           (push (keyword-from-symbol (table-column-ident column)) code)
           (push (generate-struct-binder ctor-args column) code)))))

    (setf code (nreverse code))
    (setf make-row-code (nreverse make-row-code))
    ;;(pprint code)
    (if deep-bind
        `(defun ,ctor-name  ,ctor-args-copy-2
           (,ctor ,@code))
        `(defun ,ctor-name  ,(reverse make-row-args)
           (,ctor ,@make-row-code)))))

(defun find-sequence-col (pk db-columns-list options)
  (when  (find :auto-increment options :test #'eq)
    (assert (= 1 (length pk)) )
    (let ((seq-candidate
           (ident-lisp-to-db
            (let ((pk-col (first pk)))
              (etypecase pk-col
                (atom pk-col)
                (list (etypecase (first pk-col)
                        (atom (first pk-col))
                        (list (compound-col-joiner (first pk-col))))))))))
      (let ((db-col (find seq-candidate db-columns-list :test #'string-equal
                          :key #'table-column-name)))
        (assert (equalp '(or null integer) (table-column-type db-col)))
        db-col))))

(defun find-vdb-sequence (options db-columns-list)
  "vdb spec looks like (... :vdb-sequence 'vdb-col-spec' 'seq-col-spec' ...)"
  (let ((opt (position :vdb-sequence options :test #'eq)))
    (when opt
      (assert (>= (length options) (+ 3 opt)) nil
              "bad VDB sequence spec")
      (let ((vdb-col-spec (nth (1+ opt) options))
            (seq-col-spec (nth (+ 2 opt) options)))
        (let ((vdb-col (find-index-column vdb-col-spec db-columns-list))
              (seq-col (find-index-column seq-col-spec db-columns-list)))
          (list vdb-col seq-col))))))



(defun make-parameters-sql (columns)
  (list-to-string
   (list-join
    (mapcar (lambda (col)
              (format nil "~A = $~A "
                      (table-column-name col) (incf *sql-parameter-number*) ))
            columns) " AND ")))

(defun make-range-parameters-sql (range-columns)
  (list-to-string
   (list-join
    (mapcar (lambda (query-col)
              (let* ((col (query-col-col query-col))
                     (range (query-col-range query-col))
                     (col-name (table-column-name col)))
                (if range
                    (format nil "~A >= $~A AND ~A < $~A "
                            col-name (incf *sql-parameter-number*)
                            col-name (incf *sql-parameter-number*))
                    (format nil "~A = $~A "
                            col-name (incf *sql-parameter-number*) )
                    ))
              )
            range-columns) " AND ")))

(defun make-order-by-sql (columns)
  (list-to-string
   (list-join
    (mapcar (lambda (col)
              (format nil "~A"
                      (table-column-name col) ))
            columns) ", ")))

(defun make-insert-sql (name columns seq-col)
  (let ((*sql-parameter-number* 0))
    (let ((columns-string
           (list-to-string
            (list-join columns ", " :key #'table-column-name)))
          (params-string
           (list-to-string
            (list-join columns ", "
                       :key
                       (lambda (col)
                         (declare (ignorable col))
                         (format nil "$~A" (incf *sql-parameter-number*)))))))
      (util:intern-string
       (concatenate 'string
                    "INSERT INTO " name
                    " (" columns-string ") VALUES (" params-string ")"
                    (if seq-col
                        (format nil " RETURNING ~A " (table-column-name seq-col))
                        ""))))))

(defun find-index-column (index-col-spec db-columns-list)
  (or
   (let ((col-key
          (etypecase index-col-spec
            (atom index-col-spec)
            (list (compound-col-joiner index-col-spec)))))
     (find (ident-lisp-to-db col-key) db-columns-list :test #'string-equal
           :key #'table-column-name))
   (error "index column ~A not found" index-col-spec)))

(defun path-to-column-impl (column column-tree path)
  (dolist (col column-tree)
    (typecase (table-column-db-type col)
      (atom (when (= (table-column-num col) (table-column-num column))
              (push-stack path col)
              (return-from path-to-column-impl path)))
      (list (push-stack path col)
            (let ((found
                   (path-to-column-impl column
                                        (table-column-db-type col) path)))
              (if found
                  (return-from path-to-column-impl found)
                  (pop-stack path)))))))

(defun path-to-column (column column-tree)
  (let ((path (path-to-column-impl column column-tree (make-stack nil))))
    (unless (first path)
      (error "bug"))
    (nreverse (first path))))

;;this is to be used for writing sequence column into the row obj
;;general biding does not use this
(defun get-accessor-symbol (type column)
  (if (struct-symbol-p type)
      (build-symbol-in-package (symbol-package type)
                               type "-" (table-column-ident column))
      (build-symbol-in-package (symbol-package type)
                               (table-column-ident column))))

(defun column-writer (struct-type column column-tree)
  (let ((path (path-to-column column column-tree)))
    (if (= 1 (length path))
        (let* ((col (pop path))
               (is-struct (struct-symbol-p struct-type)))
          (compile nil
                   `(lambda (row-obj slot-value)
                      (setf ,(if is-struct
                                 `(,(get-accessor-symbol struct-type col)
                                    row-obj)
                                 `(slot-value row-obj ,(get-accessor-symbol struct-type col)))
                            slot-value))))
        (let ((aclist (deep-reader 'row-obj struct-type path)))
          ;;(break)
          ;;(pprint path)
          ;;(pprint aclist)
          (compile nil
                   `(lambda (row-obj slot-value)
                      (if (and ,@(reverse (rest (reverse aclist))))
                          (setf ,(first (reverse aclist)) slot-value))))))))


(defun deep-reader (obj-symbol struct-type path)
  (let* ((type struct-type)
         (aclist (mapcar (lambda (col)
                           (let ((is-struct (struct-symbol-p type))
                                 (res (get-accessor-symbol type col)))
                             (setf type (table-column-type col))
                             (cons res is-struct) ))
                         path)))
    ;;last type is important - since we do leaf type specific post processing
    ;;(print type)
    ;;(pprint aclist)
    (let ((sqlist (maplist #'identity (reverse aclist))))
      ;;(pprint sqlist)
      (let ((code (reverse (mapcar (lambda (sublist)
                                     (let ((res obj-symbol))
                                       (dolist (ac.type (reverse sublist))
                                         (let ((ac (car ac.type))
                                               (is-struct (cdr ac.type)))
                                           (if is-struct
                                               (setf res (list ac res))
                                               (setf res (list 'slot-value res (list 'quote ac))))))
                                       res)) sqlist))))
        (values code type)))))


(defun symbol-to-string (symbol &optional allow-null)
  (if (and allow-null (null symbol))
      nil
      (format nil "~A::~A" (package-name (symbol-package symbol))
              (symbol-name symbol))))

(defun string-to-symbol (string)
  (if (null string)
      nil
      (let ((pcs (split-sequence:split-sequence #\: string)))
        (let ((symbol (find-symbol (third pcs) (find-package (first pcs )))))
          (or symbol
              (error "symbol ~A not found" string))))))


(defun boolean-to-postgres (boolean)
  (if boolean
      boolean
      "false"))

(defun postgres-to-boolean (bit)
  (if (eql bit :null)
      nil
      bit))

(defun lisp-to-db-transformer (struct-type column field-name)
  (let ((is-struct (struct-symbol-p struct-type))
        (slot (get-accessor-symbol struct-type field-name)))
    (let ((sa (if is-struct
                  (symbol-function slot)
                  (lambda (struct)
                    (slot-value struct slot)))))
      (let ((transformer (lisp-to-postgres (table-column-type column))))
        (lambda (struct)
          (funcall transformer (funcall sa struct)))))))

(defvar *where-arg-trasnformers* (make-hash-table :test #'eq))
(defun set-where-arg-trasnformer (type fn)
  (setf (gethash type *where-arg-trasnformers*) fn))

(set-where-arg-trasnformer 'symbol 'symbol-to-string)
(set-where-arg-trasnformer 'boolean 'boolean-to-postgres)

(defun lisp-to-where-clause-param (arg column )
  (let* ((column-type (table-column-type column)))
    (let ((fn (gethash column-type *where-arg-trasnformers*)))
      (if fn
          `(,fn ,arg)
          arg))))

;; (if (eq column-type 'symbol)
;;     `(symbol-to-string ,arg)
;;     (if (eq column-type 'boolean)
;;         `(boolean-to-postgres ,arg)
;;         arg))))

(defun lisp-to-postgres (lisp-type)
  (let* ((allow-null (and (consp lisp-type)
                          (eql 'or (first lisp-type))
                          (eql 'null (second lisp-type))))
         (base-type (if allow-null
                        (third lisp-type)
                        lisp-type))
         (transformer
          (case base-type
            (symbol
             (lambda (val)
               (symbol-to-string val allow-null)))
            (boolean
             #'boolean-to-postgres)
            (standard-char
             (lambda (val)
               (and val (format nil "~A" val))))
            (otherwise
             #'identity))))
    transformer))

(defun postgres-to-lisp (db-type arg)
  (case db-type
    (symbol
     `(if (symbolp ,arg)
          ,arg
          (string-to-symbol ,arg)))
    (boolean
     `(postgres-to-boolean ,arg))
    (standard-char
     `(aref ,arg 0))
    (otherwise
     arg)))

;;this is to be used for reading column to set into sql insert parameters
;;general biding does not use this
(defun column-reader (struct-type column column-tree)
  (let ((path (path-to-column column column-tree)))
    (if (= 1 (length path))
        (lisp-to-db-transformer struct-type column (pop path))
        (multiple-value-bind (code leaf-type)
            (deep-reader 'row-obj struct-type path)
          (let ((transformer (lisp-to-postgres leaf-type)))
            ;;(pprint code)
            (compile nil
                     `(lambda (row-obj)
                        (let ((resp (and ,@code)))
                          (funcall ,transformer resp)))))))))

(defvar *vdb-sequencer*)

(defun get-column-writer (table column)
  (or (table-column-writer column)
      (setf (table-column-writer column)
            (column-writer (db-table-row-type table)
                           column (db-table-columns table)))))

(defun create-vdb-sequence-setter (table)
  (let* ((vdb-seq-cols (db-table-vdb-seq-cols table))
         (vdb-col (when vdb-seq-cols (first vdb-seq-cols)))
         (vdb-seq-col (when vdb-seq-cols (second vdb-seq-cols))))
    (unless vdb-seq-cols
      (return-from create-vdb-sequence-setter #'identity))
    (lambda (row-obj)
      (let ((vdb (funcall (table-column-reader vdb-col) row-obj)))
        (assert vdb nil "can't sequence null vdb")
        (let ((seq (funcall (table-column-reader vdb-seq-col) row-obj)))
          (if seq ;;already set
              row-obj
              (progn
                (funcall (get-column-writer table vdb-seq-col) row-obj
                         (funcall *vdb-sequencer* table vdb))
                row-obj)))))))

(defun make-row-inserter (table)
  (let ((name (db-table-name table))
        (seq-col (db-table-sequence-col table))
        (columns-list (db-table-columns-list table)))
    (let ((insert-sql (make-insert-sql name columns-list seq-col))
          (insert-sql-no-seq
           (make-insert-sql name (remove seq-col columns-list) seq-col))
          (vdb-seq-setter (create-vdb-sequence-setter table))
          (seq-writer (if seq-col
                          (get-column-writer table seq-col)
                          #'identity)))
      (lambda (db-ob row-obj)
        (let* ((row-obj (funcall vdb-seq-setter row-obj))
               (auto-inc (and seq-col
                              (null (funcall (table-column-reader seq-col) row-obj)))))
          (let ((id
                 (apply #'db-insert-impl
                        (db-base-conn db-ob)
                        (if auto-inc
                            insert-sql-no-seq
                            insert-sql)
                        (mapcar (lambda (column)
                                  (funcall (table-column-reader column)
                                           row-obj))
                                (if auto-inc
                                    (remove seq-col columns-list)
                                    columns-list)))))
            (when auto-inc
              (funcall seq-writer row-obj id))
            row-obj))))))


(defun set-column-readers-writers (struct-name db-columns db-columns-list)
  (dolist (column db-columns-list)
    (setf (table-column-reader column)
          (column-reader struct-name column db-columns))
    ))

(defun create-select-sql (table-name columns query-columns)
  (let* ((index-columns (mapcar #'query-col-col query-columns))
         (*sql-parameter-number* 0)
         (order-by-sql (make-order-by-sql index-columns)))
    (util:intern-string
     (build-string "select "
                   (list-to-string (list-join columns ", "
                                              :key #'table-column-name ))
                   " FROM " table-name
                   " WHERE "
                   (make-parameters-sql index-columns)
                   " ORDER BY "
                   order-by-sql))))

(defun create-range-sql (table-name columns query-columns)
  (let* ((index-columns (mapcar #'query-col-col query-columns))
         (*sql-parameter-number* 0)
         (order-by-sql (make-order-by-sql index-columns)))
    (util:intern-string
     (build-string "select "
                   (list-to-string (list-join columns ", "
                                              :key #'table-column-name ))
                   " FROM " table-name
                   " WHERE "
                   (make-range-parameters-sql query-columns)
                   " ORDER BY "
                   order-by-sql))))

(defun create-query-sql (table-name db-columns query)
  (multiple-value-bind (strq args)
      (create-query-where db-columns query)
    ;;(print strq)
    (values (util:intern-string
             (build-string "select "
                           (list-to-string (list-join db-columns ", "
                                                      :key #'table-column-name ))
                           " FROM " table-name
                           " WHERE " strq
                           ))
            args)))


(defun create-query-where (db-columns query)
  (let* ((*sql-parameter-number* 0)
         (logq (create-logical-query-where db-columns query))
         (pvector (make-array 200 :initial-element nil)))
    ;;(print logq)
    (let ((strq (with-output-to-string (s)
                  (query-where-print s logq pvector))))
      (values strq (coerce (subseq pvector 0 *sql-parameter-number*) 'list)))))



(defun query-where-print (stream query pvector)
  (princ "(" stream)
  (dolist (qelm query)
    (if (symbolp qelm)
        (format stream " ~A " qelm )
        (if (query-elm-p qelm)
            (query-where-elm-print stream qelm pvector)
            (query-where-print stream qelm pvector))))
  (princ ")" stream))

(defun query-where-elm-print (stream qelm pvector)
  (if (query-elm-var-p qelm)
      (progn
        (format stream "$~A" (query-elm-pnum qelm))
        (setf (aref pvector (1- (query-elm-pnum qelm)))
              qelm))
      (princ (table-column-name (query-elm-col qelm)) stream)))

(defstruct query-elm
  (var-p)
  (var)
  (pnum)
  (col))

(defun create-logical-query-where (db-columns query)
  ;;(print query)
  (let ((opr (first query))
        (args (rest query)))
    (ecase opr
      ((or and) (list-join
                 (mapcar  (lambda (arg)
                            (create-logical-query-where db-columns arg))  args) opr ))
      ((= < > <= >=) (list (make-query-elm :var-p t :var (first args)
                                           :pnum (incf *sql-parameter-number*)
                                           :col (find-index-column (second args) db-columns))
                           opr
                           (make-query-elm :var-p nil
                                           :col (find-index-column (second args) db-columns))
                           )))))




(defstruct query-col
  (col nil :type table-column)
  (range nil))

(defun create-range-query-args (range-columns range)
  (if range
      (let ((list))
        (dolist (rc range-columns)
          (let ((col (query-col-col rc)))
            (if (query-col-range rc)
                (progn
                  (push (cons (build-symbol (table-column-name col ) "-low")
                              col) list)
                  (push (cons (build-symbol (table-column-name col ) "-high")
                              col) list))
                (push (cons (build-symbol (table-column-name col))
                            col) list))))
        (nreverse list))
      (mapcar (lambda (query-col)
                (let ((col (query-col-col query-col)))
                  (cons (build-symbol (table-column-name col))
                        col))
                )
              range-columns))
  )

(defun create-select-function (table-name db-columns index-columns
                               binder unique index-name range)
  (let* ((index-args (create-range-query-args index-columns range))
         (fn-name (build-symbol "db-" table-name "-select-"
                                (if range "range-" "")
                                index-name))
         (select-sql (if range
                         (create-range-sql table-name db-columns index-columns)
                         (create-select-sql table-name db-columns index-columns))))
    `(let ((binder (symbol-function ',binder)))
       (defun ,fn-name
           (,@(mapcar #'car index-args) cl:&optional (db-ob *db*))
         (let ((rows (db-base::db-select
                      db-ob
                      ,select-sql
                      ,@(mapcar (lambda (arg-col)
                                  (lisp-to-where-clause-param
                                   (car arg-col) (cdr arg-col)))
                                index-args ))))
           ,(if (and unique (not range))
                `(unless (null rows)
                   (assert (null (cdr rows)))
                   (apply binder (first rows)))
                `(unless (null rows)
                   (let ((list (list)))
                     (dolist (row rows)
                       (push (apply binder row) list))
                     (nreverse list)))))))))

(defun create-query-function (table-name db-columns query binder)
  (assert (= 2 (length query)))
  (let* ((fn-name (build-symbol "db-" table-name "-select-" (pop query))))
    (multiple-value-bind (select-sql query-args)
        (create-query-sql table-name db-columns (pop query))
      ;;(break)
      `(let ((binder (symbol-function ',binder)))
         (defun ,fn-name
             (,@(mapcar #'query-elm-var query-args) cl:&optional (db-ob *db*))
           (let ((rows (db-base::db-select
                        db-ob
                        ,select-sql
                        ;;todo 2 add lisp-to-where-clause-param call for boolean...
                        ,@(mapcar (lambda (query-arg)
                                    (lisp-to-where-clause-param
                                     (query-elm-var query-arg) (query-elm-col query-arg)))
                                  query-args))))
             (unless (null rows)
               (let ((list (list)))
                 (dolist (row rows)
                   (push (apply binder row) list))
                 (nreverse list)))))))))

(defun find-index-columns (db-columns index-definition)
  (mapcar (lambda (col-spec)
            (make-query-col :col (find-index-column
                                  (etypecase col-spec
                                    (cons (first col-spec))
                                    (symbol col-spec))
                                  db-columns)
                            :range (and (consp col-spec)
                                        (find :range col-spec))))
          index-definition))

;;need to generate functions <table name>-select-pk
;;and <table name>-select-<index> for each index
(defun create-select-functions (table-name pk indices db-columns binder)
  (let ((code (list)))
    (when pk
      (let* ((index-columns (find-index-columns db-columns pk)))
        (push (create-select-function table-name db-columns index-columns
                                      binder t "pk" nil)
              code)))
    (when indices
      (dolist (index indices)
        (let ((index (copy-seq index)))
          (let* ((unique (eql :unique (pop index)))
                 (index-name (pop index))
                 (index-columns (find-index-columns db-columns index)))
            (when (some #'query-col-range index-columns)
              (push (create-select-function table-name db-columns index-columns
                                            binder unique index-name t)
                    code))
            (push (create-select-function table-name db-columns index-columns
                                          binder unique index-name nil)
                  code)))))
    (nreverse code)))

(defun create-query-functions (table-name queries db-columns binder)
  (let ((code))
    (dolist (query queries)
      (push (create-query-function table-name db-columns (copy-seq query) binder)
            code))
    (nreverse code)))


(defgeneric db-insert-row (row-ob &optional db-ob ))
(defgeneric db-delete-row (row-ob &optional db-ob ))

;;todo 3 allow for computed columns
;;first use - compute db number based on login in auth table user-base
(defmacro def-db-table (name documentation schema columns primary-key indices
                        &optional (options) (queries))
  (let* ((*allow-null* nil)
         (table-name (ident-lisp-to-db name))
         (db-columns (let ((*column-num* 0))
                       (make-table-columns "" columns))) ;;tree
         (db-columns-list (flatten-db-columns db-columns)) ;;flat list

         (select-column-symbols (mapcar (lambda (col)
                                          (build-symbol (table-column-name col)))
                                        db-columns-list))
         (struct-name (build-symbol "db-table-" name "-row"))
         (ctor-name (build-symbol "make-" (symbol-name struct-name)))
         (binder-name (build-symbol ctor-name "-binder"))
         (hidden-ctor (build-symbol ctor-name "%"))
         (struct-slots (slots-from-columns db-columns))
         (table-var-gs (gensym "table-var"))
         (table-var (build-symbol "*db-table-" (symbol-name name) "*"))
         (binder (generate-binder hidden-ctor binder-name select-column-symbols db-columns t))
         (ctor (generate-binder hidden-ctor ctor-name select-column-symbols db-columns nil))
         (sequence-col (find-sequence-col primary-key db-columns-list options))
         (vdb-seq-cols (find-vdb-sequence options db-columns-list))
         (id (fix-table-id name))
         (select-functions (create-select-functions
                            table-name primary-key indices db-columns-list binder-name))
         (query-functions (create-query-functions
                           table-name queries db-columns-list binder-name))
         )
    ;; (print struct-name)
    ;;(pprint binder)
    ;;(pprint ctor)
    ;;(print sequence-col)
    ;;(pprint select-functions)
    ;;(pprint query-functions)

    `(progn
       (defstruct (,struct-name (:constructor ,hidden-ctor))
         ,@struct-slots)
       ,binder
       ,ctor
       (defparameter ,table-var
         (let ((,table-var-gs (make-db-table :doc ,documentation
                                             :name ,table-name
                                             :id ,id
                                             :schemas ,(make-hash-table :test #'eq)
                                             :columns ',db-columns
                                             :columns-list ',db-columns-list
                                             :row-binder   ',ctor-name
                                             :primary-key ',primary-key
                                             :indices ',indices
                                             :sequence-col ,sequence-col
                                             :vdb-seq-cols ',vdb-seq-cols
                                             :row-type ',struct-name
                                             ) ))
           (set-column-readers-writers ',struct-name ',db-columns ',db-columns-list)
           (setf (gethash ',name *db-tables*) ,table-var-gs)
           ,@select-functions
           ,@query-functions

           (let ((row-inserter (make-row-inserter ,table-var-gs)))
             (defmethod db-base:db-insert-row ((row-ob ,struct-name) &optional (db-ob *db*))
               (funcall row-inserter db-ob row-ob)))
           (add-table-to-schema ,table-var-gs ,schema)
           ,table-var-gs))

       (let* ((*sql-parameter-number* 0)
              (pk-columns (mapcar #'query-col-col
                                  (find-index-columns ',db-columns-list ',primary-key)))
              (params-sql (make-parameters-sql pk-columns))
              (delete-sql (util:intern-string
                           (format nil "DELETE FROM ~A WHERE ~A"
                                   ,table-name params-sql))))
         (defmethod db-base:db-delete-row ((row-ob ,struct-name) &optional (db-ob *db*))
           (when row-ob
             (let ((conn (db-base-conn db-ob)))
               (apply #'db-delete-impl
                      conn delete-sql
                      (mapcar (lambda (column)
                                (funcall (table-column-reader column)
                                         row-ob))
                              pk-columns))
               (or (= 1 (db-modified-row-count db-ob))
                   (error "delete count wrong 1 != ~A"
                          (db-modified-row-count db-ob)))))))
       (defmethod db-base:db-update-row ((row-ob ,struct-name) &optional (db-ob *db*))
         (and (db-delete-row row-ob db-ob)
              (db-insert-row row-ob db-ob)))

       ,table-var
       )))

(defun make-create-columns-sql (table)
  (let ((columns (db-table-columns-list table))
        (seq-col (db-table-sequence-col table)))
    (list-to-string
     (list-join
      (alexandria:flatten
       (list-join
        (mapcar (lambda (col)
                  (list (table-column-name col)
                        (if (eq col seq-col)
                            " serial "
                            (table-column-db-type col))
                        ))
                columns) ",")) " "))))

(defun order-keyword-to-db (kw)
  (ecase kw
    ((nil :ascending) "")
    (:descending "DESC")))

(defun compound-col-joiner (column)
  (ident-lisp-to-db
   (list-to-string
    (list-join column "-")
    #'build-string)))

(defun create-index-col-sql (index-cols)
  (list-to-string
   (list-join
    (alexandria:flatten
     (list-join
      (mapcar
       (lambda (index-col)
         (etypecase index-col
           (atom (list (ident-lisp-to-db index-col)
                       (order-keyword-to-db :ascending)))
           (list (etypecase (first index-col)
                   (atom (list (ident-lisp-to-db (first index-col))
                               (order-keyword-to-db (second index-col))))
                   (list (list (compound-col-joiner (first index-col))
                               (order-keyword-to-db (second index-col))))))))
       index-cols) ",")) " ")))

(defun create-primary-key-sql (pk)
  (concatenate 'string
               "PRIMARY KEY ("
               (create-index-col-sql pk)
               ")"))

(defun make-create-table-sql (table)
  (let ((create-col-sql
         (make-create-columns-sql table))
        (pk-sql (create-primary-key-sql (db-table-primary-key table))))
    (format nil "CREATE TABLE ~A (~A, ~A)"
            (db-table-name table) create-col-sql pk-sql)))

(defun create-index-sql (table index)
  (let* ((unique (eql :unique (pop index)))
         (name (pref+ident-to-db (ident-lisp-to-db (db-table-name table))
                                 (pop index)))
         (cols (create-index-col-sql index)))
    (format nil "CREATE ~A INDEX ~A ON ~A (~A)"
            (if unique "UNIQUE" "")
            name (db-table-name table) cols)))

(defun create-indexes-sql (table)
  (mapcar (lambda (index)
            (create-index-sql table index))
          (db-table-indices table)))

(defun create-table (table &key (db-ob *db*))
  (db-with-transaction ('create-table db-ob)
    (let ((tbl-sql (make-create-table-sql table)))
      (db-trace 3 "~A" tbl-sql)
      (db-ddl-impl (db-base-conn db-ob) tbl-sql)
      (let ((indexes-sql (create-indexes-sql table)))
        (dolist (index-sql indexes-sql)
          (db-trace 3 "~A~%" index-sql)
          (db-ddl-impl (db-base-conn db-ob) index-sql))))))

(defun create-all-tables (schema db-ob)
  (iterate-tables schema
                  (lambda (table)
                    (create-table table :db-ob db-ob))))

(defvar *drop-table-ignore-no-exist* nil
  "when true, drop table warnings are ignored")

(defun drop-all-tables (schema db-ob)
  (iterate-tables schema
                  (lambda (table)
                    (drop-table table :db-ob db-ob))))

(defun iterate-tables (schema callback)
  (let ((tables (gethash schema *schema-tables*)))
    (maphash  (lambda (name table)
                (declare (ignorable name))
                (funcall callback table))
              tables)))

(defun drop-table (table &key (db-ob *db*))
  (let ((msg (format nil
                     "PostgreSQL warning: table \"~A\" does not exist, skipping"
                     (db-table-name table))))
    (handler-bind
        ((cl-postgres:postgresql-warning
          (lambda (pw)
            (when (and *drop-table-ignore-no-exist*
                       (equal msg (format nil "~A" pw)))
              (muffle-warning)))))
      (db-ddl-impl (db-base-conn db-ob)
                   (build-string "drop table if exists " (db-table-name table))))))

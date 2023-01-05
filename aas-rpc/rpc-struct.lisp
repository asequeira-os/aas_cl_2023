(in-package :aas-rpc)

(defvar *rpc-format* nil)

(defvar *deserializers* (make-hash-table :test #'eq))

(defgeneric sout-object (ser-type stream type object))
(defgeneric sout-begin-object (ser-type stream name))
(defgeneric sout-end-object (ser-type stream name))
(defgeneric sout-object-attr (ser-type stream type name value))
(defgeneric sout-add-comma (ser-type stream))
(defgeneric sout-begin-vector (ser-type stream))
(defgeneric sout-end-vector (ser-type stream))
(defgeneric sout-assoc-list (ser-type stream alist rpc-meta))

(defgeneric deserialize-struct-type (ser-type stream obj-type ctor lookup))
(defgeneric deserialize-integer (ser-type stream obj-type))
(defgeneric deserialize-string (ser-type stream obj-type))
(defgeneric deserialize-boolean (ser-type stream obj-type))
(defgeneric deserialize-decimal (ser-type stream obj-type))
(defgeneric deserialize-vector (ser-type stream slot-type))
(defgeneric deserialize-null (ser-type stream))


(defgeneric deserialize-after (object)
  (:documentation
   "called after deserialization has constructed the object.
default behavior is identity"))

(defgeneric sin-begin-object (ser-type stream obj-type))
(defgeneric sin-end-object (ser-type stream obj-type))
(defgeneric sin-object-attr-name (ser-type stream obj-type))
(defgeneric sin-attr-name-value-seprator (ser-type stream))
(defgeneric sin-attr-separator (ser-type stream))
(defgeneric sin-begin-vector (ser-type stream))
(defgeneric sin-end-vector (ser-type stream ))

(defmethod sout-begin-object ((ser-type aas-json-format) stream name)
  (declare (ignorable name))
  (json-begin-object stream))

(defmethod sout-end-object ((ser-type aas-json-format) stream name)
  (declare (ignorable name))
  (json-end-object stream))

(defmethod  sout-object-attr ((ser-type aas-json-format) stream type name value)
  (json-print-attribute name stream)
  (sout-object ser-type stream type value))

(defmethod sout-add-comma ((ser-type aas-json-format) stream)
  (princ #\, stream))

(defmethod sout-object ((ser-type aas-json-format) stream (type (eql 'boolean)) object)
  (if object
      (princ "true" stream)
      (princ "false" stream)))

(defmethod sout-object ((ser-type aas-json-format) stream (type (eql 'decimal)) object)
  (princ (decimal->string object) stream))


(defmethod sout-object ((ser-type aas-json-format) stream type (object null))
  ;;todo 3 when type is passed correctly for compounds, add checks
  ;;to see null is allowed
  (declare (ignorable type))
  (princ "null" stream))

(defmethod sout-object ((ser-type aas-json-format) stream type (object number))
  (declare (ignorable type))
  (princ object stream))

(defmethod sout-object ((ser-type aas-json-format) stream type (object string))
  (declare (ignorable type))
  (json-print-primitive object stream))

(defmethod sout-begin-vector ((ser-type aas-json-format) stream)
  (princ #\[ stream))

(defmethod sout-end-vector ((ser-type aas-json-format) stream)
  (princ #\] stream))

(defmethod sout-object (ser-type stream type (object vector))
  (sout-begin-vector ser-type stream)
  (if (> (array-rank object) 1)
      (error "not supporting multi dimensional array yet")
      (let ((comma nil))
        (loop for elm across object do
             (and comma (sout-add-comma ser-type stream))
             (sout-object ser-type stream (second type) elm)
             (setf comma t))))
  (sout-end-vector ser-type stream))

(defmethod sout-assoc-list ((ser-type aas-json-format) stream (alist null) rpc-meta)
  (declare (ignorable rpc-meta))
  (princ "null" stream))

(defmethod sout-assoc-list ((ser-type aas-json-format) stream alist rpc-meta)
  (sout-begin-object ser-type stream nil)
  (let ((comma nil))
    (dolist (assoc alist)
      (when comma
        (sout-add-comma ser-type stream))
      (setf comma t)
      (let* ((param-name (car assoc))
             (param-value (cdr assoc))
             (param-meta (get-rpc-param-meta rpc-meta param-name)))
        (sout-object-attr ser-type stream (cdr param-meta) param-name param-value))))
  (sout-end-object ser-type stream nil))


(defun serialize-struct-slot (name slot)
  (let* ((slot-name (first slot))
         (slot-type (slot-type-from-slot-spec slot))
         (slot-accessor-symbol
          (aas-cl:build-symbol
           (symbol-name name) "-" (symbol-name slot-name))))
    `(sout-object-attr
      ser-type stream ',slot-type
      ',slot-name (,slot-accessor-symbol object))))

(defun rpc-struct-serializer (name &rest slots)
  (let ((comma nil) ;;comma really means field separator
        (code (list)))
    (dolist (slot slots)
      (unless (find :skip-rpc slot)
        (when comma
          (push `(sout-add-comma ser-type stream) code))
        (push (serialize-struct-slot name slot)  code)
        (setf comma t)
        ))
    `(lambda (ser-type stream object)
       (assert object);; handle null outside in method
       (sout-begin-object ser-type stream ',name)
       ,@(nreverse code)
       (sout-end-object ser-type stream ',name))))

(defun slot-type-from-slot-spec (slot)
  (let ((offset (position :type slot)))
    (or offset
        (error "can not get type from slot spec ~A" slot))
    (elt slot (1+ offset))))

(defun deserialize-compound-type (ser-type stream obj-type slot-type)
  (unless (consp slot-type)
    (error "bug. called for non compound slot ~A type ~A" slot-type obj-type))
  (ecase (first slot-type)
    (or (deserialize-or-type ser-type stream obj-type slot-type))
    (vector (deserialize-vector ser-type stream slot-type))
    (integer (deserialize-integer ser-type stream slot-type))))

(defun deserialize-or-type (ser-type stream obj-type slot-type)
  (declare (ignorable obj-type))
  (or (= 3 (length slot-type))
      (error "can not handle type ~A" slot-type))
  (let ((t1 (second slot-type))
        (t2 (third slot-type)))
    (let ((type (if (eq 'null t1)
                    t2
                    (if (eq 'null t2)
                        t1
                        (error  "can not handle type ~A" slot-type)))))
      (if (char= #\n (peek-char t stream))
          (deserialize-null ser-type stream)
          (funcall (gethash type *deserializers*)
                   ser-type stream  (third slot-type))))))

(defun expect-from-stream (expect stream)
  (when (< 0 (length expect))
    (let ((first (aref expect 0))
          (rest (subseq expect 1)))
      (unless (char= first (peek-char t stream))
        (error "expected first char ~A in ~A, got ~A"
               first expect (peek-char t stream)))
      (read-char stream)
      (map 'vector (lambda (expect)
                     (or (char= expect (peek-char t stream))
                         (error "expected ~A" expect))
                     (read-char stream)) rest))))

(defmethod deserialize-null ((ser-type aas-json-format) stream)
  (expect-from-stream "null" stream)
  nil)

(defmethod deserialize-vector ((ser-type aas-json-format) stream slot-type)
  (let ((data (list))
        (obj-type (second slot-type)))
    (sin-begin-vector ser-type stream)
    (util:while (not (sin-end-vector ser-type stream))
      (push (funcall (gethash obj-type *deserializers*)
                     ser-type stream obj-type) data)
      (and (char= #\, (peek-char t stream)) (read-char stream)))
    (coerce (nreverse data) 'vector )))

(defmethod deserialize-integer ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (json-read-integer stream))

(defmethod deserialize-string ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (json-read-string stream))

(defmethod deserialize-boolean ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (json-read-boolean stream))

(defmethod deserialize-decimal ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (decimal-from-stream stream))

(defun set-deserializer (type function)
  (setf (gethash type *deserializers*) function))

(set-deserializer
 'integer
 (lambda (ser-type stream obj-type)
   (deserialize-integer ser-type stream obj-type)))

(set-deserializer
 'string
 (lambda (ser-type stream obj-type)
   (deserialize-string ser-type stream obj-type)))

;;should not let info leak out
(set-deserializer
 'symbol
 (lambda (ser-type stream obj-type)
   (let ((name-pack-s (deserialize-string ser-type stream obj-type)))
     (let ((name-pack (split-sequence:split-sequence #\: name-pack-s)))
       (assert (= 2 (length name-pack)))
       (let* ((pkg (find-package (util:match-readtable-case (first name-pack))))
              (sym (find-symbol (util:match-readtable-case (second name-pack)) pkg)))
         sym)))))

(set-deserializer
 'boolean
 (lambda (ser-type stream obj-type)
   (deserialize-boolean ser-type stream obj-type)))

(set-deserializer
 'decimal
 (lambda (ser-type stream obj-type)
   (deserialize-decimal ser-type stream obj-type)))

(defun get-deserializer (slot-type)
  (lambda (ser-type stream obj-type)
    (let ((fn (gethash slot-type *deserializers*)))
      (or fn (error "did not find deserializer for ~A" slot-type))
      (funcall fn ser-type stream obj-type))))

(defmethod sin-begin-vector ((ser-type aas-json-format) stream)
  (let ((char (peek-char t stream)))
    (if (char= char #\[)
        (read-char stream)
        (error "did not find json array begin char ["))))

(defmethod sin-end-vector ((ser-type aas-json-format) stream )
  (let ((char (peek-char t stream)))
    (when (char= char #\])
      (read-char stream))))

(defmethod sin-begin-object ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (let ((char (peek-char t stream)))
    (if (char= char #\{)
        (read-char stream)
        (error "did not find json object begin char {"))))

(defmethod sin-end-object ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (let ((char (peek-char t stream)))
    (when (char= char #\})
      (read-char stream))))

(defmethod sin-object-attr-name ((ser-type aas-json-format) stream obj-type)
  (declare (ignorable obj-type))
  (json-read-string stream))

(defmethod sin-attr-name-value-seprator ((ser-type aas-json-format) stream)
  (assert (char= #\: (read-char stream))))

(defmethod sin-attr-separator ((ser-type aas-json-format) stream)
  (and (char= #\, (peek-char t stream)) (read-char stream)))

(defmethod deserialize-after (object)
  object)

(defmethod deserialize-struct-type ((ser-type aas-json-format) stream
                                    obj-type ctor lookup)
  (let ((ctor-args (list)))
    (sin-begin-object ser-type stream obj-type)
    (util:while (not (sin-end-object ser-type stream obj-type))
      (let ((slot-name (sin-object-attr-name ser-type stream obj-type)))
        (let ((slot-assoc (assoc slot-name lookup :test #'string-equal)))
          (unless slot-assoc
            (error "unknown slot ~A in ~A" slot-name obj-type))
          (sin-attr-name-value-seprator ser-type stream)
          (push (keyword-from-symbol slot-name) ctor-args)
          (push (funcall (cdr slot-assoc) ser-type stream obj-type) ctor-args)
          (sin-attr-separator  ser-type stream))))
    (deserialize-after (apply ctor (nreverse ctor-args)))))

(defun rpc-struct-deserializer (name &rest slots)
  (let ((lookup (list))
        (ctor-name (aas-cl:build-symbol
                    "make-" name)))
    (dolist (slot slots)
      (unless (find :skip-rpc slot)
        (let* ((slot-name (first slot))
               (slot-type (slot-type-from-slot-spec slot)))
          (push (cons slot-name
                      (if (consp slot-type)
                          (lambda (ser-type stream obj-type)
                            (deserialize-compound-type
                             ser-type stream obj-type slot-type))
                          (get-deserializer slot-type)))
                lookup))))
    `(lambda (ser-type stream obj-type)
       (declare (ignore obj-type)) ;;wrong obj-type can come
       (deserialize-struct-type ser-type stream ',name
                                (symbol-function ',ctor-name) ',lookup))))

(defun standardize-slots (slots-mv)
  (let ((slots (list)))
    (dolist (slot slots-mv)
      (push (remove :skip-rpc slot) slots))
    (nreverse slots)))

(defmacro def-rpc-struct (options &rest slots)
  (let* ((slots-mv slots)
         (standard-slots (standardize-slots slots-mv))
         (options-mv options)
         (name (if (atom options-mv)
                   options
                   (first options))))
    `(progn
       (defstruct ,options-mv ,@standard-slots)
       (let* ((name ',name)
              (slots-mv ',slots-mv)
              (serializer (compile nil (apply #'rpc-struct-serializer name slots-mv))))
         (setf (gethash name *deserializers*)
               (compile nil (apply #'rpc-struct-deserializer name slots-mv)))
         (defmethod sout-object (ser-type stream type (object ,name))
           (let ((allow-null (and (consp type)
                                  (eq 'or (first type))
                                  (eq 'null (second type)))))
             (if (null object)
                 (if allow-null
                     (sout-object ser-type stream 'null object)
                     (error "null not allowed for type ~A" type))
                 (let ((type (if allow-null
                                 (third type)
                                 type)))
                   (unless (eql type name)
                     ;;(break)
                     (error "type mismatch expect ~A got ~A" name type))
                   (assert (eql type name))
                   (funcall serializer ser-type stream object)))))))))


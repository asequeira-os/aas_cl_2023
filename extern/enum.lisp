(in-package :extern)

;;enum stuff
;; i need to support
;; rpc
;; db
;; quick comparison
;; list
;; check

(defvar *enum-hash* (make-hash-table :test #'eq))
(defvar *enum-vectors-hash* (make-hash-table :test #'eq))

(defstruct enum-elm
  (symbol)
  (string)
  (ord))

(defmethod db-base:get-struct-columns ((symbol (eql 'enum-elm)))
  '((ord integer)))

(defun make-enum-elm-from-db (&key ord)
  ord)

(defgeneric enum-vector-to-db (type vector))
(defgeneric enum-vector-from-db (type string))
(defgeneric enum-element (obj))
(defun enum-ordinal (enum)
  (enum-elm-ord (enum-element enum)))

(defmacro define-enum (name &rest vs)
  (let* ((pkg (symbol-package name))
         (enum-name (build-symbol-in-package pkg "enum-" name))
         (elm-acc (build-symbol-in-package pkg enum-name "-elm"))
         (list-symbol (build-symbol-in-package pkg "+enum-" name "-list+"))
         (from-db-ctor (build-symbol-in-package pkg "make-" enum-name "-from-db"))
         (code nil))
    `(progn
       (let ((hash (make-hash-table :test #'equal))
             (all-list nil)
             (vector (make-array (length ',vs))))
         (setf (gethash ',name *enum-hash*) hash)
         (setf (gethash ',name *enum-vectors-hash*) vector)

         (defstruct ,enum-name
           (,(build-symbol-in-package pkg "elm") nil :type enum-elm))
         (defmethod aas-rpc:sout-object (ser-type stream type (object ,enum-name))
           (declare (ignorable type))
           (aas-rpc:sout-object
            ser-type stream 'string
            (enum-elm-string (,elm-acc object))))
         (defmethod enum-element ((obj ,enum-name))
           (,elm-acc obj))

         (aas-rpc:set-deserializer
          ',enum-name
          (lambda (ser-type stream obj-type)
            (let ((string (aas-rpc::deserialize-string ser-type stream obj-type))
                  (hash (gethash ',name *enum-hash*)))
              (let ((elm (gethash string hash)))
                (unless elm
                  (error "bad enum ~A" string))
                elm))))

         (defmethod db-base:get-struct-columns ((symbol (eql ',enum-name)))
           '((elm enum-elm)))

         (defun  ,from-db-ctor (&key elm)
           ;;the elm will be just the ord and not actually enum-elm
           (let ((vector (gethash ',name *enum-vectors-hash*)))
             (aref vector elm )))

         ;;todo 2 this should be outside the macros since now I have enum-element generic
         (defmethod enum-vector-to-db ((type (eql ',enum-name)) vector)
           (if (or (null vector)
                   (zerop (length vector)))
               ""
               (db-base::list-to-string
                (db-base::list-join
                 (coerce vector 'list) " "
                 :key (lambda (e)
                        (format nil "~A" (enum-elm-ord (,elm-acc e))))))))

         (defmethod enum-vector-from-db ((type (eql ',enum-name)) string)
           (if (util:empty-string-p string)
               nil
               (coerce (mapcar (lambda (ord)
                                 (,from-db-ctor :elm ord))
                               (mapcar #'parse-integer
                                       (split-sequence:split-sequence #\space string)))
                       'vector)))


         ,(let ((dbord 0))
               (dolist (ev vs)
                 (let ((enum-elm-param (build-symbol-in-package
                                        pkg "+enum-" name "-" ev "+")))
                   (let* ((elm-name (string-downcase (symbol-name ev)))
                          )
                     (push
                      `(let nil
                         (declare (special ,enum-elm-param))
                         (export ',enum-elm-param)
                         (defvar ,enum-elm-param
                           (,(build-symbol-in-package pkg "make-enum-" name)
                             :elm (make-enum-elm :symbol ',ev
                                                 :string ,elm-name
                                                 :ord ,dbord)))
                         (setf (aref vector ,dbord) ,enum-elm-param)
                         (setf (gethash ,elm-name hash) ,enum-elm-param)
                         (push ,enum-elm-param all-list)
                         )
                      code)
                     (incf dbord)
                     ))))
         ,@(nreverse code)
         (export ',list-symbol)
         (defvar ,list-symbol (nreverse all-list))))))

(defun enum< (e1 e2)
  (<  (enum-elm-ord (enum-element e1)) (enum-elm-ord (enum-element e2)) ))
(defun enum= (e1 e2)
  (eq e1 e2))
(defun enum> (e1 e2)
  (>  (enum-elm-ord (enum-element e1)) (enum-elm-ord (enum-element e2))))
(defun enum<= (e1 e2)
  (or (eq e1 e2) (enum< e1 e2)))
(defun enum>= (e1 e2)
  (or (eq e1 e2) (enum> e1 e2)))

(defun enum-list (symbol)
  (gethash symbol *enum-vectors-hash*))

(defmacro rt-case ((case-var &key (test #'eq)) &body body-list)
  (let ((code nil)
        (var (gensym "var"))
        (test-fn (gensym "test")))
    `(let ((,test-fn ,test )
           (,var ,case-var))
       ,(dolist (body body-list)
                (let ((vv (first body)))
                  (if (eq vv t)
                      (push `(t ,@(rest body)) code)
                      (push `((funcall ,test-fn ,var ,vv)
                              ,@(rest body))
                            code))))
       (cond
         ,@(nreverse code)))))

;; (rt-case (ct :test #'eq)
;;     (bar (print "got bar"))
;;     (foo (print "got fooo")))

;;expand to
;; (cond
;;   ((funcall #'eq ct 'bar) (print "got bar"))
;;   ((funcall #'eq ct 'foo) (print "got foooo"))
;;   (t (error "unhandled ~A" ct)))










(in-package :aas-rpc)

#|
todo 4 flag minimum request version allowed by a rpc function

** secret keys for encryption
Two things (assumptions?)to note
- a given client using a given auth token will talk to a specific server.
- a host will talk to another host using token from that host
This means we need not share identical secret keys across all hosts for encrypting any tokens or cookies. each host can have it's own secret key.
If a client gets moved to another host, it will need to re-authenticate.
For http cookies, this implies we *should* not set domain

todo 1 set up known error codes
- trust expired for host to host calls
- trust invalid

|#

(defvar *request-version* nil)
;;request transport properties
;;don't set to help avoid multi thread bugs
(defvar *secure-transport* )
(defvar *client-ip* )
(defvar *post-request* )
(defvar *accept-language* )
(defvar *rpc-remote-depth* 0)
(define-constant +rpc-remote-max-depth+ 6)

(defvar *rpc-register* (make-hash-table :test #'eq))
(defvar *rpc-reverse-register* (make-hash-table :test #'eq))

(defvar *auth-token-type* nil "should be set only once on load of auth module")

;;todo 5 these two specials should have been in auth package
(defvar *auth-user* nil)
(defvar *auth-token* nil)
(defvar *trusted-host* nil "to be set by rpc mechanism")

(defstruct rpc-meta
  (name nil :type symbol)
  ;;rpc options
  ;;adding a keyword here allows automatic inclusion via the options
  ;;argument to def-rpc macro
  ;;todo 9 def-rpc option argument allows other struct members as keywords
  (allow-insecure nil)    ;;allow non secure (http) protocol when true
  (post-only t)           ;;allow non modifying (get) request when nil
  (anonymous nil)         ;;allow anonymous (no auth) access when t
  (host-to-host-only nil) ;;internal use only when t
  (application nil )
  (allow-inactive nil)
  (export-rpc-function t)
  ;;return type, arguments, and service function
  (return-type)
  (arguments)
  (function))

(defun parse-rpc-option (key options)
  (let ((offset (position key options)))
    (when offset
      (elt options (1+ offset)))))

(defgeneric make-rpc-error (error))

(define-condition rpc-error (error)
  ((cause :initarg :cause :reader rpc-error-cause))
  (:report (lambda (condition stream)
             (format stream "~%rpc error ~A :"
                     (rpc-error-cause condition)))))

(defun create-rpc-entry-log-format (name args)
  (with-output-to-string (stream)
    (format stream "RPC:(~A " name)
    (dolist (arg args)
      (format stream ":~A ~~A " arg))
    (format stream ")")))

(defmacro def-rpc (return-type name options rpc-args &body body)
  (multiple-value-bind (forms decl doc)
      (util:parse-body body :documentation t)
    (let ((argnames (list))
          (argtypes (list)))
      (unless (evenp (length rpc-args))
        (error "rpc-args should have been even items"))
      (loop for (arg arg-type) on rpc-args by #'cddr do
           (push arg argnames)
           (push (cons arg arg-type) argtypes))
      (setf argnames (nreverse argnames))
      (setf argtypes (nreverse argtypes))
      ;;(break)
      `(progn
         (defun ,name (cl:&key ,@argnames)
           ,@(when doc `(,doc))
           ,@decl
           (log:log-info 1 ,(create-rpc-entry-log-format name argnames)
                         ,@argnames)
           ,@forms)
         (let ((rpc-meta (setf (gethash ',name *rpc-register*)
                               (make-rpc-meta :name ',name
                                              ,@options
                                              :return-type ',return-type
                                              :arguments ',argtypes
                                              :function #',name))))
           (when (rpc-meta-export-rpc-function rpc-meta)
             (export ',name ,(symbol-package name))))
         (setf (gethash #',name *rpc-reverse-register*) ',name)))))

(defun get-rpc-meta-data (package name)
  "values function-pointer argument names list"
  (let ((package (find-package (util:match-readtable-case package))))
    (when package
      (let ((symbol
             (find-symbol (util:match-readtable-case name) package)))
        (when symbol
          (multiple-value-bind (rpc-meta is-rpc)
              (gethash symbol *rpc-register*)
            (and is-rpc rpc-meta)))))))

(defun get-rpc-param-meta (rpc-meta param)
  (let ((param-meta (assoc param (rpc-meta-arguments rpc-meta)
                           :key (lambda (s)
                                  (string-downcase (symbol-name s)))
                           :test #'equal)))
    (or param-meta
        (error "unknown parameter ~A" param))))

(defgeneric call-rpc-function (package name argassoc))

(defmethod call-rpc-function (package name argassoc)
  "package and name are strings designating desired function.
argassoc is a assoc list of argument name and values.
names are symbols matching original symbols in def-rpc call.
values should match the expected type."
  (let ((rpc-meta
         (get-rpc-meta-data package name)))
    (unless rpc-meta
      (error "not a valid rpc ~A::~A" package name))
    (or *secure-transport* (rpc-meta-allow-insecure rpc-meta)
        (error "~A:~A requires secure transport" package name))
    (when (rpc-meta-host-to-host-only rpc-meta)
      (unless *trusted-host*
        (error "not allowed without trusted host")))
    (and (not *post-request*) (rpc-meta-post-only rpc-meta)
         (error "~A:~A requires POST request" package name))
    (call-rpc-function-auth-impl rpc-meta argassoc)))

(defgeneric app-rpc-handler (app token rpc-meta function args))

(defmethod app-rpc-handler
    (app token rpc-meta function args)
  (declare (ignorable app token rpc-meta))
  (apply function args))

(defun call-rpc-function-auth-impl (rpc-meta argassoc)
  (when (not (rpc-meta-anonymous rpc-meta))
    (or *auth-token* (error "missing auth token")))
  (let ((args (list)))
    (dolist (arginfo (rpc-meta-arguments rpc-meta))
      (let ((argsymbol (car arginfo)))
        (let ((argcons (assoc argsymbol argassoc)))
          (when argcons
            (push (find-symbol (symbol-name argsymbol)
                               :keyword) args)
            (push (cdr argcons) args)))))
    (app-rpc-handler (rpc-meta-application rpc-meta)
                     *auth-token*
                     rpc-meta
                     (rpc-meta-function rpc-meta)
                     (nreverse args))))

(defun rpc-reverse-lookup (function)
  (when (functionp function)
    (let ((rpc-symbol (gethash function *rpc-reverse-register*)))
      (when rpc-symbol
        (let ((package (symbol-package rpc-symbol))
              (symbol (symbol-name rpc-symbol)))
          (values (string-downcase (package-name package))
                  (string-downcase symbol)))))))


(defun make-rpc-params (rpc-meta args)
  (let ((params (list)))
    (mapcar (lambda (k v)
              (push (cons (string-downcase (car k)) v)
                    params))
            (rpc-meta-arguments rpc-meta) args)
    (reverse params)))


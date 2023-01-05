(in-package :cloud)


(defvar *error-numbers* (make-hash-table :test #'eql))
(defvar *error-symbols* (make-hash-table :test #'eql))
(defvar *start-numbers* (make-hash-table :test #'eql))

(defstruct app-error
  (num 0 :type integer)
  (symbol nil :type symbol))

(defmacro define-error (num symbol)
  (assert (symbolp symbol))
  `(define-error% ,num ',symbol))

(defun define-error% (num symbol)
  (let ((package (symbol-package symbol)))
    (assert (> (abs num) 10000))
    (let ((start (truncate (/ num 10000))))
      (let ((clash-start (gethash start *start-numbers*)))
        (if clash-start
            (unless (eq clash-start package)
              (error "start range ~A0000 is already used by ~A "
                     start clash-start))
            (setf (gethash start *start-numbers*) package))
        (let ((clash-num (gethash num *error-numbers*)))
          (when clash-num
            (unless (eql (app-error-symbol clash-num) symbol)
              (error "~A is already assigned to ~A"
                     num (app-error-symbol clash-num) ))))
        (let ((clash-symbol (gethash symbol *error-symbols*)))
          (when clash-symbol
            (unless (eql (app-error-num clash-symbol) num)
              (error "~A is already assigned ~A"
                     symbol (app-error-num clash-symbol)))))
        (let ((e (make-app-error :num num :symbol symbol)))
          (setf (gethash num *error-numbers*) e)
          (setf (gethash symbol *error-symbols*) e)
          e)))))

(define-condition xed-error (error)
  ((key :initarg :key :reader xed-error-key)
   (i18n :initarg :i18n :reader xed-error-i18n)
   (data :initarg :data :reader xed-error-data)
   (cause :initarg :cause :reader xed-error-cause))
  (:report (lambda (condition stream)
             (let ((app-err (gethash (xed-error-key condition) *error-symbols*)))
               (if app-err
                   (let ((err-symbol (app-error-symbol app-err)))
                     (format stream "xed error: ~A  ~S - data: ~A"
                             (app-error-num app-err)  err-symbol
                             (xed-error-data condition)))
                   (format stream "app-error definition not found for ~S"
                           (xed-error-key condition)))))))

(defun error-i18n-msg (key &rest data)
  (when key
    (let ((fmt  (i18n:get-text key)))
      (when fmt
        (apply #'format nil fmt data)))))

(defun raise-error (key &optional data cause)
  (error 'xed-error
         :key key :i18n (error-i18n-msg key data)
         :data data :cause cause))

(defun get-xed-error-details (condition)
  (let ((app-err (gethash (xed-error-key condition) *error-symbols*)))
    (if app-err
        (values condition (app-error-num app-err)
                (format nil "~S" (xed-error-key condition))
                (xed-error-data condition)
                (xed-error-i18n condition)
                (xed-error-cause condition))
        condition)))

(defmethod aas-rpc::make-rpc-error ((error xed-error ))
  (multiple-value-bind (condition num key data i18n cause)
      (get-xed-error-details error)
    (aas-rpc::make-json-rpc-2-error
     :code (or num -32000)
     :scode key
     :message (format nil "~A" condition)
     :i18nMsg i18n
     :data (format nil "~A" data)
     :host (host:my-hostname)
     :trace (print-backtrace condition)
     :cause cause)))

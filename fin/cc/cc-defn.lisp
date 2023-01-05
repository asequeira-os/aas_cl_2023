(in-package :fin-cc)

(def-rpc-struct cc-vendor
    (key nil :type string)
  (name nil :type (or null string)))

(defvar *cc-vendors* (make-hash-table :test #'equal))

(defun define-cc-vendor (key name)
  (setf (gethash key *cc-vendors*)
        (make-cc-vendor :key key :name name)))

(defmethod db-base:get-struct-columns ((symbol (eql 'cc-vendor)))
  '((key string)))

(defmethod aas-rpc:deserialize-after ((object cc-vendor))
  (let ((vendor (gethash (cc-vendor-key object) *cc-vendors*)))
    (or vendor
        (error "CC type ~A not supported" (cc-vendor-key object)))))

(defun make-cc-vendor-from-db
    (&key key)
  (gethash key *cc-vendors*))

(def-rpc (vector cc-vendor) cc-vendors-list (:anonymous t :application +fin-cc+)
    ()
  (coerce
   (loop for v being the hash-value of *cc-vendors* collect v)
   'vector))

(defun non-empty-digits-string (s)
  (and (stringp s)
       (not (zerop (length s)))
       (every #'digit-char-p s)))

(defgeneric validate-cc-num (cc-vendor cc-num cc-exp-mm cc-exp-yy ccv))

(defmethod validate-cc-num :around (cc-vendor cc-num cc-exp-mm cc-exp-yy ccv)
  (declare (ignorable ccv))
  (or  (and (not (null cc-vendor))
            (gethash (cc-vendor-key cc-vendor)  *cc-vendors*)
            (luhn cc-num)
            (non-empty-digits-string cc-num)
            (<= 1 cc-exp-mm 12)
            (<= (time:dto-year time:*utc-now*) cc-exp-yy )
            (call-next-method)
            )
       (error "invalid CC info")))

;;from http://lemonodor.com/archives/000217.html
;;see http://en.wikipedia.org/wiki/Luhn_algorithm
(defun luhn (string)
  (let* ((checksum 0)
         (cc-length (length string))
         (double-p (evenp cc-length)))
    (dotimes (i cc-length)
      (let ((digit-value (digit-char-p (schar string i))))
        (when double-p
          (setf digit-value (* 2 digit-value))
          (when (> digit-value 9)
            (decf digit-value 9)))
        (incf checksum digit-value)
        (setf double-p (not double-p))))
    (zerop (mod checksum 10))))

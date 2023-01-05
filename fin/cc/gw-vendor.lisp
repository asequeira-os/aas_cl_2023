(in-package :fin-cc)

(defclass cc-gw-vendor ()
  ((name :type string)))

(defgeneric add-cc-gw-vendor (cc-gw-vendor))

(defparameter +production+ nil)
(defvar *cc-gw-vendors* (make-hash-table :test #'equal))
(defvar *cc-gw-vendors-list* nil)
(defvar *cc-gw-vendors-count* 0)

(defmethod add-cc-gw-vendor ((vendor cc-gw-vendor))
  (let ((name (slot-value vendor 'name)))
    (when (gethash name *cc-gw-vendors*)
        (error "vendor ~A already configured" name))
    (push (setf (gethash name *cc-gw-vendors*) vendor)
          *cc-gw-vendors-list*)
    (incf *cc-gw-vendors-count*)))

(defun get-cc-gw-vendor (name)
  (gethash name *cc-gw-vendors*))

(let ((lock (mp-make-lock "cc-random"))
      (rstate (make-random-state t)))
  (defun get-cc-gw-vendor-random ()
    (mp-with-lock (lock)
      (nth (random *cc-gw-vendors-count* rstate)
           *cc-gw-vendors-list*))))

(defun make-cc-gw-vendor-from-db (&key name)
  (get-cc-gw-vendor name))

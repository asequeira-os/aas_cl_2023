(in-package :sampling)


;;random sampling with reservoir - Jeffrey Vitter 1985

(defclass reservoir-r ()
  ((count :initform 0)
   (size :initarg :size)
   (rs :initform (make-random-state))
   (vect :initarg :vect)))


(defun create-reservoir-r (sample-size)
  (make-instance 'reservoir-r
                 :size sample-size
                 :vect (make-array sample-size :initial-element nil)))

(defmethod get-sample ((algo reservoir-r))
  (slot-value algo 'vect))

(defmethod process ((algo reservoir-r) datum)
  (let ((count (slot-value algo 'count))
        (size (slot-value algo 'size)))
    (if (< count size)
        (setf (aref (slot-value algo 'vect) count) datum) ;fill initial sample-size elements
        (let ((offset (floor (* count (random 1.0 (slot-value algo 'rs))))))
          (when (< offset size)
            (setf (aref (slot-value algo 'vect) offset) datum))))
    (incf (slot-value algo 'count))))

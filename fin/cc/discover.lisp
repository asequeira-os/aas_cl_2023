(in-package :fin-cc)

(defvar *discover* (define-cc-vendor "DSC" "Discover"))

(defmethod validate-cc-num
    ((cc-vendor (eql *discover*)) cc-num cc-exp-mm cc-exp-yy ccv)
  "discover specific custom validations"
  (declare (ignorable cc-num cc-exp-mm cc-exp-yy ccv))
  t)




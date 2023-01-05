(in-package :fin-cc)

(defvar *mastercard* (define-cc-vendor "MASTERCARD" "MasterCard"))

(defmethod validate-cc-num
    ((cc-vendor (eql *mastercard*)) cc-num cc-exp-mm cc-exp-yy ccv)
  "MasterCard specific custom validations"
  (declare (ignorable cc-num cc-exp-mm cc-exp-yy ccv))
  t)


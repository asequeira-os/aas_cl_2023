(in-package :fin-cc)

(defvar *amex* (define-cc-vendor "AMEX" "American Express"))

(defmethod validate-cc-num
    ((cc-vendor (eql *amex*)) cc-num cc-exp-mm cc-exp-yy ccv)
  "amex specific custom validations"
  (declare (ignorable cc-num cc-exp-mm cc-exp-yy ccv))
  t)


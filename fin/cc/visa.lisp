(in-package :fin-cc)

(defvar *visa* (define-cc-vendor "VISA" "VISA"))

(defmethod validate-cc-num
    ((cc-vendor (eql *visa*)) cc-num cc-exp-mm cc-exp-yy ccv)
  "visa specific custom validations"
  (declare (ignorable cc-num cc-exp-mm cc-exp-yy ccv))
  t)




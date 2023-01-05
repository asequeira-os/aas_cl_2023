(in-package :scratch)

(export (list 'custom-iterator 'with-iterate))

;;return a no arguments function
;;that function returns two values
;;first value is the next item
;;second value is true when item is valid
(defgeneric custom-iterator (custom-sequence))

;;this is defined mostly as an example
;;of how to implement the generic function
(defmethod custom-iterator ((custom-sequence list))
  (let ((cur custom-sequence))
    (lambda ()
      (let ((temp cur))
        (setf cur (cdr cur))
        (values (car temp) temp)))))

(defmethod custom-iterator ((vector vector))
  (let ((length (length vector))
        (offset 0))
    (lambda ()
      (if (>= offset length)
          (values nil nil)
          (let ((val (elt vector offset)))
            (incf offset)
            (values val t))))))


;;iterate over the custom sequence, executing the body
;;with var bound to the item from the sequence
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-iterate ((custom-sequence var) &body body)
  (cl-utilities:with-unique-names (mvl iter)
    `(let ((,iter (custom-iterator ,custom-sequence)))
       (do ((,mvl (multiple-value-list (funcall ,iter))
                 (multiple-value-list (funcall ,iter))))
           ((null (second ,mvl)) (values))
      (let ((,var (first ,mvl)))
        ,@body)))))
)


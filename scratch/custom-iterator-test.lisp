(in-package :scratch.test)

;(with-iterate ((list 2 1 6 34) foo) (format t "|~A|~%" foo))
(fiveam:test with-iterate
  (let ((list (list 2 1 6 34)) (newlist (list)))
    (with-iterate (list foo)
      (format t "|~A|~%" foo)
      (setf newlist (append newlist (list foo))))
    (fiveam:is (equal list newlist))))

(fiveam:test with-iterate-empty
  (let ((list nil))
    (with-iterate (list foo)
      (format t "not allowed |~A|~%" foo)
      (fiveam:fail))
    (fiveam:is (= 1 1))))

(fiveam:test with-iterate-vector
  (let ((vector #(3 5 1 2))
        (offset 0)
        (newvector (make-array 4)))
    (with-iterate (vector foo)
      (setf (elt newvector offset) foo)
      (incf offset))
    ;(format t "|~A|~%" newvector)))
    (fiveam:is (equalp #(3 5 1 2) vector))))


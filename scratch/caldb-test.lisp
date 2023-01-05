(in-package :scratch.test)

(in-suite :scratch.test)

(test db-manager-basic
  (scratch::with-db-manager
    (reset)
    (second-level)))

(test db-manager-two
  (scratch::with-db-manager
    (reset)))


(let ((count 0) (dbcon nil))
  (defun reset ()
    (setf count 0)
    (setf dbcon nil))
  (defun second-level ()
    (unless (> count 5)
      (incf count)
      (scratch::with-db-manager
        (if dbcon
          (fiveam:is (eq dbcon (scratch::get-db 1)))
          (setf dbcon (scratch::get-db 1)))
        (second-level)))))




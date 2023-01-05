(in-package :notify-test)

(defparameter *hits* (make-hash-table :test #'eql))


(define-notifier notify-a)
(define-notifier notify-b)


(defun listner-a (obj)
  (assert (equalp "a" obj))
  (incf (gethash 'notify-a *hits*)))

(defun listner-b (obj)
  (assert (equalp "b" obj))
  (incf (gethash 'notify-b *hits*)))

(defun listner-b-2 (obj)
  (assert (equalp "b" obj))
  (incf (gethash 'notify-b *hits*)))

(add-listener 'notify-a 0 'listner-a)
(add-listener 'notify-a 0 'listner-a)
(add-listener 'notify-b 0 'listner-b)
(add-listener 'notify-b 0 'listner-b-2)


(deftest all-tests
  (setf (gethash 'notify-a *hits*) 0)
  (setf (gethash 'notify-b *hits*) 0)

  (notify-all 'notify-a "a")
  (notify-all 'notify-b "b")
  (notify-all 'notify-b "b")
  (is (= 1 (gethash 'notify-a *hits*)))
  (is (= 4 (gethash 'notify-b *hits*)))
  t)

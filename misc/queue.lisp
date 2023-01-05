;;queue (FIFO)

(in-package :aas-misc)

(export '(make-queue enqueue dequeue empty-queue-p empty-queue iterate-queue))


;;achieving queue using a singly linked list
;;
;;the basic idea is to maintain a pointer to the end of the list
;;and one to the beginning

;;consists of two pointers - head, tail
;;they live in a cons which is passed around as the queue object
;;
;;initially both head and tail point to a weird cons
;;I am not sure if that part is necessary, but I think
;;it simplifies initial state and initial enqueue
;;

(defun make-queue ()
  "creates a FIFO queue based on a singly linked list"
  (let* ((cons (cons nil nil))
         (head cons)
         (tail cons))
    (cons head tail)))

;;invariants
;;(cdr head) gets the next data cons
;;tail points to the last added cons

;;we put the item in a new cons
;;
(defun enqueue (queue obj)
  "enqueue the object obj onto the queue queue"
  (let ((cons (cons obj nil))
        (tail (cdr queue)))
    (setf (cdr tail) cons)
    (setf (cdr queue) cons)))


(defun dequeue (queue)
  "dequeue . returns multiple values. second value is t if queue was empty"
  (let ((head (car queue))
        (tail (cdr queue)))
    (if (eq head tail)
        (values nil t) ; empty queue
        (let ((obj (car (cdr head))))
          (setf (car queue) (cdr head))
          (values obj nil)))))

(defun empty-queue-p (queue)
  "predicate to check if queue is empty"
  (eq (car queue) (cdr queue)))

(defun empty-queue (queue)
  (setf (car queue)
        (setf (cdr queue)
              (cons nil nil)))
  queue)

(defun iterate-queue (queue function)
  (unless (empty-queue-p queue)
    (dolist (obj (copy-seq (cdr (car queue))))
      (funcall function obj))))

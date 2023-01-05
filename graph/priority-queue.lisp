(in-package :aas-graph)

#|
'algorithms CLRS 2nd ed'

pp. 128

PARENT(i)
1 return Floor(i/2)

LEFT(i)
1 return 2i

RIGHT(i)
1 return 2i+1

pp. 130

MAX-HEAPIFY(A,i)
1 l <- Left(i)
2 r <- Right(i)
3 if l <= heap-size[A] and A[l] > A[i]
4    then largest <- l
5    else largest <- i
6 if r <= heap-size[A] and A[r] > A[largest]
7    then largest <- r
8 if largest <> i
9    then exchange A[i] <-> A[largest]
10        MAX-HEAPIFY (A, largest)

pp. 133

BUILD-MAX-HEAP(A)
1 heap-size[A] <- length[A]
2 for i <- FLOOR(length[A]/2) downto 1
3     do MAX-HEAPIFY(A,i)

pp. 139

HEAP-MAXIMUM(A)
1 return A[1]

HEAP-EXTRACT-MAX(A)
1 if heap-size[A] < 1
2    then error "heap underflow"
3 max <- A[1]
4 A[1] <- A[heap-size[A]]
5 heap-size[A] <- heap-size[A] - 1
6 MAX-HEAPIFY(A,1)
7 return max

pp.140

HEAP-INCREASE-KEY(A,i,key)
1 if key < A[i]
2    then error "new key is smaller than current key"
3 A[i] <- key
4 while i > 1 and A[Parent(i)] < A[i]
5     do exchange A[i] <-> A[Parent(i)]
6        i <- Parent(i)

MAX-HEAP-INSERT(A, key)
1 heap-size[A] <- heap-size[A] + 1
2 A[heap-size[A]] <- negative-infinity
3 HEAP-INCREASE-KEY(A,heap-size[A],key)

|#

#|
my thoughts
Use a use HOF for comparison.
This will obviate the need to distinguish between max and min type queues.

Need to provide for a queue that can grow boundless or a queue with a fixed size.

need to make changes for 0 based arrays instead of 1 based

Counter intutive stuff below:
For a queue where the max element is on top (MAX_QUEUE),
inserting a larger element when the queue is full is a no-op.

A MAX-QUEUE of size M formed by inserting N elements from a set will give the
bottom M elements of the set

For a queue where the min element is on top (MIN_QUEUE),
inserting a smaller element when the queue is full is a no-op

A MIN-QUEUE of size M formed by inserting N elements from a set will give the
top M elements of the set

|#

;;since we only need '>' functionality the comparator can be simplified
;;           f(x,y) : x > y -> true
;;                    else  -> false
;;example comparator
;; (lambda (x y)
;;   (> x y))
;; if x and y are numbers, and a max heap is desired
;;
;; (lambda (x y)
;;   (< x y))
;; if x and y are numbers, and a min heap is desired


(defclass priority-queue ()
  ((pq-vector :type vector :accessor pq-vector)
   (heap-size :type integer :accessor heap-size)
   (q-capacity :type integer :accessor q-capacity)
   (q-sieve :initform nil :accessor q-sieve
            :documentation
            "if true, there will not be overflow errors. use for top/bottom N behavior")
   (comparator :accessor comparator)
   (max-size :initform most-positive-fixnum :accessor max-size)))

(defun make-priority-queue (q-initial-size q-max-size q-comparator)
  (let ((q (make-instance 'priority-queue)))
    (with-slots (pq-vector heap-size comparator max-size q-capacity) q
      (when (> q-initial-size q-max-size)
        (error "bad input"))
      (setf pq-vector (make-array q-initial-size
                                  :adjustable (< q-initial-size q-max-size)))
      (setf q-capacity q-initial-size)
      (setf heap-size 0)
      (setf comparator q-comparator)
      (setf max-size q-max-size))
    q))

(defun pq-parent (i)
  (1- (ceiling (/  i 2))))

(defun pq-left (i)
  (1+ (* 2 i)))

(defun pq-right (i)
  (+ (* 2 i) 2))

(defun max-heapify (pq i)
  (with-slots (pq-vector heap-size comparator max-size) pq
    (let ((largest nil)
          (l (pq-left i))
          (r (pq-right i)))
      (if (and (< l heap-size)
               (funcall comparator (aref pq-vector l) (aref pq-vector i)))
          (setf largest l)
          (setf largest i))
      (if (and (< r heap-size)
               (funcall comparator (aref pq-vector r) (aref pq-vector largest)))
          (setf largest r))
      (if (not (= largest i))
          (progn
            (rotatef (aref pq-vector i) (aref pq-vector largest))
            (max-heapify pq largest))))))

(defun build-max-heap (pq)
  (with-slots (pq-vector heap-size comparator max-size) pq
    (do ((i (pq-parent (1- heap-size)) (1- i)))
        ((< i 0))
      (max-heapify pq i))))

(defun heap-maximum (pq)
  (with-slots (pq-vector) pq
    (aref pq-vector 0)))

(defun heap-extract-max (pq)
  (with-slots (pq-vector heap-size comparator max-size) pq
    (if (< heap-size 1)
        (error "heap underflow"))
    (decf heap-size)
    (let ((max (aref pq-vector 0)))
      (setf (aref pq-vector 0) (aref pq-vector heap-size))
      (max-heapify pq 0)
      max)))


(defun heap-increase-key (pq i key)
  (with-slots (pq-vector heap-size comparator max-size) pq
    (setf (aref pq-vector i) key)
    (do (dummy)
        ((or (= i 0) (funcall comparator
                              (aref pq-vector (pq-parent i))
                              (aref pq-vector i))))
      (declare (ignorable dummy))
      (rotatef (aref pq-vector i) (aref  pq-vector (pq-parent i)))
      (setf i (pq-parent i)))))

(defun max-heap-insert (pq key)
  (with-slots (pq-vector heap-size comparator max-size q-capacity q-sieve) pq
    (unless (zerop max-size)
      (if (= heap-size max-size)
          (if q-sieve
              (insert-if-needed pq key)
              (error "heap overflow"))
          (progn
            (incf heap-size)
            (when (< q-capacity heap-size)
              (adjust-array pq-vector heap-size)
              (setf q-capacity heap-size))
            (heap-increase-key pq (1- heap-size) key))))))

;;todo 9 this may be suboptimal
(defun insert-if-needed (pq key)
  (with-slots (pq-vector heap-size comparator max-size q-capacity q-sieve) pq
    (unless (funcall comparator key (aref pq-vector 0))
      (heap-extract-max pq)
      (max-heap-insert pq key))))

(defun queue-empty-p (pq)
  (zerop (heap-size pq)))


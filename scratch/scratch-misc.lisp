(in-package :scratch)

;;generates any geometric series
;;wrote for fun
;;

(defun geometric-series-generator (seed ratio)
  (let ((cur seed))
    (lambda ()
      (let ((temp cur))
        (setf cur (* cur ratio))
        temp))))


(fiveam:test geometric-series-test
  (let ((input '((4 10) (9 1/3) (7 1/10) (3 1) (1 -1/2) (3 -1)))
        (results '(
                   (4 40 400 4000 40000)
                   (9 3 1 1/3 1/9 )
                   (7 7/10 7/100 7/1000 7/10000 )
                   (3 3 3 3 3 )
                   (1 -1/2 1/4 -1/8 1/16 )
                   (3 -3 3 -3 3 ))))
    (let ((test-num 0))
    (loop for x in input
       do
       (let  ((series (geometric-series-generator (first x) (second x )))
              (result (list)))
         (format t "~%start ~A ratio ~A: " (first x) (second x))
         (loop for x from 1 to 5
            do
              (setf result (cons (funcall series) result)))
         (setf result (reverse result))
         (print result)
         (fiveam:is (equalp result (elt results test-num)))
         (incf test-num))))))


(defun simple-series-generator (seed next-fn)
  (let ((next seed))
    (lambda ()
      (let ((cur next))
        (setf next (funcall next-fn cur))
        cur))))

(defun integer-series-generator (start)
  (simple-series-generator
   start (lambda (cur) (1+ cur))))

(defun count-permutations (n r)
  (do* ((k n (1- k)) (ans 1 (* ans k)))
       ((= k r) ans)))

(defun unique-randoms-list (range count)
  (when (and (integerp range) (> count (/ range 2)))
    (error (format
            t "range ~A is too small to provide ~A unique randoms"
            range count)))
  (let* ((list
          (loop for i from 1 to count
             collect (random range)))
         (copy (remove-duplicates list)))
    (if (= (length list) (length copy))
        list ;there wer no duplicates
        (unique-randoms-list range count))))
;;todo 9 create tests including one for catching error


(defgeneric make-bit-vector(impl initial-size &key adjustable))

(defgeneric set-bit (bv index &optional value))

(defgeneric get-bit (bv index))

(defmethod make-bit-vector ((impl t) initial-size &key adjustable)
  (make-array initial-size :element-type 'bit :initial-element 0
              :adjustable adjustable))

(defmethod set-bit ((bv vector) index &optional (value 1))
  (setf (bit bv index) value))

(defmethod get-bit ((bv vector) index)
  (bit bv index))



(defgeneric shuffle (sequence))

(defmethod shuffle ((sequence list))
  (let* ((length (length sequence))
         (count 0)
         (list (list))
         (bv (make-bit-vector t length)))
    (if (zerop length)
        sequence
        (do ((index (random length) (random length)))
            ((= count length) list)
          (unless (= (get-bit bv index) 1)
            (set-bit bv index)
            (setf list (cons (elt sequence index) list))
            (incf count))))))

;;;depth first and breadth first search routines
;;this was tested with an old impl of queue
;;needs to be redone using the new queue in aas-misc
(defgeneric get-childern (node))
(defgeneric get-node-value (node))

(defun depth-first (node visitor)
  (when node
    (funcall visitor (get-node-value node))
    (let ((childern (get-childern node)))
      (dolist (child childern)
        (depth-first child visitor)))))


(defun breadth-first (node visitor)
  (when node
    (let ((dll (make-dll)))
      (breadth-first-impl dll visitor node))))

(defun breadth-first-impl (dll visitor node)
  (funcall visitor (get-node-value node))
  (let ((children (get-childern node)))
    (dolist (child children)
      (enqueue child dll)))
  (do () ((zerop (queue-count dll)) nil)
    (breadth-first-impl dll visitor (dequeue dll))))


;following are for test and demo purposes
(defmethod get-childern ((node t))
  nil)
(defmethod get-childern ((node list))
  (rest node))

(defmethod get-node-value ((node list))
  (first node))

(defmethod get-node-value ((node t))
  node)

(fiveam:test depth-first-test
  (let((op (list))
       (ip '(1 (2 3 4) (5 6 (7 8 (9 10))) 11 (12 13) 14)))
    (depth-first ip
                 (lambda (x) (setf op (cons x op))))
    (fiveam:is (equalp (reverse op)
                '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)))))

(fiveam:test breadth-first-test
  (let((op (list))
       (ip '(1 (2 3 4) (5 6 (7 8 (9 10))) 11 (12 13) 14)))
    (breadth-first ip
                 (lambda (x) (setf op (cons x op))))
    (fiveam:is (equalp (reverse op)
                '(1 2 5 11 12 14 3 4 6 7 13 8 9 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;great circle stuff
;;formula referenced from
;;http://www.movable-type.co.uk/scripts/latlong.html
;; excel version given was
;; ACOS(SIN(Lat1)*SIN(Lat2)+COS(Lat1)*COS(Lat2)*COS(Lon2-Lon1))*6371

(defun degree-to-radian (degree)
  (/ (* (mod degree 360.0L0) pi) 180.0L0))

(fiveam:test degree-to-radian
  (let ((degree (list 0   30         45         180 270
                      360 390
                      -0  -30        ))
        (radian (list 0   (/ pi 6.0) (/ pi 4.0) pi  (+ pi (/ pi 2))
                      0.0 (/ pi 6.0)
                      0.0 (- (* 2.0 pi) (/ pi 6.0)) )))
    (mapcar #'(lambda (degree radian)
                (fiveam:is (util::test-float-equalp radian (degree-to-radian degree))))
            degree radian)))



(defparameter +EARTH-RADIUS-KM+ 6371.0L0 "earth radius i kilometers (approx.)")

(defparameter +EARTH-RADIUS-MILES+ (/ +EARTH-RADIUS-KM+ 1.609344L0)
  "earth radius in miles (approx.)")

(defvar *metric-distance* nil "bind to true if you want metric distances")

(defun great-circle-distance (lat1 long1 lat2 long2)
  "input should be in degrees. output unit depends on *metric-distance*
   kilometers or miles"
  (let ((radius (if *metric-distance*
                    +EARTH-RADIUS-KM+
                    +EARTH-RADIUS-MILES+))
        (lat1 (degree-to-radian lat1))
        (long1 (degree-to-radian long1))
        (lat2 (degree-to-radian lat2))
        (long2 (degree-to-radian long2)))
    (* (acos (+ (* (sin lat1) (sin lat2))
                (* (cos lat1) (cos lat2) (cos (- long2 long1)))))
       radius)))

(fiveam:test great-circle-distance
  (let ((*metric-distance* t))
    (fiveam:is (util::test-float-equalp
                170.2
                (great-circle-distance 53.1506L0 -1.8444L0 52.2047L0 0.1406L0)))))

(defun degree-decimalize (deg min sec)
  (+ deg (/ min 60.0) (/ sec 3600.0)))

(fiveam:test degree-decimalize
  (fiveam:is (util::test-float-equalp (degree-decimalize 52 12 17) 52.2047)))

(defparameter +miles-per-degree+ (great-circle-distance 90 90 91 90)
  "miles in a degree")

(defparameter +km-per-degree+
  (let ((*metric-distance* t))
    (great-circle-distance 90 90 91 90))
  "kilometers in a degree")

(fiveam:test geodesi-constants
  (fiveam:is (util::test-float-equalp
              (/ +miles-per-degree+ +km-per-degree+)
              (/ +earth-radius-miles+ +earth-radius-km+))))

(defun radius-box (lat long radius)
  "approximate square region ranges lat1 lat2 long1 long2"
  (let* ((distance-per-degree (if *metric-distance*
                                  +km-per-degree+
                                  +miles-per-degree+))
         (degree (/ radius distance-per-degree)))
    (values (- lat degree) (+ lat degree)
            (- long degree) (+ long degree))))

;;todo Z unit test for radius-box

(defun degree-to-milli (degree)
  (round (* degree 1000.0)))

(defun positive-degree (degree)
  (mod degree 360.0L0))

(fiveam:test positive-degree
  (let ((deg (list 0 30 45 90 180 270 360 390
                   -30 -90 -180 -360))
        (pos (list 0 30 45 90 180 270   0  30
                   330 270 180  0)))
   (mapcar #'(lambda (deg pos)
               (fiveam:is (util::test-float-equalp
                           (positive-degree deg) pos)))
           deg pos)))





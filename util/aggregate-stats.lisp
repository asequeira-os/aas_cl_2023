;;compute aggregate statistics froma set of basic statistics

(in-package :basic-stats)

(defun make-aggregate-stats (basic-stats)
  (make-instance 'aggregate-stats :basic-stats basic-stats))

(defclass aggregate-stats (basic-stats)
  ((computed :initform nil)
   (basic-stats :initarg :basic-stats)))

(defmethod stats-item ((ob aggregate-stats) item)
  (declare (ignorable item))
  (cerror "you can not add items to an aggregate statistics object" nil ))

(defun compute (aggregate-stats list)
  (unless (slot-value aggregate-stats  'computed)
    (let ((count 0) (sum 0) (max) (min) (sum-squared 0))
      (loop for simple-stat in list do
           (setf count (+ count  (stats-count simple-stat)))
           (if max
               (setf max (max max (stats-max simple-stat)))
               (Setf max (stats-max simple-stat)))
           (if min
               (setf min (min min (stats-min simple-stat)))
               (Setf min (stats-min simple-stat)))
           (incf sum (stats-sum simple-stat))
           (incf sum-squared (stats-sum-squared simple-stat)))
      (setf (slot-value aggregate-stats 'count) count)
      (setf (slot-value aggregate-stats 'sum) sum)
      (setf (slot-value aggregate-stats 'max) max)
      (setf (slot-value aggregate-stats 'min) min)
      (setf (slot-value aggregate-stats 'sum-squared)  sum-squared))))

(defmethod stats-std-dev :before ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))

(defmethod stats-count  :before ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))

(defmethod stats-min :before  ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))

(defmethod stats-max  :before ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))

(defmethod stats-sum  :before ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))

(defmethod stats-sum-squared  :before ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))

(defmethod stats-mean :before ((ob aggregate-stats))
  (compute ob (slot-value ob 'basic-stats)))


;;my own little basic stats package
;;designed for long running process stats collection

(defpackage :basic-stats
  (:use :common-lisp)
  (:export make-aggregate-stats make-stats stats-item stats-count stats-min stats-max
           stats-sum stats-sum-squared stats-mean stats-std-dev))

(in-package :basic-stats)

(defun make-stats ()
  (make-instance 'basic-stats))

(defclass basic-stats () 
  ((count :initform 0) 
   (sum :initform 0)
   (max :initform nil)
   (min :initform nil)
   (sum-squared :initform 0)))


(defmethod stats-item ((ob basic-stats) item)
  (if (slot-value ob 'min)
      (setf (slot-value ob 'min) (min  (slot-value ob 'min) item))
      (setf (slot-value ob 'min)  item))
  (if (slot-value ob 'max)
      (setf (slot-value ob 'max) (max  (slot-value ob 'max) item))
      (setf (slot-value ob 'max) item))
  (incf (slot-value ob 'count))
  (incf (slot-value ob 'sum-squared) (* item item))
  (incf (slot-value ob 'sum ) item))

(defmethod stats-mean ((ob basic-stats))
  (if (= (slot-value ob 'count) 0)
      nil
      (/ (slot-value ob 'sum) (slot-value ob 'count))))

;; std dev formula
;;;  2     _ 2
;;;Ex  - N x
;;;  i
;;;------------
;;;    N
;;;
(defmethod stats-std-dev ((ob basic-stats))
(let ((mean (stats-mean ob)))
  (/ (- (slot-value ob 'sum-squared) 
	(* (slot-value ob 'count) (* mean mean)))
     (1- (slot-value ob 'count)))))

(defmethod stats-count ((ob basic-stats))
  (slot-value ob 'count))

(defmethod stats-min ((ob basic-stats))
  (slot-value ob 'min))

(defmethod stats-max ((ob basic-stats))
  (slot-value ob 'max))

(defmethod stats-sum ((ob basic-stats))
  (slot-value ob 'sum))

(defmethod stats-sum-squared ((ob basic-stats))
  (slot-value ob 'sum-squared))


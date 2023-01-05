(defpackage :basic-stats-test
  (:use :aas-cl :common-lisp :basic-stats :aseq-test))

(in-package :basic-stats-test)

(deftest all-tests
  (and (emptyob) (notnill) (with-55-data) (aggregate-stats)))

(defparameter *stats-ob* (make-stats))


(deftest emptyob
  (is (null (stats-mean *stats-ob*))))

;;;very lame tests
(deftest notnill
  (let ((*stats-ob* (make-stats)))
    (stats-item *stats-ob* 10)
    (stats-item *stats-ob* 15)
    (stats-item *stats-ob* 25)
					;(print (slot-value *stats-ob* 'basic-stats::sum))
					;(print (stats-mean *stats-ob*))
    (is (=  175/3 (stats-std-dev *stats-ob*)))
    (is (=  (/ (+ 10 15 25) 3) (stats-mean *stats-ob*)))))

(define-constant +epsilon+ 0.00001)

(define-constant +sample-data+
    '(
      0.6
      0.24
      0.37
      0.12
      0.07
      0.93
      0.24
      0.31
      0.91
      0.66
      0.22
      0.06
      0.6
      0.01
      0.77
      0.71
      0.45
      0.57
      0.46
      0.34
      0.29
      0.89
      0.33
      0.55
      0.78
      0.94
      0.97
      0.53
      0.25
      0.93
      0.85
      0.86
      0.17
      0.21
      0.98
      0.24
      0.14
      0.22
      0.55
      0.06
      0.88
      0.78
      0.12
      0.48
      0.78
      0.89
      0.18
      0.23
      0.46
      0.64
      0.57
      0.76
      0.54
      0.9
      0.3

      ))


;;sum 27.889996
;;mean 0.5070908
;;median 0.53
;;variance 0.29695162
;;std-dev 0.08818026
;;max 0.98
;;min 0.01

(deftest with-55-data
  (let ((ob (make-stats)))
    (mapcar (lambda (d) (stats-item ob d)) +sample-data+ )
    (is (< (abs (- (stats-std-dev ob) 0.08818026 ))
           +epsilon+))
    (is (= (stats-mean ob) 0.5070908 ))))

(deftest aggregate-stats
  (let ((ob1 (make-stats)) (ob2 (make-stats)))
    (mapcar (lambda (d)
              (stats-item ob1 d)
              (stats-item ob2 d))
            +sample-data+ )
    (let ((aggr (make-aggregate-stats (list ob1 ob2))))
      (is (= (stats-count aggr)
             (+ (stats-count ob1) (stats-count ob2))))
      (is (= (stats-max aggr)
             (max (stats-max ob1) (stats-max ob2))))
      (is (= (stats-min aggr)
             (min (stats-min ob1) (stats-min ob2))))
      (is (= (stats-sum-squared aggr)
             (+ (stats-sum-squared ob1) (stats-sum-squared ob2))))
      (is (= (stats-mean aggr)
             (/ (+ (stats-sum ob1) (stats-sum ob2))
                (stats-count aggr))))
      (is (< (abs (- (stats-std-dev aggr) 0.087371305 ))
             +epsilon+))
      (is (= (stats-mean aggr) 0.5070908 )))))


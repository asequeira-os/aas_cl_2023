(in-package :aas-local-time)

;;tz-time
;;base class to hold hour minute seconds for different purposes
(defstruct tz-time
  (h nil :type (integer 0 24))
  (m nil :type (integer 0 59))
  (s nil :type (integer 0 59))
  (ts nil :type (or null integer)) ;;total seconds
  (usw nil :type time-type))

(defun create-time (hour minute second &optional (type +enum-time-type-wall+))
  (make-tz-time :h hour :m minute :s second
                :ts (+ second (* minute 60) (* hour 60 60))
                :usw type))

(defun enum-time-type-letter (usw)
  (cond ((eq usw +enum-time-type-wall+) #\w)
        ((eq usw +enum-time-type-std+) #\s)
        ((eq usw +enum-time-type-utc+) #\u)
        (t (error "what is ~A" usw))))

(defun enum-time-type-from-letter (c)
  (cond ((eql c #\w) +enum-time-type-wall+)
        ((eql c #\s) +enum-time-type-std+)
        ((eql c #\u) +enum-time-type-utc+)
        (t (break)
           (error "what is ~A" c))))

(defmethod sout-object (ser-type stream type (tt tz-time))
  (declare (ignorable type))
  (aas-rpc:sout-object ser-type stream 'string
                       (tz-time-format-string tt)))

(defun tz-time-format-string (tt)
  (format nil "~2,'0D:~2,'0D:~2,'0D~C"
          (tz-time-h tt)(tz-time-m tt)(tz-time-s tt)
          (enum-time-type-letter (tz-time-usw tt)) ))

(aas-rpc:set-deserializer
 'tz-time
 (lambda (ser-type stream obj-type)
   (let ((ts (aas-rpc::deserialize-string ser-type stream obj-type)))
     (tz-time-parse-string ts))))

(defun tz-time-parse-string (ts)
  (let ((tsar (split-sequence:split-sequence #\: ts)))
    (unless (= 3 (length tsar))
      (error "bad time string <~A>" ts))
    (let* ((ss (third tsar))
           (h (parse-integer (first tsar)))
           (m (parse-integer (second tsar)))
           (s (parse-integer (subseq ss 0 2)))
           (uswc (aref ss 2))
           (usw (enum-time-type-from-letter uswc)))
      (create-time h m s usw))))

(defmethod db-base:get-struct-columns ((symbol (eql 'tz-time)))
  '((ts integer)
    (usw time-type)))

(defun make-tz-time-from-db (&key ts usw)
  (multiple-value-bind (second minute hour days)
      (break-seconds ts)
    (unless (zerop days)
      (error "bad data - total seconds ~A too big" ts))
    (create-time hour minute second usw)))

(defun tz-time<% (t1 t2)
  (with-slots (h m s usw) t1
    (let ((usw1 usw)
          (s1 (+ (* (+ m (* h 60)) 60) s)))
      (with-slots (h m s usw) t2
        (assert (eq usw1 usw) nil "incompatible time types comparison")
        (let ((s2 (+ (* (+ m (* h 60)) 60) s)))
          (< s1 s2))))))

(defun tz-time=% (t1 t2)
  (with-slots (h m s usw) t1
    (let ((usw1 usw)
          (s1 (+ (* (+ m (* h 60)) 60) s)))
      (with-slots (h m s usw) t2
        (assert (eq usw1 usw) nil "incompatible time types comparison")
        (let ((s2 (+ (* (+ m (* h 60)) 60) s)))
          (= s1 s2))))))

(defun tz-time>% (t1 t2)
  (and (not (tz-time<% t1 t2))
       (not (tz-time=% t1 t2))))

(def-comparison "tz-time" 'tz-time
  tz-time>% tz-time<% tz-time=%)


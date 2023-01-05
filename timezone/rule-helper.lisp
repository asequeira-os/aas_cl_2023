#|
timezone file has different types of textual presentation of points in time
this file  will try to code functions to turn them into data objects

info for this comes from the Olson zic.8.txt file
|#

(in-package :aas-local-time)

;;rules fields values parsing
(defun rule-parse-year (year-string)
  (if (or (string-equal "minimum" year-string)
          (string-equal "min" year-string))
      1
      (if (or (string-equal "maximum" year-string)
              (string-equal "max" year-string))
          10000
          (parse-integer year-string :junk-allowed nil))))

(defun rule-parse-from-year (year-string)
  (rule-parse-year year-string))

(defun rule-parse-to-year (year-string from-year)
  (if (string-equal "only" year-string)
      from-year
      (rule-parse-year year-string)))

(defun rule-parse-year-type (type-string)
  (if (string-equal "-" type-string)
      :all-years
      (if (string-equal "odd" type-string)
          :odd-years
          (if (string-equal "even" type-string)
              :even-years
              (if (or (string-equal "nonpres" type-string)
                      (string-equal "nonuspres" type-string))
                  :non-us-presidential-election-year
                  (if (string-equal "uspres" type-string)
                      :us-presidential-election-year
                      (error "unknown year type ~A" type-string)))))))

(defun rule-check-year-type (year-type year)
  (case year-type
    (:all-years t)
    (:odd-years (= 1 (mod year 2)))
    (:even-years (= 0 (mod year 2)))
    (:us-presidential-election-year
     (error "support for nonpres/nonuspres not implemented"))
    (:non-us-presidential-election-year
     (error "support for uspres not implemented"))
    (otherwise (error "unknown year-type ~A" year-type))))

(defun rule-parse-month-in (month-in)
  (let ((lc-month-in (string-downcase month-in)))
    (1+ (or (position lc-month-in month-abbreviations :test #'string-equal)
            (position lc-month-in month-full-names :test #'string-equal)
            (error "unknown month ~A" month-in)))))

;;base class for ON part of rule lines
;;no instances expected
(defclass day-on () ())

;;fixed day of month such as 5
(defclass day-on-fixed-day (day-on)
  ((day :initarg :day :type (integer 1 31))))

;;last weekday of month such as lastSun
(defclass day-on-last-dow (day-on)
  ((dow :initarg :dow :type (integer 0 6))))

;;base class for relative type
;;no instances expected
(defclass day-on-dow-relative (day-on)
  ((dow :initarg :dow :type (integer 0 6))
   (day :initarg :day :type (integer 1 31))))

;;Sun>=8   first Sunday on or after the eight
(defclass day-on-dow>= (day-on-dow-relative) () )
;;Sun<=25  last Sunday on or before the 25th
(defclass day-on-dow<= (day-on-dow-relative) () )

(defun rule-parse-day-on (day-on)
  (when (equal 0 (search "last" day-on :test #'string-equal))
    (return-from rule-parse-day-on
      (rule-parse-day-on-last-day day-on)))
  (mapcar (lambda (dow)
            (when (equal 0 (search dow day-on :test #'string-equal))
              (return-from rule-parse-day-on
                (rule-parse-day-on-dow-day day-on dow))))
          dow-abbreviations)
  (make-instance 'day-on-fixed-day :day (parse-integer day-on :junk-allowed nil)))


(defun rule-parse-day-on-last-day (day-on)
  "parse stuff that looks like lastSun or lastSunday"
  (let ((day-string (subseq day-on 4)))
    (make-instance
     'day-on-last-dow
     :dow (or (position day-string dow-full-names :test #'string-equal)
              (position day-string dow-abbreviations :test #'string-equal)
              (error "can not parse day of week ~A" day-string)))))

(defun  rule-parse-day-on-dow-day (day-on dow)
  (let ((dow-num (position dow dow-abbreviations :test #'string-equal))
        (type (or (and (search ">=" day-on) 'day-on-dow>= )
                  (and (search "<=" day-on) 'day-on-dow<= )
                  (error "did not find >= or <= in ~A" day-on)))
        (num (parse-integer day-on :start (1+ (search "=" day-on)))))
    (make-instance type :dow dow-num :day num)))

(defun rule-parse-at (hour-string)
  (let* ((reversed (reverse hour-string))
         (last-letter (aref reversed 0))
         (type (case last-letter
                 (#\w +enum-time-type-wall+ )
                 (#\s +enum-time-type-std+ )
                 ((#\u #\g #\z) +enum-time-type-utc+)
                 (otherwise +enum-time-type-wall+))))
    (if (eql 0 (position #\- hour-string))
        (create-time 0 0 0 type)
        (multiple-value-bind (hour minute second)
            (rule/zone-parse-hms hour-string)
          (create-time hour minute second type )))))

(defun rule/zone-parse-hms (hour-string)
  (let* ((comp-list (split-sequence:split-sequence #\: hour-string))
         (length (length comp-list))
         (hour (parse-integer (first comp-list) :junk-allowed t))
         (minute 0)
         (second 0))
    (when (> length 1)
      (setf minute (parse-integer (second comp-list) :junk-allowed t)))
    (when (> length 2)
      (setf second (parse-integer (third comp-list) :junk-allowed t)))
    (values hour minute second)))

(defun rule-parse-save (hour-string)
  (if (string-equal "-" hour-string)
      0
      (multiple-value-bind (hour minute second)
          (rule/zone-parse-hms hour-string)
        (+ (* hour +seconds-in-hour+)
           (* minute 60)
           second))))


;;zone fields values parsing

(defun zone-parse-gmtoff (hour-string)
  (let* ((negative (eql 0 (position #\- hour-string)))
         (hms (if negative
                  (subseq hour-string 1)
                  hour-string)))
    (* (if negative -1 1)
       (rule-parse-save hms))))

;;following are for the RULES/SAVE part of Zone lines

;;base class
(defclass zone-rules/save () ())

;;local std time applies
(defclass zone-rules/save-local-std (zone-rules/save) ())

;;add seconds to the local std time
(defclass zone-rules/save-save (zone-rules/save)
  ((seconds :initarg :seconds :type seconds-in-day-type)))

;;apply rules to the local std time
(defclass zone-rules/save-rules (zone-rules/save)
  ((rules :initarg :rules :type string)))

(defun zone-parse-rules/save (rules-save)
  (if (string-equal "-" rules-save)
      (make-instance 'zone-rules/save-local-std)
      (if (digit-char-p (aref rules-save 0))
          (make-instance 'zone-rules/save-save
                         :seconds (zone-parse-gmtoff rules-save))
          (make-instance 'zone-rules/save-rules :rules rules-save))))

(defun get-zone-line-save (zone-line)
  (let ((zs (slot-value zone-line 'rules/save)))
    (zone-save zs)))

(defgeneric zone-save (zd))

(defmethod zone-save ((zd zone-rules/save-save))
  "for zone lines with a fixed save"
  (slot-value zd 'seconds))

(defmethod zone-save ((zd zone-rules/save ))
  "for zone lines having no direct save value"
  0)

(defun get-zone-line-rules (zone-data)
  (let ((ro (slot-value zone-data 'rules/save)))
    (zone-rules ro)))

(defgeneric zone-rules (ro))

(defmethod zone-rules ((ro zone-rules/save-rules))
  (slot-value ro 'rules))

(defmethod zone-rules ((ro zone-rules/save))
  nil)

;;represents the UNTILYEAR [MONTH [DAY [TIME]]] part of Zone line
(defclass zoneline-until ()
  ((year :initarg :year )
   (month :initarg :month :type (integer 1 12))
   (day-on :initarg :day-on :type day-on)
   (at :initarg :at :type tz-time)))

(defparameter *max-zoneline-until*
  (make-instance 'zoneline-until
                 :year (1- +max-year+) :month 12
                 :day-on (make-instance 'day-on-fixed-day :day 31)
                 :at (rule-parse-at "-")))

(defun zone-parse-untilyear-month-day-time (year-string in on at)
  (if (empty-string-p year-string)
      nil ;; no end time
      (let ((year (parse-integer year-string))
            (month (or (and (empty-string-p in) 1)
                       (rule-parse-month-in in)))
            (day  (or (and (empty-string-p on)
                           (make-instance 'day-on-fixed-day :day 1))
                      (rule-parse-day-on on)))
            (until-time  (or (and (empty-string-p at)
                                  (rule-parse-at "-"))
                             (rule-parse-at at))))
        (make-instance 'zoneline-until
                       :year year :month month :day-on day :at until-time))))

(defgeneric get-day-on-date (day-on year month))

(defmethod get-day-on-date ((day-on day-on-fixed-day) year month)
  (create-date year month (slot-value day-on 'day)))

(defmethod get-day-on-date ((day-on day-on-last-dow) year month)
  (last-dow (slot-value day-on 'dow) month year))

(defmethod get-day-on-date ((day-on day-on-dow>=) year month)
  (dow>= (slot-value day-on 'dow) (slot-value day-on 'day)
         month year))

(defmethod get-day-on-date ((day-on day-on-dow<=) year month)
  (dow<= (slot-value day-on 'dow) (slot-value day-on 'day)
         month year))


(in-package :aas-local-time)

(defun load-timezones ()
  (or *timezones*
      (and
       (setf *timezones* (olson-to-transitions))
       (setf *links* (process-links *links*))
       (map-xed-zones)
       (or (< 300 (length (timezone-names)))
           (error "not enough timezones processed"));;warm up
       (or (setf +timezone-utc+ (get-timezone "UTC"))
           (error "timezone crunch failed")))))

(defclass rule ()
  ((rule-line :initarg :rule-line)
   (from :initarg :from)
   (to :initarg :to)
   (year-checker :initarg :year-checker)
   (in :initarg :in)
   (on :initarg :on)
   (at :initarg :at)
   (save :initarg :save)))

(defun make-rule(rule-line from to year-checker in on at save)
  (make-instance 'rule :rule-line rule-line :from from :to to
                 :year-checker year-checker :in in :on on :at at :save save))

(defclass zone ()
  ((zone-line :initarg :zone-line)
   (gmtoff :initarg :gmtoff)
   (rules/save :initarg :rules/save)
   (until :initarg :until)))

(defun make-zone (zone-line gmtoff rules/save until)
  (make-instance 'zone :zone-line zone-line :gmtoff gmtoff
                 :rules/save rules/save :until until))

(defun load-zones-2 ()
  (let* ((tzdb (process-data-dir))
         (files (get-db-keys tzdb))
         (rules-db (make-db :test #'string-equal))
         (zones-db (make-db :test #'string-equal)))
    (dolist (file files)
      (let* ((filedata  (get-db tzdb file))
             (zone-lines-db (first filedata))
             (rule-lines-db (second filedata))
             (zone-names (get-db-keys zone-lines-db))
             (rule-names (get-db-keys rule-lines-db)))
        (dolist (rule-name rule-names)
          (set-db rules-db rule-name
                  (crunch-rule-pass-1 (get-db rule-lines-db rule-name))))
        (dolist (zone-name zone-names)
          (set-db zones-db zone-name
                  (crunch-zone-pass-1 (get-db zone-lines-db zone-name))))))
    (values zones-db rules-db)))

(defun make-rule-year-checker (from to year-type-string)
  (let ((year-type (rule-parse-year-type year-type-string)))
    (lambda (year)
      (and (>= year from)
           (<= year to)
           (rule-check-year-type year-type year)))))


(defun crunch-rule-pass-1 (rule-lines)
  (let ((rules (list)))
    (dolist (rule-line (first rule-lines))
      (let* ((from-year (rule-parse-from-year (slot-value rule-line 'from)))
             (to-year (rule-parse-to-year (slot-value rule-line 'to) from-year))
             (year-checker (make-rule-year-checker
                            from-year to-year (slot-value rule-line 'type)))
             (month (rule-parse-month-in (slot-value rule-line 'in)))
             (day-on (rule-parse-day-on (slot-value rule-line 'on)))
             (at (rule-parse-at (slot-value rule-line 'at)))
             (save (rule-parse-save (slot-value rule-line 'save)))
             (rule (make-rule rule-line from-year to-year year-checker
                              month day-on at save)))
        (push rule rules)))
    (reverse rules)))

(defun crunch-zone-pass-1 (zone-lines)
  (let ((zones (list)))
    (dolist (zone-line (first zone-lines))
      (let* ((gmtoff (zone-parse-gmtoff (slot-value zone-line 'gmtoff)))
             (rules/save (zone-parse-rules/save (slot-value zone-line 'rules)))
             (until (apply #'zone-parse-untilyear-month-day-time
                           (slot-value zone-line 'until)))
             (zone (make-zone zone-line gmtoff rules/save until)))
        (push zone zones)))
    (reverse zones)))

(defstruct transition
  (date-time)
  (offset)
  (save))

(defun make-tr (date-time offset save)
  (make-transition :date-time date-time :offset offset :save save))

(defmethod print-object ((transition transition) stream)
  (if (and *serialize* *print-readably* *print-escape*)
      (format stream "(make-tr ~S ~S ~S)"
              (slot-value transition 'date-time)
              (slot-value transition 'offset)
              (slot-value transition 'save))
      (call-next-method)))

(define-constant +ambiguous-tr+ 'ambiguous)
(define-constant +blackhole-tr+ 'blackhole)
(define-constant +none-tr+ 'none);occurs if zone offset changes along with DST switch

(defstruct tr-data-point
  (tr nil :type transition)
  (amb-bh)
  (start nil)
  (end nil))

(defmethod zoneline-until ((until zoneline-until) (tr transition))
  (let* ((date (get-day-on-date (slot-value until 'day-on)
                                (slot-value until 'year)
                                (slot-value until 'month)))
         (tz-time (slot-value until 'at)))
    (date-time-to-tsu date tz-time tr)))

(defun date-time-to-tsu (date time tr)
  (let ((usw (tz-time-usw time)))
    (let ((offset (cond ((eq usw +enum-time-type-utc+) 0)
                        ((eq usw +enum-time-type-std+) (slot-value tr 'offset))
                        ((eq usw +enum-time-type-wall+)
                         (+ (slot-value tr 'offset)
                            (slot-value tr 'save))))))
      (make-date-time
       (tz-time-s time) (tz-time-m time) (tz-time-h time)
       (date-d date) (date-m date) (date-y date)
       offset))))


(defmethod rule-tr-tsu ((rule rule) year (tr transition))
  (let* ((date (get-day-on-date (slot-value rule 'on)
                                year
                                (slot-value rule 'in)))
         (tz-time (slot-value rule 'at)))
    (let ((hours (slot-value tz-time 'h)))
      (if (< hours 24)
          (date-time-to-tsu date tz-time tr)
          (multiple-value-bind (days newhours)
              (floor hours 24)
            (date-time-to-tsu
             (add-days date days)
             (make-tz-time
              :h newhours
              :m (slot-value tz-time 'm)
              :s (slot-value tz-time 's)
              :usw (slot-value tz-time 'usw))
             tr))))))

(defun save-transitions (zones pathname)
  (with-open-file (stream pathname :direction :output)
    (with-standard-io-syntax
      (let ((*print-circle* t)
            (*serialize* (1+ *serialize*))
            (*package* #.*package*))
        (format stream ";;auto generated on ~A" (utc-now))
        (pprint `(in-package ,(package-name *package*))
                stream)
        (dolist (zone zones)
          (let ((name (first zone))
                (tr-list (first (rest zone))))
            (pprint `(load-zone-tr-list ,name (list ,@tr-list))
                    stream))))))

  pathname)

(defstruct zone-tr-data
  (name nil :type string)
  (max-year)
  (years))

(defmethod print-object ((data zone-tr-data) stream)
  (if (not *print-readably*)
      (format stream "#<zone-tr-data ~A>#" (zone-tr-data-name data))
      (error "no readable printer yet for type zone-tr-data")))

(defun load-transitions (zones)
  (let ((zones-data (make-hash-table :test #'equalp)))
    (dolist (zone zones)
      (let((name (first zone))
           (tr-list (first (rest zone))))
        (multiple-value-bind (name max-year data)
            (load-zone-tr-list name tr-list)
          (let* ((zone-data (make-zone-data max-year data))
                 (zone-tr-data (make-zone-tr-data
                                :name name :max-year max-year
                                :years zone-data)))
            (setf (gethash name zones-data) zone-tr-data)))))
    zones-data))

(defun make-zone-data (max-year tr-points)
  (let ((vector (make-array (1+ (- max-year +epoch-year+))))
        (year +epoch-year+)
        (offset 0)
        (last-year-data nil)
        (year-data (list)))
    (dolist (tr-data-point tr-points)
      (let* ((tr (slot-value tr-data-point 'tr))
             (date-time (slot-value tr 'date-time))
             (tr-year (slot-value date-time 'year)))
        (if (= tr-year year)
            (push tr-data-point year-data)
            (progn
              (assert (> tr-year year))
              (setf last-year-data (setf  (aref vector offset)
                                          (reverse year-data)))
              (if (< 1 (- tr-year year))
                  (loop for tyear from year to (1- tr-year) do
                       (progn
                         (incf offset)
                         (incf year)
                         (setf (aref vector offset) last-year-data)))
                  (progn
                    (incf offset)
                    (incf year)))
              (assert (= tr-year year))
              (setf year-data (list tr-data-point))))))
    (setf  (aref vector offset) (reverse year-data))
    vector))

(defun load-zone-tr-list (zone transitions)
  "corresponds to the post processing of transitions in
the algorithm documented below"
  (let* ((last-adjust 0)
         (max-year +epoch-year+)
         (tr-data-points (list)))
    (dolist (tr transitions)
      (with-slots (date-time offset save) tr
        (let* ((adjust (+ offset save))
               (adjust-diff (- adjust last-adjust))
               (amb-bh (ecase (signum adjust-diff)
                         ((-1) +ambiguous-tr+)
                         ((1) +blackhole-tr+)
                         ((0) +none-tr+)))
               (tsl1 (dto-to-dt date-time))
               (tsl2 (add-seconds tsl1 adjust-diff))
               (tsmall)
               (tbig))
          (cond ((eq amb-bh +ambiguous-tr+) (progn (setf tsmall tsl2)
                                                   (setf tbig tsl1)))
                ((eq amb-bh +blackhole-tr+) (progn (setf tsmall tsl1)
                                                   (setf tbig tsl2)))
                ((eq amb-bh +none-tr+) (progn (setf tsmall tsl1)
                                              (setf tbig tsl1)))
                (t (error "unknown amb-bh ~A" amb-bh)))
          (when (not (eq  amb-bh +none-tr+))
            (assert (local-date-time< tsmall tbig)))
          ;;(format t "~% ~A ~A ~A" tsmall tbig amb-bh)
          (setf max-year
                (max max-year (slot-value (slot-value tr 'date-time) 'year)))
          (push (make-tr-data-point
                 :tr tr :amb-bh amb-bh
                 :start tsmall :end tbig) tr-data-points)
          (setf last-adjust adjust))))
    (values zone max-year (nreverse tr-data-points))))

(defun olson-to-transitions ()
  (let ((zones (load-zones-1))
        ;; (path (aas-build:relative-to-absolute-path
        ;;        (concatenate 'string +transitions-file-base-name+ ".lisp")))
        (*serialize* 0))
    ;; (declare (ignorable path))
    ;;(save-transitions zones path) ; this is not yet used
    (load-transitions zones)))

(defun load-zones-1 ()
  (multiple-value-bind (zones rules) (load-zones-2)
    (let ((zone-names (get-db-keys zones))
          (transitions (list)))
      (loop for zone-name in zone-names do
           (push (multiple-value-list
                  (crunch-zone-pass-2
                   zone-name (get-db zones zone-name) rules)) transitions))
      (nreverse transitions))))

;;this corresponds to the entry point in the algorithm documented
;;below for generating transitions for a zone
(defun crunch-zone-pass-2 (name data rules)
  ;;(print name)
  (let ((prev-until-tsu (make-date-time 0 0 0 1 1 +epoch-year+))
        (tr-list (list)))
    (dolist (zone-data data)
      (let* ((start-year (dto-year prev-until-tsu))
             (until (or (slot-value zone-data 'until)
                        *max-zoneline-until*))
             (until-year (slot-value until 'year)))
        (when (>= until-year start-year)
          (let* ((last-tr (make-tr prev-until-tsu
                                   (slot-value zone-data 'gmtoff)
                                   (get-zone-line-save zone-data)))
                 (until-tsu (zoneline-until until last-tr))
                 (rule-name (get-zone-line-rules zone-data))
                 (rules (and rule-name (get-db rules rule-name))))
            ;;(format t "~%~A" last-tr)
            (push last-tr tr-list)
            ;;(break)
            (if rules
                (loop for year from start-year to (dto-year until-tsu) do
                     (let ((rules (applicable-rules rules year)))
                       (dolist (rule rules)
                         ;;(format t "~%zone until ~A" until-tsu)
                         (let ((rule-tr-tsu (rule-tr-tsu rule year last-tr)))
                           (when (and (dto< rule-tr-tsu until-tsu)
                                      (dto< (slot-value last-tr 'date-time)
                                            rule-tr-tsu))
                             ;;(format t "~%rule tr ~A until tsu ~A"
                             ;;        rule-tr-tsu until-tsu)
                             (let* ((temp-last-tr (make-tr rule-tr-tsu
                                                           (slot-value zone-data 'gmtoff)
                                                           (slot-value rule 'save)))
                                    ;;(format t "~%last tr ~A" last-tr)
                                    (temp-until-tsu (zoneline-until until temp-last-tr)))
                               (when (dto< rule-tr-tsu temp-until-tsu)
                                 (setf last-tr temp-last-tr)
                                 (setf until-tsu temp-until-tsu)
                                 (push last-tr tr-list)))))))))
            (setf prev-until-tsu until-tsu)))))
    (setf tr-list (sort tr-list #'dto< :key (lambda (tr)
                                              (slot-value tr 'date-time))))
    (values name tr-list)))

(defun applicable-rules (rules year)
  (sort
   (remove-if-not (lambda (rule)
                    (funcall (slot-value rule 'year-checker) year))
                  rules)
   (lambda (r1 r2)
     (< (slot-value r1 'in) (slot-value r2 'in)))))

(defun verify-tr-list (tr-list)
  (let ((prev nil))
    (dolist (next tr-list)
      (when prev
        (assert (dto< (slot-value prev 'date-time)
                      (slot-value next 'date-time))))
      (setf prev next))))

(let ((cached nil))
  (defun timezone-names ()
    (if cached
        cached
        (setf cached (let ((names (list)))
                       (maphash (lambda (k v)
                                  (declare (ignorable v))
                                  (push k names))
                                *timezones*)
                       (nreverse names))))))

(defun process-links (links)
  (let ((nl (make-hash-table :test #'equal)))
    (maphash (lambda (alias zone-name)
               (setf (gethash alias nl) (olsen-data-lookup zone-name)))
             links)
    nl))

#|
|#

;;current REPL test line
#|
(let ((aas-local-time::*timezone-files-list* aas-local-time::*timezone-files-list*))
  (aas-local-time::olson-to-transitions))
|#


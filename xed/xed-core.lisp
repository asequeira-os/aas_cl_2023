(in-package :xed)

(defvar +xeduler+ (cloud:define-app xeduler 142))

(def-rpc-struct calendar
    (id nil :type (or null db-id))
  (name nil :type string) ;;user specified name
  (servicing nil :type boolean)
  (tz nil :type aas-local-time:tz)
  (pub-read nil :type boolean)
  (pub-write nil :type boolean))

(defmethod db-base:get-struct-columns ((symbol (eql 'calendar )))
  '((id db-id)
    (name string)
    (servicing boolean)
    (tz aas-local-time:tz)
    (pub-read boolean)
    (pub-write boolean)))

(defun make-calendar-from-db (&key id name servicing tz pub-read pub-write )
  (make-calendar :id id :name name :servicing servicing :tz tz
                 :pub-read pub-read :pub-write pub-write ))

(defmethod auth:obj-db-id ((obj calendar))
  (calendar-id obj))


;; todo 1 task - reminders cron job part of this server
;; see existing cron functionality (as in cron.lisp)
;; need to provide 'application' specific crons
;; they will run only the servers that serve that application
;; so +xeduler+ app servers will run the reminders cron
;; so we have proper distributed cron



(def-rpc-struct work-shift
    (id nil :type (or null db-id))
  (dow nil :type integer)
  (start-hour nil :type integer)
  (start-minute nil :type integer)
  (end-hour nil :type integer)
  (end-minute nil :type integer))

(cloud:define-error 50100 invalid-work-shift)

(defmethod aas-rpc:deserialize-after ((obj work-shift))
  (or (validate-work-shift obj)
      (raise-error 'invalid-work-shift)))

(defun validate-work-shift (shift)
  (let ((sh (work-shift-start-hour shift))
        (sm (work-shift-start-minute shift))
        (eh (work-shift-end-hour shift))
        (em (work-shift-end-minute shift)))
    (and (<= 0 (work-shift-dow shift) 6)
         (<= 0 sh 23)
         (<= 0 sm 59)
         (<= 0 eh 23)
         (<= 0 em 59)
         (if (and (zerop eh) (zerop em))
             t
             (or (< sh eh)
                 (and (= sh eh) (< sm em)))))))

(defmethod db-base:get-struct-columns ((symbol (eql 'work-shift )))
  '((id db-id)
    (dow  integer)
    (start-hour  integer)
    (start-minute  integer)
    (end-hour  integer)
    (end-minute  integer)))

(defun make-work-shift-from-db
    (&key id  dow  start-hour  start-minute  end-hour  end-minute )
  (make-work-shift :id id :dow dow
                   :start-hour start-hour :start-minute start-minute
                   :end-hour end-hour :end-minute end-minute))

(defun shift-local-start (shift)
  (let ((sh (work-shift-start-hour shift))
        (sm (work-shift-start-minute shift)))
    (let ((base (+ (* sh 3600) sm)))
      (if (zerop base)
          (* 24 3600)
          base))))

(defun shift-local-end (shift)
  (let ((eh (work-shift-end-hour shift))
        (em (work-shift-end-minute shift)))
    (let ((base (+ (* eh 3600) em)))
      (if (zerop base)
          (* 24 3600)
          base))))



;; check following cases
;; case 1
;; s1  ----|--------|--------
;; s2  -------|---------|----
;; case 2
;; s1  ---------|----|-------
;; s2  ------|----------|----
;; case 3
;; s1  -------|---------|----
;; s2  ----|--------|--------
(defun shifts-overlap-one-way (s1 s2)
  (and (= (work-shift-dow s1) (work-shift-dow s2) )
       (let ((ss1 (shift-local-start s1))
             (ss2 (shift-local-start s2))
             (es1 (shift-local-end s1))
             (es2 (shift-local-end s2)))
         (or (and (<= ss1 ss2) (>= es1 ss2))
             (and (>= ss1 ss2) (<= es1 es2))
             (and (<= ss1 es2) (>= es1 es2))))))

(defun shifts-overlap (s1 s2)
  (or (shifts-overlap-one-way s1 s2)
      (shifts-overlap-one-way s2 s1)))

(defun shift-check-overlap (&rest shifts)
  ;;(break)
  (let ((prev (first shifts)))
    (dolist (next (rest shifts))
      (assert (work-shift<% prev next) nil "unsorted shift data?")
      (when (shifts-overlap prev next)
        (raise-error 'invalid-work-shift))))
  t)

(defun work-shift<% (s1 s2)
  (or (< (work-shift-dow s1) (work-shift-dow s2) )
      (and (= (work-shift-dow s1) (work-shift-dow s2))
           (let ((ss1 (shift-local-start s1))
                 (ss2 (shift-local-start s2))
                 (es1 (shift-local-end s1))
                 (es2 (shift-local-end s2)))
             (or (< ss1 ss2)
                 (and (= ss1 ss2)
                      (< es1 es2)))))))

(defun work-shift=% (s1 s2)
  (and (= (work-shift-dow s1) (work-shift-dow s2) )
       (let ((ss1 (shift-local-start s1))
             (ss2 (shift-local-start s2))
             (es1 (shift-local-end s1))
             (es2 (shift-local-end s2)))
         (and (= ss1 ss2) (= es1 es2)))))

(defun work-shift>% (s1 s2)
  (work-shift<% s2 s1))

(aas-cl:def-comparison "work-shift" 'work-shift work-shift>% work-shift<% work-shift=%)





(in-package :aas-local-time)

(defvar *data-dir* (merge-pathnames #P"src/timezone/data/"  main::*SOURCE-TOP*))

(define-constant *timezone-files-list*
    '("africa" "antarctica" "asia" "australasia" "etcetera"
      "europe" "northamerica" "pacificnew" "southamerica" "xed"))

(defvar month-abbreviations
  (list "jan" "feb" "mar" "apr" "may" "jun"
        "jul" "aug" "sep" "oct" "nov" "dec"))

(define-constant month-full-names
    (list "January" "February" "March" "April" "May" "June"
          "July" "August" "September" "October" "November" "December"))

(define-constant dow-abbreviations
    (list "mon" "tue" "wed" "thu" "fri" "sat" "sun"))

(define-constant dow-full-names
    (list "monday" "tuesday" "wednesday" "thursday"
          "friday" "saturday" "sunday"))

;;negative years are not yet allowed
;;has to be bigger than the +xxx-epoch-year+
;;the epoch day and month are 1 1
(define-constant +epoch-year+ 2011) ;;NO change allowed once in production

;;changing this does not break logic
(define-constant +max-year+ 2051)

;;usage of this ignores leap seconds
(define-constant +seconds-in-day+ (* 24 60 60))
(deftype seconds-in-day-type () `(integer 0 ,+seconds-in-day+))
(define-constant +seconds-in-hour+ (* 60 60))

(define-constant +days-in-month+
    #(31 28 31 30 31 30 31 31 30 31 30 31))
(define-constant +days-in-month-leap+
    #(31 29 31 30 31 30 31 31 30 31 30 31))

(defvar *serialize* nil)

(define-constant +transitions-file-base-name+ "transitions")


;; this should never be set globally
(defvar *utc-now* )

(defvar *timezones* nil)

(defvar *timezone* nil) ;;bind to a tz and not the zone-tr-data
(defvar +timezone-utc+ nil)

(extern:define-enum time-type utc std wall)


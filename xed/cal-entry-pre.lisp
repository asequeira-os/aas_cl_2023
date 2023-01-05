(in-package :xed)

(define-enum recur-freq secondly minutely hourly daily weekly monthly yearly)

(define-enum weekdays mon tue wed thu fri sat sun)

(define-enum week-of-month first second third fourth last)

(defun ord-to-weekday (dow-num)
  (make-enum-weekdays-from-db :elm dow-num ))

(define-enum cal-type group individual)

(define-enum free/busy free busy unavailable busy-tentative)

(define-enum participation request accepted declined tentative delegated)

(define-enum repeat-edit-range this thisandfuture)

(define-constant +max-date+ (create-date (1- time::+max-year+) 11 30 t))
(define-constant +max-dto+ (time::make-date-time 0 0 0 1 1 (1- time::+max-year+) 0))


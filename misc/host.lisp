(defpackage :host
  (:use :cl :aas-cl))

(in-package :host)
(export '(my-hostname))

(define-constant +HOSTNAME_ENV_VAR+ "XEDULER_HOST")

(defparameter *hostname* (aas-build:get-env +HOSTNAME_ENV_VAR+))

(main:add-dump-hook (lambda ()
                      (makunbound '*hostname*)))
(main:add-startup-hook
 (lambda ()
   (setf *hostname* (aas-build:get-env +HOSTNAME_ENV_VAR+))))

(if (null *hostname*)
    (error "please set ~A" +HOSTNAME_ENV_VAR+))

(defun my-hostname ()
  *hostname*)

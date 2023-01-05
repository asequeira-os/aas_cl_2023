(in-package :aas-cl)

(defstruct aas-rand-state
  (lock)
  (rs))

(defun aas-random-state (name)
  (make-aas-rand-state :lock (mp-make-lock name) :rs (make-random-state t)))

(defvar *aas-random-state* )

(main:add-startup-hook
 (lambda ()
   (setf *aas-random-state* (aas-random-state "global-random-state"))))

(defun aas-random (limit &optional (random-state *aas-random-state*))
  (mp-with-lock ((aas-rand-state-lock random-state))
    (random limit (aas-rand-state-rs random-state))))




(in-package :aas-cl)

(defun mp-make-lock (&optional name)
  (bordeaux-threads:make-recursive-lock name))

(defun mp-acquire-lock (lock)
  (bordeaux-threads:acquire-recursive-lock lock))

(defun mp-release-lock (lock)
  (bordeaux-threads:release-recursive-lock lock))

(defmacro mp-with-lock ((lock) &body body)
  `(bordeaux-threads:with-recursive-lock-held (,lock)
     ,@body))

;;todo 2 special bindings for thread
;;i probably shouldn't do the below since it just gets me
;;the stuff bound to load time values
(setf bordeaux-threads:*default-special-bindings*
      `((,'*standard-input* . ,*standard-input*)
        (,'*standard-output* . ,*standard-output*)
        (,'*query-io* . ,*query-io*)
        (,'*trace-output* . ,*trace-output*)))

;;todo 2 see above about bindings
;;I probably should make bindings a optional with
;;a default that is computed in the dynamic context
(defun mp-make-thread (name function
                       &optional (bindings bordeaux-threads:*default-special-bindings*))
  (bordeaux-threads:make-thread function
                                :name name
                                :initial-bindings bindings))

;;todo 3 CCL does not have atomic ops. keep an eye out
;; (defmacro mp-atomic-incf (place &optional (delta 1))
;;   "current CCL implementation looks very lame"
;;   `(portable-threads:atomic-incf ,place ,delta))

;; (defmacro mp-atomic-decf (place &optional (delta 1))
;;   "current CCL implementation looks very lame"
;;   `(portable-threads:atomic-decf ,place ,delta))

(defmacro mp-thread-local (var value &body body)
  `(let ((,var (or ,var ,value)))
     ,@body))

(let ((lock (mp-make-lock "singleton_fn")))
  (defun singleton (fn ignore-errors)
    (let ((hot nil))
      (lambda ()
        (let ((execute nil))
          (unwind-protect
               (progn
                 (mp-with-lock (lock)
                   (unless hot
                     (setf hot t)
                     (setf execute t)))
                 (when execute
                   (if ignore-errors
                       (ignore-errors
                         (funcall fn))
                       (funcall fn))))
            (when execute
              (mp-with-lock (lock)
                (setf hot nil)))))))))


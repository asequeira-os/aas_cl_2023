(in-package :notify)

;;notification/listener interface
;;ideal would require specifying dependencies, building graph, finding cycles
;;i am not doing all that

(defvar *my-package* *package*)

(defvar *h* (make-hash-table :test #'eql))

(defstruct notification
  (hash (make-hash-table :test #'eql))
  (fn))

(defmacro define-notifier (name)
  `(unless (gethash ',name notify::*h*)
     (setf (gethash ',name notify::*h*) (make-notification))))

;;passing function instead of a symbol for 'callback'
;;risks duplicate notifications
(defun add-listener (name order callback)
  (unless (zerop order)
    (error "notification ordering not yet supported"))
  (let ((callback-fn (if (symbolp callback)
                         (symbol-function callback)
                         callback)))
    (let ((notification (gethash name *h*)))
      (setf (gethash callback (notification-hash notification))
            callback-fn)
      (setf (notification-fn notification) (build-notifier notification)))))

(defun remove-listener (name callback)
  (let ((notification (gethash name *h*)))
    (remhash callback (notification-hash notification))
    (setf (notification-fn notification) (build-notifier notification))))


(defun build-notifier (notification)
  (let ((list (list)))
    (maphash (lambda (k v)
               (declare (ignorable k))
               (push v list))
             (notification-hash notification))
    (lambda (object)
      (dolist (fn list)
        (funcall fn object)))))


(defun notify-all (name object)
  (funcall (notification-fn (gethash name *h*)) object))

(in-package :log)

(defvar *stream* nil)

(defvar *level* 10)
(defvar *warnings-p* t)
(defvar *errors-p* t)
(defvar *buffer* t)

(defparameter *log-dir* nil)
(defparameter *field-separator* #\tab)

(defun log-info (level message-format &rest args)
  (unless (< *level* level)
    (apply #'impl level message-format args)))

(defun log-warn (message-format &rest args)
  (when *warnings-p*
    (apply #'impl "W" message-format args)))

(defun log-fail (message-format &rest args)
  (when *errors-p*
    (apply #'impl "F" message-format args)))


(defvar *lock* (mp-make-lock "log"))
(defvar *custom-field-functions-set* (make-hash-table :test #'eql))
(defvar *custom-field-function* (lambda (stream)
                                  (declare (ignorable stream))))

(defun log-flush ()
  "safe to call anywhere at run time"
  (mp-with-lock (*lock*)
    (and *stream* (force-output *stream*))))

(defun impl (level message-format &rest args)
  (let ((s (with-output-to-string (s)
             (princ " " s) ;;space before length
             (add-timestamp s)
             (add-level level s)
             (funcall *custom-field-function* s)
             (apply #'format s message-format args))))
    (let ((length (length s)))
      (mp-with-lock (*lock*)
        (write length :stream *stream*)
        (write s :stream *stream* :escape nil)
        (terpri *stream*)
        (or *buffer* (force-output *stream*))))))


(defun add-level (level stream)
  (format stream "~A~A" level *field-separator* ))

(defun add-timestamp (stream)
  (format stream "~A~A" (timestamp-string) *field-separator*))

(defun timestamp-string ()
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
            year month date hour minute second)))

(defun register-field (name function)
  "name - a symbol, names the field. function should take no arguments and return a object that can be printed (stick to strings for now)"
  (check-type name symbol)
  (setf (gethash name *custom-field-functions-set*) function)
  (let* ((symbols (get-hash-keys *custom-field-functions-set*))
         (sorted (sort symbols
                       (lambda (s1 s2)
                         (or (string< (symbol-name s1) (symbol-name s2))
                             (string< (package-name (symbol-package s1))
                                      (package-name (symbol-package s2))))))))
    (let ((code (list)))
      (dolist (symbol sorted)
        (push
         `(format stream "~A~A"
                  (funcall ,(gethash symbol *custom-field-functions-set* ))
                  *field-separator*)
         code))
      (setf code (nreverse code))
      (setf *custom-field-function* (compile nil `(lambda (stream) ,@code))))))

(defun make-log-filename ()
  "log.txt")

(defun make-rotated-log-filename ()
  (concatenate 'string "log." (timestamp-string) ".txt"))

(defun log-init ()
  (aas-build::load-config "log-config" t)
  (or *log-dir* (error "please setup *log-dir* in log-config.lisp"))
  (let* ((file-name (make-log-filename))
         (rotated-file-name (make-rotated-log-filename))
         (file-path (merge-pathnames file-name *log-dir*))
         (rotated-file-path (merge-pathnames rotated-file-name *log-dir*)))
    (mp-with-lock (*lock*)
      (when *stream*
        (close *stream*))
      (ignore-errors
        (rename-file file-path rotated-file-path))
      (setf *stream* nil)
      (setf *stream*
            (or (open file-path
                      :direction :output
                      :element-type 'character
                      :if-exists :error
                      #+:openmcl #+:openmcl
                      :sharing :lock)
                (error "can not create log file ~A" file-path))))))

(defvar *log-trace* t)

(defmacro with-logged-error (&body body)
  "logs any error that occurs, but does not handle it"
  ;;at present i will log every error type
  ;;will have to figure out later how to
  ;;make selective logging decisions
  `(handler-bind
       ((error (lambda(c)
                 (log-fail "exception ~A~%~A"
                           c (print-backtrace c (not *log-trace*)))
                 (log-flush))))
     ,@body))

(let ((singleton
       (aas-cl:singleton
        (lambda ()
          (log-init))
        nil)))
  (defun log-rotate ()
    (funcall singleton)))


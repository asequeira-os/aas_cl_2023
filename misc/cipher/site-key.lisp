(in-package :cipher)

;;place to manage local keys and encryption/decryption using them

(defvar *keys-file-dir* )
(main:add-startup-hook (lambda ()
                         (setf *keys-file-dir*
                               (aas-build:get-env "LISP_TEMP_DIR"))
                         (or (cl-fad::directory-exists-p *keys-file-dir*)
                             (error "dir ~A does not exist" *keys-file-dir*))))

(defparameter *site-key-ttl-seconds* (* 60 60 24 2))

(defvar *site-keys-list* (list))
(defvar *site-key-latest* )

(defun encrypt-using-site-key (data)
  (encrypt data (encryption-key-key *site-key-latest*)))

(defun decrypt-using-site-key (data)
  (decrypt data (encryption-key-key *site-key-latest*)))

(let ((lock (mp-make-lock)))
  (defun init ()
    (mp-with-lock (lock)
      ;;todo 1 setup cron to call init regularly
      (unless (and (boundp '*site-key-latest*) (valid-key-p *site-key-latest*))
        (or (load-keys)
            (error "failed site keys"))))))

(defstruct encryption-key
  (key nil :type string :read-only t)
  (eol nil :type integer :read-only t))

(defun valid-key-p (key-struct)
  (< (get-universal-time) (encryption-key-eol key-struct)))

(let ((lock (mp-make-lock "keygen")))
  (defun make-new-key ()
    (mp-with-lock (lock)
      (make-new-key-thread-unsafe))))

(let ((rs (make-random-state t)))
  (defun make-new-key-thread-unsafe ()
    (let ((key
           (with-output-to-string (s)
             (princ
              (random (1- most-positive-fixnum) rs) s)
             (princ (get-universal-time) s)
             ;;todo 2 improve site key generation algorithm
             (princ (random most-positive-fixnum rs) s))))
      (make-encryption-key :key key
                           :eol (+ *site-key-ttl-seconds* (get-universal-time))))))

(defun key-file-path ()
  (merge-pathnames "keys-data.lisp" *keys-file-dir*))

(defvar *temp-keys-list*)

(defun load-keys ()
  (let ((path (key-file-path))
        (*temp-keys-list* (list)))
    (when (probe-file path)
      (or (load path)
          (error "bad site keys file")) )
    (when (zerop (length  *temp-keys-list*))
      (push (make-new-key) *temp-keys-list*))
    (setf *temp-keys-list*
          (sort *temp-keys-list* #'> :key #'encryption-key-eol))
    (unless (valid-key-p (first *temp-keys-list*))
      (push (make-new-key) *temp-keys-list*))
    (save-keys *temp-keys-list*)
    (setf *site-key-latest* (first *temp-keys-list*))
    (setf *site-keys-list* *temp-keys-list*)))

(defun load-saved-key (keys-struct)
  (push keys-struct *temp-keys-list*))

;;generate and save keys to file
(defun save-keys (keys)
  (with-open-file (kf (key-file-path)
                      :direction :output
                      :if-exists :supersede)
    (format kf "(in-package :cipher)~%")
    (dolist (ks keys)
      (format kf "(load-saved-key ~S)~%" ks))))

#|
slight improvement(may be) over what's in xeduler.org
h1 wants to call h2
h1 needs to establish trust with h2

h1 calls trust init rpc at h2
trust init rpc looks like
(trust-request h1 h2 t1)
where
h1 is the name of initiating host
h2 is name of the host h1 wants to call
t1 is a newly generated token by h1

h2 checks if h1 is a known host name. if so says init is confirmed.
h1 marks trust to h2 as pending with token t1
this ends the trust-request rpc call

h2 calls trust establish rpc on h1. this call looks like
(trust-confirm h1 h2 t1 t2 ttl)
where
t2 is a newly generated token by h2 and assigned to h1 by h2
ttl is a time to live in seconds

h1 checks that t1 was a pending token. says trust set is ok
h1 keeps received tokens as trust tokens on a per host basis
h2 keeps track of all assigned token on a per host basis along with their ttl


now to call any rpc on h2, h1 needs to send t2 as the auth token

this means every host has two list of tokens. one it has given out and one it has received


|#

(in-package :aas-rpc)

;;configuration
(defparameter *max-trust-establish-tries* 3)
(defvar  *host-trust-ttl-seconds* (* 30 60))
(define-constant +ttl-edje-seconds+ 10)

(defvar *trusted-hosts* (make-hash-table :test #'equal))

(defun trusted-host-p (host)
  (values (gethash host *trusted-hosts*)))

(defun add-trusted-host (host)
  (setf (gethash host *trusted-hosts*) t))

;;globals
(defvar *pending-tokens* (make-hash-table :test #'equal)
  "tokens sent pending confirmation")
(defvar *received-tokens* (make-hash-table :test #'equal)
  "to be used for making calls")
(defvar *my-tokens* (make-hash-table :test #'equal)
  "use for authenticating caller")
(defvar *trust-hashes-lock* (mp-make-lock "rpc-trust-hashes"))

(define-constant +host-trust+ "host trust rpc realm" )

(defun make-token ()
  (cipher:encryption-key-key (cipher:make-new-key)))

(def-rpc string trust-request (:anonymous t :application +host-trust+)
    (from-host string to-host string from-token string)
  (unless (equal (host:my-hostname) to-host)
    (error "my name is not ~A" to-host))
  (unless (and from-token (stringp from-token) (> (length from-token) 10))
    (error "invalid host trust token ~A" from-token))
  (unless (trusted-host-p from-host)
    (error "I don't know ~A" from-host))
  (when (confirm-trust-async from-host from-token)
    "ok"))

(def-rpc string trust-confirm (:anonymous t :application +host-trust+)
    (from-host string to-host string
               from-token string to-token string
               ttl integer)
  (mp-with-lock (*trust-hashes-lock*)
    (unless (equal (host:my-hostname) from-host)
      (error "my name is not ~A" from-host))
    (unless (equal (gethash to-host *pending-tokens*) from-token)
      (error "host trust token mismatch"))
    (remhash to-host *pending-tokens*)
    (setf (gethash to-host *received-tokens*)
          (cons to-token (+ (get-universal-time) ttl))))
  "ok")

(defun confirm-trust-async (from-host from-token)
  (let ((fn (lambda ()
              (confirm-trust-sync from-host from-token))))
    (mp-make-thread "rpc-confirm" fn)))

(defun confirm-trust-sync (from-host from-token)
  (let ((ttl *host-trust-ttl-seconds*)
        (trust-token (make-token)))
    (if (call-remote-anon from-host #'trust-confirm from-host
                          (host:my-hostname)
                          from-token trust-token ttl)
        (mp-with-lock (*trust-hashes-lock*)
          (setf (gethash from-host *my-tokens*)
                (cons trust-token
                      (+ (get-universal-time) ttl +ttl-edje-seconds+)))))))

(defun call-establish-trust-rpc (to-host)
  (let ((from-token (make-token)))
    (mp-with-lock (*trust-hashes-lock*)
      (setf (gethash to-host *pending-tokens*) from-token)
      (if (call-remote-anon to-host #'trust-request (host:my-hostname)
                            to-host from-token)
          t
          (remhash to-host *pending-tokens*)))))


(defun establish-trust-try (to-host try)
  (when (>= try *max-trust-establish-tries*)
    (error "fail to work with host ~A" to-host))
  (let ((rt (gethash to-host *received-tokens*))
        (expiry-point (- (get-universal-time) +ttl-edje-seconds+)))
    (if (and rt (> (cdr rt) expiry-point))
        (car rt)
        (progn
          (call-establish-trust-rpc to-host)
          (sleep (+ 1 (* try 5)))
          (establish-trust-try to-host (1+ try))))))

(defun establish-trust (to-host)
  (establish-trust-try to-host 0))

(defun verify-trust (from-host token)
  (let ((expected (gethash from-host *my-tokens*)))
    (unless expected
      (error "no host token found"))
    (when (> (get-universal-time) (cdr expected))
      (error "trust token for ~A has expired" from-host))
    (unless (equal token (car expected))
      (error "trust token mismatch"))
    (values from-host (cdr expected))))

(defmethod app-rpc-handler :before ((app (eql +host-trust+))
                                    auth-token rpc-meta function args)
  (declare (ignorable rpc-meta function args))
  (and auth-token
       (error "why trust rpc called with auth token")))


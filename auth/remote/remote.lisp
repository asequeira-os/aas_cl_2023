(in-package :auth-remote)

(define-constant MAX-AUTH-CACHE 1024)

(defvar *remote-auth-cache* (cache:make-aas-cache MAX-AUTH-CACHE))

(defun auth-verify-remote (token allow-inactive)
  (assert (or allow-inactive (auth-token-a token)) nil
          "inactive login not allowed")
  (assert (or (auth-verify-remote-uncached token nil)
              (auth-verify-remote-uncached token t))))

(defun auth-verify-remote-uncached (token cleancache)
  (unless token
    (error "no token supplied"))
  (when cleancache
    (cache:cache-remove *remote-auth-cache* (cache:cache-key token)))
  (cache:with-cached-object tkn (cache:cache-key token) *remote-auth-cache*
      (let ((host (auth-token-h token)))
        (and (equal "OK"
                    (aas-rpc:call-remote-impersonate host #'auth:verify-auth
                                                     token))
             token))
    (equal (auth-token-g tkn) (auth-token-g token))))




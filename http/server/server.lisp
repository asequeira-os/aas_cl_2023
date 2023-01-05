(in-package :aas-http-server)

(defvar *hunchentoot* nil)
(defvar *hunchentoot-secure* nil)
(defvar *http-basepath* nil)
(defvar *reverse-proxy-http-port* nil)
(defvar *reverse-proxy-https-port* nil)

(define-constant +cert-passphrase+ "ght^%&drthi23")

;;self signed cert created using
;; openssl genrsa -des3 -out server.key 1024
;; openssl req -new -key server.key -out server.csr
;; openssl x509 -req -days 3650 -in  server.csr -signkey server.key -out server.crt
;;
;;dev cert issues can mean in browser you have to set excpetion for
;; xed01.local.mmm  AND xed01 separately

;;todo 5 how to handler http request processing errors
;;for now trying to set to use debugger
(setf hunchentoot:*catch-errors-p* nil)

(defun start-server (port secure-port http-basepath
                     certificate-file-path private-key-file-path)
  (setf *http-basepath* http-basepath)
  (register-all-handlers)
  (when port
    (when *hunchentoot*
      (hunchentoot:stop *hunchentoot*))
    (setf *hunchentoot*
          (hunchentoot:start
           (make-instance 'xed-acceptor :port port))))
  (when secure-port
    (when *hunchentoot-secure*
      (hunchentoot:stop *hunchentoot-secure*))
    (setf *hunchentoot-secure*
          (hunchentoot:start
           (make-instance 'xed-ssl-acceptor :port secure-port
                          :ssl-privatekey-password +cert-passphrase+
                          :ssl-certificate-file certificate-file-path
                          :ssl-privatekey-file private-key-file-path))))
  (and (not (and port (null *hunchentoot*)))
       (not (and secure-port (null *hunchentoot-secure*)))))

(defun stop-server ()
  (and *hunchentoot* (hunchentoot:stop *hunchentoot*))
  (and *hunchentoot-secure* (hunchentoot:stop *hunchentoot-secure*))
  (setf *hunchentoot* nil)
  (setf *hunchentoot-secure* nil))

(defun restart-server (port secure-port http-basepath
                       certificate-file-path private-key-file-path)
  (stop-server)
  (start-server port secure-port http-basepath
                certificate-file-path private-key-file-path))

(defvar *prefix-handlers* (make-hash-table :test #'equal))

(defun register-handler (prefix handler)
  (setf (gethash prefix *prefix-handlers*) handler))

(defun register-all-handlers ()
  (let ((list (list)))
    (maphash (lambda (prefix handler)
               (push (hunchentoot:create-prefix-dispatcher
                      (get-handler-uri-path prefix) handler)
                     list))
             *prefix-handlers*)
    (setq hunchentoot:*dispatch-table* list)))

(defun get-handler-uri-path (handler-prefix)
  (concatenate 'string *http-basepath* handler-prefix))

(defun get-http-param (param)
  (or (hunchentoot:post-parameter param)
      (hunchentoot:get-parameter param)))

(defun https-p ()
  (hunchentoot:acceptor-ssl-p
   (hunchentoot:request-acceptor hunchentoot:*request*)))

(defun post-p ()
  (eql (hunchentoot:request-method*) :post))

(defun client-ip ()
  (hunchentoot:real-remote-addr))

(defun accept-language ()
  (let* ((raw (hunchentoot:header-in* "Accept-Language"))
         (parts (split-sequence:split-sequence #\, raw)))
    (car parts)))

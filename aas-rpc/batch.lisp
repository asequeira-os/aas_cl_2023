(in-package :aas-rpc)

(defstruct batch-entry
  (package)
  (name)
  (rpc-meta)
  (params)
  (id)
  (response))

(defun call-remote-batch (host requests)
  (assert (and requests (listp requests) (< 1 (length requests))))
  (let ((batch (list)))
    (dolist (request requests)
      (let ((function (first request))
            (args (rest request)))
        (multiple-value-bind (package name)
            (rpc-reverse-lookup function)
          (let ((rpc-meta (get-rpc-meta-data package name)))
            (let ((params (make-rpc-params rpc-meta args)))
              (push (make-batch-entry :package package
                                      :name name
                                      :rpc-meta rpc-meta
                                      :params params) batch))))))
    (setf batch (nreverse batch))
    (json-rpc-2-call-remote-batch host batch)))

(defun json-rpc-2-call-remote-batch (host batch)
  (let ((token (establish-trust host)))
    (multiple-value-bind (package name)
        (rpc-reverse-lookup #'json-rpc-2-batch)
      (let* ((uri-path (get-handler-uri-path +json-rpc-2-prefix+))
             (full-uri (make-base-url "https"
                                      *reverse-proxy-https-port* host uri-path))
             (parameters (list (cons "method"
                                     (json-method-string package name))
                               (cons "host" (host:my-hostname))
                               (cons "token" token)
                               (cons "request"
                                        (json-rpc-2-batch-array batch)))))
        ;;(pprint parameters)
        (multiple-value-bind (json-resp-str http-code http-headers)
            (drakma:http-request full-uri
                                 :method :post
                                 :parameters parameters)
          (declare (ignorable http-headers))
          (unless (= http-code 200)
            (error "http non OK status ~A" http-code))
          (decode-json-batch-response batch json-resp-str))))))

(defun json-rpc-2-batch-array (batch)
  (with-output-to-string (stream)
    (sout-begin-vector +json-format+ stream)
    (let ((comma nil))
      (dolist (req batch)
        (when comma
          (sout-add-comma +json-format+ stream))
        (setf comma t)
        (multiple-value-bind (package name rpc-meta args)
            (values (batch-entry-package req) (batch-entry-name req)
                    (batch-entry-rpc-meta req) (batch-entry-params req))
          (multiple-value-bind (request id)
              ;;todo 2 host to host batch does not support impersonation yet
              (ctor-json-request-str package name args rpc-meta nil)
            (setf (batch-entry-id req) id)
            ;;vector of two elements - method, request object
            (sout-begin-vector +json-format+ stream )
            (sout-object +json-format+ stream 'string
                         (json-method-string package name))
            (sout-add-comma +json-format+ stream)
            (princ request stream)
            (sout-end-vector +json-format+ stream ))))
      (sout-end-vector +json-format+ stream))))

(defun id-from-idkey (idkey)
  (or (> (length idkey) 2)
      (error "expected id key like id###, got ~A" idkey))
  (parse-integer (subseq idkey 2)))

(defun decode-json-batch-response (batch json-resp-str)
  (let ((seen 0))
    (with-input-from-string (stream json-resp-str)
      (sin-begin-object +json-format+ stream "ignored")
      (util:while (not (sin-end-object +json-format+ stream "ignored"))
        (let ((id (id-from-idkey
                   (sin-object-attr-name +json-format+ stream nil))) )
          (sin-attr-name-value-seprator +json-format+ stream)
          (let ((req (find id batch :key #'batch-entry-id)))
            (let ((rpc-meta (batch-entry-rpc-meta req)))
              (incf seen)
              (setf (batch-entry-response req)
                    (decode-json-rpc-2-response rpc-meta stream)))))
        (skip-optional-comma stream)))
    (values (mapcar (lambda (entry)
                      (batch-entry-response entry)) batch)
            (= seen (length batch)))))




(def-rpc vector  json-rpc-2-batch
    (:host-to-host-only nil :post-only t :application "dummy")
    (requests vector )
  (declare (ignorable requests))
  (error "bug - this is a meta proxy. should not be called"))






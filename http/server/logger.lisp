(in-package :aas-http-server)


(defclass http-access-logger () ())

(defclass xed-acceptor (hunchentoot:easy-acceptor http-access-logger) ())
(defclass xed-ssl-acceptor (hunchentoot:easy-ssl-acceptor http-access-logger) ())

;;todo 3 make access log independent


(defmethod hunchentoot:acceptor-log-access
    ((acceptor xed-ssl-acceptor) &key return-code content content-length)
  (declare (ignorable content content-length))
  (log:log-info 2 "apache access ~A" (format-access-line return-code))
  )

(defmethod hunchentoot:acceptor-log-message
    ((acceptor xed-ssl-acceptor) log-level format-string &rest format-arguments)
  (ecase log-level
    (:error (log:log-fail "http server error ~A"
                          (apply #'format nil format-string format-arguments)))
    (:info (log:log-info 2 "http server message ~A"
                         (apply #'format nil format-string format-arguments)))
    (:warning (log:log-warn "http server warning ~A"
                            (apply #'format nil format-string format-arguments)))))

(defun format-access-line (return-code)
  ;;copied from hunchentoot - supposed to be apache kind access log format
  (format nil "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
          (hunchentoot:remote-addr*)
          (hunchentoot:header-in* :x-forwarded-for)
          (hunchentoot:authorization)
          (hunchentoot::iso-time)
          (hunchentoot:request-method*)
          (hunchentoot:script-name*)
          (hunchentoot:query-string*)
          (hunchentoot:server-protocol*)
          return-code
          (hunchentoot:content-length*)
          (hunchentoot:referer)
          (hunchentoot:user-agent)))

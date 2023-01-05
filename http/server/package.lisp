(defpackage :aas-http-server
  (:use :common-lisp :aas-cl))

(in-package :aas-http-server)

(export '(get-http-param
          start-server
          stop-server
          restart-server
          get-handler-uri-path
          register-handler
          https-p
          post-p
          client-ip
          accept-language
          *reverse-proxy-http-port*
          *reverse-proxy-https-port*))

(defpackage :aas-http-server-test
  (:use :common-lisp :aas-cl :aas-http-server :aseq-test))


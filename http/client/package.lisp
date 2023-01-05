(defpackage :aas-http-client
  (:use :common-lisp :aas-cl :aas-http-common))

(in-package :aas-http-client)

(export '(get-http-url filled-http-link))

(defpackage :aas-http-client-test
  (:use :common-lisp :aas-cl :aas-http-client :aseq-test))

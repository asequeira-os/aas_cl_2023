(defpackage :cache
  (:use :cl :aas-cl))

(in-package :cache)

(export '(cache-key cache-ttl
          cache-get cache-put cache-remove
          with-cached-object
          make-aas-cache))

(defpackage :cache-test
  (:use :cl :aas-cl :aas-misc :aseq-test))

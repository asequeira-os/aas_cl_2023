;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;sole purpose is to load all the thirdparty libs

(asdf:defsystem :thirdparty
    :depends-on

(
:alexandria
:babel
:bordeaux-threads
:cffi
:chunga
:cl-base64
:cl-fad
:cl-interpol
:cl-json
;;:cl-package-aliases
:cl-ppcre
:clsql
:cl+ssl
:cl-utilities
:cl-who
:drakma
:fiveam
:flexi-streams
:hunchentoot
:ieee-floats
;;:lisa
:lisp-unit
:local-time
:md5
:parenscript
:portable-threads
:postmodern
:puri
:rfc2109
:rfc2388
:rt
:selenium
:split-sequence
:s-utils
:trivial-features
:trivial-garbage
:trivial-gray-streams
:trivial-http
:trivial-sockets
:trivial-utf-8
:uffi
:url-rewrite
:usocket
:xmls



))

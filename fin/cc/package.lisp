(defpackage :fin-cc
  (:use :cl :aas-cl :db-base :aas-rpc :cloud))

(in-package :fin-cc)
(export '(*user-login-token-ttl-seconds*
          +fin-cc+
          enable-authorize-net-prod
          ))

(defvar +fin-cc+ (cloud:define-app fin-cc 47))

(defpackage :fin-cc-test
  (:use :cl :fin-cc :aseq-test)
  (:import-from :auth
                login-response-login-token )
  (:import-from :fin-cc
                *cc-vendors*
                *visa*
                +fin-cc+
                authorize-net-avs-status
                avs-status-flags-avs-error
                avs-status-flags-no-info
                avs-status-flags-non-us-bank
                avs-status-flags-not-applicable
                avs-status-flags-service-n/a
                avs-status-flags-street
                avs-status-flags-zip4
                avs-status-flags-zip5
                avs-status-off
                avs-status-on
                cc-profile-cc-num-enc
                cc-profile-dbid
                cc-profile-first-name
                cc-profile-id
                cc-profile-resp-accepted
                cc-profile-resp-cc-profile
                cc-profile-resp-cc-profile
                cc-profile-resp-message
                cc-vendor-key
                cc-vendor-name
                cc-vendors-list
                delete-cc-user-profile
                enable-authorize-net-dev
                get-user-cc-list
                luhn
                make-cc-profile
                update-cc-user-profile
                validate-cc-num))


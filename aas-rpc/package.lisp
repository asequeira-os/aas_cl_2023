(defpackage :aas-rpc
  (:use :cl :aas-cl :aas-http-server :aas-http-client :aas-http-common))

(in-package :aas-rpc)
(export '(
          def-rpc
          def-rpc-struct
          app-rpc-handler
          *secure-transport*
          *client-ip*
          establish-trust
          deserialize-after
          sout-object
          call-remote-anon
          call-remote-trust
          call-remote-impersonate
          call-remote-trust-impersonate
          set-deserializer
          +json-format+))

(defpackage :aas-rpc-test
  (:use :cl :aas-cl :aas-rpc :aseq-test)
  (:import-from :aas-rpc
                *deserializers*
                *my-tokens*
                *optimize-local-calls*
                *pending-tokens*
                *post-request*
                *received-tokens*
                *secure-transport*
                +MAX-JSON-STRING-LENGTH+
                call-remote-batch
                call-rpc-function
                ctor-call-remote-url
                deserialize-null
                get-rpc-meta-data
                json-print-primitive
                json-read-integer
                json-read-string
                json-rpc-2-call-http
                json-rpc-2-error-code
                json-rpc-2-error-message
                json-rpc-2-server
                rpc-error
                rpc-error-cause))


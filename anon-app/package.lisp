(defpackage :anon-app
  (:use :cl :aas-cl :db-base :aas-rpc :cloud :aas-local-time)
  (:import-from :extern
                enum-element
                enum-elm-string))

(in-package :anon-app)
(export '(+anon-app+
          ))

(defpackage :anon-app-test
  (:use :cl :anon-app :aseq-test)
  (:import-from :i18n
                country-iso2)
  (:import-from :time
                tz-name))

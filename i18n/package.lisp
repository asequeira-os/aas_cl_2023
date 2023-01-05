(defpackage :i18n
  (:use :cl :aas-cl))

(in-package :i18n)

(export '(*locale*
          *usa*
          country language
          get-countries
          get-country-languages
          get-languages
          get-text
          i18n-format
          locale
          set-text
          supported-locale-p
          with-locale))

(defpackage :i18n-data
  (:use :cl :i18n))

(defpackage :i18n-test
  (:use :cl :aas-cl :i18n :aseq-test)
  (:import-from :i18n
                *countries*
                *languages*
                *locales*))


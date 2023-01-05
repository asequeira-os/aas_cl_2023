(in-package :i18n)

(defvar *usa* (define-country "US" "USA" "U.S.A"
                              (get-language "en")))
(defvar *canada* (define-country "CA" "CAN" "Canada"
                                 (get-language "en")))
(defvar *france* (define-country "FR" "FRA" "France"
                                 (get-language "fr")))
(defvar *uk* (define-country "GB" "GBR" "United Kingdom"
                             (get-language "en")))
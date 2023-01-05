(in-package :anon-app)

(defvar +anon-app+ (cloud:define-app anon-app 0))

(cloud:set-application-db-count +anon-app+ 0)
(cloud:init +anon-app+)

(def-rpc (vector i18n:country) get-countries
    (:post-only nil :anonymous t :application +anon-app+)
    ()
  (coerce (i18n:get-countries) 'vector))

(def-rpc (vector tz) get-country-timezones
    (:post-only nil :anonymous t :application +anon-app+)
    (cc string)
  (let ((zones (geo:get-country-timezone-names cc)))
    (coerce (mapcar (lambda (name)
                      (get-timezone name)) zones)
            'vector)))

(def-rpc (vector i18n:language) get-languages
    (:post-only nil :anonymous t :application +anon-app+)
    (cc (or null string))
  (coerce (if cc
              (i18n:get-country-languages cc)
              (i18n:get-languages))
          'vector))

(def-rpc-struct enum-extern
    (key nil :type string)
  (i18n nil :type (or null string)))

(def-rpc (vector enum-extern) get-enum-list
    (:post-only nil :anonymous t :application +anon-app+)
    (enum symbol)
  (let ((list (extern:enum-list  enum)))
    (when list
      (let ((i18n-text-ar (i18n:get-text (find-symbol-in-package (symbol-package enum)
                                                                 "enum-" enum))))
        (unless i18n-text-ar
          (error "i18n not set for ~A" enum))
        (assert (= (length list) (length i18n-text-ar)) nil
                "i18n array size mismatch for enum ~A" enum)
        (coerce (loop for e across list
                   for i18n across i18n-text-ar
                   collect  (make-enum-extern :key (enum-elm-string
                                                    (enum-element e))
                                              :i18n i18n))
                'vector)))))

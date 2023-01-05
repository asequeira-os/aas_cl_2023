(in-package :i18n)

(defvar *languages* (make-hash-table :test #'equal))
(defvar *countries* (make-hash-table :test #'equal))
(defvar *locales* (make-hash-table :test #'equal))

(defvar *locale* )

(aas-rpc:def-rpc-struct language
    (iso2 nil :type string)
  (name nil :type string)) ;;default name

(aas-rpc:def-rpc-struct country
    (iso2 nil :type string)
  (iso3 nil :skip-rpc :type (or null string))
  (name nil :type (or null string)) ;;default name
  (language nil :type (or null language)) ;;default language
  (languages nil :skip-rpc))

(defmethod aas-rpc:deserialize-after ((object country))
  (or
   (get-country (country-iso2 object))
   (error "Invalid country code ~A" (country-iso2 object))))


(defstruct (locale (:constructor make-locale-%))
  (key nil :type string)
  (language nil :type language)
  (country nil :type (or null country))
  (default nil :type (or null locale))
  (dictionary (make-hash-table :test #'eq)))

(defmethod aas-rpc:sout-object (ser-type stream type (object locale))
  (declare (ignorable type))
  (aas-rpc:sout-object ser-type stream 'string (locale-key object)))

(aas-rpc:set-deserializer
 'locale
 (lambda (ser-type stream obj-type)
   (get-locale (aas-rpc::deserialize-string ser-type stream obj-type))))

(defun define-language (iso2 name)
  (setf (gethash iso2 *languages*)
        (make-language :iso2 (string-downcase iso2) :name name)))

(let ((list))
  (defun get-languages ()
    (if list
        list
        (progn
          (maphash (lambda (lc language)
                     (declare (ignorable lc))
                     (push language list)) *languages*)
          (setf list (sort list #'string-lessp :key #'language-name))
          list))))

(defun define-country (iso2 iso3 name language)
  (setf (gethash iso2 *countries*)
        (make-country  :iso2 (string-upcase iso2)
                       :iso3 (string-upcase iso3)
                       :name name
                       :language language
                       :languages nil)))

(defun get-language (iso2)
  (gethash iso2 *languages*))

(defun get-country (iso2)
  (gethash iso2 *countries*))

(defun get-country-languages (iso2)
  (country-languages (get-country iso2)))

(let ((list))
  (defun get-countries ()
    (if list
        list
        (progn
          (maphash (lambda (cc country)
                     (declare (ignorable cc))
                     (push country list)) *countries*)
          (setf list (sort list #'string-lessp :key #'country-name))
          list))))

(defun make-locale (language country defaults-locale)
  (let ((key (concatenate 'string
                          (language-iso2 language) "-"
                          (country-iso2 country))))
    (push language (country-languages country))
    (setf (gethash key *locales*)
          (make-locale-% :key key
                         :language language :country country
                         :default (get-locale defaults-locale)))))

(defun get-locale (lang-CC)
  (gethash lang-CC *locales*))

(defun supported-locale-p (lang-CC)
  (get-locale lang-CC))

;;todo 3 optimize position-format


(defmacro with-locale (locale-string &body body)
  (let ((loc-gs (gensym)))
    `(let* ((,loc-gs ,locale-string)
            (i18n:*locale* (get-locale ,loc-gs)))
       (unless i18n:*locale*
         (error "invalid/unsupported locale ~A" ,loc-gs))
       ,@body)))

(defun set-text-function (key value)
  (check-type key symbol)
  (assert *locale*)
  (setf (gethash key (locale-dictionary *locale*)) value))

(defmacro set-text (key-symbol value)
  `(set-text-function ',key-symbol ,value))

(defun get-text (key)
  (get-text-function-impl key *locale*))

(defun get-text-function-impl (key locale)
  (unless locale
    (error "i18n key ~S translation not found for ~A" key *locale* ))
  (or (gethash key (locale-dictionary locale))
      (get-text-function-impl key
                              (locale-default locale))))


;;db storage
(defmethod db-base:get-struct-columns ((symbol (eql 'locale)))
  '((key string)))

(defun make-locale-from-db (&key key)
  (get-locale key))

(defmethod db-base:get-struct-columns ((symbol (eql 'country)))
  '((iso2 string)))

(defun make-country-from-db (&key iso2)
  (get-country iso2))


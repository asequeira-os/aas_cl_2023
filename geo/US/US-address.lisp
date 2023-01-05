(in-package :geo-US)

(aas-rpc:def-rpc-struct US-address
    (first nil :type string)
  (second nil :type (or null string))
  (city nil :type string)
  (state nil :type state-info)
  (zip-5 nil :type integer)
  (zip-4 nil :type (or null integer))
  (country nil :type i18n:country))

(defmethod db-base:get-struct-columns ((symbol (eql 'US-address) ))
  '((first string)
    (second (or null string))
    (city string)
    (state state-info)
    (zip-5 string)
    (zip-4 (or null string))
    (country i18n:country)))

(defun make-us-address-from-db
    (&key first second city  state  zip-5 zip-4  country )
  (make-us-address :first first :second second :city city
                   :state state :zip-5 zip-5 :zip-4 zip-4
                   :country country))

(defun create-US-address (first second city state-code zip)
  (let* ((state-code (string-upcase state-code))
         (state-info (gethash state-code *states-info*)))
    (when (null state-info)
      (error "invalid state ~A" state-code))
    (multiple-value-bind (zip-5 zip-4)
        (break-zip zip)
      (let ((city (aas-cl:clean-up-spaces city)))
        ;;(zip-cities (aref *zip-cities* (zipcode-main zipcode)))
        ;;(city-uc (string-upcase city)))
        (make-us-address :first first :second second :city city
                         :state state-info :zip-5 zip-5 :zip-4 zip-4
                         :country i18n:*usa*)))))

(defun break-zip (zip)
  (if (or (null zip) (zerop (length zip)))
      nil
      (let* ((list (split-sequence:split-sequence #\- zip))
             (length (length list))
             (first (first list))
             (second (if (> length 1)
                         (second list)
                         "")))
        (case length
          (1 (values (zip-sub-parse first ) nil))
          (2 (values (zip-sub-parse first)
                     (zip-sub-parse second )))
          (t (error "could not parse zip string ~S" zip))))))

(defun zip-sub-parse (s)
  (if (or (null s) (zerop (length  s)))
      0
      (parse-integer s)))



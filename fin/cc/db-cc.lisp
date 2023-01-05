(in-package :fin-cc)

;;for now this is separate from the user profile
;;it is intentionally duplicated since for now
;;even from business point of view, i see no connection
(def-rpc-struct cc-profile
    (dbid nil :type (or null db-id)) ;;synthetic
  (first-name nil :type string)
  (last-name nil :type string)
  (company nil :type (or null string))
  (address nil :type string)
  (city  nil :type string)
  (state  nil :type string)
  (zip  nil :type string)
  (country  nil :type string)
  (phone  nil :type string)
  (cc-vendor nil :type cc-vendor)
  (cc-num nil :type string)
  (cc-num-enc nil :skip-rpc  :type (or null string))
  (cc-exp-mm nil :type integer)
  (cc-exp-year nil :type integer)
  )

(defmethod db-base:get-struct-columns ((symbol (eql 'cc-profile )))
  '((dbid cloud:db-id)
    (first-name string)
    (last-name string)
    (company (or null string))
    (address string)
    (city string)
    (state string)
    (zip string)
    (country string)
    (phone string)
    (cc-vendor cc-vendor)
    (cc-num-enc string)
    (cc-exp-mm integer)
    (cc-exp-year integer)))

(defmethod aas-rpc:deserialize-after ((cc-profile cc-profile))
  (setf (cc-profile-cc-num-enc cc-profile)
        (encrypt-cc-num (cc-profile-cc-num cc-profile)))
  cc-profile)

;;PCI DSS
(defparameter *cc-num-encrypt-key* "gtkj^&defrqq#Mkqwezzp")

(defun encrypt-cc-num (cc-num)
  (cl-base64:usb8-array-to-base64-string (cipher:encrypt cc-num *cc-num-encrypt-key*)))

(defun decrypt-cc-num (cc-num)
  (cipher:decrypt (cl-base64:base64-string-to-usb8-array cc-num) *cc-num-encrypt-key*))
;;end PCI DSS

(defun make-cc-profile-from-db
    (&key dbid first-name last-name company address city  state
     zip country phone cc-vendor cc-num-enc cc-exp-mm cc-exp-year)
  (make-cc-profile :dbid dbid :first-name first-name :last-name last-name
                   :company company :address address :city city
                   :state state :zip zip :country country :phone phone
                   :cc-vendor cc-vendor
                   :cc-num-enc cc-num-enc :cc-num (decrypt-cc-num cc-num-enc)
                   :cc-exp-mm cc-exp-mm :cc-exp-year cc-exp-year))

(defmethod db-base:get-struct-columns ((symbol (eql 'gw-tx-request)))
  '((ts aas-local-time:dto)
    (tx-type string)
    (gw-vendor cc-gw-vendor)
    (cc-profile-dbid cloud:db-id)
    ;;(ccv (or null string)) ;;ccv is not to be stored as per PCI DSS
    (amount (or null decimal))
    (prev-tx-id (or null string))))

(defmethod db-base:get-struct-columns ((symbol (eql 'cc-gw-vendor)))
  '((name string)))

(defun gw-tx-request-cc-profile-dbid (gw-tx-request)
  (cc-profile-dbid (gw-tx-request-cc-profile gw-tx-request)))

(defun make-gw-tx-request-from-db
    (&key ts tx-type gw-vendor cc-profile-dbid ccv amount prev-tx-id)
  (let ((cc-user (db-cc_user-select-pk
                  (cloud:db-id-db cc-profile-dbid) (cloud:db-id-id cc-profile-dbid))))
    (make-gw-tx-request
     :ts ts :tx-type tx-type :gw-vendor gw-vendor
     :cc-profile (db-table-cc-user-row-profile cc-user)
     :ccv ccv :amount amount :prev-tx-id prev-tx-id)))

(defmethod db-base:get-struct-columns ((symbol (eql 'gw-tx-response)))
  '((dbid cloud:db-id)
    (request gw-tx-request)
    (approved boolean)
    (code (or null string))
    (reason-code (or null string))
    (reason-text (or null string))
    (auth-code (or null string))
    (avs avs-status)
    (tx-id (or null string))
    (description (or null string))
    (amount (or null decimal))
    (ccv-resp (or null string))
    (ccv-fail boolean)))

(defun make-gw-tx-response-from-db
    (&key dbid request approved code reason-code reason-text auth-code
     avs tx-id description amount ccv-resp ccv-fail)
  (make-gw-tx-response
   :dbid dbid :request request :approved approved :code code
   :reason-code reason-code :reason-text reason-text :auth-code auth-code
   :avs avs :tx-id tx-id :description description :amount amount
   :ccv-resp ccv-resp :ccv-fail ccv-fail))

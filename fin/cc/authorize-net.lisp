(in-package :fin-cc)

(define-constant +authorize-net+ "authorize.net")

(defclass authorize-net (cc-gw-vendor)
  ((name :initform +authorize-net+)
   (login :initarg :login :type string)
   (tran-key :initarg :tran-key :type string)
   (host :initarg :host :initform "secure.authorize.net")
   (url-path :initarg :url-path :initform "/gateway/transact.dll")
   (test-mode :initarg :test-mode :initform t)))

(defmethod add-cc-gw-vendor ((vendor authorize-net))
  (if (and (not +production+)
           (equal "secure.authorize.net"
                  (slot-value vendor 'host)))
      (error "we are not in production")
      (call-next-method)))


;;one of these three should be in config
(defun enable-authorize-net-prod (login tran-key)
  (add-cc-gw-vendor (make-instance 'authorize-net
                                   :login login
                                   :tran-key tran-key
                                   :test-mode nil)))

(defun enable-authorize-net-dev (login tran-key)
  (add-cc-gw-vendor (make-instance 'authorize-net
                                   :login login
                                   :tran-key tran-key
                                   :test-mode nil
                                   :host "test.authorize.net")))

(defun enable-authorize-net-test (login tran-key)
  (add-cc-gw-vendor (make-instance 'authorize-net
                                   :login login
                                   :tran-key tran-key
                                   :test-mode t)))

;;reqeust looks like
;;https://test.authorize.net/gateway/transact.dll?x_login=6uULkjz866V&x_tran_key=3fRnBFuT43uz2574&X_type=AUTH_CAPTURE&x_amount=123.78&x_card_num=4007000000027&x_exp_date=10/2011&x_relay_response=FALSE&x_delim_data=TRUE&x_delim_char=,&x_encap_char="
;;response looks like
;;"1","1","1","This transaction has been approved.","OQ0DXB","Y","2162257990","","","123.78","CC","auth_capture","","","","","","","","","","","","","","","","","","","","","","","","","","333333DDDDDDDD","","2","","","","","","","","","","","XXXX0027","Visa","","","","","","","","","","","","","","","",""

(defmethod vendor-cc-tx ((vendor authorize-net) gw-tx-request)
  (let ((cc-profile (gw-tx-request-cc-profile gw-tx-request))
        (ccv (gw-tx-request-ccv gw-tx-request))
        (prev-tx-id (gw-tx-request-prev-tx-id gw-tx-request))
        (amount (gw-tx-request-amount gw-tx-request))
        (tx-type (gw-tx-request-tx-type gw-tx-request)))
    (multiple-value-bind   (url parameters)
        (make-authorize-net-url vendor tx-type cc-profile ccv amount prev-tx-id)
      (multiple-value-bind (resp-str http-code http-headers)
          (drakma:http-request url
                               :method :post
                               :parameters parameters)
        (declare (ignorable http-headers))
        (unless (= http-code 200)
          (error "http non OK status ~A" http-code))
        (let ((fields (break-response-fields resp-str)))
          (let ((gw-tx-response (make-gw-tx-response
                                 :dbid (cloud:make-db-id :db (auth:auth-to-ext-vdb))
                                 :request gw-tx-request))
                (authorize-net-resp (make-authorize-net-resp :full-text-1 resp-str)))
            (loop for i from 1 to (length fields) do
                 (parse-adnrf i (aref fields (1- i)) gw-tx-response authorize-net-resp))
            (values gw-tx-response authorize-net-resp)))))))

(defun make-authorize-net-url (vendor type cc-profile ccv amount prev-tx-id)
  (with-slots (host url-path) vendor
    (let ((url (concatenate 'string
                            "https://"
                            host
                            url-path))
          (parameters (authorize-net-parameters
                       vendor type cc-profile ccv amount prev-tx-id)))
      (values url parameters))))

(defparameter +authorize-net-tx-type+
  (list (cons CC-TX-AUTH+CHARGE "AUTH_CAPTURE")
        (cons CC-TX-AUTH "AUTH_ONLY")
        (cons CC-TX-VOID "VOID")))

(defun authorize-net-parameters (vendor type profile ccv amount prev-tx-id)
  (let ((list `( "x_login" ,(slot-value vendor 'login)
                           "x_tran_key" ,(slot-value vendor 'tran-key)
                           ,@(when (slot-value vendor 'test-mode)
                                   (list "x_test_request" "TRUE"))
                           "x_type" ,(cdr (assoc type +authorize-net-tx-type+ :test #'equal))
                           ,@(if (equal type CC-TX-AUTH)
                                 (list "x_card_code" ccv ;;ccv mandatory on auth
                                       "x_description" "todo 1 need auth tx description config")
                                 (when ccv ;;once verified ccv is optional
                                   (list "x_card_code" ccv)))
                           ,@(if (equal type CC-TX-VOID)
                                 (list "x_trans_id" prev-tx-id))
                           "x_amount" ,(decimal->string amount)
                           "x_card_num" ,(cc-profile-cc-num profile)
                           "x_exp_date" ,(format nil "~A/~A"
                                                 (cc-profile-cc-exp-mm profile)
                                                 (cc-profile-cc-exp-year profile))
                           "x_relay_response" "FALSE"
                           "x_delim_data" "TRUE"
                           "x_delim_char" ","
                           "x_encap_char" "\""
                           "x_first_name" ,(truncate-seq (cc-profile-first-name profile) 50)
                           "x_last_name" ,(truncate-seq (cc-profile-last-name profile) 50)
                           ;;(cons "x_company" customer's company name (not mine)
                           ;; address 60 chars. required for AVS
                           "x_address" ,(truncate-seq (cc-profile-address profile) 60)
                           "x_city" ,(truncate-seq (cc-profile-city profile) 40)
                           ;;2 char code or 40 char
                           "x_state" ,(truncate-seq (cc-profile-state profile) 40)
                           ;;20 char required for avs
                           "x_zip" ,(truncate-seq (cc-profile-zip profile) 20))))
    (list-to-drakma-params
     (flatten list t))))

(defun list-to-drakma-params (in)
  (let ((pairs-list (partition in 2)))
    (mapcar (lambda (pair)
              (cons (first pair) (second pair))) pairs-list)))

(let ((lookup
       '((#\A (street) (zip5))
         (#\B (no-info) ())
         (#\E (avs-error) ())
         (#\G (non-US-bank) ())
         (#\N () (street zip5))
         (#\P (not-applicable) ())
         (#\R (service-n/a) ())
         (#\S (not-applicable) ())
         (#\U () (no-info))
         (#\W (zip5 zip4) (street))
         (#\X (street zip5 zip4) ())
         (#\Y (street zip5) ())
         (#\Z (zip5) (street))))
      (vector (make-array 26)))
  (loop for i from 0 to 25 do
       (let ((v (assoc (code-char (+ (char-code #\A) i))
                       lookup)))
         (flet ((fn-args (meta)
                  (alexandria:flatten
                   (mapcar (lambda (symbol)
                             (list (keyword-from-symbol symbol) t))
                           meta))))
           (setf (aref vector i)
                 (if v (list (fn-args (first (rest v)))
                             (fn-args (second (rest v)))) nil)))))
  (defun authorize-net-avs-status (code)
    (let ((info (aref vector (- (char-code code) (char-code #\A)))))
      (unless info
        (error "unknown authorize.net avs response code ~A" code))
      (make-avs-status
       :on (apply #'make-avs-status-flags (first info))
       :off (apply #'make-avs-status-flags (second info))))))

(defstruct authorize-net-resp
  (code #\X :type standard-char)
  (reason-code 0 :type integer)
  (auth-code nil :type (or null string))
  (ccv-resp nil :type (or null standard-char))
  (gw-cust-id nil :type (or null string))
  (full-text-1 nil :type string)
  )

(defmethod db-base:get-struct-columns ((symbol (eql 'authorize-net-resp )))
  '(( code standard-char)
    (reason-code integer)
    (auth-code (or null string))
    (ccv-resp (or null standard-char))
    (gw-cust-id (or null string))
    (full-text-1 string)))

(defun make-authorize-net-resp-from-db
    (&key code reason-code auth-code ccv-resp gw-cust-id full-text-1)
  (make-authorize-net-resp :code code :reason-code reason-code
                           :auth-code auth-code :ccv-resp ccv-resp
                           :gw-cust-id gw-cust-id :full-text-1 full-text-1)
  )

(defvar *resp-func-vector* (make-array 60 :initial-element nil))

;;adnrf => authorize.net response field
(let ((defn (vector
             CC-TX-RESP-APPROVED
             CC-TX-RESP-DECLINED
             CC-TX-RESP-ERROR
             CC-TX-RESP-OTHER)))
  (defun adnrf-1-resp-code (s gw-tx-response authorize-net-resp)
    (assert (= 1 (length s)))
    (let* ((adt-code (aref s 0))
           (code (aref defn (- (char-code adt-code) (char-code #\1)))))
      (setf (gw-tx-response-approved gw-tx-response)
            (eq code CC-TX-RESP-APPROVED))
      (setf (gw-tx-response-code gw-tx-response) code)
      (setf (authorize-net-resp-code authorize-net-resp) adt-code))))

(let ((defn (make-array 1024 :initial-element nil))
      (meta-list
       (list
        (list 1 CC-TX-RESP-APPROVED "transaction approved" "")
        (list 2 CC-TX-RESP-DECLINED "transaction declined" "")
        (list 3 CC-TX-RESP-DECLINED "transaction declined" "")
        (list 4 CC-TX-RESP-DECLINED "transaction declined"
              "card needs to be picked up")
        (list 5  CC-TX-RESP-OTHER-INVALID-AMOUNT "valid amount required"
              "value submitted in the amount field did  not pass for a number")
        (list 6 CC-TX-RESP-OTHER-INVALID-CC-NUM "credit card number is invalid" "")
        (list 7 CC-TX-RESP-OTHER-INVALID-EXP-DATE "expiration date is invalid"
              "format of the date submitted was incorrect")
        (list 8 CC-TX-RESP-OTHER-EXPIRED-CC "credit card has expired" "")
        (list 11 CC-TX-RESP-OTHER-DUPLICATE-TX
              "duplicate transaction has ben submitted" "")
        (list 12 CC-TX-RESP-OTHER-MISSING-AUTH-CODE "authorization code is required"
              "x_auth_code should have been present")
        (list 13 nil "merchant API Login ID is invalid or account is inactive" "")
        (list 15 nil "transaction ID is invalid"
              "VOID AUTH_CAPTURE and CREDIT need transaction ID")
        (list 16 nil "transaction was not found" "")
        (list 17 nil "merchant does not accept this type of credit card" "")
        (list 19 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" "")
        (list 20 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" "")
        (list 21 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" "")
        (list 22 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" "")
        (list 23 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" "")
        (list 25 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" "")
        (list 26 CC-TX-RESP-OTHER-TRY-AGAIN "try again in 5 minutes" ""))))
  (dolist (meta meta-list)
    (assert (= 4 (length meta)))
    (setf (aref defn (1- (first meta))) (rest meta)))
  (defun adnrf-3-resp-reason-code (adt-code-string gw-tx-response authorize-net-resp)
    (let* ((adt-code (parse-integer adt-code-string))
           (meta (aref defn  (1- adt-code))))
      (when meta
        (setf (gw-tx-response-reason-code gw-tx-response) (first meta))
        (setf (authorize-net-resp-reason-code authorize-net-resp) adt-code)))))

(defun adnrf-4-resp-reason-text (s gw-tx-response authorize-net-resp)
  (declare (ignorable authorize-net-resp))
  (setf (gw-tx-response-reason-text gw-tx-response) s))

(defun adnrf-5-resp-auth-code (s gw-tx-response authorize-net-resp)
  (declare (ignorable authorize-net-resp))
  (setf (gw-tx-response-auth-code gw-tx-response) s))

(defun adnrf-6-resp-avs (s gw-tx-response authorize-net-resp)
  (declare (ignorable authorize-net-resp))
  (setf (gw-tx-response-avs gw-tx-response)
        (authorize-net-avs-status (aref s 0))))

(defun adnrf-7-resp-tx-id (s gw-tx-response authorize-net-resp)
  (declare (ignorable authorize-net-resp))
  (setf (gw-tx-response-tx-id gw-tx-response) s))

(defun adnrf-9-resp-descr (s gw-tx-response authorize-net-resp)
  (declare (ignorable authorize-net-resp))
  (setf (gw-tx-response-description gw-tx-response) s))

(defun adnrf-10-resp-amount (s gw-tx-response authorize-net-resp)
  (declare (ignorable authorize-net-resp))
  (setf (gw-tx-response-amount gw-tx-response) (parse-decimal s)))

(defun adnrf-13-resp-customer-id (s gw-tx-response authorize-net-resp)
  (declare (ignorable gw-tx-response))
  (setf (authorize-net-resp-gw-cust-id authorize-net-resp) s))

(let ((out (vector
            CC-TX-RESP-CCV-MATCH
            CC-TX-RESP-CCV-NO-MATCH
            CC-TX-RESP-CCV-IGNORE
            CC-TX-RESP-CCV-MISSING
            CC-TX-RESP-CCV-N/A))
      (in "MNPSU")
      (fail-codes "NS"))
  (defun adnrf-39-resp-ccv (s gw-tx-response authorize-net-resp)
    (when (and s (not (zerop (length s))))
      (assert (= 1 (length s)))
      (let* ((adt-code (aref s 0))
             (code (aref out (position adt-code in)))
             (fail (position adt-code fail-codes)))
        (setf (authorize-net-resp-ccv-resp authorize-net-resp) adt-code)
        (setf (gw-tx-response-ccv-resp gw-tx-response) code)
        (setf (gw-tx-response-ccv-fail gw-tx-response) fail)))))

(let ((defn (make-array 1024 :initial-element nil))
      (meta-list
       '((1 adnrf-1-resp-code)
         (3 adnrf-3-resp-reason-code)
         (4 adnrf-4-resp-reason-text)
         (5 adnrf-5-resp-auth-code)
         (6 adnrf-6-resp-avs)
         (7 adnrf-7-resp-tx-id)
         (9 adnrf-9-resp-descr)
         (10 adnrf-10-resp-amount)
         (13 adnrf-13-resp-customer-id)
         (39 adnrf-39-resp-ccv))))
  (dolist (meta meta-list)
    (assert (= 2 (length meta)))
    (setf (aref defn (1- (first meta))) (symbol-function (second meta))))
  (defun parse-adnrf (field-num field-text gw-tx-response authorize-net-resp)
    (let ((handler (aref defn (1- field-num))))
      (when handler
        (funcall handler field-text gw-tx-response authorize-net-resp)))))

(defun break-response-fields (resp)
  (let ((delim #\,)
        (quote #\")
        (list (list)))
    (with-input-from-string (stream resp)
      (flet ((parse-field ()
               (unless (eql quote (peek-char t stream))
                 (error "expected ~A at ~A in authorize.net resp ~A"
                        quote (file-position stream) resp))
               (read-char stream);;skip opening quote
               (with-output-to-string (out)
                 (util:while (not (eql quote (peek-char nil stream)))
                   (write-char (read-char stream) out))
                 ;;skip closing quote
                 (read-char stream))))
        (util:while (peek-char t stream nil nil)
          (push (parse-field) list)
          (when (eql delim (peek-char t stream nil nil))
            ;;skip comma
            (read-char stream)))
        (coerce (nreverse list) 'vector)))))

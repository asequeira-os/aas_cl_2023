(in-package :fin-cc)

(define-constant CC-TX-AUTH+CHARGE "CC-TX-AUTH+CHARGE")
(define-constant CC-TX-AUTH "CC-TX-AUTH")
(define-constant CC-TX-CHARGE "CC-TX-CHARGE")
(define-constant CC-TX-VOID "CC-TX-VOID")

(define-constant CC-TX-RESP-APPROVED "CC-TX-RESP-APPROVED")
(define-constant CC-TX-RESP-DECLINED "CC-TX-RESP-DECLINED")
(define-constant CC-TX-RESP-ERROR "CC-TX-RESP-ERROR")
(define-constant CC-TX-RESP-OTHER "CC-TX-RESP-OTHER")

(define-constant CC-TX-RESP-OTHER-INVALID-AMOUNT "CC-TX-RESP-OTHER-INVALID-AMOUNT")
(define-constant CC-TX-RESP-OTHER-INVALID-CC-NUM "CC-TX-RESP-OTHER-INVALID-CC-NUM")
(define-constant CC-TX-RESP-OTHER-INVALID-EXP-DATE "CC-TX-RESP-OTHER-INVALID-EXP-DATE")
(define-constant CC-TX-RESP-OTHER-EXPIRED-CC "CC-TX-RESP-OTHER-EXPIRED-CC")
(define-constant CC-TX-RESP-OTHER-DUPLICATE-TX "CC-TX-RESP-OTHER-DUPLICATE-TX")
(define-constant CC-TX-RESP-OTHER-MISSING-AUTH-CODE "CC-TX-RESP-OTHER-MISSING-AUTH-CODE")
(define-constant CC-TX-RESP-OTHER-TRY-AGAIN "CC-TX-RESP-OTHER-TRY-AGAIN")

(define-constant CC-TX-RESP-CCV-MATCH "CC-TX-RESP-CCV-MATCH")
(define-constant CC-TX-RESP-CCV-NO-MATCH "CC-TX-RESP-CCV-NO-MATCH")
(define-constant CC-TX-RESP-CCV-IGNORE "CC-TX-RESP-CCV-IGNORE")
(define-constant CC-TX-RESP-CCV-MISSING "CC-TX-RESP-CCV-MISSING")
(define-constant CC-TX-RESP-CCV-N/A "CC-TX-RESP-CCV-N/A")


(defstruct avs-status-flags
  (street nil  :type boolean)
  (no-info nil :type boolean)
  (zip5 nil  :type boolean)
  (zip4 nil  :type boolean)
  (avs-error nil  :type boolean)
  (service-n/a nil  :type boolean)
  (non-US-bank nil  :type boolean)
  (not-applicable nil  :type boolean)
  )

(defmethod db-base:get-struct-columns ((symbol (eql 'avs-status-flags)))
  '((street boolean)
    (no-info boolean)
    (zip5 boolean)
    (zip4 boolean)
    (avs-error boolean)
    (service-n/a boolean)
    (non-US-bank boolean)
    (not-applicable boolean)))

(defun make-avs-status-flags-from-db
    (&key street no-info zip5 zip4 avs-error service-n/a
     non-US-bank not-applicable)
  (make-avs-status-flags
   :street street :no-info no-info :zip5 zip5 :zip4 zip4 :avs-error avs-error
   :service-n/a service-n/a :non-US-bank non-US-bank
   :not-applicable not-applicable))

(defstruct avs-status ()
           (on nil :type avs-status-flags)
           (off nil :type avs-status-flags)
           )

(defmethod db-base:get-struct-columns ((symbol (eql 'avs-status)))
  '((on avs-status-flags)
    (off avs-status-flags)))

(defun make-avs-status-from-db (&key on off)
  (make-avs-status :on on :off off))


(defstruct gw-tx-request
  (ts nil :type aas-local-time:dto)
  (tx-type nil :type string)
  (gw-vendor nil :type cc-gw-vendor)
  (cc-profile nil :type cc-profile) ;;only dbid is stored
  (ccv nil :type (or null string))
  (amount nil :type (or null decimal))
  (prev-tx-id nil :type (or null string)))

(defstruct gw-tx-response
  (dbid nil :type cloud:db-id)
  (request nil :type gw-tx-request)
  (approved nil :type boolean)
  (code nil :type (or null string))
  (reason-code nil :type (or null string))
  (reason-text nil :type (or null string))
  (auth-code nil :type (or null string))
  (avs nil :type (or null avs-status))
  (tx-id nil :type (or null string))
  (description nil :type (or null string))
  (amount nil :type (or null decimal))
  (ccv-resp nil :type (or null string))
  (ccv-fail nil :type boolean))

(defgeneric vendor-cc-tx (vendor gw-tx-request))
(defgeneric vendor-cc-tx-store (vendor vendor-resp master-row))

(defun authorize-cc (cc-profile ccv amount)
  (let* ((vendor (get-cc-gw-vendor-random))
         (request (make-gw-tx-request
                   :ts (aas-local-time:utc-now) :tx-type CC-TX-AUTH
                   :gw-vendor vendor :cc-profile cc-profile :ccv ccv
                   :amount amount)))
    (multiple-value-bind (gw-tx-resp gw-vendor-resp)
        (vendor-cc-tx vendor request)
      (declare (ignorable gw-vendor-resp))
      gw-tx-resp)))

(defun  void-cc (gw-tx-response)
  (let* ((prev-tx-id (gw-tx-response-tx-id gw-tx-response))
         (prev-tx-request (gw-tx-response-request gw-tx-response))
         (vendor (gw-tx-request-gw-vendor prev-tx-request))
         (cc-profile (gw-tx-request-cc-profile prev-tx-request)))
    (let ((request (make-gw-tx-request
                    :ts (aas-local-time:utc-now) :tx-type CC-TX-VOID
                    :gw-vendor vendor :cc-profile cc-profile
                    :amount (gw-tx-request-amount prev-tx-request)
                    :prev-tx-id prev-tx-id)))
      (multiple-value-bind (gw-tx-resp gw-vendor-resp)
          (vendor-cc-tx vendor request)
        (declare (ignorable gw-vendor-resp))
        gw-tx-resp))))


(defmethod vendor-cc-tx :around (vendor gw-tx-request)
  (declare (ignorable gw-tx-request))
  (validate-tx-input gw-tx-request)
  (multiple-value-bind (gw-tx-response vendor-resp)
      (call-next-method)
    (multiple-value-bind (auth-db-num userid)
        (auth:auth-token-user)
      (let ((row (make-db-table-gw-tx-row
                  auth-db-num userid gw-tx-response nil)))
        (db-with-transaction ('vendor-cc-tx)
          (let* ((db-row (db-insert-row row))
                 (vendor-row (vendor-cc-tx-store vendor vendor-resp db-row)))
            (assert vendor-row)
            gw-tx-response))))))

(defun validate-tx-input (gw-tx-request)
  "generic validation irrespective of gw vendor"
  (let ((tx-type (gw-tx-request-tx-type gw-tx-request))
        (ccv (gw-tx-request-ccv gw-tx-request)))
    (if (equal tx-type CC-TX-AUTH)
        (assert (and ccv (not (zerop (length ccv))))))))
(in-package :auth)

(defvar +auth+ (cloud:define-app auth 58))

(extern:define-enum user-realm password no-password)

;;any user other than the authenticated user that might need passing around
(def-rpc-struct user
    (orig-login nil :type (or null string)) ;;not always filled even if changed
  (userid nil :type user-id)
  (ext-key -1 :type integer)
  (login nil :type string))

(def-rpc-struct user-id ;;just for type safety
    (id nil :type db-id))

(defgeneric user-id (obj))

(defmethod user-id ((obj user))
  (user-userid obj))

(defgeneric user-db-num (obj))
(defmethod user-db-num ((obj user-id))
  (db-id-db (user-id-id obj)))
(defmethod user-db-num ((obj user))
  (user-db-num (user-userid obj)))

(defgeneric user-id-num (obj))
(defmethod user-id-num ((obj user-id))
  (db-id-id (user-id-id obj)))
(defmethod user-id-num ((obj user))
  (user-id-num (user-userid obj)))

(defmethod db-base:get-struct-columns ((symbol (eql 'user-id )))
  '((id db-id)))

(defun make-user-id-from-db (&key id)
  (make-user-id :id id))


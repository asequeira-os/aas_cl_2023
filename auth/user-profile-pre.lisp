(in-package :auth)

(def-rpc-struct user-profile
    (first-name nil :type string)
  (second-name nil :type (or null string))
  (last-name nil :type string))

(defmethod db-base:get-struct-columns ((symbol (eql 'user-profile )))
  '((first-name string)
    (second-name (or null string))
    (last-name string)))

(defun make-user-profile-from-db
    (&key first-name  second-name  last-name)
  (make-user-profile :first-name first-name
                     :second-name second-name :last-name last-name))


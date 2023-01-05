(in-package :auth)

(define-constant +admin-right+ "admin")

(cloud:define-error 40108 acl-perm-denied)

(defgeneric right-object-type (obj))

(defmethod right-object-type (obj)
  (let ((*package* *cl-package*))
    (format nil "~S" (type-of obj))))

(defgeneric obj-db-id (obj))

(def-rpc-struct group
    (id nil :type db-id)
  (name nil :type (or null string)))

(defmethod obj-db-id ((obj group))
  (group-id obj))

(defun dbidtype-from-userid (user-id)
  (make-db-id-type :type (right-object-type user-id) :dbid (user-id-id user-id)))

(defmethod obj-db-id ((obj db-id-type))
  (db-id-type-dbid obj))

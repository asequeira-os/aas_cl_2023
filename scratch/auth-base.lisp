
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package "LISA-MAB"))
    (defpackage "AUTH-BASE"
      (:use "LISA-LISP"))))

(in-package "AUTH-BASE")

(make-inference-engine)

(deftemplate group ()
  (slot name))

(deftemplate group-member ()
  (slot group-name)
  ;member can be another group or a role
  (slot member-name))

(deftemplate role ()
  (slot name))

(deftemplate permission ()
  (slot name))

(deftemplate group-permission () 
  (slot group-name)
  (slot permission-name))

(deftemplate role-permission ()
  (slot role-name)
  (slot permission-name))

(deffacts authdb ()
  (role (name meeting-creator))
  (role (name meeting-attendee))
  (role (name admin-asst))
  (role (name employee))

  (group (name calendar-admins))
  (group-member (group-name calendar-admins)
                (member-name admin-asst))
  (group-member (group-name calendar-admins)
                (member-name meeting-creator))

  (group (name employees))
  (group-member (group-name employees)
                (member-name employee))


  (permission (name create))
  (permission (name modify))
  (permission (name delete))
  (permission (name invite))
  (permission (name accept-invite))

  (group-permission (group-name calendar-admins)
                    (permission-name create))


 )

(defvar *allowed-perms* nil)

(deftemplate find-group-members ()
  (slot group-name))

(defrule impl-find-group-members ()
  (find-group-members (group-name ?group-name))
  (group-member (group-name ?group-name)
                (member-name ?member-name))
  =>
  (format t "got perm ~A.~%" ?member-name))


(deftemplate find-permissions ()
  (slot role-name))

(defrule list-role-allowed-perm ()
  (find-permissions (role-name ?role-name))
  (role (name ?role-name))
  (group-member (group-name ?group-name)
                (member-name ?role-name))
  (group-permission (group-name ?group-name)
                    (permission-name ?permission-name))
  (permission (name ?permission-name))
  =>
  (format t "got perm ~A.~%" ?permission-name))
  ;(append *allowed-perms* ?permission-name))

;;computed members of a group excluding direct members
(deftemplate indirect-group-member ()
  (slot group-name)
  (slot group-member))


#|
or not being supported 

(defrule make-indirect-member ()
  (group-member (group-name ?super-group)
                (group-member ?sub-group))
  (or (group-member (group-name ?sub-group)
                    (group-member ?member))
      (indirect-group-member (group-name ?sub-group)
                             (group-member ?member)))
  (not (group-member (group-name ?super-group)
                     (group-member ?member)))
  (not (indirect-group-member (group-name ?super-group)
                              (group-member ?member)))
  =>
  (assert (indirect-group-member (group-name ?super-group)
                                 (group-member ?member))))



|#


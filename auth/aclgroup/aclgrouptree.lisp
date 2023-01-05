(in-package :auth)

;; algo to find user's effective groups membership
;; G <- list of groups user belongs to
;; Q <- a queue of groups
;; H <- hash of processed groups
;; for each g in G
;;  add g to Q
;; loop:
;; for each g in Q
;;   if empty goto done
;;   dequeue g
;;   skip g if g is in H
;;   add g to H
;;   find parents of g
;;    add parents to Q
;;   goto loop
;; done:
;;   return  entries in H

(def-rpc-with-proxy (vector group) acl-subj-effective-membership
    (:application +auth+)
    (subj db-id-type)
    (key-to-vdb (db-id-db (db-id-type-dbid subj)))
  (let ((g (group-list-for-subj :subj subj))
        (q (aas-misc:make-queue))
        (h (make-hash-table :test #'equalp))
        )
    (loop for g across g do
         (aas-misc:enqueue q g))
    (util:while (not (aas-misc:empty-queue-p q))
      (let ((g (aas-misc:dequeue q)))
        (unless (gethash (group-id g) h)
          (setf (gethash (group-id g) h) t)
          (let ((parents (find-group-parents
                          :group (make-group :id (group-id g)))))
            (loop for g across parents do
                 (aas-misc:enqueue q g))))))
    (let ((list))
      (maphash (lambda (gid ign)
                 (declare (ignorable ign))
                 (push (make-group :id gid ) list))
               h)
      (coerce list 'vector))))

(def-rpc (vector group) acl-user-effective-membership
    (:application +auth+)
    ()
  (acl-subj-effective-membership :subj (dbidtype-from-userid (auth-token-user-id))))

(def-rpc-with-proxy (vector group) acl-user-direct-groups
    (:application +auth+)
    ()
    (auth-token-db-num)
  (let ((userid (auth-token-user-id)))
    ;;todo 2 i need self-or-admin check here
    (verify-self userid)
    (group-list-for-subj
     :subj (dbidtype-from-userid userid))))

(def-rpc-with-proxy (vector group) find-group-parents
    (:host-to-host-only t :application +auth+)
    (group group)
    (db-id-db (group-id group))
  (let ((rows (db-aclgroupchild-select-bychild
               *vdb* *vdb* (db-id-id (group-id group))))
        (list))
    (dolist (row rows)
      (push (make-group :id (db-table-aclgroupchild-row-pgroupid row)) list))
    (coerce (nreverse list) 'vector)))


(in-package :auth)

;;as yet i don't know if auth is the right place for this
;;keep as independent of auth  as possible

;;when both the vdbs are same, so the retrieval does not get duplicates
;;todo 2 groups deletion needs to remove member records
;;todo 2 groups deletion needs to remove rights records

;;one record lives in the group's db
(def-db-table aclgroup
    "groups master"
  +auth+
  ((groupid (or null integer))
   (db-num integer)  ;;just for safety
   (name string)
   (parents integer) ;;count for query optimization
   (creator user-id)
   (deleted boolean))
  (db-num groupid)
  ()
  (:vdb-sequence db-num groupid))

;;one record per group per member lives in group's db
;;one record per group per member lives in member modulo db
(def-db-table aclgroupmember
    "users in groups"
  +auth+
  ((groupid db-id)
   (subj db-id-type)
   (db-num integer)
   (memberdb boolean))
  (((groupid db) :ascending) ((groupid id) :ascending)
   ((subj type) :ascending) ((subj dbid db) :ascending) ((subj dbid id) :ascending)
   db-num memberdb)
  ((:not-unique bygroup-subjtype db-num ((groupid db) :ascending) ((groupid id) :ascending) ((subj type) :ascending) memberdb )
   (:not-unique bygroup db-num ((groupid db) :ascending) ((groupid id) :ascending) memberdb)
   (:not-unique bysubj db-num ((subj type)) ((subj dbid db)) ((subj dbid id) ) memberdb))
  ())

;;one record per parent group per child lives in parent group's db
;;one record lives in child group's db
(def-db-table aclgroupchild
    "groups that are members of this group"
  +auth+
  ((pgroupid db-id)
   (cgroupid db-id)
   (db-num integer)
   (cdb boolean))
  (((pgroupid db) :ascending) ((pgroupid id) :ascending)
   ((cgroupid db) :ascending) ((cgroupid id) :ascending)
   db-num cdb)
  ((:not-unique byparent db-num ((pgroupid db) :ascending) ((pgroupid id) :ascending) )
   (:not-unique bychild db-num ((cgroupid db) :ascending) ((cgroupid id) :ascending)))
  ())

;;one record in group db
;;one record in object db (mapped modulo to the auth app db num)
;;todo 2 fix me id and db0num need to be combined inot db-id
;;and add db-num to index
(def-db-table group-rights
    "rights of a group to an object"
  +auth+
  ((id (or null integer))
   (objtype string)
   (objid db-id)
   (groupid db-id)
   (priv string)
   (db-num integer))
  (db-num id)
  ((:not-unique bygroupobj
                ((groupid db) :ascending) ((groupid id) :ascending)
                ((objid db) :ascending) ((objid id) :ascending)))
  (:vdb-sequence db-num id))

;;one record in user db
;;one record in object db (mapped modulo to the auth app db num)
;;todo 2 fix me id and db0num need to be combined inot db-id
;;and add db-num to index
(def-db-table user-rights
    "rights of a user  to an object"
  +auth+
  ((id (or null integer))
   (objtype string)
   (objid db-id)
   (user user-id)
   (priv string)
   (db-num integer))
  (db-num id)
  ((:not-unique byuserobj
                ((user id db) :ascending)  ((user id id) :ascending)
                ((objid db) :ascending) ((objid id) :ascending)))
  (:vdb-sequence db-num id))



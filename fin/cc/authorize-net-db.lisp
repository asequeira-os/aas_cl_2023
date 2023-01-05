(in-package :fin-cc)

(def-db-table adn-tx
    "authorize.net specific tx data"
  +fin-cc+
  ( (master-id cloud:db-id)
    (r authorize-net-resp))
  ((( master-id db)) (( master-id id)))
  nil
  nil)

(defmethod vendor-cc-tx-store
    ((vendor authorize-net) vendor-response master-row)
  (let ((master-id (gw-tx-response-dbid (db-table-gw-tx-row-r master-row))))
    (let ((adn-row (make-db-table-adn-tx-row master-id vendor-response)))
      (db-insert-row adn-row))))

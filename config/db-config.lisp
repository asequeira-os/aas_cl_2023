;;db number to hostname configuration
;;
;;this file needs to be identical across all hosts
;;
(in-package :cloud)

;;for development or testing - choose what config parts to loads

(define-constant +dev-host-1+
    (if aseq-test:*multi-host*
        (error "todo 4 need multi host test fix hardcoded name")
        (host:my-hostname)))
(define-constant +dev-host-2+ "xed02.local.mmm")

(set-db-name-prefix "xed" xed:+xeduler+)
;; DO NOT CHANGE DB COUNT ONCE IN PRODUCTION
(set-application-db-count xed:+xeduler+ 6)
;;next line is dev config with a single host for all dbs
(set-pdb-host-range 0 5 +dev-host-1+ xed:+xeduler+)
;;next line in dev to opt for simple even vdb distribution
(distribute-vdb-over-pdb xed:+xeduler+)

;;dev setup for auth dbs
(set-db-name-prefix "auth" auth:+auth+)
(set-application-db-count auth:+auth+ 2)

(if aseq-test:*multi-host*
    (progn
      (set-pdb-host-range 0 0 +dev-host-1+ auth:+auth+)
      (set-pdb-host-range 1 1 +dev-host-2+ auth:+auth+))
    (set-pdb-host-range 0 1 +dev-host-1+ auth:+auth+))

(distribute-vdb-over-pdb auth:+auth+)

;;dev setup for fin-cc dbs
(set-db-name-prefix "cc" fin-cc:+fin-cc+)
(set-application-db-count fin-cc:+fin-cc+ 2)
(set-pdb-host-range 0 1 +dev-host-1+ fin-cc:+fin-cc+)
(distribute-vdb-over-pdb  fin-cc:+fin-cc+)

;;todo 1 need to test with multiple hosts (at least 3)

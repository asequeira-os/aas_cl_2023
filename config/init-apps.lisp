(in-package :cloud)

;;only inited apps are alive on this host
(init auth:+auth+)
(init xed:+xeduler+)
(init fin-cc:+fin-cc+)

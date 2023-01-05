;;logging configuration
(in-package :log)

(setf *level* 10)
(setf *log-dir* "~/logs/")

;;force unbuffered writes
(setf *buffer* nil)

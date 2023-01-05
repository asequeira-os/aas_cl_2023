(in-package :sampling)

(defgeneric process (algo datum))
(defgeneric get-sample (algo))

;;(defgeneric simple-stats (algo))

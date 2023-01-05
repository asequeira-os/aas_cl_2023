(in-package :fin-cc)

(defmethod cloud:init :before ((app (eql +fin-cc+)))
  (aas-build::load-config "fin-cc-config"))



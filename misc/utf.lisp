(in-package :aas-misc)

(export '(string-to-utf8 utf8-to-string))

(defun string-to-utf8 (&rest strings)
  (apply #'babel:concatenate-strings-to-octets :utf-8  strings))

(defun utf8-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

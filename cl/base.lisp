(in-package :aas-cl)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun get-hash-keys (hash-table)
  (loop for k being the hash-key of hash-table collect k))

(define-constant *cl-package* (find-package :cl))

(in-package :cloud)

(defgeneric rights-or (r1 &rest others))
(defgeneric rights-and (r1 &rest others))
(defgeneric rights-to-integer (r1))

;;[de]serialization notes
;;if needed look into locale [de]serialization code in i18n-core.lisp
;;this is wonderful stuff that i could not use
;;leaving it here for posterity
(defmacro define-rights (species &rest rights)
  (if (< 31 (length rights))
      (error "can't compile more than 31 rights"))
  (let ((dbv-symb (build-symbol-in-package (symbol-package species) 'dbv))
        (val-symb (build-symbol-in-package (symbol-package species) 'val))
        (dbv-acc (build-symbol-in-package
                  (symbol-package species) species "-" 'dbv))
        (acc-code nil)
        (bitnum 0)
        (make-symb (build-symbol-in-package
                    (symbol-package species) "make-" species))
        (ctorcode nil)
        (db-reader (build-symbol-in-package
                    (symbol-package species) "make-" species "-from-db"))
        (ctor-symb (build-symbol-in-package
                    (symbol-package species) "create-" species)))
    (dolist (right rights)
      (let ((acc-symb (build-symbol-in-package
                       (symbol-package species) species "-" right)))
        (push `(setf (,acc-symb ,species) (if ,right t nil))
              ctorcode)
        (push `(defun ,acc-symb (,species)
                 (logbitp ,bitnum (,dbv-acc ,species)))
              acc-code)
        (push
         `(defun (setf ,acc-symb) (,val-symb ,species )
            (setf (,dbv-acc ,species)
                  (dpb (if ,val-symb 1 0) (byte 1 ,bitnum) (,dbv-acc ,species))))
         acc-code)
        (incf bitnum)))
    `(progn
       (defstruct ,species
         (,dbv-symb 0))
       (defmethod cloud:rights-or ((,species ,species) &rest others)
         (let ((dbv (,dbv-acc ,species)))
           (dolist (r others)
             (setf dbv (logior dbv (,dbv-acc r))))
           (,make-symb :dbv dbv)))
       (defmethod cloud:rights-and ((,species ,species) &rest others)
         (let ((dbv (,dbv-acc ,species)))
           (dolist (r others)
             (setf dbv (logand dbv (,dbv-acc r))))
           (,make-symb :dbv dbv)))
       (defmethod cloud::rights-to-integer ((rights ,species))
         (,dbv-acc rights))
       (defmethod db-base:get-struct-columns ((symbol (eql ',species)))
         '((,dbv-symb integer)))
       (defun ,db-reader (&key ,dbv-symb)
         (,make-symb ,(keyword-from-symbol dbv-symb) ,dbv-symb))
       ,@(nreverse acc-code)
       (defun ,ctor-symb (&key ,@rights)
         (let ((,species (,(build-symbol-in-package (symbol-package species)
                                                    "make-" species))))
           ,@(nreverse ctorcode)
           ,species)))))


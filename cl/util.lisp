(in-package :aas-cl)

(let ((rs (make-random-state t))
      (max (expt 2 128))
      (lock (mp-make-lock "guid-gen")))
  (defun guid ()
    "128 bit random integer"
    (mp-with-lock (lock)
      (random max rs))))

;; todo 3 guid-string may be left padding zeros
;; check if i need a uuid function
(defun guid-string ()
  (format nil "~A" (guid)))

;;from comp.lang.lisp
;;John Thingstad Sat, 12 Jul 2008 23:53:21 +0200
(defun partition (list parts)
  (assert (= (rem (length list) parts) 0))
  (loop repeat (/ (length list) parts) collect
       (loop repeat parts collect (first list) do (pop list))))

(defun truncate-seq (seq size)
  (if (or (null seq)
          (> size (length seq)))
      seq
      (subseq seq 0 size)))

(defun struct-symbol-p (s)
  (typep (find-class s nil) 'structure-class))

(defun class-symbol-p (s)
  (typep (find-class s nil) 'standard-class))

(defun flatten (tree &optional keep-nil)
  (labels ((flatten-with-nil (tree)
             (typecase tree
               (null (when keep-nil
                       (list nil)))
               (list (mapcan #'flatten-with-nil tree))
               (atom (list tree)))))
    (when tree
      (flatten-with-nil tree))))

;; todo 8 not sure why I wrote xor this way
(defun xor (a b)
  (let ((a a)
        (b b))
    (and (or a b) (not (and a b)))))

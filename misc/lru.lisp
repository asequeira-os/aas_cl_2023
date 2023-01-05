(in-package :aas-misc)

(export '(lru-make lru-add lru-get lru-remove))

(defstruct lru-struct
  (lock :readonly)
  (size :readonly)
  (count)
  (dll)
  (hash))

(defun lru-make (size)
  (make-lru-struct
   :lock (mp-make-lock "lru")
   :size size
   :count 0
   :dll (dll-make)
   :hash (make-hash-table :test #'equal)))

(defun lru-add (lru key data)
  (mp-with-lock ((lru-struct-lock lru))
    (lru-remove lru key)
    (when (= (lru-struct-size lru) (lru-struct-count lru))
      (remhash (dll-node-value (dll-remove-tail (lru-struct-dll lru)))
               (lru-struct-hash lru))
      (decf (lru-struct-count lru)))
    (setf (gethash key (lru-struct-hash lru))
          (cons (dll-add-head (lru-struct-dll lru) key) data))
    (incf (lru-struct-count lru))
    data))

(defun lru-get (lru key)
  (mp-with-lock ((lru-struct-lock lru))
    (multiple-value-bind (cons found)
        (gethash key (lru-struct-hash lru))
      (if found
          (let ((node (car cons))
                (dll (lru-struct-dll lru)))
            (dll-add-head-node dll (dll-remove dll node))
            (values (cdr cons) t))
          (values nil nil)))))


(defun lru-remove (lru key)
  (mp-with-lock ((lru-struct-lock lru))
    (multiple-value-bind (cons found)
        (gethash key (lru-struct-hash lru))
      (when found
        (dll-remove (lru-struct-dll lru) (car cons))
        (remhash key (lru-struct-hash lru))
        (decf (lru-struct-count lru))
        t))))



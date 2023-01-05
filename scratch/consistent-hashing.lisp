(in-package :scratch)


(export (list 'algorithm 'object-key 'add-node 'remove-node 'get-key-node 'get-key-node-list 'hash-string 'nearest-binary-search ))

;;200 chosen almost arbitrary except for someone
;;stating in blog that it was a good middle ground
;;
(defvar *buckets-per-key* 200)

;;create and returns the consistent-hashing implementation
;;id identifies the implementation choice
;; we provide a default for t
(defgeneric algorithm (id))

;;implement methods that know how to get key for the things
;;you wish to store. the key returned should be a string
(defgeneric object-key (obj))

;;add a real bucket
;;impl is the conistent-hash implementation
;;node is the bucket
(defgeneric add-node (impl node))

(defgeneric remove-node (impl node))

;;returns the node in which to store
(defgeneric get-key-node (impl obj))

;;returns a sequence of nodes, upto count
(defgeneric get-key-node-list (alog obj count))


;;for now this does a sha1 and makes an integer
;;out of the first 32 bits (4 bytes)
;;the choide of 32 bits is arbitrary
(defun hash-string (string)
  (util:network-bytes-to-number
   (ironclad:digest-sequence ;sha1 gets 20 bytes
    :sha1
    (ironclad:ascii-string-to-byte-array string))
   0 32))


(defun get-key-hashes (string)
  (sort
   (remove-duplicates
    (loop for i from 1 to *buckets-per-key*
       collect
         (cons (hash-string (format nil "~A~A" string i)) string))
    :key #'car)
   #'< :key #'car))


(defclass impl-consistent-hash ()
  ((nodehash :initform (make-hash-table :test #'equal))
   (combined-nodes)))

(defmethod algorithm ((id t))
  (make-instance 'impl-consistent-hash))

(defmethod add-node ((impl impl-consistent-hash) node)
  (setf (gethash node (slot-value impl 'nodehash))
        (get-key-hashes node))
  (combine-nodes impl))


(defmethod remove-node ((impl impl-consistent-hash) node)
  (remhash node (slot-value impl 'nodehash))
  (combine-nodes impl))


(defun combine-nodes (impl)
  (setf (slot-value impl 'combined-nodes) (list))
  (maphash
   #'(lambda (node node-points)
       (setf (slot-value impl 'combined-nodes)
             (merge 'list
                    (copy-seq (slot-value impl 'combined-nodes))
                    (copy-seq node-points) #'< :key #'car)))
   (slot-value impl 'nodehash)))

;;todo 9 need to change the linear lookup to binary search
(defmethod get-key-node ((impl impl-consistent-hash) key)
  (let ((key-hash (hash-string key)))
    (let ((closest-node
           (find key-hash (slot-value impl 'combined-nodes)
                 :test #'<= :key #'car)))
      (if closest-node
          closest-node
          (first (slot-value impl 'combined-nodes))))))



(defun nearest-binary-search (elt vector)
  (let ((length (length vector)))
    (cond
      ((= length 0) nil)
      ((= length 1) (elt vector 0))
      (t (nearest-binary-search-impl elt vector 0 (1- (length vector)))))))

(defun nearest-binary-search-impl (elt vector start end)
  (let* ((length (1+ (- end start)))
         (mid (+ start (truncate (float (/ length 2)))))
         (mid-elt (elt vector mid)))
    (if (<= length 2)
        (nearest-binary-search-edge-case elt vector start end)
        (if (= elt mid-elt)
            (elt vector mid)
            (if (< elt mid-elt)
                (nearest-binary-search-impl elt vector start mid)
                (nearest-binary-search-impl elt vector mid end))))))

(defun nearest-binary-search-edge-case (elt vector start end)
  (if (<= end start)
      (elt vector start)
      (let ((start-elm (elt vector start))
            (end-elm (elt vector end)))
            (if (< elt end-elm)
                start-elm
                end-elm))))


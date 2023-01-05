(in-package :cache)

(defgeneric cache-key (object)
  (:documentation "should provide a string key"))
(defgeneric cache-ttl (object)
  (:documentation "time to live in seconds"))
(defgeneric cache-get (cache key))
(defgeneric cache-put (cache object))
(defgeneric cache-remove (cache key))

(defmethod cache-ttl (object)
  "default ttl 10 minutes"
  (declare (ignorable object))
  (* 10 60))

(defclass aas-cache ()
  ((lock :initarg :lock)
   (size :initarg :size :type fixnum)
   (fill :initarg :fill :type fixnum)
   (filled :initarg :filled :type boolean )
   (pointer :initarg :pointer :type fixnum)
   (bits :initarg :bits)
   (ttl :initarg :ttl)
   (keys :initarg :keys)
   (data :initarg :data)
   (index :initarg :index)))

;;

(defun make-aas-cache (maxcount)
  (make-instance
   'aas-cache
   :lock (mp-make-lock "cache")
   :size maxcount
   :fill 0
   :filled nil
   :pointer 0
   :bits (make-array maxcount :element-type 'bit :initial-element 0)
   :ttl (make-array maxcount :element-type 'fixnum :initial-element 0)
   :keys (make-array maxcount :element-type 'string :initial-element "")
   :data (make-array maxcount)
   :index (make-hash-table :test #'equal)
   )
  )

(defmethod cache-put ((cache aas-cache) object)
  (mp-with-lock ((slot-value cache 'lock))
    (when (null object)
      (error "can not put null without key"))
    (let ((key (cache-key object)))
      (with-slots (pointer index filled fill size) cache
        (let ((pos (gethash key index)))
          (if pos
              (insert pos cache key object) ;;already exists
              (if filled
                  (let ((pos (evict cache)))
                    (insert pos cache key object)
                    (setf pointer (mod (incf pos) size)))
                  (progn
                    (insert fill cache key object)
                    (setf filled (= (incf fill) size)))))))))
  object)

(defun evict (cache)
  (let ((now (cache-now)))
    (with-slots (bits pointer size) cache
      (loop for i from pointer to (1- size) do
           (when (maybe-removed i cache now)
             (return-from evict i)))
      (loop for i from 0 to (1- pointer) do
           (when (maybe-removed i cache now)
             (return-from evict i)))
      (evict cache))))

(defun maybe-removed (pos cache now)
  (with-slots (bits ttl size) cache
    (if (or (zerop (aref bits pos))
            (< (aref ttl pos) now))
        (force-remove pos cache)
        (progn
          (setf (aref bits pos) 0)
          nil))))

(defun force-remove (pos cache)
  (with-slots (bits pointer size ttl keys data index) cache
    (let ((key (aref keys pos)))
      (setf (aref bits pos) 0)
      (setf (aref ttl pos) -1)
      (setf (aref keys pos) nil)
      (setf (aref data pos) nil)
      (remhash key index)))
  pos)

(defun insert (pos cache key object)
    (with-slots (bits ttl keys data index) cache
      (setf (aref bits pos) 0)
      (setf (aref ttl pos) (compute-ttl object))
      (setf (aref keys pos) key)
      (setf (aref data pos) object)
      (setf (gethash key index) pos))
  object)

(defvar cache-ttl-base )
(main:add-startup-hook (lambda ()
                         (setf cache-ttl-base (get-universal-time))))

(defun compute-ttl (object)
 (+ (cache-now)
    (cache-ttl object)))

(defun cache-now ()
  (- (get-universal-time) cache-ttl-base))

(defmethod cache-get ((cache aas-cache) (key string))
  (mp-with-lock ((slot-value cache 'lock))
    (with-slots (bits ttl data index) cache
      (let ((pos (gethash key index)))
        (if pos
            (if (< (aref ttl pos) (cache-now))
                (progn ;; expired hit
                  (force-remove pos cache)
                  (values nil nil))
                (progn ;; fresh hit
                  (setf (aref bits pos) 1)
                  (values (aref data pos) t)))
            (values nil nil))))))

(defmethod cache-remove ((cache aas-cache) (key string))
  (mp-with-lock ((slot-value cache 'lock))
    (with-slots (index) cache
      (let ((pos (gethash key index)))
        (when pos
          (force-remove pos cache))))))

(defmacro with-cached-object (var key cache value-form &body forms)
  (let ((found-gs (gensym "found"))
        (key-gs (gensym "key"))
        (cache-gs (gensym "cache")))
    `(let ((,key-gs ,key)
           (,cache-gs ,cache))
       (multiple-value-bind (,var ,found-gs)
           (cache-get ,cache-gs ,key-gs)
         (let ((,var (if ,found-gs
                         ,var
                         (cache-put ,cache-gs ,value-form))))
           ,@forms)))))

(defun validate-cache (cache no-nulls)
  (let ((hots 0))
    (with-slots (size fill filled bits keys data index) cache
      (loop for i from 0 to (1- size) do
           (let* ((key (aref keys i))
                  (object (aref data i))
                  (pos (gethash key index)))
             (when (or key pos object no-nulls)
               (incf hots (aref bits pos))
               (assert (equal key (cache-key object)))
               (assert (= i pos)))))))
  t)

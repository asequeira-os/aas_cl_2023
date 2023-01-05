(in-package :aas-graph)


;;'algorithms CLRS 2nd ed' pp. 585
;; INITIALIZE-SINGLE-SOURCE(G,s)
;; 1 for each vertex v E V[G]
;; 2     do d[v] <- inifinity
;; 3        p[v] <- nil
;; 4 d[s] = 0

(defclass single-source-data ()
  ((distances :initform nil :accessor distances)
   (previouses :initform nil :accessor previouses)))

(defun initialize-single-source (graph source)
  (let ((ssd (make-instance 'single-source-data)))
    (with-slots (distances previouses) ssd
      (setf distances (make-array (vertex-count graph)
                                  :initial-element infinity))
      (setf previouses  (make-array (vertex-count graph)
                                    :initial-element nil))
      (setf (aref distances (v-num source)) 0)
      ssd)))


;;'algorithms CLRS 2nd ed' pp. 586
;; RELAX(u,v,w)
;; 1 if d[v] > d[u] + w(u,v)
;; 2    then d[v] <- d[u] + w(u,v)
;; 3         p[v] <- u


(defun relax (ssd u v w)
  (with-slots (distances previouses) ssd
    (when (or (eq infinity (aref distances (v-num v)))
              (eq infinity (aref distances (v-num u)))
              (>  (aref distances (v-num v))
                  (+ (aref distances (v-num u)) w)))
      (setf (aref distances (v-num v))
            (if (eq infinity (aref distances (v-num u)))
                w
                (+ (aref distances (v-num u)) w)))
      (setf (aref previouses (v-num v)) u))))


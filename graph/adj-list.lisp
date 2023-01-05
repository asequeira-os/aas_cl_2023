(in-package :aas-graph)

(export '(adjacency-list-graph))

(defclass adjacency-list-graph (graph)
  ((adj-lists :accessor adj-lists :initform (make-hash-table)))
  (:documentation "graph with adjacency list"))


(defun get-adj-list (graph vertex)
  (gethash vertex (adj-lists graph) (list nil)))

;;updates adjacency list
(defmethod add-edge ((graph adjacency-list-graph) edge)
  (call-next-method graph edge)
  (let ((vertex-to (vertex-to edge))
        (vertex-from (vertex-from edge)))
    (let ((adj-list (get-adj-list graph vertex-from)))
      (setf (gethash vertex-from (adj-lists graph))
            (cons (cons vertex-to (edge-weight edge)) adj-list)))))


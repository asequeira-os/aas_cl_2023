
(in-package :aas-graph)

(export '(graph make-vertex add-vertex make-edge add-edge
          breadth-first-search depth-first-search))

(define-constant infinity 'infinity)
(define-constant white 'white)
(define-constant gray 'gray)
(define-constant black 'black)

(defgeneric add-vertex (graph vertex))
(defgeneric add-edge (graph edge))
(defgeneric shortest-path (algo src dest))
(defgeneric depth-first-search (algo))

(defclass vertex ()
  ((data :accessor vertex-data)
   (v-num :type fixnum :accessor v-num))
  (:documentation "node in a graph with arbitrary data attached"))

(defclass edge ()
  ((from :accessor vertex-from)
   (to :accessor vertex-to)
   (weight :accessor edge-weight))
  (:documentation "from and to nodes with a weight"))

(defclass graph ()
  ((vertices :accessor vertices :initform (list))
   (vertex-count :accessor vertex-count :initform 0)
   (edges :accessor edges :initform (list)))
  (:documentation "base class with list of edges and vertices"))

(defclass algo ()
  ()
  (:documentation "abstract base class for algorithms"))

(defmethod add-vertex ((graph graph) vertex)
  (setf (v-num vertex) (vertex-count graph))
  (incf (vertex-count graph))
  (setf (vertices graph)
        (cons vertex (vertices graph))))

(defmethod add-edge ((graph graph) edge)
  (setf (edges graph)
        (cons edge (edges graph))))

(defun make-edge (from to weight)
  (let ((edge (make-instance 'edge)))
    (setf (vertex-from edge) from)
    (setf (vertex-to edge) to)
    (setf (edge-weight edge) weight)
    edge))

(defun make-vertex (data)
  (let ((vertex (make-instance 'vertex)))
    (setf (vertex-data vertex) data)
    vertex))

(defun map-vertices (graph fn)
  (mapcar fn (vertices graph)))


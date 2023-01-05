(in-package :aas-misc)

(export '(dll-make dll-add-head
          dll-remove-tail dll-remove dll-empty-p
          dll-node-value))

(defstruct dll-node
  (prev)
  (next)
  (datum))

(defun dll-make ()
  "ctor"
  (let ((marker (make-dll-node :datum "marker")))
    (cons marker marker)))

(defun dll-add-head (dll data)
  "create a node with data and make that the head.
returns the newly created node."
  (let* ((cur-head (car dll))
         (node (make-dll-node :prev nil :next cur-head :datum data)))
    (setf (dll-node-prev cur-head) node)
    (setf (car dll) node)
    node))

(defun dll-add-head-node (dll node)
  (let* ((cur-head (car dll)))
    (setf (dll-node-prev cur-head) node)
    (setf (car dll) node)
    node))

(defun dll-remove-tail (dll)
  "removes the tail node and returns it"
  (unless (dll-empty-p dll)
    (let* ((marker (cdr dll))
           (tail (dll-node-prev marker))
           (tail-1 (dll-node-prev tail)))
      (setf (dll-node-prev marker) tail-1)
      (if tail-1
        (setf (dll-node-next tail-1) marker)
        (setf (car dll) marker))
      tail)))


(defun dll-remove (dll node)
  "remove node from dll and return it"
  (let ((prev (dll-node-prev node))
        (next (dll-node-next node)))
    (if prev
      (setf (dll-node-next prev) next)
      (setf (car dll) next))
    (when next
      (setf (dll-node-prev next) prev))
    node))


(defun dll-empty-p (dll)
  (eq (car dll) (cdr dll)))

(defun dll-node-value (node)
  (dll-node-datum node))

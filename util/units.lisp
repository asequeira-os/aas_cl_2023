(defpackage :aas-units
  (:use :common-lisp)
  (:export list-units convert convert-func))

(in-package :aas-units)

(defparameter *units* (make-hash-table))
(defparameter *unit-conv* (make-hash-table :test #'equal))

(defclass unit ()
  ((id :initform 0)
   (symbol)
   (name)))

(defclass i18n-unit ()
  ((unit)
   (locale)
   (i18n-symbol)
   (i18n-name)))

(defclass unit-conv ()
  ((from)
   (to)
   (offset)
   (factor)))

(defun define-unit (id symbol name)
  (let ((unit (make-instance 'unit)))
    (setf (slot-value unit 'id) id)
    (setf (slot-value unit 'symbol) symbol)
    (setf (slot-value unit 'name) name)
    (setf (gethash id *units*) unit)
    unit))

(defun define-conv (unit-from unit-to factor &optional (offset 0))
   (let ((unit-conv (make-instance 'unit-conv))
         (conv-key (cons unit-from unit-to)))
     (setf (slot-value unit-conv 'from) unit-from)
     (setf (slot-value unit-conv 'to) unit-to)
     (setf (slot-value unit-conv 'offset) offset)
     (setf (slot-value unit-conv 'factor) factor)
     (setf (gethash conv-key *unit-conv*) unit-conv)
     unit-conv))

(defun list-units ()
  (let ((list (list)))
   (maphash (lambda (k v)
              (declare (ignore v))
              (setf list (cons k list)))
            *units*)
   list))

(defun find-conv-% (unit-from unit-to)
  (let ((conv-key (cons unit-from unit-to)))
    (gethash conv-key *unit-conv*)))

(defun convert (input unit-from unit-to)
  (let ((fn (convert-func unit-from unit-to)))
    (if fn
        (funcall fn input)
        (error "no conversion function for ~A ti ~A " unit-from unit-to))))

(defun convert-func (unit-from unit-to)
  (let ((conv (find-conv-% unit-from unit-to)))
    (if conv
        (let ((offset (slot-value conv 'offset))
              (factor (slot-value conv 'factor)))
          (if (not (zerop offset))
              (lambda (input)
                (+ offset (* input factor)))
              (lambda (input)
                (* input factor))))
        (let ((conv (find-conv-% unit-to unit-from)))
          (if conv
              (let ((offset (slot-value conv 'offset))
                    (factor (slot-value conv 'factor)))
                (if (not (zerop offset))
                    (lambda (input)
                      (/ (- input offset) factor))
                    (lambda (input)
                      (/ input factor)))))))))


;;todo 5
;;idea for arbitrary unit conversions
;;treat each unit as a vertex in a graph
;;treat each defined conversion between two units as an edge
;;
;;the conversion between any two components
;;is given by the shortest path

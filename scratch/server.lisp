(in-package :scratch)

(defvar *server* nil)

(defun start-server ()
  (unless *server*
    (setf *server* (hunchentoot:start-server :port 7777))))

(setf hunchentoot:*dispatch-table* (list 'handler-finder))

;;todo Z i shall make a scheme for finding handlers
;;we can register a handler finder for each path componenet
;;to form a  hierarchy of handlers
;;may be the hierarchy is a package name
;;and the final method is the name of a function in that package
;;we cna use clause method dipatch to ensure that the type of
;;argument matches *request* for basic security so people
;;can't call arbitrary functions from url
(defun handler-finder (request)
  'foo)

(defun parameter (name)
  (hunchentoot:parameter name))


(defun foo ()
  "doc"
  (cl-who:with-html-output-to-string (str)
  (let ((*standard-output* str))
    (cl-who:htm
    (:h1
     "fooo bar xada")
    (:p "we got xxx=")
    (format t "trying stdout")
    (:p (format str "~A" (parameter "xxx")))))))


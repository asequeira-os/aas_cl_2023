(defpackage :util
  (:use :common-lisp)
  (:export :test :network-bytes-to-number :test-suites-status
           :start-slime
           :stop-slime
           :empty-string-p
           :trim
           :invert-case
           :while
           :parse-body
           :match-readtable-case
           :until
           :intern-string
           :with-test-mode :with-compile-mode :with-run-mode
           :test-p :development-p
           :test-suites-explain))

(in-package :util)

;;not really sure, need to check best way to do this
;;using some arbitrary epsilon is not ok ?
(defun test-float-equalp (f1 f2)
  (<= (abs (- f1 f2)) (/ f1 1000.0)))


;;copied from
;; http://en.wikibooks.org/wiki/Programming:Common_Lisp/
;; Advanced_topics/Files_and_Directories/C_struct
;;
(defun network-bytes-to-number (buffer start-index total-bits)
  "Convert network byte ordered sequence of unsigned bytes to a number."
  (unless (= (mod total-bits 8) 0)
    (error "Please specify total-bits as total for multiples of eight bit bytes"))
  (let ((value 0))
    (loop for i downfrom (- total-bits 8) downto 0 by 8
       for cursor upfrom start-index
       do (setf value (dpb (elt buffer cursor)
                           (byte 8 i) value)))

    ;;         (format t "buffer[~d]==#x~2X; shift<< ~d bits; value=~d~%"
    ;;                 cursor (elt buffer cursor) i value)
    value))


;;todo 5 need to parameterize and config slime swank startup
(defvar *slime-running* nil)

(defun start-slime ()
  ;;todo 3 fix slime dump and load hack
  (when (and swank:*log-output* (not (open-stream-p swank:*log-output*)))
    (setf swank:*log-output* nil)
    (swank::init-log-output))
  (unless *slime-running*
    (swank:create-server :port 4005  :dont-close t)
    ;; :coding-system "utf-8-unix")
    (setf *slime-running* t)))

(defun stop-slime ()
  (when *slime-running*
    (swank:stop-server 4005)
    (setf *slime-running* nil)))

(defun empty-string-p (str)
  (or (null str) (and (stringp str) (string= "" str))))

;;todo 2 this duplicates some of the clean-up-spaces. consolidate
(defun trim (string)
  "remove leading and trailing space, tab, CR, and LF characters"
  (string-trim  #(#\space #\tab #\return #\linefeed) string))

;;todo 5 need to find a better home for these
(defmacro while (test &body body)
  "repeat body while test is true.
puts a block named while around the body"
  `(block while
     (loop
        (if ,test
            (progn ,@body)
	    (return-from while)))))

(defmacro until (test &body body)
  "repeat body until test is true.
puts a block named until around the body"
  `(block until
     (loop
        (if (not ,test)
            (progn ,@body)
	    (return-from until)))))


;;this is copied from alexandria
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defun invert-case (s)
  "Invert the case of string if it is all a single case."
  (if (null s)
      s
      (if (symbolp s)
          (invert-case (symbol-name s))
          (if (some #'upper-case-p s)
              (if (some #'lower-case-p s)
                  s
                  (string-downcase s))
              (string-upcase s)))))

(defun match-readtable-case (s)
  (ecase (readtable-case *readtable*)
    ((:upcase) (string-upcase s))
    ((:downcase) (string-downcase s))
    ((:preserve) s)
    ((:invert) (invert-case s))))


(defparameter *interned-strings* (make-hash-table :test #'equal))

(defun intern-string (string)
  (check-type string string)
  (let ((interned (gethash string *interned-strings*)))
    (if interned
        interned
        (setf (gethash string *interned-strings*) string))))

(defconstant +DEV-MODE+ '+DEV-MODE+)
(defconstant +TEST-MODE+ '+TEST-MODE+)
(defconstant +COMPILE-MODE+ '+COMPILE-MODE+)
(defconstant +RUN-MODE+ '+RUN-MODE+
  "this does not necessarily mean production environment")

(defvar *system-mode* +DEV-MODE+)

(defun set-system-mode (mode)
  (setf *system-mode* mode))

(defun development-p ()
  (string= *system-mode* +DEV-MODE+))

(defun test-p ()
  (string= *system-mode* +TEST-MODE+))

(defmacro with-system-mode (mode &body body)
  `(let ((*system-mode* ,mode))
     ,@body))

(defmacro with-test-mode (&body body)
  `(with-system-mode +TEST-MODE+ ,@body))

(defmacro with-compile-mode (&body body)
  `(with-system-mode +COMPILE-MODE+ ,@body))

(defmacro with-run-mode (&body body)
  `(with-system-mode +RUN-MODE+ ,@body))

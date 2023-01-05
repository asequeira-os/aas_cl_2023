(defpackage :main
  (:use :cl ))

(in-package :main)
(export '(make-compile
          make-test
          make-run
          *config-dir*
          console-trace
          dumped-exe-p
          add-dump-hook
          add-startup-hook))

#+nil(error "nil is in *features*  s-exp comments broken")

(defvar *aas-safety-check*)
(when (boundp '*aas-safety-check*)
  (error "unexpected loading again in dev or loading in dump"))
(setf *aas-safety-check* t)

(defun asvaaa-get-env (variable)
  "Get environment VARIABLE's value. VARIABLE may be null."
  (when variable
    (#+sbcl  sb-ext:posix-getenv
             #+cmu   unix:unix-getenv
             #+ccl   ccl:getenv
             #+ecl   ext:getenv
             #+clisp ext:getenv
             #+abcl  extensions:getenv
             variable)))

;;gets unbound in 'startup'
(defparameter *SOURCE-TOP* (truename (pathname (asvaaa-get-env "GIT_TOP")))
  "serves as the top level dir of my source/built stuff")
(let ((lisp-build-top (asvaaa-get-env "LISP_BUILD_TOP")))
  (unless lisp-build-top
    (error "please set LISP_BUILD_TOP for fasl files"))
  (defparameter *BUILD-TOP* (truename (pathname lisp-build-top))
    "place to put compiled stuff"))

(defvar *source-top-exists* )
(defvar *config-dir* )


#+ccl
(setf CCL:*DEFAULT-FILE-CHARACTER-ENCODING* :utf-8)

;;todo 2 default float format not working
;;seems liek all my tests are setup for single float
;;since SLIME REPL in it;s own thread alwasy had that
;;and now without that the tests resulst chnage to double float and fail
;;(setf *read-default-float-format* 'double-float)

(load (merge-pathnames #P"src/common/init" *SOURCE-TOP*))

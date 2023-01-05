;;
;;common init file
;;

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (proclaim '(optimize (safety 3) (space 0) (speed 0) (debug 3)))
;;   (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3))))

(in-package :main)

(defun aas-fasl-path (source)
  (compile-file-pathname
   (merge-pathnames source *BUILD-TOP*)))

(defun aas-source-path (source)
  (merge-pathnames source *SOURCE-TOP*))

(defun aas-compile-load-file (source)
  (let ((fasl-path (aas-fasl-path source))
        (source-path (aas-source-path source)))
    (ensure-directories-exist fasl-path)
    (multiple-value-bind (output-truename warnings-p failure-p)
        (compile-file source-path :output-file fasl-path)
      (declare (ignorable warnings-p))
      (unless failure-p
        (load output-truename)))))

(defun aas-load-file (source)
  (or (load (aas-fasl-path source) :if-does-not-exist nil)
      (aas-compile-load-file source)))

;;if the lisp implementation has asdf built-in,
;;I should have loaded it.
;;(or (find-package :asdf)
(aas-load-file #P"thirdparty/asdf")
;;  (error "failed to have asdf"))


;;todo 5 asdf fasl files are going to wrong place
;;/cygdrive/c/Users/Antony/AppData/Roaming/common-lisp/cache/ccl-1.7-f94-win-amd64/
(asdf:initialize-source-registry
 (list :source-registry (list :tree *SOURCE-TOP*)
       :inherit-configuration))

(asdf:initialize-output-translations
 (list :output-translations (list *BUILD-TOP*)
       :inherit-configuration))

(asdf:oos 'asdf:load-op :cl-fad :verbose nil)
(asdf:operate 'asdf:load-op :com.dvlsoft.clon :verbose nil)

;;load up my build system
(aas-load-file #P"src/common/load-utils")

;; (with-output-to-string (*standard-output*)
;;   (aas-build:load-asdf-libs
;;    '(:swank)))
(with-output-to-string (*standard-output*)
  (asdf:oos 'asdf:load-op :swank :verbose nil))

;; (aas-build:compile-load-files
;;  '("../util/util"
;;    "../test/aas-build"))
(aas-load-file #P"src/util/util")
(aas-load-file #P"src/test/test")
(aas-load-file #P"src/test/test-test")

(com.dvlsoft.clon:nickname-package)
(use-package :clon)

(aas-load-file #P"src/common/cmd-line")

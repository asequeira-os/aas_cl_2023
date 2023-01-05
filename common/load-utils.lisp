;;build/load mechanisms for my source code (no more asdf)
;;
;;I will always load file, it is pointless to compile only
;;the key reason for this - function definitions from one file
;;are not available to another unless that file is loaded
;;
;;I need to follow the convention that simple loading of any
;;of my source files does not do any 'real' runtime stuff
;;
;;implies anything needing a any real runtime initialization
;;needs to be called explicitly in the application startup



(defpackage :aas-build
  (:use :common-lisp)
  (:export :load-asdf-libs
           :*verbosity*
           :get-env
           :relative-to-absolute-path
           :load-config
           :compile-load-files)
  (:import-from :main *source-top* *build-top* *source-top-exists* ))

(in-package :aas-build)

(defvar *verbosity* 1
  "set this to nil to suppress build messages")

(defvar *verbose-asdf* nil "set true to make asdf load verbose")

;;todo 2 this is a duplicate of asvaaa-get-env in init-port.lisp
(defun get-env-% (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+cmu (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                    :key #'string))
  #+gcl (si:getenv (string var))
  #+ecl (ext:getenv var)
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+(or openmcl digitool) (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu gcl lispworks lucid openmcl digitool sbcl ecl)
  (error "asvaaa-get-env not implemented for this CL"))

(defun get-env (var)
  (or (get-env-% var)
      (error "please set environment variable ~A"  var)))

(defvar *cache* nil)

(setf *source-top-exists*
      (or (cl-fad::directory-exists-p *source-top*)
          (error "*source-top* ~A doe snot exist" *source-top*)))

(or (cl-fad::directory-exists-p *build-top*)
    (error "*build-top* ~A does not exist" *build-top*))

(defun message (level fmt &rest args)
  (when (and (numberp *verbosity*) (not (zerop *verbosity*))
             (<= level *verbosity*))
    (apply #'format t fmt args)))

(defun aas-built-already (file)
  (and *cache*
       (gethash file *cache*)))

(defun mark-built (file status)
  (message 2 "~%marking ~A as ~A" file status)
  (setf (gethash file *cache*) (or status :fail))
  status)

(defmacro cached (&body body)
  `(let ((*cache* (or *cache* (make-hash-table :test #'equalp ))))
     (progn ,@body)))

;;seperate cache for asdf since I don't need them rebuilt
(defvar *asdf-cache* (make-hash-table :test #'equalp ))

(defun load-asdf-lib (lib)
  (unless (gethash lib *asdf-cache*)
    (asdf:load-system lib :verbose *verbose-asdf*)
    (setf (gethash lib *asdf-cache*) t)))

(defun load-asdf-libs (libs)
  (mapc (lambda (lib)
          (load-asdf-lib lib))
        libs))

(defvar *loaded-configs* (make-hash-table :test #'equal))

(defun load-config (config-path &optional (allow-reload nil))
  (let ((path (merge-pathnames config-path main:*config-dir*)))
    (when  (or allow-reload (not (gethash path *loaded-configs*)))
      (or (setf (gethash path *loaded-configs*) (load path))
          (error "failed to load config ~A from ~A" config-path path)))))

(defun get-self-path ()
  (if *compile-file-truename*
      *compile-file-truename*
      (if *load-pathname*
          *load-pathname*
          (error "no compile or load context"))))

(defun get-self-dir ()
  (directory-namestring (get-self-path)))

(defmacro compile-load-files (&rest files)
  `(find-if-not (lambda (file)
                  (aas-compile-file (get-self-dir) file))
                ,@files))

(defmacro aas-compile-file (dir file)
  `(if *source-top-exists*
       (aas-compile-load-source ,dir ,file)
       (aas-load-fasl ,dir ,file)))

(defun aas-load-fasl (dir file)
  (declare (ignorable dir file))
  ;;not necessary since we are doing dump
  (error "todo 9 implement fasl load for production use"))

(defun fix-dir (dir)
  (enough-namestring (enough-namestring dir *build-top*) *source-top*))

(defparameter *force-rebuild* nil)

(defun aas-compile-load-source (orig-dir file)
  (let* ((dir (fix-dir orig-dir))
         (source-path (merge-pathnames file (merge-pathnames dir *source-top*)))
         (source-relative-path (enough-namestring source-path *source-top*))
         (full-compile-path
          (merge-pathnames (enough-namestring
                            (compile-file-pathname source-path) *source-top*)
                           *build-top*)))
    (let ((fasl-date (and (probe-file full-compile-path)
                          (file-write-date  full-compile-path)))
          (source-date (file-write-date
                        (make-pathname :type "lisp" :defaults source-path))))
      (if (or *force-rebuild* (null fasl-date) (< fasl-date source-date))
          (progn
            (message 2 "~A -> ~A -> ~A~%"
                     source-path source-relative-path
                     full-compile-path )
            (ensure-directories-exist full-compile-path)
            (multiple-value-bind (output-truename warnings-p failure-p)
                (compile-file source-path
                              :output-file full-compile-path)
              (declare (ignorable warnings-p))
              (if failure-p
                  (error "compile failed")
                  (load output-truename))))
          (load full-compile-path)))))

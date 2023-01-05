(in-package :main)

(defvar *dumped-exe* nil)

(defun dumped-exe-p ()
  *dumped-exe*)

(defun quit-cl (status)
  (or #+ccl (ccl:quit status)
      (error "quit not implemented for this ~A"  (lisp-implementation-type))))

(defun console-trace (msg)
  (format t "~%;# ~A" msg))

;;this can be used to tell which binary I am running in production
(defstruct server-build
  (doc "build info")
  (utc (get-universal-time))
  (impl (format nil "~A ~A"
                (lisp-implementation-type)
                (lisp-implementation-version )))
  (host (machine-instance))
  (os (format nil "~A ~A" (software-type) (software-version) ))
  (hw (format nil "~A ~A" (machine-type) (machine-version)))
  (home (user-homedir-pathname)))

(defparameter *server-build* (make-server-build)
  "executable build info")

(defun set-config-dir ()
  (let ((config-dir (getopt :short-name "cd")))
    (when (util:empty-string-p config-dir)
      (error "please specify config dir"))
    (setf *config-dir* (truename (pathname config-dir)))
    (console-trace (format nil "config dir is ~A" *config-dir*))))

;;-------------------------------------------------
;;startup hooks

(defvar *startup-hooks* nil)
(defvar *startup-hooks-called* nil)

(defun add-startup-hook (hook)
  (assert (functionp hook))
  (assert (not *dumped-exe*))
  (assert (or (null *startup-hooks-called*) (util:development-p) (util:test-p)) nil
          "start up already done")
  (unless *startup-hooks-called*
    (push hook *startup-hooks*)))

(defun call-startup-hooks ()
  (unless *startup-hooks-called*
    (console-trace "calling startup hooks")
    (assert *startup-hooks* nil "no startup hooks at all?")
    (setf *startup-hooks-called* t)
    (dolist (hook (reverse *startup-hooks*))
      (funcall hook))))

(add-startup-hook #'set-config-dir)

;;-------------------------------------------------
;;dump hooks

(defvar *dump-hooks* nil)
(defvar *dump-hooks-called* nil)

(defun add-dump-hook (hook)
  (assert (functionp hook))
  (assert (not *dumped-exe*))
  (assert (or (null *dump-hooks-called*) (util:development-p)) nil
          "dumping already done")
  (unless *dump-hooks-called*
    (push hook *dump-hooks*)))

(defun call-dump-hooks ()
  (unless *dump-hooks-called*
    (console-trace "calling dump hooks")
    (assert *dump-hooks* nil "no dump hooks at all?")
    (setf *dump-hooks-called* t)
    (dolist (hook *dump-hooks*) ;;LIFO order is important
      (funcall hook))))

(add-dump-hook (lambda ()
                 (util:stop-slime)
                 (makunbound '*source-top-exists*)
                 (makunbound '*source-top*)
                 (makunbound '*build-top*)))
;;-------------------------------------------------------


(defsynopsis (:postfix "no other arguments")
  (text :contents "xeduler.com main rpc server")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit."))
  (group (:header "main action")
         (flag :short-name "dev" :long-name "dev-mode"
               :description "Development mode")
         (flag :short-name "run" :long-name "run-mode"
               :description "run the server")
         (flag :short-name "test" :long-name "test-mode"
               :description "run unit tests")
         (flag :short-name "dump" :long-name "dump-binary"
               :description "compile and dump")
         (stropt :short-name "x" :long-name "cmd-test"
                 :description "run arbitrary lisp s-exp"))
  (group (:header "configuration options:")
         (flag :short-name "dbg" :long-name "debug-mode"
               :description "set debugflags")
         (path :short-name "cd" :long-name "config-dir"
               :type :directory :env-var "XEDULER_CONFIG_DIR"
               :description "path to the config dir")
         (path :short-name "f" :long-name "init-file"
               :type :file
               :description "path to the init file")))

(defun main ()
  (make-context)
  (when (getopt :short-name "h")
    (help)
    (exit))
  (set-debug-mode (getopt :short-name "dbg"))
  (when (getopt :short-name "dev")
    (make-development))
  (when (getopt :short-name "dump")
    (make-dump))
  (when (getopt :short-name "test")
    (make-test))
  (when (getopt :short-name "x")
    (console-trace "supposed to execute s-exp argument"))
  (when (getopt :short-name "run")
    (make-run)))

(defun make-run ()
  (util:with-run-mode
    (util:start-slime)
    (unless *dumped-exe*
      (make-compile))
    (call-startup-hooks)
    ;;todo 3 how to fix startup function instead of this hardcoding
    (funcall (symbol-function 'load-xeduler))
    (console-trace "server started")
    (when *dumped-exe*
      (read))))

(defun make-compile ()
  (when *dumped-exe*
    (error "compiling not allowed from exe"))
  (console-trace "compiling")
  (let ((aas-build::*verbose-asdf* nil)
        (aas-build::*force-rebuild* t) )
    (load (merge-pathnames  #P"src/build" *SOURCE-TOP*))
    (console-trace "compile finished")))

(defun make-dump ()
  (when *dumped-exe*
    (error "dumping not allowed from exe"))
  (make-compile)
  (console-trace "dumping")
  (call-dump-hooks)
  (setf *dumped-exe* t)
  (asdf:clear-source-registry)
  (asdf:clear-output-translations)
  ;;todo 1 need name for the dump file
  (dump "fooo" main))

(defun make-development ()
  ;;todo 2 remove the slime start hack if slime provides alternative
  (when *dumped-exe*
    (error "development not allowed from exe"))
  (assert (util:development-p))
  (util:start-slime)
  ;;set debugger on for unit tests when in dev
  (set-debug-mode t)
  (console-trace "development mode"))

(defun set-debug-mode (flag)
  (setf aseq-test:*debug-on-error* flag)
  (setf aseq-test:*debug-on-failure* flag))

(defun make-test ()
  (util:with-test-mode
    (unless *dumped-exe*
      (util:start-slime)
      (make-compile))
    (call-startup-hooks)
    (funcall (symbol-function 'load-xeduler))
    (let ((aseq-test:*skip-succeeded* nil)
          ;; todo 4 need multi host test fix hardcoded name
          (aseq-test:*multi-host* nil)
          ;;(aas-rpc::*optimize-local-calls* t)
          )
      (console-trace "running tests")
      (funcall (symbol-function
                'cl-user::aas-all-tests
                ;;(find-symbol "ALL-TESTS" (find-package :auth-test))
                ))
      (let ((all-pass (zerop (length (aseq-test:failed-tests)))))
        (unless aseq-test:*debug-on-error*
          (quit-cl (if all-pass 0 1)))))))

(main)

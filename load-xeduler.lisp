;;the start up script
;;application data and config load

(in-package :main)


(defvar *xeduler-loaded* nil)
(defvar *xeduler-offline-loaded* nil)

(defun load-xeduler-offline ()
  (unless *xeduler-offline-loaded*
    (when (main:dumped-exe-p)
      (error "load-xeduler-offline should not be called from dumped executable"))
    (main:console-trace "loading offline data")
    (geo-US::load-data)
    (aas-local-time:load-timezones)
    (i18n-data::load-all-data)

    (setf *xeduler-offline-loaded* t)))

(defun load-xeduler ()
  (unless *xeduler-loaded*
    (load-xeduler-offline)

    (main:console-trace "loading xeduler init data and config")
    (log:log-init)
    (cipher::init)
    (email::load-config)
    (aas-build::load-config "db-config")
    (aas-build::load-config "init-apps")
    (aas-build::load-config "disabled-vdb")

    (aas-build:load-config "www-ui-config")

    (aas-local-time:cron-set-entry 'log-rotate #'log:log-rotate 0 0)

    ;;start the server
    ;;should be the last line of the start up
    (aas-build:load-config "http-config")

    (setf *xeduler-loaded* t)
    (main:console-trace "xeduler loaded")))


(main:add-dump-hook #'load-xeduler-offline)

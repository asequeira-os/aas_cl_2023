(in-package :i18n-data)

;;todo 3 loading all i18n data during build, no run time change possible
(defvar *data-dir* (merge-pathnames #P"src/i18n/data/"
                                    main::*source-top*))

(main:add-dump-hook (lambda ()
                      (makunbound '*data-dir*)))

(defun load-data (filename)
  (load (merge-pathnames filename *data-dir*)))

(defun load-all-data ()
  (load (merge-pathnames "load-all.lisp" *data-dir*)))

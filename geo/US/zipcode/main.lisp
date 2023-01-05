(in-package :geo-US)

(defvar *data-dir* (merge-pathnames #P"src/geo/US/zipcode/"  main::*source-top*))
(main:add-dump-hook (lambda ()
                      (makunbound '*data-dir*)))

(defvar *data-file-name*  "zipcodes1.csv")
(defparameter +MAX-ZIP-RECORDS+ 10000
  "lower this during development to reduce load time")

(defvar *states-info* (make-hash-table :test #'equal :size 60))

(defvar *zip-cities* (make-array 100000 :adjustable nil
                                 :initial-element nil))
(defvar *zip-states* (make-array 100000 :adjustable nil
                                 :initial-element nil))

(aas-rpc:def-rpc-struct state-info
    (code nil :type string)
  (name nil :skip-rpc)
  (cities nil :skip-rpc)
  (timezones nil :skip-rpc))

(defmethod aas-rpc:deserialize-after ((object state-info))
  (or
   (gethash (state-info-code object) *states-info*)
   (error "Invalid state code ~A" (state-info-code object))))

(defmethod db-base:get-struct-columns ((symbol (eql 'state-info)))
  '((code :db-type "VARCHAR(2)")))

(defun make-state-info-from-db (&key code)
  (gethash code *states-info*))

(defmethod print-object ((data state-info) stream)
  (if (not *print-readably*)
      (format stream "#<state-info ~A>#" (state-info-code data))
      (error "no readable printer yet for type state-info")))

(defun insert-zipcode-db (zipcode city state)
  (let* ((zip (parse-integer zipcode))
         (state-info (or (gethash state *states-info*)
                         (make-state-info :code state :cities (list)))))
    (setf (gethash state *states-info*) state-info)
    (push city (state-info-cities state-info))
    (if (aref *zip-cities* zip)
        (if (consp (aref *zip-cities* zip))
            (push city (aref *zip-cities* zip))
            (setf (aref *zip-cities* zip)
                  (list (aref *zip-cities* zip) city)))
        (setf (aref *zip-cities* zip) city))
    (if (aref *zip-states* zip)
        (if (consp (aref *zip-states* zip))
            (unless (find state (aref *zip-states* zip) :test #'equal)
              (push state (aref *zip-states* zip)))
            (unless (equal state (aref *zip-states* zip))
              (setf (aref *zip-states* zip)
                    (list (aref *zip-states* zip) state))))
        (setf (aref *zip-states* zip) state))))



(defun load-db (pathname)
  (aas-misc:process-csv-file pathname #'process-csv-line)
  (post-process-db))

(defun process-csv-line (line-num fields)
  "this needs to change if we change our zipcodes data source"
  (if (= 1 line-num) ;skip header line
      t
      (let ((zipcode (elt fields 0))
            (city (elt fields 3))
            (state (elt fields 4)))
        (insert-zipcode-db zipcode city state)
        (< line-num +MAX-ZIP-RECORDS+))))

(defun post-process-db ()
  (maphash (lambda (state state-info)
             (declare (ignorable state))
             (setf (state-info-name state-info)
                   (US-state-name state))
             (setf (state-info-timezones state-info)
                   (get-US-state-timezones state))
             (setf (state-info-cities state-info)
                   (coerce (sort (remove-duplicates
                                  (state-info-cities state-info) :test #'equal)
                                 #'string-lessp)
                           'simple-vector)))
           *states-info*)
  (map-into *zip-cities*
            (lambda ( city-or-list)
              (if (null city-or-list)
                  nil
                  (if (stringp city-or-list)
                      city-or-list
                      (if (consp city-or-list)
                          (let ((list (sort
                                       (remove-duplicates city-or-list
                                                          :test #'equal)
                                       #'string-lessp)))
                            (if (= 1 (length list))
                                (first list)
                                list))
                          (error "unknown ~A of type ~A"
                                 city-or-list (type-of city-or-list))))))
            *zip-cities*))

(defvar *loaded-zip-flag* nil)

(defun load-data ()
  (unless *loaded-zip-flag*
    (load-db (merge-pathnames *data-dir*  *data-file-name*))
    (setf *loaded-zip-flag* t)

    ;;these are the top of the test data
    ;;so will work even if i read partial db during development
    (assert (equal (aref *zip-cities* 68623) "CEDAR RAPIDS"))
    (assert (equal (aref *zip-cities* 22727) (list "BANCO" "ETLAN")))
    (assert (equal (aref *zip-states* 68623) "NE"))
    (assert (equal (aref *zip-states* 22727) "VA"))))


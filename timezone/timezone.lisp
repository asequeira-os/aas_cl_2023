(in-package :aas-local-time)

(defvar *current-timezone-file*)
(defvar *current-line-no*)

(defvar *current-zone*)

(defvar *files-data*)

(defvar *current-file-rules*)
(defvar *current-file-zones*)

(defvar *links* (make-hash-table :test #'equal))

(defclass rule-line ()
  (filename
   line-no
   name
   from
   to
   type
   in
   on
   at
   save
   letter
   comment))

(defclass zone-line ()
  (filename
   line-no
   name
   gmtoff
   rules
   format
   until
   comment))

(defun make-zone-line (name gmtoff rules format &optional year in on at comment)
  (let ((obj (make-instance 'zone-line)))
    (setf (slot-value obj 'filename) *current-timezone-file*)
    (setf (slot-value obj 'line-no ) *current-line-no*)
    (setf (slot-value obj 'name) name)
    (setf (slot-value obj 'gmtoff) gmtoff)
    (setf (slot-value obj 'rules) rules)
    (setf (slot-value obj 'format) format)
    (setf (slot-value obj 'until) (list year in on at))
    (setf (slot-value obj 'comment ) comment)
    obj))

(defun make-rule-line (name from to type in on at save letter &optional comment)
  (let ((obj (make-instance 'rule-line)))
    (setf (slot-value obj 'filename) *current-timezone-file*)
    (setf (slot-value obj 'line-no ) *current-line-no*)
    (setf (slot-value obj 'name    ) name   )
    (setf (slot-value obj 'from    ) from   )
    (setf (slot-value obj 'to      ) to     )
    (setf (slot-value obj 'type    ) type   )
    (setf (slot-value obj 'in      ) in     )
    (setf (slot-value obj 'on      ) on     )
    (setf (slot-value obj 'at      ) at     )
    (setf (slot-value obj 'save    ) save   )
    (setf (slot-value obj 'letter  ) letter )
    (setf (slot-value obj 'comment ) comment)
    obj))

(defun make-rule-from-fields (fields)
  (let* ((rule-ob (apply #'make-rule-line (cdr fields)))
         (rule-lines (get-db *current-file-rules*
                             (slot-value rule-ob 'name))))
    (if rule-lines
        (push rule-ob (first rule-lines))
        (set-db  *current-file-rules*  (slot-value rule-ob 'name)
                 (list (list rule-ob))))))

(defun fix-rule-order ()
  (let ((rule-names (get-db-keys *current-file-rules*)))
    (dolist (rule-name rule-names)
      (set-db *current-file-rules* rule-name
              (list (reverse (first (get-db *current-file-rules* rule-name))))))))

(defun make-zone-from-fields (fields)
  (let* ((zone-ob (apply #'make-zone-line (cdr fields)))
         (zone-lines (get-db *current-file-zones*
                             (slot-value zone-ob 'name))))
    (if zone-lines
        (push zone-ob (first zone-lines))
        (set-db  *current-file-zones*  (slot-value zone-ob 'name)
                 (list (list zone-ob))))))

(defun fix-zone-order ()
  (let ((zone-names (get-db-keys *current-file-zones*)))
    (dolist (zone-name zone-names)
      (set-db *current-file-zones* zone-name
              (list (reverse (first (get-db *current-file-zones* zone-name))))))))

(defun process-data-dir
    (&optional (data-dir  *data-dir*) (file-list *timezone-files-list*))
  ;;(format t "~%processing dir ~A" data-dir)
  (let ((*files-data* (make-db :test #'equal)))
    (dolist (file file-list)
      (let ((*current-timezone-file* file)
            (path (merge-pathnames file data-dir)))
        (process-data-file path)))
    *files-data*))

(defun process-data-file (file)
  ;;(format t "~%processing file ~A" file)
  (let ((*current-file-rules* (make-db :test #'string-equal))
        (*current-file-zones* (make-db :test #'string-equal)))
    (with-open-file (fs file :direction :input)
      (do ((line (read-line fs) (read-line fs nil 'eof))
           (*current-line-no* 1 (incf *current-line-no*)))
          ((eq line 'eof) "Reached end of file.")
        (let ((line (trim line)))
          (or (empty-string-p line)
              (match-comment line)
              (match-rule line)
              (match-link line)
              (match-zone line)
              (match-zone-continuation line)))))
    ;;(print *current-file-rules*)
    (fix-rule-order)
    (fix-zone-order)
    (set-db *files-data* file
            (list *current-file-zones* *current-file-rules*))))

(defun match-comment (line)
  (eql #\# (aref line 0)))

(let ((re (cl-ppcre:create-scanner
           "^Link\\s+")))
  (defun match-link (line)
    (when (cl-ppcre:scan-to-strings re line)
      (let ((fields (get-fields line)))
        (assert (<= 3 (length fields) 4) (fields) "expected nine fields ~A" fields)
        ;;(print line)
        ;;(print fields)
        (setf (gethash (clean-tz-name% (third fields)) *links*)
              (clean-tz-name% (second fields))))
      t)))

(let ((re (cl-ppcre:create-scanner
           "^Rule\\s+")))
  (defun match-rule (line)
    (when (cl-ppcre:scan-to-strings re line)
      ;;(format t "~%~dgot rule~A" *current-line-no* line)
      (let ((fields (get-fields line)))
        ;;(print  fields)
        (assert (<= 10 (length fields) 11) (fields) "unexpected fields ~A" fields)
        ;;(print line)
        ;;(print fields)
        (make-rule-from-fields fields)
        )
      t)))

(defun clean-tz-name% (name)
  (cl-ppcre:regex-replace-all "_" name " "))

(let ((re (cl-ppcre:create-scanner
           "^Zone\\s+")))
  (defun match-zone (line)
    (when (cl-ppcre:scan-to-strings re line)
      ;;(format t "~%~dgot zone~A" *current-line-no* line)
      (let* ((fields (get-fields line))
             (length (length fields)))
        ;;(print  fields)

        (assert (<= 5 length 10) (fields)  "expected fields ~A" fields)
        (setf (second fields) (clean-tz-name% (second fields)))
        (setf *current-zone* (second fields))
        (make-zone-from-fields fields)
        )
      t)))

(defun match-zone-continuation (line)
  (let* ((fields (get-fields line))
         (length (length fields)))
    ;;(print  fields)
    (assert (<= 3 length) (fields)  "expected fields ~A" fields)
    (push *current-zone* fields)
    (push "Zone" fields)
    (make-zone-from-fields fields)
    t))

(defun get-fields (line)
  (let ((length (length line)))
    (do* ((fields '())
          (start 0)
          (index 0 (incf index))
          (char (aref line index) (aref line index) )
          (is-quote-char (char= char #\") (char= char #\"))
          (is-comment-char (char= char #\#) (char= char #\#))
          (in-quote is-quote-char (or (and in-quote (not is-quote-char ))
                                      (and (not in-quote) is-quote-char)))
          (in-space (is-space-char char)
                    (and (not in-quote) (is-space-char char)))
          (was-in-comment nil in-comment)
          (in-comment is-comment-char
                      (or in-comment (and (not in-quote) is-comment-char)))
          (was-in-field nil in-field)
          (in-field (and (not in-space) (not in-comment))
                    (and (not in-space) (not in-comment))))
         ;;((= length (1+ index)) (reverse fields))
         (nil nil)
      ;;process
      (if (and was-in-field (not in-field)) ; end of field
          (push (subseq line start index) fields)
          (if (or (and in-field (not was-in-field)) ;start of field
                  (and in-comment (not was-in-comment)));start of comment
              (setf start index)))
      (when (= index (1- length))
        (and in-field (push (subseq line start (1+ index)) fields))
        (return-from get-fields
          (values (reverse fields)
                  (and in-comment (subseq line start (1+ index)))))))))

(defun is-space-char (char)
  (or (char= char #\space)
      (char= char #\tab)))
(in-package :aas-misc)

(defvar *line-no* )

(defmethod process-csv-file ((pathname pathname) callback)
  "see process-csv-file"
  (let ((*line-no* 0))
    (with-open-file (s pathname :direction :input
                       :if-does-not-exist :error)
      (process-csv-file s callback))))

(defmethod process-csv-file ((s stream) callback)
  "call back has to return true for reading to continue"
  (loop until (not (peek-char nil s nil nil)) do
       (unless (funcall callback (incf *line-no*) (fare-csv:read-csv-line s))
         (return-from process-csv-file nil)))
  t)
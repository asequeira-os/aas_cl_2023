(in-package :aas-rpc)

(defparameter +MAX-JSON-STRING-LENGTH+ (* 1 1024 1024));; 1 MB
(defclass aas-json-format () ())
(defvar +json-format+ (make-instance 'aas-json-format))

(defun json-begin-object (stream)
  (princ #\{ stream))

(defun json-end-object (stream)
  (princ #\} stream))

(defgeneric json-print-primitive (obj stream))

(defmethod json-print-primitive ((obj string) stream)
  ;;todo 0 newline in string needs to be changed to \n in json?
  ;;newline from the CL stack trace was causing issue in JSON.parse in browser
  (let ((obj (remove #\Newline obj)))
    (prin1 obj stream)))

(defmethod json-print-primitive ((obj number) stream)
  (princ obj stream))

;;todo 5 json field name - instead of symbol downcase try invert
(defmethod json-print-primitive ((obj symbol) stream)
  (prin1 (string-downcase (symbol-name obj)) stream))

(defun json-print-attribute (name stream)
  (json-print-primitive name stream)
  (princ #\: stream))


(defun json-read-string (stream)
  (peek-char t stream) ;;skip white space
  (unless (char= #\" (read-char stream))
    (error "expected start of string \" not found"))
  (let ((slash nil)
        (count 0))
    (with-output-to-string (s)
      (do ((char (peek-char nil stream) (peek-char nil stream)))
          ((and (not slash) (char= #\" char)))
        (incf count)
        (when (> count +MAX-JSON-STRING-LENGTH+)
          (error "json string too long"))
        (if slash ;;handle escapes
            (json-decode-escape char stream s)
            (progn
              (read-char stream)
              (unless (char= #\\ char)
                (princ char s))))
        (setf slash (if slash nil (char= #\\ char))))
      ;;read last "
      (read-char stream))))

(defun json-decode-escape (char ip-stream op-stream)
  (declare (ignorable ip-stream))
  (if (char= #\u char)
      (princ (decode-json-unicode ip-stream) op-stream)
      (princ (read-char ip-stream) op-stream)))

(defun decode-json-unicode (ip)
  (assert (eql #\u (read-char ip)))
  (let ((s (with-output-to-string (ts)
             (loop for i from 1 to 4 do
                  (princ (read-char ip) ts)))))
    (code-char (parse-integer s :radix 16))))

(defun json-read-integer (stream)
  (let ((first-char (peek-char t stream)))
    (if (or (equal #\" first-char)
            (equal #\' first-char))
        (let ((sv (json-read-string stream)))
          (parse-integer sv))
        (parse-integer
         (with-output-to-string (s)
           (when (or (char= #\+ first-char) (char= #\- first-char))
             (princ (read-char stream) s))
           (unless (digit-char-p (peek-char t stream nil #\$))
             (error "no digits"))
           (util:while (digit-char-p (peek-char t stream nil #\$))
                       (princ (read-char stream) s)))))))

(defun json-read-boolean (stream)
  (parse-json-boolean
   (with-output-to-string (s)
     (util:while (alpha-char-p (peek-char t stream nil #\$))
                 (princ (read-char stream) s)))))

(defun parse-json-boolean (string)
  (with-hashed-identity (:test #'equal)
    (case string
      ("true" t)
      ("false" nil)
      (t (error "unknown json boolean value ~A" string)))))

(defun skip-optional-comma (stream)
  (when (equal #\, (peek-char t stream))
    (read-char stream)))

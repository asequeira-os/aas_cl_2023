(in-package :aas-cl)

(defun decimal-from-stream  (stream)
  (let ((first (peek-char t stream)))
    (unless (or (digit-char-p first) (member first '(#\+ #\- #\.)))
      (error "decimal can not start with ~A" first))
    (let ((seen-dot (eql first #\.)))
      (let ((str (with-output-to-string (out)
                   (write-char (read-char stream) out)
                   (loop for c = (read-char stream nil)
                      while (and c (or (digit-char-p c)))
                      do (write-char c out)
                      finally (when c (unread-char c stream)))
                   (when (and (not seen-dot) (eql (peek-char t stream) #\.))
                     (write-char (read-char stream) out)
                     (loop for c = (read-char stream nil)
                        while (and c (or (digit-char-p c)))
                        do (write-char c out)
                        finally (when c (unread-char c stream)))))))
        (wu-decimal:parse-decimal str)))))

(deftype decimal () '(or integer wu-decimal:decimal))

(defun decimal->string (decimal)
  (format nil "~/wu-decimal:F/" decimal))

(defun parse-decimal (string)
  (wu-decimal:parse-decimal string))

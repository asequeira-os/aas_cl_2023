(in-package :aas-http-client)

(defun get-http-url (url)
  (values-list (multiple-value-list (drakma:http-request url))))


(defun filled-http-link (protocol host url &rest args)
  "url should be i18n fillable format pattern"
  (let ((encoded (mapcar #'url-encode args)))
    (let ((url-filled (apply #'format nil url encoded)))
      (concatenate 'string
                   protocol "://"
                   host
                   url-filled))))

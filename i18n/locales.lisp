(in-package :i18n)

(defvar *locales-defined* nil)

(unless *locales-defined*

  (make-locale *english*   *usa*      nil)
  (make-locale *english*   *canada*   "en-US")
  (make-locale *french*    *france*   "en-US")
  (make-locale *french*    *canada*   "fr-FR")
  (make-locale *english*   *uk*       "en-US")
  (make-locale *spanish*   *usa*      "es-US")

  (setf *locales-defined* t))

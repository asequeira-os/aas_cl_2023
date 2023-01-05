(in-package :aas-cl)

(defvar *inhibit-backtrace* nil)

(defun print-backtrace (error &optional (inhibit-backtrace *inhibit-backtrace*))
  (if inhibit-backtrace
      "stack trace inhibited"
      (trivial-backtrace:print-backtrace error :output nil)))

(in-package :aas-rpc-test)

(define-constant +test-app+ "dummy test app")

(def-rpc string func1 (:anonymous t :post-only t :application +test-app+)
    (arg1 string arg2 string)
  (with-output-to-string (op)
    (format op "arg1=~A arg2=~A" arg1 arg2)))

(def-rpc string func2 (:anonymous t :application +test-app+) ()
  "hello func2")

(def-rpc integer rpc-test-adder (:anonymous t :application +test-app+)
    (x string y string)
  (+ (parse-integer x) (parse-integer y)))

(def-rpc string rpc-test-2 (:anonymous t :application +test-app+)
    (x integer y string z (or null integer))
  (declare (ignorable z))
  (format nil "~A ~A" (+ x 12) y))

(defparameter +test-error-string+ "qwerty me test error")

(def-rpc string error-thrower (:anonymous t :application +test-app+) ()
  (error +test-error-string+))

(def-rpc string bool-arg-rpc (:anonymous t :application +test-app+)
    (b1 boolean s1 string s2 string)
  (assert (or (null b1) (eq t b1)))
  (if b1 s1 s2))

(def-rpc boolean bool-resp-rpc (:anonymous t :application +test-app+)
    (b1 boolean s1 string s2 string)
  (assert (or (null b1) (eq t b1)))
  (if b1
      (string-equal s1 "s1 me")
      (string-equal s2 "s2 he")))

(def-rpc string blow-safety (:anonymous t :application +test-app+)
    (counter integer)
  (blow-safety-help counter))

(defun blow-safety-help (counter)
  (let ((*optimize-local-calls* nil))
    (aas-rpc:call-remote-anon "localhost" #'blow-safety (1+ counter))))

(def-rpc (vector string) vector-rpc (:anonymous t :application +test-app+)
    (in (vector string))
  (reverse in) )

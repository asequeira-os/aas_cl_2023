(defpackage :aas-cl
  (:use :common-lisp ))

(in-package :aas-cl)

(export '(time-ex
          *cl-package*
          partition
          guid
          guid-string
          get-hash-keys
          mp-make-lock mp-acquire-lock mp-release-lock
          mp-with-lock mp-make-thread mp-thread-local
          aas-random
          define-constant
          clean-up-spaces
          build-string
          build-symbol
          build-symbol-in-package
          find-symbol-in-package
          keyword-from-symbol
          struct-symbol-p
          class-symbol-p
          positional-format
          truncate-seq
          decimal
          decimal->string
          parse-decimal
          decimal-from-stream
          flatten
          singleton
          print-backtrace *inhibit-backtrace*
          xor
          def-comparison))

;; ;;stuff from cl-op cl-op.hof
;; (export '(op op*
;;           flip
;;           disjoin
;;           conjoin
;;           compose))

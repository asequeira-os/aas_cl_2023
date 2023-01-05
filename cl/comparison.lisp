(in-package :aas-cl)

(defmacro time-ex ((var) &body body)
  "evaluate body
set var to time taken in seconds
return  values from body"
  (let ((t1 (gensym))
        (t2 (gensym))
        (body-values (gensym)))
    `(let* ((,t1 (get-internal-real-time))
            (,body-values (multiple-value-list ,@body))
            (,t2 (get-internal-real-time)))
       (setf ,var
             (coerce (/ (- ,t2 ,t1) internal-time-units-per-second)
                     'double-float))
       (values-list ,body-values))))


(let ((re (cl-ppcre:create-scanner "(\\s|\\t)+")))
  (defun clean-up-spaces (s)
    (if (null s)
        s
        (string-trim " " (cl-ppcre:regex-replace-all re s " ")))))

(defun build-string (&rest args)
  (with-output-to-string (s)
    (dolist (p args) (princ p s))))

(defun build-symbol(&rest args)
  (values (intern (util:match-readtable-case
                   (apply #'build-string args)))))

(defun build-symbol-in-package(package &rest args)
  (values (intern (util:match-readtable-case
                   (apply #'build-string args)) package)))

(defun find-symbol-in-package(package &rest args)
  (find-symbol (util:match-readtable-case
                (apply #'build-string args)) package))

(defun keyword-from-symbol (symbol)
  (intern (util:match-readtable-case symbol)
          (find-package :keyword)))

(defmacro def-dual-comparison (name type comp1 comp2)
  `(defun ,name (arg1 &rest args)
     (unless (typep arg1 ,type)
       (error (make-condition 'type-error :datum arg1 :expected-type ,type)))
     (when (null args)
       (return-from ,name t))
     (let ((prev arg1))
       (loop for next in args do
            (progn
              (unless (typep next ,type)
                (error (make-condition 'type-error
                                       :datum next :expected-type ,type)))
              ,(if comp2
                     `(if (or (,comp1 prev next) (,comp2 prev next))
                          (setf prev next)
                          (return-from ,name nil))
                     `(if (,comp1 prev next)
                          (setf prev next)
                          (return-from ,name nil)))))
       t)))


;;I am not going to document this, see it's usage.
;;It will be tested as a result of testing of it's usage.
(defmacro def-comparison (symb-arg type-arg >macro <macro =macro)
  `(progn
     ,(let* ((lt `,<macro)
             (gt `,>macro)
             (=m `,=macro)
             (symb `,symb-arg)
             (type `,type-arg)
             (=fname (build-symbol symb "="))
             (<fname (build-symbol symb "<"))
             (>fname (build-symbol symb ">"))
             (/=fname (build-symbol symb "/="))
             (>=fname (build-symbol symb ">="))
             (<=fname (build-symbol symb "<=")))
            `(progn
               (export '(,=fname ,<fname ,>fname ,/=fname ,>=fname ,<=fname))
               (def-dual-comparison ,<fname ,type ,lt nil)
               (def-dual-comparison ,=fname ,type ,=m nil)
               (def-dual-comparison ,>fname ,type ,gt nil)
               ;;not defining /= it's expensive
               ;;if (not op) works for the user code, use that
               (def-dual-comparison ,>=fname ,type ,gt ,=m)
               (def-dual-comparison ,<=fname ,type ,lt ,=m)))))

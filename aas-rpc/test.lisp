(in-package :aas-rpc-test)

(deftest all-tests
  (let ((*secure-transport* t))
    (and (basic-json-tests)
         (test-remote-error)
         (basic-json-tests-2)
         (json-string-tests)
         (json-intger-tests)
         (rpc-base-tests)
         (json-rpc-tests)
         (host-trust-tests)
         (rpc-error-tests)
         (blow-fuse-test)
         (vector-rpc-test)
         (rpc-struct-tests))))

(deftest basic-json-tests
  (dolist (s (list "" "X" "x" "ad" "1" "-12"  "\\\""))
    (let ((qs (concatenate 'string "\"" s "\"")))
      (with-input-from-string (stream qs)
        (let ((result (json-read-string stream)))
          (is (equal qs (format nil "~S" result)))
          (is (equal qs
                     (with-output-to-string (os)
                       (json-print-primitive result os))))))))
  (let ((large (make-string +MAX-JSON-STRING-LENGTH+
                            :initial-element #\a)))
    (is (equal large (with-input-from-string
                         (stream
                          (concatenate 'string "\"" large "\""))
                       (json-read-string stream))))
    (verify-error error (with-input-from-string
                            (stream
                             (concatenate 'string "\"xxx" large "\""))
                          (json-read-string stream)))))

(deftest basic-json-tests-2
  (let ((oklist (list "null" " null" " null " "null}" "null," "null]"))
        (remain (list nil nil nil #\} #\, #\])))
    (map 'list (lambda (ok remain)
                 (with-input-from-string (stream ok)
                   (let ((parsed
                          (deserialize-null +json-format+ stream)))
                     (is-not parsed)
                     (if (characterp remain)
                         (is (char= remain (peek-char t stream nil nil)))
                         (is (null (peek-char t stream nil nil)))))))

         oklist remain)))


(deftest rpc-base-tests
  (let ((*secure-transport* t)
        (*post-request* t))
    (and (valid-test) (invalid-test))))

(deftest valid-test
  (let ((rpc-meta
         (get-rpc-meta-data "aas-rpc-test" "func2")))
    (is rpc-meta)
    (let ((resp (call-rpc-function "aas-rpc-test" "func2" nil)))
      (is (string-equal "hello func2"
                        resp))))
  (let ((rpc-meta
         (get-rpc-meta-data "aas-rpc-test" "func1")))
    (is rpc-meta)
    (let ((data (call-rpc-function "aas-rpc-test" "func1"
                                   '((arg1 . "v1") (arg2 . "v2")))))
      (is (string-equal "arg1=v1 arg2=v2" data)))))

(deftest invalid-test
  (multiple-value-bind
        (func args)
      (get-rpc-meta-data "cl" "qqqq")
    (is-not args)
    (is-not func))
  (multiple-value-bind
        (func args)
      (get-rpc-meta-data "qqq" "+")
    (is-not args)
    (is-not func))
  (multiple-value-bind
        (func args)
      (get-rpc-meta-data "qqq" "yyy")
    (is-not args)
    (is-not func))
  (multiple-value-bind
        (func args)
      (get-rpc-meta-data "cl" "+")
    (is-not args)
    (is-not func)))

(deftest host-trust-tests
  (let ((now (get-universal-time))
        (pending-count (hash-table-count *pending-tokens*)))
    (let ((te-ok
           (aas-rpc:establish-trust (host:my-hostname))))
      (is te-ok)
      (is (typep te-ok 'string))
      (is (< 20 (length te-ok))))
    (is (= pending-count (hash-table-count *pending-tokens*)))
    (unless (gethash (host:my-hostname) *my-tokens*)
      (sleep 5))
    (is (and (gethash (host:my-hostname) *received-tokens*)
             (gethash (host:my-hostname) *my-tokens*)))
    (is (>= (cdr (gethash (host:my-hostname) *my-tokens*))
            (cdr (gethash (host:my-hostname) *received-tokens*))))
    (is (> 5 (- (cdr (gethash (host:my-hostname) *received-tokens*))
                (cdr (gethash (host:my-hostname) *my-tokens*)))))
    (is (> 2 (- now (cdr (gethash (host:my-hostname) *my-tokens*)))))))

(deftest rpc-error-tests
  (handler-case
      (progn
        (call-remote-anon (host:my-hostname) #'error-thrower)
        (is nil))
    (rpc-error (e)
      (let ((cause (rpc-error-cause e)))
        (is (string-equal (json-rpc-2-error-message cause) +test-error-string+))
        (is (= -32000 (json-rpc-2-error-code  cause)))))))


(deftest blow-fuse-test
  (verify-error rpc-error
      (blow-safety-help 0)))

(deftest vector-rpc-test
  (let ((iv #("xxx" "yyy")))
    (let ((rv (call-remote-anon (host:my-hostname) #'vector-rpc iv)))
      (is (equalp (reverse iv) rv)))))

(deftest json-string-tests
  (let ((vin  #("" "a" "ab" "a " " b" "\\x" "\\u0065" "foo\\bXYZ\\uffe5bar"))
        (vout #("" "a" "ab" "a " " b" "x"   "e"       "foobXYZ￥bar")))
    (flet ((trip (str)
             (with-input-from-string (in (concatenate 'string "\"" str "\""))
               (json-read-string in))))
      (loop for i from 0 to (1- (length vin)) do
           (let ((in (aref vout i))
                 (out (trip (aref vin i))))
             (is (equal in out)))))))


(deftest json-intger-tests
  (let ((okv (list 0 1 2 9 10 11 19 99 100 123456)))
    (dolist (i okv)
      (let ((s (format nil "~A" i)))
        (with-input-from-string (stream s)
          (let ((jv (json-read-integer stream)))
            (is (= jv i)))))
      (let ((s (format nil "-~A" i)))
        (with-input-from-string (stream s)
          (let ((jv (json-read-integer stream)))
            (is (= jv (* -1 i))))))
      (let ((s (format nil "+~A" i)))
        (with-input-from-string (stream s)
          (let ((jv (json-read-integer stream)))
            (is (= jv i)))))
      (let ((s (format nil "\"~A\"" i)))
        (with-input-from-string (stream s)
          (let ((jv (json-read-integer stream)))
            (is (= jv i)))))
      (let ((s (format nil "\"-~A\"" i)))
        (with-input-from-string (stream s)
          (let ((jv (json-read-integer stream)))
            (is (= jv (* -1 i))))))
      (let ((s (format nil "\"+~A\"" i)))
        (with-input-from-string (stream s)
          (let ((jv (json-read-integer stream)))
            (is (= jv i)))))))
  ;;trailing bad characters are allowed,
  ;;they are caught by next token parser when invalid
  (let ((bv (list "" "x" "+x" "+" "-" "-x")))
    (dolist (b bv)
      (verify-error error
          (with-input-from-string (stream b)
            (json-read-integer stream)))
      (verify-error error
          (with-input-from-string (stream (concatenate 'string "\"" b "\""))
            (json-read-integer stream))))))


(def-rpc string test-rpc-remote-err (:anonymous t :application +test-app+)
    (count integer)
  (unless (< 0 count 10)
    ;;something wrong in test
    (break))
  (when (= 4 count)
    (error "see my message at count 3"))
  (incf count)
  (json-rpc-2-call-http
   (host:my-hostname) nil nil :post "aas-rpc-test" "test-rpc-remote-err" count))

(deftest test-remote-error
  (verify-error rpc-error
      (test-rpc-remote-err :count 1)
    (let* ((cause (rpc-error-cause aseq-test::e))
           (message (json-rpc-2-error-message cause)))
      (is (equal "-- -- see my message at count 3" message)))))

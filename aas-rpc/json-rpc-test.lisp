(in-package :aas-rpc-test)

(deftest json-rpc-tests
  (and (simple-json-rpc-test) (zero-arg-rpc-test)
       (bool-rpc-test)
       (email-url-test)))

(deftest simple-json-rpc-test
  (verify-error error
      (json-rpc-2-call-http
       "localhost" nil nil :get "aas-rpc-test" "func1" "foo" "bar"))
  (let ((result (json-rpc-2-call-http
                 "localhost" nil nil :post "aas-rpc-test" "func1" "foo" "bar"))
        (expect "arg1=foo arg2=bar"))
    (is (equal result expect))))

(deftest zero-arg-rpc-test
  (let ((result (json-rpc-2-call-http
                 "localhost" nil nil :post "aas-rpc-test" "func2")))
    (is (equal "hello func2" result))))

(deftest email-url-test
  "expected use for email url generation"
  (let ((result (ctor-call-remote-url
                 "localhost" #'func1
                 "xxxhost" "foo" "bar"))
        (expected "https://localhost:8083/xapi/v1/json-rpc-2?request=%7B%22jsonrpc%22%3A%222.0%22"))
    (is (alexandria:starts-with-subseq expected result))))


(deftest bool-rpc-test
  (let ((tdl '(
               (t "s1 me" "s2 he" "s1 me" t)
               (t "s1 mex" "s2 he" "s1 mex" nil)
               ('zz "s1 me" "s2 he" "s1 me" t)
               ("qqq" "s1 mex" "s2 he" "s1 mex" nil)
               (nil "s1 me" "s2 he" "s2 he" t)
               (nil "s1 xe" "s2 xxe" "s2 xxe" nil)
               )))
    (dolist (td tdl)
      (let ((br (json-rpc-2-call-http
                 "localhost" nil nil :post "aas-rpc-test" "bool-resp-rpc"
                 (first td) (second td) (third td)))
            (sr (json-rpc-2-call-http
                 "localhost" nil nil :post "aas-rpc-test" "bool-arg-rpc"
                 (first td) (second td) (third td))))
        (is (equal sr (fourth td)))
        (is (eq br (fifth td)))))))



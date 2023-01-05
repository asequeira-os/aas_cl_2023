(in-package :aas-rpc-test)

(deftest rpc-struct-tests
  (let ((*secure-transport* t)
        (*post-request* t))
    (and (rpc-struct-out-test) (rpc-struct-in/out-test)
         (batch-test)
         (rpc-custom-ser-test)
         (rpc-decimal-test)
         (rpc-read-only-test)
         (rpc-struct-null-compund-test) (test-rpc-complex-test))))

(def-rpc-struct foo-9
    (a nil :type (or null integer))
  (b "qq" :type string))

(def-rpc-struct foo-8
    (m 0 :type integer))

(def-rpc-struct foo-10
    (x "" :type string)
  (y 0 :type integer)
  (z nil :type foo-9)
  (w nil :type (vector foo-8 *)))

(def-rpc-struct comp-1
    (x nil :type string)
  (y nil :type (vector integer *))
  (z nil :type (or null integer)))

(deftest compound-test-2
  (let ((s1 (make-comp-1 :x "foo" :y #(53 65 72) :z nil))
        (s2 (make-comp-1 :x "foo" :y #(53 65 72) :z 458)))
    (let ((json1 (with-output-to-string (stream)
                   (sout-object +json-format+ stream 'comp-1 s1)))
          (json2 (with-output-to-string (stream)
                   (sout-object +json-format+ stream 'comp-1 s2))))
      (is (not (equal json1 json2)))
      (is (< 10 (length json1)))
      (is (< 10 (length json2)))
      (let* ((fn (gethash 'comp-1 *deserializers*))
             (s1r (funcall fn +json-format+
                           (make-string-input-stream  json1)
                           'comp-1))
             (s2r (funcall fn +json-format+
                           (make-string-input-stream  json2)
                           'comp-1)))
        (is (equalp s1 s1r))
        (is (equalp s2 s2r))))))

(defparameter *t-9* (make-foo-9 :a 23 :b "qqq"))
(defparameter *t-9-null* (make-foo-9  :b "qqq"))
(defparameter *t-10*
  (make-foo-10 :x "x me" :y 765 :z *t-9*
               :w (vector (make-foo-8 :m 77) (make-foo-8 :m 88))))

(deftest rpc-struct-out-test
  (let ((result
         (with-output-to-string (stream)
           (sout-object +json-format+ stream 'foo-10 *t-10*)))
        (expected "{\"x\":\"x me\",\"y\":765,\"z\":{\"a\":23,\"b\":\"qqq\"},\"w\":[{\"m\":77},{\"m\":88}]}"))
    (is (equal expected result))))

(deftest rpc-struct-in/out-test
  (let* ((orig "{\"x\":\"x me\",\"y\":765,\"z\":{\"a\":23,\"b\":\"qqq\"},\"w\":[{\"m\":77},{\"m\":88}]}")
         (round-trip (with-output-to-string (stream)
                       (sout-object
                        +json-format+ stream 'foo-10
                        (funcall (gethash 'foo-10 *deserializers*)
                                 +json-format+
                                 (make-string-input-stream orig)
                                 'foo-10)))))
    (is (equal orig round-trip))
    (let* ((double (concatenate 'string orig orig))
           (stream (make-string-input-stream double))
           (fn (gethash 'foo-10 *deserializers*))
           (ob1 (funcall fn +json-format+ stream 'foo-10))
           (ob2 (funcall fn +json-format+ stream 'foo-10)))
      (is (equalp ob1 ob2)))))


(deftest rpc-struct-null-compund-test
  (is (compound-test-2))
  (let* ((result
          (with-output-to-string (stream)
            (sout-object +json-format+ stream 'foo-9 *t-9-null*)))
         (expect "{\"a\":null,\"b\":\"qqq\"}"))
    (is (equal result expect))
    (let* ((fn (gethash 'foo-9 *deserializers*))
           (struct (funcall fn +json-format+
                            (make-string-input-stream  expect)
                            'foo-9)))
      (let ((round-trip (with-output-to-string (stream)
                          (sout-object
                           +json-format+ stream 'foo-9 struct))))
        (is (equal expect round-trip))))))

(def-rpc foo-9 test-rpc-complex (:anonymous t :application +test-app+)
    (arg1 foo-10 arg2 foo-8)
  (make-foo-9 :a (foo-10-y arg1) :b (format nil "from arg2 ~A"(foo-8-m arg2))))

(deftest test-rpc-complex-test
  (let ((result (json-rpc-2-server nil nil
                                   "aas-rpc-test:test-rpc-complex"
                                   "{\"jsonrpc\": \"2.0\", \"method\": \"aas-rpc-test:\", \"params\": {\"arg1\": {\"x\":\"xstr\",\"y\":123,\"z\":{\"a\":null,\"b\":\"hellob\"},\"w\":[]} , \"arg2\": {\"m\":24}}, \"id\": 3}")))
    ;;(print result)
    (is (equal "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"a\":123,\"b\":\"from arg2 24\"}}"
               result))))

(def-rpc-struct rpc-custom-test-struct
    (x 0 :skip-rpc :type integer)
  (y 0 :skip-rpc)
  (z "" :type string))


(defmethod deserialize-after ((object rpc-custom-test-struct))
  (let* ((str (rpc-custom-test-struct-z object))
         (xy (split-sequence:split-sequence #\, str))
         (x (parse-integer (first xy)))
         (y (parse-integer (second xy))))
    (setf (rpc-custom-test-struct-x object) x)
    (setf (rpc-custom-test-struct-y object) y))
  object)

(defmethod sout-object :before (ser-type stream type (object rpc-custom-test-struct))
  (declare (ignorable ser-type stream type))
  (setf (rpc-custom-test-struct-z object)
        (format nil "~A,~A"
                (rpc-custom-test-struct-x object)
                (rpc-custom-test-struct-y object))))

(deftest rpc-custom-ser-test
  (let ((orig (make-rpc-custom-test-struct :x 4 :y 3)))
    (let ((ser-out
           (with-output-to-string (stream)
             (sout-object +json-format+ stream 'rpc-custom-test-struct orig)))
          )
      (is (equal ser-out  "{\"z\":\"4,3\"}"))
      (let ((struct (funcall
                     (gethash 'rpc-custom-test-struct *deserializers*)
                     +json-format+
                     (make-string-input-stream  ser-out)
                     'rpc-custom-test-struct)))
        (is (= 3 (rpc-custom-test-struct-y struct)))
        (is (= 4 (rpc-custom-test-struct-x struct)))))))

(deftest batch-test
  (let ((requests (list)))
    (push (list #'func1 "f1s1" "f1s2") requests)
    (push (list #'func2 ) requests)
    (push (list #'rpc-test-2 10 "qq" nil) requests)
    (push (list #'test-rpc-complex
                (make-foo-10 :x "qq" :y 23 :z (make-foo-9) :w #())
                (make-foo-8 :m 49)) requests)
    (multiple-value-bind (resp-list success)
        (call-remote-batch
         (if aseq-test:*multi-host*
             (error "todo 4 need multi host test fix hardcoded name")
             (host:my-hostname))
         (reverse requests))
      (is success)
      (is (= (length resp-list) (length requests))))))

(def-rpc-struct decimal-1
    (x nil :type string)
  (y nil :type decimal))

(def-rpc decimal rpc-decimal-test-1 (:anonymous t :application +test-app+)
    (s1 decimal-1 d2 integer)
  (* d2 (decimal-1-y s1)))

(deftest rpc-decimal-test
  (let ((in (make-decimal-1 :x "fooo" :y  (parse-decimal "-10.25"))))
    (let ((resp (aas-rpc:call-remote-anon "localhost"
                                          #'rpc-decimal-test-1
                                          in 13)))
      (is (equal (* 13 -1025/100) resp)))))

(def-rpc-struct read-only
    (x nil :type string)
  (y nil :type integer :read-only t))

(def-rpc read-only  read-only-test-1 (:anonymous t :application +test-app+)
    (i1 integer ro read-only)
  (make-read-only :x (read-only-x ro)
                  :y (+ i1 (read-only-y ro))))

(deftest rpc-read-only-test
  (let ((in (make-read-only :x "fooo" :y  28)))
    (let ((resp (call-remote-anon "localhost"
                                  #'read-only-test-1
                                  10 in)))
      (is (equalp "fooo" (read-only-x resp)))
      (is (= (+ 10 28) (read-only-y resp))))))

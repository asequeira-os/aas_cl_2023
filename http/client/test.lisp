(in-package :aas-http-client-test)

(deftest all-tests
  (is (equal
       "https://www.foo.com/h/confirm-email?login=hj%25%5E&guid=gtr+r$%23"
       (filled-http-link "https"
                         "www.foo.com"
                         "/h/confirm-email?login=~0@*~A&guid=~1@*~A"
                         "hj%^" "gtr r$#")))

  (multiple-value-bind (body code headers uri)
      (get-http-url "http://www.stanford.edu/")
    (is (= 200 code))
    (is (listp headers))
    (is uri)
    (is (search "Stanford University" body))))

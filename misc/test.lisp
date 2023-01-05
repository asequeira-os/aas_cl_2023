(in-package :aas-misc)

(aseq-test:deftest all-tests
  (and (csv-test) (file-test-1) (aas-misc-queue-1) (amt-fmt-test)
       (group-by-test)
       (aas-misc-test::dll-tests) (aas-misc-test::utf8-tests)
       (aas-misc-test::lru-tests)))

(aseq-test:deftest file-test-1
  (let ((testdir (aas-build:get-env "LISP_TEMP_DIR")))
    (let ((fulldir (cl-fad:list-directory testdir))
          (subdirs (sub-dirs testdir))
          (files (files-in-dir testdir)))
      (aseq-test:is (= (length fulldir)
                       (+ (length subdirs) (length files)))))))


(aseq-test:deftest csv-test
  (let ((aas-misc::*line-no* 0)
        (result (list))
        (ln 1))
    (with-input-from-string
        (s
         (concatenate 'string
                      "123,456,\"aaa\",,\"aaa" #(#\Return #\Newline)
                      "qqq\"
                      "
                      "\"785\",wwww
                  aaaa,bbb,\" cc \""))
      (process-csv-file
       s
       (lambda (line-num fields)
         (aseq-test:is (= ln line-num))
         (incf ln)
         (push fields result))))
    (let ((result (nreverse result))
          (expected (list
                     (list "123" "456" "aaa" ""
                           (concatenate 'string "aaa" #(#\Return #\Newline) "qqq"))
                     (list "785" "wwww")
                     (list "aaaa" "bbb" " cc "))))
      (aseq-test:is (equalp result expected))))
  ;;2nd test
  (let ((max-lines 2)
        (got-lines 0)
        (aas-misc::*line-no* 0))
    (with-input-from-string
        (s "1,2
3,4
5,6
7,8")
      (process-csv-file
       s
       (lambda (line-num fields)
         (declare (ignorable  fields))
         (incf got-lines)
         (< line-num max-lines)))
      (aseq-test:is (= 2 got-lines)))))


(aseq-test:deftest amt-fmt-test
  (aseq-test:is
   (string= "+0.00" (amount-format 0 t #\, #\. 2)))
  (aseq-test:is
   (string= "+0.000" (amount-format 0 t #\, #\. 3)))
  (aseq-test:is
   (string= "0" (amount-format 0 nil #\, #\. 0)))
  (aseq-test:is
   (string= "0.00" (amount-format 0 nil #\, #\. 2)))
  (aseq-test:is
   (string= "+123,456.65" (amount-format 123456.654321d0 t #\, #\. 2)))
  (aseq-test:is
   (string= "-12,345,678.65" (amount-format -12345678.654321d0 t #\, #\. 2)))
  (aseq-test:is
   (string= "12,345,678.655" (amount-format 12345678.654921d0 nil #\, #\. 3)))
  (aseq-test:is
   (string= "-12,345,678.65" (amount-format -12345678.654321d0 nil #\, #\. 2)))
  (aseq-test:is
   (string= "-12.345.678,65" (amount-format -12345678.654321d0 t #\. #\, 2))))


(aseq-test:deftest group-by-test
  (let ((data (list '(1 (2 3)) '(1 (3 4)) '(2 (5 6)) '(1 (7 9)) '(5))))
    (let ((gd (group-by data :key #'car :test #'eql )))
      (aseq-test:is (equalp gd (list '(1 (1 (2 3)) (1 (3 4)) (1 (7 9)))
                                     '(2 (2 (5 6))) '(5 (5))))))))

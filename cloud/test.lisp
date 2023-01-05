;;-*- coding: utf-8 -*-
(in-package :cloud-test)

(set-db-name-prefix "test" +test-app+)
(set-application-db-count +test-app+ 3)
(set-pdb-host-range 0 2 (host:my-hostname) +test-app+)
(distribute-vdb-over-pdb  +test-app+)
(init +test-app+)


(deftest all-tests
  (and (db-number-test) (rights-test) (errors-i18n-test)))

(deftest db-number-test
  (with-application +test-app+
    (loop for i from 0 to (1- db-base-test::+max-test-db-num+) do
         (let ((host (get-vdb-host i)))
           (is (not (null host)))
           (is (stringp host))))
    (let ((data '(("" . 0)
                  ("作者被禁止或删除内容自动屏蔽" . 2)
                  ("hdasjhwu@hjdshjsa^%&&45$#%kjk" . 7))))
      (dolist (data data)
        (let* ((in (car data))
               (expect (cdr data))
               (vdb (key-to-vdb in))
               (result vdb))
          (is (= expect result)))))))

(define-rights rights read write admin)

(deftest rights-test
  (let ((r1 (create-rights :read t))
        (r2 (create-rights :admin t))
        (r3 (create-rights :read t :admin t)))
    (is (= 1 (rights-dbv r1)))
    (is (= 4 (rights-dbv r2)))
    (is (= 5 (rights-dbv r3)))
    (is (= 4 (rights-to-integer r2)))
    (is (= 5 (rights-to-integer r3)))
    (is (rights-read r1))
    (is (rights-admin r2))
    (is (rights-read r3))
    (is (rights-admin r3))
    (is (equalp (rights-or r1 r2) r3))
    (is (equalp (rights-and r1 r1) r1))
    (is (equalp (rights-and r3 r1) r1))
    (is (zerop (rights-dbv (rights-and r1 r3 r2))))
    (is-not (rights-admin r1))
    (is-not (rights-read r2))
    (is-not (rights-write r1))
    (is-not (rights-write r2))
    (is-not (rights-write r3))
    (setf (rights-admin r1) t)
    (is (rights-admin r1))
    (is (equalp r1 r3))
    (setf (rights-write r1) t)
    (is (= 7 (rights-dbv r1)))
    (setf (rights-read r1) nil)
    (is (= 6 (rights-dbv r1)))
    (is (equalp r3 (create-rights :read t :write nil :admin t)))))

;;check every defined error has i18n text at least in default en-US locale
(deftest errors-i18n-test
  (i18n:with-locale "en-US"
    (maphash (lambda (symbol v)
               (declare (ignorable v))
               (let ((text (i18n:get-text symbol)))
                 (is text)
                 (is-not (zerop (length text)))
                 (is (stringp text))))
             *error-symbols*)))


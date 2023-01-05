(in-package :i18n-test)

(deftest all-tests
  (and (lang-tests)
       (country-tests)
       (locale-tests)
       (text-tests)))

(deftest lang-tests
  (is-not  (zerop (hash-table-count *languages*)))
  t)

(deftest country-tests
  (is-not  (zerop (hash-table-count *countries*)))
  t)

(deftest locale-tests
  (is  (< 3 (hash-table-count *locales*)))
  (with-locale "en-US"
    (is (eq i18n:*locale*
            (i18n::get-locale "en-US"))))
  t)

(deftest text-tests
  (verify-error error (set-text foo "tests"))
  (let ((v1 #("foo 0" "bar 1")))
    (with-locale "en-US"
      (set-text a "got a")
      (set-text b "got b")
      (set-text a-ar v1))
    )
  (with-locale "en-CA"
    (set-text b "got CA b"))
  (with-locale "en-US"
    (let ((v2 (get-text 'a-ar)))
      (is (equal "foo 0" (aref v2 0)))
      (is (equal "bar 1" (aref v2 1))))
    (is (equal "got a" (get-text 'a)))
    (is (equal "got b" (get-text 'b))))
  (with-locale "en-CA"
    (is (equal "got a" (get-text 'a)))
    (is (equal "got CA b" (get-text 'b))))
  (with-locale "en-CA"
    (verify-error error (get-text 'foo)))
  (with-locale "en-US"
    (verify-error error (get-text 'foo))))

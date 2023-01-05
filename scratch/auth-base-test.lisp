;define set of facts to use for unit testing auth-base

#|
(deffacts auth-unit-test-a ()
  (group g11 (name g11)) ;low level groups
  (group g12 (name g12))
  (group g13 (name g13))
  (group g14 (name g14))

  (group g21 (name g21)) ;2nd level groups
  (group g22 (name g22))

  (group g31 (name g31)) ; 3rd level group

  (group gmix (name gmix)); oddball group


  (group-member (group-name g21) (group-member g11))
  (group-member (group-name g21) (group-member g12))
  

  (group-member (group-name g22) (group-member g13))
  (group-member (group-name g22) (group-member g14))

  (group-member (group-name g31) (group-member g21))
  (group-member (group-name g31) (group-member g22))

  (group-member (group-name gmix) (group-member g22))
  (group-member (group-name gmix) (group-member g12))


  )
  
 |#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package "LISA-TEST"))
    (defpackage "LISA-TEST"
      (:use "LISA-LISP"))))

(in-package "LISA-TEST")

(deftemplate t1 () (slot s))
(deftemplate t2 () (slot s))
(deftemplate t3 () (slot s))

(deffacts testfacts ()
  (t1 (s s1))
  (t2 (s s2))
  (t3 (s s3))
  (t1 (s x1)))

(defrule r1 ()
  (t1 (s ?s1))
  =>
  (format t "from rule r1 - we got ~A. ~%" ?s1))


(defrule r2 ()
  (or (t1 )
      (t2 ))
  =>
  (format t "from rule r2 . ~%"))

#|
commenting out cause lisa is missing 'or' now
(defrule r3 ()
  (or (t1 (s s1))
      (t2 (s s2)))
  =>
  (format t "from rule r3 . ~%"))
|#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "doing stuff ~%")
  (reset)
  (run))


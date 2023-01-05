(defpackage :aas-units-test
  (:use :common-lisp :aas-units :it.bese.FiveAM))

(in-package :aas-units-test)

(def-suite :aas-units-test :description "test the aas-units package")

(in-suite :aas-units-test)


(test inits-check
  (is (not (null aas-units::*unit-conv*)))
  (is (not (null aas-units::*units*))))

(test list-units
  (let ((list (list-units)))
    (is (> (length list) 3))
    (is (eq (find :meter list) :meter))
    (is (eq (find :kilo-meter list) :kilo-meter))
    (is (not (null (aas-units::find-conv-% :kilo-meter :mile))))
    (is (= (aas-units:convert 1.0 :kilo-meter :mile) 0.621371192))
    (is (= (aas-units:convert 1.0 :mile :kilo-meter) 1/0.621371192))
    (is (= (funcall (aas-units:convert-func :kilo-meter :mile) 1.0) 0.621371192))
    (is (= (funcall (aas-units:convert-func :mile :kilo-meter) 1.0) 1/0.621371192))))



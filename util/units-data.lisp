(in-package :aas-units)

;;;;define all units

;;;length
;;length metric
(define-unit :meter "m" "meters")
(define-unit :kilo-meter "km" "kilometers")

;;length US
(define-unit :mile "mi" "miles")

;;weight metric
(define-unit :gram  "g" "grams")
(define-unit :kilo-gram  "kg" "kilograms")

;;conversions
;;length
(define-conv :kilo-meter :mile 0.621371192)

(in-package :aas-misc)

(defun amount-format (amount sign-p commachar dotchar decimals)
  "provide a display string for the numeric amount.
sign-p if true a + sign is printed for positive values.
comachar - the thousands separator.
dotchar - the decimals separator.
decimals - the number of digits after the decimals point."
  (with-output-to-string (s)
    (let ((sign (if (minusp amount)
                    #\-
                    (if sign-p #\+ nil))))
      (multiple-value-bind (dollars cents)
          (floor (abs amount) 1)
        (format s "~@[~C~]~,,V:D" sign commachar dollars)
        (if (> decimals 0)
            (let ((cents (round (* cents (expt 10 decimals)))))
              (format s "~C" dotchar)
              (if (zerop cents)
                  (loop for i from 1 to decimals do
                       (format s "0"))
                  (format s "~D" cents))))))))

(defun group-by (data &key key test)
  (group-by:group-by (coerce data 'list) :key key  :test test :value #'identity))

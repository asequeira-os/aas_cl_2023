;;
;; comments for algo below says T is a constant to be initialized
;; to some integer in the range 10 to 40.
;; Got results best as per paper for T=22
;;
;; n is sample size [try with 10, 100, 1000 and see performance]
;;
;; algorithm Z from Vitter 1985 pp. 54
;;
;;     {Make the first n records candidates for the sample}
;;     for j := 0 to n - 1 do READ-NEXT-RECORD(C [j]);
;;     t := n; {t is the number of records processed so far}
;;     {Process records using the method of Algorithm X until t is large enough}
;;     thresh := T * n;
;;     num := 0; {num is equal to t - n}
;;     while not eof and (t ≤ thresh) do
;;         begin
;;         V := RANDOM( ); {Generate V}
;;         Ρ := 0;
;;         t := t + 1;
;;         num := num + 1;
;;         quot := num/t;
;;         while quot > V do   {Find min Ρ satisfying (4.1)}
;;             begin
;;             Ρ := Ρ + 1;
;;             t := t + 1;
;;             num := num + 1;
;;             quot := (quot * num)/t
;;             end;
;;         SKIP-RECORDS(Ρ); {Skip over the next Ρ records}
;;         if not eof then
;;             begin {Make the next record a candidate, replacing one at random}
;;             Μ := TRUNC(n * RANDOM( )); {Μ is uniform in the range 0 ≤ Μ ≤ n - 1)
;;             READ-NEXT-RECORD(C [Μ] )
;;             end
;;         end;
;;     {Process the rest of the records using the rejection technique}
;;     W := EXP(- LOG(RANDOM( ))/n); {Generate W}
;;     term := t - n + 1; {term is always equal to t - n + 1}
;;     while not eof do
;;         begin
;;         loop
;;             {Generate U and χ}
;;             U := RANDOM( );
;;             χ := t * (W - 1.0); .
;;             Ρ = TRUNC(χ); {Ρ is tentatively set to ⌊χ⌋ }
;;             {Test if U ≤ h(Ρ)/cg(χ) in the manner of (6.3)}
;;             lhs := EXP(LOG(((U * (((t + 1)/term) ^ 2)) * (term + Ρ))/(t + χ))/n);
;;             rhs := (((t + χ)/(term + Ρ)) * term)/t;
;;             if lhs ≤ rhs then
;;                 begin W := rhs/lhs; break loop end;
;;             {Test if U ≤ f(Ρ)/cg(χ)}
;;             y := (((U * (t + 1))/term) * (t + Ρ + 1))/(t + χ);
;;             if n < Ρ then begin denom := t; numer_lim := term + Ρ end
;;             else begin denom := t - n + Ρ; numer_lim := t + 1 end;
;;             for numer := t + Ρ downto numer_lim do
;;                 begin y := (y * numer)/denom; denom := denom - 1 end;
;;             W := EXP(- LOG(RANDOM( ))/n); {Generate W in advance}
;;             if EXP(LOG(y)/n) ≤ (t + χ)/t then break loop
;;         end loop;
;;         SKIP-RECORDS(Ρ); {Skip over the next Ρ records}
;;         if not eof then
;;             begin {Make the next record a candidate, replacing one at random}
;;             M := TRUNC (n * RANDOM( )); {M is uniform in the range 0 ≤ M ≤ n-1}
;;             READ-NEXT-RECORD(C [M])
;;             end,
;;         t := t + Ρ + 1;
;;         term := term + Ρ + 1
;;         end;




(in-package :sampling)

(defconstant TEE-DEFAULT 22 )

(defclass reservoir-z ()
  ((count :initform 0) ;; plays role of 't' in algorithm Z
   (size :initarg :size) ;; plays role of 'n'
   (skip :initform 0) ;; plays role of 'P'
   (num :initform 0) ;; plays role of num
   (vect :initarg :vect) ;; plays role of 'C'
   (tee :initform TEE-DEFAULT :initarg :tee) ;; plays role of 'T'
   (thresh :initarg :thresh) ;; plays role of thresh
   (term :initform 0) ;; plays role of term
   (w :initarg :w) ;; plays role of W
   (rs :initform (make-random-state))))

(defun create-reservoir-z (size &optional (tee TEE-DEFAULT))
  (make-instance 'reservoir-z
                 :size size
                 :thresh (* tee size)
                 ;;W := EXP(- LOG(RANDOM( ))/n)
                 :w (exp (/ (* -1 (log (random 1.0))) size))
                 :vect (make-array size :initial-element nil)))

(defmethod get-sample ((algo reservoir-z))
  (slot-value algo 'vect))

(defmethod process ((algo reservoir-z) datum)
  (with-slots (skip) algo
    (if (not (zerop skip))
        (decf skip) ;; skip logic for algo X as well as Z
        (with-slots (count size) algo
          (if (< count size)
              (progn  ;fill initial elements
                (setf (aref (slot-value algo 'vect) count) datum)
                (incf count))
              (with-slots (thresh rs) algo
                (let ((m (floor (* size (random 1.0 rs)))))
                  (setf (aref (slot-value algo 'vect) m) datum)) ;set the value
                (if (<= count thresh) ; pick algo to compute skip
                    (algo-x-skip algo)
                    (algo-z-skip algo))))))))

(defun algo-x-skip (algo)
  (with-slots (skip count term size  num rs) algo
    (let ((v (random 1.0 rs))
          (p 0))
      (incf count)
      (incf num)
      (let ((quot (/ num count)))
        (util:while (> quot v)
          (incf p)
          (incf count)
          (incf num)
          (setf quot (/ (* quot num) count)))
        (setf skip p)
        (setf term (+ (- count size) 1))))))

(defun algo-z-skip (algo)
  (with-slots (rs skip count term size w) algo
    (tagbody
     loop
       (let* ((u (random 1.0 rs))
              (phi (* count (- w 1.0)))
              (p (floor phi))
              ;;lhs := EXP(LOG(((U * (((t + 1)/term) ^ 2)) * (term + Ρ))/(t + χ))/n);
              (lhs (exp (/ (log (/ (* u (expt (/ (+ count 1) term)  2) (+ term p))
                                   (+ count phi)))
                           size)))
              ;;rhs := (((t + χ)/(term + Ρ)) * term)/t;
              (rhs (/ (* (/ (+ count phi)  (+ term p) ) term) count))
              )
         (when  (<= lhs rhs)
           (setf w (/ rhs lhs))
           (setf skip p)
           (go end-loop))
         ;; y := (((U * (t + 1))/term) * (t + Ρ + 1))/(t + χ);
         (let ((denom 0)
               (numer-lim 0)
               ;;y := (((U * (t + 1))/term) * (t + Ρ + 1))/(t + χ);
               (y (/ (* (/ (* u (+ count 1)) term) (+ count p 1)) (+ count phi))))
           (if (< size p)
               (progn
                 (setf denom count)
                 (setf numer-lim (+ term p)))
               (progn
                 (setf denom (+ (- count size) p))
                 (setf numer-lim (+ count 1))))
           (loop for numer from (+ count p) downto numer-lim do
              ;; y := (y * numer)/denom;
                (setf y (/ (* y numer) denom))
              ;; denom := denom - 1
                (decf denom)
                )
           ;;W := EXP(- LOG(RANDOM( ))/n); {Generate W in advance}
           (setf w (exp (/ (* -1 (log (random 1.0))) size)))
           ;;EXP(LOG(y)/n) ≤ (t + χ)/t
           (when (<=  (exp (/ (log y)  size)) (/ (+ count phi) count))
             (setf skip p)
             (go end-loop))))
       (go loop)
     end-loop)
    (setf count (+ count skip 1))
    (setf term (+ term skip 1))))

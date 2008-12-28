; Copyright Lightship Software, Incorporated.
;
; $Id$
; 
; Larceny library -- arithmetic functions.

($$trace "number")

(define (rational? obj)
  (and (real? obj)
       (or (exact? obj)
           (= 0.0 (- obj obj)))))

(define positive? (lambda (x) (> x 0)))

(define negative? (lambda (x) (< x 0)))
 
(define abs
 (lambda (n)
   (if (< n 0)
       (-- n)
       n)))

(define min
  (letrec ((min (lambda (x . y)
                  (if (<= x x)
                      (loop y x (exact? x))
                      x)))
           (loop (lambda (y x exact)
                   (if (null? y)
                       x
                       (let ((y1 (car y)))
                         (cond ((< y1 x)
                                (if exact
                                    (loop (cdr y) y1 (exact? y1))
                                    (loop (cdr y) (+ y1 0.0) exact)))
                               ((and exact (not (exact? y1)))
                                (loop (cdr y) (+ x 0.0) #f))
                               (else (loop (cdr y) x exact))))))))
    min))
 
(define max
  (letrec ((max (lambda (x . y)
                  (if (<= x x)
                      (loop y x (exact? x))
                      x)))
           (loop (lambda (y x exact)
                   (if (null? y)
                       x
                       (let ((y1 (car y)))
                         (cond ((> y1 x)
                                (if exact
                                    (loop (cdr y) y1 (exact? y1))
                                    (loop (cdr y) (+ y1 0.0) exact)))
                               ((and exact (not (exact? y1)))
                                (loop (cdr y) (+ x 0.0) #f))
                               (else (loop (cdr y) x exact))))))))
    max))
 
(define random
  (letrec ((random14
            (lambda (n)
              (set! x (fxlogand (+ (* a x) c) m))
              (remainder (fxrshl x 3) n)))
           (a 701)
           (x 1)
           (c 743483)
           (m 524287)
           (loop
            (lambda (q r n)
              (if (zero? q)
                  (remainder r n)
                  (loop (quotient q 16384)
                        (+ (* 16384 r) (random14 16384))
                        n)))))
    (lambda (n)
      (if (and (fixnum? n) (< n 16384))
          (random14 n)
          (loop (quotient n 16384) (random14 16384) n)))))
 
(define gcd
  (letrec ((loop (lambda (x y)
                   (cond ((zero? x) (finish y x))
                         ((zero? y) (finish x y))
                         ((< x y)
                          (if (= x 1)
                              (finish x y)
                              (loop x (remainder y x))))
                         ((> x y)
                          (if (= y 1)
                              (finish y x)
                              (loop (remainder x y) y)))
                         (else (finish x y)))))
           (finish (lambda (x y)
                     (if (exact? y) x (+ x 0.0))))
           (gcd (lambda (x . rest)
                  (let ((x (abs x)))
                    (cond ((null? rest) x)
                          ((null? (cdr rest)) (loop x (abs (car rest))))
                          (else (apply gcd
                                       (cons (loop x (abs (car rest)))
                                             (cdr rest)))))))))
    (lambda args
      (cond ((null? args) 0)
            ((null? (cdr args)) (abs (car args)))
            ((null? (cddr args))
             (loop (abs (car args)) (abs (cadr args))))
            (else (apply gcd args))))))

(define (lcm . args)
  (cond ((null? args) 1)
        ((null? (cdr args)) (abs (car args)))
        ((null? (cddr args))
         (let ((x (abs (car args)))
               (y (abs (cadr args))))
           (* x (quotient y (max 1 (gcd x y))))))
        (else (apply lcm
                     (cons (lcm (car args) (cadr args))
                           (cddr args))))))

; Doing the computation in exact arithmetic avoids needless loss 
; of precision.

(define (modulo x y)
  (if (or (inexact? x) (inexact? y))
      (exact->inexact (modulo (inexact->exact x) (inexact->exact y)))
      (let* ((q (quotient x y))
	     (r (- x (* q y))))
	(cond ((zero? r)
	       r)
	      ((negative? r)
	       (if (negative? y)
		   r
		   (+ r y)))
	      ((negative? y)
	       (+ r y))
	      (else
	       r)))))

(define (expt x y)

  ; x is nonzero, and y is an exact natural number.

  (define (e x y)
    (cond ((= y 0)
	   1)
	  ((odd? y)
	   (* x (e x (- y 1))))
	  (else 
	   (let ((v (e x (quotient y 2))))
	     (* v v)))))

  (cond ((zero? x)
         (let ((result (cond ((= y 0) 1)
                             ((> (real-part y) 0) 0)
                             (else +nan.0))))
           (if (and (exact? x) (exact? y))
               result
               (exact->inexact result))))
	((and (exact? y) (integer? y))
	 (if (negative? y)
	     (/ (expt x (abs y)))
	     (e x y)))
	(else
	 (exp (* y (log x))))))

; From MacScheme.
;
; This code was written by Alan Bawden.
; Its copyright status is unknown to me [i.e., to Will. --lars]
;
; Modified for R6RS semantics on infinities and NaNs.

(define (rationalize x e)
  (define (simplest-rational x y)
    (define (simplest-rational-internal x y)      ; assumes 0 < X < Y
      (let ((fx (floor x))        ; [X] <= X < [X]+1
            (fy (floor y)))       ; [Y] <= Y < [Y]+1, also [X] <= [Y]
        (cond ((not (< fx x))
               ;; X is an integer so X is the answer:
               fx)
              ((= fx fy)
               ;; [Y] = [X] < X < Y so expand the next term in the continued
               ;; fraction:
               (+ fx (/ (simplest-rational-internal
			 (/ (- y fy)) (/ (- x fx))))))
              (else
               ;; [X] < X < [X]+1 <= [Y] <= Y so [X]+1 is the answer:
               (+ 1 fx)))))
    (cond ((< y x)
           ;; Y < X so swap and try again:
           (simplest-rational y x))
          ((not (< x y))
           ;; X = Y so if either is a rational that is the answer, otherwise
           ;; X and Y are both infinite or both NaN.
           (cond ((rational? x) x)
                 ((rational? y) y)
                 (else x)))
          ((positive? x) 
           ;; 0 < X < Y which is what SIMPLEST-RATIONAL-INTERNAL expects:
           (simplest-rational-internal x y))
          ((negative? y)
           ;; X < Y < 0 so 0 < -Y < -X and we negate the answer:
           (- (simplest-rational-internal (- y) (- x))))
          ((and (exact? x) (exact? e))
           ;; X <= 0 <= Y so zero is the answer:
           0)
          (else 0.0)))
  (simplest-rational (- x e) (+ x e)))

;---------------------------------------------------------------------------

; The following are not present in the MacScheme version of this library.

; Floor of x.
; A little contorted to avoid generic arithmetic in flonum case.

(define (floor x)
  (cond ((flonum? x)
	 (if (< x 0.0)
	     (let ((g (truncate x)))
	       (if (not (= g x))
		   (- g 1.0)
		   g))
	     (truncate x)))
        ((not (real? x))
         (error "floor: " x " is not a real number."))
        ((compnum? x)
         (floor (real-part x)))
	((< x 0)
	 (let ((g (truncate x)))
	   (if (not (= g x))
	       (- g 1)
	       g)))
	(else
	 (truncate x))))


; Ceiling of x.
; A little contorted to avoid generic arithmetic in flonum case.
  
(define (ceiling x)
  (cond ((flonum? x)
	 (if (< x 0.0)
	     (truncate x)
	     (let ((g (truncate x)))
	       (if (not (= g x))
		   (+ g 1.0)
		   g))))
        ((not (real? x))
         (error "ceiling: " x " is not a real number."))
        ((compnum? x)
         (ceiling (real-part x)))
	((< x 0)
	 (truncate x))
	(else
	 (let ((g (truncate x)))
	   (if (not (= g x))
	       (+ g 1)
	       g)))))


; Odd and even, optimized for the fixnum and bignum cases.
; The bignum case matters for some system code.

(define (even? x)
  (cond ((fixnum? x)
	 (= (fxlogand x 1) 0))
	((bignum? x)
	 (= (fxlogand (bignum-ref x 0) 1) 0))
	(else
	 (zero? (remainder x 2)))))

(define (odd? x)
  (cond ((fixnum? x) 
	 (= (fxlogand x 1) 1))
	((bignum? x)
	 (= (fxlogand (bignum-ref x 0) 1) 1))
	(else
	 (not (zero? (remainder x 2))))))


; Polar numbers

(define (make-polar a b)
  (if (not (and (real? a) (real? b)))
      (begin (error "make-polar: invalid arguments: " a " " b)
	     #t)
      (make-rectangular (* a (cos b)) (* a (sin b)))))

(define (angle c)
  (atan (imag-part c) (real-part c)))

; NOTE: CLtL2 notes that this implementation may not be ideal for very
;       large or very small numbers.

(define (magnitude c)
  (let ((r (real-part c))
	(i (imag-part c)))
    (sqrt (+ (* r r) (* i i)))))


; The procedures flonum:{sin,cos,tan,asin,acos,atan,exp,log,sqrt} have
; system-specific implementations; if they are not primops they may
; be found in the OS file (Lib/unix.sch, for example) or in the
; flonum file (Lib/flonums.sch).

; Square root
; Formula for complex square root from CLtL2, p310.

(define (sqrt z)
  (cond ((and (flonum? z) (>= z 0.0))
	 (flonum:sqrt z))
	((not (real? z))
	 (exp (/ (log z) 2)))
	((< z 0)
	 (make-rectangular 0 (sqrt (- z))))
	(else
	 (flonum:sqrt (exact->inexact z)))))

; Trancendentals.
; Complex algorithms for SIN, COS, TAN from Abramowitz & Stegun (1972), p74.
; Complex algorithms for ASIN, ACOS, ATAN from the R4.95RS.
;
; NOTE: CLtL2 notes that the formulae for ASIN, ACOS, and ATAN may not be
;       ideal ("may be terrible") when using floating-point computation.

(define (sin z)
  (cond ((flonum? z)
	 (flonum:sin z))
	((not (real? z))
	 (let ((x (exact->inexact (real-part z)))
	       (y (exact->inexact (imag-part z))))
	   (+ (* (flonum:sin x) (flonum:cosh y))
	      (* +1.0i (flonum:cos x) (flonum:sinh y)))))
	(else
	 (flonum:sin (exact->inexact z)))))

(define (cos z)
  (cond ((flonum? z)
	 (flonum:cos z))
	((not (real? z))
	 (let ((x (exact->inexact (real-part z)))
	       (y (exact->inexact (imag-part z))))
	   (+ (* (flonum:cos x) (flonum:cosh y))
	      (* +1.0i (flonum:sin x) (flonum:sinh y)))))
	(else
	 (flonum:cos (exact->inexact z)))))

(define (tan z)
  (cond ((flonum? z)
	 (flonum:tan z))
	((not (real? z))
	 (let ((x (* 2.0 (exact->inexact (real-part z))))
	       (y (* 2.0 (exact->inexact (imag-part z)))))
	   (/ (+ (flonum:sin x) (* +1.0i (flonum:sinh y)))
	      (+ (flonum:cos x) (flonum:cosh y)))))
	(else
	 (flonum:tan (exact->inexact z)))))

(define (asin z)
  (cond ((and (flonum? z)
	      (<= -1.0 z 1.0))
	 (flonum:asin z))
	((and (real? z)
	      (<= -1.0 z 1.0))
	 (flonum:asin (exact->inexact z)))
        ((or (and (real? z)
                  (< z -1.0))
             (let ((y (imag-part z)))
               (or (> y 0)
                   (and (= y 0)
                        (< (real-part z) 0)))))
         (- (asin (- z))))
	(else
	 (* -1.0i (log (+ (* +1.0i z) (sqrt (- 1 (* z z)))))))))

(define (acos z)
  (cond ((and (flonum? z)
	      (<= -1.0 z 1.0))
	 (flonum:acos z))
	((and (real? z)
	      (<= -1.0 z 1.0))
	 (flonum:acos (exact->inexact z)))
	(else
         (- (/ (acos -1.0) 2.0) (asin z)))))

(define (atan z . rest)
  (if (null? rest)
      (cond ((flonum? z)
	     (flonum:atan z))
	    ((not (real? z))
	     (/ (- (log (+ 1.0 (* +1.0i z))) (log (- 1.0 (* +1.0i z))))
		+2.0i))
	    (else
	     (flonum:atan (exact->inexact z))))
      (let ((x z)
	    (y (car rest)))
	(cond ((and (flonum? x) (flonum? y))
	       (flonum:atan2 x y))
	      ((not (and (real? x) (real? y)))
	       (error "atan: domain error:" (list x y))
	       #t)
	      (else
	       (flonum:atan2 (exact->inexact x) (exact->inexact y)))))))

; Complex/negative case from the R^4.95RS, p25.

(define (log z . rest)
  (cond ((pair? rest)
         (let ((z2 (car rest)))
           (if (and (complex? z2)
                    (null? (cdr rest)))
               (/ (log z) (log z2))
               (error "log: domain error" (list x y)))))
        ((and (flonum? z) (> z 0.0))
	 (flonum:log z))
	((or (not (real? z)) (< z 0))
	 (+ (log (magnitude z)) (* +1.0i (angle z))))
	((zero? z)
         (if (exact? z)
             (error "log: domain error:" z)
             -1e500))
	(else
	 (flonum:log (exact->inexact z)))))

; Complex case from Abramowitz&Stegun (1972), p74.

(define (exp z)
  (cond ((flonum? z)
	 (flonum:exp z))
	((not (real? z))
	 (let ((i (imag-part z)))
	   (* (exp (real-part z))
	      (+ (cos i) (* +1.0i (sin i))))))
	(else
	 (flonum:exp (exact->inexact z)))))

(define (make-rectangular a b) 

  (define (construct-compnum a b)
    (if (and (exact? b) (= b 0))
	a
	(make-compnum a b)))

  (define (construct-rectnum a b)
    (if (and (exact? b) (= b 0))
	a
	(make-rectnum a b)))

  (define (fail x)
    (error "make-rectangular: " x " is not a real number."))

  (cond ((flonum? a)
	 (cond ((flonum? b)
		(construct-compnum a b))
	       ((compnum? b)
		(if (= 0.0 (imag-part b))
		    (construct-compnum a (real-part b))
		    (fail b)))
               ((and (exact? b) (= b 0))
                a)
	       (else
		(make-rectangular a (exact->inexact b)))))
	((compnum? a) 
	 (if (= 0.0 (imag-part a))
	     (make-rectangular (real-part a) b)
	     (fail a)))
	((inexact? b)
	 (make-rectangular (exact->inexact a) b))
	((rectnum? a)
	 (fail a))
	((rectnum? b)
	 (fail b))
	(else
	 (construct-rectnum a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger
;
; Procedures added for R6RS
;
; FIXME:  Some of these should be integrable.
; exact and inexact now are.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (real-valued? obj)
  (and (number? obj)
       (zero? (imag-part obj))))

(define (rational-valued? obj)
  (and (number? obj)
       (zero? (imag-part obj))
       (finite? (real-part obj))))

(define (integer-valued? obj)
  (and (number? obj)
       (zero? (imag-part obj))
       (finite? (real-part obj))
       (= 1 (denominator (real-part obj)))))

(define (exact z) (inexact->exact z))
(define (inexact z) (exact->inexact z))

(define (finite? x)
  (and (real? x)
       (not (infinite? x))
       (not (nan? x))))

(define (infinite? x)
  (and (inexact? x) (or (= x 1e500) (= x -1e500))))

(define (nan? x)
  (and (inexact? x) (not (= x x))))

; FIXME: all of these should be faster

(define (div-and-mod x y)
  (cond ((and (fixnum? x) (fixnum? y))
         (cond ((= y 0)
                (assertion-violation 'div "zero divisor" x y))
               ((>= x 0)
                (values (quotient x y) (remainder x y)))
               ((< y 0)
                ; x < 0, y < 0
                (let* ((q (quotient x y))
                       (r (- x (* q y))))
                  (if (= r 0)
                      (values q 0)
                      (values (+ q 1) (- r y)))))
               (else
                ; x < 0, y > 0
                (let* ((q (quotient x y))
                       (r (- x (* q y))))
                  (if (= r 0)
                      (values q 0)
                      (values (- q 1) (+ r y)))))))
        ((or (not (real? x))
             (not (real? y))
             (infinite? x)
             (nan? x)
             (= y 0))
         (assertion-violation 'div "illegal arguments" x y))
        ((< 0 y)
         (let* ((q (floor (/ x y)))
                (r (- x (* q y))))
           (values q r)))
        (else
         (let* ((q (floor (/ x (- y))))
                (r (+ x (* q y))))
           (values (- q) r)))))

(define (div x y)
  (cond ((and (fixnum? x)
              (fixnum? y)
              (fx>=? x 0))
         (quotient x y))
        ((and (exact? x)
              (exact? y)
              (integer? x)
              (integer? y)
              (>= x 0)
              (> y 0))
         (quotient x y))
        (else
         (call-with-values
          (lambda () (div-and-mod x y))
          (lambda (q r) q)))))

(define (mod x y)
  (cond ((and (fixnum? x)
              (fixnum? y)
              (fx>=? x 0))
         (remainder x y))
        ((and (exact? x)
              (exact? y)
              (integer? x)
              (integer? y)
              (>= x 0)
              (> y 0))
         (remainder x y))
        (else
         (call-with-values
          (lambda () (div-and-mod x y))
          (lambda (q r) r)))))

(define (div0-and-mod0 x y)
  (call-with-values
   (lambda () (div-and-mod x y))
   (lambda (q r)
     (cond ((< r (abs (/ y 2)))
            (values q r))
           ((> y 0)
            (values (+ q 1) (- x (* (+ q 1) y))))
           (else
            (values (- q 1) (- x (* (- q 1) y))))))))

(define (div0 x y)
  (call-with-values
   (lambda () (div0-and-mod0 x y))
   (lambda (q r) q)))

(define (mod0 x y)
  (call-with-values
   (lambda () (div0-and-mod0 x y))
   (lambda (q r) r)))

(define (exact-integer-sqrt k)
  (if (and (exact? k) (integer? k) (<= 0 k))
      (do ((s 1 (quotient (+ s (quotient k s)) 2)))
          ((<= (* s s) k (* s (+ s 2)))
           (values s (- k (* s s)))))
      (assertion-violation 'exact-integer-sqrt "illegal argument" k)))

; eof

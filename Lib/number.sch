; Copyright Lightship Software.
;
; Arithmetic functions for MacScheme.
; Augmented and changed for Larceny, which has a different set of primops.
;
; $Id: number.sch,v 1.3 1997/03/05 19:28:51 lth Exp $

(define positive? (lambda (x) (> x 0)))
 
(define negative? (lambda (x) (< x 0)))
 
(define abs
 (lambda (n)
   (if (< n 0)
       (-- n)
       n)))

(define min
  (letrec ((min (lambda (x . y) (loop y x (exact? x))))
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
  (letrec ((max (lambda (x . y) (loop y x (exact? x))))
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
              (set! x (logand (+ (* a x) c) m))
              (remainder (rshl x 3) n)))
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
          (loop n (random14 16384) n)))))
 
(define gcd
  (letrec ((loop (lambda (x y)
                   (cond ((or (zero? x) (zero? y)) (+ x y))
                         ((< x y) (let ((q (quotient y x)))
                                    (loop x (- y (* q x)))))
                         ((> x y) (let ((q (quotient x y)))
                                    (loop (- x (* q y)) y)))
                         (else (if (exact? y) x (+ x 0.0))))))
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
           (* x (quotient y (gcd x y)))))
        (else (apply lcm
                     (cons (lcm (car args) (cadr args))
                           (cddr args))))))

(define modulo
  (lambda (x y)
    (let* ((q (quotient x y))
           (r (- x (* q y))))
      (if (negative? r)
          (if (negative? y)
              r
              (+ r y))
          (if (negative? y)
              (+ r y)
              r)))))

; Takes only exact integer powers for now. The full gory version should
; go here, as a bootstrap version is loaded early.

(define (expt x y)

  (define (e x y)
    (cond ((= y 0)
	   1)
	  ((odd? y)
	   (* x (e x (- y 1))))
	  (else 
	   (let ((v (e x (quotient y 2))))
	     (* v v)))))

  (cond ((and (exact? x) (zero? x))
	 1)
	((and (exact? y) (integer? y))
	 (if (negative? y)
	     (/ (expt x (abs y)))
	     (e x y)))
	(else
	 (error "expt: don't yet know how to deal with" x y))))

; From MacScheme.
;
; This code was written by Alan Bawden.
; Its copyright status is unknown to me [i.e., to Will. --lars]

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
           ;; I don't know of anything implementation independent we can do.
           (cond ((rational? x) x)
                 ((rational? y) y)
                 (else (error "What should we do in this case? " x y))))
          ((positive? x) 
           ;; 0 < X < Y which is what SIMPLEST-RATIONAL-INTERNAL expects:
           (simplest-rational-internal x y))
          ((negative? y)
           ;; X < Y < 0 so 0 < -Y < -X and we negate the answer:
           (- (simplest-rational-internal (- y) (- x))))
          (else
           ;; X <= 0 <= Y so zero is the answer:
           0)))
  (simplest-rational (- x e) (+ x e)))


;---------------------------------------------------------------------------

; The following are not present in the MacScheme version of this library.
; Some used to be in flonums.sch, which ceased to exist.

; Floor of x.
; A little contorted to avoid generic arithmetic in flonum case.

(define (floor x)
  (cond ((or (flonum? x)
	     (and (compnum? x)
		  (= (imag-part x) 0.0)))
	 (if (< x 0.0)
	     (let ((g (truncate x)))
	       (if (not (= g x))
		   (- g 1.0)
		   g))
	     (truncate x)))
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
  (cond ((or (flonum? x)
	     (and (compnum? x) (= (imag-part x) 0.)))
	 (if (< x 0.0)
	     (truncate x)
	     (let ((g (truncate x)))
	       (if (not (= g x))
		   (+ g 1.0)
		   g))))
	((< x 0)
	 (truncate x))
	(else
	 (let ((g (truncate x)))
	   (if (not (= g x))
	       (+ g 1)
	       g)))))


; Odd and even, optimized for the fixnum case.
; FIXME.  Should optimize for bignums, too.

(define (even? n)
  (if (fixnum? x)
      (= (logand x 1) 0)
      (zero? (remainder n 2))))

(define (odd? x)
  (if (fixnum? x) 
      (= (logand x 1) 1)
      (not (zero? (remainder n 2)))))


; Polar numbers

(define (make-polar a b)
  (make-rectangular (* b (cos a)) (* b (sin a))))

(define (angle c)
  (atan (imag-part c) (real-part c)))

(define (magnitude c)
  (let ((r (real-part c))
	(i (imag-part c)))
    (sqrt (+ (* r r) (i i)))))

; The procedures flonum:{sin,cos,tan,asin,acos,atan,exp,log,sqrt} have
; system-specific implementations; if they are not primops they may
; be found in the OS file (Lib/unix.sch, for example) or in the
; flonum file (Lib/flonums.sch).

; Square root

(define (sqrt z)
  (cond ((flonum? z)
	 (flonum:sqrt z))
	((not (real? z))
	 (error "SQRT not implemented for complexes yet."))
	(else
	 (flonum:sqrt (exact->inexact z)))))

; Trancendentals

(define (sin z)
  (cond ((flonum? z)
	 (flonum:sin z))
	((not (real? z))
	 (error "SIN not implemented for complexes yet."))
	(else
	 (flonum:sin (exact->inexact z)))))

(define (cos z)
  (cond ((flonum? z)
	 (flonum:cos z))
	((not (real? z))
	 (error "COS not implemented for complexes yet."))
	(else
	 (flonum:cos (exact->inexact z)))))

(define (tan z)
  (cond ((flonum? z)
	 (flonum:tan z))
	((not (real? z))
	 (error "TAN not implemented for complexes yet."))
	(else
	 (flonum:tan (exact->inexact z)))))

(define (asin z)
  (cond ((flonum? z)
	 (flonum:asin z))
	((not (real? z))
	 (error "ASIN not implemented for complexes yet."))
	(else
	 (flonum:asin (exact->inexact z)))))

(define (acos z)
  (cond ((flonum? z)
	 (flonum:acos z))
	((not (real? z))
	 (error "ACOS not implemented for complexes yet."))
	(else
	 (flonum:acos (exact->inexact z)))))

(define (atan z . rest)
  (if (null? rest)
      (cond ((flonum? z)
	     (flonum:atan z))
	    ((not (real? z))
	     (error "ATAN not implemented for complexes yet."))
	    (else
	     (flonum:atan (exact->inexact z))))
      (let ((x z)
	    (y (car rest)))
	(cond ((and (flonum? x) (flonum? y))
	       (flonum:atan2 x y))
	      ((not (and (real? x) (real? y)))
	       (error "ATAN: domain error: " x " " y))
	      (else
	       (flonum:atan2 (exact->inexact x) (exact->inexact y)))))))

(define (log z)
  (cond ((flonum? z)
	 (flonum:log z))
	((not (real? z))
	 (error "log: can't handle complexes yet."))
	((<= z 0)
	 (error "log: Domain error: " z))
	(else
	 (flonum:log (exact->inexact z)))))

(define (exp z)
  (cond ((flonum? z)
	 (flonum:exp z))
	((not (real? z))
	 (error "EXP not implemented for complexes yet."))
	(else
	 (flonum:exp (exact->inexact z)))))

; eof

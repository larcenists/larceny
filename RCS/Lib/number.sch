; Copyright Lightship Software.
;
; Arithmetic functions for MacScheme.
; Augmented and changed for Larceny, which has a different set of primops.
;
; $Id: number.sch,v 1.2 1992/05/15 22:18:09 lth Exp lth $

 
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
 
; This is a primop in Larceny
;
;(define remainder
;  (lambda (n modulus)
;    (- n (* modulus (quotient n modulus)))))
 
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

; This is a primop in Larceny
;
;(define modulo
;  (lambda (x y)
;    (let* ((q (quotient x y))
;           (r (- x (* q y))))
;      (if (negative? r)
;          (if (negative? y)
;              r
;              (+ r y))
;          (if (negative? y)
;              (+ r y)
;              r)))))

; Takes only nonnegative integer powers for now. The full gory version should
; go here, as a bootstrap version is loaded early.

(define (expt x y)

  (define (expt x y)
    (if (zero? y)
        1
        (* x (expt x (- y 1)))))

  (if (negative? y)
      (error "Negative argument to expt.")
      (expt x y)))

;---------------------------------------------------------------------------

; The following are not present in the MacScheme version of this library.
; Some used to be in flonums.sch, which ceased to exist.

; Natural logarithm of x.

(define (log x)
  (if (< x 0)
      (error 'log "Can't deal with nonnegative numbers.")
      (begin (display "Warning: log not implemented; returning e.")
	     (newline)
	     2.7182)))

; Floor of x.

(define (floor x)
  (if (flonum? x)
      (if (negative? x)
	  (let ((z (truncate x)))
	    (if (< x z)
		(- z 1)
		z))
	  (truncate x))
      (truncate x)))

; Ceiling of x.
   
(define (ceiling x)
  (if (flonum? x)
      (if (negative? x)
	  (truncate x)
	  (- (floor (- 0.0 x))))
      (truncate x)))

;

(define (even? n)
  (zero? (remainder n 2)))

(define (odd? n)
  (not (zero? (remainder n 2))))

; Copyright 1998 Lars T Hansen.
;
; $Id$
; 
; Larceny library -- millicode support code.
;
; This file contains architecture-independent procedures which set up
; the Scheme-level support for millicode, like the vector of millicode-
; callable scheme procedures.

($$trace "mcode")

; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; 'a' is known to be a non-fixnum exact number, or not a number at all.

(define (generic-exact->inexact a)
  (cond ((bignum? a) 
         (bignum->flonum a))
	((ratnum? a)
	 (exact->inexact:rational a))
	((rectnum? a) 
	 (make-rectangular (exact->inexact (real-part a))
			   (exact->inexact (imag-part a))))
	(else 
	 (error "exact->inexact: " a " is not a number.")
	 #t)))

; Date: Mon, 18 May 1998 13:41:40 -0400
; From: William D Clinger <will@ccs.neu.edu>  [since modified]
;
; What EXACT->INEXACT should do when given an exact rational.
;
; Assumes inexact reals are represented using IEEE double precision
; floating point.  IEEE single and extended precision can be handled
; by changing n and flonum:minexponent.
;
; Test case: (exact->inexact 14285714285714285714285) should be
; 1.4285714285714286e22, not 1.4285714285714284e22.

(define exact->inexact:rational
  (let* ((n         53)
         (two^n-1   4503599627370496)     ; (expt 2 (- n 1))
         (two^n     9007199254740992)     ; (expt 2 n)
         (flonum:maxexponent 1024)
         (flonum:minexponent -1023)
         (log:2     0.6931471805599453))  ; (log 2)
    
    ; x is an inexact approximation to p/q
    
    (define (hard-case p q x)
      ; Beware of NaNs and infinities.
      (let* ((k (- (inexact->exact
                    (round (approximate-log2-of-ratio p q x)))
                   n)))
        (cond ((> k flonum:maxexponent)
               (expt 2.0 k))
              ((> k 0)
               (loop p (* q (expt 2 k)) k))
              ;; subtracting n twice allows for denormalized results
              ((< k (- flonum:minexponent n n))
               0.0)
              (else
               (loop (* p (expt 2 (- k))) q k)))))

    ; Returns an exact integer that approximates (lg p/q).
    ; x is an inexact approximation to p/q.

    (define (approximate-log2-of-ratio p q x)
      (let ((unity (/ x x)))
        (cond ((= unity unity)
               ; x is not 0.0, +inf.0, -inf.0, or +nan.0
               (inexact->exact (ceiling (log2 x))))
              ((and (> p 1) (> q 1))
               (if (< p q)
                   (let* ((q/p (quotient q p))
                          (x (/ 1.0 (exact->inexact q/p))))
                     (approximate-log2-of-ratio 1 q/p x))
                   (let* ((p/q (quotient p q))
                          (x (/ 1.0 (exact->inexact p/q))))
                     (approximate-log2-of-ratio p/q 1 x))))
              ((< p q)
               ; p is 1
               (- (approximate-log2-of-ratio q 1 (exact->inexact q))))
              (else
               ; p is very large, and q is 1
               (approximate-log2-of-integer p)))))
    
    ; Given a positive integer p, Returns an approximation to (lg p).

    (define (approximate-log2-of-integer p)
      (if (<= p two^n)
          (log2 (exact->inexact p))
          (bitwise-length p)))
    
    (define (log2 x)
      (/ (log x) log:2))

    ; r = u/v * 2^k
    
    (define (loop u v k)
      (let ((x (quotient u v)))
        (cond ((and (<= two^n-1 x) (< x two^n))
               (ratio->float u v k))
              ((< x two^n-1)
               (loop (* 2 u) v (- k 1)))
              ((<= two^n x)
               (loop u (* 2 v) (+ k 1)))
              (else
               (error "exact->inexact:rational: internal error: " u v k)))))
    
    ; Given exact positive integers u and v with
    ; 2^(n-1) <= u/v < 2^n, and exact integer k,
    ; returns the float closest to u/v * 2^k.
    
    (define (ratio->float u v k)
      (let* ((q (quotient u v))
             (r (- u (* q v)))
             (v-r (- v r)))
        (cond ((< r v-r)               (make-float q k))
              ((> r v-r)               (make-float (+ q 1) k))
              ((zero? (remainder q 2)) (make-float q k))
              (else                    (make-float (+ q 1) k)))))
    
    ; Primitive operations on flonums.
    
    (define (make-float m q)
      (if (< q flonum:minexponent)
          (make-float (* .5 m) (+ q 1))
          (* (+ m 0.0) (expt 2.0 q))))

    (lambda (r)
      (if (negative? r)
          (- (exact->inexact:rational (- r)))
          (let* ((p (numerator r))
                 (q (denominator r))
		 (a (exact->inexact p))
		 (b (exact->inexact q))
		 (x (/ a b)))
            (cond ((and (<= p two^n)
			(<= q two^n))
		   x)
		  (else
		   (hard-case p q x))))))))

; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; Identity operations are handled by the millicode.
; We have to handle the cases flonum->rational and compnum->rectnum.

(define (generic-inexact->exact a)
  (cond ((flonum? a)
	 (if (integer? a)
	     (flonum->integer a)
	     (flonum->ratnum a)))
	((compnum? a)
	 (make-rectangular (inexact->exact (real-part a))
			   (inexact->exact (imag-part a))))
	(else
	 (error "generic-inexact->exact: internal error: " a)
	 #t)))

(define (heavy-quotient a b)
  (cond ((and (bignum? a) (bignum? b))
	 (bignum-quotient a b))
	((and (integer? a) (integer? b))
	 (cond ((and (flonum? a) (flonum? b))
		(truncate (/ a b)))
	       ((and (compnum? a) (compnum? b))
		(truncate (/ (real-part a) (real-part b)))) ; Could be (/ a b)
	       (else
		(contagion a b quotient))))
	(else
	 (error "quotient: arguments must be integers: " a " " b)
	 #t)))

(define (heavy-remainder a b)
  (cond ((and (bignum? a) (bignum? b))
	 (bignum-remainder a b))
	((and (integer? a) (integer? b))
	 (- a (* (quotient a b) b)))
	(else
	 (error "remainder: arguments must be integers: " a " " b)
	 #t)))


; When calling fixnum2ratnum-div by saying '(/ 2 4):
;  the arguments are: 2 and 4
;  they are known to be integers and to be exact  
;
;   -r'm
(define (fixnum2ratnum-div a b)
  (cond ((= b 0) (error "fixnum2ratnum-div: division by zero") #t)
	((< b 0) (make-reduced-ratnum (- a) (- b)))
	(else    (make-reduced-ratnum a b))))


; "install-millicode-support" makes a vector of *all* scheme procedures
; which are callable from millicode and puts this vector in the global
; variable "millicode-support". It also sets up the global procedure
; "contagion-error-code" which returns an error code for certain operations
; supported by contagion.
;
; The order of the procedures in this vector is very important and 
; *must* correspond to the offsets defined in the file "../milliprocs.cfg".
;
; DO NOT CHANGE THE ORDER OF ARGUMENTS TO "vector"!
;
; FIXME: should assign by index, rather than having a large vector
;        expression.

(define millicode-support)
(define contagion-error-code)

(define (install-millicode-support)
  (let ((v (vector #f                   ; obsolete: scheme2scheme-helper
		   #f                   ; obs: undefined-global-exception
		   exception-handler
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   bignum-add		; loc 10
		   bignum-subtract
		   bignum-multiply
		   bignum-divide
		   bignum-negate
		   bignum-abs
		   bignum=?
		   bignum<?
		   bignum<=?
		   bignum>?
		   bignum>=?		; loc 20
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   ratnum-add		; loc 30
		   ratnum-sub
		   ratnum-mul
		   ratnum-div
		   ratnum-neg
		   ratnum-abs
		   ratnum=?
		   ratnum<?
		   ratnum<=?
		   ratnum>?
		   ratnum>=?		; loc 40
		   ratnum-round
		   ratnum-truncate
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   rectnum-add		; loc 50
		   rectnum-sub
		   rectnum-mul
		   rectnum-div
		   rectnum-neg
		   rectnum=?
		   #f
		   #f
		   #f
		   #f
		   #f			; loc 60
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   (lambda (a b) (+ a b))		; loc 70
		   (lambda (a b) (- a b))
		   (lambda (a b) (* a b))
		   (lambda (a b) (/ a b))
		   fixnum2ratnum-div
		   heavy-quotient
		   heavy-remainder
		   #f ; heavy-modulo ; no longer in millicode
		   (lambda (a b) (= a b))
		   (lambda (a b) (< a b))
		   (lambda (a b) (<= a b))		; loc 80
		   (lambda (a b) (> a b))
		   (lambda (a b) (>= a b))
		   contagion
		   pcontagion
		   econtagion
		   #f
		   #f
		   #f
		   #f
		   #f                                   ; loc 90
		   generic-inexact->exact
		   generic-exact->inexact
		   )))

    ; Return the error code for the given millicode support vector index,
    ; for operations supported by contagion.
    ;
    ; Returns the error code for the given millicode support vector procedure,
    ; for operations supported by contagion.
    ;
    ; Returns #f if no error code can be found.

    (define %contagion-error-code
      (let ((v (vector $ex.add $ex.sub $ex.mul $ex.div #f
		       $ex.quotient $ex.remainder $ex.modulo 
		       $ex.equalp $ex.lessp $ex.lesseqp
		       $ex.greaterp $ex.greatereqp)))
	(lambda (idx)
	  (cond ((procedure? idx)
		 (let loop ((i 70))
		   (cond ((= i (+ 70 (vector-length v)))
			  #f)
			 ((eq? idx (vector-ref millicode-support i))
			  (vector-ref v (- i 70)))
			 (else (loop (+ i 1))))))
		((< idx 70) #f)
		((>= idx (+ 70 (vector-length v))) #f)
		(else (vector-ref v (- idx 70)))))))

    ; Given an error code as returned by contagion-error-code,
    ; returns the name of the procedure.
    ;
    ; Returns #f if no name can be found.

    (define (%contagion-error-proc code)
      (cond ((eq? code $ex.add)        '+)
            ((eq? code $ex.sub)        '-)
            ((eq? code $ex.mul)        '*)
            ((eq? code $ex.div)        '/)
            ((eq? code $ex.quotient)   'quotient)
            ((eq? code $ex.remainder)  'remainder)
            ((eq? code $ex.modulo)     'modulo)
            ((eq? code $ex.equalp)     '=)
            ((eq? code $ex.lessp)      '<)
            ((eq? code $ex.lesseqp)    '<=)
            ((eq? code $ex.greaterp)   '>)
            ((eq? code $ex.greatereqp) '>=)
            (else #f)))

    (set! millicode-support v)
    (set! contagion-error-code %contagion-error-code)
    (set! contagion-error-proc %contagion-error-proc)))

; eof

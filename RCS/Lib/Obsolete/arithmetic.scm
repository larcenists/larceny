; -*- Scheme -*-
;
; Scheme 313 run-time system
; Arithmetic and other numeric stuff.
;
; $Id$
;
; NOTES
; -----
; The `-' and `/' are extended with respect to the IEEE standard: they allow
; one or more arguments, not just one or two. These operations associate 
; their arguments to the left, as expected.

(define + '())
(define - '())
(define * '())
(define / '())
(define quotient '())
(define remainder '())
(define number? '())
(define zero? '())
(define negative? '())
(define positive? '())
(define odd? '())
(define even? '())
(define exact? '())
(define inexact? '())
(define = '())
(define > '())
(define < '())
(define >= '())
(define <= '())
(define abs '())
(define gcd '())
(define lcm '())
(define exp '())
(define expt '())
(define sqrt '())
(define log '())
(define sin '())
(define cos '())
(define tan '())
(define arcsin '())
(define arccos '())
(define arctan '())
(define floor '())
(define ceiling '())
(define truncate '())
(define round '())
(define rationalize '())
(define exact->inexact '())
(define inexact->exact '())
(define max '())
(define min '())

(let ()

  ; this is (- (expt 2 29))
  (define smallest-negative-fixnum -536870912)

  ; this is (- (expt 2 29) 1)
  (define greatest-positive-fixnum 536870911)

  ; this is an approximation to the magic number `e'
  (define e 2.718281828459046)

  (define (fixnum? a)
    (and (integer? a)
	 (>= a smallest-negative-fixnum)
	 (<= a greatest-positive-fixnum)))

  (define (bignum? a)
    (and (integer? a)
	 (not (fixnum? a))))

  (define (plus-loop id p)
    (lambda rest
      (let loop ((r id) (rest rest))
	(if (null? rest)
	    r
	    (loop (p r (car rest)) (cdr rest)))))

  (define (minus-loop id p)
    (lambda (a . rest)
      (if (null? rest)
	  (p id a)
	  (let loop ((r a) (rest rest))
	    (if (null? rest)
		r
		(loop (- r (car rest)) (cdr rest)))))))

  (define (generic-divide a b)
    (cond ((and (fixnum? a) (fixnum? b))
	   (fixnum-divide a b))
	  ((and (bignum? a) (bignum? b))
	   (bignum-divide a b))
	  ((and (flonum? a) (flonum? b))
	   (flonum-divide a b))
	  ((and (compnum? a) (compnum? b))
	   (compnum-divide a b))
	  ((and (ratnum? a) (ratnum? b))
	   (ratnum-divide a b))
	  ((and (rectnum? a) (rectnum? b))
	   (rectnum-divide a b))
	  (else
	   (let ((ab (contagion a b)))
	     (divide (car ab) (cdr ab))))))

  (define (generic-quotient a b)
    (cond ((and (fixnum? a) (fixnum? b))
	   (fixnum-quotient a b))
	  ((and (bignum? a) (bigum? b))
	   (bignum-quotient a b))
	  ((and (integer? a) (integer? b))
	   (let ((ab (contagion a b)))
	     (generic-quotient (car ab) (cdr ab))))
	  (else
	   (error 'quotient "Argument is not an integer."))))

  (define (generic-remainder a b)
    (cond ((and (fixnum? a) (fixnum? b))
	   (fixnum-remainder a b))
	  ((and (bignum? a) (bginum? b))
	   (bignum-remainder a b))
	  ((and (integer? a) (integer? b))
	   (let ((ab (contagion a b)))
	     (generic-remainder (car ab) (cdr ab))))
	  (else
	   (error 'remainder "Argument is not an integer."))))

  (define (generic-zero? a)
    (cond ((fixnum? a)
	   (= a 0))
	  ((bignum? a)         ; shouldn't happen, but can, within the system.
	   (bignum-zero? a))
	  ((flonum? a)
	   (= a 0.0))
	  ((compnum? a)
	   ??)
	  ((ratnum? a)         ; ditto
	   ??)
	  ((rectnum? a)        ; ditto
	   ??)
	  (else
	   (error 'zero? "Non-numeric argument."))))

  (define (generic-negative? a)
    (cond ((fixnum? a)
	   (< a 0))
	  ((bignum? a)
	   (bignum-negative? a))
	  ((flonum? a)
	   (< a -0.0))         ; IEEE; the compiler had better handle this!
	  ((compnum? a)
	   ??)
	  ((ratnum? a)
	   ??)
	  ((rectnum? a)
	   ??)
	  (else
	   (error 'negative? "Non-numeric argument."))))

  (define (generic-positive? a)
    (cond ((fixnum? a)
	   (> a 0))
	  ((bignum? a)
	   (bignum-positive? a))
	  ((flonum? a)
	   (> a 0.0))
	  ((compnum? a)
	   ??)
	  ((ratnum? a)
	   ??)
	  ((rectnum? a)
	   ??)
	  (else
	   (error 'positive? "Non-numeric argument."))))

  (define (generic-number? a)
    (or (integer? x) (rational? x) (real? x) (complex? x)))

  (define (generic-odd? a)
    (cond ((fixnum? a)
	   (= 1 (fixnum-remainder a 2)))
	  ((bignum? a)
	   (bignum-odd? a))
	  (else
	   (error 'odd? "Non-integer argument."))))

  (define (generic-even? a)
    (cond ((fixnum? a)
	   (= 0 (fixnum-remainder a 2)))
	  ((bignum? a)
	   (not (bignum-odd? a)))
	  (else
	   (error 'even? "Non-integer argument."))))

  (define (cmp-loop p)
    (lambda (a b . rest)
      (let loop ((a a) (b b) (rest rest))
	(cond ((null? rest)
	       (p a b))
	      ((not (p a b))
	       #f)
	      (else
	       (loop b (car rest) (cdr rest)))))))

  (define (generic-expt a b)
    (cond ((integer? b)
	   (cond ((> b 0)
		  (let loop ((r 1) (b b))
		    (cond ((zero? b)
			   r)
			  ((odd? b)
			   (loop (* r a) (- b 1)))
			  (else
			   (loop (* r r) (/ b 2)))))) 
		 ((= b 0)
		  1)
		 ((< b 0)
		  (/ (generic-expt a (- b))))))
	  (else
	   (exp (* b (log a))))))

  (define (generic-exp a)
    '())

  (define (generic-log a)
    '())

  (define (generic-sqrt a)
    '())

  (define (generic-gcd . rest)
    '())

  (define (generic-lcm . rest)
    '())


  (set! + (plus-loop 0 (lambda (a b) (+ a b))))
  (set! - (minus-loop 0 (lambda (a b) (- a b))))
  (set! * (plus-loop 1 (lambda (a b) (* a b))))
  (set! / (minus-loop 1 generic-divide))
  (set! quotient (minus-loop 1 generic-quotient))
  (set! remainder (minus-loop 1 generic-remainder))
  (set! number? generic-number?)
  (set! zero? generic-zero?)
  (set! negative? generic-negative?)
  (set! positive? generic-positive?)
  (set! odd? generic-odd?)
  (set! even? generic-even?)
  (set! = (cmp-loop (lambda (a b) (= a b))))
  (set! > (cmp-loop (lambda (a b) (> a b))))
  (set! < (cmp-loop (lambda (a b) (< a b))))
  (set! >= (cmp-loop (lambda (a b) (>= a b))))
  (set! <= (cmp-loop (lambda (a b) (<= a b))))
  (set! exp generic-exp)
  (set! expt generic-expt)
  (set! log generic-log)
  (set! sqrt generic-sqrt)
  '())


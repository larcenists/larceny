; -*- Scheme -*-
;
; Scheme 313 runtime system.
; Scheme code for ratnum arithmetic.
;
; $Id: ratnums.scm,v 1.1 91/08/12 12:48:01 lth Exp Locker: lth $
;
; We have to be careful about the sign here. The numerator is signed; the
; denominator is always positive.

; (export make-reduced-ratnum
;         make-unreduced-ratnum
;         ratnum-add
;         ratnum-subtract
;         ratnum-multiply
;         ratnum-divide
;         ratnum-=
;         ratnum->
;         ratnum-<
;         ratnum->=
;         ratnum-<=
;         ratnum-positive?
;         ratnum-negative?
;         ratnum-negate
;         ratnum->flonum)

; LOW-LEVEL STUFF
;
; Ratnums look like vectors. These procedures should be integrable and anyway
; we're not really allowed to treat ratnums as vectors, so some magic must
; happen.

(define (numerator ratnum)
  (vector-ref ratnum 0))

(define (denominator ratnum)
  (vector-ref ratnum 1))

(define (make-ratnum a b)
  (let ((c (make-vector a b)))
    (vector-tag-set! c ratnum)
    c))


; HIGH-LEVEL STUFF
;
; Given two integers, return a ratnum or integer. In the former case, the
; ratnum has the smallest possible denominator.
;
; Knuth vol II, 2nd ed, section 4.5

(define (make-reduced-ratnum a b)
  (let ((gcd (gcd a b)))
    (make-ratnum (/ a gcd) (/ b gcd))))

(define (make-unreduced-ratnum a b)
  (make-ratnum a b))

(define (ratnum-add a b)
  '())

(define (ratnum-subtract a b)
  '())

(define (ratnum-multiply a b)
  (let ((gcd1 (gcd (numerator a) (denominator b)))
	(gcd2 (gcd (denominator a) (numerator b))))
    (make-ratnum (* (/ (numerator a) gcd1) (/ (numerator b) gcd2))
		 (* (/ (denominator a) gcd2) (/ (denominator b) gcd1)))))

(define (ratnum-divide a b)
  '())

(define (ratnum-= a b)
  (and (= (numerator a) (numerator b))
       (= (denominator a) (denominator b))))

(define (ratnum-> a b)
  '())

(define (ratnum->= a b)
  '())

(define (ratnum-< a b)
  '())

(define (ratnum-<= a b)
  '())

(define (ratnum-positive? a)
  (positive? (numerator a)))

(define (ratnum-negative? a)
  (negative? (denominator a)))

(define (ratnum-negate a)
  (make-ratnum (- (numerator a)) (denominator a)))

(define (ratnum->flonum a)
  (/ (exact->inexact (numerator a))
     (exact->inexact (denominator a))))

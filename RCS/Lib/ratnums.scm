; -*- Scheme -*-
;
; Scheme 313 runtime system.
; Scheme code for ratnum arithmetic.
;
; $Id: ratnums.scm,v 1.3 92/02/10 03:19:50 lth Exp Locker: lth $
;
; We have to be careful about the sign here. The numerator is signed; the
; denominator is always positive.

; (export make-reduced-ratnum
;         make-unreduced-ratnum
;         ratnum-add
;         ratnum-subtract
;         ratnum-multiply
;         ratnum-divide
;         ratnum-neg
;         ratnum-abs
;         ratnum=?
;         ratnum>?
;         ratnum<?
;         ratnum>=?
;         ratnum<=?
;         ratnum->flonum)

; SYSTEM CODE

; both arguments must be exact integers, and the denominator must be positive.

(define (make-ratnum a b)
  (let ((c (make-vector a b)))
    (typetag-set! c sys$tag.ratnum-typetag)
    c))

; ditto 

(define (make-reduced-ratnum a b)
  (let ((gcd (gcd a b)))
    (make-ratnum (/ a gcd) (/ b gcd))))

; ditto
 
(define (make-unreduced-ratnum a b)
  (make-ratnum a b))

; LOW-LEVEL USER STUFF
;
; Ratnums look like vectors.

(define (ratnum? x)
  (and (vector-like? x)
       (= (typetag x) sys$tag.ratnum-typetag)))

(define (numerator ratnum)
  (cond ((ratnum? ratnum)
	 (vector-like-ref ratnum 0))
	((integer? ratnum)
	 ratnum)
	(else
	 (error "numerator: not a rational" ratnum))))

(define (denominator ratnum)
  (cond ((ratnum? ratnum)
	 (vector-like-ref ratnum 1))
	((integer? ratnum)
	 (if (exact? ratnum)
	     1
	     1.0))
	(else
	 (error "denominator: not a rational" ratnum))))

(define (ratnum-add a b) (error "Ratnum-add not implemented"))

(define (ratnum-sub a b) (error "Ratnum-sub not implemented"))

(define (ratnum-mul a b)
  (let ((gcd1 (gcd (numerator a) (denominator b)))
	(gcd2 (gcd (denominator a) (numerator b))))
    (make-ratnum (* (/ (numerator a) gcd1) (/ (numerator b) gcd2))
		 (* (/ (denominator a) gcd2) (/ (denominator b) gcd1)))))

(define (ratnum-div a b) (error "Ratnum-div not implemented"))

(define (ratnum-abs a) (error "Ratnum-abs not implemented"))

(define (ratnum-neg a)
  (make-ratnum (- (numerator a)) (denominator a)))

(define (ratnum=? a b)
  (and (= (numerator a) (numerator b))
       (= (denominator a) (denominator b))))

(define (ratnum<? a b) (error "Ratnum<? not implemented"))

(define (ratnum<=? a b) (not (ratnum>? a b)))

(define (ratnum>? a b) (error "Ratnum>? not implemented"))

(define (ratnum>=? a b) (not (ratnum<? a b)))

(define (ratnum->flonum a)
  (/ (exact->inexact (numerator a))
     (exact->inexact (denominator a))))

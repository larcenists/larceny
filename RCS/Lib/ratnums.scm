; -*- Scheme -*-
;
; Scheme 313 runtime system.
; Scheme code for ratnum arithmetic.
;
; $Id: ratnums.scm,v 1.4 1992/03/31 12:31:16 lth Exp remy $
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
  (let ((c (vector a b)))
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

(define (ratnum->string r radix)
  (string-append
   (number->string (numerator r) radix)
   "/"
   (number->string (denominator r) radix)))

(define (remove-1-from-denominator r)
  (if (= 1 (denominator r))
      (numerator r)
      r))

; From Knuth.  I believe that this routine will always return
;  a simplified ratnum, but this must be confirmed.  (Seems to work.)
;
(define (ratnum-add-or-sub a b +/-)
  (let ((gcd1 (gcd (denominator a) (denominator b))))
    (remove-1-from-denominator
     (if (= gcd1 1)
	 (make-ratnum (+/- (* (numerator a) (denominator b))
			   (* (numerator b) (denominator a)))
		      (* (denominator a) (denominator b)))
	 (let* ((t (+/- (* (numerator a)
			   (/ (denominator b) gcd1))
			(* (numerator b)
			   (/ (denominator a) gcd1))))
		(gcd2 (gcd t gcd1)))
	   (make-ratnum (/ t gcd2)
			(* (/ (denominator a) gcd1)
			   (/ (denominator b) gcd2))))))))

(define (ratnum-add a b)
  (ratnum-add-or-sub a b +))

(define (ratnum-sub a b) 
  (ratnum-add-or-sub a b -))

(define (ratnum-mul a b)
  (remove-1-from-denominator
   (let ((gcd1 (gcd (numerator a) (denominator b)))
	 (gcd2 (gcd (denominator a) (numerator b))))
     (make-ratnum (* (/ (numerator a) gcd1) (/ (numerator b) gcd2))
		  (* (/ (denominator a) gcd2) (/ (denominator b) gcd1))))))

; What about division when b == 0?
(define (ratnum-div a b)
  (remove-1-from-denominator
   (let ((gcd1 (gcd (numerator a) (numerator b)))
	 (gcd2 (gcd (denominator a) (denominator b))))
     (cond ((< (numerator b) 0)
	    (make-ratnum (- (* (/ (numerator a) gcd1) 
			       (/ (denominator b) gcd2)))
			 (- (* (/ (denominator a) gcd2) 
			       (/ (numerator b) gcd1)))))
	   ((= (numerator b) 0)
	    (error "ratnum-div - division by 0"))
	   (else
	    (make-ratnum (* (/ (numerator a) gcd1) (/ (denominator b) gcd2))
			 (* (/ (denominator a) gcd2) 
			    (/ (numerator b) gcd1))))))))
	  

(define (ratnum-abs r) 
  (make-ratnum (abs (numerator r))
	       (abs (denominator r))))

(define (ratnum-neg a)
  (make-ratnum (- (numerator a)) 
	       (denominator a)))

; Assumes simplified ratnums
(define (ratnum=? a b)
  (and (= (numerator a) (numerator b))
       (= (denominator a) (denominator b))))

(define (ratnum<? a b) 
  (< 0 (numerator (ratnum-sub b a))))

(define (ratnum<=? a b) 
  (not (ratnum>? a b)))

(define (ratnum>? a b) 
  (> 0 (numerator (ratnum-sub b a))))

(define (ratnum>=? a b) 
  (not (ratnum<? a b)))

(define (ratnum->flonum a)
  (/ (exact->inexact (numerator a))
     (exact->inexact (denominator a))))

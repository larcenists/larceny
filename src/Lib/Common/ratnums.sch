; Copyright 1992 Rémy Evard.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted.
;
; $Id$
;
; Larceny library  -  Scheme code for ratnum arithmetic.
;
; We have to be careful about the sign here. The numerator is signed; the
; denominator is always positive.

($$trace "ratnums")

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

; Both arguments must be exact integers, and the denominator `b' must 
; be positive. No check is made for this.
;
; Make-ratnum should in principle be called from within this module
; only; other modules should all call either make-reduced-ratnum or
; make-unreduced-ratnum.

(define (make-ratnum a b)
  (let ((c (vector a b)))
    (typetag-set! c sys$tag.ratnum-typetag)
    c))

; Both arguments must be exact integers, and the denominator `b' must 
; be positive. If the reduced ratnum a'/b' has b'=1 then a' is returned
; as an exact integer.

(define (make-reduced-ratnum a b)
  (let ((gcd (gcd a b)))
    (let ((newa (quotient a gcd))
	  (newb (quotient b gcd)))
      (if (= newb 1)
	  newa
	  (make-ratnum newa newb)))))

; Both arguments must be exact integers, and the denominator `b' must 
; be positive and greater than 1.

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
	((or (flonum? ratnum)
	     (and (compnum? ratnum)
		  (zero? (imag-part ratnum))))
	 (exact->inexact (numerator (inexact->exact ratnum))))
	(else
	 (error "numerator: not a rational" ratnum)
	 #t)))

(define (denominator ratnum)
  (cond ((ratnum? ratnum)
	 (vector-like-ref ratnum 1))
	((integer? ratnum)
	 (if (exact? ratnum)
	     1
	     1.0))
	((or (flonum? ratnum)
	     (and (compnum? ratnum)
		  (zero? (imag-part ratnum))))
	 (exact->inexact (denominator (inexact->exact ratnum))))
	(else
	 (error "denominator: not a rational" ratnum)
	 #t)))

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

(define (ratnum-add-or-sub a b add/subtract)
  (let ((gcd1 (gcd (denominator a) (denominator b))))
    (remove-1-from-denominator
     (if (= gcd1 1)
         (make-ratnum (add/subtract (* (numerator a) (denominator b))
                                    (* (numerator b) (denominator a)))
                      (* (denominator a) (denominator b)))
         (let* ((t (add/subtract (* (numerator a)
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
	    (error "ratnum-div - division by 0")
	    #t)
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

(define (ratnum-round a)
  (if (< a 0)
      (- (ratnum-round (- a)))
      (let ((g (- a 1/2)))
	(cond ((not (integer? g)) (truncate (+ a 1/2)))
	      ((odd? g)           (+ g 1))
	      (else               g)))))
	      
(define (ratnum-truncate a)
  (quotient (numerator a) (denominator a)))

; eof

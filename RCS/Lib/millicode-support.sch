; -*- Scheme -*-
;
; This file contains architecture-independent procedures which set up
; the Scheme-level support for millicode, like the vector of millicode-
; callable scheme procedures.
;
; $Id: millicode-support.sch,v 1.3 92/02/23 16:56:09 lth Exp Locker: lth $

; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; The scheme procedure 'make-rectangular' is implemented in Scheme for now.
; Perhaps it always should be; anyway, calling make-rectangular is expensive
; because we fall straight thru millicode into _schemecall and then into
; this procedure.

(define (generic-make-rectangular a b) 
  (if (and (exact? a) (exact? b))
      (if (not (zero? b))
          (make-rectnum a b)
          a)
      (make-compnum a b)))


; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; 'a' is known to be a non-fixnum exact number, or not a number at all.

(define (generic-exact->inexact a)
  (cond ((bignum? a) 
	 (bignum->flonum a))
	((ratnum? a)
	 (/ (exact->inexact (numerator a)) (exact->inexact (denominator a))))
	((rectnum? a) 
	 (make-rectangular (exact->inexact (real-part a))
			   (exact->inexact (imag-part a))))
	(else 
	 (error "exact->inexact: " a " is not a number."))))


; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; Identity operations are handled by the millicode.
; We have to do is to handle the cases flonum->rational and compnum->rectnum.

(define (generic-inexact->exact a)
  (cond ((flonum? a)
	 (flonum->integer a))
	((compnum? a)
	 (make-rectangular (flonum->integer (real-part a))
			   (flonum->integer (imag-part a))))
	(else ???)))

(define (heavy-quotient a b)
  (cond ((and (bignum? a) (bignum? b))
	 (bignum-quotient a b))
	((and (integer? a) (integer? b))
	 (let ((a (cond ((flonum? a) (inexact->exact a))
			((compnum? a) (inexact->exact (real-part a)))
			(else a)))
	       (b (cond ((flonum? b) (inexact->exact b))
			((compnum? b) (inexact->exact (real-part b)))
			(else b))))
	   (contagion a b quotient)))
	(else
	 (error "quotient: arguments must be integers: " a b))))

(define (heavy-remainder a b)
  (cond ((and (bignum? a) (bignum? b))
	 (bignum-remainder a b))
	((and (integer? a) (integer? b))
	 (let ((a (cond ((flonum? a) (inexact->exact a))
			((compnum? a) (inexact->exact (real-part a)))
			(else a)))
	       (b (cond ((flonum? b) (inexact->exact b))
			((compnum? b) (inexact->exact (real-part b)))
			(else b))))
	   (contagion a b remainder)))
	(else
	 (error "remainder: arguments must be integers: " a b))))

; "install-millicode-support" makes a vector of *all* scheme procedures
; which are callable from millicode and puts this vector in the global
; variable "millicode-support". The order of the procedures in this
; vector is very important and *must* correspond to the offsets defined
; in the file "../milliprocs.cfg".
;
; DO NOT CHANGE THE ORDER OF ARGUMENTS TO "vector"!
;
; (Not all names are right here.)

(define (install-millicode-support)
  (let ((v (vector scheme2scheme-helper
		   undefined-global-exception
		   #f
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
		   #f
		   #f
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
		   +			; loc 70
		   -
		   *
		   /
		   fixnum2ratnum-div
		   heavy-quotient
		   heavy-remainder
		   heavy-modulo
		   =
		   <
		   <=			; loc 80
		   >
		   >=
		   contagion
		   pcontagion
		   econtagion
		   #f
		   #f
		   #f
		   #f
		   generic-make-rectangular ; loc 90
		   generic-inexact->exact
		   generic-exact->inexact
		   )))
    (set! millicode-support v)))



; -*- Scheme -*-
;
; This file contains architecture-independent procedures which set up
; the Scheme-level support for millicode, like the vector of millicode-
; callable scheme procedures.
;
; $Id$

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
		   #f			; reserved
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
		   >			; loc 80
		   <=
		   >=
		   contagion
		   pcontagion
		   econtagion
		   #f
		   #f
		   #f
		   #f
		   generic-make-rectangular ; loc 90
		   )))
    (break)
    (set! millicode-support v)))


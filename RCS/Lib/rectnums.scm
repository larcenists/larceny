; -*- Scheme -*-
;
; Scheme 313 runtime system
; Procedures for rectnum arithmetic
;
; $Id: rectnums.scm,v 1.2 92/02/17 18:27:14 lth Exp Locker: lth $
;
; A rectnum is a pair of two exact real numbers (the "real" and "imaginary"
; parts) where the imaginary part is nonzero.

; (export rectnum-add
;         rectnum-subtract
;         rectnum-multiply
;         rectnum-divide
;         rectnum-negate
;         rectnum-zero?
;         rectnum-negative?
;         rectnum-positive?
;         rectnum=?
;         rectnum>?
;         rectnum>=?
;         rectnum<?
;         rectnum<=?)
;
; Yours truly has minimal knowledge of complex arithmetic at this point.
; Some of these procedures may be meaningless.

; Low-level stuff.
; A rectnum is a vector-like object with two parts, the first being the
; real and the second being the imaginary part. Both parts are exact real
; numbers (integers or ratnums in this implementation).
;
; These procedures should be recognized by the compiler or at the very
; least coded in assembly language, since we cannot really use vector-ref
; etc. to access these objects.

(define (make-rectnum a b)
  (if (or (inexact? a) (inexact? b))
      (error "Inexact argument(s) to make-rectnum.")
      (let ((v (vector a b)))
	(typetag-set! v sys$tag.rectnum-typetag)
	v)))

(define (rectnum? obj)
  (and (vector-like? obj)
       (= (typetag obj) sys$tag.rectnum-typetag)))

(define (rectnum-real-part r)
  (if (rectnum? r)
      (vector-like-ref r 0)
      (error "Not a rectnum.")))

(define (rectnum-imag-part r)
  (if (rectnum? r)
      (vector-like-ref r 1)
      (error "Not a rectnum.")))

; High-level stuff

(define (rectnum-add a b) (error "Rectnum-add not implemented"))
(define (rectnum-sub a b) (error "Rectnum-sub not implemented"))
(define (rectnum-mul a b) (error "Rectnum-mul not implemented"))
(define (rectnum-div a b) (error "Rectnum-div not implemented"))
(define (rectnum-neg a)   (error "Rectnum-neg not implemented"))
(define (rectnum=? a b)   (error "Rectnum=? not implemented"))

; -*- Scheme -*-
;
; Scheme 313 runtime system
; Procedures for rectnum arithmetic
;
; $Id$
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
  (let ((v (vector a b)))
    (vector-tag-set! v 'rectnum)
    v))

(define (rectnum-real-part r)
  (vector-ref r 0))

(define (rectnum-imag-part r)
  (vector-ref r 1))

; High-level stuff

(define (rectnum-add a b)
  (unimp 'rectnum-add))

(define (rectnum-subtract a b)
  (unimp 'rectnum-subtract))

(define (rectnum-multiply a b)
  (unimp 'rectnum-multiply))

(define (rectnum-divide a b)
  (unimp 'rectnum-divide))

(define (rectnum-zero? a)
  (zero? (rectnum-real-part a)))

(define (rectnum-positive? a)
  (unimp 'rectnum-positive?))

(define (rectnum-negative? a)
  (unimp 'rectnum-negative?))

(define (rectnum=? a b)
  (unimp 'rectnum=?))

(define (rectnum>? a b)
  (unimp 'rectnum>?))

(define (rectnum<? a b)
  (unimp 'rectnum<?))

(define (rectnum>=? a b)
  (unimp 'rectnum>=?))

(define (rectnum<=? a b)
  (unimp 'rectnum<=?))

(define (unimp x)
  (error x "Unimplemented operation"))

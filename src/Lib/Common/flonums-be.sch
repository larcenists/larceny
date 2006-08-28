; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; 30 April 1999 / lth
;
; Flonum constructors and accessors for big-endian 32-bit architectures.
; Depends on both flonum and bignum representations.
;
; Larceny flonums are represented as a bytevector of 12 bytes, where the
; first four are unused (may be garbage) and the remaining eight hold an 
; IEEE flonum in native endianness.
;
; Larceny bignums are represented as a bytevector with word-sized data
; in native endianness.  The first word contains the sign (high two bytes)
; and the number of word-sized bigits (low two bytes).  The remaining
; words contain bigits.

($$trace "flonums-be")

; Create a boxed flonum from a bignum:
;   `s' is 0 or 1, a fixnum.
;   `m', the mantissa (with the leading 1 present), is a bignum with 
;        exactly 53 significant bits, and the high bit is 1.
;   `e', the unbiased exponent, is a fixnum.
;
; The parameters represent the number -1^s * m.0 * 2^e.
;
; This can be sped up by just doing shifts and stores rather than computing
; the bignum t and copying the bits.

(define make-flonum
  (let ((two^52 4503599627370496)
	(two^63 9223372036854775808))
    (lambda (s m e)
      (let ((t (+ (if (zero? s) 0 two^63)
		  (* (+ e 1023) two^52)
		  (remainder m two^52)))
	    (f (make-flonum-datum)))
	; t is a normalized bignum.
	(bytevector-like-set! f 4  (bytevector-like-ref t 8))
	(bytevector-like-set! f 5  (bytevector-like-ref t 9))
	(bytevector-like-set! f 6  (bytevector-like-ref t 10))
	(bytevector-like-set! f 7  (bytevector-like-ref t 11))
	(bytevector-like-set! f 8  (bytevector-like-ref t 4))
	(bytevector-like-set! f 9  (bytevector-like-ref t 5))
	(bytevector-like-set! f 10 (bytevector-like-ref t 6))
	(bytevector-like-set! f 11 (bytevector-like-ref t 7))
	f))))

; Return the fraction of a flonum as a nonnegative bignum.

(define float-significand
  (let ((two^52 4503599627370496))
    (lambda (f)
      (let ((n (make-bytevector 12)))
	(bytevector-set! n 0 0)
	(bytevector-set! n 1 0)		; sign
	(bytevector-set! n 2 0)
	(bytevector-set! n 3 2)		; size
	(bytevector-set! n 4 (bytevector-like-ref f 8))
	(bytevector-set! n 5 (bytevector-like-ref f 9))
	(bytevector-set! n 6 (bytevector-like-ref f 10))
	(bytevector-set! n 7 (bytevector-like-ref f 11))
	(bytevector-set! n 8 0)
	(bytevector-set! n 9 (fxlogior 16 (fxlogand 15 (bytevector-like-ref f 5))))
	(bytevector-set! n 10 (bytevector-like-ref f 6))
	(bytevector-set! n 11 (bytevector-like-ref f 7))

	; Subtract hidden bit if x is denormalized or zero.

	(typetag-set! n sys$tag.bignum-typetag)
	(if (and (zero? (fxlogand 127 (bytevector-like-ref f 4)))
		 (zero? (fxlogand -16 (bytevector-like-ref f 5))))
	    (- n two^52)
	    n)))))

; Return the unbiased exponent of a flonum as a fixnum.
;
; The returned value is the "natural" exponent in the sense that if
; F is a nonnegative flonum, then 
;        F = (float-significand F) * (expt 2 (float-exponent F))

(define float-exponent
  (let ((flonum:minexponent-51 -1074))
    (lambda (f)
      (let ((e (fxlogior (fxlsh (fxlogand 127 (bytevector-like-ref f 4)) 4)
		       (fxrshl (bytevector-like-ref f 5) 4))))
	(if (zero? e)
	    flonum:minexponent-51	; no hidden bit
	    (- e (+ 1023 52)))))))

; Return the unbiased exponent of a flonum as a fixnum.
;
; The returned value is just the exponent field with the bias subtracted,
; thus not the "natural" exponent.

(define (float-unbiased-exponent f)
  (let ((e (fxlogior (fxlsh (fxlogand 127 (bytevector-like-ref f 4)) 4)
		   (fxrshl (bytevector-like-ref f 5) 4))))
    (- e 1023)))

; Return the sign of the flonum as 0 (positive) or 1 (negative).

(define (float-sign f)
  (fxrshl (bytevector-like-ref f 4) 7))

; eof

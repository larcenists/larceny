; Low-level floatingpoint stuff.
;
; The procedures in this file all operate on IEEE flonums.
; Formats of flonums and bignums and operations used are all specific
; to Larceny.
;
; $Id$
;
; Based on code from MacScheme. Copyright Lightship Software.
;
; Larceny flonums are represented as a bytevector of 12 bytes, where the
; first four are unused and the remaining eight is an IEEE flonum.
;
; Larceny bignums are represented as a bytevector, where the first two bytes
; have the sign (#x0000 or #xFFFF), the next two have the number of 32-bit
; digits, and the remaining bytes are grouped in 4s as 32-bit digits.
;
; See the documentation for details.

; Rip out the fraction and stuff it into a bignum.
; The fraction is always positive.

(begin
  (display "$Id$")
  (newline))

(define float-significand
  (let ((two^n-1 (expt 2 52)))
    (lambda (x)
      (let ((n (make-bytevector 12)))
	(bytevector-set! n 0 0)
	(bytevector-set! n 1 0)	                   ; sign
	(bytevector-set! n 2 0)
	(bytevector-set! n 3 2)                    ; size
	(bytevector-set! n 4 (bytevector-like-ref x 8))
	(bytevector-set! n 5 (bytevector-like-ref x 9))
	(bytevector-set! n 6 (bytevector-like-ref x 10))
	(bytevector-set! n 7 (bytevector-like-ref x 11))
	(bytevector-set! n 8 0)
	(bytevector-set! n 9 (logior 16 (logand 15 (bytevector-like-ref x 5))))
	(bytevector-set! n 10 (bytevector-like-ref x 6))
	(bytevector-set! n 11 (bytevector-like-ref x 7))
        
	; subtract hidden bit if x is denormalized
        
	(typetag-set! n sys$bignum-typetag)
	(if (and (zero? (logand 127 (bytevector-like-ref x 4)))
		 (zero? (logand -16 (bytevector-like-ref x 5))))
	    (- n two^n-1)
	    n)))))

; Rip out the exponent and return it as a fixnum.

(define float-exponent
  (let ((flonum:minexponent -1023))
    (lambda (x)
      (let ((e (logior (lsh (logand 127 (bytevector-like-ref x 4)) 4)
		       (rshl (bytevector-like-ref x 5) 4))))
	(if (zero? e)
	    (- flonum:minexponent 51)           ; no hidden bit
	    (- e (+ 1023 52)))))))

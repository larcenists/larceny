; *Way* low-level floating point stuff.
;
; $Id: flonum-stuff.sch,v 1.3 92/02/17 18:26:56 lth Exp Locker: lth $
;
; The procedures in this file all operate on IEEE flonums.
; Formats of flonums and bignums and operations used are all specific
; to Larceny.
;
; Some is based on code from MacScheme; Copyright Lightship Software.
;
; Larceny flonums are represented as a bytevector of 12 bytes, where the
; first four are unused and the remaining eight is an IEEE flonum.
;
; Larceny bignums are represented as a bytevector, where the first two bytes
; have the sign (0 or 1, for positive or negative), the next two have the 
; number of 32-bit digits, and the remaining bytes are grouped in 4s as 32-bit
; digits.
;
; NEEDS TESTING!

; exports

(define flonum? #f)                 ; eventually a primop
(define compnum? #f)                ; ditto
(define float-significand #f)
(define float-exponent #f)
(define float-sign #f)
(define make-compnum #f)
(define bignum->flonum #f)
(define flonum->bignum #f)
(define flonum->integer #f)

(let ()

  (define bits-per-bigit 16)         ; depends on bignums.scm also!

; Until the compiler can do this at compile time, we need to have the
; constants here explicitly, because bignum arithmetic is not available.
;
;  (define two^52 (expt 2 52))
;  (define two^63 (expt 2 63))
;  (define e1 (expt 2 bits-per-bigit))

  (define e1 65536)                  ; 2^(bits-per-bigit)
  (define e2 4294967296)             ; 2^(bits-per-bigit*2)
  (define e3 281474976710656)        ; 2^(bits-per-bigit*3)
  (define e4 18446744073709551616)   ; 2^(bits-per-bigit*4)

  (define two^52 4503599627370496)
  (define two^53 9007199254740992)
  (define two^54 18014398509481984)
  (define two^63 9223372036854775808)

  (define minexponent -1023)

  ; Is it a flonum?

  (define (%flonum? x)
    (and (bytevector-like? x)
	 (= (typetag x) sys$tag.flonum-typetag)))

  ; Rip out the fraction and stuff it into a bignum.
  ; The fraction is always positive.

  (define (%float-significand x)
    (let ((n (make-bytevector 12)))
      (bytevector-set! n 0 0)
      (bytevector-set! n 1 0)		; sign
      (bytevector-set! n 2 0)
      (bytevector-set! n 3 2)		; size
      (bytevector-set! n 4 (bytevector-like-ref x 8))
      (bytevector-set! n 5 (bytevector-like-ref x 9))
      (bytevector-set! n 6 (bytevector-like-ref x 10))
      (bytevector-set! n 7 (bytevector-like-ref x 11))
      (bytevector-set! n 8 0)
      (bytevector-set! n 9 (logior 16 (logand 15 (bytevector-like-ref x 5))))
      (bytevector-set! n 10 (bytevector-like-ref x 6))
      (bytevector-set! n 11 (bytevector-like-ref x 7))
        
      ; subtract hidden bit if x is denormalized
        
      (typetag-set! n sys$tag.bignum-typetag)
      (if (and (zero? (logand 127 (bytevector-like-ref x 4)))
	       (zero? (logand -16 (bytevector-like-ref x 5))))
	  (- n two^52)
	  n)))

  ; Rip out the exponent and return it as a fixnum.

  (define (%float-exponent x)
    (let ((e (logior (lsh (logand 127 (bytevector-like-ref x 4)) 4)
		     (rshl (bytevector-like-ref x 5) 4))))
      (if (zero? e)
	  (- minexponent 51)           ; no hidden bit
	  (- e (+ 1023 52)))))


  ; Create a boxed flonum from a bignum on a special format.
  ;
  ; `s' is 0 or 1, a fixnum.
  ; `m', the mantissa (with the leading 1 present), is a bignum.
  ; `e', the exponent (unbiased), is a fixnum.
  ;
  ; The parameters represent the number -1^s * m.0 * 2^e.

  (define (make-flonum s m e)
    (lambda (s m e)
      (let ((t (bignum-add (if (zero? s) 0 two^63)
			   (bignum-multiply (fixnum->bignum (+ e (- 1023 53)))
					    two^52)
			   (bignum-remainder m two^52)))
	    (f (make-bytevector 12)))
	(bytevector-set! f 4  (bytevector-ref t 8))
	(bytevector-set! f 5  (bytevector-ref t 9))
	(bytevector-set! f 6  (bytevector-ref t 10))
	(bytevector-set! f 7  (bytevector-ref t 11))
	(bytevector-set! f 8  (bytevector-ref t 4))
	(bytevector-set! f 9  (bytevector-ref t 5))
	(bytevector-set! f 10 (bytevector-ref t 6))
	(bytevector-set! f 11 (bytevector-ref t 7))
	(typetag-set! f 'flonum)
	f)))

  ; Return the sign of the flonum.

  (define (%float-sign f)
    (quotient (bytevector-ref f 4) 127))


  ; Convert a bignum to an IEEE double precision number.
  ;
  ; Knows about range of IEEE double precision, but oblivious of bignum
  ; representation.
  ;
  ; Not tested (and not trusted).

  (define %bignum->flonum
    (let ()

      ; used for rounding

      (define sticky #f)
      (define non-zero-tail #f)

      
      ; Count leading zeroes in a bigit by shifting right (there are
      ; better ways, but this will do). `n' and `e' are always fixnums.

      (define (leading-zeroes n e)
	(if (zero? n)
	    (- bits-per-bigit e)
	    (leading-zeroes (rsha n 2) (+ e 1))))

      ; `m' and `limit' are always bignums.

      (define (adjust m limit)
	(if (< m limit)
	    m
	    (begin (set! sticky (or sticky (bignum-odd? m)))
		   (adjust (bignum-quotient m 2) limit))))

      ; Rounds to nearest, and to even on ties.
      
      (define (round m)
	(if (odd? m)
	    (if (or sticky non-zero-tail)
		(+ m 1)
		(if (>= (bignum-remainder m 4) 2) ; ick.
		    (+ m 1)
		    m))
	    m))

      ; main

      (lambda (b)
	(if (bignum-zero? b)
	    0.0
	    (let* ((l  (bignum-length b))
		   (d4 (bignum-ref b (- l 1)))
		   (d3 (if (> l 1) (bignum-ref b (- l 2)) 0))
		   (d2 (if (> l 2) (bignum-ref b (- l 3)) 0))
		   (d1 (if (> l 3) (bignum-ref b (- l 4)) 0))
		   (d0 (if (> l 4) (bignum-ref b (- l 5)) 0))
		   (e  (- (* l bits-per-bigit)
			  (leading-zeroes d4 0)))
		   (m  (+ (* d4 e4) (* d3 e3) (* d2 e2) (* d1 e1) d0)))
	      (set! sticky #f)
	      (set! non-zero-tail #f)

	      ; figure out if the tail of the bignum is nonzero

	      (let loop ((i (- l 6)))
		(if (>= i 0)
		    (begin (set! non-zero-tail
				 (or non-zero-tail
				     (not (zero? (bignum-ref b i)))))
			   (loop (- i 1)))))

					; shift, round, convert

	      (let ((m (round (adjust m two^54))))
		(make-flonum (if (bignum-negative? b) 1 0)
			     (adjust m two^53)
			     e)))))))

  ; Convert a flonum to a bignum. If the flonum is not representable as an
  ; integer, then the excess fraction is simply dropped.
  ;
  ; Knows about the representation of flonums as well as bignums.
  ; Flonums are IEEE double, boxed as a bytevector.
  ;
  ; Not tested.

  (define (%flonum->bignum f)

    ; convert int to bignum

    (define (->bignum x)
      (if (fixnum? x)
	  (fixnum->bignum x)
	  x))

    ; main

    (let ((m (flonum-mantissa f))
	  (e (flonum-exponent f)))
      (cond ((and (zero? m) (zero? e))
	     (fixnum->bignum 0))
	    ((= e 1024)
	     (error 'flonum->bignum "Cannot convert NaN to bignum."))
	    (else
	     (let* ((e (- e 53))
		    (q (cond ((= e 0)
			      m)
			     ((< e -53)
			      (fixnum->bignum 0))
			     ((< e 0)
			      (bignum-quotient m (->bignum (expt 2 (abs e)))))
			     (else
			      (bignum-multiply m (->bignum (expt 2 e)))))))
	       (bignum-limited-normalize! (if (not (zero? (flonum-sign f)))
					      (bignum-negate! q)
					      q)))))))



  (define (%flonum->integer a)
    (bignum-normalize! (%flonum->bignum a)))

  (define (%compnum? obj)
    (and (bytevector-like? obj)
	 (= (typetag obj) sys$tag.compnum-typetag)))

  (define (%make-compnum real imag)

    (define (cp from i to j c)
      (if (zero? c)
	  #t
	  (begin (bytevector-like-set! to j (bytevector-like-ref from i))
		 (cp from (+ i 1) to (+ j 1) (- c 1)))))

    (if (not (and (flonum? real) (flonum? imag)))
	(error "Arguments to make-compnum must be flonums.")
	(let ((b (make-bytevector 20)))
	  (cp real 8 b 8 8)
	  (cp imag 8 b 16 8)
	  (typetag-set! b sys$tag.compnum-typetag)
	  b)))

  ; install-flonum-stuff

  (set! flonum? %flonum?)
  (set! compnum? %compnum?)
  (set! float-significand %float-significand)
  (set! float-exponent %float-exponent)
  (set! float-sign %float-sign)
  (set! make-compnum %make-compnum)
  (set! bignum->flonum %bignum->flonum)
  (set! flonum->bignum %flonum->bignum)
  (set! flonum->integer %flonum->integer)

  #t)

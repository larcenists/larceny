; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; 18 December 1998 / wdc
;
; Larceny -- low-level floating point code
;
; The procedures in this file all operate on IEEE flonums.  Formats of
; flonums and bignums and operations used are all specific to Larceny.
;
; Larceny flonums are represented as a bytevector of 12 bytes, where the
; first four are unused and the remaining eight is an IEEE flonum.  The
; first four may be garbage and should not be used in comparisons.
;
; Larceny bignums are represented as a bytevector, where the first two bytes
; have the sign (0 or 1, for positive or negative), the next two have the 
; number of 32-bit digits, and the remaining bytes are grouped in 4s as 32-bit
; digits.

($$trace "flonums")

; exports

(define float-significand)
(define float-exponent)
(define float-sign)
(define make-compnum)
(define bignum->flonum)
(define flonum->bignum)
(define flonum->integer)
(define flonum->ratnum)
(define make-raw-flonum)
(define make-flonum)

(let ()

  (define bits-per-bigit 16)		; depends on bignums.scm also!

  (define two^52 4503599627370496)
  (define two^53 9007199254740992)
  (define two^54 18014398509481984)
  (define two^63 9223372036854775808)

  (define flonum:minexponent    -1023)
  (define flonum:minexponent-51 -1074)
  (define flonum:maxexponent     1023)
  (define flonum:zero             0.0)
  (define flonum:infinity      +inf.0)
  
  (define (flonum-infinity? x)
    (or (= x +inf.0) (= x -inf.0)))

  (define (flonum-nan? x)
    (not (= x x)))

  ; Rip out the fraction and stuff it into a bignum.
  ; The fraction is always positive and exact.

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

      ; subtract hidden bit if x is denormalized or zero

      (typetag-set! n sys$tag.bignum-typetag)
      (if (and (zero? (logand 127 (bytevector-like-ref x 4)))
	       (zero? (logand -16 (bytevector-like-ref x 5)))) ; bletch.
	  (- n two^52)
	  n)))

  ; Rip out the exponent and return it unbiased as a fixnum.
  ; The exponent returned has a value such that if we have a flonum F
  ; then let f=(float-significand F)
  ;      and e=(float-exponent F)
  ; and it will be the case that F=f*2^e.

  (define (%float-exponent x)
    (let ((e (logior (lsh (logand 127 (bytevector-like-ref x 4)) 4)
		     (rshl (bytevector-like-ref x 5) 4))))
      (if (zero? e)
	  flonum:minexponent-51       ; no hidden bit
	  (- e (+ 1023 52)))))

  ; Rip out the exponent and return it unbiased as a fixnum.
  ; This is used internally by flonum->bignum.

  (define (%float-raw-exponent x)
    (let ((e (logior (lsh (logand 127 (bytevector-like-ref x 4)) 4)
		     (rshl (bytevector-like-ref x 5) 4))))
      (- e 1023)))

  ; Return the sign of the flonum as 0 (positive) or 1 (negative).

  (define (%float-sign f)
    (rshl (bytevector-like-ref f 4) 7))


  ; Create a boxed flonum from a bignum on a special format.
  ;
  ; `s' is 0 or 1, a fixnum.
  ; `m', the mantissa (with the leading 1 present), is a bignum with 
  ;      exactly 53 significant bits, and the high bit is 1.
  ; `e', the exponent (unbiased), is a fixnum.
  ;
  ; The parameters represent the number -1^s * m.0 * 2^e. In fact,
  ; if F is a flonum then it is true that
  ;
  ;      (= F (make-flonum (flonum-sign F) 
  ;                        (flonum-significand F) 
  ;                        (flonum-exponent F)))
  ;
  ; We can make this faster by avoiding the arithmetic and just do
  ; shifts and stores.

  (define (%make-flonum s m e)
    (let ((t (+ (if (zero? s) 0 two^63)
		(* (+ e 1023) two^52)
		(remainder m two^52)))
	  (f (%make-raw-flonum)))
      ; t is now a normalized bignum.
      (bytevector-like-set! f 4  (bytevector-like-ref t 8))
      (bytevector-like-set! f 5  (bytevector-like-ref t 9))
      (bytevector-like-set! f 6  (bytevector-like-ref t 10))
      (bytevector-like-set! f 7  (bytevector-like-ref t 11))
      (bytevector-like-set! f 8  (bytevector-like-ref t 4))
      (bytevector-like-set! f 9  (bytevector-like-ref t 5))
      (bytevector-like-set! f 10 (bytevector-like-ref t 6))
      (bytevector-like-set! f 11 (bytevector-like-ref t 7))
      f))

  (define (%make-raw-flonum)
    (let ((b (make-bytevector 12)))
      (typetag-set! b sys$tag.flonum-typetag)
      b))


  ; Convert a bignum to an IEEE double precision number.

  (define (%bignum->flonum b)
    
    ; Count leading zeroes in a bigit by shifting right.
    ; `n' and `e' are always nonnegative fixnums.
    
    (define (leading-zeroes n e)
      (if (zero? n)
          (- bits-per-bigit e)
          (leading-zeroes (rsha n 1) (+ e 1))))
    
    ; Returns the greatest fixnum n such that (<= (expt 2 n) (abs b)).
    
    (define (floor-of-lg b)
      (let* ((l (bignum-length b))
             (v (bignum-ref b (- l 1))))
        (- (* l bits-per-bigit) (leading-zeroes v 0) 1)))
    
    ; Given:  b0 and e0 such that
    ;     2^52 <= b0 < 2^53 <= b0 * 2^{e0} <= b < (b0 + 1) * 2^{e0}
    ; Returns:  Best flonum approximation to b.
    
    (define (convert-large b0 e0)
      (if (> e0 flonum:maxexponent)
          flonum:infinity
          (let* ((x0 (%make-flonum 0 b0 e0))
                 (x1 (%make-flonum 0 (+ b0 1) e0))
                 (d0 (- b (%flonum->bignum x0)))
                 (d1 (- (%flonum->bignum x1) b)))
            (cond ((< d0 d1) x0)
                  ((< d1 d0) x1)
                  ((even? b0) x0)
                  (else x1)))))
    
    ; Bignums are not usually zero, but it happens in system code.
    
    (cond ((negative? b)
           (- (%bignum->flonum (- b))))
          ((zero? b)
           flonum:zero)
          (else
           (let* ((e (floor-of-lg b))
                  (e-52 (- e 52)))
             (if (<= e-52 0)
                 (%make-flonum 0
                               (* b (expt 2 (- e-52)))
                               e)
                 (convert-large (quotient b (expt 2 e-52))
                                e))))))

  ; Convert an exact integer (fixnum or bignum) to a bignum

  (define (->bignum x)
    (if (fixnum? x) (fixnum->bignum x) x))

  ; Convert a flonum to a bignum by truncating it if necessary.
  ; This must work properly in the case of a compnum with zero imag part!

  (define (%flonum->bignum f)
    (if (or (flonum-infinity? f)
	    (flonum-nan? f))
	(error "Can't convert " f " to an exact number."))
    (let ((q (->bignum
	      (let* ((f (round f))
		     (m (%float-significand f))
		     (e (%float-raw-exponent f)))
		(cond ((= e 52)
		       m)
		      ((> e 52)
		       (* m (expt 2 (- e 52))))
		      ((< e 0)
		       0)
		      (else
		       ; 0 < e < 52
		       (let* ((divisor (expt 2 (abs (- e 52))))
			      (q       (quotient m divisor)))
			 q)))))))
      (if (not (zero? (float-sign f)))
	  (flip-sign! q))
      q))

  ; Convert a flonum to an exact integer.

  (define (%flonum->integer a)
    (big-normalize! (%flonum->bignum a)))

  ; Given two flonums 'real' and 'imag' create a compnum from the two.
  ; FIXME: needs to be a primop, really (although an improvement would be
  ; to use sys$bvl-copy-into!).

  (define (%make-compnum real imag)

    (define (cp from i to j c)
      (if (zero? c)
	  #t
	  (begin (bytevector-like-set! to j (bytevector-like-ref from i))
		 (cp from (+ i 1) to (+ j 1) (- c 1)))))

    (if (not (and (flonum? real) (flonum? imag)))
	(begin
	  (error "make-compnum: not a flonum: " (if (flonum? real) imag real))
	  #t)
	(let ((b (make-bytevector 20)))
	  (cp real 4 b 4 8)
	  (cp imag 4 b 12 8)
	  (typetag-set! b sys$tag.compnum-typetag)
	  b)))

  ; For internal use only.
  ; Flonum should not be an integer (although this implementation works
  ; anyway).

  (define (%flonum->ratnum f)
    (if (or (flonum-infinity? f)
	    (flonum-nan? f))
	(error "Can't convert " f " to an exact number."))
    (let ((q (let* ((m (%float-significand f))
		    (e (%float-raw-exponent f)))
	       (cond ((>= e 52)
		      (* m (expt 2 (- e 52))))
		     (else
		      (/ m (expt 2 (abs (- e 52)))))))))
      (if (not (zero? (%float-sign f)))
	  (- q)
	  q)))

  ; install-flonum-stuff

  (set! float-significand %float-significand)
  (set! float-exponent %float-exponent)
  (set! float-sign %float-sign)
  (set! make-compnum %make-compnum)
  (set! bignum->flonum %bignum->flonum)
  (set! flonum->bignum %flonum->bignum)
  (set! flonum->integer %flonum->integer)
  (set! flonum->ratnum %flonum->ratnum)
  (set! make-raw-flonum %make-raw-flonum)
  (set! make-flonum %make-flonum)

  #t)

; eof

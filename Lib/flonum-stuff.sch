; *Way* low-level floating point stuff.
;
; $Id: flonum-stuff.sch,v 1.5 92/03/31 12:31:01 lth Exp $
;
; The procedures in this file all operate on IEEE flonums.
; Formats of flonums and bignums and operations used are all specific
; to Larceny.
;
; Some is based on code from MacScheme; so
;
;    Copyright Lightship Software.
;
; Larceny flonums are represented as a bytevector of 12 bytes, where the
; first four are unused and the remaining eight is an IEEE flonum.
;
; Larceny bignums are represented as a bytevector, where the first two bytes
; have the sign (0 or 1, for positive or negative), the next two have the 
; number of 32-bit digits, and the remaining bytes are grouped in 4s as 32-bit
; digits.

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
(define flonum->ratnum #f)

(let ()

  (define bits-per-bigit 16)         ; depends on bignums.scm also!

  (define e1 65536)                  ; 2^(bits-per-bigit)
  (define e2 4294967296)             ; 2^(bits-per-bigit*2)
  (define e3 281474976710656)        ; 2^(bits-per-bigit*3)
  (define e4 18446744073709551616)   ; 2^(bits-per-bigit*4)

  (define two^52 4503599627370496)
  (define two^53 9007199254740992)
  (define two^54 18014398509481984)
  (define two^63 9223372036854775808)

  (define flonum:minexponent    -1023)
  (define flonum:minexponent-51 -1074)
  (define flonum:zero           0.0)
  
  ; Is it a flonum?

  (define (%flonum? x)
    (and (bytevector-like? x)
	 (= (typetag x) sys$tag.flonum-typetag)))

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
    (quotient (bytevector-like-ref f 4) 127))


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

  (define (make-flonum s m e)
    (let ((t (+ (if (zero? s) 0 two^63)
		(* (+ e 1023) two^52)
		(remainder m two^52)))
	  (f (make-bytevector 12)))
      ; t is now a normalized bignum.
      (bytevector-set! f 4  (bytevector-like-ref t 8))
      (bytevector-set! f 5  (bytevector-like-ref t 9))
      (bytevector-set! f 6  (bytevector-like-ref t 10))
      (bytevector-set! f 7  (bytevector-like-ref t 11))
      (bytevector-set! f 8  (bytevector-like-ref t 4))
      (bytevector-set! f 9  (bytevector-like-ref t 5))
      (bytevector-set! f 10 (bytevector-like-ref t 6))
      (bytevector-set! f 11 (bytevector-like-ref t 7))
      (typetag-set! f sys$tag.flonum-typetag)
      f))

  ; Convert a bignum to an IEEE double precision number.

  (define (%bignum->flonum b)
    (let ((sticky #f))   ; for rounding

      ; Divide m by 2 until it is less than the limit, setting the sticky
      ; bit if a 1 bit is lost in the process. Return the new m.
      ; `m' and `limit' are always nonnegative bignums.
      ;
      ; We can make this faster by simply doing shifts.

      (define (adjust m limit)
	(if (< m limit)
	    m
	    (begin (set! sticky (or sticky (odd? m)))
		   (adjust (quotient m 2) limit))))

      ; 'Rounds' m to nearest, and to even on ties.
      ; Rounding means adding 1 or not, and waiting for an eventual
      ; division to lop off the least significant bit.
      ;
      ; We can make this faster by operating on the representation.

      (define (round m)
	(if (odd? m)
	    (if sticky
		(+ m 1)
		(if (>= (remainder m 4) 2) ; ick.
		    (+ m 1)
		    m))
	    m))

      (define (convert)
	(set! sticky (non-zero-tail? b))
	(let* ((v  (enough-bigits-for-a-flonum b))
	       (m1 (car v))
	       (e  (cdr v))
	       (m2 (adjust m1 two^54))
	       (m3 (round m2)))
	  (make-flonum (if (negative? b) 1 0)
		       (adjust m3 two^53)
		       (- e 1))))

      ; Bignums are not usually zero, but it happens in system code.

      (if (zero? b)
	  flonum:zero
	  (convert))))


  ; Given a bignum, return another bignum which has enough significant
  ; bits to represent the bignum as an IEEE double. Also return the number
  ; of significant bits in the number.
  ;
  ; This procedure knows that a bigit is 16 bits.

  (define (enough-bigits-for-a-flonum b)

    ; Count leading zeroes in a bigit by shifting right.
    ; `n' and `e' are always nonnegative fixnums.
	
    (define (leading-zeroes n e)
      (if (zero? n)
	  (- bits-per-bigit e)
	  (leading-zeroes (rsha n 1) (+ e 1))))
    
    (let* ((l  (bignum-length b))
	   (d4 (bignum-ref b (- l 1)))
	   (d3 (if (> l 1) (bignum-ref b (- l 2)) 0))
	   (d2 (if (> l 2) (bignum-ref b (- l 3)) 0))
	   (d1 (if (> l 3) (bignum-ref b (- l 4)) 0))
	   (d0 (if (> l 4) (bignum-ref b (- l 5)) 0))
	   (e  (- (* l bits-per-bigit) (leading-zeroes d4 0)))
	   (v  (+ (* d4 e4) (* d3 e3) (* d2 e2) (* d1 e1) d0)))
      ; Should probably use bytevector-like-set! and create a
      ; bignum.
      (cons v e)))
	  

  ; Figure out if the tail of the bignum (that is, those bigits which
  ; will not figure explicitly in the flonum) has any non-zero bigit.
  ; Knows that a bigit has 16 bits.

  (define (non-zero-tail? b)
    (let loop ((i (- (bignum-length b) 6)))
      (cond ((negative? i)
	     #f)
	    ((not (zero? (bignum-ref b i)))
	     #t)
	    (else
	     (loop (- i 1))))))


  ; Convert an exact integer (fixnum or bignum) to a bignum

  (define (->bignum x)
    (if (fixnum? x) (fixnum->bignum x) x))

  ; Convert a flonum to a bignum by truncating it if necessary.

  (define (%flonum->bignum f)
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
			 ;(display "%flonum->bignum case 4:") (newline)
			 ;(display "  m=") (display m) (newline)
			 ;(display "  e=") (display e) (newline)
			 ;(display "  2^abs(e-52)=") (display divisor)(newline)
			 ;(display "  q=") (display q) (newline)
			 q)))))))
      (if (not (zero? (float-sign f)))
	  (flip-sign! q))
      q))

  ; Convert a flonum to an exact integer.

  (define (%flonum->integer a)
    (big-normalize! (%flonum->bignum a)))

  ; Test an object for compnum-ness.

  (define (%compnum? obj)
    (and (bytevector-like? obj)
	 (= (typetag obj) sys$tag.compnum-typetag)))

  ; Given two flonums 'real' and 'imag' create a compnum from the two.

  (define (%make-compnum real imag)

    (define (cp from i to j c)
      (if (zero? c)
	  #t
	  (begin (bytevector-like-set! to j (bytevector-like-ref from i))
		 (cp from (+ i 1) to (+ j 1) (- c 1)))))

    (if (not (and (flonum? real) (flonum? imag)))
	(error "make-compnum: not a flonum: " (if (flonum? real) imag real))
	(let ((b (make-bytevector 20)))
	  (cp real 8 b 8 8)
	  (cp imag 8 b 16 8)
	  (typetag-set! b sys$tag.compnum-typetag)
	  b)))

  ; For internal use only.
  ; Flonum should not be an integer (although this implementation works
  ; anyway).

  (define (%flonum->ratnum f)
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

  (set! flonum? %flonum?)
  (set! compnum? %compnum?)
  (set! float-significand %float-significand)
  (set! float-exponent %float-exponent)
  (set! float-sign %float-sign)
  (set! make-compnum %make-compnum)
  (set! bignum->flonum %bignum->flonum)
  (set! flonum->bignum %flonum->bignum)
  (set! flonum->integer %flonum->integer)
  (set! flonum->ratnum %flonum->ratnum)

  #t)

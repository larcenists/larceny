; Convert various data into bytevector representation.
; The bytevector header is not included in the returned bytevector.
;
; These support routines run under Chez Scheme and other implementations
; without native support for byte vectors.
;
; $Id: misc2bytevector.ss,v 1.1 92/01/31 16:47:04 lth Exp Locker: lth $

; Generic.

(define (string->bytevector s)
  (list->bytevector (map char->integer (string->list s))))

; Generic.

(define (symbol->bytevector s)
  (string->bytevector (symbol->string s)))

; For implementations w/o bytevectors.
; (Implementations w/ bytevectors should have this in the std library.)

(define (list->bytevector l)
  (list->vector l))

; SPARC specific.
;
; Bignums are bytevector-like with the sign in the first two bytes
; (0 for 0 or positive, 1 for negative), followed by a digit
; count (two bytes) and then base-2^32 digits in the next words.
; with the least significant word first.
;
;	+------------------------+--------+
;	|       length           | header |
;	+------------------------+--------+
;	| sign          |   digitcount    |
;	+---------------------------------+
;	|              lsd                |
;	+---------------------------------+
;	...

(define (bignum->bytevector b)

  (define two^32 (expt 2 32))

  (define (flatten x)
    (apply append x))

  ; returned list has length congruent to 0 mod 4.

  (define (divide b l)
    (if (< b two^32)
	(flatten (reverse (cons (split-int b) l)))
	(divide (quotient b two^32)
		(cons (split-int (remainder b two^32)) l))))

  (let* ((sign   (if (negative? b) '(0 1) '(0 0)))
	 (b      (abs b))
	 (digits (divide b '()))
	 (len    (quotient (length digits) 4))
	 (count  (list (quotient len 256) (remainder len 256))))
    (list->bytevector
     (append sign count digits))))


; IEEE specific, and specific to Chez Scheme.
;
; Flonums (IEEE double) are bytevector-like. The first word is unused. The two
; next words contain the double:
;
;	+------------------------+--------+
;	|      length            | header |
;	+------------------------+--------+
;	|      unused                     |
;	+---------------------------------+
;	|      IEEE double precision      |
;	|                                 |
;	+---------------------------------+
;
; An IEEE number, in turn, is represented as follows (64 bits)
;
;       +-+----------+--------------------+
;       |s| exponent |                    |    low word
;       +-+----------+--------------------+
;       |                                 |    high word
;       +---------------------------------+
;
; where the sign (s) is one bit, the exponent is 11 bits, and the fraction is
; the remaining 52 bits. The exponent is biased with 1023, and the fraction
; is normalized with a hidden leading '1' bit.
;
; Under Chez Scheme, there are peculiar global variables to represent NaNs and
; Infs: +inf.0, -inf.0, +nan.0, and -nan.0. These "numbers" have specific
; representations in the IEEE format, and are special-cased below. For NaNs,
; the sign is to be ignored (in fact, +nan.0 and -nan.0 both evaluate to
; +nan.0), and we always give these numbers a 0 sign bit.
;
; In Chez Scheme v4, all NaNs are equal?.
;
; There is no provision for denormals here.

(define (flonum->bytevector f)

  (define nan 
    (list->bytevector '(0 0 0 0 #x7f #xff #xff #xff #xff #xff #xff #xff)))

  (define infinity+
    (list->bytevector '(0 0 0 0 #x7f #xf8 #x00 #x00 #x00 #x00 #x00 #x00)))

  (define infinity-
    (list->bytevector '(0 0 0 0 #xff #xf8 #x00 #x00 #x00 #x00 #x00 #x00)))

  (define zero+
    (list->bytevector '(0 0 0 0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))

  (define zero-
    (list->bytevector '(0 0 0 0 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))

  ; I'm sure there are portable ways of figuring out the bit patterns of the
  ; fraction and the exponent. For now we'll just observe that Chez Scheme
  ; uses IEEE doubles and that we can write a C routine which returns the 
  ; number as a string.

  (define (bitpattern f)
    (let ((i1 (unix$bitpattern 0 f))
	  (i2 (unix$bitpattern 1 f)))
      (append (split-int i1) (split-int i2))))

  (cond ((or (equal? f -nan.0) (equal? f +nan.0))
	 nan)
	((equal? f +inf.0)
	 infinity+)
	((equal? f -inf.0)
	 infinity-)
	(else
	 (list->bytevector (append '(0 0 0 0) (bitpattern f))))))

; Begin gross hack.

(if (not (bound? 'has-loaded-bitpattern))
    (load-foreign "../Chez/bitpattern.o"))
(set! has-loaded-bitpattern #t)

(define unix$bitpattern
  (foreign-procedure "bitpattern" (integer-32 double-float) unsigned-32))

; End gross hack.

; utility

(define (split-int b)
  (define two^32 (expt 2 32))
  (define two^24 (expt 2 24))
  (define two^16 (expt 2 16))
  (define two^8  (expt 2 8))

  (list (quotient b two^24)
	(quotient (remainder b two^24) two^16)
	(quotient (remainder b two^16) two^8)
	(remainder b two^8)))


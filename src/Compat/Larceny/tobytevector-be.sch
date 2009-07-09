; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Convert various data into bytevector representation, big-endian.
; The bytevector header is not included in the returned bytevector.
;
; Implementation specific to Larceny.

(if (not (eq? (nbuild-parameter 'target-endianness) 'big))
    (error "tobytevector-be.sch is only for big-endian targets."))

(define (flat1->bytevector s)
  (list->bytevector (map char->integer (string->list s))))

(define (symbol->bytevector s)
  (flat1->bytevector (symbol->string s)))

; Bignums are bytevector-like with the sign in the high byte of
; the first word (0 for 0 or positive, 1 for negative), a digit
; count in the low 24 bits (three bytes) and then base-2^32 digits
; in the next words with the least significant word first.
;
;       big end                  little end
;       +------------------------+--------+
;       |       length           | header |
;       +------------------------+--------+
;       | sign   |          digitcount    |
;       +---------------------------------+
;       |              lsd                |
;       +---------------------------------+
;       ...

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

  (let* ((sign   (if (negative? b) 1 0))
         (b      (abs b))
         (digits (divide b '()))
         (len    (quotient (length digits) 4)))
    (list->bytevector
     (append (split-int (+ len (fxlsh sign 24))) digits))))


; IEEE specific
;
; Flonums (IEEE double) are bytevector-like. The first word is unused. The two
; next words contain the double:
;
;       +------------------------+--------+
;       |      length            | header |
;       +------------------------+--------+
;       |      unused                     |
;       +---------------------------------+
;       |      IEEE double precision      |
;       |                                 |
;       +---------------------------------+
;
; Compnums are similar:
;
;       +------------------------+--------+
;       |      length            | header |
;       +------------------------+--------+
;       |      unused                     |
;       +---------------------------------+
;       |      (real part)                |
;       |      IEEE double precision      |
;       +---------------------------------+
;       |      (imaginary part)           |
;       |      IEEE double precision      |
;       +---------------------------------+
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
; There is no provision for denormals here.

(define (flonum->bytevector f)
  (list->bytevector (append '(0 0 0 0) (flonum-bits f))))

(define (compnum->bytevector c)
  (let ((f1 (flonum-bits (real-part c)))
        (f2 (flonum-bits (imag-part c))))
    (list->bytevector (append '(0 0 0 0) f1 f2))))

; Return a list of byte values representing an IEEE double precision number,
; in big-endian order.

(define (flonum-bits f)
  (let ((bits (let loop ((i 4) (l '()))
                (if (= i 12)
                    l
                    (loop (+ i 1) (cons (bytevector-like-ref f i) l))))))
    (if (eq? (nbuild-parameter 'host-endianness) 'little)
        bits
        (reverse bits))))

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

; eof

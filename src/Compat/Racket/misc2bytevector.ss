; Convert various data into bytevector representation for the Racket host.
; The bytevector header is not included in the returned bytevector.
;
; Handles both endiannesses and both 32-bit and 64-bit targets;
; see Compat/MzScheme/misc2bytevector.ss for the representations.

(define (target-little-endian?)
  (eq? (nbuild-parameter 'target-endianness) 'little))

(define (target-word-bytes)
  (quotient (nbuild-parameter 'word-size) 8))

(define (string->bytevector s)
  (list->bytevector (map char->integer (string->list s))))

(define (symbol->bytevector s)
  (string->bytevector (symbol->string s)))

; Bignums are bytevector-like.  The first 32 bits hold the sign in the
; high byte (0 for 0 or positive, 1 for negative) and a digit count in
; the low 24 bits; then follow base-2^32 digits, least significant digit
; first.  On 64-bit targets the digits are still 32 bits, and the data
; length is still a multiple of 4 (the heap dumper pads to a word).

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

; Flonums and compnums are bytevector-like.  On 32-bit targets the
; first 4 bytes of data are unused (so the doubles are 8-byte aligned);
; on 64-bit targets the doubles immediately follow the header.

(define (flonum-pad)
  (if (= (target-word-bytes) 4) '(0 0 0 0) '()))

(define (flonum->bytevector f)
  (list->bytevector (append (flonum-pad) (flonum-bits f))))

(define (compnum->bytevector c)
  (let ((f1 (flonum-bits (real-part c)))
        (f2 (flonum-bits (imag-part c))))
    (list->bytevector (append (flonum-pad) f1 f2))))

; Return a list of byte values representing an IEEE double precision
; number, in target byte order.

(define (flonum-bits x)
  (let ((bv (real->floating-point-bytes (exact->inexact x)
                                        8
                                        (not (target-little-endian?)))))
    (bytevector->list bv)))

; utility: 32-bit integer to 4 bytes in target byte order.

(define (split-int b)
  (define two^24 (expt 2 24))
  (define two^16 (expt 2 16))
  (define two^8  (expt 2 8))
  (let ((l (list (remainder b two^8)
                 (quotient (remainder b two^16) two^8)
                 (quotient (remainder b two^24) two^16)
                 (quotient b two^24))))
    (if (target-little-endian?) l (reverse l))))

; eof

; Copyright 1999 Lars T Hansen
;
; $Id$
;
; More compatibility code for Gambit-C  --  this code will be compiled
; by compile-gambit-files.

(define (twobit-format port fmt . rest)
  (if (not port)
      (call-with-output-string 
       (lambda (out)
         (apply format out fmt rest)))
      (apply format port fmt rest)))

(define (write-lop x p)
  (write x p)
  (newline p)
  (newline p))

(define write-fasl-datum write)

(define (cerror . irritants)
  (display "Error: ")
  (for-each display irritants)
  (newline)
  (reset))

(define (vector-copy x)
  (list->vector (vector->list x)))

(define (list-copy x)
  (append x '()))

(define (append! . args)

  (define (loop rest tail)
    (cond ((null? rest)
	   tail)
	  ((null? (car rest))
	   (loop (cdr rest) tail))
	  (else
	   (loop (cdr rest)
		 (begin (set-cdr! (last-pair (car rest)) tail)
			(car rest))))))

  (if (null? args)
      '()
      (let ((a (reverse! args)))
	(loop (cdr a) (car a)))))

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (reverse! l)
  (define (loop0 prev curr next)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop1 (cdr next) curr next)))
  (define (loop1 next prev curr)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop2 next (cdr next) curr)))
  (define (loop2 curr next prev)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop0 curr next (cdr next))))
  (if (null? l)
      '()
      (loop0 '() l (cdr l))))

(define (every? p l)

  (define (loop l)
    (cond ((null? (cdr l)) (p (car l)))
	  ((p (car l)) (loop (cdr l)))
	  (else #f)))

  (if (null? l)
      #t
      (loop l)))

(define (some? p l)
  (cond ((null? l) #f)
	((p (car l)))
	(else (some? p (cdr l)))))

(define (remove x l)
  (cond ((not (pair? l)) l)
	((equal? x (car l)) (remove x (cdr l)))
	(else (cons (car l) (remove x (cdr l))))))

(define (remq! key list)
  (cond ((null? list) list)
	((eq? key (car list))
	 (remq! key (cdr list)))
	(else
	 (set-cdr! list (remq! key (cdr list)))
	 list)))

(define (string-hash string)
  (define (loop s i h)
    (if (< i 0)
	h
	(loop s
	      (- i 1)
	      (remainder (+ (char->integer (string-ref s i)) h h h) 65536))))
  (let ((n (string-length string)))
    (loop string (- n 1) n)))

; We could use Gambit-native bytevectors here, but these primitives
; have been tested with Chez Scheme, and I know they work.

(define *bv-key* (vector 'bytevector))

(define (make-bytevector n)
  (let ((v (make-vector (+ n 1) '*bv-init*)))
    (vector-set! v 0 *bv-key*)
    v))

(define (bytevector-ref bv k) 
  (vector-ref bv (+ k 1)))

(define (bytevector-set! bv k v)
  (vector-set! bv (+ k 1) v))

(define (bytevector-length bv)
  (- (vector-length bv) 1))

(define bytevector-copy vector-copy)

(define (bytevector? x) 
  (and (vector? x)
       (> (vector-length x) 0)
       (equal? (vector-ref x 0) *bv-key*)))   ; [sic]

(define (list->bytevector l)
  (list->vector (cons *bv-key* l)))

(define (flat1->bytevector s)
  (list->bytevector (map char->integer (string->list s))))

(define (symbol->bytevector s)
  (flat1->bytevector (symbol->string s)))

; Specific to IEEE double precision floating point.
;
; In the following drawings, the lowest addresses are on the top left.
;
; Flonums (IEEE double) are bytevector-like. The first word is unused. The two
; next words contain the double:
;
;	+---------------------------------+
;	|      structure header           |
;	+---------------------------------+
;	|      unused                     |
;	+---------------------------------+
;	|      IEEE double precision      |
;	|                                 |
;	+---------------------------------+
;
; Compnums are similar:
;
;	+---------------------------------+
;	|      structure header           |
;	+---------------------------------+
;	|      unused                     |
;	+---------------------------------+
;	|      (real part)                |
;	|      IEEE double precision      |
;	+---------------------------------+
;	|      (imaginary part)           |
;	|      IEEE double precision      |
;	+---------------------------------+
;
; An IEEE number, in turn, is represented in native layout (64 bits):
;
; Big endian:
;
;       +-+----------+--------------------+
;       |s| exponent |                    |    low word
;       +-+----------+--------------------+
;       |                                 |    high word
;       +---------------------------------+
;
; Little endian:
;
;       +---------------------------------+
;       |                                 |    low word
;       +--------------------+----------+-+
;       |                    | exponent |s|    high word
;       +--------------------+----------+-+

;
; where the sign (s) is one bit, the exponent is 11 bits, and the fraction is
; the remaining 52 bits.

(define (flonum->bytevector f)
  (list->bytevector (append '(0 0 0 0) (flonum-bits f))))

(define (compnum->bytevector c)
  (let ((f1 (flonum-bits (real-part c)))
	(f2 (flonum-bits (imag-part c))))
    (list->bytevector (append '(0 0 0 0) f1 f2))))

; Return a list of byte values representing an IEEE double precision number
; in the native endianness.

(define (flonum-bits f)
  (call-with-values 
   (lambda () (ieee-words f))
   (lambda (first second)
     (append (split-int32 first) (split-int32 second)))))

; The coercion to inexact is necessary because of a Gambit-C weirdness:
;
;   (real-part +1.i) => 0  [ exact! ]
;
; This makes sense if an implementation has mixed-representation compnums
; (where the real and imaginary parts are separately exact or not) but
; is not what we want here.

(define (ieee-words f)
  (let ((f (if (exact? f) (exact->inexact f) f)))
    (values (double-bits 0 f) (double-bits 1 f))))

; In the following drawings, the lowest addresses are on the top left.
;
; Bignums are represented in the same way on big-endian and little-
; endian architectures.  The first word is the header: the tag is
; in the low byte, the structure length is in the high three bytes.
; The second word is the bignum meta-data: the length (in 32-bit 
; bigits) is in the high two bytes, the sign is in the low two bytes; 
; both fields are stored in native-endian format.  The remaining words
; are 32-bit native-endian data.
;
; On a big-endian architecture it looks like this:
;
;	+------------------------+--------+
;	|       length           | header |
;	+------------------------+--------+
;	| sign          |   digitcount    |
;	+---------------------------------+
;	|              lsd                |
;	+---------------------------------+
;	...
;
; On a little-endian architecture it looks like this:
;
;	+--------+------------------------+
;	| header |       length           |
;	+---------------------------------+
;	| digitcount    |   sign          |
;	+---------------------------------+
;	|              lsd                |
;	+---------------------------------+
;	...

(define (bignum->bytevector b)

  (define (meta-data sign count)
    (case (nbuild-parameter 'endianness)
      ((little) (append count sign))
      ((big) (append sign count))
      (else ???)))

  (define two^32 (expt 2 32))

  (define (flatten x)
    (apply append x))

  ; returned list has length divisible by 4.

  (define (divide b l)
    (if (< b two^32)
	(flatten (reverse (cons (split-int32 b) l)))
	(divide (quotient b two^32)
		(cons (split-int32 (remainder b two^32)) l))))

  (let* ((sign   (split-int16 (if (negative? b) 1 0)))
	 (b      (abs b))
	 (digits (divide b '()))
	 (len    (quotient (length digits) 4))
	 (count  (split-int16 len)))
    (list->bytevector
     (append (meta-data sign count) digits))))

(define split-int16
  (let ((endianness (nbuild-parameter 'endianness)))
    (lambda (x)
      (case endianness
	((big)
	 (list (quotient x 256) (remainder x 256)))
	((little)
	 (list (remainder x 256) (quotient x 256)))
	(else ???)))))

(define split-int32
  (let ((two^32 (expt 2 32))
	(two^24 (expt 2 24))
	(two^16 (expt 2 16))
	(two^8  (expt 2 8))
	(endianness (nbuild-parameter 'endianness)))
    (lambda (b)
      (case endianness
	((big)
	 (list (quotient b two^24)
	       (quotient (remainder b two^24) two^16)
	       (quotient (remainder b two^16) two^8)
	       (remainder b two^8)))
	((little)
	 (list (remainder b two^8)
	       (quotient (remainder b two^16) two^8)
	       (quotient (remainder b two^24) two^16)
	       (quotient b two^24)))
	(else ???)))))

(define (write-bytevector-like bv . rest)
  (cond ((bytevector? bv)
	 (let ((limit (vector-length bv))
	       (port  (if (null? rest) (current-output-port) (car rest))))
	   (do ((i 1 (+ i 1)))
	       ((= i limit))
	     (write-char (integer->char (vector-ref bv i)) port))))
	((string? bv)
	 (apply display bv rest))
	(else
	 ???)))

(define gensym
  (let ((gensym gensym))
    (lambda (x)
      (gensym (string->symbol x)))))

(define fixnum?
  (let ((smallest-fixnum (- (expt 2 29)))
	(largest-fixnum (- (expt 2 29) 1)))
    (lambda (x)
      (and (integer? x)
	   (exact? x)
	   (<= smallest-fixnum x largest-fixnum)))))

(define *undefined-expression* ''*undefined*)
(define *unspecified-expression* ''*unspecified*)
(define *eof-object* ''*eof-object*)

(define (unspecified) *unspecified-expression*)
(define (undefined) *undefined-expression*)
(define (eof-object) *eof-object*)

; eof

; Arithmetic and bit operations on 32-bit unsigned numbers
; 2003-12-15 / lth
;
; A "word" is a non-numeric data type representing an exact 32-bit
; unsigned integer.  Words are created using the "word" syntax or
; "integer->word"; words are converted back to integers using
; "word->integer".
;
; The object is not to avoid boxing but to avoid bignums: Larceny has
; pretty slow bignums but fast allocation and garbage collection, and
; eg MD5 is much faster with this code than with code based on bignum
; arithmetic.
;
; Words are represented as pairs of fixnums.  This implementation
; requires that the Scheme system provide at least 17-bit fixnums with
; bit operations on fixnums.


; (word <numeric constant>) => <word>

(define-syntax word
  (transformer 
    (lambda (expr rename compare)
      (list 'quote (integer->word (cadr expr))))))

; (integer->word <integer>) => <word>
;
; The argument may be negative, in which case the low 32 bits of its
; two's complement bit pattern is used

(define (integer->word i)
  (cond ((fixnum? i)
	 (cons (logand (rsha i 16) 65535) (logand i 65535)))
	((< i 0)
	 (let ((j (+ 4294967296 i)))
	   (if (< j 0)
	       (error "integer->word: out of range: " i)
	       (integer->word j))))
	((< i 4294967296)
	 (cons (quotient i 65536) (remainder i 65536)))
	(else
	 (error "integer->word: out of range: " i))))

(define (word->integer w)
  (+ (* (car w) 65536) (cdr w)))

(define (word+ a b)
  (let* ((t2 (+ (cdr a) (cdr b)))
	 (t1 (+ (car a) (car b) (rshl t2 16))))
    (cons (logand t1 65535) (logand t2 65535))))

(define (word- a b) 
  (let* ((t2 (- (cdr a) (cdr b)))
	 (t1 (- (car a) (car b) (logand (rshl t2 16) 1))))
    (cons (logand t1 65535) (logand t2 65535))))

(define (word= a b) 
  (and (= (car a) (car b)) (= (cdr a) (cdr b))))

(define (word< a b) 
  (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

(define (word<= a b)
  (or (< (car a) (car b)) (and (= (car a) (car b)) (<= (cdr a) (cdr b)))))

(define (word> a b)
  (or (> (car a) (car b)) (and (= (car a) (car b)) (> (cdr a) (cdr b)))))

(define (word>= a b)
  (or (> (car a) (car b)) (and (= (car a) (car b)) (>= (cdr a) (cdr b)))))

(define (word-not a)
  (cons (logand (lognot (car a)) 65535) (logand (lognot (cdr a)) 65535)))

(define (word-ior a b)
  (cons (logior (car a) (car b)) (logior (cdr a) (cdr b))))

(define (word-xor a b)
  (cons (logxor (car a) (car b)) (logxor (cdr a) (cdr b))))
    
(define (word-and a b)
  (cons (logand (car a) (car b)) (logand (cdr a) (cdr b))))

(define (%mask k)
  (lognot (lsh -1 k)))

; If s >= 0, shift the word s bits to the left, shifting in 0 bits.
; If s < 0, shift the word s bits to the right, shifting in the high bit.

(define (word-shift a s) 
  (define (shift-left hi lo s)
    (cons (logior (logand (lsh hi s) 65535)
		  (logand (rshl lo (- 16 s)) (%mask s)))
	  (logand (lsh lo s) 65535)))
  (define (shift-right hi lo s)
    (let ((signbit (rsha hi 15)))
      (cons (logior (rshl hi s) 
		    (if (zero? signbit)
			0
			(logand (lsh -1 (- 16 s)) 65535)))
	    (logior (logand (lsh hi (- 16 s)) 65535) (rshl lo s)))))
  (cond ((<= 0 s 15)
	 (shift-left (car a) (cdr a) s))
	((<= 16 s 31)
	 (shift-left (cdr a) 0 (- s 16)))
	((<= -15 s 0)
	 (shift-right (car a) (cdr a) (- s)))
	((<= -31 s -16)
	 (shift-right 65535 (car a) (- (- s) 16)))
	(else
	 (word-shift a (modulo s 32)))))

; If s >= 0, rotate the word s bits to the left.
; If s < 0, rotate the word (abs s) bits to the right.

(define (word-rotate a s)
  (define (rot hi lo s)
    (cons (logior (lsh (logand hi (%mask (- 16 s))) s)
		  (logand (rshl lo (- 16 s)) (%mask s)))
	  (logior (lsh (logand lo (%mask (- 16 s))) s)
		  (logand (rshl hi (- 16 s)) (%mask s)))))
  (cond ((<= 0 s 15)
	 (rot (car a) (cdr a) s))
	((<= 16 s 31)
	 (rot (cdr a) (car a) (- s 16)))
	((< -32 s 0)
	 (word-rotate a (+ 32 s)))
	(else
	 (word-rotate a (modulo s 32)))))


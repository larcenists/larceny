; Asm/Sparc/asmutil32.sch
; Larceny assembler -- common machine-level utility procedures, 32-bit.
;
; $Id$
;
; Instructions for RISC machines are represented as 4-byte bytevectors
; where byte 0 is the least significant and byte 3 is the most significant.
;
; FIXME:
;  - test cases for asm:bv->int
;  - test cases for asm:add

; Common ASSERT macro that can be turned off for production runs.

; (define-syntax ASSERT
;   (syntax-rules ()
;    ((ASSERT test msg ...)
;     (if (not test)
;         (error msg ...)))))

; We need a macro system.

(define (ASSERT test . rest)
  (if (not test)
      (apply error rest)))

; Given four bytes, create a length-4 bytevector. 
; n1 is the most significant byte, n4 the least significant.

(define (asm:bv n1 n2 n3 n4)
  (let ((bv (make-bytevector 4)))
    (bytevector-set! bv 0 n4)
    (bytevector-set! bv 1 n3)
    (bytevector-set! bv 2 n2)
    (bytevector-set! bv 3 n1)
    bv))

; Convert an integer to a length-4 bytevector using two's complement 
; representation for negative numbers.
; Returns length-4 bytevector.
;
; The procedure handles numbers in the range -2^31..2^32-1 [sic].
; It is an error for the number to be outside this range.
;
; FIXME: quotient/remainder may be slow; we could have special fixnum
;        case that uses shifts (that could be in-lined as macro).  It could
;        work for negative numbers too.
; FIXME: should probably check that the number is within range.

(define asm:int->bv
  (let ((two^32 (expt 2 32)))
    (lambda (m)
      (ASSERT (and (exact? m) (integer? m))
	      "asm:int->bv: not an exact integer: " m)
      (let* ((m  (if (< m 0) (+ two^32 m) m))
	     (b0 (remainder m 256))
	     (m  (quotient m 256))
	     (b1 (remainder m 256))
	     (m  (quotient m 256))
	     (b2 (remainder m 256))
	     (m  (quotient m 256))
	     (b3 (remainder m 256)))
	(asm:bv b3 b2 b1 b0)))))

(define (asm:bv->int bv)
  (let ((i (+ (* (+ (* (+ (* (bytevector-ref bv 3) 256)
			  (bytevector-ref bv 2))
		       256)
		    (bytevector-ref bv 1))
		 256)
	      (bytevector-ref bv 0))))
    (if (> (bytevector-ref bv 3) 127)
	(- i)
	i)))

; `Or' the bits of multiple operands together. 
; Each operand may be an exact integer or a length-4 bytevector.
; Returns a length-4 bytevector.
;
; Performance: Larceny rest arguments are slow; see file logior-extra.sch.

(define (asm:logior . ops)
  ; (declare (optimize (speed 3) (safety 0)))
  (let ((r (asm:bv 0 0 0 0)))
    (do ((ops ops (cdr ops)))
	((null? ops) r)
      (let* ((op (car ops))
	     (op (if (bytevector? op) op (asm:int->bv op))))
	(bytevector-set! r 0 (logior (bytevector-ref r 0)
				     (bytevector-ref op 0)))
	(bytevector-set! r 1 (logior (bytevector-ref r 1)
				     (bytevector-ref op 1)))
	(bytevector-set! r 2 (logior (bytevector-ref r 2)
				     (bytevector-ref op 2)))
	(bytevector-set! r 3 (logior (bytevector-ref r 3)
				     (bytevector-ref op 3)))))))


; `And' the bits of two operands together.
; Either may be an exact integer or length-4 bytevector.
; Returns length-4 bytevector.

(define (asm:logand op1 op2)
  (let ((op1 (if (bytevector? op1) op1 (asm:int->bv op1)))
	(op2 (if (bytevector? op2) op2 (asm:int->bv op2))))
    (asm:bv (logand (bytevector-ref op1 3) (bytevector-ref op2 3))
	    (logand (bytevector-ref op1 2) (bytevector-ref op2 2))
	    (logand (bytevector-ref op1 1) (bytevector-ref op2 1))
	    (logand (bytevector-ref op1 0) (bytevector-ref op2 0)))))

; Shift the bits of m left by n bits, shifting in zeroes at the low end.
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.
; Returns length-4 bytevector.

(define (asm:lsh m n)
  (if (not (bytevector? m))
      (asm:lsh (asm:int->bv m) n)
      (let ((m (bytevector-copy m))
	    (n (remainder n 33)))
	(if (>= n 8)
	    (let ((k (quotient n 8)))
	      (do ((i 3 (- i 1)))
		  ((< i k))
		(bytevector-set! m i (bytevector-ref m (- i k))))
	      (do ((i 0 (+ i 1)))
		  ((= i k))
		(bytevector-set! m i 0))))
	(let* ((d0 (bytevector-ref m 0))
	       (d1 (bytevector-ref m 1))
	       (d2 (bytevector-ref m 2))
	       (d3 (bytevector-ref m 3))
	       (n  (remainder n 8))
	       (n- (- 8 n)))
	  (asm:bv (logand (logior (lsh d3 n) (rshl d2 n-)) 255)
		  (logand (logior (lsh d2 n) (rshl d1 n-)) 255)
		  (logand (logior (lsh d1 n) (rshl d0 n-)) 255)
		  (logand (lsh d0 n) 255))))))

; Shift the bits of m right by n bits, shifting in zeroes at the high end.
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.
; Returns length-4 bytevector.

(define (asm:rshl m n)
  (if (not (bytevector? m))
      (asm:rshl (asm:int->bv m) n)
      (let ((m (bytevector-copy m))
	    (n (remainder n 33)))
	(if (>= n 8)
	    (let ((k (quotient n 8)))
	      (do ((i k (+ i 1)))
		  ((= i 4))
		(bytevector-set! m (- i k) (bytevector-ref m i)))
	      (do ((i 3 (- i 1)))
		  ((= i (- 3 k)))
		(bytevector-set! m i 0))))
	(let* ((d0 (bytevector-ref m 0))
	       (d1 (bytevector-ref m 1))
	       (d2 (bytevector-ref m 2))
	       (d3 (bytevector-ref m 3))
	       (n  (remainder n 8))
	       (n- (- 8 n)))
	  (asm:bv (logand (rshl d3 n) 255)
		  (logand (logior (rshl d2 n) (lsh d3 n-)) 255)
		  (logand (logior (rshl d1 n) (lsh d2 n-)) 255)
		  (logand (logior (rshl d0 n) (lsh d1 n-)) 255))))))

; Shift the bits of m right by n bits, shifting in the sign bit at the
; high end.
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.
; Returns length-4 bytevector.

(define asm:rsha
  (let ((ones (asm:bv #xff #xff #xff #xff)))
    (lambda (m n)
      (let* ((m (if (bytevector? m) m (asm:int->bv m)))
	     (n (remainder n 33))
	     (h (rshl (bytevector-ref m 3) 7))
	     (k (asm:rshl m n)))
	(if (zero? h)
	    k
	    (asm:logior k (asm:lsh ones (- 32 n))))))))

; Extract the n low-order bits of m.
; m may be an exact integer or a length-4 bytevector.
; n must be an exact nonnegative integer, interpreted modulo 32.
; Returns length-4 bytevector.

(define asm:lobits 
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (asm:int->bv (- (expt 2 i) 1))))
    (lambda (m n)
      (asm:logand m (vector-ref v (remainder n 33))))))

; Extract the n high-order bits of m.
; m may be an exact integer or a length-4 bytevector.
; n must be an exact nonnegative integer, interpreted modulo 33.
; Returns length-4 bytevector with the high-order bits of m at low end.

(define (asm:hibits m n)
  (asm:rshl m (- 32 (remainder n 33))))

; Test that the given number (not! bytevector) m fits in an n-bit 
; signed slot.

(define asm:fits?
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (expt 2 i)))
    (lambda (m n)
      (<= (- (vector-ref v (- n 1))) m (- (vector-ref v (- n 1)) 1)))))

; Test that the given number (not! bytevector) m fits in an n-bit 
; unsigned slot.

(define asm:fits-unsigned?
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (expt 2 i)))
    (lambda (m n)
      (<= 0 m (- (vector-ref v n) 1)))))

(define (asm:add a b)
  (asm:int->bv (+ (if (bytevector? a) (asm:bv->int a) a)
		  (if (bytevector? b) (asm:bv->int b) b))))

(define (asm:signed n)
  (if (< n 2147483647)
      n
      (- n 4294967296)))

; Test code.
; FIXME: it should automatically check the results.

(define (asm:test-utils)

  (define (printbv msg bv)
    (display "(")
    (display msg)
    (display ") = ")
    (do ((i 3 (- i 1)))
	((< i 0))
      (display (bytevector-ref bv i))
      (display " "))
    (newline))
  
  (define (printbvx msg bv)
    (display "(")
    (display msg)
    (display ") = ")
    (do ((i 3 (- i 1)))
	((< i 0))
      (display (number->string (bytevector-ref bv i) 16))
      (display " "))
    (newline))
  
  (define (printst caption answer result)
    (display caption)
    (display " should be ")
    (write answer)
    (display " and is ")
    (write result)
    (newline)
    (if (not (equal? answer result))
	(begin (display "WARNING!! Failed.")
	       (newline))))

  (define (test-construction)
    (display "CONSTRUCT") (newline)
    (printbv "bv 1 2 3 4" (asm:bv 1 2 3 4))
    (printbv "bv 255 255 255 255" (asm:bv 255 255 255 255))
    (printbv "int->bv 0" (asm:int->bv 0))
    (printbv "int->bv 1" (asm:int->bv 1))
    (printbv "int->bv 1024" (asm:int->bv 1024))
    (printbv "int->bv -1" (asm:int->bv -1))
    (printbv "int->bv -1024" (asm:int->bv -1024))
    (printbv "int->bv 2^31-1" (asm:int->bv (- (expt 2 31) 1)))
    (printbv "int->bv -2^31" (asm:int->bv (expt -2 31)))
    #t)

  (define (test-logior)
    (display "LOGIOR")
    (newline)
    (printbv "logior 0" (asm:logior 0))
    (printbv "logior 256" (asm:logior 256))
    (printbv "logior 256 255" (asm:logior 256 255))
    (printbv "logior (bv 1 2 3 4) (bv 4 3 2 1)" (asm:logior (asm:bv 1 2 3 4)
							    (asm:bv 4 3 2 1)))
    (printbv "logior 2 4 8 16 32" (asm:logior 2 4 8 16 32))
    #t)

  (define (test-logand)
    (display "LOGAND")
    (newline)
    (printbv "logand 0 2^31-1" (asm:logand 0 (- (expt 2 31) 1)))
    (printbv "logand #xffff #xf0f0" (asm:logand #xffff #xf0f0))
    (printbv "loagand (bv 1 2 3 4) (bv 4 3 2 1)" (asm:logand (asm:bv 1 2 3 4)
							     (asm:bv 4 3 2 1)))
    #t)

  (define (test-lsh)
    (let ((bv (asm:bv 1 2 3 4)))
      (printbv "LEFT SHIFT" bv)
      (printbv "shift left 0 bits" (asm:lsh bv 0))
      (printbv "shift left 1 bit"  (asm:lsh bv 1))
      (printbv "shift left 8 bits" (asm:lsh bv 8))
      (printbv "shift left 9 bits" (asm:lsh bv 9))
      (printbv "shift left 17 bits" (asm:lsh bv 17))
      (printbv "shift left 25 bits" (asm:lsh bv 25))
      (printbv "shift left 32 bits" (asm:lsh bv 32))
      #t))

  (define (test-rsha)
    (rsha2 (asm:bv 1 2 3 4))
    (rsha2 (asm:bv 128 2 3 4)))

  (define (rsha2 bv)
    (printbv "RIGHT SHIFT ARITHMETIC" bv)
    (printbv "shift right 0 bits" (asm:rsha bv 0))
    (printbv "shift right 1 bit"  (asm:rsha bv 1))
    (printbv "shift right 8 bits" (asm:rsha bv 8))
    (printbv "shift right 9 bits" (asm:rsha bv 9))
    (printbv "shift right 17 bits" (asm:rsha bv 17))
    (printbv "shift right 25 bits" (asm:rsha bv 25))
    (printbv "shift right 32 bits" (asm:rshl bv 32))
    #t)

  (define (test-rshl)
    (let ((bv (asm:bv 128 2 3 4)))
      (printbv "RIGHT SHIFT LOGICAL" bv)
      (printbv "shift right 0 bits" (asm:rshl bv 0))
      (printbv "shift right 1 bit"  (asm:rshl bv 1))
      (printbv "shift right 8 bits" (asm:rshl bv 8))
      (printbv "shift right 9 bits" (asm:rshl bv 9))
      (printbv "shift right 17 bits" (asm:rshl bv 17))
      (printbv "shift right 25 bits" (asm:rshl bv 25))
      (printbv "shift right 32 bits" (asm:rshl bv 32))
      #t))

  (define (test-lobits) 
    (let ((bv (asm:bv #xf1 #xe6 #x4a #x85)))
      (printbvx "LOBITS" bv)
      (printbvx "lobits 0" (asm:lobits bv 0))
      (printbvx "lobits 5" (asm:lobits bv 5))
      (printbvx "lobits 10" (asm:lobits bv 10))
      (printbvx "lobits 16" (asm:lobits bv 16))
      (printbvx "lobits 32" (asm:lobits bv 32))))

  (define (test-hibits)
    (let ((bv (asm:bv #xf1 #xe6 #x4a #x85)))
      (printbvx "HIBITS" bv)
      (printbvx "hibits 0" (asm:hibits bv 0))
      (printbvx "hibits 5" (asm:hibits bv 5))
      (printbvx "hibits 10" (asm:hibits bv 10))
      (printbvx "hibits 16" (asm:hibits bv 16))
      (printbvx "hibits 32" (asm:hibits bv 32))))

  (define (test-fits)
    (display "FITS") (newline)
    (printst "2^12-1 in 13" #t (asm:fits? (- (expt 2 12) 1) 13))
    (printst "2^12 in 13" #f (asm:fits? (expt 2 12) 13))
    (printst "-2^12 in 13" #t (asm:fits? (- (expt 2 12)) 13))
    (printst "-2^12-1 in 13" #f (asm:fits? (- (- (expt 2 12)) 1) 13))
    (printst "2^22-1 in 22" #t (asm:fits-unsigned? (- (expt 2 22) 1) 22))
    (printst "2^22 in 22" #f (asm:fits-unsigned? (expt 2 22) 22))
    (printst "0 in 22" #t (asm:fits-unsigned? 0 22))
    (printst "-1 in 22" #f (asm:fits-unsigned? -1 22))
    )
    
  (test-construction)
  (test-logior)
  (test-logand)
  (test-lsh)
  (test-rsha)
  (test-rshl)
  (test-lobits)
  (test-hibits)
  (test-fits)
  #t)


; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny assembler -- 32-bit big-endian utility procedures.
;
; 32-bit numbers are represented as 4-byte bytevectors where byte 3
; is the least significant and byte 0 is the most significant.
;
; Logically, the 'big' end is on the left and the 'little' end
; is on the right, so a left shift shifts towards the 'big' end.
;
; Performance: poor, for good reasons.  See asmutil32.sch.

; Identifies the code loaded.

(define asm:endianness 'big)


; Given four bytes, create a length-4 bytevector. 
; N1 is the most significant byte, n4 the least significant.

(define (asm:bv n1 n2 n3 n4)
  (let ((bv (make-bytevector 4)))
    (bytevector-set! bv 0 n1)
    (bytevector-set! bv 1 n2)
    (bytevector-set! bv 2 n3)
    (bytevector-set! bv 3 n4)
    bv))


; Given a length-4 bytevector, convert it to an integer.

(define (asm:bv->int bv)
  (let ((i (+ (* (+ (* (+ (* (bytevector-ref bv 0) 256)
			  (bytevector-ref bv 1))
		       256)
		    (bytevector-ref bv 2))
		 256)
	      (bytevector-ref bv 3))))
    (if (> (bytevector-ref bv 0) 127)
	(- i)
	i)))


; Shift the bits of m left by n bits, shifting in zeroes at the right end.
; Returns a length-4 bytevector.
;
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.

(define (asm:lsh m n)
  (if (not (bytevector? m))
      (asm:lsh (asm:int->bv m) n)
      (let ((m (bytevector-copy m))
	    (n (remainder n 33)))
	(if (>= n 8)
	    (let ((k (quotient n 8)))
	      (do ((i 0 (+ i 1)))
		  ((= (+ i k) 4)
		   (do ((i i (+ i 1)))
		       ((= i 4))
		     (bytevector-set! m i 0)))
		(bytevector-set! m i (bytevector-ref m (+ i k))))))
	(let* ((d0 (bytevector-ref m 0))
	       (d1 (bytevector-ref m 1))
	       (d2 (bytevector-ref m 2))
	       (d3 (bytevector-ref m 3))
	       (n  (remainder n 8))
	       (n- (- 8 n)))
	  (asm:bv (logand (logior (lsh d0 n) (rshl d1 n-)) 255)
		  (logand (logior (lsh d1 n) (rshl d2 n-)) 255)
		  (logand (logior (lsh d2 n) (rshl d3 n-)) 255)
		  (logand (lsh d3 n) 255))))))


; Shift the bits of m right by n bits, shifting in zeroes at the high end.
; Returns a length-4 bytevector.
;
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.

(define (asm:rshl m n)
  (if (not (bytevector? m))
      (asm:rshl (asm:int->bv m) n)
      (let ((m (bytevector-copy m))
	    (n (remainder n 33)))
	(if (>= n 8)
	    (let ((k (quotient n 8)))
	      (do ((i 3 (- i 1)))
		  ((< (- i k) 0)
		   (do ((i i (- i 1)))
		       ((< i 0))
		     (bytevector-set! m i 0)))
		(bytevector-set! m i (bytevector-ref m (- i k))))))
	(let* ((d0 (bytevector-ref m 0))
	       (d1 (bytevector-ref m 1))
	       (d2 (bytevector-ref m 2))
	       (d3 (bytevector-ref m 3))
	       (n  (remainder n 8))
	       (n- (- 8 n)))
	  (asm:bv (rshl d0 n)
		  (logand (logior (rshl d1 n) (lsh d0 n-)) 255)
		  (logand (logior (rshl d2 n) (lsh d1 n-)) 255)
		  (logand (logior (rshl d3 n) (lsh d2 n-)) 255))))))


; Shift the bits of m right by n bits, shifting in the sign bit at the
; high end.  Returns a length-4 bytevector.
;
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.

(define asm:rsha
  (let ((ones (asm:bv #xff #xff #xff #xff)))
    (lambda (m n)
      (let* ((m (if (bytevector? m) m (asm:int->bv m)))
	     (n (remainder n 33))
	     (h (rshl (bytevector-ref m 0) 7))
	     (k (asm:rshl m n)))
;	(format #t "~a ~a ~a~%" h (bytevector-ref m 0) n)
;	(prnx (asm:lsh ones (- 32 n))) (newline)
	(if (zero? h)
	    k
	    (asm:logior k (asm:lsh ones (- 32 n))))))))

; eof

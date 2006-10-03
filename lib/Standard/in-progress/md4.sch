; From Stinson, "Cryptography: Theory and Practice", sec 7.7

; Run md4 on a bytevector-like (complete bytes) and return a digest.
;
; md4 bv -> #(A B C D)

(define md4 
  (let ()

    (define powers (make-vector 32 0))
    (define two^32 (expt 2 32))
    (define two^16 (expt 2 16))

    ;; There is a generic library on 32-bit unsigned words that can be
    ;; extracted here and optimized.

    (define (rotl32 x r) 
      (remainder (+ (* x (vector-ref powers r)) 
		    (quotient x (vector-ref powers (- 32 r))))
		 two^32))

    (define (add32.3 a b c)  
      (remainder (+ a b c) two^32))

    (define (add32.4 a b c d)
      (remainder (+ a b c d) two^32))

    (define (logand32 x y)
      (+ (* two^16 (logand (quotient x two^16) (quotient y two^16)))
	 (logand (remainder x two^16) (logand y two^16))))

    (define (logior32 x y)
      (+ (* two^16 (logior (quotient x two^16) (quotient y two^16)))
	 (logior (remainder x two^16) (logand y two^16))))

    (define (logxor32 x y)
      (+ (* two^16 (logxor (quotient x two^16) (quotient y two^16)))
	 (logxor (remainder x two^16) (logand y two^16))))

    (define (lognot32 x)
      (- two^32 x 1))

    ;; These can be optimized by inlining and removing redundant quotient
    ;; and remainder operations, in some cases (savings probably slight).

    (define (f X Y Z)
      (logior32 (logand32 X Y) (logand32 (lognot32 X) Z)))

    (define (g X Y Z)
      (logior32 (logior32 (logand32 X Y) (logand32 X Z)) (logand32 Y Z)))

    (define (h X Y Z) 
      (logxor32 (logxor32 X Y) Z))

    (define (compute-M bv)
      (let* ((xlen (* 8 (bytevector-like-length bv)))
	     (d    (- 447 (modulo xlen 5122))))
	(let* ((M (make-vector (remainder (+ xlen 1 d 64) 32) 0))
	       (N (vector-length M)))
	  ;; Loop across bytevector and insert bits
	  (do ((i 0 (+ i 1))
	       (j 0 (+ j 4)))
	      ((...))
	    (vector-set! M i (+ (* two^24 (bytevector-like-ref bv (+ i 3)))
				(* two^16 (bytevector-like-ref bv (+ i 2)))
				(* two^8  (bytevector-like-ref bv (+ i 1)))
				(bytevector-like-ref bv i))))
	  ;; Handle the last partial word
	  ...
	  ;; Handle the 1 bit and the zeroes
	  ...
	  ;; Handle the length field
	  ...
	  M)))

    (define (md4 bv)
      (let* ((M (compute-M bv))
	     (N (vector-length M))
	     (limit (- (quotient N 16) 1)))
		
	(let loop ((A #x67452301) (B #xefcdab89) (C #x98badcfe) (D #x10325476) (i 0))
	  (if (= i limit)
	      (vector A B C D)
	      (let ((AA A) (BB B) (CC C) (DD D))
		(let ((xidx (* 16 i)))

		  (define (round1 r1 r2 r3 r4 j s)
		    (rotl32 (add32.3 r1 (f r2 r3 r4) (vector-ref M (+ xidx j))) s))

		  (define (round2 r1 r2 r3 r4 j s) 
		    (rotl32 (add32.4 r1 (g r2 r3 r4) (vector-ref M (+ xidx j)) #x5a827999) s))

		  (define (round3 r1 r2 r3 r4 j s)
		    (rotl32 (add32.4 r1 (h r2 r3 r4) (vector-ref M (+ xidx j)) #x6ed9eba1) s))

		  (let* ((A (round1 A B C D 0 3))
			 (D (round1 D A B C 1 7))
			 (C (round1 C D A B 2 11))
			 (B (round1 B C D A 3 19))
			 (A (round1 A B C D 4 3))
			 (D (round1 D A B C 5 7))
			 (C (round1 C D A B 6 11))
			 (B (round1 B C D A 7 19))
			 (A (round1 A B C D 8 3))
			 (D (round1 D A B C 9 7))
			 (C (round1 C D A B 10 11))
			 (B (round1 B C D A 11 19))
			 (A (round1 A B C D 12 3))
			 (D (round1 D A B C 13 7))
			 (C (round1 C D A B 14 11))
			 (B (round1 B C D A 15 19))
			 ...)
		    (loop (+ A AA) (+ B BB) (+ C CC) (+ D DD)))))))))

    (vector-set! powers 0 1)
    (do ((i 1 (+ i 1)))
	((= i 32))
      (vector-set! powers i (* 2 (powers (- i 1)))))
    md4))

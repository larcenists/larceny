(define (bitwise-and x y)
  (cond ((fixnum? x)
	 (if (fixnum? y)
	     (logand x y)
	     (let-values (((y1 y0 (low-signed-bigits y))))
	       (logior (lsh (logand (rsha x 16) y1) 16)
		       (logand (logand x #xFFFF) y0)))))
	((fixnum? y)
	 (bitwise-and y x))
	((negative? x)
	 (if (negative? y)
	     ...
	     ...))
	((negative? y)
	 (bitwise-and y x))
	(else
	 ; Both positive bignums
	 ...)))

(define (bitwise-and x y)
  (let-syntax ((map-bignums
		(syntax-rules ()
		  ((map-bignums opx opy)
		   (let* ((lx  (bignum-length16 x))
			  (ly  (bignum-length16 y))
			  (len (min lx ly))
			  (r   (bignum-alloc16 len)))
		     (do ((i 0 (+ i 1)))
			 ((= i len)
			  (bignum-normalize16! r))
		       (bignum-set16! r i 
				      (logand (opx (bignum-ref16 x i))
					      (opy (bignum-ref16 y i))))))))))
    (cond ((fixnum? x)
	   (if (fixnum? y)
	       (logand x y)
	       (logior (lsh (logand (rsha x 16) (bignum-ref16 y 1)) 16)
		       (logand (logand x #xFFFF)
			       (bignum-ref16 y 0)))))
	  ((fixnum? y)
	   (bitwise-and y x))
	  ((and (negative? x) (negative? y))
	   (- (bitwise-and (- x) (- y))))
	  (else
	   (let ((r (bitwise-and (- x
	   (- (bi
	   (let* ((lx  (bignum-length16 x))
		  (ly  (bignum-length16 y))
		  (len (min lx ly))
		  (r   (bignum-alloc16 (+ len 1))))
	     (let loop ((i 0) (cx 1) (cy 1))
	       (if (= i len)
		   (begin (bignum-normalize16! r)
			  (if (not (zero? (logand cx cy)))
			      (bignum-sign-set! r 1))
			  r)
		   (let* ((a  (+ (lognot (bignum-ref16 x i)) cx))
			  (b  (+ (lognot (bignum-ref16 y i)) cy))
			  (cx (rshl a 16))
			  (cy (rshl b 16)))
		     (bignum-set16! r i (logand (logand a b) #xFFFF))
		     (loop (+ i 1) cx cy))))))
	  ((negative? x)
	   (bitwise-and y x))
	  (else
	   (map-bignums begin begin)))))

(define (bitwise-not x)
  (if (fixnum? x)
      (lognot x)
      (- (- x) 1)))

(define (arithmetic-shift x n)
  (if (fixnum? x)
      (if (negative? n)
	  (rsha x (- n))
	  (shl x n))
      (quotient x (expt 2 n))))


(define max-bignum-bytes 100000)
(define sys$tag.bignum-typetag (typetag (+ (most-postitive-fixnum) 1)))

; Big-endian
; (define (bignum-ref16 a i)
;   (let ((x (+ i (if (eq? (logand i 1) 0) 3 1))))  ; (big->hw-index i)
;     (bytevector-like-halfword-ref a x)))

; (define (bignum-set16! a i v)
;   (let ((x (+ i (if (eq? (logand i 1) 0) 3 1))))  ; (big->hw-index i)
;     (bytevector-like-halfword-set! a x v)))

; (define (bignum-length16 b)
;   (let* ((l0 (logior (lsh (bytevector-like-ref b 2) 8)
;                      (bytevector-like-ref b 3)))
;          (l  (+ l0 l0)))
;     (cond ((zero? l) l)
;           ((zero? (bignum-ref b (- l 1))) (- l 1))
;           (else l))))

; Little-endian
(define (bignum-ref16 a i)
  (bytevector-like-halfword-ref a (+ i 2)))

(define (bignum-set16! a i v)
  (bytevector-like-halfword-set! a (+ i 2) v))

(define (bignum-length16 b)
  (let* ((l0 (bytevector-like-halfword-ref b 0))
	 (l  (+ l0 l0)))
    (cond ((zero? l) l)
	  ((zero? (bignum-ref b (- l 1))) (- l 1))
	  (else l))))

(define (bignum-length-set16! b l)
  (let ((l (rsha (+ l 1) 1)))
    (bytevector-like-halfword-set! b 0 l)))

(define (bignum-sign b)
  (bytevector-like-ref b 2))

(define (bignum-sign-set! b s)
  (bytevector-like-set! b 2 s))

; Portable

(define (big-normalize16! b)

  (define (big-fits-in-fix? b)
    (and (bignum>? b largest-negative-bignum)
	 (bignum<? b smallest-positive-bignum)))

  (define (bignum->fixnum b)
    (let ((d0 (bignum-ref16 b 0))
	  (d1 (bignum-ref16 b 1))
	  (n  (sign-negative? (bignum-sign b))))
      (if (>= (lsh d1 2) bignum-base/2)
	  max-negative-fixnum
	  (let ((c (logior (lsh d1 16) d0)))
	    (if n
		(- c)
		c)))))

  (define (loop i)
    (cond ((< i 0) 
	   0)
	  ((= (bignum-ref16 b i) 0)
	   (loop (- i 1)))
	  (else
	   (bignum-length-set16! b (+ i 1))
	   (if (big-fits-in-fix? b)
	       (bignum->fixnum16 b)
	       b))))))
    (loop (- (bignum-length16 b) 1))))

(define (bignum-alloc16 bigits)

  (define (roundup4 n)
    (logand (+ n 3) (lognot 3)))

  (let ((l (roundup4 (* bigits bytes-per-bigit))))
    (if (> l max-bignum-bytes)
        (begin (error "Bignum too large: " bigits " bigits.")
               #t)
        (let ((v (make-bytevector (+ l 4))))
          (bytevector-fill! v 0)
          (typetag-set! v sys$tag.bignum-typetag)
          (bignum-length-set16! v bigits)
          v))))

(define (bignum-normalize16! ...)
  ...)

(define (bytevector-like-halfword-ref bv i)
  (let ((i (+ i i)))
    (logior (lsh (bytevector-like-ref bv i) 8)
            (bytevector-like-ref bv (+ i 1)))))

(define (bytevector-like-halfword-set! bv i v)
  (let ((hi (rsha v 8))
        (lo (logand v 255))
        (i  (+ i i)))
    (bytevector-like-set! bv i hi)
    (bytevector-like-set! bv (+ i 1) lo)))


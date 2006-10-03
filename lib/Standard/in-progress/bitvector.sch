(define (bitvector? x)
  (or (fixnum? x) (bignum? x)))

(define (bitvector-ref x n)
  (if (fixnum? x)
      (if (<= n 29)
          (logand (rsha x n) 1)
          (logand (rsha x 29) 1))
      (let ((l (bitvector-length x)))
        (cond ((< n l)  
               (logand (rsha (bignum-ref16 x (quotient n 16))
                             (remainder n 16))))
              ((negative? x) 1)
              (else 0)))))

(define (bitvector-set x n b)
  (if (and (fixnum? x)
           (<= n 29))
      (logior (logand x (lognot (lsh 1 n))) (lsh (logand b 1) n))
      (let ((mask (bitvector-not (bitvector-shift-left 1 n))))
        (bitvector-or (bitvector-and x mask)
                      (bitvector-shift-left (logand b 1) n)))))

(define (bitvector-and x y)
  (cond ((fixnum? x)
         (if (fixnum? y)
             (logand x y)
             (logior (lsh (logand (rsha x 16)  (bignum-ref16 y 1)) 16)
                     (logand (logand x #xFFFF)
                             (bignum-ref16 y 0)))))
        ((fixnum? y)
         ; FIXME: not guaranteed to fit in a fixnum.
         ; FIXME: if y < 0 then this is wrong.
         (logior (lsh (logand (bignum-ref16 x 1) (rsha y 16)) 16)
                 (logand (bignum-ref16 x 0) (logand y #xFFFF))))
        (else
         ...)))

(define (bitvector-or x y)
  (cond ((fixnum? x)
         (if (fixnum? y)
             (logior x y)
             (let ((hi (bignum-ref16 y 1)))
               (if (not (zero? (logand hi #xC000)))
                   ...
                   (logior (lsh (logior (rsha x 16) hi) 16)
                           (logior (logand x #xFFFF)
                                   (bignum-ref16 y 0)))))))
        ((fixnum? y)
         ...)
        (else
         ; FIXME: Not good enough to handle sign properly.
         (let ((len (max (bignum-length16 x)
                         (bignum-length16 y))))
           (do ((i 0 (+ i 1))
                (l '() (cons (logior (bignum-ref16 x i)
                                     (bignum-ref16 y i))
                             l)))
               ((= i len)
                (bignum-implode16 l)))))))

(define (bitvector-xor x y)
  (cond ((fixnum? x)
         (if (fixnum? y)
             (logxor x y)
             (let* ((l (bignum-length16 y))
                    (b (make-bignum l)))
               ...)))
        ((fixnum? y)
         ...)
        (else
         ...)))

define (bitvector-not x)
  (if (fixnum? x)
      (lognot x)
      (- (- x) 1)))

(define (bitvector-shift-left x n)
  (if (and (fixnum? x)
           (< n 32)
           (= (rsha (lsh x n) n) x))
      (lsh x n)
      (* x (expt 2 n))))

(define (bitvector-shift-right x n)
  (if (and (fixnum? x)
           (< n 32))
      (rsha x n)
      (quotient x (expt 2 n))))

(define (bitvector-length x)
  (if (fixnum? x)
      (fxlength x)
      (let ((l (bignum-length16 x)))
        (+ (* 16 (- l 1))
           (fixlength (abs (bignum-ref16 x (- l 1))))))))

(define (bitvector-field x n1 n2)
  (cond ((>= n1 n2)
         0)
        ((and (fixnum? x)
              (<= n2 29))
         (logand (rsha x n1)
                 (- (lsh 1 (- n2 n1)) 1)))
        (else
         (bitvector-and (bitvector-shift-right x n1)
                        (- (bitvector-shift-left 1 (- n2 n1)) 1)))))

(define (bitvector-replace x n1 n2 y)
  (cond ((>= n1 n2)
         x)
        ((and (fixnum? x)
              (fixnum? y)
              (<= n2 29))
         (let ((mask (lsh (- (lsh 1 (- n2 n1)) 1) n1)))
           (logior (logand x (lognot mask))
                   (lsh (logand y mask) n1))))
        (else
         (let ((mask (bitvector-shift-left 
                      (- (bitvector-shift-left 1 (- n2 n1)) 1) 
                      n1)))
           (bitvector-or (bitvector-and x (bitvector-not mask))
                         (bitvector-shift-left (bitvector-and y mask) n1))))))

(define (bitvector-population-count x)
  (if (fixnum? x)
      (fxpopcount x)
      (let ((l (bignum-length16 x)))
        ...)))
      ...))

; The following can be implemented efficiently as primitives
; or using low-level Scheme code.  

(define (fxlength x)
  
  (define (fixlength x)
    (cond ((< x 256)
           (fixlength0 x))
          ((< x 65536)
           (+ 8 (fixlength0 (rshl x 8))))
          ((< x 16777216)
           (+ 16 (fixlength0 (rshl x 16))))
          (else
           (+ 24 (fixlength0 (rshl x 24))))))
  
  (define (fixlength0 x)
    (if (< x 16)
        (if (< x 4)
            (if (< x 2)
                1
                2)
            (if (< x 8)
                3
                4))
        (if (< x 64)
            (if (< x 32)
                5
                6)
            (if (< x 128)
                7
                8))))
   
  (+ 1 (if (< x 0)
           (fixlength (- x))
           (fixlength x))))

(define (fxpopcount x)

  (define (popcount x)
    (vector-ref '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4) x))
  
  (+ (popcount (logand x 16))
     (popcount (logand (rsha x 4)  15))
     (popcount (logand (rsha x 8)  15))
     (popcount (logand (rsha x 12) 15))
     (popcount (logand (rsha x 16) 15))
     (popcount (logand (rsha x 20) 15))
     (popcount (logand (rsha x 24) 15))
     (popcount (logand (rsha x 28)  3))))

; Bignum-ref16 can memoize its arguments if negative, thereby avoiding 
; recomputing the two's complement representation as the client iterates 
; across the vector.

(define (bignum-ref16 x n)
  (logand (cond ((= n 0)
                 (remainder x 65536))
                ((= n 1)
                 (remainder x 65536 (quotient x 65536)))
                (else
                 (remainder x 65536 (quotient x (expt 65536 n)))))
          #xFFFF))

(define (bignum-length16 b)
  (let* ((l0 (logior (lsh (bytevector-like-ref b 2) 8)
                     (bytevector-like-ref b 3)))
         (l  (+ l0 l0)))
    (cond ((zero? l) l)
          ((zero? (bignum-ref b (- l 1))) (- l 1))
          (else l))))

; l is presented most significant digit first.

(define (bignum-implode16 l)
  (let ((b (bignum-alloc (length l))))
    (do ((i 0 (+ i 1))
         (l l (cdr l)))
        ((null? l) b)
      (bignum-set! b i (car l)))))

(define (bignum-alloc bigits)

  (define (roundup4 n)
    (logand (+ n 3) (lognot 3)))

  (let ((l (roundup4 (* bigits bytes-per-bigit))))
    (if (> l max-bignum-bytes)
        (begin (error "Bignum too large: " bigits " bigits.")
               #t)
        (let ((v (make-bytevector (+ l 4))))
          (bytevector-fill! v 0)
          (typetag-set! v sys$tag.bignum-typetag)
          (bignum-length-set! v bigits)
          v))))

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

(define (bignum-ref a i)
  (let ((x (+ i (if (eq? (logand i 1) 0) 3 1))))  ; (big->hw-index i)
    (bytevector-like-halfword-ref a x)))

(define (bignum-set! a i v)
  (let ((x (+ i (if (eq? (logand i 1) 0) 3 1))))  ; (big->hw-index i)
    (bytevector-like-halfword-set! a x v)))

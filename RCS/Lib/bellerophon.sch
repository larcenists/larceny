; Copyright Lightship Software
;
; $Id$
;
; A version of Algorithm Bellerophon for implementations
; of Scheme that support IEEE double precision arithmetic
; and exact integer arithmetic of unlimited precision.

(begin (display "$Id$")
       (newline))

(define bellerophon
  
  (let ()
    
    (define (bellerophon f e)
      (cond ((negative? f) (- (bellerophon (- f) e)))
            ((zero? f) flonum:zero)
            ((<= bellerophon:big-f f) (fail0 f e))
            ((< e bellerophon:small-e) (fail0 f e))
            ((< bellerophon:big-e e) flonum:infinity)
            ((and (< f two^n) (>= e 0) (< e log5-of-two^n))
             (* (exact->inexact f)
                (exact->inexact (expt 10 e))))
            ((and (< f two^n) (< e 0) (< (- e) log5-of-two^n))
             (/ (exact->inexact f)
                (exact->inexact (expt 10 (- e)))))
            (else (multiply-and-test
                   f e (cond ((< e -216) slop-216)
                             ((< e -108) slop-108)
                             ((< e -54)  slop-54)
                             ((< e -27)  slop-27)
                             ((< e 0)    slop0)
                             ((<= e 27)  slop27)
                             ((<= e 54)  slop54)
                             ((<= e 108) slop108)
                             ((<= e 216) slop216)
                             (else       slop324))))))
    
    (define (multiply-and-test f e slop)
      (let ((x (integer->extended f))
            (y (ten-to-e e)))
        (let ((z (extended-multiply x y)))
          (if (<= (abs (- (extended-lowbits z) two^p-n-1)) slop)
              (fail f e z)
              (extended->flonum z)))))
    
    (define (fail0 f e) (AlgorithmM f e))
    
    (define (fail f e z) (AlgorithmM f e))
    
    
    ; Powers of ten are computed from a small table containing
    ; the best extended precision approximations to
    ; 10^-216, 10^-108, 10^-54, 10^-27, 10^27, 10^54, 10^108, 10^216.
    
    ; The table of powers of ten; see assignments below.
    
    (define ten^-216 0)
    (define ten^-108 0)
    (define ten^-54  0)
    (define ten^-27  0)
    (define ten^27   0)
    (define ten^54   0)
    (define ten^108  0)
    (define ten^216  0)
    
    (define (ten-to-e e)
      (cond ((< e -432) ???)
            ((< e -216)
             (extended-multiply ten^-216
                                (extended-multiply ten^-216
                                                   (ten-to-e (+ e 432)))))
            ((< e -108)
             (extended-multiply ten^-216 (ten-to-e (+ e 216))))
            ((< e -54)
             (extended-multiply ten^-108 (ten-to-e (+ e 108))))
            ((< e -27)
             (extended-multiply ten^-54 (ten-to-e (+ e 54))))
            ((< e 0)
             (extended-multiply ten^-27 (ten-to-e (+ e 27))))
            ((<= e 27) (integer->extended (expt 10 e)))
            ((<= e 54)
             (extended-multiply ten^27 (ten-to-e (- e 27))))
            ((<= e 108)
             (extended-multiply ten^54 (ten-to-e (- e 54))))
            ((<= e 216)
             (extended-multiply ten^108 (ten-to-e (- e 108))))
            ((<= e 324)
             (extended-multiply ten^216 (ten-to-e (- e 216))))
            (else ???)))
    
    ; These slop factors assume that f can be represented exactly
    ; as an extended precision number, so the slop factor is exactly
    ; twice the maximum error in the approximation to 10^e.
    
    (define slop-216 45)
    (define slop-108  9)
    (define slop-54   3)
    (define slop-27   3)
    (define slop0     1)
    (define slop27    0)
    (define slop54    1)
    (define slop108   3)
    (define slop216   9)
    (define slop324  21)
    
    (define n         53)
    (define two^n-1   (expt 2 (- n 1)))
    (define two^n     (expt 2 n))
    (define p         64)
    (define two^p-1   (expt 2 (- p 1)))
    (define two^p     (expt 2 p))
    (define two^p-n-1 (expt 2 (- p n 1)))
    (define two^p-n   (expt 2 (- p n)))
    
    (define flonum:zero 0.0)
    (define flonum:infinity 1e500)
    (define flonum:minexponent -1023)
    (define flonum:minexponent-51 -1074)
    (define bellerophon:big-f (expt 2 64))
    (define bellerophon:small-e -306)
    (define bellerophon:big-e 309)
    (define log5-of-two^n
      (inexact->exact (ceiling (/ (log two^n) (log 5)))))
    
    (define (slow-ten-to-e e)
      (define (loop1 y s guardbit)
        (cond ((< y two^p)
               (make-extended (if (zero? guardbit) y (+ y 1)) s))
              (else (loop1 (quotient y 2) (+ s 1) (remainder y 2)))))
      (define (loop2 y s)
        (cond ((<= two^p-1 y)
               (make-extended y s))
              (else (loop2 (* 2 y) (- s 1)))))
      (define (loop3 x y n)
        (cond ((>= x (* y two^p-1))
               (loop3-help x y (- n)))
              (else (loop3 (* 2 x) y (+ n 1)))))
      (define (loop3-help x y n)
        (let* ((q (quotient x y))
               (r (- x (* q y)))
               (y-r (- y r)))
          (make-extended (cond ((< r y-r) q)
                               ((> r y-r) (+ q 1))
                               ((zero? (remainder q 2)) q)
                               (else (+ q 1)))
                         n)))
      (if (negative? e)
          (loop3 1 (expt 10 (- e)) 0)
          (let ((10^e (expt 10 e)))
            (cond ((>= 10^e two^p)
                   (loop1 10^e 0 0))
                  (else (loop2 10^e 0))))))
    
    ; Extended precision floating point, implemented entirely
    ; in Scheme for portability and ease of maintenance.
    ;
    ; The following operations are used directly by Algorithm Bellerophon:
    ;
    ;   (integer->extended m)
    ;   (make-extended m q)
    ;   (extended->flonum ex)
    ;   (multiply-extended ex1 ex2)
    ;   (extended-lowbits ex)
    ;
    ; All numbers are positive; negative numbers and zero are
    ; not supported.
    ;
    ; An extended is represented as a pair (x t) where x and t are
    ; exact integers.  Its value is x * 2^t.  A normalized extended
    ; is an extended for which 2^{p-1} <= x < 2^p.
    
    (define make-extended list)
    (define (extended-significand f) (car f))
    (define (extended-exponent f) (cadr f))
    
    ; This flag is set by some operations to indicate whether
    ; any accuracy was lost during the operation.
    
    (define inexact-flag #f)
    
    (define two^2p-1 (expt 2 (- (* 2 p) 1)))
    (define two^2n-1 (expt 2 (- (* 2 n) 1)))
    
    (define (integer->extended n)
      (cond ((< n two^p-1)
             (extended-normalize (make-extended n 0) two^p-1))
            ((>= n two^p)
             (extended-normalize-and-round (make-extended n 0) two^p))
            (else (make-extended n 0))))
    
    ; Given an extended whose significand is less than two^p-1,
    ; returns the normalized extended having the same value.
    ; Because two^p-1 is a parameter, this can be used for various
    ; precisions other than p.
    
    (define (extended-normalize f two^p-1)
      (let ((x (extended-significand f))
            (t (extended-exponent f)))
        (if (<= two^p-1 x)
            (begin (set! inexact-flag #f)
                   (make-extended x t))
            (extended-normalize (make-extended (* 2 x) (- t 1)) two^p-1))))
    
    ; Given an extended whose significand is greater than two^p,
    ; returns the normalized extended closest to the given float,
    ; rounding to even in case of ties.
    ; two^p is a parameter so this can be used for different
    ; precisions.
    
    (define (extended-normalize-and-round f two^p)
      (do ((x (extended-significand f) (quotient x 2))
           (guard 0 (remainder x 2))
           (sticky 0 (max sticky guard))
           (t (extended-exponent f) (+ t 1)))
          ((< x two^p)
           ; The result is probably inexact.
           ; This setting will be changed if incorrect.
           (set! inexact-flag #t)
           (cond ((zero? guard)
                  (set! inexact-flag (not (zero? sticky)))
                  (make-extended x t))
                 ((and (zero? sticky) (even? x))
                  (make-extended x t))
                 ((= x (- two^p 1))
                  (make-extended (quotient two^p 2)
                                 (+ t 1)))
                 (else (make-extended (+ x 1) t))))))
    
    ; Given an extended, round it to the nearest flonum
    ; (with n bits of precision instead of p).
    
    (define (extended->flonum f)
      (let ((ff (extended-normalize-and-round f two^n)))
        (make-float (extended-significand ff)
                    (extended-exponent ff))))
    
    ; Given normalized extendeds, return their normalized product.
    
    (define (extended-multiply f1 f2)
      (let ((f (* (extended-significand f1)
                  (extended-significand f2)))
            (t (+ (extended-exponent f1)
                  (extended-exponent f2)
                  p)))
        ; Set flag for most common case.
        (set! inexact-flag #t)
        (if (<= two^2p-1 f)
            (let ((q (quotient f two^p))
                  (r (remainder f two^p)))
              (cond ((< r two^p-1)
                     (if (zero? r)
                         (set! inexact-flag #f))
                     (make-extended q t))
                    ((> r two^p-1)
                     (make-extended (+ q 1) t))
                    ((even? q)
                     (make-extended q t))
                    (else (make-extended (+ q 1) t))))
            (let ((q (quotient f two^p-1))
                  (r (remainder f two^p-1)))
              (cond ((< r two^p-1)
                     (if (zero? r)
                         (set! inexact-flag #f))
                     (make-extended q (- t 1)))
                    ((> r two^p-1)
                     (make-extended (+ q 1) (- t 1)))
                    ((even? f)
                     (make-extended q (- t 1)))
                    ((= q (- two^p-1 1))
                     (make-extended two^p-1 t))
                    (else (make-extended (+ q 1) (- t 1))))))))
    
    (define (extended-lowbits ex)
      (remainder (extended-significand ex)
                 two^p-n))
    
    ; End of extended precision number implementation.
    
    ; Backup algorithm.
    ; I'm using an extremely slow backup algorithm, mainly because
    ; the slow algorithm is needed anyway for denormalized numbers
    ; and I'm trying to keep things simple.
    
    ; Given exact integers f and e, with f nonnegative,
    ; returns the floating point number closest to f * 10^e.
    
    (define (AlgorithmM f e)
      
      ; f * 10^e = u/v * 2^k
      
      (define (loop u v k)
        (let ((x (quotient u v)))
          (cond ((and (<= two^n-1 x) (< x two^n))
                 (ratio->float u v k))
                ((< x two^n-1)
                 (loop (* 2 u) v (- k 1)))
                ((<= two^n x)
                 (loop u (* 2 v) (+ k 1))))))
      
      (if (negative? e)
          (loop f (expt 10 (- e)) 0)
          (loop (* f (expt 10 e)) 1 0)))
    
    ; Given exact positive integers p and q with
    ; 2^(n-1) <= u/v < 2^n, and exact integer k,
    ; returns the float closest to u/v * 2^k.
    
    (define (ratio->float u v k)
      (let* ((q (quotient u v))
             (r (- u (* q v)))
             (v-r (- v r)))
        (cond ((< r v-r)               (make-float q k))
              ((> r v-r)               (make-float (+ q 1) k))
              ((zero? (remainder q 2)) (make-float q k))
              (else                    (make-float (+ q 1) k)))))
    
    ; END OF ALGORITHM MultiplyByTwos
    
    ; Primitive operations on flonums.
    
    (define (make-float m q)
      (if (< q flonum:minexponent)
          (make-float (* .5 m) (+ q 1))
          (* (+ m 0.0) (expt 2.0 q))))
    
    ; (define (float-significand x)
    ;   (cond ((and (<= .5 x) (< x 1.0))
    ;          (inexact->exact (* x (expt 2.0 53))))
    ;         ((< x .5) (float-significand (* 2.0 x)))
    ;         ((<= 1.0 x) (float-significand (* .5 x)))))
    ; 
    ; (define (float-exponent x)
    ;   (define (loop x k)
    ;     (cond ((and (<= .5 x) (< x 1.0)) (- k 53))
    ;           ((< x .5) (loop (* 2.0 x) (- k 1)))
    ;           ((<= 1.0 x) (loop (* .5 x) (+ k 1)))))
    ;   (loop x 0))
    
; For Larceny, the following code has been moved out of bellerophon, into
; the general libraries, since these procedures are useful in general.
;
;    ; MacScheme-specific code.
;    ; Depends on the representations of bignums as well as flonums.
;    
;    (define (float-significand x)
;      (let ((x (->bytevector x))
;            (n (make-bytevector 12)))
;        (bytevector-set! n 0 0)
;        (bytevector-set! n 1 1)   ; sign
;        (bytevector-set! n 2 0)
;        (bytevector-set! n 3 4)   ; size
;        (bytevector-set! n 4 (bytevector-ref x 6))
;        (bytevector-set! n 5 (bytevector-ref x 7))
;        (bytevector-set! n 6 (bytevector-ref x 4))
;        (bytevector-set! n 7 (bytevector-ref x 5))
;        (bytevector-set! n 8 (bytevector-ref x 2))
;        (bytevector-set! n 9 (bytevector-ref x 3))
;        (bytevector-set! n 10 0)
;        (bytevector-set! n 11 (logior 16 (logand 15 (bytevector-ref x 1))))
;        
;        ; subtract hidden bit if x is denormalized
;        
;        (if (and (zero? (bytevector-ref x 0))
;                 (zero? (logand -16 (bytevector-ref x 1))))
;            (- (typetag-set! n (typetag two^n-1)) two^n-1)
;            (typetag-set! n (typetag two^n-1)))))
;    
;    (define (float-exponent x)
;      (let* ((x (->bytevector x))
;             (e (logior (lsh (logand 127 (bytevector-ref x 0)) 4)
;                        (lsh (bytevector-ref x 1) -4))))
;        (if (zero? e)
;            (- flonum:minexponent 51)          ; no hidden bit
;            (- e (+ 1023 52)))))


    (set! ten^-216 (slow-ten-to-e -216))
    (set! ten^-108 (slow-ten-to-e -108))
    (set! ten^-54  (slow-ten-to-e -54))
    (set! ten^-27  (slow-ten-to-e -27))
    (set! ten^27   (slow-ten-to-e 27))
    (set! ten^54   (slow-ten-to-e 54))
    (set! ten^108  (slow-ten-to-e 108))
    (set! ten^216  (slow-ten-to-e 216))
    
    bellerophon))

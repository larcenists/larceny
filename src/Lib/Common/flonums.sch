; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; 30 April 1999 / lth
;
; Endianness-independent IEEE double-precision floating point code.
; See also flonums-be.sch and flonums-el.sch.

($$trace "flonums")

(define make-flonum-datum)
(define make-compnum)
(define bignum->flonum)
(define flonum->bignum)
(define flonum->integer)
(define flonum->ratnum)
(define flonum:log)
(define flonum:exp)
(define flonum:sin)
(define flonum:cos)
(define flonum:tan)
(define flonum:asin)
(define flonum:acos)
(define flonum:atan)
(define flonum:atan2)
(define flonum:sqrt)
(define flonum:sinh)
(define flonum:cosh)

(let ()

  (define two^52 4503599627370496)
  (define two^53 9007199254740992)
  (define two^54 18014398509481984)
  (define two^63 9223372036854775808)

  (define flonum:minexponent    -1023)
  (define flonum:minexponent-51 -1074)
  (define flonum:maxexponent     1023)
  (define flonum:+infinity      1e500)
  (define flonum:-infinity     -1e500)
  
  (define (flonum-infinity? x)
    (or (= x flonum:+infinity) (= x flonum:-infinity)))

  (define (flonum-nan? x)
    (not (= x x)))

  ; Convert a bignum to an IEEE double precision number.

  (define (%bignum->flonum b)
    
    ; Count leading zeroes in a bigit by shifting right.
    ; `n' and `e' are always nonnegative fixnums.
    
    (define (leading-zeroes n e)
      (if (zero? n)
          (- bits-per-bigit e)
          (leading-zeroes (fxrsha n 1) (+ e 1))))
    
    ; Returns the greatest fixnum n such that (<= (expt 2 n) (abs b)).
    
    (define (floor-of-lg b)
      (let* ((l (bignum-length b))
             (v (bignum-ref b (- l 1))))
        (- (* l bits-per-bigit) (leading-zeroes v 0) 1)))
    
    ; Given:  b0 and e0 such that
    ;     2^52 <= b0 < 2^53 <= b0 * 2^{e0} <= b < (b0 + 1) * 2^{e0}
    ; Returns:  Best flonum approximation to b.
    
    (define (convert-large b0 e0)
      (if (> e0 flonum:maxexponent)
          flonum:+infinity
          (let* ((x0 (make-flonum 0 b0 e0))
                 (b0+1 (+ b0 1))
                 (x1 (if (= b0+1 two^53)
                         (make-flonum 0 two^52 (+ e0 1))
                         (make-flonum 0 b0+1 e0)))
                 (d0 (- b (%flonum->bignum x0)))
                 (d1 (- (%flonum->bignum x1) b)))
            (cond ((< d0 d1) x0)
                  ((< d1 d0) x1)
                  ((even? b0) x0)
                  (else x1)))))
    
    ; Bignums are not usually zero, but it happens in system code.
    
    (cond ((negative? b)
           (- (%bignum->flonum (- b))))
          ((zero? b)
           0.0)
          (else
           (let* ((e (floor-of-lg b))
                  (e-52 (- e 52)))
             (if (<= e-52 0)
                 (make-flonum 0
                              (* b (expt 2 (- e-52)))
                              e)
                 (convert-large (quotient b (expt 2 e-52))
                                e))))))

  ; Convert an exact integer (fixnum or bignum) to a bignum

  (define (->bignum x)
    (if (fixnum? x) (fixnum->bignum x) x))

  ; Convert a flonum to a bignum by truncating it if necessary.
  ; This must work properly in the case of a compnum with zero imag part!
  ; Independent of endianness.

  (define (%flonum->bignum f)
    (if (or (flonum-infinity? f)
            (flonum-nan? f))
        (error "Can't convert " f " to an exact number."))
    (let ((q (->bignum
              (let* ((f (round f))
                     (m (float-significand f))
                     (e (float-unbiased-exponent f)))
                (cond ((= e 52)
                       m)
                      ((> e 52)
                       (* m (expt 2 (- e 52))))
                      ((< e 0)
                       0)
                      (else
                       ; 0 < e < 52  [ what happened to e=0 case? ]
                       (let ((divisor (expt 2 (abs (- e 52)))))
                         (quotient m divisor))))))))
      (if (not (zero? (float-sign f)))
          (big-flip-sign! q))
      q))

  ; Convert a flonum to an exact integer.

  (define (%flonum->integer a)
    (big-normalize! (%flonum->bignum a)))

  ; Given two flonums 'real' and 'imag' create a compnum from the two.
  ; Independent of endianness.

  (define (%make-compnum real imag)

    (define (cp from i to j c)
      (if (zero? c)
          #t
          (begin (bytevector-like-set! to j (bytevector-like-ref from i))
                 (cp from (+ i 1) to (+ j 1) (- c 1)))))

    (if (not (and (flonum? real) (flonum? imag)))
        (begin
          (error "make-compnum: not a flonum: " (if (flonum? real) imag real))
          #t)
        (let ((b (make-bytevector 20)))
          (cp real 4 b 4 8)
          (cp imag 4 b 12 8)
          (typetag-set! b sys$tag.compnum-typetag)
          b)))

  ; For internal use only.
  ; Flonum should not be an integer (although this implementation works
  ; anyway).

  (define (%flonum->ratnum f)
    (if (or (flonum-infinity? f)
            (flonum-nan? f))
        (error "Can't convert " f " to an exact number."))
    (let ((q (let* ((m (float-significand f))
                    (e (float-unbiased-exponent f)))
               (cond ((>= e 52)
                      (* m (expt 2 (- e 52))))
                     (else
                      (/ m (expt 2 (abs (- e 52)))))))))
      (if (not (zero? (float-sign f)))
          (- q)
          q)))

  ; Create a bytevector-like structure that can hold a flonum.

  (define (%make-flonum-datum)
    (let ((b (make-bytevector 12)))
      (typetag-set! b sys$tag.flonum-typetag)
      b))

  (set! make-flonum-datum %make-flonum-datum)
  (set! make-compnum %make-compnum)
  (set! bignum->flonum %bignum->flonum)
  (set! flonum->bignum %flonum->bignum)
  (set! flonum->integer %flonum->integer)
  (set! flonum->ratnum %flonum->ratnum)

  'flonums)

; No type checking, as these are not public.

(define (flonum:log x)
  (syscall syscall:flonum-log x (make-flonum-datum)))

(define (flonum:exp x)
  (syscall syscall:flonum-exp x (make-flonum-datum)))

(define (flonum:sin x)
  (syscall syscall:flonum-sin x (make-flonum-datum)))

(define (flonum:cos x)
  (syscall syscall:flonum-cos x (make-flonum-datum)))

(define (flonum:tan x)
  (syscall syscall:flonum-tan x (make-flonum-datum)))

(define (flonum:asin x)
  (syscall syscall:flonum-asin x (make-flonum-datum)))

(define (flonum:acos x)
  (syscall syscall:flonum-acos x (make-flonum-datum)))

(define (flonum:atan x)
  (syscall syscall:flonum-atan x (make-flonum-datum)))

(define (flonum:atan2 x y)
  (syscall syscall:flonum-atan2 x y (make-flonum-datum)))

(define (flonum:sqrt x)
  (syscall syscall:flonum-sqrt x (make-flonum-datum)))

(define (flonum:sinh x)
  (syscall syscall:flonum-sinh x (make-flonum-datum)))

(define (flonum:cosh x)
  (syscall syscall:flonum-cosh x (make-flonum-datum)))

; FIXME:  This seems out of place.

(define (flonum:time)
  (syscall syscall:time (make-flonum-datum)))

; eof

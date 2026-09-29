; Logical operations for the Racket host.

(define fxlogior bitwise-ior)
(define fxlogand bitwise-and)
(define fxlogxor bitwise-xor)
(define fxlognot bitwise-not)

(define fxlsh arithmetic-shift)

;; Logical right shift, emulating the target's fixnum width so that
;; behavior matches Larceny's own fxrshl.  See the comment in
;; Compat/MzScheme/logops.ss; callers mask the result anyway.

(define (fxrshl n m)
  (if (< n 0)
      (let ((bits (- (nbuild-parameter 'word-size) 2)))
        (arithmetic-shift (bitwise-and n (- (expt 2 bits) 1)) (- 0 m)))
      (arithmetic-shift n (- 0 m))))

(define (fxrsha n m) (arithmetic-shift n (- 0 m)))

(define logior bitwise-ior)
(define logand bitwise-and)
(define logxor bitwise-xor)
(define lognot bitwise-not)
(define lsh arithmetic-shift)
(define (rsha n m) (arithmetic-shift n (- 0 m)))
(define (rshl n m) (fxrshl n m))

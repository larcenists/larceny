; $Id$

;; Logical operations

(define logior bitwise-ior)
(define logand bitwise-and)
(define logxor bitwise-xor)
(define lognot bitwise-not)

(define lsh arithmetic-shift)
;(define (rshl n m) (arithmetic-shift n (- 0 m)))
(define (rsha n m) (arithmetic-shift n (- 0 m)))

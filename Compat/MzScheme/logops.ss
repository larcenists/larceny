; $Id$

;; Logical operations

(define logior bitwise-ior)
(define logand bitwise-and)
(define logxor bitwise-xor)
(define lognot bitwise-not)

(define lsh arithmetic-shift)


;; This is hardcoded for 30 bit words (to ensure that its as much like
;; the Larceny implementation we're emulating as possible.  Its only
;; used in asmutil32*.sch, so that seem safe.  It would be a good idea
;; to go over the references to rshl and ensure that every value
;; produced by a call to rsh eventually flows into a logand that cuts
;; off the upper most bits; then we would know that we could replace
;; this implementation with one that did not need to emulate 30 bit
;; twos-complement.
(define (rshl n m)
  (if (< n 0)
      (arithmetic-shift (bitwise-and n (- (expt 2 30) 1)) (- 0 m))
      (arithmetic-shift n (- 0 m))))
(define (rsha n m) (arithmetic-shift n (- 0 m)))

; Copyright Lightship Software, Incorporated.
; $Id$
;
; 2000-08-29 / lth

; (MAKE-RANDOM-GENERATOR [seed])
;   Returns a procedure that accepts a modulus r and returns a 
;   pseudorandom integer in the range 0..r-1 (a new number on every
;   call).  If the seed is supplied then it is used to seed the
;   stream; if not, a quasi-random seed is chosen.  Seed must be 
;   a fixnum.

(define (make-random-generator . rest)
  (let* ((a 701)
         (c 743483)
         (m 524287)
         (x (fxlogand (if (null? rest)
                        (quasi-random-seed)
                        (car rest))
                    m)))
    (letrec ((random14
              (lambda (n)
                (set! x (fxlogand (+ (* a x) c) m))
                (remainder (fxrshl x 3) n)))
             (loop
              (lambda (q r n)
                (if (zero? q)
                    (remainder r n)
                    (loop (quotient q 16384)
                          (+ (* 16384 r) (random14 16384))
                          n)))))
      (lambda (n)
        (if (and (fixnum? n) (< n 16384))
            (random14 n)
            (loop (quotient n 16384) (random14 16384) n))))))

; (QUASI-RANDOM-SEED)
;   Returns a fixnum that is not uniformly distributed over anything in
;   any sense -- it's just an arbitrary number that is unlikely to recur
;   in some unspecified smallish number of consecutive runs.  A valid
;   implementation would be to store a counter in a file and return
;   a new on every call.

(define (quasi-random-seed)
  (remainder 
   (cond ((file-exists? "/dev/proc")
          (apply * (vector->list (file-modification-time "/dev/proc"))))
         ((file-exists? "/dev/fd")
          (apply * (vector->list (file-modification-time "/dev/fd"))))
         ((file-exists? "/tmp")
          (apply * (vector->list (file-modification-time "/tmp"))))
         ((getenv "HOME")
          => (lambda (x)
               (apply * (vector->list (file-modification-time x)))))
         (else 1))
   (most-positive-fixnum)))

; eof

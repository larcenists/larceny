; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Standard-C machine assembler flags.

(define runtime-safety-checking
  (make-twobit-flag 'runtime-safety-checking))

(define catch-undefined-globals
  (make-twobit-flag 'catch-undefined-globals))

(define inline-allocation
  (make-twobit-flag 'inline-allocation))
  
(define inline-assignment
  (make-twobit-flag 'inline-assignment))

(define peephole-optimization
  (make-twobit-flag 'peephole-optimization))

; Backwards compatible

(define (single-stepping . rest) #f)    ; Not a switch

(define (unsafe-code . args)
  (if (null? args)
      (not (runtime-safety-checking))
      (runtime-safety-checking (not (car args)))))

(define (display-assembler-flags which)
  (case which
    ((debugging) #t)
    ((safety)
     (display-twobit-flag runtime-safety-checking)
     (if (runtime-safety-checking)
         (begin (display "  ")
                (display-twobit-flag catch-undefined-globals))))
    ((optimization)
     (display-twobit-flag peephole-optimization)
     (display-twobit-flag inline-allocation)
     (display-twobit-flag inline-assignment))
    (else #t)))

(define (set-assembler-flags! mode)
  (case mode
    ((no-optimization)
     (set-assembler-flags! 'standard)
     (peephole-optimization #f))
    ((standard)
     (runtime-safety-checking #t)
     (catch-undefined-globals #t)
     (inline-allocation #f)
     (inline-assignment #f)
     (peephole-optimization #t))
    ((fast-safe)
     (set-assembler-flags! 'standard)
     (inline-allocation #f)
     (inline-assignment #f))
    ((fast-unsafe)
     (set-assembler-flags! 'standard)
     (catch-undefined-globals #f)
     (runtime-safety-checking #f))
    (else ???)))

(set-assembler-flags! 'standard)

; eof

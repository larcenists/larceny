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

(define (assembler-all-flags)
  (let ((r.s.c (runtime-safety-checking))
        (c.u.g (catch-undefined-globals))
        (i.al  (inline-allocation))
        (i.as  (inline-assignment))
        (p.o   (peephole-optimization))
        (s.s   (single-stepping)))
    (lambda ()
      (runtime-safety-checking r.s.c)
      (catch-undefined-globals c.u.g)
      (inline-allocation i.al)
      (inline-assignment i.as)
      (peephole-optimization p.o)
      (single-stepping s.s))))

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
    ((no-optimization standard fast-safe)
     (inline-allocation #f)
     (inline-assignment #f)
     (runtime-safety-checking #t)
     (catch-undefined-globals #t)
     (peephole-optimization #f))
    ((fast-unsafe)
     (set-assembler-flags! 'standard)
     (runtime-safety-checking #f))
    (else 
     (error "set-assembler-flags!: unknown mode: " mode))))

(set-assembler-flags! 'standard)

(display "asm-switches loaded") (newline)


; eof

; Copyright 1998 Lars T Hansen.
;
; $Id: asm-switches.sch 2543 2005-07-20 21:54:03Z pnkfelix $
;
; Fence machine assembler flags.

; If (runtime-safety-checking) = #f then (catch-undefined-globals) is ignored.

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

(define optimize-c-code
  (make-twobit-flag "optimize-c-code"))

(define single-stepping
  (make-twobit-flag "single-stepping"))

; cant-optimization may be overridden by procedures in machine-specific optimizers.

(define (cant-optimization . rest)
  #f)

; Backwards compatible

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
     (display-twobit-flag inline-assignment)
     (display-twobit-flag optimize-c-code))
    (else #t)))

(define (assembler-all-flags)
  (let ((r.s.c (runtime-safety-checking))
        (c.u.g (catch-undefined-globals))
        (i.a   (inline-allocation))
        (p.o   (peephole-optimization))
        (o.o.c (optimize-c-code))
        (s.s   (single-stepping)))
    (lambda ()
      (runtime-safety-checking r.s.c)
      (catch-undefined-globals c.u.g)
      (inline-allocation i.a)
      (peephole-optimization p.o)
      (optimize-c-code o.o.c)
      (single-stepping s.s))))

(define (assembler-global-optimization-flags)
  (lambda ()
    #t))

(define (assembler-runtime-safety-flags)
  (let ((r.s.c (runtime-safety-checking))
        (c.u.g (catch-undefined-globals)))
    (lambda ()
      (runtime-safety-checking r.s.c)
      (catch-undefined-globals c.u.g))))

(define (set-assembler-flags! mode)
  (case mode
    ((no-optimization)
     (set-assembler-flags! 'standard)
     (optimize-c-code #f)
     (peephole-optimization #f))
    ((standard)
     (single-stepping #f)
     (runtime-safety-checking #t)
     (catch-undefined-globals #t)
     (optimize-c-code #t)
     (inline-allocation #f)
     (inline-assignment #f)
     (peephole-optimization #t))
    ((fast-safe)
     (set-assembler-flags! 'standard)
     (inline-allocation #t)
     (inline-assignment #t))
    ((fast-unsafe)
     (set-assembler-flags! 'standard)
     (catch-undefined-globals #f)
     (runtime-safety-checking #f))
    (else ???)))

(set-assembler-flags! 'standard)

; eof

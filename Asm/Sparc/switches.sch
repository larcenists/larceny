; Asm/Sparc/switches.sch
; Larceny -- Switches for the Sparc assembler
;
; $Id$

; INTERNAL!
(define short-effective-addresses
  (make-twobit-flag 'short-effective-addresses))

(define unsafe-code
  (make-twobit-flag 'unsafe-code))

(define catch-undefined-globals
  (make-twobit-flag 'catch-undefined-globals))

(define inline-allocation
  (make-twobit-flag 'inline-allocation))
  
;(define inline-assignment
;  (make-twobit-flag 'inline-assignment))

(define write-barrier
  (make-twobit-flag 'write-barrier))  

(define peephole-optimization
  (make-twobit-flag 'peephole-optimization))

(define single-stepping
  (make-twobit-flag 'single-stepping))

(define fill-delay-slots
  (make-twobit-flag 'fill-delay-slots))

(define (display-assembler-flags)
  (display "SPARC Assembler flags") (newline)
  (display-twobit-flag catch-undefined-globals)
  (display-twobit-flag inline-allocation)
;  (display-twobit-flag inline-assignment)
  (display-twobit-flag peephole-optimization)
  (display-twobit-flag unsafe-code)
  (display-twobit-flag write-barrier)
  (display-twobit-flag single-stepping)
  (display-twobit-flag fill-delay-slots))

(define (set-assembler-flags! mode)
  (case mode
    ((no-optimization)
     (set-assembler-flags! 'default)
     (peephole-optimization #f)
     (fill-delay-slots #f))
    ((default)
     (short-effective-addresses #t)
     (catch-undefined-globals #t)
     (inline-allocation #f)
     (peephole-optimization #t)
     (unsafe-code #f)
     (write-barrier #t)
     (single-stepping #f)
     (fill-delay-slots #t))
    ((fast-safe)
     (set-assembler-flags! 'default)
     ; (inline-assignment #t)
     (inline-allocation #t))
    ((fast-unsafe)
     (set-assembler-flags! 'fast-safe)
     (catch-undefined-globals #f)
     (unsafe-code #t))
    (else
     (error "set-assembler-flags!: unknown mode " mode))))

(set-assembler-flags! 'default)

; eof

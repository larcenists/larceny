; Asm/Sparc/switches.sch
; Larceny -- Switches for the Sparc assembler
;
; $Id: switches.sch,v 1.3 1997/09/23 20:02:38 lth Exp lth $

; INTERNAL!
(define short-effective-addresses
  (make-twobit-flag 'short-effective-addresses))

(define unsafe-code
  (make-twobit-flag 'unsafe-code))

(define catch-undefined-globals
  (make-twobit-flag 'catch-undefined-globals))

(define inline-cons
  (make-twobit-flag 'inline-cons))
  
(define inline-assignment
  (make-twobit-flag 'inline-assignment))

(define write-barrier
  (make-twobit-flag 'write-barrier))  

(define peephole-optimization
  (make-twobit-flag 'peephole-optimization))

(define single-stepping
  (make-twobit-flag 'single-stepping))

(define fill-delay-slots
  (make-twobit-flag 'fill-delay-slots))

;(define (fast-write-barrier)
;  (eq? (target-architecture) 'sun4-sunos))

(define (fast-write-barrier) #f)

; Initialization

(short-effective-addresses #t)  ; Faster code (still safe).
(inline-cons #f)                ; Allocation of pairs in-line.
(inline-assignment #f)          ; Assignment generation check in-line.
(write-barrier #t)              ; For generational garbage collection.
(catch-undefined-globals #t)    ; #!undefined causes error.
(unsafe-code #f)                ; Generate unsafe code.
(single-stepping #f)            ; Single-step MacScheme instructions.
(fill-delay-slots #t)           ; Branch delay slot filling in assembler.

; eof

; Copyright 1994 William Clinger
;
; $Id$
;
; 8 April 1999
;
; Compiler switches needed by Twobit.

(define make-twobit-flag)
(define display-twobit-flag)

(define make-twobit-flag
  (lambda (name)

    (define (twobit-warning)
      (display "Error: incorrect arguments to ")
      (write name)
      (newline)
      (reset))

    (define (display-flag state)
      (display (if state "  + " "  - "))
      (display name)
      (display " is ")
      (display (if state "on" "off"))
      (newline))

    (let ((state #t))
      (lambda args
        (cond ((null? args) state)
              ((and (null? (cdr args))
                    (boolean? (car args)))
               (set! state (car args))
               state)
              ((and (null? (cdr args))
                    (eq? (car args) 'display))
               (display-flag state))
              (else (twobit-warning)))))))

(define (display-twobit-flag flag)
  (flag 'display))
  
; Debugging and convenience.

(define issue-warnings
  (make-twobit-flag 'issue-warnings))

(define include-source-code
  (make-twobit-flag 'include-source-code))

(define include-variable-names
  (make-twobit-flag 'include-variable-names))

(define include-procedure-names
  (make-twobit-flag 'include-procedure-names))

; Space efficiency.
; This switch isn't fully implemented yet.  If it is true, then
; Twobit will generate flat closures and will go to some trouble
; to zero stale registers and stack slots.
; Don't turn this switch off unless space is more important than speed.

(define avoid-space-leaks
  (make-twobit-flag 'avoid-space-leaks))

; Major optimizations.

(define integrate-usual-procedures
  (make-twobit-flag 'integrate-usual-procedures))

(define control-optimization
  (make-twobit-flag 'control-optimization))

(define parallel-assignment-optimization
  (make-twobit-flag 'parallel-assignment-optimization))

(define lambda-optimization
  (make-twobit-flag 'lambda-optimization))

(define benchmark-mode
  (make-twobit-flag 'benchmark-mode))

(define benchmark-block-mode
  (make-twobit-flag 'benchmark-block-mode))

(define global-optimization
  (make-twobit-flag 'global-optimization))

(define interprocedural-inlining
  (make-twobit-flag 'interprocedural-inlining))

(define interprocedural-constant-propagation
  (make-twobit-flag 'interprocedural-constant-propagation))

(define common-subexpression-elimination
  (make-twobit-flag 'common-subexpression-elimination))

(define representation-inference
  (make-twobit-flag 'representation-inference))

(define local-optimization
  (make-twobit-flag 'local-optimization))

; For backwards compatibility, until I can change the code.

(define (ignore-space-leaks . args)
  (if (null? args)
      (not (avoid-space-leaks))
      (avoid-space-leaks (not (car args)))))

(define lambda-optimizations lambda-optimization)
(define local-optimizations local-optimization)

(define (set-compiler-flags! how)
  (case how
    ((no-optimization)
     (set-compiler-flags! 'standard)
     (avoid-space-leaks #t)
     (integrate-usual-procedures #f)
     (control-optimization #f)
     (parallel-assignment-optimization #f)
     (lambda-optimization #f)
     (benchmark-mode #f)
     (benchmark-block-mode #f)
     (global-optimization #f)
     (interprocedural-inlining #f)
     (interprocedural-constant-propagation #f)
     (common-subexpression-elimination #f)
     (representation-inference #f)
     (local-optimization #f))
    ((standard) 
     (issue-warnings #t)
     (include-source-code #f)
     (include-procedure-names #t)
     (include-variable-names #t)
     (avoid-space-leaks #f)
     (runtime-safety-checking #t)
     (integrate-usual-procedures #f)
     (control-optimization #t)
     (parallel-assignment-optimization #t)
     (lambda-optimization #t)
     (benchmark-mode #f)
     (benchmark-block-mode #f)
     (global-optimization #t)
     (interprocedural-inlining #t)
     (interprocedural-constant-propagation #t)
     (common-subexpression-elimination #t)
     (representation-inference #t)
     (local-optimization #t))
    ((fast-safe) 
     (set-compiler-flags! 'standard)
     (integrate-usual-procedures #t)
     (benchmark-mode #t))
    ((fast-unsafe) 
     (set-compiler-flags! 'fast-safe)
     (runtime-safety-checking #f))
    (else 
     (error "set-compiler-flags!: unknown mode " how))))

(define (display-twobit-flags which)
  (case which
    ((debugging)
     (display-twobit-flag issue-warnings)
     (display-twobit-flag include-procedure-names)
     (display-twobit-flag include-variable-names)
     (display-twobit-flag include-source-code))
    ((safety)
     (display-twobit-flag avoid-space-leaks))
    ((optimization)
     (display-twobit-flag integrate-usual-procedures)
     (display-twobit-flag control-optimization)
     (display-twobit-flag parallel-assignment-optimization)
     (display-twobit-flag lambda-optimization)
     (display-twobit-flag benchmark-mode)
     (display-twobit-flag benchmark-block-mode)
     (display-twobit-flag global-optimization)
     (if (global-optimization)
         (begin (display "  ")
                (display-twobit-flag interprocedural-inlining)
                (display "  ")
                (display-twobit-flag interprocedural-constant-propagation)
                (display "  ")
                (display-twobit-flag common-subexpression-elimination)
                (display "  ")
                (display-twobit-flag representation-inference)))
     (display-twobit-flag local-optimization))
    (else
     ; The switch might mean something to the assembler, but not to Twobit
     #t)))

; eof

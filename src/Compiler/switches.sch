; Copyright 1994 William Clinger
;
; $Id$
;
; 2000-01-11 / lth
;
; Compiler switches needed by Twobit.

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

(define compile-despite-errors
  (make-twobit-flag 'compile-despite-errors))

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

(define integrate-procedures
  (let ((state 'larceny))

    (define (twobit-warning)
      (display "Error: incorrect arguments to integrate-procedures")
      (newline)
      (reset))

    (lambda args
      (cond ((null? args) state)
            ((null? (cdr args))
             (case (car args)
               ((#t larceny) (set! state 'larceny) state)
               ((#f none)    (set! state 'none) state)
               ((display)    (display (if (eq? state 'none)
                                          "  -"
                                          "  +"))
                             (display " integrate-procedures is \"")
                             (display state)
                             (display "\"")
                             (newline))
               (else
                (twobit-warning))))
            (else
             (twobit-warning))))))

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

; meta-switches

(define (twobit-global-optimization-flags)
  (let ((g.o   (global-optimization))
        (i.i   (interprocedural-inlining))
        (i.c.p (interprocedural-constant-propagation))
        (c.s.e (common-subexpression-elimination))
        (r.i   (representation-inference)))
    (lambda ()
      (global-optimization g.o)
      (interprocedural-inlining i.i)
      (interprocedural-constant-propagation i.c.p)
      (common-subexpression-elimination c.s.e)
      (representation-inference r.i))))

(define (twobit-runtime-safety-flags)
  (lambda ()
    #t))

(define (twobit-all-flags)
  (let ((c.d.e (compile-despite-errors))
        (i.w   (issue-warnings))
        (i.s.c (include-source-code))
        (i.v.n (include-variable-names))
        (i.p.n (include-procedure-names))
        (a.s.l (avoid-space-leaks))
        (i.p   (integrate-procedures))
        (c.o   (control-optimization))
        (p.a.o (parallel-assignment-optimization))
        (la.o   (lambda-optimization))
        (b.m   (benchmark-mode))
        (b.b.m (benchmark-block-mode))
        (g.o   (global-optimization))
        (i.i   (interprocedural-inlining))
        (i.c.p (interprocedural-constant-propagation))
        (c.s.e (common-subexpression-elimination))
        (r.i   (representation-inference))
        (lo.o   (local-optimization)))
    (lambda ()
      (compile-despite-errors c.d.e)
      (issue-warnings i.w)
      (include-source-code i.s.c)
      (include-variable-names i.v.n)
      (include-procedure-names i.p.n)
      (avoid-space-leaks a.s.l)
      (integrate-procedures i.p)
      (control-optimization c.o)
      (parallel-assignment-optimization p.a.o)
      (lambda-optimization la.o)
      (benchmark-mode b.m)
      (benchmark-block-mode b.b.m)
      (global-optimization g.o)
      (interprocedural-inlining i.i)
      (interprocedural-constant-propagation i.c.p)
      (common-subexpression-elimination c.s.e)
      (representation-inference r.i)
      (local-optimization lo.o))))


; Control

(define (set-compiler-flags! how)
  (case how
    ((no-optimization)
     (set-compiler-flags! 'standard)
     (avoid-space-leaks #t)
     (integrate-procedures 'none)
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
     (compile-despite-errors #t)
     (issue-warnings #t)
     (include-source-code #f)
     (include-procedure-names #t)
     (include-variable-names #t)
     (avoid-space-leaks #f)
     (runtime-safety-checking #t)
     (integrate-procedures 'none)
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
     (let ((bbmode (benchmark-block-mode)))
       (set-compiler-flags! 'standard)
       (compile-despite-errors #f)
       (integrate-procedures 'larceny)
       (benchmark-mode #t)
       (benchmark-block-mode bbmode)))
    ((fast-unsafe) 
     (set-compiler-flags! 'fast-safe)
     (runtime-safety-checking #f))
    (else 
     (error "set-compiler-flags!: unknown mode " how))))

(define (display-twobit-flags which)
  (case which
    ((debugging)
     (display-twobit-flag compile-despite-errors)
     (display-twobit-flag issue-warnings)
     (display-twobit-flag include-procedure-names)
     (display-twobit-flag include-variable-names)
     (display-twobit-flag include-source-code))
    ((safety)
     (display-twobit-flag avoid-space-leaks))
    ((optimization)
     (display-twobit-flag integrate-procedures)
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

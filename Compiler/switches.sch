; Copyright 1994 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 12 October 1998 (Larceny-specific)
;
; Compiler switches needed by Twobit.

(define (make-twobit-flag name)
  (define (twobit-warning)
    (display "Error: incorrect arguments to ")
    (write name)
    (newline)
    (reset))
  (let ((state #t))
    (lambda args
      (cond ((null? args) state)
            ((and (null? (cdr args))
                  (boolean? (car args)))
             (set! state (car args))
             state)
            (else (twobit-warning))))))

; Debugging and convenience.

(define issue-warnings
  (make-twobit-flag 'issue-warnings))

(define include-source-code
  (make-twobit-flag 'include-source-code))

(define include-variable-names
  (make-twobit-flag 'include-variable-names))

(define include-procedure-names
  (make-twobit-flag 'include-procedure-names))

; Semantics.
; Is this still supported?  If so, then why?

(define empty-list-is-true
  (make-twobit-flag 'empty-list-is-true))

; Space efficiency.
; This switch isn't fully implemented yet.  If it is false, then
; Twobit will generate flat closures and will go to some trouble
; to zero stale registers and stack slots.
; Don't turn this switch off unless space is more important than speed.

(define ignore-space-leaks
  (make-twobit-flag 'ignore-space-leaks))

; Major optimizations.

(define integrate-usual-procedures
  (make-twobit-flag 'integrate-usual-procedures))

(define benchmark-mode
  (make-twobit-flag 'benchmark-mode))

(define local-optimizations
  (make-twobit-flag 'local-optimizations))

(define global-optimizations
  (make-twobit-flag 'local-optimizations))

(define representation-optimizations
  (make-twobit-flag 'representation-optimizations))

(define lambda-optimizations
  (make-twobit-flag 'lambda-optimizations))

(define parallel-assignment-optimization
  (make-twobit-flag 'parallel-assignment-optimization))

; For bootstrap heap dumper

(define generate-global-symbols
  (make-twobit-flag 'generate-global-symbols))

; eof

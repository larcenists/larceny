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
; 24 May 1995 (Larceny-specific)
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

(define integrate-usual-procedures
  (make-twobit-flag 'integrate-usual-procedures))

(define benchmark-mode
  (make-twobit-flag 'benchmark-mode))

(define include-source-code
  (make-twobit-flag 'include-source-code))

(define include-variable-names
  (make-twobit-flag 'include-variable-names))

(define include-procedure-names
  (make-twobit-flag 'include-procedure-names))

(define empty-list-is-true
  (make-twobit-flag 'empty-list-is-true))

(define local-optimizations
  (make-twobit-flag 'local-optimizations))

(define issue-warnings
  (make-twobit-flag 'issue-warnings))

; For bootstrap heap dumper

(define generate-global-symbols
  (make-twobit-flag 'generate-global-symbols))

; eof


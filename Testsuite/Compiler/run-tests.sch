; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Driver for compiler test suite.

(load "../Lib/test.sch")

(define switches
  (list avoid-space-leaks
        integrate-usual-procedures
        control-optimization
        parallel-assignment-optimization
        lambda-optimization
        benchmark-mode
        benchmark-block-mode
        ;global-optimization ; must be on for some of these tests
        interprocedural-inlining
        interprocedural-constant-propagation
        common-subexpression-elimination
        ;representation-inference ; not yet implemented
        local-optimization
        
        ; Assembler switches
        
        ;peephole-optimization
        ;inline-allocation
        ;fill-delay-slots
        ;runtime-safety-checking
        ;catch-undefined-globals
        ))

(define files
  '("p2tests" "p4tests"))

(define (run-compiler-tests)

  (define (set-switches! i)
    (do ((l switches (cdr l))
         (i i (quotient i 2)))
        ((null? l))
      ((car l) (if (zero? (remainder i 2)) #f #t))))

  (test-reporter (lambda (id answer correct)
                   (compiler-switches)))
  (let ((k (expt 2 (length switches))))
    (do ((i 0 (+ i 1)))
        ((= i k))
      (display ".") (flush-output-port)
      (set-switches! i)
      (for-each (lambda (fn)
                  (compile-file (string-append fn ".sch")))
                files)
      (for-each (lambda (fn)
                  (load (string-append fn ".fasl")))
                files))))

; Warnings are too annoying with this test.

(display "*** Note: Turning off warnings.") (newline)
(display "To enable warnings, evaluate (issue-warnings #t).") (newline)
(issue-warnings #f)

; eof

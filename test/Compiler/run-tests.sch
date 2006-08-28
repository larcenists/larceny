; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Driver for compiler test suite.

(load "../Lib/test.sch")

(define-syntax define-options
  (syntax-rules ()
    ((define-options name opt ...)
         (define name (list (cons 'opt opt) ...)))))

; It takes too long to test every combination of compiler switches,
; so we have to factor them into moderately independent sets.

; A small set of switches is useful for quick sanity checks.
(define-options sanity-switches
  integrate-procedures
  global-optimization
  peephole-optimization
  runtime-safety-checking
  )

; Switches that exist primarily for compiler research,
; and are seldom disabled by real users.
(define-options basic-switches
  avoid-space-leaks
  control-optimization
  parallel-assignment-optimization
  lambda-optimization
  )

; Compiler and assembler switches that are likely to interact.
(define-options optimization-switches
  integrate-procedures
  benchmark-mode
;  benchmark-block-mode
;  global-optimization ; must be on for some of these tests
  interprocedural-inlining
  interprocedural-constant-propagation
  common-subexpression-elimination
  representation-inference
  local-optimization

  ; Assembler switches
  peephole-optimization
  runtime-safety-checking
  )

; Assembler switches that are fairly independent of the compiler.
(define-options backend-switches
  inline-allocation
;  fill-delay-slots
  catch-undefined-globals
  )

(define files
  '("p2tests" "p4tests" "primtests"))

(define (fold-switches on off i l)
  (if (null? l) '()
    ((if (zero? (remainder i 2)) off on)
     (car l)
     (decode-switches (quotient i 2) (cdr l)))))

(define (decode-switches i l)
  (fold-switches (lambda (a b) (cons (car a) b))
                (lambda (a b) b)
                i l))

;; run-compiler-tests : [ list-of(switches) [ starting-test ] ] ->
(define (run-compiler-tests . rest)
  ;; default arguments:
  (define switches (if (null? rest) optimization-switches (car rest)))
  (define starting-num (if (or (null? rest) (null? (cdr rest))) 0 (cadr rest)))

  (define (set-switches! i)
    (fold-switches (lambda (a b) ((cdr a) #t))
                   (lambda (a b) ((cdr a) #f))
                   i switches))

  (define (faslify base i)
    (string-append base "-" (number->string i) ".fasl"))

  ;;; What does this next line do?  AFAICT, nothing.
  ;(test-reporter (lambda (id answer correct)
                   ;(compiler-switches)))
  (let ((k (expt 2 (length switches))))
    (do ((i starting-num (+ i 1)))
        ((= i k))
      (display ">>>> Starting test ") (display i) (newline)
      (flush-output-port)
      (set-switches! i)
      (for-each (lambda (fn)
                  (display ">> Compiling ") (display fn) (newline)
                  (flush-output-port)
                  (compile-file (string-append fn ".sch")
                                (faslify fn i))
                  (flush-output-port))
                files)
      (for-each (lambda (fn)
                  (display ">> Loading ") (display fn) (newline)
                  (flush-output-port)
                  (load (faslify fn i))
                  (flush-output-port))
                files)))
  (newline))

; Warnings are too annoying with this test.

(display "*** Note: Turning off warnings.") (newline)
(display "To enable warnings, evaluate (issue-warnings #t).") (newline)
(issue-warnings #f)

(newline)
(display "To run tests: (run-compiler-tests [ SWITCHES ])") (newline)
(display "SWITCHES: sanity-switches | basic-switches") (newline)
(display "    | optimization-switches | backend-switches") (newline)

(newline)
(display "To decode switches, use (decode-switches TEST-NUMBER SWITCHES)")
(newline)

; eof

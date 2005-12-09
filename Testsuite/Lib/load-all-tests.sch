; Testsuite/Lib/load-all-tests.sch
; Larceny test suite -- load script for Library test suite.
;
; $Id$

(load "test.fasl")			; Scaffolding

(load "bool.fasl")			; Booleans
(load "char.fasl")			; Characters
;(load "complex.fasl")                   ; Complex numbers
(load "ctak.fasl")			; Call/cc test
(load "dynamic-wind.fasl")              ; Dynamic-wind test
;(load "env.fasl")                       ; Environments
(load "fact.fasl")                      ; Factorial, big and small
(load "fib.fasl")			; Fibonacci function
;(load "fixnums.fasl")                   ; Fixnum-specific operations
(load "number.fasl")			; Numbers and arithmetic
(load "pred.fasl")                      ; Predicates
;(load "regression.fasl")		; Past error cases

(define (run-all-tests)
  (run-boolean-tests)
  (run-char-tests)
  (run-predicate-tests)
  (run-number-tests)
  (run-fact-tests)
  (run-fib-tests)
  (run-ctak-tests)
  (run-dynamic-wind-tests)
  ;(run-env-tests)
  ;(run-regression-tests)
  ;(run-fixnum-tests)
  )


; eof

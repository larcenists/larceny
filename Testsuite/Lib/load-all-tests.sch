; Testsuite/Lib/load-all-tests.sch
; Larceny test suite -- load script for Library test suite.
;
; $Id: load-all-tests.sch,v 1.1.1.1 1998/11/19 21:52:33 lth Exp $

(load "test.fasl")			; Scaffolding
(load "fib.fasl")			; Fibonacci test
(load "ctak.fasl")			; Call/cc test
(load "number.fasl")			; Numbers and arithmetic
(load "char.fasl")			; Characters
(load "bool.fasl")			; Booleans
;(load "env.fasl")                       ; Environments

(define (run-all-tests)
  (run-fib-tests)
  (run-ctak-tests)
  (run-number-tests)
  (run-char-tests)
  (run-boolean-tests)
  (run-env-tests))


; eof

; Testsuite/Lib/load-all-tests.sch
; Larceny test suite -- load script for Library test suite.
;
; $Id$

(load "test.fasl")			; Scaffolding
(load "fib.fasl")			; Fibonacci test
(load "ctak.fasl")			; Call/cc test
(load "number.fasl")			; Numbers and arithmetic
(load "char.fasl")			; Characters
(load "bool.fasl")			; Booleans

(define (run-all-tests)
  (run-fib-tests)
  (run-ctak-tests)
  (run-number-tests)
  (run-char-tests)
  (run-boolean-tests))


; eof

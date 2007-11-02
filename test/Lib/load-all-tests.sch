; Testsuite/Lib/load-all-tests.sch
; Larceny test suite -- load script for Library test suite.
;
; $Id$

(compile-file "test.sch")

(compile-file "bool.sch")
(compile-file "char.sch")
(compile-file "string.sch")
(compile-file "normalization.sch")
(compile-file "bytevector.sch")
(compile-file "io.sch")
(compile-file "hashtable.sch")
;(compile-file "complex.sch")
(compile-file "ctak.sch")
(compile-file "dynamic-wind.sch")
;(compile-file "env.sch")
(compile-file "fact.sch")
(compile-file "fib.sch")
(compile-file "fixnums.sch")
(compile-file "number.sch")
(compile-file "pred.sch")
(compile-file "regression.sch")
(compile-file "wcm.sch")
(compile-file "record.sch")
(compile-file "condition.sch")

(load "test.fasl")			; Scaffolding

(load "bool.fasl")			; Booleans
(load "char.fasl")			; Characters
(load "string.fasl")			; Strings
(load "normalization.fasl")		; String normalization
(load "bytevector.fasl")                ; Bytevectors
(load "io.fasl")                        ; Input and output
(load "hashtable.fasl")                 ; Hashtables
;(load "complex.fasl")                   ; Complex numbers
(load "ctak.fasl")			; Call/cc test
(load "dynamic-wind.fasl")              ; Dynamic-wind test
;(load "env.fasl")                       ; Environments
(load "fact.fasl")                      ; Factorial, big and small
(load "fib.fasl")			; Fibonacci function
(load "fixnums.fasl")                   ; Fixnum-specific operations
(load "number.fasl")			; Numbers and arithmetic
(load "pred.fasl")                      ; Predicates
(load "regression.fasl")		; Past error cases
(load "wcm.fasl")                       ; Continuation marks
(load "record.fasl")                    ; Records
(load "condition.fasl")                 ; Conditions

(define (run-all-tests)
  (run-boolean-tests)
  (run-char-tests)
  (run-string-tests)

  ; Why run 350000+ tests you know will fail?

  (let ((string-rep (cdr (assq 'string-representation (system-features)))))

    (if (not (eq? string-rep 'flat1))
        (run-normalization-tests)))

  (run-bytevector-tests)
  (run-io-tests)
  (run-hashtable-tests)
  (run-predicate-tests)
  (run-number-tests)
  (run-fact-tests)
  (run-fib-tests)
  (run-ctak-tests)
  (run-dynamic-wind-tests)
  ;(run-env-tests)
  (run-regression-tests)
  (run-fixnum-tests)
  (run-wcm-tests)
  (run-record-tests)
  (run-condition-tests)
  )


; eof

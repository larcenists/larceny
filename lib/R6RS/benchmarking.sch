;;; This (larceny benchmarking) library defines several things
;;; we use for benchmarking:
;;;
;;;     (time <expression>)                                 ; syntax
;;;     (time-it <string> <thunk>)                          ; procedure
;;;     (run-benchmark <string> <int> <thunk> <predicate>)  ; procedure
;;;
;;; The main purpose of this library is to show how the primitives
;;; extension to R6RS-style import forms can be used to import any
;;; of Larceny's predefined R5RS procedures.

(library (larceny benchmarking)

  (export time time-it run-benchmark)
  (import (rnrs base)
          (primitives run-with-stats run-benchmark))

  (define-syntax time
    (syntax-rules ()
     ((_ expr)
      (run-with-stats (lambda () expr)))))

  (define (time-it name thunk)
    (run-benchmark name 1 thunk (lambda args #t))))

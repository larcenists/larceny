;;; This ERR5RS library exports Larceny's run-benchmark procedure.
;;;
;;; run-benchmark
;;; -------------
;;; The first argument is a string identifying the benchmark.
;;; The second argument is the number of times to run the benchmark.
;;; The third argument is a thunk that runs the benchmark.
;;; The fourth argument is a unary predicate that warns if the result
;;; returned by the benchmark is incorrect.
;;;
;;; Example:
;;; (run-benchmark "make-vector"
;;;                1
;;;                (lambda () (make-vector 1000000))
;;;                (lambda (v) (and (vector? v) (= (vector-length v) #e1e6))))

(library (local run-benchmark)

  (export run-benchmark)
  (import (primitives run-benchmark)))

(load "test.sch")
(load "number.sch")
(load "char.sch")

(define (alltests)
  (run-char-tests)
  (run-number-tests))

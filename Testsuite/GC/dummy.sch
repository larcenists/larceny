(define (dummy-benchmark parameter)
  (run-benchmark "dummy" (lambda () #t) 1000))

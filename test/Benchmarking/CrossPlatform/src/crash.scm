;;; CRASH - Test of crash condition.

(define (main . args)
  (run-benchmark
   "crash"
   1
   (lambda () (#f))
   (lambda (result)
     #t)))

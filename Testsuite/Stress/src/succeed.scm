;;; SUCCEED - Test of success condition.

(define (main . args)
  (run-benchmark
   "succeed"
   1
   (lambda () #f)
   (lambda (result)
     (equal? result #f))))

;;; FAIL - Test of failure condition.

(define (main . args)
  (run-benchmark
   "fail"
   1
   (lambda () #f)
   (lambda (result)
     (eq? result #t))))

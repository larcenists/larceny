;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(define (run)
  (let loop ((i 10000.) (n 0.))
    (if (FLOAT< i 0.)
      n
      (loop (FLOAT- i 1.) (FLOAT+ i n)))))
 
(define (main . args)
  (run-benchmark
    "sumfp"
    sumfp-iters
    (lambda () (run))
    (lambda (result) (equal? result 50005000.))))

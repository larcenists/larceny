;;; FPSUM - Compute sum of integers from 0 to 1e6 using floating point

(define (run)
  (let loop ((i 1e6) (n 0.))
    (if (FLOAT< i 0.)
      n
      (loop (FLOAT- i 1.) (FLOAT+ i n)))))
 
(define (main . args)
  (run-benchmark
    "fpsum"
    fpsum-iters
    (lambda () (run))
    (lambda (result) (equal? result 500000500000.))))

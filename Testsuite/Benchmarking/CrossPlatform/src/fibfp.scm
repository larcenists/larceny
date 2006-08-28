;;; FIBFP -- Computes fib(30) using floating point

(define (fibfp n)
  (if (FLOAT< n 2.)
    n
    (FLOAT+ (fibfp (FLOAT- n 1.))
            (fibfp (FLOAT- n 2.)))))

(define (main . args)
  (run-benchmark
    "fibfp"
    fibfp-iters
    (lambda () (fibfp 30.))
    (lambda (result) (equal? result 832040.))))

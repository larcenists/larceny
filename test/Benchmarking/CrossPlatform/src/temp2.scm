;;; FIBFP -- Computes fib(25) using floating point

(define (fibfp n)
  (write n)
  (newline)
  (if (< n 2.)
    n
    (+ (fibfp (- n 1.))
       (fibfp (- n 2.)))))

(define (main . args)
  (run-benchmark
    "fibfp"
    fibfp-iters
    (lambda () (fibfp 1.))
    (lambda (result) (equal? result 75025.))))

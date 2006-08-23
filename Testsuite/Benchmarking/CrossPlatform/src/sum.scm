;;; SUM -- Compute sum of integers from 0 to 10000

(define (run)
  (let loop ((i 10000) (n 0))
    (if (< i 0)
      n
      (loop (- i 1) (+ i n)))))
 
(define (main . args)
  (run-benchmark
    "sum"
    sum-iters
    (lambda () (run))
    (lambda (result) (equal? result 50005000))))

;;; FIBFP -- Computes fib(35) using floating point

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

(define (fibfp n)
  (if (< n 2.)
    n
    (+ (fibfp (- n 1.))
       (fibfp (- n 2.)))))

(define (main)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fibfp"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fibfp (hide count input)))
     (lambda (result) (= result output)))))


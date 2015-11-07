;;; SUMFP -- Compute sum of integers from 0 to n using floating point

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

(define (run n)
  (let loop ((i n) (sum 0.))
    (if (< i 0.)
        sum
        (loop (- i 1.) (+ i sum)))))
 
(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "sumfp"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (run (hide count input1)))
     (lambda (result) (equal? result output)))))

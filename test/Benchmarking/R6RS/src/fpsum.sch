;;; FPSUM - Compute sum of integers from 0 to 1e6 using floating point

(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

(define (run input)
  (let loop ((i input) (n 0.))
    (if (fl<? i 0.)
      n
      (loop (fl- i 1.) (fl+ i n)))))
 
(define (main)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fpsum"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (run (hide count input)))
     (lambda (result) (= result output)))))

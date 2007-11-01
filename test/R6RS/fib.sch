; This R6RS top-level program
; reads a non-negative integer n from (current-input-port)
; and then calculates and prints (fib n).

(import (rnrs base)
        (rnrs io simple))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define n (read))
(display "Computing ")
(write (list 'fib n))
(newline)
(write (fib n))
(newline)

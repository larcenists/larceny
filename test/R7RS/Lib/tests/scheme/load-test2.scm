(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

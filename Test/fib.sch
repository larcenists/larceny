(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-test)
  (display "(fib 1) = ") (display (fib 1)) (newline)
  (display "(fib 10) = ") (display (fib 10)) (newline)
  (display "(fib 20) = ") (display (fib 20)) (newline)
  (display "(fib 30) = ") (display (fib 30)) (newline)
  #t)


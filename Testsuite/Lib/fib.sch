; Testsuite/Lib/fib.sch
; Fibonacci test
;
; $Id$
;
; Tests non-tail calls; fixnum comparison; fixnum arithmetic.

(define (run-fib-tests)
  (display "Fib") (newline)
  (allof "fibonacci tests"
	 (test "(fib 1)" (fib 1) 1)
	 (test "(fib 10)" (fib 10) 55)
	 (test "(fib 20)" (fib 20) 6765)
	 (test "(fib 30)" (fib 30) 832040)))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; eof

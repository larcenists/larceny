; $Id$
; Test: fixnum <, -, *

(define (fact n)
  (if (< n 2) 1 (* n (fact (- n 1)))))

(define (fact-test)
  (test 'fact (fact 10) 3628800)
  #t)

; eof

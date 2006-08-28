; Test suite for SRFI-31
; 2004-01-01 / lth

(cond-expand (srfi-31))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? 120 ((rec (fact n)
		   (if (zero? n)
		       1
		       (* n (fact (- n 1)))))
		 5))
    (fail 'rec:1))

(or (equal? 100000000 ((rec f (lambda (x y) 
				(if (zero? x) 
				    y
				    (f (- x 1) (* y y)))))
		       3 10))
    (fail 'rec:2))

(writeln "Done.")

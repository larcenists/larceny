; Test suite for SRFI-11
; 2004-01-01 / lth

(cond-expand (srfi-11))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? '(1 2 (3 4))
	    (let-values (((a b . c) (values 1 2 3 4)))
	      (list a b c)))
    (fail 'let-values:1))
(or (equal? '(x y a b)
	    (let ((a 'a) (b 'b) (x 'x) (y 'y))
	      (let-values (((a b) (values x y))
			   ((x y) (values a b)))
                 (list a b x y))))
    (fail 'let-values:2))
(or (equal? '(1 2 3 4)
	    (let-values ((l (values 1 2 3 4)))
	      l))
    (fail 'let-values:3))

(or (equal? '(x y x y)
	    (let ((a 'a) (b 'b) (x 'x) (y 'y))
	      (let*-values (((a b) (values x y))
                            ((x y) (values a b)))
                (list a b x y))))
    (fail 'let*-values:1))

(writeln "Done.")

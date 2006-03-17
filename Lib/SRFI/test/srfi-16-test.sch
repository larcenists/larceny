; Test suite for SRFI-16
; 2004-01-01 / lth

(cond-expand (srfi-16))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define plus
  (case-lambda 
   (() 0)
   ((x) x)
   ((x y) (+ x y))
   ((x y z) (+ (+ x y) z))
   (args (apply + args))))

(or (equal? 0 (plus))
    (fail 'plus:1))
(or (equal? 1 (plus 1))
    (fail 'plus:2))
(or (equal? 6 (plus 1 2 3))
    (fail 'plus:3))
(or (equal? 55 (apply plus '(1 2 3 4 5 6 7 8 9 10)))
    (fail 'plus:4))

(or (not (call-with-current-continuation
	  (lambda (abort)
	    (parameterize ((error-handler
			    (lambda args
			      (abort #f))))
	      ((case-lambda 
		((a) a)
		((a b c) (* a b c)))
	       1 2)))))
    (fail 'argmatch))

(writeln "Done.")


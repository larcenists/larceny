; Test suite for SRFI-8
; 2004-01-01 / lth

(cond-expand (srfi-8))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
			 (k #f))))
         (eval '(receive (a b) (values 1 2)
		  (or (and (equal? a 1)
			   (equal? b 2))
		      (fail 'receive:1))))
	 (eval '(receive (a) 1
		  (or (equal? a 1)
		      (fail 'receive:2))))
	 (eval '(receive (a) (values 1)
		  (or (equal? a 1)
		      (fail 'receive:3))))
	 (eval '(receive () (values) #t))
	 (eval '(receive (a . rest) (values 1 2 3)
		  (or (and (equal? a 1)
			   (equal? rest '(2 3)))
		      (fail 'receive:4))))
	 (eval '(receive a (values 1 2 3)
		  (or (equal? a '(1 2 3))
		      (fail 'receive:5))))
	 #t)))
    (fail 'receive:0))			; Syntax or the 0-values form

(writeln "Done.")

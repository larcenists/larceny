(define (main)
  (write (call-with-current-continuation
	  (lambda (k)
	    (write "hi!")
	    (newline)
	    (k 3))))
  (newline))

	    
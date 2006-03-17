; Test suite for SRFI-23
; 2004-01-01 / lth

(cond-expand (srfi-23))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? '("This is an error" "with info" 37)
	    (call-with-current-continuation
	     (lambda (abort)
	       (parameterize ((error-handler
			       (lambda args
				 (abort (cdr args)))))
		(error "This is an error" "with info" 37)
		#f))))
    (fail 'error))

(writeln "Done.")
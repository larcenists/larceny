; Test/test.sch
; Larceny test suite -- test scaffolding
;
; $Id$

; Generic test procedure; compares answer to expected answer.
; It requires that the answer and the expected answer are represented in
; a way which exercises only trusted procedures.

(define (test id ans correct)
  (if (not (equal? ans correct))
      (begin (display "********** FAILURE *********") (newline)
	     (display "  ") (display id) (display " did not pass test.")
	     (newline)
	     (display "  Returned value = ") (display ans) (newline)
	     (display "  Correct value  = ") (display correct) (newline)
	     #f)
      #t))

; This really ought to be a macro that evaluates the tests in order and
; stops when a threshold of errors is reached.  It should also protect
; each executed test from aborting the test suite.

(define (allof test-name . l)
  (do ((l l (cdr l))
       (errors 0))
      ((null? l)
       (if (not (zero? errors))
	   (begin (newline)
		  (display errors)
		  (display " failure(s) detected in the group \"")
		  (display test-name)
		  (display "\".")
		  (newline)
		  (newline)))
       errors)
    (if (not (car l))
	(set! errors (+ errors 1)))))

(define (safely thunk token)
  (call-with-current-continuation
   (lambda (k)
     (call-with-error-handler
      (lambda args
	(k token))
      thunk))))

; Should really override the error handler (so that an error message
; is not printed).  FIXME.

(define (shouldfail id thunk)
  (let ((token (list 'token)))
    (let ((result (safely thunk token)))
      (if (not (eq? result token))
	  (begin (display id) (display " did not pass test.") (newline)
		 (display "It should have failed, but did not.") (newline)
		 #f)
	  #t))))

; eof

; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test scaffolding.

; Generic test procedure; compares answer to expected answer.
; It requires that the answer and the expected answer are represented in
; a way which exercises only trusted procedures.

(define (test id ans correct)
  (if (not (equal? ans correct))
      (begin (failure-message-failed id ans correct)
             (if (test-reporter) ((test-reporter) id ans correct))
	     #f)
      #t))

; A parameter whose value is a procedure that's called with id, answer, 
; and correct result if the test fails.

(define test-reporter
  (let ((reporter #f))
    (lambda rest
      (cond ((null? rest) reporter)
            ((null? (cdr rest)) 
             (set! reporter (car rest))
             reporter)
            (else
             (error "Test-reporter: too many arguments " rest))))))

(define (failure-message-failed id ans correct)
  (display "********** FAILURE *********") (newline)
  (display "  ") (display id) (display " did not pass test.")
  (newline)
  (display "  Correct value  = ") (display correct) (newline)
  (display "  Returned value = ") (display ans) (newline)
  )

(define (failure-message-succeeded id)
  (display "********** FAILURE *********") (newline)
  (display "  ") (display id) (display " did not pass test.")
  (newline)
  (display "It should have failed but did not.") (newline))

; Another generic test procedure, but ans and correct are assumed to
; be lists and are the same if they have the same contents.

(define (test-all-same id ans correct)

  (define *with-display* #f)

  (define (all-same? l1 l2)
    (let ((ok #t))
      (do ((l l1 (cdr l)))
	  ((null? l))
	(if (not (memq (car l) l2))
	    (begin (if *with-display*
		       (begin (display "Unmatched ")
			      (display (car l))
			      (display " in arg1")
			      (newline)))
		   (set! ok #f))))
      (do ((l l2 (cdr l)))
	  ((null? l))
	(if (not (memq (car l) l1))
	    (begin (if *with-display*
		       (begin (display "Unmatched ")
			      (display (car l))
			      (display " in arg2")
			      (newline)))
		   (set! ok #f))))
      ok))

  (if (not (all-same? ans correct))
      (begin (display "********** FAILURE *********") (newline)
	     (display "  ") (display id) (display " did not pass test.")
	     (newline)
	     (set! *with-display* #t)
	     (all-same? ans correct)
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

(define (allof-map test-name p l)
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
    (if (not (p (car l)))
	(set! errors (+ errors 1)))))

(define (safely thunk token)
  (call-with-current-continuation
   (lambda (k)
     (call-with-error-handler
      (lambda args
	(k token))
      thunk))))

(define (mustfail name p . args)
  (let ((eh #f))
    (if (call-with-current-continuation
	 (lambda (return)
	   (dynamic-wind
	    (lambda () 
	      (set! eh (error-handler))
	      (error-handler (lambda args (return #f))))
	    (lambda () 
	      (apply p args)
	      #t)
	    (lambda () 
	      (error-handler eh)))))
	(begin
	  (failure-message-succeeded name)
	  #f)
	#t)))

; eof

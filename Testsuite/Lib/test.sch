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
	     (display id) (display " did not pass test.") (newline)
	     (display "answer=") (display ans) (newline)
	     (display "correct=") (display correct) (newline)
	     #f)
      #t))

(define (allof . x)
  (let loop ((l x))
    (cond ((null? l) #t)
	  ((not (car l)) #f)
	  (else (loop (cdr l))))))

(define (allof/noncritical . x)
  #t)

(define (safely thunk token)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
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

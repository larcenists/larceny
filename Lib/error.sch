; Lib/error.sch
; Larceny library -- error system
;
; $Id: error.sch,v 1.3 1997/07/18 13:55:49 lth Exp $
;
; Documented behavior:
;  The default error handler prints all its arguments and then calls reset.
;  The default reset handler exits to the operating system.
;
;  An installed error handler should take a code and additional 
;  data elements. The code is either numeric (system exception: there will
;  be three additional arguments), null (ignore), or something else 
;  (user specific). The numeric codes are defined in Lib/ecodes.sch. 
;  An installed error handler may not return. 
;
;  An installed reset handler takes no arguments. It may not return.

($$trace "error")

(define (error . args)
  (apply (error-handler) '() args))

(define (call-with-error-handler handler thunk)
  (let ((old-handler (error-handler)))
    (dynamic-wind 
     (lambda () (error-handler handler))
     thunk
     (lambda () (error-handler old-handler)))))

(define (call-without-errors thunk . rest)
  (let ((fail (if (null? rest) #f (car rest))))
    (call-with-current-continuation
     (lambda (k)
       (call-with-error-handler (lambda (who . args) (k fail)) thunk)))))

(define (reset)
  ((reset-handler)))

(define (call-with-reset-handler handler thunk)
  (let ((old-handler (reset-handler)))
    (dynamic-wind 
     (lambda () (reset-handler handler))
     thunk
     (lambda () (reset-handler old-handler)))))

; The error handler is a procedure that takes a keyword as the first
; argument and then some additional arguments.  If the keyword is a number,
; then the error occured in compiled code or in a system subroutine.
; If the keyword is null, it is ignored.  Otherwise it is printed with the
; rest of the arguments.  Installed error handlers should obey this logic
; as far as reasonable.
;
; Error messages from system primitives come fully laid out with spaces,
; so the error handler does not need to add any.

(define *error-handler*
  (lambda (who . args)
    (if (number? who)
	(apply system-error-handler who args)
	(begin (display "Error: ")
	       (if (not (null? who))
		   (begin (display who)
			  (display ": ")))
	       (for-each display args)
	       (newline)
	       (reset)))))

(define (error-handler . args)
  (cond ((null? args)
	 *error-handler*)
	((and (null? (cdr args))
	      (procedure? (car args)))
	 (let ((old *error-handler*))
	   (set! *error-handler* (lambda a
				   (apply (car args) a)
				   (display "FATAL: Error handler returned.")
				   (newline)
				   (exit)))
	   old))
	(else
	 (display "Error: Error-handler: Invalid argument: ")
	 (display args)
	 (newline))))

(define *reset-handler*
  (lambda ()
    (exit)))

(define (reset-handler . args)
  (cond ((null? args)
	 *reset-handler*)
	((and (null? (cdr args))
	      (procedure? (car args)))
	 (let ((old *reset-handler*))
	   (set! *reset-handler*
		 (lambda ()
		   ((car args))
		   (display "FATAL: Reset handler returned.")
		   (newline)
		   (exit)))
	   old))
	(else
	 (display "Error: Reset-handler: Invalid argument: " args))))

; eof

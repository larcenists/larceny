; Lib/error.sch
; Larceny library -- error system
;
; $Id$
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

(define error-handler
  (system-parameter "error-handler" 
		    (lambda (who . args)
		      (if (number? who)
			  (begin (apply system-error-handler who args)
				 (display "FATAL: Error handler returned.")
				 (newline)
				 (exit))
			  (begin (display "Error: ")
				 (if (not (null? who))
				     (begin (display who)
					    (display ": ")))
				 (for-each display args)
				 (newline)
				 (reset))))))

(define reset-handler
  (system-parameter "reset-handler" 
		    (lambda ignored
		      (exit))))

; eof

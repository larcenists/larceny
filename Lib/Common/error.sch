; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- error system
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

; This takes an argument list as presented to ERROR-HANDLER (below)
; and optionally a port to print on (defaults to current output) and
; prints a human-readable error message based on the information in 
; the argument list.

(define (decode-error the-error . rest)
  (let ((who (car the-error))
        (port (if (null? rest) (current-output-port) (car rest))))
    (if (number? who)
        (decode-system-error who 
                             (cadr the-error) 
                             (caddr the-error)
                             (cadddr the-error)
                             port)
        (begin
          (display "Error: " port)
          (if (not (null? who))
              (begin (display who port)
                     (display ": " port)))
          (for-each (lambda (x) (display x port)) (cdr the-error))
          (newline port)))))

; The error handler is a procedure that takes a keyword as the first
; argument and then some additional arguments.  If the keyword is a number,
; then the error occured in compiled code or in a system subroutine.
; If the keyword is null, it is ignored.  Otherwise it is printed with the
; rest of the arguments.  Installed error handlers should obey this logic
; as far as reasonable.
;
; The error handler is called with interrupts in the state they were
; when the error was encountered.

(define error-handler
  (system-parameter "error-handler" 
		    (lambda args
                      (decode-error args)
                      (reset))))

(define reset-handler
  (system-parameter "reset-handler" 
		    (lambda ignored
		      (exit))))

; eof

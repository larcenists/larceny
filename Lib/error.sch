; -*- scheme -*-
;
; Larceny -- error system
;
; lth@cs.uoregon.edu / August 30, 1995
; $Id: error.sch,v 1.1 1997/02/03 20:07:13 lth Exp $
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
;
;  Error messages from system primitives come fully laid out with spaces,
;  so the error handler does not need to add any.

(define (error . args)
  (apply (error-handler) '() args))

; The error handler is a procedure which takes a keyword as the first
; argument and then some additional arguments. If the keyword is a number,
; then the error occured in compiled code or in a system subroutine.
; If the keyword is null, it is ignored. Otherwise it is printed with the
; rest of the arguments. Installed error handlers should obey this logic
; as far as reasonable.

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

(define reset
  (lambda ()
    (exit)))

(define (reset-handler . args)
  (cond ((null? args)
	 reset)
	((and (null? (cdr args))
	      (procedure? (car args)))
	 (let ((old reset))
	   (set! reset (lambda ()
			 ((car args))
			 (display "FATAL: Reset handler returned.")
			 (newline)
			 (exit)))
	   old))
	(else
	 (display "Error: Reset-handler: Invalid argument: " args))))

; The interrupt handler is a procedure of one argument which handles 
; various types of interrupts.

(define *interrupt-handler*
  (lambda (kind)
    (cond ((eq? kind 'timer)
	   (display "UNHANDLED TIMER INTERRUPT -- EXITING.")
	   (newline)
	   (exit))
	  (else
	   (display "UNKNOWN INTERRUPT TYPE: ")
	   (display kind)
	   (display ". EXITING.")
	   (newline)
	   (exit)))))

(define (interrupt-handler . args)
  (cond ((null? args)
	 *interrupt-handler*)
	((and (null? (cdr args))
	      (procedure? (car args)))
	 (let ((old *interrupt-handler*))
	   (set! *interrupt-handler* (car args))
	   old))
	(else
	 (error "Interrupt-handler: Invalid argument: " args))))

; eof

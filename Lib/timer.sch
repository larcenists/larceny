; Lib/timer.sch
; Larceny library -- timer interrupts
;
; $Id: timer.sch,v 1.1 1997/07/07 20:52:12 lth Exp lth $

(define (call-without-interrupts thunk)
  (let ((old (disable-interrupts)))
    (let ((r (thunk)))
      (if old
	  (enable-interrupts old))
      r)))

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
	 (error "Interrupt-handler: Invalid argument: " args)
	 #t)))

; eof

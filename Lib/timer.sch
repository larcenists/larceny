; Lib/timer.sch
; Larceny library -- timer interrupts
;
; $Id: timer.sch,v 1.1.1.1 1998/11/19 21:52:13 lth Exp $

($$trace "timer")

; This is well-behaved but somewhat expensive w.r.t. allocation.

(define (call-without-interrupts thunk)
  (let ((old #f))
    (dynamic-wind 
     (lambda () (set! old (disable-interrupts)))
     thunk
     (lambda () (if old (enable-interrupts old))))))

; This is not well-behaved but less expensive; I've left it here for 
; reference.

(define (old-call-without-interrupts thunk)
  (let ((old (disable-interrupts)))
    (let ((r (thunk)))
      (if old
	  (enable-interrupts old))
      r)))

; The system interrupt handler is a procedure of one argument that handles 
; various types of interrupts.
;
; Timer interrupts are signalled with timer interrupts turned off; all
; other interrupts with the timer interrupt state unchanged.

(define *interrupt-handler*
  (lambda (kind)
    (disable-interrupts)
    (cond ((eq? kind 'timer)
	   ($$debugmsg "Unhandled timer interrupt -- exiting.")
	   (exit))
	  ((eq? kind 'keyboard)
	   ($$debugmsg "Unhandled keyboard interrupt -- exiting.")
	   (exit))
	  (else
	   ($$debugmsg "Unhandled interrupt of unknown type -- exiting.")
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

; A timeslice of 50,000 is a compromise between overhead and response time.
; On atlas.ccs.neu.edu (a SPARC 10 (?)), 50,000 is really too much for
; longer sections of straight-line code yet too little for very branch-
; or call-intensive programs.  See Util/timeslice.sch for details.
;
; The _right_ solution is probably the following:
;  * the scheduler uses priorities
;  * higher-priority jobs get longer time slices
;  * interactive input tasks (mouse, keyboard) get to interrupt
;    whatever task is running.
;  * run-benchmark gets to ask for a very long time slice.
;
; Since the scheduler will run at a different level from this low-level
; RTS code, the 50,000 is an OK compromise for this level.
;
; (Alternatively, the timer has some arbitrary value but that value is
; chunked into pieces of 10,000, say, maintained by millicode.  Each 10K
; decrements a quick trip is made into millicode to check for interrupts
; and get the next timer chunk.)

(define *max-ticks* 536870911)            ; (- (expt 2 29) 1)

(define *standard-timeslice* 50000)       ; Empirical.

(define (standard-timeslice . rest)
  (cond ((null? rest) *standard-timeslice*)
	((and (null? (cdr rest))
	      (fixnum? (car rest))
	      (> (car rest) 0))
	 (set! *standard-timeslice* (car rest))
	 *standard-timeslice*)
	(else
	 (error "standard-timeslice: "
		rest " is not a valid argument list."))))


; eof

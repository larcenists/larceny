; ASSERT macro
; 2004-01-18 / lth
;
; ASSERT checks that a condition holds and invokes the debugger
; if it does not.
;
; There is some unreliable cruft here.  Most of the cruft comes from
; trying to make it work reliably with the various tasking packages;
; it is probably not entirely reliable.  A WITHOUT-TASKING primitive
; might work better in practice.
;
; Usage note: Try to make sure it gets loaded early so that the saved
; factories are valid.

(require 'debugger)

(define *assert-console-output-port-factory* (console-output-port-factory))
(define *assert-console-input-port-factory* (console-input-port-factory))

(define-syntax assert
  (syntax-rules ()
    ((assert e)
     (if (not e)
	 (begin (failed-assertion 'e) #t)))))  ; non-tail call

(define (failed-assertion e)
  (let ((disabled? (disable-interrupts)))
    (parameterize ((console-output-port-factory 
		    *assert-console-output-port-factory*)
		   (console-input-port-factory 
		    *assert-console-input-port-factory*)
		   (standard-timeslice
		    (most-positive-fixnum))
		   (timer-interrupt-handler
		    (lambda ()
		      (enable-interrupts (standard-timeslice)))))
      (enable-interrupts (standard-timeslice))
      (newline)
      (display "FAILED ASSERTION: ")
      (display e)
      (dbg)
      (if disabled?
	  (disable-interrupts)))))



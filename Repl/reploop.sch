; Repl/reploop.sch
; Larceny -- read-eval-print loop and error handler.
;
; $Id: reploop.sch,v 1.5 1997/09/17 15:14:58 lth Exp $

; User-accessible parameter procedures:
;
;  repl-prompt
;    Procedure parameter that takes the nesting level and prints a prompt.
;
;  repl-evaluator 
;    Procedure parameter that takes an expression and an environment and
;    evaluates the expression, returning all its return values.
;
;  repl-printer
;    Procedure parameter that takes a value and prints it on the current
;    output port.


($$trace "reploop")

(define *init-file-name* ".larceny")  ; Unix-ism.
(define *file-arguments* #f)          ; #t if file arguments were loaded
(define *reset-continuation* #f)      ; current longjump point
(define *saved-continuation* #f)      ; last saved error continuation
(define *repl-level* 0)               ; nesting level

; Entry point in a bootstrap heap.

(define (main argv)
  ($$trace "In main")
  (init-toplevel-environment)
  (interaction-environment (larceny-environment))
  (setup-error-handlers)
  (repl-evaluator eval)
  (rep-loop-bootstrap argv))


; Entry point in a saved interactive heap.

(define (rep-loop-bootstrap argv)
  ($$trace "In rep-loop-bootstrap")
  (set! *file-arguments* #f)
  (set! *reset-continuation* #f)
  (set! *repl-level* 0)
  (command-line-arguments argv)
  (record-current-console-io)
  (standard-timeslice (- (expt 2 29) 1)) ; Largest possible slice.
  (setup-interrupts)
  (failsafe-load-init-file)
  (failsafe-load-file-arguments)
  (if (herald)
      (begin (display (herald))
	     (newline)))
  (newline)
  (repl)
  (exit 0))

(define (repl)

  (define done #f)

  (define (repl-display result)
    (call-with-error-handler
     (lambda (who . args)
       (reestablish-console-io)
       (format #t "Error: Bogus display procedure; reverting to default.")
       (repl-printer default-repl-printer)
       (reset))
     (lambda ()
       ((repl-printer) result))))

  (define (repl)
    ((repl-prompt) *repl-level*)
    (flush-output-port)
    (let ((expr (read)))
      (if (not (eof-object? expr))
	  (let ((results (call-with-values 
			  (lambda ()
			    ((repl-evaluator) expr (interaction-environment)))
			  (lambda values
			    values))))
	    (reestablish-console-io)
	    (cond ((null? results)
		   (display "; No values") (newline))
		  ((null? (cdr results))
		   (repl-display (car results)))
		  (else
		   (display (string-append
			     "; "
			     (number->string (length results))
			     " values"))
		   (newline)
		   (for-each (lambda (x)
			       (repl-display x))
			     results)))
	    (repl))
	  (begin
	    (reestablish-console-io)
	    (newline)
	    (set! done #t)
	    (*reset-continuation* #f)))))

  ; Setup the error continuation.  We wrap it in a dynamic-wind to allow
  ; REPLs to be nested.

  ($$trace "In repl (toplevel)")
  (let ((x *reset-continuation*))
    (let ((k #f))
      (call-with-current-continuation
       (lambda (c) (set! k c)))
      (if (not done)
	  (dynamic-wind
	   (lambda ()
	     (set! *reset-continuation* k)
	     (set! *repl-level* (+ *repl-level* 1)))
	   (lambda ()
	     (repl))
	   (lambda ()
	     (set! *repl-level* (- *repl-level* 1))
	     (set! *reset-continuation* x))))))
  (unspecified))

; The default printer uses "write" and does not print unspecified values.

(define default-repl-printer
  (lambda (result)
    (if (not (eq? result (unspecified)))
	(begin (write result)
	       (newline)))))

(define repl-prompt
  (system-parameter "repl-prompt"
		    (lambda (level)
		      (do ((i 0 (+ i 1)))
			  ((= i level))
			(write-char #\>))
		      (write-char #\space))))

(define repl-evaluator
  (system-parameter "repl-evaluator" #f))

(define repl-printer
  (system-parameter "repl-printer" default-repl-printer))

(define herald
  (system-parameter "herald" #f))

; User-available procedures.

(define (error-continuation)
  *saved-continuation*)

(define (dump-interactive-heap filename)
  (dump-heap filename rep-loop-bootstrap))


; Console i/o handling -- after an error, console i/o must be reset.

(define *conin* #f)                   ; console input
(define *conout* #f)                  ; console output

(define (record-current-console-io)
  (set! *conin* (current-input-port))
  (set! *conout* (current-output-port)))

(define (reestablish-console-io)
  (if (or (not (io/open-port? *conin*))
	  (io/port-error-condition? *conin*)
	  (io/port-at-eof? *conin*))
      (set! *conin* (console-io/open-input-console)))
  (if (or (not (io/open-port? *conout*))
	  (io/port-error-condition? *conout*))
      (set! *conout* (console-io/open-output-console)))
  (current-input-port *conin*)
  (current-output-port *conout*))


; Error handling.
;
; We set up customized error and reset handlers so that we can
; capture errors and save a debugging continuation for a backtrace.
;
; The new error handler captures the current continuation _structure_,
; saves it in the global *saved-continuation*, and then invokes the
; previously defined (default) error handler, which in normal cases will
; print an error message and then do a (reset).
;
; The new reset handler does a longjump to the currently saved
; reset continuation, which will be set up by the read-eval-print loop.

(define (setup-error-handlers)
  (reset-handler new-reset-handler)
  (error-handler (new-error-handler)))

(define (new-error-handler)
  (let ((old (error-handler)))
    (lambda args
      (reestablish-console-io)          ; So that we can print the message.
      (let ((k (current-continuation-structure)))
	 (set! *saved-continuation* k)
	 (apply old args)))))

(define (new-reset-handler)
  (if *reset-continuation*
      (*reset-continuation* #f)
      (exit)))


; Interrupt handling.

(define (setup-interrupts)
  (let ((old-handler (interrupt-handler)))
    (interrupt-handler
     (lambda (type)
       (cond ((eq? type 'timer)
	      (enable-interrupts (standard-timeslice)))
	     ((eq? type 'keyboard)
	      (enable-interrupts (standard-timeslice))
	      (reestablish-console-io)
	      (display "Keyboard interrupt.")
	      (newline)
	      (let ((k (current-continuation-structure)))
		(set! *saved-continuation* k)
		(reset)))
	     (else
	      (old-handler type)))))
    (enable-interrupts (standard-timeslice))))


; Init file loading.

(define (failsafe-load-init-file)
  (cond ((file-exists? *init-file-name*)
	 (failsafe-load-file *init-file-name*))
	((getenv "HOME") 
	 => 
	 (lambda (home)
	   (let ((fn (string-append home "/" *init-file-name*)))
	     (if (file-exists? fn)
		 (failsafe-load-file fn)))))))

(define (failsafe-load-file-arguments)
  (let ((argv (command-line-arguments)))
    (do ((i 0 (+ i 1)))
	((= i (vector-length argv)))
      (if (file-exists? (vector-ref argv i))
	  (begin (set! *file-arguments* #t)
		 (failsafe-load-file (vector-ref argv i)))))))

(define (failsafe-load-file filename)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
	(format #t "Error while loading ~a.~%" filename)
	(k #f))
      (lambda ()
	(load filename))))))

; eof

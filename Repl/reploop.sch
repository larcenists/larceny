; Repl/reploop.sch
; Larceny -- read-eval-print loop and error handler.
;
; $Id: reploop.sch,v 1.5 1997/09/17 15:14:58 lth Exp lth $

($$trace "reploop")

(define *init-file-name* ".larceny")  ; Unix-ism.
(define *file-arguments* #f)          ; #t if file arguments were loaded
(define *reset-continuation* #f)      ; current longjump point
(define *saved-continuation* #f)      ; last saved error continuation

;;; Entry point in a bootstrap heap.

(define (main argv)
  (init-toplevel-environment)
  (interaction-environment (larceny-environment))
  (setup-error-handlers)
  (rep-loop-bootstrap argv))


;;; Entry point in a saved interactive heap.

(define (rep-loop-bootstrap argv)
  (command-line-arguments argv)
  (record-current-console-io)
  (standard-timeslice (- (expt 2 29) 1))    ; Largest possible slice.
  (setup-interrupts)
  (failsafe-load-init-file)
  (failsafe-load-file-arguments)
  (rep-loop))


;;; Read-eval-print loop.

(define (rep-loop)

  (define (loop)
    (display "> ")
    (flush-output-port)
    (let ((expr   (read)))
      (if (not (eof-object? expr))
	  (let ((result (eval expr)))
	    (reestablish-console-io)
	    (repl-display result)
	    (loop))
	  (begin (newline)
		 (exit)))))

  ; Setup the error continuation

  (call-with-current-continuation
   (lambda (k)
     (set! *reset-continuation* k)))
  (newline)
  (loop))

;;; Read-eval-print loop printer.  The print procedure defaults to a simple
;;; wrapper around 'display', but the procedure is installable, so that
;;; higher-level code can install e.g. a pretty-printer as the default
;;; print procedure.

; Default

(define default-repl-display-proc
  (lambda (result)
    (if (not (eq? result (unspecified)))
	(begin (display result)
	       (newline)))))

; Install hook

(define repl-display-procedure
  (let ((proc default-repl-display-proc))
    (lambda rest
      (cond ((null? rest) proc)
	    ((null? (cdr rest))
	     (let ((old proc))
	       (if (not (car rest))
		   (set! proc default-repl-display-proc)
		   (set! proc (car rest)))
	       old))
	    (else
	     (error "repl-display-proc: too many arguments.")
	     #t)))))

; Called by repl.

(define (repl-display result)
  (call-with-error-handler
   (lambda (who . args)
     (reestablish-console-io)
     (format #t "Error: Bogus display procedure; reverting to default.")
     (repl-display-procedure default-repl-display-proc)
     (reset))
   (lambda ()
     ((repl-display-procedure) result))))


;;; Console i/o handling -- after an error, console i/o must be reset.

(define *conin* #f)                   ; console input
(define *conout* #f)                  ; console output

(define (record-current-console-io)
  (set! *conin* (current-input-port))
  (set! *conout* (current-output-port)))

(define (reestablish-console-io)
  (if (or (not (io/open-port? *conin*))
	  (io/port-error-condition? *conin*))
      (set! *conin* (console-io/open-input-console)))
  (if (or (not (io/open-port? *conout*))
	  (io/port-error-condition? *conout*))
      (set! *conout* (console-io/open-output-console)))
  (current-input-port *conin*)
  (current-output-port *conout*))


;;; Error handling.
;;;
;;; We set up customized error and reset handlers so that we can
;;; capture errors and save a debugging continuation for a backtrace.
;;;
;;; The new error handler captures the current continuation _structure_,
;;; saves it in the global *saved-continuation*, and then invokes the
;;; previously defined (default) error handler, which in normal cases will
;;; print an error message and then do a (reset).
;;;
;;; The new reset handler does a longjump to the currently saved
;;; reset continuation, which will be set up by the read-eval-print loop.

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


;;; Interrupt handling.

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
	      (let ((k (current-continuation-structure)))
		(set! *saved-continuation* k)
		(reset)))
	     (else
	      (old-handler type)))))
    (enable-interrupts (standard-timeslice))))


;;; User-available procedures.

(define (error-continuation)
  *saved-continuation*)

(define (dump-interactive-heap filename)
  (dump-heap filename rep-loop-bootstrap))


;;; Init file loading.

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

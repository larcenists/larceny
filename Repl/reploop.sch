; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Read-eval-print loop and basic interaction logic.

($$trace "reploop")

; Entry point in a bootstrap heap.

(define (main argv)
  ($$trace "In main")
  (init-toplevel-environment)
  (setup-interrupt-and-error-handlers)
  (evaluator interpret)
  (interactive-entry-point argv))


; Entry point in a saved interactive heap.

(define (interactive-entry-point argv)
  ($$trace "In interactive-entry-point")
  (repl-reset-continuation #f)
  (command-line-arguments argv)
  (standard-timeslice (most-positive-fixnum))
  (enable-interrupts (standard-timeslice))
  (failsafe-load-init-file)
  (failsafe-load-file-arguments)
  (if (herald)
      (writeln (herald)))
  (writeln)
  ($$trace "Entering REPL")
  (repl-level 0)
  (repl)
  (exit 0))

(define (repl)

  (define done #f)

  (define (repl-display result)
    (call-with-error-handler
     (lambda (who . args)
       (writeln "Error during printing; reverting to default printer.")
       (repl-printer default-repl-printer)
       (reset))
     (lambda ()
       ((repl-printer) result (console-output-port)))))

  (define (repl)
    ((repl-prompt) (repl-level) (console-output-port))
    (flush-output-port (console-output-port))
    (let ((expr (read (console-input-port))))
      (if (not (eof-object? expr))
	  (let ((results (call-with-values 
			  (lambda ()
                            ((repl-evaluator) expr (interaction-environment)))
			  (lambda values
			    values))))
            (cond ((null? results)
                   (writeln "; No values"))
                  ((null? (cdr results))
                   (repl-display (car results)))
                  (else
                   (writeln "; " (length results) " values")
                   (for-each (lambda (x)
                               (repl-display x))
                             results)))
	    (repl))
	  (begin
            (writeln)
	    (set! done #t)))))

  ; Setup the reset continuation and repl level.  REPLs can be nested,
  ; and they can be concurrent, though they share the underlying console
  ; architecture (There Can Be Only One).

  (let ((repl-continuation #f))
    (call-with-current-continuation
     (lambda (k) 
       (set! repl-continuation k)))
    (if (not done)
        ; FIXME: use PARAMETERIZE
        (let ((old-reset (repl-reset-continuation))
              (old-repl-level (repl-level)))
	  (dynamic-wind
	   (lambda ()
	     (repl-reset-continuation repl-continuation)
	     (repl-level (+ (repl-level) 1)))
	   (lambda ()
             (enable-interrupts (standard-timeslice))
	     (repl))
	   (lambda ()
	     (repl-level old-repl-level)
	     (repl-reset-continuation old-reset))))))
  (unspecified))


; Default REPL printer.
; The default printer uses "write" and does not print unspecified values.

(define default-repl-printer
  (lambda (result port)
    (if (not (eq? result (unspecified)))
        (begin (write result port)
               (newline port)))))


; REPL parameters (some public, some not)

(define repl-level
  (system-parameter "repl-level" 0))

(define repl-reset-continuation
  (system-parameter "reset-continuation" #f))

(define repl-prompt
  (system-parameter "repl-prompt"
		    (lambda (level port)
                      (display (make-string level #\>) port)
                      (display " " port))))

(define repl-evaluator
  (system-parameter "repl-evaluator" eval))

(define repl-printer
  (system-parameter "repl-printer" default-repl-printer))

(define herald
  (system-parameter "herald" #f))


; Other public procedures.

(define (dump-interactive-heap filename)
  (dump-heap filename interactive-entry-point))


; Error/reset/interrupt handling.

(define (setup-interrupt-and-error-handlers)
  (reset-handler 
   (lambda ()
     (if (repl-reset-continuation)
         ((repl-reset-continuation) #f)
         (exit))))

  (error-handler 
   (let ((old-handler (error-handler)))
     (lambda args
       (with-output-to-port (console-output-port)
         (lambda ()
           (apply old-handler args))))))

  (timer-interrupt-handler
   (lambda ()
     (enable-interrupts (standard-timeslice))))

  (keyboard-interrupt-handler
   (lambda ()
     (writeln "Keyboard interrupt.")
     (reset))))


; Init file loading.

(define (failsafe-load-init-file)
  (let ((fn (osdep/find-init-file)))
    (if fn (failsafe-load-file fn))))

(define (failsafe-load-file-arguments)
  (let ((argv (command-line-arguments)))
    (do ((i 0 (+ i 1)))
	((= i (vector-length argv)))
      (if (file-exists? (vector-ref argv i))
          (failsafe-load-file (vector-ref argv i))))))

(define (failsafe-load-file filename)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
        (writeln "Error while loading " filename)
	(k #f))
      (lambda ()
	(load filename))))))

(define (writeln . xs)
  (let ((p (console-output-port)))
    (for-each (lambda (x) (display x p)) xs)
    (newline p)))

; eof

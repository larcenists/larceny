; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; REPL and REPL parameters.

($$trace "reploop")

(define (start-repl)
  ($$trace "Entering REPL")
  (repl-level 0)
  (repl))

(define (repl)

  (define done #f)

  (define repl-reset-continuation #f)

  (define (repl-display result)
    (call-with-error-handler
     (lambda (who . args)
       (display "Error during printing; reverting to the ur-printer.")
       (newline)
       (repl-printer ur-printer)
       (reset))
     (lambda ()
       ((repl-printer) result (current-output-port)))))

  (define (repl)
    ((repl-prompt) (repl-level) (console-output-port))
    (flush-output-port (console-output-port))
    (let ((expr (read (console-input-port))))
      (if (not (eof-object? expr))
	  (let-values ((results
                        (with-input-from-port (console-input-port)
                          (lambda ()
                            (with-output-to-port (console-output-port)
                              (lambda ()
                                ((repl-evaluator)
                                 expr
                                 (interaction-environment))))))))
            (with-output-to-port (console-output-port)
              (lambda ()
                (cond ((null? results)
                       (display "; No values")
                       (newline))
                      ((null? (cdr results))
                       (repl-display (car results)))
                      (else
                       (display "; ")
                       (display (length results))
                       (display " values")
                       (newline)
                       (for-each repl-display results)))))
	    (repl))
	  (begin
            (newline (console-output-port))
	    (set! done #t)))))

  (parameterize ((reset-handler
                  (lambda ()
                    (repl-reset-continuation #f)))
                 (quit-handler
                  (lambda ()
                    (set! done #t)
                    (repl-reset-continuation #f))))
    (let ((repl-continuation #f))
      (call-with-current-continuation
       (lambda (k)
         (set! repl-reset-continuation k)))
      (if (not done)
          (parameterize ((repl-level (+ (repl-level) 1)))
            (enable-interrupts (standard-timeslice))
            (repl)))))
  (unspecified))


; The ur-printer uses "write" and does not print unspecified values.

(define ur-printer
  (lambda (result port)
    (if (not (eq? result (unspecified)))
        (begin (write result port)
               (newline port)))))


; REPL parameters (some public, some not)

(define repl-level
  (make-parameter "repl-level" 0))

(define repl-prompt
  (make-parameter "repl-prompt"
		    (lambda (level port)
                      (newline port)
                      (display (make-string level #\>) port)
                      (display " " port))))

(define repl-evaluator
  (make-parameter "repl-evaluator" eval))

(define repl-printer
  (make-parameter "repl-printer" ur-printer))


; eof

; Copyright 1998, 1999 Lars T Hansen.
;
; $Id$
;
; Read-eval-print loop for nonblocking-aware task system.
;
; It's the same as the default REPL except in the way resets are
; handled, and that it's sensitive to redefinitions of
; CONSOLE-INPUT-PORT and CONSOLE-OUTPUT-PORT.

(define repl-level
  (let ((level 1))
    (lambda args
      (cond ((null? args) level)
            ((null? (cdr args)) (set! level (car args)) level)
            (else ???)))))

(define (repl)

  (define done #f)
  (define repl-reset-continuation #f)

  (define (writeln . xs)
    (let ((p (console-output-port)))
      (for-each (lambda (x) (display x p)) xs)
      (newline p)))

  (define default-repl-printer
    (lambda (result port)
      (if (not (eq? result (unspecified)))
          (begin (write result port)
                 (newline port)))))

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

  (parameterize ((reset-handler
                  (lambda ()
                    (repl-reset-continuation #f))))
    (let ((repl-continuation #f))
      (call-with-current-continuation
       (lambda (k) 
         (set! repl-reset-continuation k)))
      (if (not done)
          (parameterize ((repl-level (+ (repl-level) 1)))
            (repl)))))
  (unspecified))

; eof

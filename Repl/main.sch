; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Bootstrap code.

($$trace "main")

; Entry point in a bootstrap heap.

(define (main argv)
  ($$trace "In main")
  (init-toplevel-environment)
  (setup-interrupt-and-error-handlers)
  (evaluator interpret)
  (issue-warnings #t)              ; Warnings are off during bootstrapping
  (interactive-entry-point argv))


; Entry point in a saved interactive heap.

(define (interactive-entry-point argv)
  (eval
   '(define-syntax .javadot
      (transformer
       (lambda (exp rename compare)
         (let ((exp (cadr exp)))
           (display "inside .javadot: ") (write exp) (newline)
           (list (rename 'dotnet-mumble) (javadot-symbol->symbol exp)))))))

  ($$trace "In interactive-entry-point")
  (command-line-arguments argv)
  (standard-timeslice (most-positive-fixnum))
  (enable-interrupts (standard-timeslice))
  (failsafe-load-init-file)
  (failsafe-load-file-arguments)
  (if (herald)
      (writeln (herald)))
  (writeln)
  (start-repl)
  (exit 0))


; Public procedures.

(define herald
  (make-parameter "herald" #f))

(define (dump-interactive-heap filename)
  (dump-heap filename interactive-entry-point))


; Error/reset/interrupt handling.

(define (setup-interrupt-and-error-handlers)
  (error-handler 
   (let ((old-handler (error-handler)))
     (lambda error
       (parameterize ((error-handler
                       (lambda error
                         ($$debugmsg "Error handler signalled an error")
                         (cond ((string? (car error))
                                ($$debugmsg (car error)))
                               ((and (null? (car error))
                                     (not (null? (cdr error)))
                                     (string? (cadr error)))
                                ($$debugmsg (cadr error)))
                               ((number? (car error))
                                ($$debugmsg (number->string (car error)))))
                         (exit))))
         (with-output-to-port (console-output-port)
           (lambda ()
             (apply old-handler error)))))))

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

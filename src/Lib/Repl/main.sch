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
  (for-each eval *interactive-eval-list*)
  ($$trace "In interactive-entry-point")
  (command-line-arguments argv)
  (standard-timeslice (most-positive-fixnum))
  (enable-interrupts (standard-timeslice))
  (failsafe-load-init-files)
  (failsafe-process-arguments)
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

(define (failsafe-load-init-files)
  (map failsafe-load-file (osdep/find-init-files)))

(define (failsafe-process-arguments)
  (let ((argv (command-line-arguments)))
    (let loop ((i 0))
      (cond 
       ((>= i (vector-length argv)) #t)
       (else
        (let ((arg (vector-ref argv i)))
          (cond 
           ((or (string=? arg "-e")
                (string=? arg "--eval"))
            (failsafe-eval-thunk 
             (lambda () 
               (retract-eof 
                (read (open-input-string (vector-ref argv (+ i 1))))))
             (list "Error parsing argument " (+ i 1)))
            (loop (+ i 2)))
           (else
            (if (file-exists? arg)
                (failsafe-load-file arg))
            (loop (+ i 1))))))))))

;; retract-eof : Any -> Any
(define (retract-eof x)
  (cond ((eof-object? x) (error "Encountered EOF"))
        (else x)))
  
;; failsafe-eval-thunk : (-> S-exp) [Listof Any] -> Any
(define (failsafe-eval-thunk arg-thunk error-mesgs)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler 
      (lambda ()
        (apply writeln error-mesgs)
        (k #f))
      (lambda ()
        (failsafe-eval (arg-thunk)))))))

;; failsafe-eval-string : S-exp -> Any
(define (failsafe-eval arg-exp)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
        (writeln "Error while evaluating " arg-exp)
        (k #f))
      (lambda ()
        (eval arg-exp))))))

;; failsafe-load-file : String -> Any
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

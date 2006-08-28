; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- the exit procedure, and init/exit hooks.

($$trace "exit")

; Exit: leave the system.

(define (exit . rest)
  (run-exit-procedures)
  (cond ((null? rest)
	 (sys$exit 0))
	((and (null? (cdr rest)) (fixnum? (car rest)))
	 (sys$exit (car rest)))
	((null? (cdr rest))
	 (error "exit: bad argument: " (car rest))
	 #t)
	(else
	 (error "exit: too many arguments.")
	 #t)))

(define *init-procedures* '())
(define *exit-procedures* '())


; Init procedures must be run in the order they were added.

(define (add-init-procedure! thunk)
  (set! *init-procedures* (append! *init-procedures* (list thunk)))
  (unspecified))

(define (run-init-procedures)
  (for-each (lambda (x) (x)) *init-procedures*))


; Exit procedures are run in reverse order.

(define (add-exit-procedure! thunk)
  (set! *exit-procedures* (cons thunk *exit-procedures*))
  (unspecified))

(define (run-exit-procedures)
  (for-each (lambda (x) (x)) *exit-procedures*))


; Quit: do something system-defined.  The REPL overrides this to allow
; QUIT at a nested REPL to return from the REPL.

(define (quit)
  ((quit-handler)))

(define quit-handler
  (make-parameter "quit-handler" exit procedure?))

; eof

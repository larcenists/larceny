; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Simplistic trace and breakpoint facility -- only for top-level names,
; and traces names rather than values.
;
; (trace name)             Trace 'name' on entry and exit
; (trace-entry name)       Trace 'name' on entry
; (trace-exit name)        Trace 'name' on exit
; (untrace name)           Disable tracing for 'name'
; (untrace)                Disable tracing for all traced names
;
; (breakpt name)           Break on entry to 'name'
; (unbreak name)           Disable breaking for 'name'
; (unbreak)                Disable all breakpoints
;
; FIXME: 'Breakpt' is called 'breakpt' because 'break' is currently a 
; primop; that should be fixed.  The primop could be renamed to
; 'break-here' or somesuch (or 'debugvsm', even).
;
; FIXME: Doesn't preserve proper tail recursion when tracing.

; Requires
;  Debugger/debug.sch         [ for debug/print ]


(define *traced* '())			; list of (trace-proc proc name)
(define *broken* '())			; list of (break-proc proc name)
(define *trace-level* 0)		; indent level

; Install an evaluator that resets the trace level each time a top-level 
; expression is evaluated. (This isn't ideal but it's OK.)

(let ((old-evaluator (repl-evaluator)))
  (repl-evaluator (lambda (expr env)
		    (set! *trace-level* 0)
		    (old-evaluator expr env))))

(define (trace . rest)
  (if (null? rest)
      (map caddr *traced*)
      (trace-name (car rest) 'both)))

(define (trace-entry toplevel-name)
  (trace-name toplevel-name 'entry))

(define (trace-exit toplevel-name)
  (trace-name toplevel-name 'exit))

(define (trace-name toplevel-name how)
  (let* ((e (interaction-environment))
	 (x (environment-get e toplevel-name)))
    (cond ((not (procedure? x))
	   (error toplevel-name " does not hold a procedure."))
	  ((assv x *traced*)
	   (error toplevel-name " is already traced."))
	  (else
	   (let ((t (case how
		      ((both)
		       (lambda args
			 (debug/trace-enter toplevel-name args)
			 (set! *trace-level* (+ *trace-level* 2))
			 (call-with-values
			  (lambda () (apply x args))
			  (lambda r
			    (set! *trace-level* (- *trace-level* 2))
			    (apply debug/trace-exit toplevel-name r)
			    (apply values r)))))
		      ((entry)
		       (lambda args
			 (debug/trace-enter toplevel-name args)
			 (apply x args)))
		      ((exit)
		       (lambda args
			 (call-with-values
			  (lambda () (apply x args))
			  (lambda r
			    (apply debug/trace-exit toplevel-name r)
			    (apply values r)))))
		      (else ???))))
	     (environment-set! e toplevel-name t)
	     (set! *traced* (cons (list t x toplevel-name) *traced*))
	     (unspecified))))))

(define (untrace . rest)
  (if (null? rest)
      (begin (for-each (lambda (n) (untrace-name n #f)) (map caddr *traced*))
	     (set! *traced* '()))
      (untrace-name (car rest)))
  (unspecified))

(define (untrace-name toplevel-name . rest)
  (let* ((silent? (not (null? rest)))
	 (e (interaction-environment))
	 (x (environment-get e toplevel-name)))
    (let loop ((t *traced*) (prev #f))
      (cond ((null? t)
	     (if (not silent?)
		 (error toplevel-name " is not traced.")))
	    ((eqv? (caar t) x)
	     (environment-set! e toplevel-name (cadar t))
	     (if prev
		 (set-cdr! prev (cdr t))
		 (set! *traced* (cdr t)))
	     (unspecified))
	    (else
	     (loop (cdr t) t))))))

(define (breakpt toplevel-name)
  (let* ((e (interaction-environment))
	 (x (environment-get e toplevel-name)))
    (cond ((not (procedure? x))
	   (error toplevel-name " does not hold a procedure."))
	  ((assv x *broken*)
	   (error toplevel-name " is already broken."))
	  (else
	   (let ((t (lambda args
		      (debug/breakpoint toplevel-name args)
		      (apply x args))))
	     (environment-set! e toplevel-name t)
	     (set! *broken* (cons (list t x toplevel-name) *broken*))
	     (unspecified))))))

(define (unbreak . rest)
  (if (null? rest)
      (for-each unbreak-name (map caddr *broken*))
      (unbreak-name (car rest)))
  (unspecified))

(define (unbreak-name toplevel-name)
  (let* ((e (interaction-environment))
	 (x (environment-get e toplevel-name)))
    (let loop ((t *broken*) (prev #f))
      (cond ((null? t)
	     (error toplevel-name " is not broken."))
	    ((eqv? (caar t) x)
	     (environment-set! e toplevel-name (cadar t))
	     (if prev
		 (set-cdr! prev (cdr t))
		 (set! *broken* (cdr t)))
	     (unspecified))
	    (else
	     (loop (cdr t) t))))))

; Helpers

(define (debug/trace-enter name args)
  (display (make-string *trace-level* #\space))
  (display "Enter: ")
  (debug/print-call name args)
  (newline))

(define (debug/trace-exit name . ret)
  (display (make-string *trace-level* #\space))
  (display "Exit ")
  (display name)
  (display " => ")
  (cond ((null? ret)
	 (display "[ No values ]"))
	((null? (cdr ret))
	 (debug/print-object (car ret)))
	(else
	 (display "[ ")
	 (do ((ret ret (cdr ret)))
	     ((null? ret))
	   (debug/print-object (car ret))
	   (if (not (null? (cdr ret)))
	       (display ", ")))
	 (display " ]")))
  (newline))

(define (debug/breakpoint toplevel-name args)
  (display "Breakpoint: ")
  (debug/print-call toplevel-name args)
  (newline)
  (debug-continuation-structure (current-continuation-structure) #f))

(define (debug/print-call name args)
  (display "(")
  (display name)
  (for-each (lambda (arg)
	      (display " ")
	      (debug/print-object arg))
	    args)
  (display ")"))

; eof

; Simplistic trace and breakpoint facility -- only for top-level names.
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
; primop; that should be fixed.  The primop should be renamed to
; 'break-here' or somesuch (or 'debugvsm', even).
;
; FIXME: Really want to make sure *trace-level* is set to 0 on error
; (or, equivalently, when a new top-level expression is evaluated).

(define *traced* '())			; list of (trace-proc proc name)
(define *broken* '())			; list of (break-proc proc name)
(define *trace-level* 0)		; indent level

(define (trace . rest)
  (if (null? rest)
      (map caddr *traced*)
      (trace-name (car rest) 'both)))

(define (trace-entry toplevel-name)
  (trace-name toplevel-name 'entry))

(define (trace-exit toplevel-name)
  (trace-name toplevel-name 'exit))

; FIXME: should do multiple return values when tracing the exit.

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
			 (let ((r (apply x args)))
			   (set! *trace-level* (- *trace-level* 2))
			   (debug/trace-exit toplevel-name r)
			   r)))
		      ((entry)
		       (lambda args
			 (debug/trace-enter toplevel-name args)
			 (apply x args)))
		      ((exit)
		       (lambda args
			 (let ((r (apply x args)))
			   (debug/trace-exit toplevel-name r)
			   r)))
		      (else ???))))
	     (environment-set! e toplevel-name t)
	     (set! *traced* (cons (list t x toplevel-name) *traced*))
	     (unspecified))))))

(define (untrace . rest)
  (if (null? rest)
      (begin (for-each (lambda (n) (untrace-name n #f)) (map caddr *traced*))
	     (set! *traced* '()))
      (untrace-name (car rest))))

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
      (unbreak-name (car rest))))

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
  (print-call name args)
  (newline))

(define (debug/trace-exit name ret)
  (display (make-string *trace-level* #\space))
  (display "Exit ")
  (display name)
  (display " => ")
  (debug/print-object ret)
  (newline))

(define (debug/breakpoint toplevel-name args)
  (display "Breakpoint: ")
  (print-call toplevel-name args)
  (newline)
  (debug-continuation-structure (current-continuation-structure)))

(define (print-call name args)
  (display "(")
  (display name)
  (for-each (lambda (arg)
	      (display " ")
	      (debug/print-object arg))
	    args)
  (display ")"))

; eof

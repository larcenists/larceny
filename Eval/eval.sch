; -*- scheme -*-
;
; Larceny -- Simple evaluator
;
; lth@cs.uoregon.edu / 1992
; $Id: eval.sch,v 1.7 1992/08/04 18:27:43 lth Exp $
;
; April 1, 1996
;
; Scheme evaluator which provides a minimal interface to Larceny.
; Typically one does not want to write programs at the interpreter prompt
; but rather compile them and load the compiled code. This works. But one
; can also load interpretable code (raw scheme) and have it work, slowly.
;
; 'Eval' takes an expression and optionally an R5RS environment and
; evaluates the expression in the environment or in the 
; interaction-environment. It returns the result of the evaluation,
; and may change the environment argument (or the interaction-environment).
;
; 'Eval' accepts full R4RS Scheme, but requires a procedure "rewrite" which
; converts full Scheme to its core form.
;
; FIXME: it's possible that there should be some restrictions here for
;        certain environment arguments.

(define eval-version "0.2")

(define eval
  (let ()

    (define (empty-env) '())

    (define (extend-env env names values)
      (cond ((and (null? names) (null? values))
	     env)
	    ((null? names)
	     (error "Wrong number of arguments to procedure; " n " " v))
	    ((not (pair? names))
	     (cons (cons names values) env))
	    (else
	     (extend-env (cons (cons (car names) (car values)) env)
			 (cdr names)
			 (cdr values)))))

    (define (env-set! env name value)
      (let ((probe (assq name env)))
	(if probe
	    (set-cdr! probe value)
	    (global-cell-set! ((global-name-resolver) name) value))))

    (define (env-lookup env name)
      (let ((probe (assq name env)))
	(if probe
	    (cdr probe)
	    (let ((v (global-cell-ref ((global-name-resolver) name))))
	      (if (eq? v (undefined))
		  (error "Undefined global variable \"" name "\".")
		  v)))))

    ; This is devious. It allows us to pass interpreted procedures
    ; to compiled code.

    (define (make-proc env expr) 
      (lambda args
	(eval (cons 'begin (cddr expr))
	      (extend-env env (cadr expr) args))))
    
    (define (eval expr env)
      (cond ((symbol? expr)
	     (env-lookup env expr))
	    ((pair? expr)
	     (cond ((eq? (car expr) 'quote)
		    (cadr expr))
		   ((eq? (car expr) 'set!) 
		    (env-set! env (cadr expr) (eval (caddr expr) env)))
		   ((eq? (car expr) 'lambda)
		    (make-proc env expr))
		   ((eq? (car expr) 'begin)
		    (let loop ((exprs (cdr expr)))
		      (cond ((null? exprs)
			     (error "Empty BEGIN."))
			    ((null? (cdr exprs))
			     (eval (car exprs) env))
			    (else
			     (eval (car exprs) env)
			     (loop (cdr exprs))))))
		   ((eq? (car expr) 'if)
		    (let ((test       (cadr expr))
			  (consequent (caddr expr))
			  (alternate  (if (null? (cdddr expr))
					  #f
					  (cadddr expr))))
		      (if (eval test env)
			  (eval consequent env)
			  (eval alternate env))))
		   (else 
		    (let ((args (map (lambda (x) (eval x env)) expr)))
		      (cond ((null? args)
			     (error "Null procedure call."))
			    ((procedure? (car args))
			     (apply (car args) (cdr args)))
			    (else
			     (error "Not a procedure")))))))
	    ((procedure? expr) expr)
	    ((bytevector-like? expr) expr)
	    ((vector-like? expr) expr)
	    ((boolean? expr) expr)
	    ((char? expr) expr)
	    (else
	     (error "EVAL: unknown expression: " expr))))

    (define (toplevel-eval expr env)
      (let ((new-resolver (lambda (sym)
			    (environment-lookup-binding env sym)))
	    (old-resolver (global-name-resolver)))
	(dynamic-wind
	 (lambda ()
	   (global-name-resolver new-resolver))
	 (lambda ()
	   (if (and (pair? expr) (eq? (car expr) 'define))
	       (begin (toplevel-env-set!
		       (cadr expr)
		       (eval (caddr expr) (empty-env)))
		      (cadr expr))
	       (eval expr (empty-env))))
	 (lambda ()
	   (global-name-resolver old-resolver)))))

    ; The proposal adopted for R5RS seems to require the second argument;
    ; we'll let it default to (interaction-environment).

    (lambda (expr . rest)
      (toplevel-eval (rewrite expr)
		     (cond ((null? rest)
			    (interaction-environment))
			   ((and (null? (cdr rest))
				 (environment? (car rest)))
			    (car rest))
			   (else
			    (error "Eval: bad args: " rest)))))))

; eof

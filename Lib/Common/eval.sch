; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; `Eval' procedure.
;
; Eval is a simple procedure: it calls the value of the system parameter
; `evaluator' with two arguments, the expression and the environment.
; The environment is an optional argument to eval, but not to the evaluator
; procedure.

($$trace "eval")

(define (eval expr . rest)
  (let* ((env (cond ((null? rest)
		     (interaction-environment))
		    ((and (null? (cdr rest))
			  (environment? (car rest)))
		     (car rest))
		    (else
		     (error "Eval: bad arguments: " rest)
		     #t))))
    ((evaluator) expr env)))

(define evaluator
  (make-parameter "evaluator"
                  (lambda (expr env)
                    (error "No evaluator procedure installed."))
                  procedure?))

; eof

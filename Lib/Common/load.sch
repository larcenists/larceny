; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- the 'load' procedure.
;
; FIXME:
;  - Not entirely robust, but ok for now.
;  - Loader should install reader macros for #^G, #^B, #^P so that 
;    the reader would not need to be aware of these extensions.

($$trace "load")

(define load-evaluator
  (system-parameter "load-evaluator"
		    (lambda (expr env)
		      (eval expr env)))) ; That's in Eval/eval.sch.

(define (load filename . rest)

  (define (get-environment)
    (cond ((null? rest)
	   (interaction-environment))
	  ((null? (cdr rest))
	   (car rest))
	  (else
	   (error "load: too many arguments")
	   #t)))

  ;; The environment must be recomputed for each expression evaluation --
  ;; the loaded expressions may change the interaction environment, and
  ;; when the environment is implicit, that change should be reflected in
  ;; subsequent evaluations.

  (define (load-file)
    (let ((p (open-input-file filename)))
      (do ((expr (read p) (read p)))
	  ((eof-object? expr))
	((load-evaluator) expr (get-environment)))
      (close-input-port p)
      (unspecified)))

  ;; The linker is implicit in the loader (as the #^G thing) and uses
  ;; global-name-resolver as the linker.  The following hacks make
  ;; sure that global-name-resolver uses the right environment.  We
  ;; need to separate linking from loading, which will remove this
  ;; silliness.

  (let ((old-resolver (global-name-resolver))
	(new-resolver (lambda (sym)
			(environment-lookup-binding (get-environment) sym))))
    (dynamic-wind 
     (lambda () (global-name-resolver new-resolver))
     (lambda () (load-file))
     (lambda () (global-name-resolver old-resolver)))))


; List->procedure is used by the reader to deal with #^P.

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
	  p
	  (begin (procedure-set! p i (car l))
		 (loop (cdr l) (+ i 1)))))))

; eof

; Lib/load.sch
; Larceny -- the 'load' procedure.
;
; $Id: load.sch,v 1.4 1997/07/18 13:55:49 lth Exp $
;
; Not entirely robust, but ok for now.
;
; FIXME: Loader should install reader macros for #^G, #^B, #^P so that 
; the reader would not need to be aware of these extensions.

($$trace "load")

(define *load-noise-level* #f)

(define load-evaluator
  (system-parameter "load-evaluator"
		    (lambda (expr env)
		      (eval expr env))))

(define (load filename . rest)

  (define (get-environment)
    (cond ((null? rest)
	   (interaction-environment))
	  ((and (null? (cdr rest))
		(environment? (car rest)))
	   (car rest))
	  (else
	   (error "load: too many arguments")
	   #t)))

  (define (eval-expr expr env)
    ((load-evaluator) expr env))

  (define (display-result value)
    (if *load-noise-level*
	(repl-display value)))

  (define (load-file env)
    (let ((p (open-input-file filename)))
      (do ((expr (read p) (read p)))
	  ((eof-object? expr))
	(display-result (eval-expr expr env)))
      (close-input-port p)
      (unspecified)))

  (let* ((env (get-environment))
	 (old-resolver (global-name-resolver))
	 (new-resolver (lambda (sym)
			 (environment-lookup-binding env sym))))
    (dynamic-wind 
     (lambda () (global-name-resolver new-resolver))
     (lambda () (load-file env))
     (lambda () (global-name-resolver old-resolver)))))

(define (load-noisily . args)
  (let ((noise-level *load-noise-level*))
    (set! *load-noise-level* #t)
    (if (not (null? args))
	(begin (load (car args))
	       (set! *load-noise-level* noise-level)))
    (unspecified)))

(define (load-quietly . args)
  (let ((noise-level *load-noise-level*))
    (set! *load-noise-level* #f)
    (if (not (null? args))
	(begin (load (car args))
	       (set! *load-noise-level* noise-level)))
    (unspecified)))

; list->procedure is used by the reader to deal with #^P.

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
	  p
	  (begin (procedure-set! p i (car l))
		 (loop (cdr l) (+ i 1)))))))

; eof

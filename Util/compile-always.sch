; Util/compile-always.sch
; Setup the REPL and the loader to use Twobit for all evaluation, so that
; each expression will be compiled and then evaluated.
;
; $Id$

; FIXME: This only works if loaded into an nbuild-built system 
; (because compile-and-assemble-expression is not visible otherwise).

; FIXME: The next expression should load the fasl file if it exists.

(load "Asm/Common/link-lop.sch")

(let ()

  ; The repl evaluator always takes two arguments, but eval takes one or two.

  (define twobit-repl-eval
    (lambda (expr . rest)
      (let ((env (if (null? rest)
		     (interaction-environment)
		     (car rest))))
	((link-lop-segment (compile-and-assemble-expression expr) env)))))

  ; The load evaluator always takes two arguments.

  (define twobit-load-eval
    (lambda (expr env)
      (if (procedure? expr)
	  (expr)
	  ((link-lop-segment (compile-and-assemble-expression expr) env)))))

  (repl-evaluator twobit-repl-eval)
  (load-evaluator twobit-load-eval)
  (set! eval twobit-repl-eval)
  #t)

; eof

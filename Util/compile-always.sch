; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Install an evaluator that uses Twobit for all evaluation, so that
; each expression will be compiled and then evaluated.

(if (file-exists? "Asm/Common/link-lop.fasl")
    (load "Asm/Common/link-lop.fasl")
    (load "Asm/Common/link-lop.sch"))

(let ()

  (define twobit-eval
    (lambda (expr . rest)
      (let ((env (if (null? rest)
		     (interaction-environment)
		     (car rest))))
	((link-lop-segment (compile-expression expr env) env)))))

  (evaluator twobit-eval)

  (set! macro-expand
	(lambda (expr . rest)
	  (let ((env (if (null? rest)
			 (interaction-environment)
			 (car rest))))
	    (macro-expand-expression expr env))))
  #t)

; eof

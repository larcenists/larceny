; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Install an evaluator that uses Twobit for all evaluation, so that
; each expression will be compiled and then evaluated.

; Note, this _only_ makes sense if twobit has been loaded in a sealed
; environment; if not, the clobbering of macro-expand below is going
; to make a mess of things.

(if (file-exists? "Asm/Common/link-lop.fasl")
    (load "Asm/Common/link-lop.fasl")
    (load "Asm/Common/link-lop.sch"))

; MUST bind the names here so later updates don't clobber them.

(let ((interaction-environment interaction-environment)
      (compile-expression compile-expression)
      (link-lop-segment link-lop-segment)
      (evaluator evaluator)
      (macro-expand-expression macro-expand-expression))

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

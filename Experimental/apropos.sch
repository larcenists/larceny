; Experimental/apropos.sch
; Apropos
;
; $Id: apropos.sch,v 1.1.1.1 1998/11/19 21:52:30 lth Exp $
;
; Idea from Alexander Taranov <tay@jet.msk.su>
; Could use regular expression matching, but OK for now.

; (apropos substring)  => list
; (apropos substring environment)  => list

(define (apropos substr . rest)
  (let ((env (if (null? rest)
		 (interaction-environment)
		 (car rest)))
	(substr (if (symbol? substr)
		    (symbol->string substr)
		    substr)))
    (filter (lambda (name)
	      (and (environment-gettable? env name)
		   (cond ((symbol? name)
			  (substring-match (symbol->string name) substr))
			 ((string? name)
			  (substring-match name substr)))))
	    (environment-variables env))))

; eof

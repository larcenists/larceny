; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Apropos function.
;
; Idea from Alexander Taranov <tay@jet.msk.su>
; Could use regular expression matching, but OK for now.

(require 'string)
(require 'list)

; (apropos substring)  => list
; (apropos substring environment)  => list

(define (apropos substr . rest)
  (let ((env (if (null? rest)
		 (interaction-environment)
		 (car rest)))
	(substr (if (symbol? substr)
		    (symbol->string substr)
		    substr)))
    (sort
     (filter (lambda (name)
               (and (environment-variable? env name)
                    (cond ((symbol? name)
                           (substring-match (symbol->string name) substr))
                          ((string? name)
                           (substring-match name substr)))))
             (environment-variables env))
     (lambda (a b)
       (string<? (symbol->string a) (symbol->string b))))))

; eof

; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Apropos function.
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

; Experimental: print info about each binding.

(define (malapropos x)
  (for-each
   (lambda (name)
     (let ((obj (environment-get (interaction-environment) name)))
       (if (procedure? obj)
           (let ((pname (procedure-name obj))
                 (arity (procedure-arity obj))
                 (expr (procedure-expression obj)))
             (if expr
                 (format #t "~a: procedure~%  name=~a~%  formals=~a~%"
                         name pname (if expr (cadr expr) #f))
                 (format #t "~a: procedure~%  name=~a~%  arity=~a~%"
                         name pname arity)))
           (let ((tag (cond ((list? obj) 'list)
                            ((pair? obj) 'pair)
                            ((number? obj) 'number)
                            ((string? obj) 'string)
                            ((input-port? obj) 'input-port)
                            ((output-port? obj) 'output-port)
                            ((char? obj) 'char)
                            ((null? obj) 'null)
                            ((boolean? obj) 'boolean)
                            ((structure? obj) 'structure)
                            ((vector? obj) 'vector)
                            (else 'weird))))
             (format #t "~a: value (currently a ~a)~%" name tag)))))
            (apropos x)))

; eof

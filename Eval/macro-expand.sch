; Eval/macro-expand.sch
; Larceny -- interpreter's macro expander
;
; $Id: macro-expand.sch,v 1.3 1997/07/18 13:51:43 lth Exp $
;
; USAGE
;   (macro-expand <expr>)
;     Returns a new expression which shares structure with the input 
;     expression in quoted constants only.
;
; DESCRIPTION
;   This is a stop-gap macro expander for use by the interpreter.
;
;   Rewrites a scheme expression to a primitive expression type involving
;   only LAMBDA, IF, QUOTE, SET!, top-level DEFINE, variable references,
;   and procedure call.  All constants are quoted.  No alpha conversion
;   is performed.
;
;   User-defined macros are not supported.
;
; KNOWN BUGS
; * Should expand internal definitions of the form 
;       (begin (define x ...) ...) 
;   to the corresponding sequence of definitions.
; * DELAY is not supported.
;
; MISFEATURES
; * Could use constants a lot more in quasiquotation rewrites.
; * These procedures are fragile and bad syntax will break many of them.

($$trace "macro-expand")

(define *eval-macros* '())

(define (define-eval-macro kwd proc)
  (set! *eval-macros* (cons (cons kwd proc) *eval-macros*)))

(define macro-expand
  (let ()

    (define (definition? expr)
      (and (pair? expr) (eq? (car expr) 'define)))

    (define (lambda? expr)
      (and (pair? expr) (eq? (car expr) 'lambda)))

    (define (constant? expr)
      (or (and (pair? expr) (eq? (car expr) 'quote))
	  (number? expr)
	  (boolean? expr)
	  (char? expr)
	  (string? expr)
	  (procedure? expr)
	  (null? expr)
	  (eq? expr (unspecified))))

    ; Rewrite any old expression. This is a recursive process, and the base 
    ; cases are simple expressions (quotations, constants, identifiers) and
    ; ``lambda'' forms, which require special handling.

    (define (rewrite-expr expr)

      (define (quote-expr expr)
	(if (not (and (pair? expr) (eq? (car expr) 'quote)))
	    (list 'quote expr)
	    expr))

      (cond ((constant? expr)
	     (quote-expr expr))
	    ((symbol? expr)
	     expr)
	    ((lambda? expr)
	     (rewrite-lambda expr))
	    ((pair? expr)
	     (let ((form (assq (car expr) *eval-macros*)))
	       (if form
		   (rewrite-expr ((cdr form) expr))
		   (map rewrite-expr expr))))
	    (else
	     (error "Strange expression: " expr))))


    ; Rewriting top-level definitions and lambda expressions are really 
    ; special cases, since they recursively call 'rewrite'. None of the 
    ; other special forms need to do that.

    (define (rewrite-define expr)
      (cond ((pair? (cadr expr))
	     `(define 
		,(caadr expr) 
		,(rewrite-lambda `(lambda ,(cdadr expr) ,@(cddr expr)))))
	    ((not (symbol? (cadr expr)))
	     (error "Illegal define form: " expr)
	     #t)
	    ((null? (cddr expr))
	     `(define ,(cadr expr) ,(undefined)))
	    (else
	     `(define ,(cadr expr) ,(rewrite-expr (caddr expr))))))


    ; Top-level defines are left as a 'define', with the usual special 
    ; case for shorthanded lambda-definitions.

    (define (rewrite-top-level-define expr)
      (rewrite-define expr))


    ; Rewriting a lambda expression involves dealing with internal defines
    ; in addition to rewriting the body of the expression.

    (define (rewrite-lambda expr)

      (define (collect-defines body defs)
	(if (or (null? body) (not (definition? (car body))))
	    (cons (reverse defs) body)
	    (collect-defines (cdr body) (cons (car body) defs))))

      (define (rewrite-internal-defines defs body) 
	`(letrec ,(map (lambda (d) (cdr (rewrite-define d))) defs) ,@body))

      (let* ((result (collect-defines (cddr expr) '()))
	     (defs   (car result))
	     (body   (cdr result)))
	(if (null? defs)
	    `(lambda ,(cadr expr)
	       ,(rewrite-expr (cons 'begin body)))
	    `(lambda ,(cadr expr)	
	       ,(rewrite-expr (rewrite-internal-defines defs body))))))


    ; Top-level rewriter -- handles top-level definitions as special case.
    ; They are left as definitions, while internal defines are rewritten as
    ; a letrec.

    (define (rewrite expr)
      (if (definition? expr)
	  (rewrite-top-level-define expr)
	  (rewrite-expr expr)))

    rewrite))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Derived forms

(define-eval-macro 'and
  (lambda (expr)
    (cond ((null? (cdr expr)) #t)
	  ((null? (cddr expr)) (cadr expr))
	  (else  `(if ,(cadr expr) (and ,@(cddr expr)) #f)))))

(define-eval-macro 'or
  (lambda (expr)
    (cond ((null? (cdr expr)) #f)
	  ((null? (cddr expr)) (cadr expr))
	  (else   (let ((tmp (gensym "or")))
		    `(let ((,tmp ,(cadr expr)))
		       (if ,tmp ,tmp (or ,@(cddr expr)))))))))

(define-eval-macro 'let
  (lambda (expr)
    (define (named-let? expr) (symbol? (cadr expr)))
    (define nlet-bindings caddr)
    (define let-bindings cadr)
    (define nlet-body cdddr)
    (define let-body cddr)
    (define (collect-vars bindings) (map car bindings))
    (define (collect-inits bindings) (map cadr bindings))
    (define named-let-name cadr)

    (let ((bindings (if (named-let? expr)
			(nlet-bindings expr)
			(let-bindings expr)))
	  (body (if (named-let? expr)
		    (nlet-body expr)
		    (let-body expr))))

      (let ((vars (collect-vars bindings))
	    (inits (collect-inits bindings)))
	(if (named-let? expr)
	    (let ((id (named-let-name expr)))
	      `((letrec ((,id (lambda ,vars ,@body)))
		  ,id)
		,@inits))
	    `((lambda ,vars ,@body) ,@inits))))))

(define-eval-macro 'letrec
  (lambda (expr)
    (define (collect-vars) (map car (cadr expr)))
    (define (collect-inits) (map cadr (cadr expr)))

    (let ((vars (collect-vars))
	  (inits (collect-inits)))
      `((lambda
	    ,vars
	  ,@(map (lambda (x y) `(set! ,x ,y)) vars inits)
	  ,@(cddr expr))
	,@(map (lambda (x) (unspecified)) vars)))))

(define-eval-macro 'let*
  (lambda (expr)
    (let ((bindings (cadr expr)))
      (if (null? bindings)
	  `(let () ,@(cddr expr))
	  `(let (,(car bindings))
	     (let* ,(cdr bindings) ,@(cddr expr)))))))

(define-eval-macro 'cond
  (lambda (expr)
    (if (null? (cdr expr))
	(unspecified)
	(let ((test (caadr expr))
	      (e-sequence (cdadr expr))
	      (clause2 (cddr expr)))
	  (cond ((eq? test 'else) `(begin ,@e-sequence))
		((null? e-sequence) `(or ,test (cond ,@clause2)))
		((eq? (car e-sequence) '=>)
		 (let ((tmp (gensym "cond")))
		   `(let ((,tmp ,test))
		      (if ,tmp
			  (,(cadr e-sequence) ,tmp)
			  (cond ,@clause2)))))
		(else 
		 `(if ,test (begin ,@e-sequence) (cond ,@clause2))))))))

(define-eval-macro 'case
  (lambda (expr)
    (let ((s (gensym "case")))
      `(let ((,s ,(cadr expr)))
	 (cond
	  ,@(let loop ((q (cddr expr)) (r '()))
	      (cond ((null? q) (reverse r))
		    ((eq? (caar q) 'else)
		     (reverse (cons (car q) r)))
		    (else
		     (loop (cdr q)
			   (cons 
			    (let ((x (car q)))
			      (cons (cons 'or
					  (map (lambda (z)
						 `(eqv? ,s (quote ,z)))
					       (car x)))
				    (cdr x)))
			    r))))))))))

(define-eval-macro 'do
  (lambda (expr)

    (define (atom? x) (not (pair? x)))

    (if (or (not (list? expr))
	    (< (length expr) 3)
	    (not (list? (cadr expr)))
	    (not (and (list? (caddr expr)) (pair? (caddr expr)))))
	(error "Malformed DO expression: " expr))

    (let ((loop     
	   (gensym "do"))
	  (bindings 
	   (map (lambda (x)
		  (cond ((atom? x)
			 (error "Malformed DO expression: " expr))
			((atom? (cdr x))
			 (list (car x) '() (car x)))
			((atom? (cddr x))
			 (list (car x) (cadr x) (car x)))
			(else x)))
		(cadr expr)))
	  (test 
	   (caddr expr)))
      `(letrec ((,loop (lambda ,(map car bindings) 
			 (if ,(car test) 
			     ,(if (null? (cdr test))
				  (unspecified)
				  `(begin ,@(cdr test)))
			     (begin ,@(cdddr expr)
				    (,loop ,@(map caddr bindings)))))))
	 (,loop ,@(map cadr bindings))))))


; Quasiquotations.
;
; Notation:
;
;   0 <= m <= infty
;   1 <= n <= infty
;   v is a vector
;   Caps indicate literals. Lower case indicates actual code.
;   Aliases: QQ = quasiquote
;            UNQ = unquote
;            UNQS = unquote-splicing
;            L->V = list->vector
;            v->l = vector->list
;
; Rewrite Rules (apply matching top-down, from the outside and in):
;
;   (r (QQ v) 0) => (L->V (r (QQ (v->l v)) 0)))
;   (r (QQ v) n) => (LIST (QUOTE QQ) (L->V (r (QQ (v->l v)) n))))
;   (r (QQ (UNQ x) 0)) => x
;   (r (QQ (UNQ x) n)) => (LIST (QUOTE UNQ) (r (QQ x) (- n 1)))
;   (r (QQ (QQ x) m)) => (LIST (QUOTE QQ) (r (QQ x) (+ m 1)))
;   (r (QQ ((UNQS x) . y)) 0) => (APPEND x (r (QQ y) 0))
;   (r (QQ ((UNQS x) . y)) n) => (LIST (QUOTE QQ)
;                                      (LIST (LIST (QUOTE UNQS) 
;                                            (r (QQ x) (- n 1)))
;                                      (r (QQ y) n)))
;;   (r (QQ (QUOTE x)) m) => (LIST (QUOTE QUOTE) (QUOTE x))
;   (r (QQ (x . y) m)) => (CONS (r (QQ x) m) (r (QQ y) m))
;   (r (QQ x) m) => (QUOTE x)
;
; Notes:
;
;   Not terribly robust.
;   One could write a set of rules which would expand to more 
;   efficient code, using literals wherever possible. Here, we 
;   almost always expand to runnable code, resulting in slower
;   code. A decent source-level peephole optimizer can recover
;   many of the literals, however.

(define-eval-macro 'quasiquote

  ;; "Safe" names of procedures we use. Needs improvement, but good 
  ;; enough for now.

  (let ((hyg-list '%list)
	(hyg-cons '%cons)
	(hyg-append '%append)
	(hyg-list->vector '%list->vector))
    (lambda (expr)

      (define (r e l)
	(cond ((vector? (cadr e))
	       (let ((v (cadr e)))
		 (if (zero? l)
		     (list hyg-list->vector
			   (r (list 'QUASIQUOTE (vector->list v)) l))
		     (list hyg-list
			   'QUASIQUOTE
			   (list hyg-list->vector 
				 (r (list 'QUASIQUOTE (vector->list v)) l))))))
	      ((pair? (cadr e))
	       (cond ((eq? (car (cadr e)) 'UNQUOTE)
		      (let ((x (cadr (cadr e))))
			(if (zero? l)
			    x
			    (list hyg-list '(QUOTE UNQUOTE)
				  (r (list 'QUASIQUOTE x) (- l 1))))))
		     ((eq? (car (cadr e)) 'QUASIQUOTE)
		      (let ((x (cadr (cadr e))))
			(list hyg-list 
			      '(QUOTE QUASIQUOTE)
			      (r (list 'QUASIQUOTE x) (+ l 1)))))
		     ((and (pair? (car (cadr e)))
			   (eq? (caar (cadr e)) 'UNQUOTE-SPLICING))
		      (let ((x (cadr (car (cadr e))))
			    (y (cdr (cadr e))))
			(if (zero? l)
			    (list hyg-append x (r (list 'QUASIQUOTE y) 0))
			    (list hyg-list
				  '(QUOTE quasiquote)
				  (list hyg-list
					(list hyg-list 
					      '(QUOTE UNQUOTE-SPLICING)
					      (r (list 'QUASIQUOTE x) (- l 1)))
					(r (list 'QUASIQUOTE y) l))))))
;		     ((eq? (car (cadr e)) 'QUOTE)
;		      (list hyg-list '(QUOTE QUOTE) (cadr e)))
		     (else
		      (let ((x (car (cadr e)))
			    (y (cdr (cadr e))))
			(list hyg-cons 
			      (r (list 'QUASIQUOTE x) l) 
			      (r (list 'QUASIQUOTE y) l))))))
	      (else
	       (let ((x (cadr e)))
		 (list 'QUOTE x)))))

      (r expr 0))))

; eof

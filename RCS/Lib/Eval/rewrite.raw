;; -*- Scheme -*-
;;
;; REWRITE --  rewrite Scheme syntax forms to basic forms
;;
;; DESCRIPTION
;;   Rewrites a scheme expression to a primitive expression type involving
;;   only LAMBDA, IF, QUOTE, SET!, top-level DEFINE, variable references,
;;   and procedure application. All constants are quoted. No alpha conversion
;;   is done.
;;
;; USAGE
;;   (rewrite <expr>)
;;     Returns a new expression which shares structure with the input 
;;     expression in quoted constants only.
;;
;;   (rewrite-file <inputfilename> <outputfilename>)
;;     Rewrite all expressions in the given input file and write the 
;;     rewritten forms to the given output file.
;;
;; BUGS
;;   Should expand (begin (define x ...) ...) to the corresponding sequence
;;   of definitions; is now expanded to a letrec with an empty body, which
;;   is wrong.
;;
;;   Multiple nested BEGINs should be folded.
;;
;;   The => subsyntax in COND is not implemented.
;;
;;   These procedures are fragile and bad syntax will break many
;;   of them.
;;
;;   Could use constants a lot more in quasiquotation rewrites (see below).
;;
;;   Use of the "unspecified" procedure is nonportable but is nice to have
;;   for Larceny; search for NONPORTABLE to remove.

(define rewrite
  (let ()

    ;; "Portable" interface to Chez Scheme's bogus "error" procedure.

    (define (rewrite-error . msgs)
      (display "Error:")
      (for-each (lambda (x) (display " ") (display x)) msgs)
      (newline)
      (error '() ""))

    (define (atom? x) (not (pair? x)))

    ;; "Safe" names of procedures we use. Needs improvement, but good 
    ;; enough for now.

    (define hyg-list '%list)
    (define hyg-cons '%cons)
    (define hyg-append '%append)
    (define hyg-list->vector '%list->vector)

    ;; Non-basic special forms.

    (define special-forms-list #f)

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
	  (eq? expr (unspecified))      ; NONPORTABLE. For Larceny.
	  ))

    ;; Rewrite any old expression. This is a recursive process, and the base 
    ;; cases are simple expressions (quotations, constants, identifiers) and
    ;; ``lambda'' forms, which require special handling.
    ;;
    ;; Special forms are found in the assoc list `special-forms-list', the cdr
    ;; of each element is the procedure used to rewrite the form.

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
	     (let ((form (assq (car expr) special-forms-list)))
	       (if form
		   (rewrite-expr ((cdr form) expr))
		   (map rewrite-expr expr))))
	    (else
	     (rewrite-error "Strange expression" expr))))


    ;; Rewriting top-level definitions and lambda expressions are really 
    ;; special cases, since they recursively call 'rewrite'. None of the 
    ;; other special forms need to do that.

    ;; Rewrite a "define" to get rid of any shorthands and non-basic forms.

    (define (rewrite-define expr)
      (if (pair? (cadr expr))
	  `(define 
	     ,(caadr expr) 
	     ,(rewrite-lambda `(lambda ,(cdadr expr) ,@(cddr expr))))
	  `(define ,(cadr expr) ,(rewrite-expr (caddr expr)))))


    ;; Top-level defines are left as a 'define', with the usual special 
    ;; case for shorthanded lambda-definitions.

    (define rewrite-top-level-define rewrite-define)

    ;; Rewriting a lambda expression involves dealing with internal defines
    ;; in addition to rewriting the body of the expression.

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


    ;;***********************************************************************
    ;; The following are the templates for the common-case special forms
    ;; (the ones that don't recursively call "rewrite").
    ;;***********************************************************************

    (define (rewrite-and expr)
      (cond ((null? (cdr expr)) #t)
	    ((null? (cddr expr)) (cadr expr))
	    (else  `(if ,(cadr expr) (and ,@(cddr expr)) #f))))

    (define (rewrite-or expr)
      (cond ((null? (cdr expr)) #f)
	    ((null? (cddr expr)) (cadr expr))
	    (else   `(if ,(cadr expr) #t (or ,@(cddr expr))))))

    (define (rewrite-let expr)
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
		`((lambda (,id)
		    (set! ,id (lambda ,vars ,@body))
		    (,id ,@inits))
		  '()))
	      `((lambda ,vars ,@body) ,@inits)))))

    
    (define (rewrite-letrec expr)
      (define (collect-vars) (map car (cadr expr)))
      (define (collect-inits) (map cadr (cadr expr)))

      (let ((vars (collect-vars))
	    (inits (collect-inits)))
	`((lambda
	      ,vars
	    ,@(map (lambda (x y) `(set! ,x ,y)) vars inits)
	    ,@(cddr expr))
	  ,@(map (lambda (x) #f) vars))))


    (define (rewrite-let* expr)
      (let ((bindings (cadr expr)))
	(if (null? bindings)
	    `(begin ,@(cddr expr))
	    `(let (,(car bindings))
	       (let* ,(cdr bindings) ,@(cddr expr))))))

    ;; Doesn't do the => syntax.

    (define (rewrite-cond expr)
      (if (null? (cdr expr))
	  'unspecified
	  (let ((test (caadr expr))
		(e-sequence (cdadr expr))
		(clause2 (cddr expr)))
	    (cond ((eq? test 'else) `(begin ,@e-sequence))
		  ((null? e-sequence) `(or ,test (cond ,@clause2)))
		  ( else 
		    `(if ,test (begin ,@e-sequence) (cond ,@clause2)))))))

    (define (rewrite-case l)
      (let ((s (gensym "T")))
	`(let ((,s ,(cadr l)))
	   (cond
	    ,@(let loop ((q (cddr l)) (r '()))
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
			      r)))))))))


    (define (rewrite-do expr)
      (if (or (not (list? expr))
	      (< (length expr) 3)
	      (not (list? (cadr expr)))
	      (not (and (list? (caddr expr)) (pair? (caddr expr)))))
	  (rewrite-error "Malformed do expression" expr))
      (let ((loop     
	     (gensym "DO"))
	    (bindings 
	     (map (lambda (x)
		    (cond ((atom? x)
			   (rewrite-error "Malformed do expression" expr))
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
			       (begin ,@(cdr test))
			       (begin (begin ,@(cdddr expr))
				      (,loop ,@(map caddr bindings)))))))
	   (,loop ,@(map cadr bindings)))))


    ;; Rewrites quasiquotations. Painfully.
    ;;
    ;; Notation:
    ;;
    ;;   0 <= m <= infty
    ;;   1 <= n <= infty
    ;;   v is a vector
    ;;   Caps indicate literals. Lower case indicates actual code.
    ;;   Aliases: QQ = quasiquote
    ;;            UNQ = unquote
    ;;            UNQS = unquote-splicing
    ;;            L->V = list->vector
    ;;            v->l = vector->list
    ;;
    ;; Rewrite Rules (apply matching top-down, from the outside and in):
    ;;
    ;;   (r (QQ v) 0) => (L->V (r (QQ (v->l v)) 0)))
    ;;   (r (QQ v) n) => (LIST (QUOTE QQ) (L->V (r (QQ (v->l v)) n))))
    ;;   (r (QQ (UNQ x) 0)) => x
    ;;   (r (QQ (UNQ x) n)) => (LIST (QUOTE UNQ) (r (QQ x) (- n 1)))
    ;;   (r (QQ (QQ x) m)) => (LIST (QUOTE QQ) (r (QQ x) (+ m 1)))
    ;;   (r (QQ ((UNQS x) . y)) 0) => (APPEND x (r (QQ y) 0))
    ;;   (r (QQ ((UNQS x) . y)) n) => (LIST (QUOTE QQ)
    ;;                                      (LIST (LIST (QUOTE UNQS) 
    ;;                                            (r (QQ x) (- n 1)))
    ;;                                      (r (QQ y) n)))
    ;;   (r (QQ (QUOTE x)) m) => (LIST (QUOTE QUOTE) (QUOTE x))
    ;;   (r (QQ (x . y) m)) => (CONS (r (QQ x) m) (r (QQ y) m))
    ;;   (r (QQ x) m) => (QUOTE x)
    ;;
    ;; Notes:
    ;;
    ;;   Not terribly robust.
    ;;   One could write a set of rules which would expand to more 
    ;;   efficient code, using literals wherever possible. Here, we 
    ;;   almost always expand to runnable code, resulting in slower
    ;;   code. A decent source-level peephole optimizer can recover
    ;;   many of the literals, however.

    ;; Rewrite procedure.

    (define (rewrite-quasiquotation expr)

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
		     ((eq? (car (cadr e)) 'QUOTE)
		      (list hyg-list '(QUOTE QUOTE) (cadr e)))
		     (else
		      (let ((x (car (cadr e)))
			    (y (cdr (cadr e))))
			(list hyg-cons 
			      (r (list 'QUASIQUOTE x) l) 
			      (r (list 'QUASIQUOTE y) l))))))
	      (else
	       (let ((x (cadr e)))
		 (list 'QUOTE x)))))

      (r expr 0))

    ;; Top-level rewriter -- handles top-level definitions as special case.
    ;; They are left as definitions, while internal defines are rewritten as
    ;; a letrec.

    (define (rewrite expr)
      (if (definition? expr)
	  (rewrite-top-level-define expr)
	  (rewrite-expr expr)))

    (set! special-forms-list
      (list (cons 'or rewrite-or)
	    (cons 'and rewrite-and)
	    (cons 'let rewrite-let)
	    (cons 'letrec rewrite-letrec)
	    (cons 'let* rewrite-let*)
	    (cons 'case rewrite-case)
	    (cons 'cond rewrite-cond)
	    (cons 'do rewrite-do)
	    (cons 'quasiquote rewrite-quasiquotation)))

    rewrite))

(define (rewrite-file infilename outfilename)
  (delete-file outfilename)
  (call-with-input-file infilename
    (lambda (in)
      (call-with-output-file outfilename
	(lambda (out)
	  (let loop ((item (read in)))
	    (if (eof-object? item)
		#t
		(begin (write (rewrite item) out)
		       (loop (read in))))))))))

; eof

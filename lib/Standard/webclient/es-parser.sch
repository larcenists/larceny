; BUG: do not use fluid-let, it is not thread safe.

; First(SourceElements) = First(Statement) + First(FunctionDeclaration)
; Follow(SourceElements) = EOF, rbrace + First(SourceElements)

; First(FunctionDeclaration) = function
; First(Statement) = lbrace, var, semi, First(Expression), if, for, 
;                    while, do, continue, break, return, with, switch,
;                    throw, try

; First(Expression) = ident, number, string, regexp, lparen, lbracket, 
;                     null, true, false, +,, -, ~, !, new, delete, void, 
;                     typeof, ++, --, this

(define (es-parse-sourceelement ctx)
  (case (current-token ctx)
    ((function) 
     (es-parse-function ctx #t))
    (else
     (es-parse-statement ctx))))

(define (es-parse-function ctx decl?)
  (consume ctx 'function)
  (let ((id ((if decl? consume consume-if-present) ctx 'ident)))
    (consume ctx 'lparen)
    (let ((formals (es-parse-formals ctx)))
      (consume ctx 'rparen)
      (consume ctx 'lbrace)
      (let ((body (es-parse-function-body ctx)))
	(consume ctx 'rbrace)
	((if decl? make-function-declaration make-function-expression) 
	 id 
	 formals
	 body)))))

(define (es-parse-statement ctx)
  (let ((s (case (current-token ctx)
	     ((lbrace)   (es-parse-block ctx))
	     ((var)      (es-parse-var ctx))
	     ((if)       (es-parse-if ctx))
	     ((do)       (es-parse-do ctx))
	     ((while)    (es-parse-while ctx))
	     ((for)      (es-parse-for ctx))
	     ((continue) (es-parse-continue ctx))
	     ((break)    (es-parse-break ctx))
	     ((return)   (es-parse-return ctx))
	     ((throw)    (es-parse-throw ctx))
	     ((with)     (es-parse-with ctx))
	     ((switch)   (es-parse-switch ctx))
	     ((try)      (es-parse-try ctx))
	     ((ident)    (if (eq? 'colon (lookahead-token ctx))
			     (es-parse-label ctx)
			     (es-parse-exprstmt ctx)))
	     ((semi)     (make-empty-statement))
	     (else       (es-parse-exprstmt ctx)))))
    (consume-semi ctx)
    s))

(define (es-parse-statements ctx . terminators)
  (let loop ((stmts '()))
    (let ((t (current-token ctx)))
      (if (memq t terminators)
	  (reverse stmts)
	  (loop (cons (es-parse-statement ctx) stmts))))))

(define (es-parse-block ctx)
  (consume ctx 'lbrace)
  (let ((stmts (es-parse-statements ctx 'rbrace)))
    (no-semicolon-insertion ctx)
    (make-block-statement stmts)))

(define (es-parse-var ctx)
  (consume ctx 'var)
  (make-var-statement
   (es-parse-list ctx
		  (lambda (ctx)
		    (let ((id (consume-if-present ctx 'ident)))
		      (if id
			  (let ((t (current-token ctx)))
			    (if (eq? t 'assign)
				(begin
				  (consume ctx 'assign)
				  (fluid-let ((es-comma? #f))
				    (let ((e (es-parse-expression ctx)))
				      (cons id e))))
				id))
			  #f)))
		  'comma)))

(define (es-parse-if ctx)
  (consume ctx 'if)
  (consume ctx 'lparen)
  (let ((test (es-parse-expression ctx)))
    (consume ctx 'rparen)
    (let ((consequent (es-parse-statement ctx)))
      (let ((alternate
	     (if (eq? (current-token ctx) 'else)
		 (begin (consume ctx 'else)
			(es-parse-statement ctx))
		 (make-empty-statement))))
	(make-if-statement test consequent alternate)))))

(define (es-parse-do ctx)
  (consume ctx 'do)
  (let ((body (es-parse-statement ctx)))
    (consume ctx 'while)
    (consume ctx 'lparen)
    (let ((test (es-parse-expression ctx)))
      (consume ctx 'rparen)
      (make-do-statement body test))))

(define (es-parse-while ctx)
  (consume ctx 'while)
  (consume ctx 'lparen)
  (let ((test (es-parse-expression ctx)))
    (consume ctx 'rparen)
    (let ((body (es-parse-statement ctx)))
      (make-while-statement test body))))

(define (es-parse-for ctx)
  (consume ctx 'for)
  (consume ctx 'lparen)
  (let ((var? (consume-if-present ctx 'var)))
    (let ((e0 (if (eq? 'semi (current-token ctx))
		  #f
		  (fluid-let ((in? #f))
		    (es-parse-expression ctx))))) ; BUG: use var parser if var?
      (case (current-token ctx) ; BUG: only if not semi above
	((in) 
	 (consume ctx 'in)
	 (let ((e1 (es-parse-expression ctx)))
	   (consume ctx 'rparen)
	   (let ((stmt (es-parse-statement ctx)))
	     (make-for-in-statement var? e0 e1 stmt))))
	((semi)
	 (consume ctx 'semi)
	 (let* ((e1 (if (eq? 'semi (current-token ctx))
			#f
			(begin
			  (consume ctx 'semi)
			  (es-parse-expression ctx))))
		(e2 (if (eq? 'rparen (current-token ctx))
			#f
			(begin
			  (consume ctx 'semi)
			  (es-parse-expression ctx)))))
	   (make-for-statement var? e0 e1 e2)))
	(else 
	 (es-syntax-error "Unexpected token in FOR: " 
			  (current-token ctx)))))))

(define (es-parse-continue ctx)
  (consume ctx 'continue)
  (insert-semi-if-linebreak ctx)
  (let ((id (consume-if-present ctx 'ident)))
    (make-continue-statement id)))

(define (es-parse-break ctx)
  (consume ctx 'break)
  (insert-semi-if-linebreak ctx)
  (let ((id (consume-if-present ctx 'ident)))
    (make-break-statement id)))

(define (es-parse-return ctx)
  (consume ctx 'return)
  (insert-semi-if-linebreak ctx)
  (if (eq? (current-token ctx) 'semi)
      (make-return-statement #f)
      (make-return-statement (es-parse-expression ctx))))

(define (es-parse-throw ctx)
  (consume ctx 'throw)
  (insert-semi-if-linebreak ctx)
  (make-throw-statement (es-parse-expression ctx)))

(define (es-parse-with ctx)
  (consume ctx 'with)
  (consume ctx 'lparen)
  (let ((test (es-parse-expression ctx)))
    (consume ctx 'rparen)
    (let ((body (es-parse-statement ctx)))
      (make-with-statement test body))))

(define (es-parse-switch ctx)
  (consume ctx 'switch)
  (consume ctx 'lparent)
  (let ((switch-expr (es-parse-expression ctx)))
    (consume ctx 'rparen)
    (consume ctx 'lbrace)
    (let loop ((case-blocks '()))
      (cond ((consume-if-present ctx 'case)
	     (let ((case-expr (es-parse-expression ctx)))
	       (consume ctx 'colon)
	       (let ((stmts (es-parse-statements ctx 'case 'default 'rbrace)))
		 (loop (cons (list case-expr stmts) case-blocks)))))
	    ((consume-if-present ctx 'default)
	     (consume ctx 'colon)
	     (let ((stmts (es-parse-statements ctx 'case 'default 'rbrace)))
	       (loop (cons (list #f stmts) case-blocks))))
	    (else
	     (consume ctx 'rbrace)
	     (make-switch-statement switch-expr (reverse case-blocks)))))))

(define (es-parse-try ctx)
  (consume ctx 'try)
  (let ((trybody (es-parse-block ctx))
	(catchvar #f)
	(catchbody #f)
	(finally #f))
    (if (eq? (current-token ctx) 'catch)
	(begin
	  (consume-token ctx)
	  (consume ctx 'lparen)
	  (set! catchvar (consume ctx 'ident))
	  (consume ctx 'rparen)
	  (set! catchbody (es-parse-block ctx))))
    (if (eq? (current-token ctx) 'finally)
	(begin
	  (consume ctx 'finally)
	  (set! finally (es-parse-block ctx))))
    (make-try-statement trybody catchvar catchbody finally)))

(define (es-parse-label ctx)
  (let ((label (consume-token ctx)))
    (consume ctx 'colon)
    (make-label-statement label (es-parse-statement ctx))))

(define (es-parse-exprstmt ctx)
  (make-expr-statement (es-parse-expr ctx)))

(define es-parse-expr 
  (let ()

    (define (fail t)
      (es-syntax-error "Expected expression following " t))

    ; Each parser returns #f if it can't make progress and is not
    ; in an error state.

    (define (binop-parser ops factor)
      (lambda (ctx)
	(let ((e (factor ctx)))
	  (if (not e)
	      e
	      (let loop ((e e))
		(let ((t (current-token/operator ctx)))
		  (if (memq t ops)
		      (begin 
			(consume-token ctx)
			(let ((f (factor ctx)))
			  (if (not f)
			      (fail t)
			      (loop (make-binop t e f))))))))))))

    (define (conditional-parser factor)
      (lambda (ctx)
	(let ((e (factor ctx)))
	  (if (not e)
	      e
	      (let ((t (current-token/operator ctx)))
		(if (not (eq? t 'question))
		    e
		    (begin
		      (consume-token t)
		      (let ((f (factor ctx)))
			(if (not f)
			    (fail t)
			    (begin
			      (consume-token ctx)
			      (let ((t (current-token/operator ctx)))
				(if (not (eq? t 'colon))
				    (fail t)
				    (begin
				      (consume-token ctx)
				      (let ((g (factor ctx)))
					(if (not g)
					    (fail t)
					    (make-cond e f g))))))))))))))))


    (define (translate t translations)
      (cond ((assq t translations) => cdr)
	    (else t)))

    (define (unop-postfix-parser ops translations factor)
      (lambda (ctx)
	(let ((f (factor ctx)))
	  (if f
	      (let ((t (current-token/no-newline ctx)))
		(if (memq t ops)
		    (begin
		      (consume-token ctx)
		      (make-unop (translate t translations) f))
		    f))))))

    (define (unop-prefix-parser ops translations factor)
      (lambda (ctx)
	(let recur ()
	  (let ((t (current-token ctx)))
	    (if (memq t ops)
		(begin
		  (consume-token ctx)
		  (let ((e (recur)))
		    (if (not e)
			(fail t)
			(make-unop (translate t translations) e))))
		(factor ctx))))))

    (define (lefthandside-parser)
      (lambda (ctx)
	(case (current-token ctx)
	  ((function) ...)
	  ((new) ...)
	  (else
	   (let ((p (primary ctx)))
	     ...)))))

    (define (primary ctx)
      (let ((t (current-token ctx)))
	(cond ((eq? t 'this) 
	       (make-this-expr))
	      ((eq? t 'lparen)
	       (consume-token ctx)
	       (let ((e (es-parse-expr ctx)))
		 (consume ctx 'rparen)
		 e))
	      ((eq? t 'lbracket)
	       (array-literal ctx))
	      ((eq? t 'lbrace)
	       (object-literal ctx))
	      ((eq? t 'true)
	       (begin (consume ctx) (make-true-expr))
	       ((eq? t 'false)
		(begin (consume ctx) (make-false-expr)))
	       ((pair? t) 
		(consume ctx))
	       (else #f)))))

    (define (array-literal ctx)
      (consume ctx 'lbracket)
      ...)

    (define (object-literal ctx)
      (consume ctx 'lbrace)
      ...)

    ; FIXME: handle 'in' properly, it is not always allowed.

    (binop-parser '(comma)
     (binop-parser '(assign add-assign sub-assign mul-assign div-assign
	  	     mod-assign bitand-assign bitior-assign bitxor-assign 
		     bitshl-assign bitshr-assign bitshru-assign)
      (conditional-parser
       (binop-parser '(or)
        (binop-parser '(and)
         (binop-parser '(bitior)
	  (binop-parser '(bitxor)
  	   (binop-parser '(bitand)
	    (binop-parser '(eq eqv neq neqv)
	     (binop-parser '(lt le gt ge in instanceof)
	      (binop-parser '(bitshr bitshru bitshl)
	       (binop-parser '(add sub)
	        (binop-parser '(mul div mod)
	         (unop-prefix-parser '(delete void typeof increment decrement 
				       add sub bitnot not)
				     '((increment . preincrement)
				       (decrement . predecrement)
				       (add       . identity)
				       (sub       . negate))
		  (unop-postfix-parser '(increment decrement)
				       '((increment . postincrement)
					 (decrement . postdecrement))
		   (lefthandside-parser))))))))))))))))))

(define (es-parse-list ctx parser separator)
  (let loop ((xs '()))
    (let ((x (parser ctx)))
      (if x
	  (let ((t (current-token ctx)))
	    (if (eq? t separator)
		(begin
		  (consume-token ctx)
		  (loop (cons x xs)))
		(reverse (cons x xs))))
	  (reverse xs)))))


; Parser machinery

(define (consume ctx token)
  (let ((t (current-token ctx)))
    (if (eq? t token)
	(begin (consume-token ctx) t)
	(if (and (pair? t) (eq? (car t) token))
	    (begin (consume-token ctx) t)
	    (es-syntax-error "Expected token: " token)))))

(define (consume-if-present ctx token)
  (let ((t (current-token ctx)))
    (if (eq? t token)
	(begin (consume-token ctx) t)
	(if (and (pair? t) (eq? (car t) token))
	    (begin (consume-token ctx) t)
	    #f))))

(define (current-token ctx)  ...)
(define (consume-token ctx)  ...)


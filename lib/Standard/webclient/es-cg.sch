; FIXME: statement return values, not important except in browsers
; FIXME: statement labels, require passing of label sets

(define (cg elements)
  (for-each cg-element elements))

(define (cg-element element)
  (case (node.type element)
    ((function) ...)
    ((label)    ...)
    ((block)    (cg-block (map cg-element (block.stmts element))))
    ((var)      (cg-var (map cg-vardecl (var.decls element))))
    ((if)       (cg-if (if.test element) (if.then element) (if.else element)))
    ((do)       (cg-do (do.body element) (do.test element)))
    ((while)    (cg-while (while.test element) (while.body element)))
    ((for-in)   ...)
    ((for)      (cg-for (for.e0 element)
			(for.e1 element)
			(for.e2 element)
			(for.body element)))
    ((continue) (cg-continue (continue.label element)))
    ((break)    (cg-break (break.label element)))
    ((return)   (cg-return (return.expr element)))
    ((throw)    (cg-throw (throw.expr element)))
    ((with)     (cg-with (with.expr element) (with.body element)))
    ((try)      (cg-try (try.try element) 
			(try.catchvar element)
			(try.catch element)
			(try.finally element)))
    ((switch)   (cg-switch (switch.expr element) (switch.cases element)))
    ((expr)     (cg-expr (expr.expr element)))
    (else ???)))

(define cg-block
  (lambda (stmts)
    (lambda (env)
      (do ((stmts stmts (cdr stmts)))
	  ((null? stmts))
	((car stmts) env)))))

(define cg-functionbody
  (lambda (stmts)
    (lambda (env)
      (call-with-current-continuation
       (lambda (r)
	 (let ((env (es-env-escape env 'return r)))
	   (do ((stmts stmts (cdr stmts)))
	       ((null? stmts))
	     ((car stmts) env))))))))

(define cg-var
  (lambda (decls)
    (lambda (env)
      (do ((decls decls (cdr decls)))
	  ((null? decls))
	((car decls) env)))))

(define cg-vardecl
  (lambda (d)
    (if (pair? d)
	(cg-assign (cg-var (car d)) (cg-expr (cdr d)))
	(cg-assign (cg-var (car d)) (lambda (env) (es-undefined))))))

(define cg-if
  (lambda (test consequent alternate)
    (lambda (env)
      (if (test env)
	  (consequent env)
	  (alternate env)))))

(define cg-do
  (lambda (body test)
    (lambda (env)
      (let continue-loop ()
	(if (call-with-current-continuation
	     (lambda (b/c)
	       (let ((env (es-env-escape (es-env-escape env 'continue b/c)
					 'break b/c)))
		 (let loop ()
		   (body env)
		   (if (test env) 
		       (loop)
		       #f)))))
	    (continue-loop))))))

(define cg-while
  (lambda (test body)
    (lambda (env)
      (let continue-loop ()
	(if (call-with-current-continuation
	     (lambda (b/c)
	       (let ((env (es-env-escape (es-env-escape env 'continue b/c)
					 'break b/c)))
		 (let loop ()
		   (if (test env) 
		       (begin (body env)
			      (loop))
		       #f)))))
	    (continue-loop))))))

(define cg-for-in
  (lambda (e0 e1 body)
    (lambda (env)
      ...)))

(define cg-for
  (lambda (e0 e1 e2 body)
    (lambda (env)
      (if e0
	  (e0 env))
      (let continue-loop ()
	(if (call-with-current-continuation
	     (lambda (b/c)
	       (let ((env (es-env-escape (es-env-escape env 'continue b/c)
					 'break b/c)))
		 (let loop ()
		   (if (if e1 (e1 env) #t)
		       (begin (body env)
			      (if e2 (e2 env))
			      (loop))
		       #f)))))
	    (continue-loop))))))

(define cg-continue
  (lambda (label)
    (lambda (env)
      ((es-find-escape env 'continue label) #t))))

(define cg-break
  (lambda (label)
    (lambda (env)
      ((es-find-escape env 'break label) #f))))

(define cg-return
  (lambda (expr)
    (lambda (env)
      (let ((v (if expr (expr env) (es-undefined))))
	((es-find-escape env 'return #f) v)))))

(define cg-throw
  (lambda (expr)
    (lambda (env)
      ((es-exception-handler env) (expr env)))))

(define cg-with
  (lambda (expr body)
    (lambda (env)
      (body (cons (es-toobject (expr env)) env)))))

(define cg-try
  (lambda (try catchvar catch finally)
    (lambda (env)
      (let ((e (call-with-current-continuation
		(lambda (eh)
		  (es-with-exception-handler env eh
		    (lambda ()
		      (try env)
		      'no-exn))))))
	(if (not (eq? e 'no-exn))
	    (if catchvar
		(let ((env (cons (let ((o (es-make-object)))
				   (es-put! o catchvar exn)
				   o)
				 env)))
		  (let ((e2 (call-with-current-continuation
			     (lambda (eh)
			       (es-with-exception-handler env eh
				 (lambda ()
				   (catch env)
				   'no-exn))))))
		    (if finally
			(finally env))
		    (if (not (eq? e2 'no-exn))
			((es-exception-handler env) e2))))
		(if finally
		    (finally env)))
	    (if finally
		(finally env)))))))

(define cg-switch
  (lambda (expr cases)
    (let* ((cases (map ... cases))  ; list of (value . code)
	   (default ...))           ; pointer into the list or #f
      (lambda (env)
	(let ((v (expr env)))
	  (call-with-current-continuation
	   (lambda (b)
	     (let ((env (es-env-escape env 'break b)))
	       (let loop ((cases cases))
		 (cond ((null? cases) 
			(if default 
			    (for-each (lambda (x) ((cdr x) env)) default)))
		       ((es=? v (caar cases))
			(for-each (lambda (x) ((cdr x) env)) cases))
		       (else
			(loop (cdr cases)))))))))))))

(define cg-expr
  (lambda (e)
    (case (node.type e)
      ((binop) 
       (let ((t1 (cg-expr (binop.lhs e)))
	     (t2 (cg-expr (binop.rhs e))))
	 (case (binop.op e)
	   ((add sub mul div mod bitand bitior bitxor bitshl bitshr bitshru
	     lt le gt ge eq eqv neq neqv in instanceof)
	    (let ((op (es-simple-binop (binop.op e))))
	      (lambda (env)
		(op (t1 env) (t2 env)))))
	   ((and)
	    (lambda (env)
	      (if (es-true? (t1 env))
		  (t2 env)
		  #f)))
	   ((or)
	    (lambda (env)
	      (if (es-true? (t1 env))
		  #t
		  (t2 env))))
	   ((assign)
	    (es-handle-assignment (binop.lhs e) t2))
	   ((add-assign sub-assign mul-assign div-assign mod-assign
	     bitand-assign bitior-assign bitxor-assign bitshl-assign
	     bitshr-assign bitshru-assign)
	    ; FIXME: this is wrong, it computes subexpressions on the
	    ; lhs twice.
	    (es-handle-assignment (binop.lhs e)
				  (let ((op (es-simple-binop (binop.op e))))
				    (lambda (env)
				      (op (t1 env) (t2 env))))))
	   (else ???))))
      ((unop) 
       (let ((t (cg-expr (unop.expr e))))
	 (case (unop.op e)
	   ((lognot bitnot add sub) 
	    (let ((op (es-simple-unop (unop.op e))))
	      (lambda (env)
		(op (t env)))))
	   ((preincrement predecrement postincrement postdecrement) 
	    ...)
	   ((void) 
	    (lambda (env)
	      (t env)
	      (es-undefined)))
	   ((delete) 
	    ...)
	   ((typeof)
	    (if (ident? (unop.expr e))
		(let ((n (ident.id (unop.expr e))))
		  (lambda (env)
		    (es-typeof (lookup-or-undefined env n))))
		(lambda (env)
		  (es-typeof (t env)))))
	   ((new) 
	    ...)
	   (else ???))))
      ((cond) 
       (let ((test (cg-expr (cond.test e)))
	     (consequent (cg-expr (cond.then e)))
	     (alternate (cg-expr (cond.else e))))
	 (lambda (env)
	   (if (test env)
	       (consequent env)
	       (alternate env)))))
      ((this) ...)
      ((aref) ...)
      ((propref) ...)
      ((ident) ...)
      (else
       ???))))

(define (es-handle-assignment lhs t2)
  (cond ((ident? lhs)
	 (let ((name (ident.id lhs)))
	   (lambda (env)
	     (env-set env name (t2 env)))))
	((aref? lhs)
	 ...)
	((propref? lhs)
	 ...)
	(else ???)))

(define (es-simple-binop op)
  (case op
    ((add add-assign) es+)
    ((sub sub-assign) es-)
    ((mul mul-assign) es*)
    ((div div-assign) es/)
    ((mod mod-assign) es%)
    ((bitand bitand-assign) es-bitand)
    ((bitior bitior-assign) es-bitior)
    ((bitxor bitxor-assign) es-bitxor)
    ((bitshl bitshl-assign) es-bitshl)
    ((bitshr bitshr-assign) es-bitshr)
    ((bitshru bitshru-assign) es-bitshru)
    ((lt) es<)
    ((le) es<=)
    ((gt) es>)
    ((ge) es>=)
    ((eqv) es==)
    ((eq) es===)
    ((neqv) es!=)
    ((neq) es!==)
    ((in) es-in)
    ((instanceof) es-instanceof)
    (else ???)))

(define (es-simple-unop op)
  ...)

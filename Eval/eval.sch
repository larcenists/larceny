; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny's interpreter.
;
; Description
;   `Interpret' takes an expression and an environment and evaluates the
;   expression in the environment.  It returns the result of the evaluation,
;   and may change the environment argument.
;
;   `Interpret' accepts full R4RS Scheme, but requires a procedure 
;   `macro-expand' that converts full Scheme to its core form with 
;   all constants quoted.
;
; Technique
;   The interpreter works by preprocessing the source code into a procedure
;   that takes a run-time environment as its only argument.  The procedure
;   in turn calls procedures representing subexpressions, and so on, so all
;   steps in the interpretation are translated into direct function calls,
;   with roughly one call per subexpression.  This technique strongly
;   resembles the use of threaded code.
;
;   By judiciously translating common cases specially and using some caching
;   (using local transformations only), much interpretive overhead is avoided.
;   A number of further optimizations are possible; see the comments.
;
;   Run-time lexical environments are lists of vectors, in inside-out lexical
;   order.
;
; Features Needed or Deeply Desired
;   Optimizations:
;   * More primitives (we need macros before this is practical).  Notably,
;     we need more predicates and both vector and string operations.  See
;     the file Interpret/evalprim.sch.
;
;   * If+predicate, for example, (if (< a b) ...) can be optimized to
;     use no calls.  (if (not (< a b)) ...) is more of a mess because
;     `not' is a procedure that may have been redefined, also, but it's
;     clearly doable.
;
;   * Immediate optimization in primitives: if a primitive uses a constant,
;     then avoid the call by just closing over a variable that uses the
;     constant and let the primitive reference that variable.
;
;   * Rib0 optimization in primitives: if a local is fetched in a primitive,
;     and that local is at rib0, then do the vector-ref in the primitive.
;
;   * `Let' optimization akin to letrec optimization.  This avoids calls,
;     but payoff is smaller because an intermediate structure must be built
;     to avoid problems with call/cc.
;
;   * `Eval:benchmark-mode' switch that makes the interpreter skip the
;     global cache check for primitive names.  It's a bit of a joke, but
;     it might improve performance some.
;
;   * We could unroll several cases for interpret/setlex just as for 
;     interpret/lexical.
;
;   * We could have a special case for calling a global variable: the
;     reference to the variable could be lifted out, saving a call to get it.
;
;   Niceties:
;   * In general, it would be nice to be able to give better error messages
;     when calling a procedure that was fetched from a variable, rather than
;     just "#f is not a procedure".
;
;   * Debuggability -- can the procedures be constructed so that they
;     contain the source code for the expression being evaluated?
;
; Bugs
;   It's possible that there should be some restrictions here for
;   certain environment arguments; for example, some environments
;   are immutable (cf the Report).  We can detect immutable arguments
;   at preprocessing time and generate code for setglbl that signals
;   an error if executed (and perhaps a warning during preprocessing).

($$trace "interpret")

; Macro expander parameterization

(define kwd:define 'define)
(define kwd:lambda 'lambda)
(define kwd:named-lambda '.named-lambda) ; Introduced by the interpreter
(define kwd:if 'if)
(define kwd:set! 'set!)
(define kwd:quote 'quote)
(define kwd:begin 'begin)

; End macro expander parameterization

(define interpret
  (let ()

    (define (definition? x)
      (and (pair? x) (eq? (car x) kwd:define)))

    (define (begin? x)
      (and (pair? x) (eq? (car x) kwd:begin)))

    (define (all-definitions? elist)
      (if (null? elist)
	  #t
	  (let ((expr (car elist)))
	    (or (and (definition? expr)
		     (all-definitions? (cdr elist)))
		(and (begin? expr)
		     (all-definitions? (cdr expr))
		     (all-definitions? (cdr elist)))))))

    (define (rewrite-begin-nest elist)
      (if (null? elist)
	  (list (unspecified))
	  (let ((expr (car elist)))
	    (cond ((definition? expr)
		   (cons `(,kwd:set! ,(cadr expr) ,(caddr expr))
			 (rewrite-begin-nest (cdr elist))))
		  ((begin? expr)
		   (append (rewrite-begin-nest (cdr expr))
			   (rewrite-begin-nest (cdr elist))))
		  (else
		   ???)))))

    (define (lambda->named-lambda name x)
      `(,kwd:named-lambda ,name ,@(cdr x)))

    (define (toplevel-preprocess expr env)
      (cond ((definition? expr)
	     (let ((lhs (cadr expr))
		   (rhs (caddr expr)))
	       (if (and (pair? rhs) (eq? kwd:lambda (car rhs)))
		   (really-preprocess
		    `(,kwd:begin
		      (,kwd:set! ,lhs ,(lambda->named-lambda lhs rhs))
		      ,(unspecified))
		    env)
		   (really-preprocess
		    `(,kwd:begin
		      (,kwd:set! ,lhs ,rhs)
		      ,(unspecified))
		    env))))
	    ((begin? expr)
	     (if (all-definitions? (cdr expr))
		 (really-preprocess (cons kwd:begin
					  (rewrite-begin-nest (cdr expr)))
				    env)
		 (really-preprocess expr env)))
	    (else
	     (really-preprocess expr env))))

    (define (really-preprocess expr env)
      (interpret/preprocess expr 
			    '() 
			    (lambda (sym)
			      (environment-lookup-binding env sym))))

    (define (interpret expr env)
      (let ((expr (interpreter-macro-expand expr env)))
	((toplevel-preprocess expr env) '())))

    interpret))

(define (interpret/preprocess expr env find-global)
  (cond ((symbol? expr)
	 (let ((address (interpret/var-address expr env)))
	   (if address
	       (interpret/lexical (car address) (cdr address) expr)
	       (interpret/global expr find-global expr))))
	((pair? expr)
	 (let ((kwd (car expr)))
	   (cond ((eq? kwd:quote kwd)
		  (interpret/const (cadr expr)))
		 ((eq? kwd:set! kwd)
		  (let ((address (interpret/var-address (cadr expr) env))
			(rhs
			 (interpret/preprocess (caddr expr) env find-global)))
		    (if address
			(interpret/setlex (car address) (cdr address) rhs expr)
			(interpret/setglbl (cadr expr) rhs find-global expr))))
		 ((eq? kwd:lambda kwd)
		  (interpret/make-proc expr env find-global expr))
		 ((eq? kwd:named-lambda kwd)
		  (interpret/make-proc expr env find-global expr))
		 ((eq? kwd:begin kwd)
		  (if (null? (cdr expr))
		      (begin (error "EVAL: empty BEGIN")
			     #t)
		      (interpret/sequence
		       (map (lambda (x)
			      (interpret/preprocess x env find-global))
			    (cdr expr))
		       expr)))
		 ((eq? kwd:if kwd)
		  (let ((test
			 (interpret/preprocess (cadr expr) env find-global))
			(*then
			 (interpret/preprocess (caddr expr) env find-global))
			(*else
			 (if (null? (cdddr expr))
			     (interpret/const (unspecified))
			     (interpret/preprocess (cadddr expr) env
						   find-global))))
		    (interpret/if test *then *else expr)))
		 (else
		  (interpret/make-call expr env find-global expr)))))
	((interpret/self-evaluating? expr)
	 (interpret/const expr))
	(else
	 (error "EVAL: preprocess: unknown expression: " expr)
	 #t)))

(define (interpret/self-evaluating? expr)
  (or (procedure? expr) 
      (bytevector-like? expr)
      (vector-like? expr)
      (boolean? expr)
      (number? expr)
      (char? expr)
      (null? expr)
      (eq? expr (unspecified))
      (eq? expr (undefined))
      (eof-object? expr)))

; Closure creation.  Special cases handled: 
;  - procedures of 0..4 arguments.
;  - varargs procedures.

(define (interpret/make-proc expr env find-global src)

  (define (named-lambda? x)
    (eq? (car x) kwd:named-lambda))

  (define (listify x)
    (cond ((null? x) x)
	  ((pair? x) (cons (car x) (listify (cdr x))))
	  (else (list x))))

  (define (fixed-args x n)
    (if (pair? x)
	(fixed-args (cdr x) (+ n 1))
	n))

  (let* ((args  (if (named-lambda? expr) (caddr expr) (cadr expr)))
	 (body  (if (named-lambda? expr) (cdddr expr) (cddr expr)))
	 (name  (if (named-lambda? expr) (cadr expr) #f))
	 (nenv  (interpret/extend-env env (listify args)))
	 (exprs (interpret/preprocess (cons kwd:begin body) nenv find-global)))
    (if (list? args)
	(case (length args)
	  ((0) (interpret/lambda0 exprs src name))
	  ((1) (interpret/lambda1 exprs src name))
	  ((2) (interpret/lambda2 exprs src name))
	  ((3) (interpret/lambda3 exprs src name))
	  ((4) (interpret/lambda4 exprs src name))
	  (else (interpret/lambda-n (length args) exprs src name)))
	(interpret/lambda-dot (fixed-args args 0) exprs src name))))

; Procedure call.  Special cases handled:
;  - letrec:  ((lambda (a b ...) ...) #!unspecified ...)
;  - primitive: (op a b ...)
;  - short: 0..4 arguments

(define (interpret/make-call expr env find-global src)

  (define (lambda? op)
    (and (pair? op) (kwd:lambda? (car op))))

  (define (let? op n)
    (and (lambda? op)
	 (list? (cadr op))
	 (= (length (cadr op)) n)))

  (define unspecd `',(unspecified))

  (define (letrec? op n args)
    (and (let? op n)
	 (not (null? (cadr op)))
	 (every? (lambda (v) (equal? v unspecd))
		 args)))

  (let* ((pexps (map (lambda (x)
		       (interpret/preprocess x env find-global))
		     expr))
	 (proc  (car pexps))
	 (args  (cdr pexps))
	 (n     (length args)))
    (cond ;((letrec? (car expr) n (cdr expr))
	  ; (interpret/invoke-letrec 
	  ; (interpret/preprocess (cons kwd:begin (cddar expr)) env find-global)
	  ; (length args)))
	  ((<= n 4)
	   (interpret/invoke-short proc args (car expr) n env find-global src))
	  (else
	   (interpret/invoke-n proc args src)))))

(define (interpret/extend-env env names)
  (cons names env))

(define (interpret/var-address name env)
  (let r-loop ((env env) (i 0))
    (if (null? env)
	#f
	(let a-loop ((rib (car env)) (j 0))
	  (cond ((null? rib)
		 (r-loop (cdr env) (+ i 1)))
		((eq? (car rib) name)
		 (cons i j))
		(else
		 (a-loop (cdr rib) (+ j 1))))))))

(define (interpret/global name find-global src)
  (let ((cell (find-global name)))
    (interpreted-expression
     (lambda (env)
       (let ((v (car cell)))
	 (if (eq? v (undefined))
	     (begin
	       (error "Reference to undefined global variable `" name "'.")
	       #t)
	     v)))
     src)))

; This never fails, so the source code is not recorded.

(define (interpret/setglbl name expr find-global src)
  (let ((cell (find-global name)))
    (lambda (env)
      (set-car! cell (expr env)))))

; Unroll loop for the closest ribs.
; We don't record the source because these are primitive expressions that
; never fail.

(define (interpret/lexical rib offset src)
  (case rib
    ((0) (interpret/lexical0 offset src))
    ((1) (interpret/lexical1 offset src))
    ((2) (interpret/lexical2 offset src))
    ((3) (interpret/lexical3 offset src))
    (else (interpret/lexical-n rib offset src))))

(define (interpret/lexical0 offset src)
  (lambda (env)
    (vector-ref (car env) offset)))

(define (interpret/lexical1 offset src)
  (lambda (env)
    (vector-ref (cadr env) offset)))

(define (interpret/lexical2 offset src)
  (lambda (env)
    (vector-ref (caddr env) offset)))

(define (interpret/lexical3 offset src)
  (lambda (env)
    (vector-ref (cadddr env) offset)))

(define (interpret/lexical-n rib offset src)
  (lambda (env0)
    (let loop ((rib rib) (env env0))
      (if (= rib 0)
	  (vector-ref (car env) offset)
	  (loop (- rib 1) (cdr env))))))

(define (interpret/setlex rib offset expr src)
  (lambda (env0)
    (let loop ((rib rib) (env env0))
      (if (= rib 0)
	  (vector-set! (car env) offset (expr env0))
	  (loop (- rib 1) (cdr env))))))

(define (interpret/const c)
  (lambda (env)
    c))

(define (interpret/if test consequent alternate src)
  (interpreted-expression
   (lambda (env)
     (if (test env) (consequent env) (alternate env)))
   src))

; Special cases: 1..4 expressions.

(define (interpret/sequence exprs src)
  (case (length exprs)
    ((1) (car exprs))
    ((2) (interpret/sequence2 (car exprs) (cadr exprs) src))
    ((3) (interpret/sequence3 (car exprs) (cadr exprs) (caddr exprs) src))
    ((4) (interpret/sequence4 (car exprs) (cadr exprs) (caddr exprs)
			      (cadddr exprs) src))
    (else (interpret/sequence-n exprs src))))

(define (interpret/sequence2 a b src)
  (interpreted-expression
   (lambda (env)
     (a env) (b env))
   src))

(define (interpret/sequence3 a b c src)
  (interpreted-expression
   (lambda (env)
     (a env) (b env) (c env))
   src))

(define (interpret/sequence4 a b c d src)
  (interpreted-expression
   (lambda (env)
     (a env) (b env) (c env) (d env))
   src))

(define (interpret/sequence-n exprs src)
  (interpreted-expression
   (lambda (env)
     (let loop ((exprs exprs))
       (cond ((null? (cdr exprs))
	      ((car exprs) env))
	     (else
	      ((car exprs) env)
	      (loop (cdr exprs))))))
   src))

(define (interpret/invoke-prim1 name a find-global src)
  ((interpret/primitive name 1) a (interpret/prim-orig name)
				(find-global name)))

(define (interpret/invoke-prim2 name a b find-global src)
  ((interpret/primitive name 2) a b (interpret/prim-orig name)
				(find-global name)))

; Call to a literal lambda expression where all the arguments are
; (quote #!unspecified).  Could be generalized to where all are the same
; constant, or indeed to the case where all arguments are constants.

(define (interpret/invoke-letrec lambda-body n)
  (lambda (env)
    (let ((env (cons (make-vector n (unspecified)) env)))
      ((lambda-body env)))))

; Calls that take 0..4 arguments.

(define (interpret/invoke-short proc args op n env find-global src)

  (define (prim?)
    (and (symbol? op)
	 (not (interpret/var-address op env))
	 (interpret/primitive? op n)))

  (case n
    ((0) (interpret/invoke0 proc src))
    ((1) (if (prim?)
	     (interpret/invoke-prim1 op (car args) find-global src)
	     (interpret/invoke1 proc (car args) src)))
    ((2) (if (prim?)
	     (interpret/invoke-prim2 op (car args) (cadr args) find-global src)
	     (interpret/invoke2 proc (car args) (cadr args) src)))
    ((3) (interpret/invoke3 proc (car args) (cadr args) (caddr args) src))
    ((4) (interpret/invoke4 proc (car args) (cadr args) (caddr args)
		       (cadddr args) src))
    (else ???)))

(define (interpret/invoke0 proc src)
  (interpreted-expression
   (lambda (env)
     ((proc env)))
   src))

(define (interpret/invoke1 proc a src)
  (interpreted-expression
   (lambda (env)
     ((proc env) (a env)))
   src))

(define (interpret/invoke2 proc a b src)
  (interpreted-expression
   (lambda (env)
     ((proc env) (a env) (b env)))
   src))

(define (interpret/invoke3 proc a b c src)
  (interpreted-expression
   (lambda (env)
     ((proc env) (a env) (b env) (c env)))
   src))

(define (interpret/invoke4 proc a b c d src)
  (interpreted-expression
   (lambda (env)
     ((proc env) (a env) (b env) (c env) (d env)))
   src))

(define (interpret/invoke-n proc args src)
  (interpreted-expression
   (lambda (env)
     (let ((proc (proc env))
	   (args (map (lambda (p) (p env)) args)))
       (apply proc args)))
   src))

; Closure creation.
;
; If 'vector' were faster, it would be a better choice for constructing 
; ribs than make-vector + vector-set!.

(define (interpret/lambda0 body src name)
  (let ((doc (vector name src 0 #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda ()
	  (body (cons '#() env)))))
     src)))

(define (interpret/lambda1 body src name)
  (let ((doc (vector name src 1 #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda (a)
	  (let ((v (make-vector 1 a)))
	    (body (cons v env))))))
     src)))

(define (interpret/lambda2 body src name)
  (let ((doc (vector name src 2 #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda (a b)
	  (let ((v (make-vector 2 a)))
	    (vector-set! v 1 b)
	    (body (cons v env))))))
     src)))

(define (interpret/lambda3 body src name)
  (let ((doc (vector name src 3 #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda (a b c)
	  (let ((v (make-vector 3 a)))
	    (vector-set! v 1 b)
	    (vector-set! v 2 c)
	    (body (cons v env))))))
     src)))

(define (interpret/lambda4 body src name)
  (let ((doc (vector name src 4 #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda (a b c d)
	  (let ((v (make-vector 4 a)))
	    (vector-set! v 1 b)
	    (vector-set! v 2 c)
	    (vector-set! v 3 d)
	    (body (cons v env))))))
     src)))

(define (interpret/lambda-n n body src name)
  (let ((doc (vector name src n #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda args
	  (body (cons (list->vector args) env)))))
     src)))

; `n' is the number of fixed arguments.

(define (interpret/lambda-dot n body src name)
  (let ((doc (vector name src (exact->inexact n) #f #f)))
    (interpreted-expression
     (lambda (env)
       (interpreted-procedure
	doc
	(lambda args
	  (let ((l (length args))
		(v (make-vector (+ n 1) (unspecified))))
	    (if (< l n)
		(error "Too few arguments to procedure."))
	    (do ((args args (cdr args))
		 (i 0 (+ i 1)))
		((= i n)
		 (vector-set! v i args)
		 (body (cons v env)))
	      (vector-set! v i (car args)))))))
     src)))


; Debugger support

; Interpreted-procedure, interpreted-expression, and
; interpreted-primitive can always return their procedure argument
; without any harm to the interpreter; only debugging will be affected.

; Interpreted-procedure takes a standard documentation structure and any
; procedure (currently anything) and returns a new procedure that is
; identical to the old except that the it has typetag 0 and is one
; element longer.  The new, last element contains the pair 
; ($eval-lambda . <doc>) where <doc> is the documentation structure.
; This procedure is on the critical path in the interpreter and should
; do no more work than absolutely necessary.

(define (interpreted-procedure doc proc)
  (let* ((l (procedure-length proc))
	 (p (make-procedure (+ l 1))))
    (do ((i 0 (+ i 1)))
	((= i l))
      (procedure-set! p i (procedure-ref proc i)))
    (procedure-set! p l (cons '$eval-lambda doc))
    (typetag-set! p 0)
    p))

(define (interpreted-procedure? x)
  (and (procedure? x)
       (zero? (typetag x))
       (let ((last (procedure-ref x (- (procedure-length x) 1))))
	 (and (pair? last)
	      (eq? (car last) '$eval-lambda)))))

(define (interpreted-procedure-documentation proc)
  (cdr (procedure-ref proc (- (procedure-length proc) 1))))


; Interpreted-expression takes any procedure and the source code
; and returns a new procedure that is identical to the old except 
; that the it is one element longer and has typetag 0.  
;
; The new, last element contains the pair ($evalproc . <doc>) where <doc>
; is the source code.

(define (interpreted-expression proc doc)
  (let* ((l (procedure-length proc))
	 (p (make-procedure (+ l 1))))
    (do ((i 0 (+ i 1)))
	((= i l))
      (procedure-set! p i (procedure-ref proc i)))
    (procedure-set! p l (cons '$evalproc doc))
    (typetag-set! p 0)
    p))

(define (interpreted-expression? x)
  (and (procedure? x)
       (zero? (typetag x))
       (let ((last (procedure-ref x (- (procedure-length x) 1))))
	 (and (pair? last)
	      (eq? (car last) '$evalproc)))))


; Interpreted-primitive takes a name (a symbol), a number of arguments
; (a fixnum), and a procedure, and returns a new procedure that is
; identical to the old except that it is one element longer and has
; typetag 0.  
; The new, last element contains the list ($evalprim <name> <argc>).

(define (interpreted-primitive name argc proc)
  (let* ((l (procedure-length proc))
	 (p (make-procedure (+ l 1))))
    (do ((i 0 (+ i 1)))
	((= i l))
      (procedure-set! p i (procedure-ref proc i)))
    (procedure-set! p l (list '$evalprim name argc))
    (typetag-set! p 0)
    p))

(define (interpreted-primitive? x)
  (and (procedure? x)
       (zero? (typetag x))
       (let ((last (procedure-ref x (- (procedure-length x) 1))))
	 (and (pair? last)
	      (eq? (car last) '$evalprim)))))


; Augments the definition in Lib/procinfo.sch to deal with interpreted
; procedures.

(define procedure-documentation 
  (let ((procedure-documentation procedure-documentation))
    (lambda (proc . rest)
      (cond ((interpreted-procedure? proc)
	     (interpreted-procedure-documentation proc))
	    ((interpreted-expression? proc)
	     (vector #f (interpreted-procedure-documentation proc)))
	    ((interpreted-primitive? proc)
	     (let ((x (interpreted-procedure-documentation proc)))
	       (vector (car x) #f (cadr x))))
	    (else
	     (apply procedure-documentation proc rest))))))

; eof

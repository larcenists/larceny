(define (es-syntax-error . terms)
  (apply error "Syntax error: " terms))

; Abstract syntax trees

(define node.type car)

(define (make-block-statement stmts) (list 'block stmts))
(define block.stmts cadr)

(define (make-var-statement decls) (list 'var decls))
(define var.decls cadr)

(define (make-if-statement test consequent alternate) 
  (list 'if test consequent alternate))
(define if.test cadr)
(define if.then caddr)
(define if.else cadddr)

(define (make-do-statement body test) (list 'do body test))
(define do.body cadr)
(define do.test caddr)

(define (make-while-statement test body) (list 'while test body))
(define while.test cadr)
(define while.body caddr)

(define (make-for-in-statement var? e0 e1 body) (list 'for-in var? e0 e1 body))
(define for-in.var? cadr)
(define for-in.e0 caddr)
(define for-in.e1 cadddr)
(define for-in.body (lambda (x) (car (cddddr x))))

(define (make-for-statement var? e0 e1 e2 body) (list 'for var? e0 e1 e2 body))
(define for.var? cadr)
(define for.e0 caddr)
(define for.e1 cadddr)
(define for.e2 (lambda (x) (car (cddddr x))))
(define for.body (lambda (x) (cadr (cddddr x))))

(define (make-continue-statement label) (list 'continue label))
(define continue.label cadr)

(define (make-break-statement label) (list 'continue label))
(define break.label cadr)

(define (make-return-statement expr) (list 'return expr))
(define return.expr cadr)

(define (make-throw-statement expr) (list 'throw expr))
(define throw.expr cadr)

(define (make-with-statement expr body) (list 'with expr body))
(define with.expr cadr)
(define with.body caddr)

(define (make-try-statement try catchvar catch finally) 
  (list 'try try catchvar catch finally))
(define try.try cadr)
(define try.catchvar caddr)
(define try.catch cadddr)
(define try.finally (lambda (x) (car (cddddr x))))

(define (make-switch-statement expr cases) (list 'switch expr cases))
(define switch.expr cadr)
(define switch.cases caddr)

(define (make-expr-statement expr) (list 'expr expr))
(define expr.expr cadr)

(define (make-cond e0 e1 e2) (list 'cond e0 e1 e2))
(define cond.test cadr)
(define cond.then caddr)
(define cond.else cadddr)
 
(define (make-binop op lhs rhs) (list 'binop op lhs rhs))
(define binop.op cadr)
(define binop.lhs caddr)
(define binop.rhs cadddr)

(define (make-unop op e) (list 'unop op e))
(define unop.op cadr)
(define unop.expr caddr)


; Run-time object representation
;
; ECMAScript   Scheme
; ----------   ------
; true         #t
; false        #f
; null         ()
; undefined    undefined (the symbol)
; number       number
; string       string
; object       (props proto)
; function     (props proto call construct)
;
; Props is an assoc list where the keys are symbols or fixnums.
; Proto is #f or another object
; 
; Open question: how host objects fit into this
; Open question: how the "arguments" object shares with activation record
;
; Possibly use an escape mechanism: if props is #f then the object
; looks different.

(define (es-make-object . rest)
  (if (null? rest)
      (list '() #f)
      (list '() (car rest))))


; Run-time environments
;
; env ::= (envnode ...)
; envnode ::= obj
;           | (return k)
;           | (break k labels)
;           | (continue k labels)
;
; Open question: global (outermost) environments.  Actually these will
; typically be host objects or at least special case.

(define (es-env-escape env esc k)
  (cons (cons esc k) env))

(define (es-find-escape env esc label)
  (cond ((null? env) ???)
	((eq? (caar env) esc)
	 (if (or (not label)
		 (eq? label (cadar env)))
	     (case esc
	       ((return) (return cadar env))
	       ((continue) (return caddar env))
	       ((break) (return caddar env))
	       (else ???))))
	(else
	 (es-find-escape (cdr env) esc label))))

(define (es-exception-handler env) ...)

(define (es-with-exception-handler env eh thunk) ...)




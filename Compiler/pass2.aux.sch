; Copyright 1991 William D Clinger.
;
; $Id$
;
; 14 September 2000.
;
; Procedures for fetching and clobbering parts of expressions.

($$trace "pass2.aux")

(define (constant? exp) (eq? (car exp) 'quote))
(define (variable? exp)
  (and (eq? (car exp) 'begin)
       (null? (cddr exp))))
(define (lambda? exp) (eq? (car exp) 'lambda))
(define (call? exp) (pair? (car exp)))
(define (assignment? exp) (eq? (car exp) 'set!))
(define (conditional? exp) (eq? (car exp) 'if))
(define (begin? exp)
  (and (eq? (car exp) 'begin)
       (not (null? (cddr exp)))))

(define (make-constant value) (list 'quote value))
(define (make-variable name) (list 'begin name))
(define (make-lambda formals defs R F G decls doc body)
  (list 'lambda
        formals
        (cons 'begin defs)
        (list 'quote (list R F G decls doc))
        body))
(define (make-call proc args) (cons proc (append args '())))
(define (make-assignment lhs rhs) (list 'set! lhs rhs))
(define (make-conditional e0 e1 e2) (list 'if e0 e1 e2))
(define (make-begin exprs)
  (if (null? (cdr exprs))
      (car exprs)
      (cons 'begin (append exprs '()))))
(define (make-definition lhs rhs) (list 'define lhs rhs))

(define (constant.value exp) (cadr exp))
(define (variable.name exp) (cadr exp))
(define (lambda.args exp) (cadr exp))
(define (lambda.defs exp) (cdr (caddr exp)))
(define (lambda.R exp) (car (cadr (cadddr exp))))
(define (lambda.F exp) (cadr (cadr (cadddr exp))))
(define (lambda.G exp) (caddr (cadr (cadddr exp))))
(define (lambda.decls exp) (cadddr (cadr (cadddr exp))))
(define (lambda.doc exp) (car (cddddr (cadr (cadddr exp)))))
(define (lambda.body exp) (car (cddddr exp)))
(define (call.proc exp) (car exp))
(define (call.args exp) (cdr exp))
(define (assignment.lhs exp) (cadr exp))
(define (assignment.rhs exp) (caddr exp))
(define (if.test exp) (cadr exp))
(define (if.then exp) (caddr exp))
(define (if.else exp) (cadddr exp))
(define (begin.exprs exp) (cdr exp))
(define (def.lhs exp) (cadr exp))
(define (def.rhs exp) (caddr exp))

(define (variable-set! exp newexp)
  (set-car! exp (car newexp))
  (set-cdr! exp (append (cdr newexp) '())))
(define (lambda.args-set! exp args) (set-car! (cdr exp) args))
(define (lambda.defs-set! exp defs) (set-cdr! (caddr exp) defs))
(define (lambda.R-set! exp R) (set-car! (cadr (cadddr exp)) R))
(define (lambda.F-set! exp F) (set-car! (cdr (cadr (cadddr exp))) F))
(define (lambda.G-set! exp G) (set-car! (cddr (cadr (cadddr exp))) G))
(define (lambda.decls-set! exp decls) (set-car! (cdddr (cadr (cadddr exp))) decls))
(define (lambda.doc-set! exp doc) (set-car! (cddddr (cadr (cadddr exp))) doc))
(define (lambda.body-set! exp exp0) (set-car! (cddddr exp) exp0))
(define (call.proc-set! exp exp0) (set-car! exp exp0))
(define (call.args-set! exp exprs) (set-cdr! exp exprs))
(define (assignment.rhs-set! exp exp0) (set-car! (cddr exp) exp0))
(define (if.test-set! exp exp0) (set-car! (cdr exp) exp0))
(define (if.then-set! exp exp0) (set-car! (cddr exp) exp0))
(define (if.else-set! exp exp0) (set-car! (cdddr exp) exp0))
(define (begin.exprs-set! exp exprs) (set-cdr! exp exprs))

(define expression-set! variable-set!)  ; used only by pass 3

; FIXME:  This duplicates information in Lib/procinfo.sch.

(define (make-doc name arity formals source-code filename filepos)
  (vector name source-code arity filename filepos formals))
(define (doc.name d)    (vector-ref d 0))
(define (doc.code d)    (vector-ref d 1))
(define (doc.arity d)   (vector-ref d 2))
(define (doc.file d)    (vector-ref d 3))
(define (doc.filepos d) (vector-ref d 4))
(define (doc.formals d) (vector-ref d 5))
(define (doc.name-set! d x)    (if d (vector-set! d 0 x)))
(define (doc.code-set! d x)    (if d (vector-set! d 1 x)))
(define (doc.arity-set! d x)   (if d (vector-set! d 2 x)))
(define (doc.file-set! d x)    (if d (vector-set! d 3 x)))
(define (doc.filepos-set! d x) (if d (vector-set! d 4 x)))
(define (doc.formals-set! d x) (if d (vector-set! d 5 x)))
(define (doc-copy d) (list->vector (vector->list d)))

(define (ignored? name) (eq? name name:IGNORED))

; Fairly harmless bug: rest arguments aren't getting flagged.

(define (flag-as-ignored name L)
  (define (loop name formals)
    (cond ((null? formals)
           ;(pass2-error p2error:violation-of-invariant name formals)
           #t)
          ((symbol? formals) #t)
          ((eq? name (car formals))
           (set-car! formals name:IGNORED)
           (if (not (local? (lambda.R L) name:IGNORED))
               (lambda.R-set! L
                 (cons (make-R-entry name:IGNORED '() '() '())
                       (lambda.R L)))))
          (else (loop name (cdr formals)))))
  (loop name (lambda.args L)))

(define (make-null-terminated formals)
  (cond ((null? formals) '())
        ((symbol? formals) (list formals))
        (else (cons (car formals)
                    (make-null-terminated (cdr formals))))))

(define (list-head x n)
  (cond ((zero? n) '())
        (else (cons (car x) (list-head (cdr x) (- n 1))))))

(define (remq x y)
  (cond ((null? y) '())
        ((eq? x (car y)) (remq x (cdr y)))
        (else (cons (car y) (remq x (cdr y))))))

(define (make-call-to-LIST args)
  (cond ((null? args) (make-constant '()))
        ((null? (cdr args))
         (make-call (make-variable name:CONS)
                    (list (car args) (make-constant '()))))
        (else (make-call (make-variable name:LIST) args))))

(define (pass2-error i . etc)
  (apply cerror (cons (vector-ref pass2-error-messages i) etc)))

(define pass2-error-messages
  '#("System error: violation of an invariant in pass 2 "
     "Wrong number of arguments to known procedure "))

(define p2error:violation-of-invariant 0)
(define p2error:wna 1)

; Procedures for fetching referencing information from R-tables.

(define (make-R-entry name refs assigns calls)
  (list name refs assigns calls))

(define (R-entry.name x) (car x))
(define (R-entry.references x) (cadr x))
(define (R-entry.assignments x) (caddr x))
(define (R-entry.calls x) (cadddr x))

(define (R-entry.references-set! x refs) (set-car! (cdr x) refs))
(define (R-entry.assignments-set! x assignments) (set-car! (cddr x) assignments))
(define (R-entry.calls-set! x calls) (set-car! (cdddr x) calls))

(define (local? R I)
  (assq I R))

(define (R-entry R I)
  (assq I R))

(define (R-lookup R I)
  (or (assq I R)
      (pass2-error p2error:violation-of-invariant R I)))

(define (references R I)
  (cadr (R-lookup R I)))

(define (assignments R I)
  (caddr (R-lookup R I)))

(define (calls R I)
  (cadddr (R-lookup R I)))

(define (references-set! R I X)
  (set-car! (cdr (R-lookup R I)) X))

(define (assignments-set! R I X)
  (set-car! (cddr (R-lookup R I)) X))

(define (calls-set! R I X)
  (set-car! (cdddr (R-lookup R I)) X))

; A notepad is a vector of the form #(L0 (L1 ...) (L2 ...) (I ...)),
; where the components are:
;    element 0: a parent lambda expression (or #f if there is no enclosing
;               parent, or we want to pretend that there isn't).
;    element 1: a list of lambda expressions that the parent lambda
;               expression encloses immediately.
;    element 2: a subset of that list that does not escape.
;    element 3: a list of free variables.

(define (make-notepad L)
  (vector L '() '() '()))

(define (notepad.parent np)      (vector-ref np 0))
(define (notepad.lambdas np)     (vector-ref np 1))
(define (notepad.nonescaping np) (vector-ref np 2))
(define (notepad.vars np)        (vector-ref np 3))

(define (notepad.lambdas-set! np x)     (vector-set! np 1 x))
(define (notepad.nonescaping-set! np x) (vector-set! np 2 x))
(define (notepad.vars-set! np x)        (vector-set! np 3 x))

(define (notepad-lambda-add! np L)
  (notepad.lambdas-set! np (cons L (notepad.lambdas np))))

(define (notepad-nonescaping-add! np L)
  (notepad.nonescaping-set! np (cons L (notepad.nonescaping np))))

(define (notepad-var-add! np I)
  (let ((vars (notepad.vars np)))
    (if (not (memq I vars))
        (notepad.vars-set! np (cons I vars)))))

; Given a notepad, returns the list of variables that are closed
; over by some nested lambda expression that escapes.

(define (notepad-captured-variables np)
  (let ((nonescaping (notepad.nonescaping np)))
    (apply-union
     (map (lambda (L)
            (if (memq L nonescaping)
                (lambda.G L)
                (lambda.F L)))
          (notepad.lambdas np)))))

; Given a notepad, returns a list of free variables computed
; as the union of the immediate free variables with the free
; variables of nested lambda expressions.

(define (notepad-free-variables np)
  (do ((lambdas (notepad.lambdas np) (cdr lambdas))
       (fv (notepad.vars np)
           (let ((L (car lambdas)))
             (union (difference (lambda.F L)
                                (make-null-terminated (lambda.args L)))
                    fv))))
      ((null? lambdas) fv)))

; Copyright 1991 William D Clinger.
;
; $Id$
;
; 14 September 2000.
;
; Procedures for fetching and clobbering parts of expressions.

($$trace "pass2.aux")

(define code-object? #f)

(define constant? #f)
(define variable? #f)
(define lambda? #f)
(define call? #f)
(define assignment? #f)
(define conditional? #f)
(define begin? #f)
(define definition? #f)
(define make-constant #f)
(define make-variable #f)
(define make-lambda #f)
(define make-call #f)
(define make-assignment #f)
(define make-conditional #f)
(define make-begin #f)
(define make-definition #f)

(define constant.value #f)
(define variable.name #f)
(define lambda.args #f)
(define lambda.defs #f)
(define lambda.R #f)
(define lambda.F #f)
(define lambda.G #f)
(define lambda.decls #f)
(define lambda.doc #f)
(define lambda.body #f)
(define call.proc #f)
(define call.args #f)
(define assignment.lhs #f)
(define assignment.rhs #f)
(define if.test #f)
(define if.then #f)
(define if.else #f)
(define begin.exprs #f)
(define def.lhs #f)
(define def.rhs #f)

(define variable-set! #f)
(define assignment-set! #f)
(define lambda.args-set! #f)
(define lambda.defs-set! #f)
(define lambda.R-set! #f)
(define lambda.F-set! #f)
(define lambda.G-set! #f)
(define lambda.decls-set! #f)
(define lambda.doc-set! #f)
(define lambda.body-set! #f)
(define call.proc-set! #f)
(define call.args-set! #f)
(define assignment.rhs-set! #f)
(define if.test-set! #f)
(define if.then-set! #f)
(define if.else-set! #f)
(define begin.exprs-set! #f)

(let ((assignment-tag  (list "assignment"))
      (begin-tag       (list "begin"))
      (call-tag        (list "call"))
      (conditional-tag (list "conditional"))
      (constant-tag    (list "constant"))
      (definition-tag  (list "definition"))
      (lambda-tag      (list "lambda"))
      (variable-tag    (list "variable")))

  (define (%make-call procedure arguments)
    (vector call-tag (vector procedure (append arguments '()))))

  (define %call.proc-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 0 new-value)))
  (define %call.args-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 1 new-value)))

  (define %make-conditional
    (lambda (predicate consequent alternative)
      (vector conditional-tag (vector predicate consequent alternative))))

  (define %if.test-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 0 new-value)))
  (define %if.then-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 1 new-value)))
  (define %if.else-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 2 new-value)))

  (set! code-object?
        (lambda (object)
          (and (vector? object)
               (= (vector-length object) 2)
               (let ((tag (vector-ref object 0)))
                 (cond ((eq? tag assignment-tag) #t)
                       ((eq? tag begin-tag) #t)
                       ((eq? tag call-tag) #t)
                       ((eq? tag conditional-tag) #t)
                       ((eq? tag constant-tag) #t)
                       ((eq? tag definition-tag) #t)
                       ((eq? tag lambda-tag) #t)
                       ((eq? tag variable-tag) #t)
                       (else #f))))))

  (set! make-constant (lambda (value)
                        (vector constant-tag (vector value))))

  (set! constant? (lambda (object)
                    (eq? (vector-ref object 0) constant-tag)))

  (set! constant.value (lambda (object) (vector-ref (vector-ref object 1) 0)))

  (set! make-variable (lambda (name)
                        (if (symbol? name)
                            (vector variable-tag (vector name))
                            (error "Bad variable name" name))))

  (set! variable? (lambda (object)
                    (eq? (vector-ref object 0) variable-tag)))

  (set! variable.name
        (lambda (object)
          (vector-ref (vector-ref object 1) 0)))

  (set! variable-set!
        (lambda (exp newexp)
          (vector-set! exp 0 (vector-ref newexp 0));; clobber tag
          (vector-set! exp 1 (list->vector (vector->list (vector-ref newexp 1))))))

  (set! make-lambda (lambda (formals defs R F G decls doc body)
                      (vector lambda-tag (vector formals defs R F G decls doc body))))

  (set! lambda? (lambda (object)
                  (eq? (vector-ref object 0) lambda-tag)))

  (set! lambda.args  (lambda (object) (vector-ref (vector-ref object 1) 0)))
  (set! lambda.defs  (lambda (object) (vector-ref (vector-ref object 1) 1)))
  (set! lambda.R     (lambda (object) (vector-ref (vector-ref object 1) 2)))
  (set! lambda.F     (lambda (object) (vector-ref (vector-ref object 1) 3)))
  (set! lambda.G     (lambda (object) (vector-ref (vector-ref object 1) 4)))
  (set! lambda.decls (lambda (object) (vector-ref (vector-ref object 1) 5)))
  (set! lambda.doc   (lambda (object) (vector-ref (vector-ref object 1) 6)))
  (set! lambda.body  (lambda (object) (vector-ref (vector-ref object 1) 7)))

  (set! lambda.args-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 0 new-value)))
  (set! lambda.defs-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 1 new-value)))
  (set! lambda.R-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 2 new-value)))
  (set! lambda.F-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 3 new-value)))
  (set! lambda.G-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 4 new-value)))
  (set! lambda.decls-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 5 new-value)))
  (set! lambda.doc-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 6 new-value)))
  (set! lambda.body-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 7 new-value)))

  ;; Calls
  (set! call.proc (lambda (object) (vector-ref (vector-ref object 1) 0)))
  (set! call.args (lambda (object) (vector-ref (vector-ref object 1) 1)))

  (set! make-call (lambda (procedure arguments)
                    (if (and (code-object? procedure)
                             (every? code-object? arguments))
                        (%make-call procedure arguments)
                        (error "Bad arguments to make-call." (list procedure arguments)))))

  (set! call? (lambda (object)
                (eq? (vector-ref object 0) call-tag)))

  (set! call.proc-set! (lambda (object new-value)
                         (if (code-object? new-value)
                             (%call.proc-set! object new-value)
                             (error "Bad proc in call.proc-set!" new-value))))

  (set! call.args-set! (lambda (object new-value)
                         (if (every? code-object? new-value)
                             (%call.args-set! object new-value)
                             (error "Bad args in call.args-set!" new-value))))

  ;; Assignments
  (set! make-assignment (lambda (lhs rhs)
                          (vector assignment-tag (vector lhs rhs))))

  (set! assignment? (lambda (object)
                      (eq? (vector-ref object 0) assignment-tag)))

  (set! assignment-set! (lambda (exp newexp)
                        (vector-set! exp 0 (vector-ref newexp 0)) ;; clobber tag
                        (vector-set! exp 1 (list->vector (vector->list (vector-ref newexp 1))))))

  (set! assignment.lhs (lambda (object) (vector-ref (vector-ref object 1) 0)))
  (set! assignment.rhs (lambda (object) (vector-ref (vector-ref object 1) 1)))
  (set! assignment.rhs-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 1 new-value)))

  ;; Conditionals
  (set! conditional? (lambda (object)
                       (eq? (vector-ref object 0) conditional-tag)))



  (set! make-conditional
        (lambda (predicate consequent alternative)
          (if (and (code-object? predicate)
                   (code-object? consequent)
                   (code-object? alternative))
              (%make-conditional predicate consequent alternative)
              (error "Bad arguments to make-conditional" (list predicate consequent alternative)))))

  (set! if.test (lambda (object) (vector-ref (vector-ref object 1) 0)))
  (set! if.then (lambda (object) (vector-ref (vector-ref object 1) 1)))
  (set! if.else (lambda (object) (vector-ref (vector-ref object 1) 2)))

  (set! if.test-set! (lambda (object new-value)
                       (if (code-object? new-value)
                           (%if.test-set! object new-value)
                           (error "Bad argument to if.test-set!" new-value))))
  (set! if.then-set! (lambda (object new-value)
                       (if (code-object? new-value)
                           (%if.then-set! object new-value)
                           (error "Bad argument to if.test-set!" new-value))))

  (set! if.else-set! (lambda (object new-value)
                       (if (code-object? new-value)
                           (%if.else-set! object new-value)
                           (error "Bad argument to if.test-set!" new-value))))

  ;; Begin
  (set! make-begin (lambda (exprs)
                     (cond ((pair? exprs) (if (null? (cdr exprs))
                                              (car exprs)
                                              (vector begin-tag (vector exprs))))
                           (else (error "Bad begin.")))))

  (set! begin? (lambda (object)
                 (eq? (vector-ref object 0) begin-tag)))

  (set! begin.exprs (lambda (object) (vector-ref (vector-ref object 1) 0)))
  (set! begin.exprs-set! (lambda (object new-value) (vector-set! (vector-ref object 1) 0 new-value)))

  (set! make-definition (lambda (lhs rhs)
                          (vector definition-tag (vector lhs rhs))))

  (set! definition? (lambda (object)
                      (eq? (vector-ref object 0) definition-tag)))

  (set! def.lhs (lambda (object) (vector-ref (vector-ref object 1) 0)))
  (set! def.rhs (lambda (object) (vector-ref (vector-ref object 1) 1)))
)

;(define (constant? exp) (eq? (car exp) 'quote))
;(define (variable? exp)
;  (and (eq? (car exp) 'begin)
;       (null? (cddr exp))))
;(define (lambda? exp) (eq? (car exp) 'lambda))
;(define (call? exp) (pair? (car exp)))
;(define (assignment? exp) (eq? (car exp) 'set!))
;(define (conditional? exp) (eq? (car exp) 'if))
;(define (begin? exp)
;  (and (eq? (car exp) 'begin)
;       (not (null? (cddr exp)))))

;(define (make-constant value) (list 'quote value))
;(define (make-variable name) (list 'begin name))
;(define (make-lambda formals defs R F G decls doc body)
;  (list 'lambda
;        formals
;        (cons 'begin defs)
;        (list 'quote (list R F G decls doc))
;        body))
;(define (make-call proc args) (cons proc (append args '())))
;(define (make-assignment lhs rhs) (list 'set! lhs rhs))
;(define (make-conditional e0 e1 e2) (list 'if e0 e1 e2))
;(define (make-begin exprs)
;  (if (null? (cdr exprs))
;      (car exprs)
;      (cons 'begin (append exprs '()))))
;(define (make-definition lhs rhs) (list 'define lhs rhs))

;(define (constant.value exp) (cadr exp))
;(define (variable.name exp) (cadr exp))
;(define (lambda.args exp) (cadr exp))
;(define (lambda.defs exp) (cdr (caddr exp)))
;(define (lambda.R exp) (car (cadr (cadddr exp))))
;(define (lambda.F exp) (cadr (cadr (cadddr exp))))
;(define (lambda.G exp) (caddr (cadr (cadddr exp))))
;(define (lambda.decls exp) (cadddr (cadr (cadddr exp))))
;(define (lambda.doc exp) (car (cddddr (cadr (cadddr exp)))))
;(define (lambda.body exp) (car (cddddr exp)))
;(define (call.proc exp) (car exp))
;(define (call.args exp) (cdr exp))
;(define (assignment.lhs exp) (cadr exp))
;(define (assignment.rhs exp) (caddr exp))
;(define (if.test exp) (cadr exp))
;(define (if.then exp) (caddr exp))
;(define (if.else exp) (cadddr exp))
;(define (begin.exprs exp) (cdr exp))
;(define (def.lhs exp) (cadr exp))
;(define (def.rhs exp) (caddr exp))

;(define (variable-set! exp newexp)
;  (set-car! exp (car newexp))
;  (set-cdr! exp (append (cdr newexp) '())))
;(define (lambda.args-set! exp args) (set-car! (cdr exp) args))
;(define (lambda.defs-set! exp defs) (set-cdr! (caddr exp) defs))
;(define (lambda.R-set! exp R) (set-car! (cadr (cadddr exp)) R))
;(define (lambda.F-set! exp F) (set-car! (cdr (cadr (cadddr exp))) F))
;(define (lambda.G-set! exp G) (set-car! (cddr (cadr (cadddr exp))) G))
;(define (lambda.decls-set! exp decls) (set-car! (cdddr (cadr (cadddr exp))) decls))
;(define (lambda.doc-set! exp doc) (set-car! (cddddr (cadr (cadddr exp))) doc))
;(define (lambda.body-set! exp exp0) (set-car! (cddddr exp) exp0))
;(define (call.proc-set! exp exp0) (set-car! exp exp0))
;(define (call.args-set! exp exprs) (set-cdr! exp exprs))
;(define (assignment.rhs-set! exp exp0) (set-car! (cddr exp) exp0))
;(define (if.test-set! exp exp0) (set-car! (cdr exp) exp0))
;(define (if.then-set! exp exp0) (set-car! (cddr exp) exp0))
;(define (if.else-set! exp exp0) (set-car! (cdddr exp) exp0))
;(define (begin.exprs-set! exp exprs) (set-cdr! exp exprs))

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

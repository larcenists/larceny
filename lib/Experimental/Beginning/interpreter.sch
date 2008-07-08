; A simple interpreter for the beginning student language.
;
; Uses ERR5RS records.
; Uses R6RS hashtables.
; Uses the following Larceny extensions:
;     (unspecified)

; Given a list of definitions and/or expressions,
; returns a list of results for the expressions.
; Assumes the syntax of the program is correct.
;
; Side effect: calls the stepper.

(define (interpret-beginning-program pgm)
  (interpret-beginning-program-loop pgm
                                    pgm
                                    (make-hashtable symbol-hash eq?)
                                    '()))

(define (interpret-beginning-program-loop pgm0 pgm globals results)
  (define (beginning-definition? def/exp)
    (and (pair? def/exp)
         (or (eq? 'define (car def/exp))
             (eq? 'define-struct (car def/exp)))))
  (cond ((null? pgm)
         (reverse results))
        ((beginning-definition? (car pgm))
         (interpret-beginning-definition (car pgm) globals)
         (interpret-beginning-program-loop pgm0 (cdr pgm) globals results))
        (else
         (interpret-beginning-expression (car pgm)
                                         globals
                                         (make-program-cont pgm0
                                                            (cdr pgm)
                                                            globals
                                                            results)))))

(define (interpret-beginning-definition def globals)
  (cond ((eq? 'define (car def))
         (cond ((list? (cadr def))
                (hashtable-set! globals
                                (car (cadr def))
                                (make-beginning-closure
                                 (car (cadr def))
                                 (cdr (cadr def))
                                 (caddr def)
                                 (beginning-env globals))))
               ((and (pair? (caddr def))
                     (eq? 'lambda (car (caddr def))))
                (interpret-beginning-definition
                 (list 'define
                       (cons (cadr def) (cadr (caddr def)))
                       (caddr (caddr def)))
                 globals))
               (else
                (let* ((cont (make-defn-cont globals (cadr def)))
                       (result
                        (interpret-beginning-expression (caddr def)
                                                        globals
                                                        cont)))
                  (hashtable-set! globals (cadr def) result)))))
        (else
         ; FIXME
         (beginning:error "not yet implemented" def))))

(define (interpret-beginning-expression exp env cont)
  (evaluate-beginning-expression exp env cont))

(define (evaluate-beginning-expression exp env cont)
  (step-beginning-expression exp env cont))

(define (step-beginning-expression exp env cont)
  (cond ((pair? exp)
         (case (car exp)
          ((cond)
           (let* ((clauses (cdr exp))
                  (clause1 (car clauses))
                  (test (car clause1))
                  (test (if (eq? 'else test) 'true test))
                  (exp1 (cadr clause1))
                  (exp2 (if (null? (cdr clauses))
                            ; FIXME
                            "ran out of clauses"
                            (cons 'cond (cdr clauses))))
                  (cont1 (make-cond-cont exp1 exp2 env cont)))
             (evaluate-beginning-expression test env cont1)))
          ((if)
           (let* ((test (cadr exp))
                  (exp1 (caddr exp))
                  (exp2 (cadddr exp))
                  (cont1 (make-if-cont exp1 exp2 env cont)))
             (evaluate-beginning-expression test env cont1)))
          ((and)
           (let* ((clauses (cdr exp))
                  (test (car clauses))
                  (exp1 (if (null? (cddr clauses))
                            (cadr clauses)
                            (cons 'and (cdr clauses))))
                  (exp2 'false)
                  (cont1 (make-and-cont exp1 exp2 env cont)))
             (evaluate-beginning-expression test env cont1)))
          ((or)
           (let* ((clauses (cdr exp))
                  (test (car clauses))
                  (exp1 'true)
                  (exp2 (if (null? (cddr clauses))
                            (cadr clauses)
                            (cons 'or (cdr clauses))))
                  (cont1 (make-or-cont exp1 exp2 env cont)))
             (evaluate-beginning-expression test env cont1)))
          ((quote)
           (apply-beginning-continuation cont (cadr exp)))
          (else
           (let* ((val0 (env-lookup env (car exp)))
                  (val0 (if (eq? val0 (unspecified))
                            (primop-lookup (car exp))
                            val0)))
             (cond ((eq? val0 (unspecified))
                    ; FIXME
                    (beginning:error "undefined variable" exp))
                   ((null? (cdr exp))
                    (apply-beginning-procedure val0 '() cont))
                   (else
                    (let* ((exps (cdr exp))
                           (exp1 (car exps))
                           (cont1
                            (make-call-cont val0 '() (cdr exps) env cont)))
                      (evaluate-beginning-expression (car exps)
                                                     env
                                                     cont1))))))))
        ((eq? exp 'empty)
         (apply-beginning-continuation cont '()))
        ((number? exp)
         (apply-beginning-continuation cont exp))
        ((eq? exp 'true)
         (apply-beginning-continuation cont #t))
        ((eq? exp 'false)
         (apply-beginning-continuation cont #f))
        ((symbol? exp)
         (let ((val (env-lookup env exp)))
           (if (eq? val (unspecified))
               ; FIXME
               (beginning:error "undefined variable" exp)
               (apply-beginning-continuation cont val))))
        ((string? exp)
         (apply-beginning-continuation cont exp))
        ((char? exp)
         (apply-beginning-continuation cont exp))
        (else
         (beginning:error "bad expression" exp))))

; Continuations.

(define cont:program
  (make-rtd 'cont:program
            '#((immutable pgm0)
               (immutable forms)
               (immutable globals)
               (immutable results))))
(define make-program-cont (rtd-constructor cont:program))
(define program-cont? (rtd-predicate cont:program))
(define program-cont-pgm0    (rtd-accessor cont:program 'pgm0))
(define program-cont-forms   (rtd-accessor cont:program 'forms))
(define program-cont-globals (rtd-accessor cont:program 'globals))
(define program-cont-results (rtd-accessor cont:program 'results))

(define cont:defn
  (make-rtd 'cont:defn
            '#((immutable globals)
               (immutable rhs))))
(define make-defn-cont (rtd-constructor cont:defn))
(define defn-cont? (rtd-predicate cont:defn))
(define defn-cont-globals (rtd-accessor cont:defn 'globals))
(define defn-cont-rhs     (rtd-accessor cont:defn 'rhs))

(define cont:if
  (make-rtd 'cont:if
            '#((immutable exp1)
               (immutable exp2)
               (immutable env)
               (immutable cont))))
(define make-if-cont (rtd-constructor cont:if))
(define if-cont? (rtd-predicate cont:if))
(define if-cont-exp1 (rtd-accessor cont:if 'exp1))
(define if-cont-exp2 (rtd-accessor cont:if 'exp2))
(define if-cont-env  (rtd-accessor cont:if 'env))
(define if-cont-cont (rtd-accessor cont:if 'cont))

(define cont:cond (make-rtd 'cont:cond '#() cont:if))
(define make-cond-cont (rtd-constructor cont:cond))
(define cond-cont? (rtd-predicate cont:cond))

(define cont:and (make-rtd 'cont:and '#() cont:if))
(define make-and-cont (rtd-constructor cont:and))
(define and-cont? (rtd-predicate cont:and))

(define cont:or (make-rtd 'cont:or '#() cont:if))
(define make-or-cont (rtd-constructor cont:or))
(define or-cont? (rtd-predicate cont:or))

(define cont:call
  (make-rtd 'cont:call
            '#((immutable val0)
               (immutable args)
               (immutable exps)
               (immutable env)
               (immutable cont))))
(define make-call-cont (rtd-constructor cont:call))
(define call-cont? (rtd-predicate cont:call))
(define call-cont-val0 (rtd-accessor cont:call 'val0))
(define call-cont-vals (rtd-accessor cont:call 'args))
(define call-cont-exps (rtd-accessor cont:call 'exps))
(define call-cont-env  (rtd-accessor cont:call 'env))
(define call-cont-cont (rtd-accessor cont:call 'cont))

(define (apply-beginning-continuation cont val)
  (step-beginning-continuation cont val))

(define (step-beginning-continuation cont val)
  (cond ((program-cont? cont)
         (let ((pgm0    (program-cont-pgm0 cont))
               (forms   (program-cont-forms cont))
               (globals (program-cont-globals cont))
               (results (cons val (program-cont-results cont))))
           (interpret-beginning-program-loop pgm0 forms globals results)))
        ((defn-cont? cont)
         ; FIXME (?)
         val)
        ((if-cont? cont)
         (let ((exp1 (if-cont-exp1 cont))
               (exp2 (if-cont-exp2 cont))
               (env (if-cont-env cont))
               (cont (if-cont-cont cont)))
           (evaluate-beginning-expression (if val exp1 exp2) env cont)))
        ((call-cont? cont)
         (let ((proc (call-cont-val0 cont))
               (vals (cons val (call-cont-vals cont)))
               (exps (call-cont-exps cont))
               (env (call-cont-env cont))
               (cont (call-cont-cont cont)))
           (if (null? exps)
               (apply-beginning-procedure proc
                                          (reverse vals)
                                          cont)
               (let ((cont (make-call-cont proc vals (cdr exps) env cont)))
                 (evaluate-beginning-expression (car exps) env cont)))))
        (else
         (beginning:error "bad continuation" cont))))
         

; Closures

(define beginning-closure-base
  (make-rtd 'beginning-closure-base
            '#((immutable name))))
(define beginning-closure-base? (rtd-predicate beginning-closure-base))
(define beginning-closure-name (rtd-accessor beginning-closure-base 'name))

(define beginning-closure
  (make-rtd 'beginning-closure
            '#((immutable formals)
               (immutable body)
               (immutable env))
            beginning-closure-base))
(define make-beginning-closure (rtd-constructor beginning-closure))
(define beginning-closure? (rtd-predicate beginning-closure))
(define beginning-closure-formals (rtd-accessor beginning-closure 'formals))
(define beginning-closure-body (rtd-accessor beginning-closure 'body))
(define beginning-closure-env  (rtd-accessor beginning-closure 'env))

(define beginning-primop
  (make-rtd 'beginning-primop
            '#((immutable proc))
            beginning-closure-base))
(define make-beginning-primop (rtd-constructor beginning-primop))
(define beginning-primop? (rtd-predicate beginning-primop))
(define beginning-primop-proc (rtd-accessor beginning-primop 'proc))

(define (apply-beginning-procedure proc args cont)
  (step-beginning-procedure proc args cont))

(define (step-beginning-procedure proc args cont)
  (cond ((beginning-primop? proc)
         ; FIXME: should allow for runtime errors here
         (let ((proc (beginning-primop-proc proc)))
           (apply-beginning-continuation cont (apply proc args))))
        ((beginning-closure? proc)
         (let* ((formals (beginning-closure-formals proc))
                (body (beginning-closure-body proc))
                (env (beginning-closure-env proc))
                (env (env-extend env formals args)))
           (evaluate-beginning-expression body env cont)))
        (else
         (beginning:error "bad proc" proc))))

; Environments.
;
; An environment is one of
;     a hashtable that represents the top-level environment
;     a pair
;         whose car is an association list
;         whose cdr is an environment

(define (beginning-env globals) globals)

(define (env-extend env vars vals)
  (cons (map cons vars vals) env))

(define (env-lookup env id)
  (if (hashtable? env)
      (hashtable-ref env id (unspecified))
      (let ((probe (assq id (car env))))
        (if probe
            (cdr probe)
            (env-lookup (cdr env) id)))))

; Errors

(define (beginning:error msg irritant)
  (error #f msg irritant))

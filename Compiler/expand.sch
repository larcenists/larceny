; Copyright 1992 William Clinger
;
; $Id$
;
; 13 December 1998

($$trace "expand")

; This procedure sets the default scope of global macro definitions.

(define define-syntax-scope
  (let ((flag 'letrec))
    (lambda args
      (cond ((null? args) flag)
            ((not (null? (cdr args)))
             (apply m-warn
                    "Too many arguments passed to define-syntax-scope"
                    args))
            ((memq (car args) '(letrec letrec* let*))
             (set! flag (car args)))
            (else (m-warn "Unrecognized argument to define-syntax-scope"
                          (car args)))))))

; The main entry point.
; The outermost lambda allows known procedures to be lifted outside
; all local variables.

(define (macro-expand def-or-exp)
  (call-with-current-continuation
   (lambda (k)
     (set! m-quit k)
     (set! renaming-counter 0)
     (make-call
      (make-lambda '() ; formals
                   '() ; definitions
                   '() ; R
                   '() ; F
                   '() ; G
                   '() ; declarations
                   #f  ; documentation
                   (desugar-definitions def-or-exp
                                        global-syntactic-environment
                                        make-toplevel-definition))
      '()))))

(define (desugar-definitions exp env make-toplevel-definition)
  (letrec
    
    ((define-loop 
       (lambda (exp rest first env)
         (cond ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-begin)
                     (pair? (cdr exp)))
                (define-loop (cadr exp) (append (cddr exp) rest) first env))
               ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define))
                (let ((exp (desugar-define exp env)))
                  (cond ((and (null? first) (null? rest))
                         exp)
                        ((null? rest)
                         (make-begin (reverse (cons exp first))))
                        (else (define-loop (car rest)
                                (cdr rest)
                                (cons exp first)
                                env)))))
               ((and (pair? exp)
                     (or (eq? (syntactic-lookup env (car exp))
                              denotation-of-define-syntax)
                         (eq? (syntactic-lookup env (car exp))
                              denotation-of-define-inline))
                     (null? first))
                (define-syntax-loop exp rest))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (macro-denotation? (syntactic-lookup env (car exp))))
                (m-transcribe exp
                              env
                              (lambda (exp env)
                                (define-loop exp rest first env))))
               ((and (null? first) (null? rest))
                (m-expand exp env))
               ((null? rest)
                (make-begin (reverse (cons (m-expand exp env) first))))
               (else (make-begin
                      (append (reverse first)
                              (map (lambda (exp) (m-expand exp env))
                                   (cons exp rest))))))))
     
     (desugar-define
      (lambda (exp env)
        (cond 
         ((null? (cdr exp)) (m-error "Malformed definition" exp))
         ; (define foo) syntax is transformed into (define foo (undefined)).
         ((null? (cddr exp))
          (let ((id (cadr exp)))
            (if (or (null? pass1-block-inlines)
                    (not (memq id pass1-block-inlines)))
                (begin
                 (redefinition id)
                 (syntactic-bind-globally! id (make-identifier-denotation id))))
            (make-toplevel-definition id (make-undefined))))
         ((pair? (cadr exp))              
          (desugar-define
           (let* ((def (car exp))
                  (pattern (cadr exp))
                  (f (car pattern))
                  (args (cdr pattern))
                  (body (cddr exp)))
             (if (and (symbol? (car (cadr exp)))
                      (benchmark-mode)
                      (list? (cadr exp)))
                 `(,def ,f
                         ((,lambda0 (,f)
                                    (,set!0 ,f (,lambda0 ,args ,@body))
                                    (,lambda0 ,args ,pattern))
                          0))
                 `(,def ,f (,lambda0 ,args ,@body))))
           env))
         ((> (length exp) 3) (m-error "Malformed definition" exp))
         (else (let ((id (cadr exp)))
                 (if (or (null? pass1-block-inlines)
                         (not (memq id pass1-block-inlines)))
                     (begin
                      (redefinition id)
                      (syntactic-bind-globally! id (make-identifier-denotation id))))
                 (make-toplevel-definition id (m-expand (caddr exp) env)))))))
     
     (define-syntax-loop 
       (lambda (exp rest)
         (cond ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-begin)
                     (pair? (cdr exp)))
                (define-syntax-loop (cadr exp) (append (cddr exp) rest)))
               ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define-syntax))
                (if (pair? (cdr exp))
                    (redefinition (cadr exp)))
                (if (null? rest)
                    (m-define-syntax exp env)
                    (begin (m-define-syntax exp env)
                           (define-syntax-loop (car rest) (cdr rest)))))
               ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define-inline))
                (if (pair? (cdr exp))
                    (redefinition (cadr exp)))
                (if (null? rest)
                    (m-define-inline exp env)
                    (begin (m-define-inline exp env)
                           (define-syntax-loop (car rest) (cdr rest)))))
               ((null? rest)
                (m-expand exp env))
               (else (make-begin
                      (map (lambda (exp) (m-expand exp env))
                           (cons exp rest)))))))
     
     (redefinition
      (lambda (id)
        (if (symbol? id)
            (if (not (identifier-denotation?
                      (syntactic-lookup global-syntactic-environment id)))
                (m-warn "Redefining " id))
            (m-error "Malformed variable or keyword" id)))))
    
    ; body of letrec
    
    (define-loop exp '() '() env)))

; Given an expression and a syntactic environment,
; returns an expression in core Scheme.

(define (m-expand exp env)
  (cond ((not (pair? exp))
         (m-atom exp env))
        ((not (symbol? (car exp)))
         (m-application exp env))
        (else
         (let ((keyword (syntactic-lookup env (car exp))))
           (case (denotation-class keyword)
             ((special)
              (cond
               ((eq? keyword denotation-of-quote)         (m-quote exp))
               ((eq? keyword denotation-of-lambda)        (m-lambda exp env))
               ((eq? keyword denotation-of-if)            (m-if exp env))
               ((eq? keyword denotation-of-set!)          (m-set exp env))
               ((eq? keyword denotation-of-begin)         (m-begin exp env))
               ((eq? keyword denotation-of-let-syntax)
		(m-let-syntax exp env))
               ((eq? keyword denotation-of-letrec-syntax)
		(m-letrec-syntax exp env))
               ((or (eq? keyword denotation-of-define)
                    (eq? keyword denotation-of-define-syntax)
                    (eq? keyword denotation-of-define-inline))
                (m-error "Definition out of context" exp))
               (else (m-bug "Bug detected in m-expand" exp env))))
             ((macro) (m-macro exp env))
             ((inline) (m-inline exp env))
             ((identifier) (m-application exp env))
             (else (m-bug "Bug detected in m-expand" exp env)))))))

(define (m-atom exp env)
  (cond ((not (symbol? exp))
         ; Here exp ought to be a boolean, number, character, or string.
         ; I'll warn about other things but treat them as if quoted.
	 ;
	 ; I'm turning off some of the warnings because notably procedures
	 ; and #!unspecified can occur in loaded files and it's a major
	 ; pain if a warning is printed for each. --lars
         (if (and (not (boolean? exp))
                  (not (number? exp))
                  (not (char? exp))
                  (not (string? exp))
		  (not (procedure? exp))
		  (not (eq? exp (unspecified))))
             (m-warn "Malformed constant -- should be quoted" exp))
         (make-constant exp))
        (else (let ((denotation (syntactic-lookup env exp)))
                (case (denotation-class denotation)
                  ((special macro)
                   (m-warn "Syntactic keyword used as a variable" exp)
                   ; Syntactic keywords used as variables are treated as #t.
                   (make-constant #t))
                  ((inline)
                   (make-variable (inline-name denotation)))
                  ((identifier)
                   (let ((var (make-variable (identifier-name denotation)))
                         (R-entry (identifier-R-entry denotation)))
                     (R-entry.references-set!
                      R-entry
                      (cons var (R-entry.references R-entry)))
                     var))
                  (else (m-bug "Bug detected by m-atom" exp env)))))))

(define (m-quote exp)
  (if (and (pair? (cdr exp))
           (null? (cddr exp)))
      (make-constant (m-strip (cadr exp)))
      (m-error "Malformed quoted constant" exp)))

(define (m-lambda exp env)
  (if (> (safe-length exp) 2)
      
      (let* ((formals (cadr exp))
             (alist (rename-vars formals))
             (env (syntactic-rename env alist))
             (body (cddr exp)))
        
        ; To simplify the run-time system, there's a limit on how many
        ; fixed arguments can be followed by a rest argument.
        ; That limit is removed here.
        ; Bug: documentation slot isn't right when this happens.
        ; Bug: this generates extremely inefficient code.
        
        (if (and (not (list? formals))
                 (> (length alist) @maxargs-with-rest-arg@))
            (let ((TEMP (car (rename-vars '(temp)))))
              (m-lambda
               `(,lambda0 ,TEMP
                           ((,lambda0 ,(map car alist)
                                      ,@(cddr exp))
                            ,@(do ((actuals '() (cons (list name:CAR path)
                                                      actuals))
                                   (path TEMP (list name:CDR path))
                                   (formals formals (cdr formals)))
                                  ((symbol? formals)
                                   (append (reverse actuals) (list path))))))
               env))
            (make-lambda (rename-formals formals alist)
                         '() ; no definitions yet
                         (map (lambda (entry)
                                (cdr (syntactic-lookup env (cdr entry))))
                              alist) ; R
                         '() ; F
                         '() ; G
                         '() ; decls
                         (make-doc #f
                                   (if (list? formals)
                                       (length alist)
                                       (exact->inexact (- (length alist) 1)))
                                   (if (include-variable-names)
                                       formals
                                       #f)
                                   (if (include-source-code)
                                       exp
                                       #f)
                                   source-file-name
                                   source-file-position)
                         (m-body body env))))
      
      (m-error "Malformed lambda expression" exp)))

(define (m-body body env)
  (define (loop body env defs)
    (if (null? body)
        (m-error "Empty body"))
    (let ((exp (car body)))
      (if (and (pair? exp)
               (symbol? (car exp)))
          (let ((denotation (syntactic-lookup env (car exp))))
            (case (denotation-class denotation)
              ((special)
               (cond ((eq? denotation denotation-of-begin)
                      (loop (append (cdr exp) (cdr body)) env defs))
                     ((eq? denotation denotation-of-define)
                      (loop (cdr body) env (cons exp defs)))
                     (else (finalize-body body env defs))))
              ((macro)
               (m-transcribe exp
                             env
                             (lambda (exp env)
                               (loop (cons exp (cdr body))
                                     env
                                     defs))))
              ((inline identifier)
               (finalize-body body env defs))
              (else (m-bug "Bug detected in m-body" body env))))
          (finalize-body body env defs))))
  (loop body env '()))

(define (finalize-body body env defs)
  (if (null? defs)
      (let ((body (map (lambda (exp) (m-expand exp env))
                       body)))
        (if (null? (cdr body))
            (car body)
            (make-begin body)))
      (let ()
        (define (sort-defs defs)
          (let* ((augmented
                  (map (lambda (def)
                         (let ((rhs (cadr def)))
                           (if (not (pair? rhs))
                               (cons 'trivial def)
                               (let ((denotation
                                      (syntactic-lookup env (car rhs))))
                                 (cond ((eq? denotation
                                             denotation-of-lambda)
                                        (cons 'procedure def))
                                       ((eq? denotation
                                             denotation-of-quote)
                                        (cons 'trivial def))
                                       (else
                                        (cons 'miscellaneous def)))))))
                       defs))
                 (sorted (twobit-sort (lambda (x y)
                                        (or (eq? (car x) 'procedure)
                                            (eq? (car y) 'miscellaneous)))
                                      augmented)))
            (map cdr sorted)))
        (define (desugar-definition def)
          (if (> (safe-length def) 2)
              (cond ((pair? (cadr def))
                     (desugar-definition
                      `(,(car def)
                        ,(car (cadr def))
                        (,lambda0
                          ,(cdr (cadr def))
                          ,@(cddr def)))))
                    ((and (= (length def) 3)
                          (symbol? (cadr def)))
                     (cdr def))
                    (else (m-error "Malformed definition" def)))
              (m-error "Malformed definition" def)))
        (define (expand-letrec bindings body)
          (make-call
           (m-expand
            `(,lambda0 ,(map car bindings)
                       ,@(map (lambda (binding)
                                `(,set!0 ,(car binding)
                                         ,(cadr binding)))
                              bindings)
                         ,@body)
            env)
           (map (lambda (binding) (make-unspecified)) bindings)))
        (expand-letrec (sort-defs (map desugar-definition
                                       (reverse defs)))
                       body))))

(define (m-if exp env)
  (let ((n (safe-length exp)))
    (if (or (= n 3) (= n 4))
        (make-conditional (m-expand (cadr exp) env)
                          (m-expand (caddr exp) env)
                          (if (= n 3)
                              (make-unspecified)
                              (m-expand (cadddr exp) env)))
        (m-error "Malformed if expression" exp))))

(define (m-set exp env)
  (if (= (safe-length exp) 3)
      (let ((lhs (m-expand (cadr exp) env))
            (rhs (m-expand (caddr exp) env)))
        (if (variable? lhs)
            (let* ((x (variable.name lhs))
                   (assignment (make-assignment x rhs))
                   (denotation (syntactic-lookup env x)))
              (if (identifier-denotation? denotation)
                  (let ((R-entry (identifier-R-entry denotation)))
                    (R-entry.references-set!
                     R-entry
                     (remq! lhs (R-entry.references R-entry)))
                    (R-entry.assignments-set!
                     R-entry
                     (cons assignment (R-entry.assignments R-entry)))))
              (if (and (lambda? rhs)
                       (include-procedure-names))
                  (let ((doc (lambda.doc rhs)))
                    (doc.name-set! doc x)))
              (if pass1-block-compiling?
                  (set! pass1-block-assignments
                        (cons x pass1-block-assignments)))
              assignment)
            (m-error "Malformed assignment" exp)))
      (m-error "Malformed assignment" exp)))

(define (m-begin exp env)
  (cond ((> (safe-length exp) 1)
         (make-begin (map (lambda (exp) (m-expand exp env)) (cdr exp))))
        ((= (safe-length exp) 1)
         (m-warn "Non-standard begin expression" exp)
         (make-unspecified))
        (else
         (m-error "Malformed begin expression" exp))))

(define (m-application exp env)
  (if (> (safe-length exp) 0)
      (let* ((proc (m-expand (car exp) env))
             (args (map (lambda (exp) (m-expand exp env))
                        (cdr exp)))
             (call (make-call proc args)))
        (if (variable? proc)
            (let* ((procname (variable.name proc))
                   (entry
                    (and (not (null? args))
                         (constant? (car args))
                         (integrate-usual-procedures)
                         (every1? constant? args)
                         (let ((entry (constant-folding-entry procname)))
                           (and entry
                                (let ((predicates
                                       (constant-folding-predicates entry)))
                                  (= (length args)
                                     (length predicates))
                                  (let loop ((args args)
                                             (predicates predicates))
                                    (cond ((null? args) entry)
                                          (((car predicates)
                                            (constant.value (car args)))
                                           (loop (cdr args) (cdr predicates)))
                                          (else #f)))))))))
              (if entry
                  (make-constant (apply (constant-folding-folder entry)
                                        (map constant.value args)))
                  (let ((denotation (syntactic-lookup env procname)))
                    (if (identifier-denotation? denotation)
                        (let ((R-entry (identifier-R-entry denotation)))
                          (R-entry.calls-set!
                           R-entry
                           (cons call (R-entry.calls R-entry)))))
                    call)))
            call))
      (m-error "Malformed application" exp)))

; The environment argument should always be global here.

(define (m-define-inline exp env)
  (cond ((and (= (safe-length exp) 3)
              (symbol? (cadr exp)))
         (let ((name (cadr exp)))
           (m-define-syntax1 name
                             (caddr exp)
                             env
                             (define-syntax-scope))
           (let ((denotation
                  (syntactic-lookup global-syntactic-environment name)))
             (syntactic-bind-globally!
              name
              (make-inline-denotation name
                                      (macro-rules denotation)
                                      (macro-env denotation))))
           (make-constant name)))
        (else
         (m-error "Malformed define-inline" exp))))

; The environment argument should always be global here.

(define (m-define-syntax exp env)
  (cond ((and (= (safe-length exp) 3)
              (symbol? (cadr exp)))
         (m-define-syntax1 (cadr exp)
                           (caddr exp)
                           env
                           (define-syntax-scope)))
        ((and (= (safe-length exp) 4)
              (symbol? (cadr exp))
              ; FIXME: should use denotations here
              (memq (caddr exp) '(letrec letrec* let*)))
         (m-define-syntax1 (cadr exp)
                           (cadddr exp)
                           env
                           (caddr exp)))
        (else (m-error "Malformed define-syntax" exp))))

(define (m-define-syntax1 keyword spec env scope)
  (if (and (pair? spec)
           (symbol? (car spec)))
      (let* ((transformer-keyword (car spec))
             (denotation (syntactic-lookup env transformer-keyword)))
        (cond ((eq? denotation denotation-of-syntax-rules)
               (case scope
                 ((letrec)  (m-define-syntax-letrec keyword spec env))
                 ((letrec*) (m-define-syntax-letrec* keyword spec env))
                 ((let*)    (m-define-syntax-let* keyword spec env))
                 (else      (m-bug "Weird scope" scope))))
              ((same-denotation? denotation denotation-of-transformer)
               ; FIXME: no error checking here
               (syntactic-bind-globally!
                keyword
                (make-macro-denotation (eval (cadr spec)) env)))
              (else
               (m-error "Malformed syntax transformer" spec))))
      (m-error "Malformed syntax transformer" spec))
  (make-constant keyword))

(define (m-define-syntax-letrec keyword spec env)
  (syntactic-bind-globally!
   keyword
   (m-compile-transformer-spec spec env)))

(define (m-define-syntax-letrec* keyword spec env)
  (let* ((env (syntactic-extend (syntactic-copy env)
                                (list keyword)
                                '((fake denotation))))
         (transformer (m-compile-transformer-spec spec env)))
    (syntactic-assign! env keyword transformer)
    (syntactic-bind-globally! keyword transformer)))

(define (m-define-syntax-let* keyword spec env)
  (syntactic-bind-globally!
   keyword
   (m-compile-transformer-spec spec (syntactic-copy env))))

(define (m-let-syntax exp env)
  (if (and (> (safe-length exp) 2)
           (every1? (lambda (binding)
                      (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding))
                           (null? (cddr binding))))
                    (cadr exp)))
      (m-body (cddr exp)
              (syntactic-extend env
                                (map car (cadr exp))
                                (map (lambda (spec)
                                       (m-compile-transformer-spec
                                        spec
                                        env))
                                     (map cadr (cadr exp)))))
      (m-error "Malformed let-syntax" exp)))

(define (m-letrec-syntax exp env)
  (if (and (> (safe-length exp) 2)
           (every1? (lambda (binding)
                      (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding))
                           (null? (cddr binding))))
                    (cadr exp)))
      (let ((env (syntactic-extend env
                                   (map car (cadr exp))
                                   (map (lambda (id)
                                          '(fake denotation))
                                        (cadr exp)))))
        (for-each (lambda (id spec)
                    (syntactic-assign!
                     env
                     id
                     (m-compile-transformer-spec spec env)))
                  (map car (cadr exp))
                  (map cadr (cadr exp)))
        (m-body (cddr exp) env))
      (m-error "Malformed let-syntax" exp)))

(define (m-macro exp env)
  (m-transcribe exp
                env
                (lambda (exp env)
                  (m-expand exp env))))

(define (m-inline exp env)
  (if (integrate-usual-procedures)
      (m-transcribe-inline exp
                           env
                           (lambda (newexp env)
                             (if (eq? exp newexp)
                                 (m-application exp env)
                                 (m-expand newexp env))))
      (m-application exp env)))

(define m-quit             ; assigned by macro-expand
  (lambda (v) v))

; To do:
; Clean up alist hacking et cetera.
; Declarations.
; Integrable procedures.
; New semantics for body of LET-SYNTAX and LETREC-SYNTAX.

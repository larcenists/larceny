; Copyright 1991 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 14 December 1994
;
; First pass of the Twobit compiler:
;   macro expansion, syntax checking, alpha conversion,
;   preliminary annotation.
;
; This pass uses a MacScheme-like macro system and should be
; replaced by a pass based on the R4RS macro system.
;
; The input to this pass is a Scheme definition or expression.
; The output is an expression in the subset of Scheme described
; by the following grammar, where the output satisfies certain
; additional invariants described below.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the output:
;   *  There are no internal definitions.
;   *  No identifier containing an upper case letter is bound anywhere.
;      (Change the "name:..." variables if upper case is preferred.)
;   *  No identifier is bound in more than one place.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.
;   *  F and G are garbage.

(define (pass1 def-or-exp)
  (set! renaming-counter 0)
  (m-scan `((lambda () ,(desugar-definitions def-or-exp)))
          '()))

(define (desugar-definitions exp)
  (letrec 
    ((define-loop 
       (lambda (exp rest first)
         (cond ((and (pair? exp) (eq? (car exp) 'begin) (pair? (cdr exp)))
                (define-loop (cadr exp) (append (cddr exp) rest) first))
               ((and (pair? exp) (eq? (car exp) 'define))
                (let* ((exp (desugar-define exp))
                       (exp (list 'begin
                                  (cons 'set! (cdr exp))
                                  (list 'quote (cadr exp)))))
                  (cond ((and (null? first) (null? rest)) exp)
                        ((null? rest) (cons 'begin (reverse (cons exp first))))
                        (else (define-loop (car rest) (cdr rest) (cons exp first))))))
               ((and (pair? exp) (assq (car exp) @macros@))
                (define-loop ((cadr (assq (car exp) @macros@)) exp) rest first))
               ((and (null? first) (null? rest)) exp)
               ((null? rest) (cons 'begin (reverse (cons exp first))))
               (else (cons 'begin (append (reverse first) (cons exp rest)))))))
     
     (desugar-define
      (lambda (exp)
        (cond 
         ((null? (cdr exp)) (complain exp))
         ((null? (cddr exp))
          (list 'define (cadr exp) '(undefined)))
         ((and (pair? (cadr exp))
               (symbol? (car (cadr exp)))
               (benchmark-mode)
               (list? (cadr exp)))
          `(define ,(car (cadr exp))
                   (lambda ,(cdr (cadr exp))
                     (define ,(car (cadr exp))
                       (lambda ,(cdr (cadr exp))
                         ,@(cddr exp)))
                     ,(cadr exp))))
         ((pair? (cadr exp))
          (desugar-define `(define ,(car (cadr exp))
                                   (lambda ,(cdr (cadr exp))
                                     ,@(cddr exp)))))
         ((> (length exp) 3) (complain exp))
         (else exp))))
     
     (complain (lambda (exp)
                 (error "Malformed definition" exp))))
    
    ; body of letrec
    
    (define-loop exp '() '())))

; A renaming/annotation environment is an association list of the form
;
;     ((<identifier> . <entry>) ...)
;
; where the <entry> is an R-entry giving the new name and referencing
; information being constructed for the <identifier>.

; Given a renaming/annotation environment, returns a predicate that
; returns true if a given identifier is bound in that environment.

(define (make-bound? env)
  (lambda (id)
    (assq id env)))

(define renaming-counter 0)

(define (rename vars)
  (set! renaming-counter (+ renaming-counter 1))
  (let ((s (string-append "_" (number->string renaming-counter))))
    (map (lambda (var)
           (string->symbol
            (string-append renaming-prefix (symbol->string var) s)))
         vars)))

(define (rename-formals formals newnames)
  (cond ((null? formals) '())
        ((symbol? formals) (car newnames))
        (else (cons (car newnames)
                    (rename-formals (cdr formals)
                                    (cdr newnames))))))

; Given an expression and a renaming/annotation environment,
; returns an expression suitable for pass 2.

(define (m-scan exp env)
  (if (not (pair? exp))
      (m-atom exp env)
      (let ((keyword (car exp)))
        (cond ((eq? keyword 'quote)  (m-quote exp))
              ((eq? keyword 'lambda) (m-lambda exp env #f))
              ((eq? keyword 'if)     (m-if exp env))
              ((eq? keyword 'set!)   (m-set exp env))
              ((eq? keyword 'begin)  (m-begin exp env))
              ((memq keyword @special-forms@)
               (m-macro exp env))
              (else (m-application exp env))))))

(define (m-atom exp env)
  (cond ((not (symbol? exp))
         (if (and (not (boolean? exp))
                  (not (number? exp))
                  (not (char? exp))
                  (not (string? exp)))
             (static-warning 21 exp))
         (make-constant exp))
        ((memq exp @special-forms@)     ; Can't use keywords
         (static-warning 1 exp)         ; as variables.
         (make-constant #t))
        (else (let ((probe (assq exp env)))
                (if probe
                    (let* ((entry (cdr probe))
                           (name (R-entry.name entry))
                           (var (make-variable name)))
                      (R-entry.references-set!
                       (cdr probe)
                       (cons var (R-entry.references (cdr probe))))
                      var)
                    (make-variable exp))))))

(define (m-quote exp)
  (if (and (pair? (cdr exp))
           (null? (cddr exp)))
      (make-constant (cadr exp))
      (static-error 14 exp)))

(define (m-lambda original-exp env proc-id)
  
  ; Get rid of internal definitions, standardize
  ; bound variable list, and perform syntax checking.  
  
  (let* ((exp&decls (desugar-lambda original-exp))
         (exp (car exp&decls))
         (declarations (cadr exp&decls))
         (formals (cadr exp))
         (bvl (make-null-terminated formals)))
    
    (cond
     
     ; can't bind keywords
     
     ((some? (lambda (var)
               (memq var @special-forms@))
             bvl)
      (static-error 1 original-exp))
     
     ; formals must be symbols
     
     ((not (every? symbol? bvl))
      (static-error 4 original-exp))
     
     ; To simplify the run-time system, there's a limit on how many
     ; fixed arguments can be followed by a rest argument.
     ; That limit is removed here.
     ; Bug: documentation slot isn't right when this happens.
     ; Bug: macros at the beginning of the macro get expanded twice.
     ; These aren't hard to fix, but a pain.
     
     ((and (not (list? formals))
           (> (length bvl) @maxargs-with-rest-arg@))
      (let ((TEMP (car (rename '(temp)))))
        (m-lambda
         `(lambda ,TEMP
                   ((lambda ,bvl ,@(cddr original-exp))
                    ,@(do ((actuals '() (cons (list 'car path) actuals))
                           (path TEMP (list 'cdr path))
                           (formals formals (cdr formals)))
                          ((symbol? formals)
                           (append (reverse actuals) (list path))))))
         env
         proc-id)))
     
     (else
      (let* ((newnames (rename bvl))
             (refinfo (map (lambda (var)
                             (make-R-entry var '() '() '()))
                           newnames))
             (newenv (append (map cons bvl refinfo) env)))
        (make-lambda
         (rename-formals formals newnames)
         '()
         refinfo
         '()
         '()
         declarations
         (cond ((and (include-source-code)
                     (include-procedure-names)
                     proc-id)
                (cons proc-id (cdr original-exp)))
               ((include-source-code) original-exp)
               ((include-procedure-names) proc-id)
               (else #f))
         (m-scan (cons 'begin (cddr exp)) newenv)))))))

(define (m-if exp env)
  (if (list? exp)
      (cond ((= (length exp) 4)
             (m-if-help (m-scan (cadr exp) env)
                        (m-scan (caddr exp) env)
                        (m-scan (cadddr exp) env)
                        env))
            ((= (length exp) 3)
             (m-if-help (m-scan (cadr exp) env)
                        (m-scan (caddr exp) env)
                        (make-constant '#f)
                        env))
            (else (static-error 5 exp)))
      (static-error 5 exp)))

(define (m-if-help e0 e1 e2 env)
  (if (empty-list-is-true)
      (make-conditional e0 e1 e2)
      (let ((proc (begin (empty-list-is-true #t)
                         (m-scan '(lambda (t) (if t (not (null? t)) #f))
                                 '()))))
        (empty-list-is-true #f)
        (make-conditional
         (make-call proc
                    (list e0))
         e1
         e2
         env))))

(define (m-set exp env)
  (if (and (pair? (cdr exp))
           (pair? (cddr exp))
           (null? (cdddr exp))
           (symbol? (cadr exp))
           (not (memq (cadr exp) @special-forms@)))
      (let ((lhs (cadr exp))
            (rhs (caddr exp)))
        (let ((rhs (if (and (pair? rhs)
                            (eq? (car rhs) 'lambda))
                       (m-lambda rhs env lhs)
                       (m-scan rhs env)))
              (probe (assq lhs env)))
          (if probe
              (let* ((entry (cdr probe))
                     (exp (make-assignment (R-entry.name entry)
                                           rhs)))
                (R-entry.assignments-set!
                 entry
                 (cons exp (R-entry.assignments entry)))
                exp)
              (begin (if (and (integrate-usual-procedures)
                              (or (prim-entry lhs)
                                  (assq lhs @integrable@)))
                         (static-warning 7 exp))
                     (make-assignment lhs rhs)))))
      (static-error 6 exp)))

(define (m-begin exp env)
  (cond ((not (list? exp)) (static-error 13 exp))
        ((null? (cdr exp)) (make-constant #f))
        ((null? (cddr exp)) (m-scan (cadr exp) env))
        ((not (pair? (cadr exp)))
         (m-begin (cons 'begin (cddr exp)) env))
        (else (make-begin (map (lambda (exp) (m-scan exp env))
                               (cdr exp))))))

(define (m-macro exp env)
  (m-scan ((cadr (assq (car exp) @macros@)) exp) env))

; The arity of calls to primitive procedures is standardized in
; this routine.  Integrable procedures are also handled specially.

(define (m-application exp env)
  (cond ((not (list? exp))
         (static-error 8 exp))
        ((not (symbol? (car exp)))
         (make-call (m-scan (car exp) env)
                    (map (lambda (x) (m-scan x env)) (cdr exp))))
        ((assq (car exp) env)
         (let ((entry (cdr (assq (car exp) env)))
               (call (make-call (m-scan (car exp) env)
                                (map (lambda (x) (m-scan x env))
                                     (cdr exp)))))
           (R-entry.calls-set! entry
                               (cons call (R-entry.calls entry)))
           call))
        ((integrate-usual-procedures)
         (let ((integrable (assq (car exp) @integrable@)))
           (if integrable
               (m-integrable-application exp env integrable)
               (let ((prim (prim-entry (car exp))))
                 (if prim
                     (m-prim-application exp env prim)
                     (make-call (m-scan (car exp) env)
                                (map (lambda (exp) (m-scan exp env))
                                     (cdr exp))))))))
        (else (make-call (m-scan (car exp) env)
                         (map (lambda (exp) (m-scan exp env))
                              (cdr exp))))))

(define (m-integrable-application exp env integrable)
  ((cadr integrable) exp env))
          
(define (m-prim-application exp env prim)
  (cond ((not (= (prim-arity prim) (length (cdr exp))))
         (static-error 9 exp))    ; wrong # of args
        (else
         (make-call (make-variable (prim-opcodename prim))
                    (map (lambda (x) (m-scan x env))
                         (cdr exp))))))

; desugar-lambda returns a list of two elements.  The first 
; element is the transformed lambda expression.  The second is 
; a (possibly empty) list of declarations. 
; Transformations include flattening nested begins
; and converting internal defines to a letrec.
; Macro calls that expand to internal defines 
; are also transformed into the letrec variables.
; Macro calls that expand to declarations are expanded
; if they appear where a declaration may appear.

; The code checks for define and declare before expanding macro calls
; so that we can create define and declare macros that generate
; error messages saying they were called in an illegal context.

(define desugar-lambda
  (lambda (exp)
    (if (and (list? exp)
             (> (length exp) 2))
        (letrec 
          ((body (cddr exp))
           (defs '())
           (decls '())   ; list of declarations
           (declare-loop
            (lambda ()
              (cond ((not (pair? body)) (static-error 4 exp))
                    ((not (pair? (car body))) #f)
                    ((eq? (caar body) 'declare)
                     (cond ((legal-declaration? (car body))
                            (set! decls (cons (car body) decls))
                            (set! body (cdr body))
                            (declare-loop))
                           (else (static-warning 17 (car body))
                                 (set! body (cdr body))
                                 (declare-loop))))
                    ((eq? (caar body) 'define) #f)
                    ((assq (caar body) @macros@)
                     (set! body 
                           (cons ((cadr (assq (caar body) @macros@))
                                  (car body))
                                 (cdr body)))
                     (declare-loop))
                    (else #f))))
           (define-loop
             (lambda ()
               (cond ((not (pair? body))
                      (static-error 4 exp))
                     ((not (pair? (car body))) #f)
                     ((eq? (caar body) 'define)
                      (set! defs (cons (car body) defs))
                      (set! body (cdr body))
                      (define-loop))
                     ((eq? (caar body) 'begin)
                      (set! body (append (cdr (car body)) (cdr body)))
                      (define-loop))
                     ((assq (caar body) @macros@)
                      (set! body
                            (cons 
                             ((cadr (assq (caar body) @macros@))
                              (car body))
                             (cdr body)))
                      (define-loop))
                     (else #f))))
           ) ; end of letrec variables
          
          ; body of letrec
          
          (declare-loop)
          (define-loop)
          (list 
           (if (not (null? defs))
               `(lambda ,(cadr exp) 
                        (letrec
                          ,(map cdr 
                                (map desugar-define$ 
                                     (reverse defs)))
                          ,@body))
               `(lambda ,(cadr exp) ,@body))
           (apply append
                  (map cdr (reverse decls)))))
        
        ; Other branch of if expression
        (static-error 4 exp))))

; Returns #t if a declaration is legal, #f if illegal.
; Currently, legal declarations are of the form 
; (declare (optimize <val>)) where val is one of 
; 0, 1, 2, 3, 4, #f, or #t.  val does not get evaluated.

(define legal-declaration?
  (let ((legal-declarations '((optimize (0 1 2 3 4 #f #t)))))
    (lambda (decl)
      (cond ((< (length decl) 2) #f)
            (else (every?
                   (lambda (d)
                     (and (list? d)
                          (= (length d) 2)
                          (let ((temp 
                                 (assq (car d) 
                                       legal-declarations)))
                            (and temp
                                 (memq (cadr d) (cadr temp))))))
                   (cdr decl)))))))

; Takes a list whose car is "define".  Any define syntax other 
; than (define var expr) or (define (var1 ...) expr1 ...)
; or (define (form var1 ...) expr1 ...) or (define var) is an
; error here.  Transforms the last three syntaxes above 
; into the first.

(define desugar-define$
  (lambda (l)
    (cond ((null? (cdr l)) (static-error 20 l))  ; Malformed define
          ((and (null? (cddr l)) (symbol? (cadr l)))
           `(define ,(cadr l) (undefined)))
          ((pair? (cadr l))
           (if (symbol? (car (cadr l)))
               `(define
                 ,(car (cadr l))
                 (lambda ,(cdr (cadr l)) ,@(cddr l)))
               (desugar-define$
                `(define ,(caadr l) (lambda ,(cdadr l) ,@(cddr l))))))
          ((symbol? (cadr l)) l)
          (else (static-error 20 l)))))

; Reports error message to the terminal and resets.

(define (static-error msg . irritants)
  (set! msg (assq msg static-error-table))
  (if msg
      (set! msg (cdr msg))
      (error (cons "Compiler error -- bad static error number"
                   (cons msg irritants))))
  (apply error (cons msg irritants)))

; Like static-error, but just issues a warning.

(define (static-warning msg . irritants)
  (if (issue-warnings)
      (begin (set! msg (assq msg static-error-table))
             (if msg
                 (set! msg (cdr msg))
                 (error (cons "Compiler error -- bad static error number"
                              (cons msg irritants))))
             (display "WARNING:  ")
             (display msg)
             (newline)
             (for-each (lambda (x) (write x) (newline)) irritants))))

(define static-error-table
  '((1 . "Keywords of special forms may not be used as variables")
    (2 . "Compiler error -- lambda expressions nested too deeply")
    (3 . "Compiler error -- too many arguments to procedure")
    (4 . "Malformed lambda expression")
    (5 . "Malformed if expression")
    (6 . "Malformed set! expression")
    (7 . "Assignment to integrable procedure")
    (8 . "Malformed procedure call")
    (9 . "Incorrect call to integrable procedure")
    ;(10 . "Compiler error -- expression stack overflow")
    ;(11 . "Compiler error -- branch offset out of range")
    ;(12 . "Compiler error -- too many global variables and constants")
    (13 . "Malformed begin expression")
    (14 . "Malformed constant")
    (15 . "Compiler error -- bad primop arity")
    (16 . "Malformed or expression")
    (17 . "Malformed declaration")
    ;(18 . "Malformed internal definition")
    ;(19 . "Declaration in illegal context")
    (20 . "Malformed definition")
    (21 . "Malformed constant -- should be quoted")
    ))

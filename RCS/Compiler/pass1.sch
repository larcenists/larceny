; Copyright 1991 Lightship Software, Incorporated.
;
; First pass of the Scheme 313 compiler:
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
;           (quote (R F <decls> <doc>)
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
;   *  F is garbage.

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
               (benchmark-mode))
          `(define ,(car (cadr exp))
                   (lambda ,(cdr (cadr exp))
                     (define ,(car (cadr exp))
                       (lambda ,(cdr (cadr exp))
                         ,@(cddr exp)))   ; wrong for dotted arglist.
                     ,(cadr exp))))
         ((pair? (cadr exp))
          (desugar-define `(define ,(car (cadr exp))
                                   (lambda ,(cdr (cadr exp))
                                     ,@(cddr exp)))))
         ((> (length exp) 3) (complain exp))
         (else exp))))
     
     (complain (lambda ()
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
            (string-append "." (symbol->string var) s)))
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
                  (not (string? exp))
                  (not (eq? exp hash-bang-unspecified)))
             (static-warning 21 exp))
         (make-constant exp))
        ((memq exp **special-forms**)   ; Can't use keywords
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
     
     ((not (every symbol? bvl))
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
      (make-conditional
       (make-call (m-scan '(lambda (t) (if t (not (null? t)) #f))
                          '())
                  (list e0))
       e1
       e2
       env)))

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
  (let ((newexp ((cadr integrable) exp (make-bound? env))))
    (cond ((eq? newexp exp)
           (let ((prim (prim-entry (car exp))))
             (if prim
                 (m-prim-application exp env prim)
                 (make-call (m-scan (car exp) env)
                            (map (lambda (exp) (m-scan exp env))
                                 (cdr exp))))))
          (newexp (m-scan newexp env))
          (else (static-error 9 exp)))))
          
(define (m-prim-application exp env prim)
  (cond ((not (= (prim-arity prim) (length (cdr exp))))
         (static-error 9 exp))    ; wrong # of args
        (else
         (make-call (if (and (eq? prim 'not)
                             (integrate-usual-procedures)
                             (not (empty-list-is-true)))
                        (m-scan '(lambda (t) (if t (null? t) #f)) '())
                        (make-variable (prim-opcodename prim)))
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


; Macros for cross-compilation.

(define @special-forms@
  '(quote lambda if set! begin))

(define @macros@ '())

(define (install-macro keyword transformer)
  (if (assq keyword @macros@)
      (begin (set! @special-forms@ (remq! keyword @special-forms@))
             (set! @macros@ (remq! (assq keyword @macros@) @macros@))
             (install-macro keyword transformer))
      (begin (set! @macros@ (cons (list keyword transformer) @macros@))
             (set! @special-forms@ (cons keyword @special-forms@))
             keyword)))

; (for-each (lambda (x) (install-macro (car x) (cadr x)))
;          **macros**)

(extend-syntax (define-macro)
  ((define-macro boing-keyword boing-transformer)
   (install-macro (quote boing-keyword) boing-transformer)))

; (macro define-macro
;        (lambda (l)
;          (let ((keyword (cadr l))
;                (transformer (caddr l)))
;            `(install-macro ',keyword ,transformer))))

(define-macro undefined
  (lambda (l) `',hash-bang-unspecified))

(define-macro or
  (lambda (l)
    (cond ((null? (cdr l)) '#f)
          ((null? (cddr l)) (cadr l))
          (else (let ((temp (gensym "T")))
                  `(let ((,temp ,(cadr l)))
                        (if ,temp ,temp (or ,@(cddr l)))))))))

(define-macro and
  (lambda (l)
    (cond ((null? (cdr l)) #t)
	  ((null? (cddr l)) (cadr l))
	  (else (let ((temp (gensym "T")))
		  `(let ((,temp ,(cadr l)))
		     (if ,temp (and ,@(cddr l)) #f)))))))

(define-macro case
  (lambda (l)
    (error "Case not implemented.")))

(define-macro let*
  (lambda (l)
    (error "let* not implemented.")))

(define-macro cond
  (lambda (l)
    (if (null? (cdr l))
        `',hash-bang-unspecified
        (if (memq (car (car (cdr l))) '(#t else))
            (cons 'begin (cdr (car (cdr l))))
            (if (= (length (car (cdr l))) 1)
                (list 'or
                      (car (car (cdr l)))
                      (cons 'cond (cdr (cdr l))))
                (if (eq? (car (cdr (car (cdr l)))) '=>)
                    (if (= (length (car (cdr l))) 3)
                        (list '(lambda (test-result thunk2 thunk3)
                                       (if test-result
                                           ((thunk2) test-result)
                                           (thunk3)))
                              (car (car (cdr l)))
                              (list 'lambda
                                    '()
                                    (car (cdr (cdr (car (cdr l))))))
                              (list 'lambda
                                    '()
                                    (cons 'cond (cdr (cdr l)))))
                        (error "Malformed cond clause" l))
                    (list 'if
                          (car (car (cdr l)))
                          (cons 'begin (cdr (car (cdr l))))
                          (cons 'cond (cdr (cdr l))))))))))

; This generates pretty terrible code for named let.
; Ought to fix it someday.

(define-macro let
  (lambda (l)
    (if (and (atom? (cadr l))
             (not (null? (cadr l))))
        (cons (list 'letrec                  ; named let
                    (list (list (cadr l)
                                (cons 'lambda
                                      (cons (map car (caddr l))
                                            (cdddr l)))))
                    (cadr l))
              (map (lambda (x) (cadr x)) (caddr l)))
        (cons (cons 'lambda               ; standard let
                    (cons (map car (cadr l))
                          (cddr l)))
              (map (lambda (x) (cadr x)) (cadr l))))))

(define-macro letrec
  (lambda (l)
    (let ((bindings (sort (cadr l)
                          (lambda (x y)
                            (and (pair? (cadr x))
                                 (eq? (car (cadr x)) 'lambda)))))
          (body (cddr l)))
      (cons (list 'lambda
                  (map car bindings)
                  (cons 'begin
                        (map (lambda (x) (cons 'set! x)) bindings))
                  (cons 'let (cons '() body)))
            (map (lambda (x) '(undefined)) bindings)))))

(define-macro do
  (let ((oops (lambda (l) 
                (error "Malformed do expression" l))))
    (lambda (l)
      (if (not (proper-list? l)) (oops l))
      (if (<? (length l) 3) (oops l))
      (if (not (proper-list? (cadr l))) (oops l))
      (if (not (and (proper-list? (caddr l)) (pair? (caddr l))))
          (oops l))
      (let
        ((loop (gensym "DO"))
         (bindings (map (lambda (x)
                          (cond ((atom? x) (oops l))
                                ((atom? (cdr x))
                                 (list (car x) '() (car x)))
                                ((atom? (cddr x))
                                 (list (car x) (cadr x) (car x)))
                                (else x)))
                        (cadr l)))
         (test (caddr l)))
        (list 'letrec
              (list (list loop
                          (list 'lambda
                                (map car bindings)
                                (list 'if
                                      (car test)
                                      (cons 'begin (cdr test))
                                      (list 'begin
                                            (cons 'begin (cdddr l))
                                            (cons loop (map caddr bindings)))))))
              (cons loop (map cadr bindings)))))))

; (define-macro caar (lambda (l) `(car (car ,(cadr l)))))
; (define-macro cadr (lambda (l) `(car (cdr ,(cadr l)))))
; (define-macro cdar (lambda (l) `(cdr (car ,(cadr l)))))
; (define-macro cddr (lambda (l) `(cdr (cdr ,(cadr l)))))
; (define-macro caaar (lambda (l) `(car (car (car ,(cadr l))))))
; (define-macro caadr (lambda (l) `(car (car (cdr ,(cadr l))))))
; (define-macro cadar (lambda (l) `(car (cdr (car ,(cadr l))))))
; (define-macro caddr (lambda (l) `(car (cdr (cdr ,(cadr l))))))
; (define-macro cdaar (lambda (l) `(cdr (car (car ,(cadr l))))))
; (define-macro cdadr (lambda (l) `(cdr (car (cdr ,(cadr l))))))
; (define-macro cddar (lambda (l) `(cdr (cdr (car ,(cadr l))))))
; (define-macro cdddr (lambda (l) `(cdr (cdr (cdr ,(cadr l))))))
; (define-macro caaaar (lambda (l) `(car (car (car (car ,(cadr l)))))))
; (define-macro caaadr (lambda (l) `(car (car (car (cdr ,(cadr l)))))))
; (define-macro caadar (lambda (l) `(car (car (cdr (car ,(cadr l)))))))
; (define-macro caaddr (lambda (l) `(car (car (cdr (cdr ,(cadr l)))))))
; (define-macro cadaar (lambda (l) `(car (cdr (car (car ,(cadr l)))))))
; (define-macro cadadr (lambda (l) `(car (cdr (car (cdr ,(cadr l)))))))
; (define-macro caddar (lambda (l) `(car (cdr (cdr (car ,(cadr l)))))))
; (define-macro cadddr (lambda (l) `(car (cdr (cdr (cdr ,(cadr l)))))))
; (define-macro cdaaar (lambda (l) `(cdr (car (car (car ,(cadr l)))))))
; (define-macro cdaadr (lambda (l) `(cdr (car (car (cdr ,(cadr l)))))))
; (define-macro cdadar (lambda (l) `(cdr (car (cdr (car ,(cadr l)))))))
; (define-macro cdaddr (lambda (l) `(cdr (car (cdr (cdr ,(cadr l)))))))
; (define-macro cddaar (lambda (l) `(cdr (cdr (car (car ,(cadr l)))))))
; (define-macro cddadr (lambda (l) `(cdr (cdr (car (cdr ,(cadr l)))))))
; (define-macro cdddar (lambda (l) `(cdr (cdr (cdr (car ,(cadr l)))))))
; (define-macro cddddr (lambda (l) `(cdr (cdr (cdr (cdr ,(cadr l)))))))

; For testing.

; MAKE-READABLE strips the referencing information
; and replaces (begin I) by I.

(define (make-readable exp)
  (case (car exp)
    ((quote)    exp)
    ((lambda)   `(lambda ,(lambda.args exp)
                         ,@(map (lambda (def)
                                  `(define ,(def.lhs def)
                                           ,(make-readable (def.rhs def))))
                                (lambda.defs exp))
                           ,(make-readable (lambda.body exp))))
    ((set!)     `(set! ,(assignment.lhs exp)
                       ,(make-readable (assignment.rhs exp))))
    ((if)       `(if ,(make-readable (if.test exp))
                     ,(make-readable (if.then exp))
                     ,(make-readable (if.else exp))))
    ((begin)    (if (variable? exp)
                    (variable.name exp)
                    `(begin ,@(map make-readable (begin.exprs exp)))))
    (else       `(,(make-readable (call.proc exp))
                  ,@(map make-readable (call.args exp))))))

; MAKE-UNREADABLE does the reverse.
; It assumes there are no internal definitions.

(define (make-unreadable exp)
  (cond ((symbol? exp) (list 'begin exp))
        ((pair? exp)
         (case (car exp)
           ((quote) exp)
           ((lambda) (list 'lambda
                           (cadr exp)
                           '(begin)
                           '(() ())
                           (make-unreadable (cons 'begin (cddr exp)))))
           ((set!) (list 'set! (cadr exp) (make-unreadable (caddr exp))))
           ((if) (list 'if
                       (make-unreadable (cadr exp))
                       (make-unreadable (caddr exp))
                       (if (= (length exp) 3)
                           (list 'quote hash-bang-unspecified)
                           (make-unreadable (cadddr exp)))))
           ((begin) (if (= (length exp) 2)
                        (make-unreadable (cadr exp))
                        (cons 'begin (map make-unreadable (cdr exp)))))
           (else (map make-unreadable exp))))
        (else (list 'quote exp))))

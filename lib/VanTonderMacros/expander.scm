
;;;===============================================================================
;;;
;;; R6RS Macros and R6RS libraries:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;===============================================================================

;;;================================================================================
;;;
;;; PORTING:
;;; --------
;;;
;;; Uncomment whichever (load ---) is applicable or provide your own.
;;; Compat-*.scm should supply whatever is missing from your implementation of:
;;;
;;;  - SRFI-9.
;;;  - Procedure (ex:unique-token) that provides a GUID string once per run.
;;;  - Procedure (make-parameter init) and syntax (parameterize ((param val) ...) exp ...)
;;;  - Syntax let-values.
;;;  - Procedure pretty-print.
;;;  - Procedures file-exists? and delete-file.
;;;
;;; IMPORTANT:
;;; ----------
;;;
;;; Read HOOKS and R6RS compatibility sections below for a few further
;;; customization issues affecting production systems, including compiler
;;; and REPL integration.
;;;
;;;=================================================================================

(load "compat-mzscheme.scm")
;; (load "compat-larceny.scm")
;; (load "compat-chez.scm")

;;;=================================================================================
;;;
;;; EXPORTS:
;;;
;;;=================================================================================

;; Direct exports:

(define $ex:make-variable-transformer #f)
(define $ex:identifier?               #f)
(define $ex:bound-identifier=?        #f)
(define $ex:free-identifier=?         #f)
(define $ex:generate-temporaries      #f)
(define $ex:datum->syntax             #f)
(define $ex:syntax->datum             #f)
(define $ex:environment               #f)
(define $ex:environment-bindings      #f)
(define $ex:eval                      #f)
(define $ex:syntax-violation          #f)

;; System exports:  

(define $ex:expand-file               #f)
(define $ex:repl                      #f)

;; Indirect exports:

(define $ex:invalid-form              #f)
(define $ex:register-macro!           #f)
(define $ex:extend-table-of-envs!     #f)
(define $ex:import-libraries          #f)
(define $ex:syntax-rename             #f)
(define $ex:map-while                 #f)
(define $ex:dotted-length             #f)
(define $ex:dotted-butlast            #f)
(define $ex:dotted-last               #f)
(define $ex:unspecified               #f)

;;;===============================================================================
;;;
;;; R6RS compatibility:
;;;
;;; These are only partial implementations for specific use cases needed.
;;; They should be removed and fully r6rs-compliant versions
;;; should be provided by host implementation.
;;;
;;;===============================================================================

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

(define (filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (filter p? (cdr lst)))
          (filter p? (cdr lst)))))

(define (for-all proc l . ls)
  (or (null? l)
      (and (apply proc (car l) (map car ls))
           (apply for-all proc (cdr l) (map cdr ls)))))

;; Non-exported bindings are "hidden" with the ex: prefix.
;; If you already have letrec* semantics for internal definitions,
;; you may replace begin with let () for better hiding of locals.

(begin
  
  ;;;===============================================================================
  ;;;
  ;;; Hooks:
  ;;;
  ;;;===============================================================================
  
  ;; For compiler and REPL integration, see the procedures
  ;;
  ;;   - $ex:REPL            : Use this as REPL evaluator
  ;;   - $ex:EXPAND-FILE     : Use this to expand a file containing libraries and/or
  ;;                           toplevel programs before feeding result to a compiler
  ;;
  ;; Example compilation sequences, and REPL example, can be seen in examples.scm
  
  ;; IMPORTANT:
  ;; ----------
  ;; Generate-guid must return a fresh symbol that has a globally
  ;; unique external representation and is read-write invariant.
  ;; Your local gensym will probably not satisfy both conditions.
  ;; Using a character like "~" like here won't cut it either.
  ;; Uniqueness is important for incremental and separate
  ;; expansion.
  
  (define ex:generate-guid
    (let ((token (ex:unique-token))
          (ticks 0))
      (lambda (symbol)
        (set! ticks (+ ticks 1))
        (string->symbol
         (string-append (symbol->string symbol)
                        "~"
                        token
                        "~"
                        (number->string ticks))))))
  
  ;; IMPORTANT:
  ;; ----------
  ;; Used to generate user program toplevel names.
  ;; Result must be disjoint from all system primitive bindings.
  ;; Result must be disjoint from output of ex:generate-guid.
  ;; Must be read-write invariant.
  ;; Using a character like "~" like here won't cut it.
  
  (define (ex:free-name symbol)
    (string->symbol (string-append "~" (symbol->string symbol)))) 
  
  ;; IMPORTANT:
  ;; ----------
  ;; This maps library name (list of symbols) plus a postfix symbol
  ;; to a symbol that should be disjoint from all system bindings,
  ;; all results of ex:generate-guid, and all results of ex:free-name.
  ;; Must be read-write invariant.
  ;; If this mapping is changed, the definitions of the
  ;; bootstrap library (core primitive-macros) at the end of
  ;; this file must be changed to be consistent with this.  
  
  (define (ex:library->symbol postfix name)
    (ex:free-name (string->symbol (string-append (ex:list->string name ".")
                                                 "~"
                                                 (symbol->string postfix)))))
  
  ;;;==========================================================================
  ;;;
  ;;; Identifiers:
  ;;;
  ;;;==========================================================================
  
  ;; <name>             ::= <symbol>
  ;; <colors>           ::= (<color> ...)
  ;; <transformer-envs> ::= (<env> ...)
  ;; <displacement>     ::= <integer>
  ;; <maybe-library>    ::= (<symbol> ...) | #f
  ;;
  ;; where
  ;;   <name>             : The symbolic name of the identifier in the source.
  ;;   <colors>           : Each time an introduced identifier is renamed, a fresh
  ;;                        color gets prepended to its <colors>.
  ;;   <transformer-envs> : The environment (car <transformer-envs>) was the usage
  ;;                        environment valid during expansion of the (syntax id*) expression
  ;;                        introducing this identifier, while (cdr <transformer-envs>)
  ;;                        are in turn the <transformer-envs> of id*.
  ;;   <displacement>     : Integer that keeps track of shifts in phases
  ;;                        between transformer and usage sites of identifier.
  ;;   <maybe-library>    : Library name if identifier was introduced by evaluation of 
  ;;                        a (syntax ...) expression, otherwise #f.
  ;;                        The empty name '() is used for toplevel.  
  
  (define-record-type ex:identifier
    (ex:make-identifier name colors transformer-envs displacement maybe-library)
    ex:identifier?
    (name             ex:id-name)
    (colors           ex:id-colors)
    (transformer-envs ex:id-transformer-envs)
    (displacement     ex:id-displacement)
    (maybe-library    ex:id-maybe-library))
  
  (define (ex:id-library id)
    (or (ex:id-maybe-library id)
        (ex:*current-library*)))
  
  (define ex:*current-library* (make-parameter '()))
  
  (define (ex:bound-identifier=? x y)
    (ex:check x ex:identifier? 'bound-identifier=?)
    (ex:check y ex:identifier? 'bound-identifier=?)
    (and (eq? (ex:id-name x)
              (ex:id-name y))
         (equal? (ex:id-colors x)
                 (ex:id-colors y))))
  
  ;; As required by r6rs, when this returns, the result is #t
  ;; if and only if the two identifiers resolve to the same binding.
  ;; It also treats unbound identifiers specially.
  ;; As allowed by R6RS, included phase checking of arguments.
  ;; An out of phase error is raised if the comparison succeeds but
  ;; either argument is out of phase.  This is sufficient to ensure
  ;; that literals such as ... in syntax-case are used in the correct phase.
  ;; For more dicussion on this choice, see the readme and the examples file.  
  
  (define (ex:free-identifier=? x y)
    (ex:check x ex:identifier? 'free-identifier=?)
    (ex:check y ex:identifier? 'free-identifier=?)
    (let  ((bx (ex:binding x))
           (by (ex:binding y)))
      (let ((result (if bx
                        (and by
                             (eq? (ex:binding-name bx)
                                  (ex:binding-name by)))
                        (and (not by)
                             (eq? (ex:id-name x)
                                  (ex:id-name y))))))
        (and result
             bx
             (begin (ex:check-binding-level x bx)   
                    (ex:check-binding-level y by)))
        ;; A later definition in the same body can only change
        ;; #t to #f, so only record usage in that case.
        (and result
             (ex:register-use! x bx)
             (ex:register-use! y by))
        result)))
  
  ;; For internal use
  
  (define (ex:free=? x symbol)  
    (and (ex:identifier? x)
         (let  ((bx (ex:binding x)))
           (let ((result
                  (and bx
                       (eq? (ex:binding-name bx) symbol))))
             (and result
                  bx
                  (ex:check-binding-level x bx))
             (and result
                  (ex:register-use! x bx))
             result))))  
  
  ;; Returns <color> ::= globally unique symbol
  
  (define (ex:generate-color)
    (ex:generate-guid 'c))
  
  (define ex:*color* (make-parameter (ex:generate-color)))
  
  ;;;=========================================================================
  ;;;
  ;;; Bindings:
  ;;;
  ;;;=========================================================================
  
  ;; For simplicity, the rest of the expander assumes bindings are
  ;; s-expressions that are write-read invariant after replacing macro
  ;; objects by #f.
  
  ;; <type>    ::= variable | pattern-variable | macro
  ;; <name>    ::= <symbol>
  ;; <levels>  ::= (<integer> ...)
  ;; <content> ::= #f if immutable variable | imported macro
  ;;            |  #t if mutable variable
  ;;            |  <macro> if local macro
  ;;            |  <dimension> if pattern variable 
  
  (define (ex:make-binding type name levels content library)
    (list type name levels content library))
  
  (define ex:binding-type    car)
  (define ex:binding-name    cadr)
  (define ex:binding-levels  caddr)
  (define ex:binding-content cadddr)
  (define (ex:binding-content-set! b x) (set-car! (cdddr b) x)) 
  (define (ex:binding-library b) (car (cddddr b))) 
  
  ;; Looks up binding first in usage environment and 
  ;; then in attached transformer environments.  
  ;; Returns <binding> | #f if unbound.                         
  
  (define (ex:binding id)  
    (let ((name (ex:id-name id)))
      (let loop ((env    (ex:*usage-env*))  
                 (envs   (ex:id-transformer-envs id))
                 (colors (ex:id-colors id)))
        (or (ex:env-lookup (cons name colors) env #f)  
            (and (pair? envs)
                 (loop (car envs)
                       (cdr envs)
                       (cdr colors)))))))
  
  ;;;=========================================================================
  ;;;
  ;;; Mapping in environment: ((<name> <color> ...) . <binding>)
  ;;;
  ;;;=========================================================================
  
  ;; Generates a local mapping at the current meta-level
  ;; that can be added to the usage environment.
  ;; Returns <mapping>.
  
  (define (ex:make-local-mapping type id content)
    (cons (cons (ex:id-name id)
                (ex:id-colors id))
          (ex:make-binding type
                           (ex:generate-guid (ex:id-name id))
                           (list (ex:source-level id))
                           content
                           (ex:*current-library*))))
  
  ;; Toplevel binding forms use as binding name the free name  
  ;; so that source-level forward references will work in REPL.
  ;; If identifier is macro-generated, bind it with a fresh name.
  ;; This ensures that generated toplevel defines are not visible
  ;; from toplevel source code, thus approximating the behaviour
  ;; of generated internal definitions.
  ;; Returns <mapping>.
  
  (define (ex:make-toplevel-mapping type id content)   
    (if (null? (ex:id-colors id))
        (cons (cons (ex:id-name id)
                    (ex:id-colors id))
              (ex:make-binding type
                               (ex:free-name (ex:id-name id))
                               '(0) 
                               content
                               (ex:*current-library*)))
        (ex:make-local-mapping type id content)))
  
  ;;;=========================================================================
  ;;;
  ;;; Infrastructure for binding levels:
  ;;;
  ;;;=========================================================================
  
  ;; The phase for the current expansion step:
  
  (define ex:*phase* (make-parameter 0))
  
  (define (ex:source-level id)    
    (- (ex:*phase*)
       (ex:id-displacement id)))
  
  (define (ex:check-binding-level id binding)  
    (if (not binding)
        (or (and (null? (ex:id-library id))
                 (= (ex:*phase*) 0))
            (ex:syntax-violation       
             "invalid reference"
             (string-append "No binding available for " (symbol->string (ex:id-name id))
                            " in library (" (ex:list->string (ex:id-library id) " ") ")")
             
             id))
        (or (memv (ex:source-level id)
                  (ex:binding-levels binding))
            (ex:syntax-violation       
             "invalid reference"
             (string-append "Attempt to use binding of " (symbol->string (ex:id-name id))
                            " in library (" (ex:list->string (ex:id-library id) " ")
                            ") at invalid level " (number->string (ex:source-level id))
                            ".  Binding is only available at levels: "
                            (ex:list->string (ex:binding-levels binding) " "))
             id))))
  
  ;;;=========================================================================
  ;;;
  ;;; Environments:
  ;;;
  ;;;=========================================================================
  
  ;; An environment consists of a non-empty sequence of frames.
  ;; The leftmost frame can be destructively extended.
  
  (define (ex:make-unit-env) (list (ex:make-frame '())))
  
  ;; Adds a new frame containing mappings to env.
  
  (define (ex:env-extend mappings env)
    (cons (ex:make-frame mappings) env))
  
  ;; Destructively extends the leftmost frame in env.
  
  (define (ex:env-extend! mappings env)
    (ex:frame-extend! mappings (car (ex:env-reify env))))
  
  ;; Returns <object> | default
  
  (define (ex:env-lookup key env default)
    (let ((env (ex:env-reify env)))
      (cond ((null? env) default)
            ((ex:frame-lookup key (car env)) => cdr)
            (else
             (ex:env-lookup key (cdr env) default)))))
  
  ;; Is id already bound in leftmost frame?
  
  (define (ex:duplicate? id env)
    (assoc (cons (ex:id-name id)
                 (ex:id-colors id))
           (ex:unbox (car (ex:env-reify env)))))
  
  (define (ex:make-frame mappings) (ex:box mappings))
  
  (define (ex:frame-extend! mappings frame)
    (ex:set-box! frame (append mappings (ex:unbox frame))))
  
  (define (ex:frame-lookup key frame)
    (assoc key (ex:unbox frame)))
  
  (define ex:box      list)
  (define ex:unbox    car)
  (define ex:set-box! set-car!)
  
  ;; Table of the form ((<key> . <env>) ...)
  
  (define ex:*table-of-envs* (make-parameter '()))
  
  ;; Returns a single-symbol <key> representating an 
  ;; environment that can be included in object code.
  
  (define (ex:env-reflect env)
    (cond ((symbol? env) env)
          ((and (not (null? (ex:*table-of-envs*)))      ;+++
                (eq? env (cdar (ex:*table-of-envs*))))  ;+++
           (caar (ex:*table-of-envs*)))                 ;+++
          (else
           (let ((key (ex:generate-guid 'env)))
             (ex:*table-of-envs*
              (cons (cons key env)
                    (ex:*table-of-envs*)))
             key))))
  
  ;; The inverse of the above.
  
  (define (ex:env-reify env) 
    (if (symbol? env)
        (cdr (assq env (ex:*table-of-envs*)))
        env))
  
  (define (ex:extend-table-of-envs! envs)
    (ex:*table-of-envs*
     (append envs (ex:*table-of-envs*))))
  
  ;;;=========================================================================
  ;;;
  ;;; Syntax-reflect and syntax-rename:
  ;;;
  ;;; This is the basic building block of the implicit renaming mechanism for
  ;;; maintaining hygiene.  Syntax-reflect generates the expanded code for
  ;;; (syntax id), including the expand-time environment in the
  ;;; external representation.  It expands to syntax-rename, which performs
  ;;; the implicit renaming when this expanded code is eventually run.
  ;;; The displacement computations calculate the difference between the 
  ;;; usage phase and the transformer phase.  
  ;;;
  ;;;=========================================================================
  
  (define (ex:syntax-reflect id)
    (ex:*syntax-reflected* #t)
    `($ex:syntax-rename ',(ex:id-name id)
                        ',(ex:id-colors id)
                        ',(ex:capture-transformer-envs id)
                        ,(- (ex:*phase*) (ex:id-displacement id) 1)
                        ',(ex:id-library id)))
  
  (define (ex:syntax-rename name colors transformer-envs transformer-phase source-library)
    (ex:make-identifier name
                        (cons (ex:*color*) colors)
                        transformer-envs
                        (- (ex:*phase*) transformer-phase)
                        source-library))
  
  (define (ex:capture-transformer-envs id)                
    (cons (ex:env-reflect (ex:*usage-env*))
          (ex:id-transformer-envs id)))
  
  ;; Will be #t if expanded library contains compiled SYNTAX expressions.
  ;; If not, we can save a lot of space by not including environment
  ;; table in object code.
  
  (define ex:*syntax-reflected* (make-parameter #f)) ; +++ space
  
  ;;;=====================================================================
  ;;;
  ;;; Capture and sexp <-> syntax conversions:
  ;;;
  ;;;=====================================================================
  
  (define (ex:datum->syntax tid datum)
    (ex:check tid ex:identifier? 'datum->syntax)
    (ex:sexp-map (lambda (leaf)
                   (cond ((symbol? leaf) 
                          (ex:make-identifier leaf
                                              (ex:id-colors tid)
                                              (ex:id-transformer-envs tid)
                                              (ex:id-displacement tid)
                                              (ex:id-maybe-library tid)))
                         (else leaf)))
                 datum))
  
  (define (ex:syntax->datum exp)
    (ex:sexp-map (lambda (leaf)
                   (cond ((ex:identifier? leaf) (ex:id-name leaf))
                         ((symbol? leaf)
                          (assertion-violation 'syntax->datum "A symbol is not a valid syntax object" leaf))
                         (else leaf)))
                 exp))
  
  ;; Fresh identifiers:
  
  (define (ex:generate-temporaries ls)
    (ex:check ls list? 'generate-temporaries)
    (map (lambda (ignore)
           (ex:rename 'variable (ex:generate-guid 'gen)))       
         ls))
  
  ;; For use internally as in the explicit renaming system.
  
  (define (ex:rename type symbol)
    (ex:make-identifier symbol
                        (list (ex:*color*))
                        (list (ex:env-extend
                               (list (cons (list symbol)
                                           (ex:make-binding type symbol '(0) #f '())))
                               (ex:make-unit-env)))
                        (ex:*phase*)
                        #f))
  
  ;;;=========================================================================
  ;;;
  ;;; Macro objects:
  ;;;
  ;;;=========================================================================
  
  ;; Expanders are system macros that fully expand
  ;; their arguments to core Scheme, while
  ;; transformers and variable transformers are
  ;; user macros.
  
  ;; <type> ::= expander | transformer | variable-transformer
  
  (define-record-type ex:macro (ex:make-macro type proc) ex:macro?
    (type ex:macro-type)
    (proc ex:macro-proc))
  
  (define (ex:make-expander proc)             (ex:make-macro 'expander proc))
  (define (ex:make-transformer proc)          (ex:make-macro 'transformer proc))
  (define (ex:make-variable-transformer proc) (ex:make-macro 'variable-transformer proc))
  
  (define (ex:make-user-macro procedure-or-macro)
    (if (procedure? procedure-or-macro)
        (ex:make-transformer procedure-or-macro)
        procedure-or-macro))
  
  ;; Toplevel table for imported macros:
  ;; Since macros contain procedures, they are removed from
  ;; the external representation of exported environments.
  ;; Instead, the client will look for them in this table,
  ;; which will be populated upon visiting the exported library.
  
  (define ex:*macro-env* (make-parameter '()))
  
  ;; If binding-content is #f, this must be an imported
  ;; macro, in which case we fetch it from the toplevel
  ;; table.  
  ;; Returns <macro>.
  
  (define (ex:binding-macro binding)
    (or (ex:binding-content binding)
        (let ((probe (assq (ex:binding-name binding) (ex:*macro-env*))))
          (if probe
              (cdr probe)
              (ex:syntax-violation #f "Reference to macro keyword out of context" (ex:binding-name binding))))))
  
  ;; Registering imported macro.
  
  (define (ex:register-macro! binding-name procedure-or-macro)
    (ex:*macro-env* (cons (cons binding-name (ex:make-user-macro procedure-or-macro))
                          (ex:*macro-env*))))
  
  ;;;=========================================================================
  ;;;
  ;;; Expander dispatch:
  ;;;
  ;;;=========================================================================
  
  (define (ex:expand t)
    (ex:trace
     t
     (lambda ()
       (let ((binding (ex:operator-binding t)))
         (cond (binding (case (ex:binding-type binding)
                          ((macro)
                           (let ((macro (ex:binding-macro binding)))
                             (ex:*color* (ex:generate-color))
                             (let ((expanded-once ((ex:macro-proc macro) t)))
                               (case (ex:macro-type macro)
                                 ((expander) expanded-once)
                                 (else
                                  (ex:expand expanded-once))))))
                          ((variable)
                           (check-implicit-import-of-mutable binding t)
                           (if (list? t) 
                               (cons (ex:binding-name binding)
                                     (map ex:expand (cdr t)))
                               (ex:binding-name binding)))
                          ((pattern-variable)
                           (ex:syntax-violation #f "Pattern variable used outside syntax template" t))))
               ((list? t)          (map ex:expand t))
               ((ex:identifier? t) (ex:free-name (ex:id-name t)))
               ((pair? t)          (ex:syntax-violation #f "Invalid procedure call syntax" t))
               ((symbol? t)        (ex:syntax-violation #f "Symbol may not appear in syntax object" t))
               (else t))))))
  
  ;; Only expands while t is a user macro invocation.
  ;; Used by expand-lambda to detect internal definitions.
  
  (define (ex:head-expand t) 
    (ex:trace
     t
     (lambda ()
       (let ((binding (ex:operator-binding t)))
         (cond (binding (case (ex:binding-type binding)
                          ((macro)
                           (let ((macro (ex:binding-macro binding)))
                             (ex:*color* (ex:generate-color))
                             (case (ex:macro-type macro)
                               ((expander) (values t binding))
                               (else
                                (ex:head-expand ((ex:macro-proc macro) t))))))
                          (else (values t binding))))
               (else (values t binding)))))))
  
  ;; Returns binding of identifier in operator position | #f if none.
  ;; Singleton identifiers are also considered operators for
  ;; the purpose of discovering identifier macros and variables. 
  ;; Checks availability and registers as a use.
  
  (define (ex:operator-binding form)  
    (let ((operator (if (pair? form) (car form) form)))
      (and (ex:identifier? operator)
           (let ((binding (ex:binding operator)))
             (ex:check-binding-level operator binding)
             (ex:register-use! operator binding)
             binding))))
  
  ;; We cannot implicitly import a mutable variable.
  
  (define (check-implicit-import-of-mutable binding t)
    (or (equal? (ex:binding-library binding) (ex:*current-library*))
        (not (ex:binding-content binding))
        (ex:syntax-violation
         #f 
         (string-append "Attempt to implicitly import variable that is mutable in library (" 
                        (ex:list->string (ex:binding-library binding) " ") ")")
         t)))
  
  ;;;=========================================================================
  ;;;
  ;;; Quote, if, set!, expression begin, expression let[rec]-syntax, and, or:
  ;;;
  ;;;=========================================================================
  
  (define (ex:expand-quote exp)
    (or (and (list? exp)
             (= (length exp) 2))
        (ex:invalid-form exp))
    (ex:syntax->datum exp))
  
  (define (ex:expand-if exp)
    (or (and (list? exp)
             (<= 3 (length exp) 4))
        (ex:invalid-form exp))
    `(if ,(ex:expand (cadr exp))
         ,(ex:expand (caddr exp))
         ,@(if (= (length exp) 4)
               (list (ex:expand (cadddr exp)))
               `())))
  
  (define (ex:expand-set! exp)           
    (or (and (list? exp)
             (= (length exp) 3)
             (ex:identifier? (cadr exp)))
        (ex:invalid-form exp))
    (let ((binding (ex:binding (cadr exp))))
      (ex:check-binding-level (cadr exp) binding)
      (ex:register-use! (cadr exp) binding)
      (case (ex:binding-type binding)
        ((macro)
         (let ((macro (ex:binding-macro binding)))
           (case (ex:macro-type macro)
             ((variable-transformer)
              (ex:expand ((ex:macro-proc macro) exp)))
             (else
              (ex:syntax-violation 'set! "Keyword being set! is not a variable transformer" exp (cadr exp))))))
        ((variable)
         (or (eq? (ex:binding-library binding) (ex:*current-library*))
             (ex:syntax-violation 'set! "Directly or indirectly imported variable cannot be assigned" exp (cadr exp)))
         ;; Set binding mutable.
         (ex:binding-content-set! binding #t)
         `(set! ,(ex:binding-name binding)       
                ,(ex:expand (caddr exp))))
        ((pattern-variable)
         (ex:syntax-violation 'set! "Pattern variable used outside syntax template" exp (cadr exp))))))
  
  ;; Expression begin.
  
  (define (ex:expand-begin exp)
    (or (and (list? exp)
             (not (null? (cdr exp))))
        (ex:invalid-form exp))
    (ex:scan-sequence 'expression-sequence
                      #f
                      (cdr exp)
                      (lambda (forms no-syntax-definitions no-bound-variables)
                        `(begin ,@(map cdr forms)))))
  
  ;; Expression let(rec)-syntax:
  
  (define (ex:expand-local-syntax t)
    (ex:expand-begin `(,(ex:rename 'macro 'begin) ,t)))
  
  ;; And and or must be primitive, since they are also part of the library
  ;; language, which is primitive.
  
  (define (ex:expand-and exp)
    (or (list? exp)
        (ex:invalid-form exp))
    (cond ((null? (cdr exp))  #t)
          ((null? (cddr exp)) (ex:expand (cadr exp)))
          (else
           `(if ,(ex:expand (cadr exp))
                ,(ex:expand `(,(ex:rename 'macro 'and) ,@(cddr exp)))
                #f))))
  
  (define (ex:expand-or exp)
    (or (list? exp)
        (ex:invalid-form exp))
    (cond ((null? (cdr exp))  #f)
          ((null? (cddr exp)) (ex:expand (cadr exp)))
          (else
           `(let ((x ,(ex:expand (cadr exp))))
              (if x x ,(ex:expand `(,(ex:rename 'macro 'or) ,@(cddr exp))))))))
  
  ;;;=========================================================================
  ;;;
  ;;; Lambda:
  ;;;
  ;;;=========================================================================
  
  (define (ex:expand-lambda exp)
    (or (and (pair?    exp)
             (pair?    (cdr exp))
             (ex:formals? (cadr exp))
             (list?    (cddr exp)))
        (ex:invalid-form exp))
    (let ((formals (cadr exp))
          (body    (cddr exp)))
      (parameterize ((ex:*usage-env* 
                      (ex:env-extend (map (lambda (formal)
                                            (ex:make-local-mapping 'variable formal #f))
                                          (ex:flatten formals))
                                     (ex:*usage-env*))))
        (let ((formals (ex:dotted-map (lambda (formal) (ex:binding-name (ex:binding formal))) formals)))
          ;; Scan-sequence expects the caller to have prepared 
          ;; the frame to which to destructively add bindings.
          ;; Lambda bodies need a fresh frame.  
          (parameterize ((ex:*usage-env* (ex:env-extend '() (ex:*usage-env*))))  
            (ex:scan-sequence 'lambda
                              ex:make-local-mapping
                              body
                              (lambda (forms syntax-definitions bound-variables)
                                `(lambda ,formals
                                   ((lambda ,bound-variables
                                      ,@(ex:emit-body forms 'set!))
                                    ,@(map (lambda (ignore) `($ex:unspecified))
                                           bound-variables))))))))))
  
  (define (ex:formals? s)
    (or (null? s)
        (ex:identifier? s)
        (and (pair? s)
             (ex:identifier? (car s))
             (ex:formals? (cdr s))
             (not (ex:dotted-memp (lambda (x)
                                    (ex:bound-identifier=? x (car s)))
                                  (cdr s))))))
  
  ;;=========================================================================
  ;;;
  ;;; Bodies and sequences:
  ;;;
  ;;;=========================================================================
  
  ;; R6RS splicing of internal let-syntax and letrec-syntax
  ;; requires that we remember the bindings visible in each
  ;; form for later expansion of deferred right hand sides
  ;; and expressions.  This is done by attaching
  ;; the environment to the expression.
  ;; We call the resulting data structure a wrap.
  ;; Wraps are only used internally in processing of bodies,
  ;; and are never seen by user macros.
  
  (define-record-type ex:wrap (ex:make-wrap env exp) ex:wrap?         
    (env ex:wrap-env)
    (exp ex:wrap-exp))
  
  ;; The continuation k is evaluated in the body environment.  This is
  ;; used for example by expand-library to obtain the correct bindings of
  ;; exported identifiers.
  ;;
  ;; <body-type> ::= toplevel | library | program | lambda | expression-sequence
  ;;
  ;; All but TOPLEVEL are as in r6rs. 
  ;; TOPLEVEL is meant for the REPL.
  ;; At TOPLEVEL, we may have a sequence of expressions, definitions, macros,
  ;; import declarations, libraries and programs wrapped in (program ---).
  ;; Redefinitions are allowed at toplevel.
  
  (define (ex:scan-sequence body-type make-map body-forms k)   
    
    ;; Each <form> ::= (<symbol> . <wrap or s-expr>)  (definition)
    ;;              |  (#f       . <wrap or s-expr>)  (expression)
    (define (expand-deferred forms)
      (map (lambda (form)
             (cons (car form)
                   (let ((exp (cdr form)))
                     (if (ex:wrap? exp)
                         (parameterize ((ex:*usage-env* (ex:wrap-env exp)))
                           (ex:expand (ex:wrap-exp exp)))
                         exp))))
           forms))
    
    (let ((common-env (ex:*usage-env*)))
      
      ;; Add new frame for keeping track of bindings used
      ;; so we can detect redefinitions violating lexical scope.
      (ex:add-fresh-used-frame!)
      
      (let loop ((ws (map (lambda (e) (ex:make-wrap common-env e))
                          body-forms))
                 (forms           '())
                 (syntax-defs     '())
                 (bound-variables '()))
        (cond
         ((null? ws)
          (ex:check-expression-body body-type forms body-forms)              
          ;; Add denotations used in this frame to those of parent.
          (ex:merge-used-with-parent-frame!)            
          (k (reverse (expand-deferred forms))
             (reverse syntax-defs)
             bound-variables))
         (else
          (parameterize ((ex:*usage-env* (ex:wrap-env (car ws))))
            (let-values (((form operator-binding) (ex:head-expand (ex:wrap-exp (car ws)))))
              (let ((type (and operator-binding (ex:binding-name operator-binding))))
                (ex:check-expression-sequence body-type type form)
                (ex:check-toplevel            body-type type form)
                (case type
                  ((import)
                   (let-values (((imported-libraries imports) (ex:scan-imports form)))
                     (ex:import-libraries imported-libraries 0 'compile)
                     (ex:env-import! (car form) imports common-env)
                     (loop (cdr ws)
                           (cons (cons #f `($ex:import-libraries ',imported-libraries 0 'execute))
                                 forms)
                           syntax-defs
                           bound-variables)))
                  ((program)
                   (loop (cdr ws)
                         (cons (cons #f (ex:expand-program form)) forms)
                         syntax-defs
                         bound-variables))
                  ((library)
                   (loop (cdr ws)
                         (cons (cons #f (ex:expand-library form)) forms)
                         syntax-defs
                         bound-variables))
                  ((define)
                     (let-values (((id rhs) (ex:parse-definition form)))
                       (ex:check-valid-definition id common-env body-type form forms type)
                       (ex:env-extend! (list (make-map 'variable id #f)) common-env)
                       (loop (cdr ws)
                             (cons (cons (ex:binding-name (ex:binding id))
                                         (ex:make-wrap (ex:*usage-env*) rhs))
                                   forms)
                             syntax-defs
                             (cons (ex:binding-name (ex:binding id)) bound-variables)))) 
                  ((define-syntax)
                   (let-values (((id rhs) (ex:parse-definition form)))
                     (ex:check-valid-definition id common-env body-type form forms type)
                     (let ((mapping (make-map 'macro id #f)))
                       (ex:env-extend! (list mapping) common-env)
                       (let ((rhs (parameterize ((ex:*phase* (+ 1 (ex:*phase*))))
                                    (ex:expand rhs))))
                         (ex:binding-content-set! (cdr mapping) (ex:make-user-macro (eval rhs))) 
                         (loop (cdr ws)
                               forms
                               (cons (cons (ex:binding-name (ex:binding id)) rhs) syntax-defs)
                               bound-variables)))))
                  ((begin)  
                   (or (list? form)
                       (ex:invalid-form form))
                   (loop (append (map (lambda (exp)
                                        (ex:make-wrap (ex:*usage-env*) exp))
                                      (cdr form))
                                 (cdr ws))
                         forms
                         syntax-defs
                         bound-variables))
                  ((let-syntax letrec-syntax)
                   (let-values (((formals rhs body) (ex:parse-local-syntax form)))
                     (let* ((original-env (ex:*usage-env*))
                            (usage-diff   (map (lambda (formal)
                                                 (ex:make-local-mapping 'macro formal #f))
                                               formals))
                            (extended-env (ex:env-extend usage-diff original-env))
                            (rhs-expanded
                             (parameterize ((ex:*phase* (+ 1 (ex:*phase*)))
                                            (ex:*usage-env*
                                             (case type
                                               ((let-syntax)    original-env)
                                               ((letrec-syntax) extended-env))))
                               (map ex:expand rhs)))
                            (macros (map eval rhs-expanded))) 
                       (for-each (lambda (mapping macro)
                                   (ex:binding-content-set! (cdr mapping) (ex:make-user-macro macro)))
                                 usage-diff
                                 macros)
                       (loop (append (map (lambda (form) (ex:make-wrap extended-env form))
                                          body)
                                     (cdr ws))
                             forms
                             syntax-defs
                             bound-variables))))
                  (else
                   (loop (cdr ws)
                         (cons (cons #f (ex:make-wrap (ex:*usage-env*) form))
                               forms)
                         syntax-defs
                         bound-variables)))))))))))
  
  (define (ex:emit-body body-forms define-or-set)
    (map (lambda (body-form)
           (if (symbol? (car body-form))
               `(,define-or-set ,(car body-form) ,(cdr body-form))
               (cdr body-form)))
         body-forms))
  
  (define (ex:parse-definition t)
    (or (and (pair? t)
             (pair? (cdr t)))
        (ex:syntax-violation #f "Invalid definition format" t))
    (let ((k    (car t))
          (head (cadr t))
          (body (cddr t)))
      (cond ((and (ex:identifier? head)
                  (list? body)
                  (<= (length body) 1))
             (values head (if (null? body)
                              `(,(ex:rename 'variable '$ex:unspecified))
                              (car body))))
            ((and (pair? head)
                  (ex:identifier? (car head))
                  (ex:formals? (cdr head)))
             (values (car head)
                     `(,(ex:rename 'macro 'lambda) ,(cdr head) . ,body)))
            (else (ex:syntax-violation #f "Invalid definition format" t)))))
  
  (define (ex:parse-local-syntax t)
    (or (and (pair? t)
             (pair? (cdr t))
             (list? (cadr t))
             (list? (cddr t))
             (for-all (lambda (binding)
                        (and (pair? binding)
                             (ex:identifier? (car binding))
                             (pair? (cdr binding))
                             (null? (cddr binding))))
                      (cadr t)))
        (ex:syntax-violation #f "Invalid form" t))
    (let ((formals (map car (cadr t)))
          (exps    (map cadr (cadr t)))
          (body    (cddr t)))
      (or (ex:formals? formals)
          (ex:syntax-violation #f "Duplicate binding" t))
      (values formals
              exps
              body)))
  
  (define (ex:check-expression-sequence body-type type form)
    (and (eq? body-type 'expression-sequence)
         (memq type '(import program library define define-syntax))
         (ex:syntax-violation type "Invalid form in expression sequence" form)))
  
  (define (ex:check-toplevel body-type type form)
    (and (not (eq? body-type 'toplevel))
         (memq type '(import program library))
         (ex:syntax-violation type "Expression may only occur at toplevel" form)))

  (define (ex:check-valid-definition id common-env body-type form forms type)
    (and (not (eq? body-type 'toplevel))
         (ex:duplicate? id common-env)
         (ex:syntax-violation type "Redefinition of identifier in body" form id))
    (ex:check-used id body-type form)
    (and (not (memq body-type `(toplevel program)))
         (not (null? forms))
         (not (symbol? (car (car forms))))
         (ex:syntax-violation type "Definitions may not follow expressions in a body" form)))
  
  (define (ex:check-expression-body body-type forms body-forms)
    (and (eq? body-type 'lambda)
         (or (null? forms)
             (symbol? (caar forms)))
         (ex:syntax-violation body-type "Body must be nonempty and end with an expression" body-forms)))
  
  ;;;=========================================================================
  ;;;
  ;;; Syntax-case:
  ;;;
  ;;;=========================================================================
  
  (define (ex:expand-syntax-case exp)
    (if (and (list? exp)
             (>= (length exp) 3))
        (let ((literals (caddr exp))
              (clauses (cdddr exp)))
          (if (and (list? literals)
                   (for-all ex:identifier? literals)
                   (not (memp (lambda (x) (or (ex:free=? x '_) 
                                              (ex:free=? x '...)))
                              literals)))
              (let ((input (ex:generate-guid 'input)))
                `(let ((,input ,(ex:expand (cadr exp))))
                   ,(ex:process-clauses clauses input literals)))
              (ex:syntax-violation 'syntax-case "Invalid literals list" exp literals)))
        (ex:invalid-form exp)))
  
  (define (ex:process-clauses clauses input literals)
    
    (define (process-match input pattern sk fk)
      (cond
        ((not (symbol? input)) (let ((temp (ex:generate-guid 'temp)))  
                                 `(let ((,temp ,input))
                                    ,(process-match temp pattern sk fk))))
        ((and (ex:identifier? pattern)
              (memp (lambda (x)
                      (ex:bound-identifier=? x pattern))
                    literals))
         `(if (and ($ex:identifier? ,input)
                   ($ex:free-identifier=? ,input ,(ex:syntax-reflect pattern)))
              ,sk
              ,fk))
        ((ex:ellipses? pattern)   (ex:syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
        ((null? pattern)          `(if (null? ,input) ,sk ,fk))
        ((ex:wildcard? pattern)   sk)
        ((ex:identifier? pattern) `(let ((,(ex:binding-name (ex:binding pattern)) ,input)) ,sk))    
        ((ex:segment-pattern? pattern)
         (let ((tail-pattern (cddr pattern)))
           (if (null? tail-pattern)
               (let ((mapped-pvars (map (lambda (pvar) (ex:binding-name (ex:binding pvar)))   
                                        (map car (pattern-vars (car pattern) 0)))))
                 (if (and (ex:identifier? (car pattern))                                ; +++
                          (= (length mapped-pvars) 1))                                       ; +++
                     `(if (list? ,input)                                                ; +++
                          (let ((,(car mapped-pvars) ,input))                           ; +++  
                            ,sk)                                                        ; +++
                          ,fk)                                                          ; +++
                     (let ((columns (ex:generate-guid 'cols))
                           (rest    (ex:generate-guid 'rest)))
                       `($ex:map-while (lambda (,input)
                                         ,(process-match input
                                                         (car pattern)
                                                         `(list ,@mapped-pvars)
                                                         #f))
                                       ,input
                                       (lambda (,columns ,rest)
                                         (if (null? ,rest)
                                             (apply (lambda ,mapped-pvars ,sk)
                                                    (if (null? ,columns)
                                                        ',(map (lambda (ignore) '()) mapped-pvars)
                                                        (apply map list ,columns)))
                                             ,fk))))))
               (let ((tail-length (ex:dotted-length tail-pattern)))
                 `(if (>= ($ex:dotted-length ,input) ,tail-length)
                      ,(process-match `($ex:dotted-butlast ,input ,tail-length)
                                      `(,(car pattern) ,(cadr pattern))
                                      (process-match `($ex:dotted-last ,input ,tail-length)
                                                     (cddr pattern)
                                                     sk
                                                     fk)
                                      fk)
                      ,fk)))))
        ((pair? pattern)   `(if (pair? ,input)
                                ,(process-match `(car ,input)
                                                (car pattern)
                                                (process-match `(cdr ,input) (cdr pattern) sk fk)
                                                fk)
                                ,fk))
        ((vector? pattern) `(if (vector? ,input)
                                ,(process-match `(vector->list ,input)
                                                (vector->list pattern)
                                                sk
                                                fk)
                                ,fk))
        ((symbol? pattern) (ex:syntax-violation 'syntax-case "Invalid pattern" pattern))
        (else              `(if (equal? ,input ',pattern) ,sk ,fk))))
    
    (define (pattern-vars pattern level)
      (cond
        ((ex:identifier? pattern)      (if (or (ex:ellipses? pattern)
                                               (ex:wildcard? pattern)
                                               (memp (lambda (x)
                                                       (ex:bound-identifier=? x pattern))
                                                     literals))
                                           '()
                                           (list (cons pattern level))))
        ((ex:segment-pattern? pattern) (append (pattern-vars (car pattern) (+ level 1))
                                               (pattern-vars (cddr pattern) level)))
        ((pair? pattern)               (append (pattern-vars (car pattern) level)
                                               (pattern-vars (cdr pattern) level)))
        ((vector? pattern)             (pattern-vars (vector->list pattern) level))
        (else                          '())))
    
    (define (process-clause clause input fk)
      (or (and (list? clause)
               (>= (length clause) 2))
          (ex:syntax-violation 'syntax-case "Invalid clause" clause))
      (let* ((pattern  (car clause))
             (template (cdr clause))
             (pvars    (pattern-vars pattern 0)))
        (ex:check-set? (map car pvars)
                       ex:bound-identifier=?
                       (lambda (dup)
                         (ex:syntax-violation 'syntax-case "Repeated pattern variable" clause dup)))
        (let ((mappings (map (lambda (pvar)
                               (ex:make-local-mapping 'pattern-variable (car pvar) (cdr pvar)))  
                             pvars)))
          (parameterize ((ex:*usage-env* (ex:env-extend mappings (ex:*usage-env*))))
            (process-match input
                           pattern
                           (cond ((null? (cdr template))
                                  (ex:expand (car template)))
                                 ((null? (cddr template))
                                  `(if ,(ex:expand (car template))
                                       ,(ex:expand (cadr template))
                                       ,fk))
                                 (else (ex:syntax-violation 'syntax-case "Invalid clause" clause)))
                           fk)))))
    
    ;; process-clauses
    
    (if (null? clauses)
        `($ex:invalid-form ,input)
        (let ((fail  (ex:generate-guid 'fail)))
          `(let ((,fail (lambda () ,(ex:process-clauses (cdr clauses) input literals))))
             ,(process-clause (car clauses) input `(,fail))))))
  
  (define (ex:wildcard? x) (ex:free=? x '_))
  
  ;; Ellipsis utilities:
  
  (define (ex:ellipses? x) (ex:free=? x '...))
  
  (define (ex:segment-pattern? pattern)
    (and (ex:segment-template? pattern)
         (or (for-all (lambda (p)
                        (not (ex:ellipses? p)))
                      (ex:flatten (cddr pattern)))
             (ex:syntax-violation 'syntax-case "Invalid segment pattern" pattern))))
  
  (define (ex:segment-template? pattern)
    (and (pair? pattern)
         (pair? (cdr pattern))
         (ex:identifier? (cadr pattern))
         (ex:ellipses? (cadr pattern))))
  
  ;; Count the number of `...'s in PATTERN.
  
  (define (ex:segment-depth pattern)
    (if (ex:segment-template? pattern)
        (+ 1 (ex:segment-depth (cdr pattern)))
        0))
  
  ;; Get whatever is after the `...'s in PATTERN.
  
  (define (ex:segment-tail pattern)
    (let loop ((pattern (cdr pattern)))
      (if (and (pair? pattern)
               (ex:identifier? (car pattern))
               (ex:ellipses? (car pattern)))
          (loop (cdr pattern))
          pattern)))
  
  ;; Ellipses-quote:
  
  (define (ex:ellipses-quote? template)
    (and (pair? template)
         (ex:ellipses? (car template))
         (pair? (cdr template))
         (null? (cddr template))))
  
  ;;;=========================================================================
  ;;;
  ;;; Syntax:
  ;;;
  ;;;=========================================================================
  
  (define (ex:expand-syntax form)
    (or (and (pair? form)
             (pair? (cdr form))
             (null? (cddr form)))
        (ex:invalid-form form))
    (ex:process-template (cadr form) 0 #f))
  
  (define (ex:process-template template dim quote-ellipses)
    (cond ((and (ex:ellipses? template)
                (not quote-ellipses))
           (ex:syntax-violation 'syntax "Invalid occurrence of ellipses in syntax template" template))
          ((ex:identifier? template)
           (let ((binding (ex:binding template)))
             (cond ((and binding
                         (eq? (ex:binding-type binding) 'pattern-variable)   
                         (ex:binding-content binding))
                    => (lambda (pdim)  
                         (if (<= pdim dim)
                             (begin
                               (ex:check-binding-level template binding)    
                               (ex:register-use! template binding)
                               (ex:binding-name binding)) 
                             (ex:syntax-violation 'syntax "Template dimension error (too few ...'s?)" template))))
                   (else 
                    (ex:syntax-reflect template)))))
          ((ex:ellipses-quote? template)
           (ex:process-template (cadr template) dim #t))
          ((and (ex:segment-template? template)
                (not quote-ellipses))
           (let* ((depth (ex:segment-depth template))
                  (seg-dim (+ dim depth))
                  (vars
                   (map (lambda (mapping)
                          (let ((id      (car mapping))
                                (binding (cdr mapping)))
                            (ex:check-binding-level id binding)           
                            (ex:register-use! id binding)   
                            (ex:binding-name binding))) 
                        (ex:free-meta-variables (car template) seg-dim '()))))
             (if (null? vars)
                 (ex:syntax-violation 'syntax "Too many ...'s" template)
                 (let* ((x (ex:process-template (car template) seg-dim quote-ellipses))
                        (gen (if (equal? (list x) vars)   ; +++
                                 x                        ; +++
                                 `(map (lambda ,vars ,x)
                                       ,@vars)))
                        (gen (do ((d depth (- d 1))
                                  (gen gen `(apply append ,gen)))
                               ((= d 1)
                                gen))))
                   (if (null? (ex:segment-tail template))
                       gen                                ; +++
                       `(append ,gen ,(ex:process-template (ex:segment-tail template) dim quote-ellipses)))))))
          ((pair? template)
           `(cons ,(ex:process-template (car template) dim quote-ellipses)
                  ,(ex:process-template (cdr template) dim quote-ellipses)))
          ((vector? template)
           `(list->vector ,(ex:process-template (vector->list template) dim quote-ellipses)))
          (else
           `(quote ,(ex:expand template)))))
  
  ;; Return a list of meta-variables of given higher dim
  
  (define (ex:free-meta-variables template dim free)
    (cond ((ex:identifier? template)
           (if (memp (lambda (x) (ex:bound-identifier=? (car x) template))
                     free)
               free
               (let ((binding (ex:binding template)))  
                 (if (and binding
                          (eq? (ex:binding-type binding) 'pattern-variable)   
                          (let ((pdim (ex:binding-content binding)))
                            (>= pdim dim)))
                     (cons (cons template binding) free)
                     free))))
          ((ex:segment-template? template)
           (ex:free-meta-variables (car template) dim
                                   (ex:free-meta-variables (cddr template) dim free)))
          ((pair? template)
           (ex:free-meta-variables (car template) dim
                                   (ex:free-meta-variables (cdr template) dim free)))
          (else free)))
  
  ;;;=========================================================================
  ;;;
  ;;; Detecting violations of lexical scope.
  ;;;
  ;;;=========================================================================
  
  ;; For avoiding giving lexically invalid semantics to body     
  ;; sequences according to the following semantics described in r6rs:
  ;; A definition in the sequence of forms must not define any
  ;; identifier whose binding is used to determine the meaning of the
  ;; undeferred portions of the definition or any definition that precedes
  ;; it in the sequence of forms. 
  ;; This implementation treats a possble violation of the restriction
  ;; as a syntax violation.
  
  ;; Keeps track of bindings used so we can detect
  ;; redefinitions violating lexical scope in body sequences.    
  ;; The car of (ex:*used*) contains bindings used in
  ;; current frame.
  
  (define ex:*used* (make-parameter (list '())))
  
  (define (ex:add-fresh-used-frame!)
    (ex:*used* (cons '() (ex:*used*))))
  
  (define (ex:register-use! id binding)  
    (ex:*used* (cons (cons (cons id binding)   
                           (car (ex:*used*)))        
                     (cdr (ex:*used*)))))
  
  (define (ex:merge-used-with-parent-frame!)
    (ex:*used* (cons (append (car  (ex:*used*))
                             (cadr (ex:*used*)))
                     (cddr (ex:*used*)))))
  
  (define (ex:check-used id body-type form)  
    (and (not (eq? body-type 'toplevel))      
         ;; The car contains bindings for current frame and nested frames
         (let* ((already-used (car (ex:*used*)))
                ;; This destructively changes *used* and must follow previous
                (binding (ex:binding id)))  
           (if (memp (lambda (mapping)
                       (and (eq? binding (cdr mapping))
                            (ex:bound-identifier=? id (car mapping))))
                     already-used)
               (ex:syntax-violation 
                'definition
                "Definition of identifier that may have already affected meaning of undeferred portions of body"
                form
                id)))))
  
  ;;;==========================================================================
  ;;;
  ;;; Libraries:
  ;;;
  ;;;==========================================================================
  
  (define (ex:expand-program t)
    (ex:expand-library-or-program
     `(,(car t) (,(ex:datum->syntax (car t) (ex:generate-guid 'program)))
                (,(ex:datum->syntax (car t) 'export)) . ,(cdr t))
     'program))
  
  (define (ex:expand-library t)
    (ex:expand-library-or-program t 'library))
  
  ;; <library-type> ::= library | program
  
  (define (ex:expand-library-or-program t library-type)
    (or (and (list? t)
             (>= (length t) 4))
        (ex:syntax-violation 'library "Invalid number of clauses in library" t))
    
    (let ((keyword (car t))
          (name    (ex:syntax->datum (ex:library-name (cadr t)))))
      
      (let-values (((exports)                    (ex:scan-exports (caddr t)))
                   ((imported-libraries imports) (ex:scan-imports (cadddr t)))
                   ((body)                       (cddddr t)))

        ;; Make sure we start with a clean compilation environment,
        ;; and that we restore any global state afterwards.
        ;; Also make sure toplevel macros registered when visiting
        ;; imported libraries are removed once we are done.
        
        (parameterize ((ex:*table-of-envs*    '())
                       (ex:*usage-env*        (ex:make-unit-env))
                       (ex:*macro-env*        '())
                       (ex:*current-library*  name)
                       (ex:*syntax-reflected* #f))              ; +++ space
          
          (ex:import-libraries imported-libraries 0 'compile)
          (ex:env-import! keyword imports (ex:*usage-env*))
          
          (let ((initial-table-of-envs (ex:*table-of-envs*)))   ; +++ space
            
            (ex:*usage-env* (ex:env-reflect (ex:*usage-env*)))  ; +++ space
            
            (ex:scan-sequence library-type
                              ex:make-local-mapping
                              body
                              (lambda (forms syntax-definitions bound-variables)
                                
                                (let* ((exports
                                        (map (lambda (mapping)
                                               (cons (ex:id-name (car mapping))
                                                     (let ((binding (ex:binding (cadr mapping))))
                                                       (or binding
                                                           (ex:syntax-violation 'library "Unbound export" t
                                                                                (cadr mapping)))
                                                       (if (eq? (ex:binding-content binding) #t)
                                                           (ex:syntax-violation 'library
                                                                                "Attempt to export mutable variable" t
                                                                                (cadr mapping)))
                                                       (ex:sexp-replace ex:macro? #f binding))))
                                             exports))
                                       (expanded-library
                                        (case library-type
                                          ((program) `(begin
                                                        ($ex:import-libraries ',imported-libraries 0 'execute)
                                                        ,@(ex:emit-body forms 'define)))
                                          ((library)
                                           `(begin
                                              (define ,(ex:library->symbol 'envs name)
                                                ',(if (ex:*syntax-reflected*)                    ; +++ space
                                                      (ex:sexp-replace
                                                       ex:macro?
                                                       #f
                                                       (ex:amputate-tail (ex:*table-of-envs*)    ; +++ space
                                                                         initial-table-of-envs)) ; +++ space
                                                      '()))                                      ; +++ space
                                              
                                              (define ,(ex:library->symbol 'exports name) ',exports)
                                              (define ,(ex:library->symbol 'imports name) ',imported-libraries) 
                                              
                                              (define (,(ex:library->symbol 'visit name))
                                                ,@(map (lambda (def)
                                                         `($ex:register-macro! ',(car def) ,(cdr def)))
                                                       syntax-definitions)
                                                ($ex:unspecified))
                                              
                                              ,@(map (lambda (var)
                                                       `(define ,var ($ex:unspecified)))
                                                     bound-variables)
                                              
                                              (define (,(ex:library->symbol 'invoke name))
                                                ,@(ex:emit-body forms 'set!)
                                                ($ex:unspecified)))))))
                                  
                                  ;; Make library available for further expansion.
                                  (if (eq? library-type 'library)
                                      (eval expanded-library))

                                  expanded-library))))))))
  
  (define (ex:env-import! keyword imports env)
    (ex:env-extend! (map (lambda (import)
                           (cons (cons (car import)
                                       (ex:id-colors keyword))
                                 (cdr import)))
                         imports)
                    env))
  
  ;; session ::= compile | execute
  
  (define (ex:import-libraries imports level session)
    
    (define *visited*  '())
    (define *invoked*  '())
    (define *imported* '())
    
    (define (import-libraries* imports level session)
      (for-each (lambda (import)
                  (let ((name   (car import))
                        (levels (cdr import)))
                    (for-each (lambda (level*)
                                (import-library name (+ level level*) session))
                              levels)))
                imports))
    
    (define (import-library name level session)
      (and (not (member (cons name level) *imported*))
           (let ((imports (eval (ex:library->symbol 'imports name))))
             ;; Do this first so accidental cyclic dependencies will not hang
             (set! *imported* (cons (cons name level) *imported*))
             (import-libraries* imports level session)
             (and (>= level 0)
                  (case session
                    ((compile)
                     (and (>= level 0)
                          (or (member name *visited*)
                              (begin
                                (ex:extend-table-of-envs! (eval (ex:library->symbol 'envs name)))
                                (parameterize ((ex:*phase* level))
                                  (eval `(,(ex:library->symbol 'visit name))))
                                (set! *visited* (cons name *visited*)))))
                     (and (>= level 1)
                          (or (member name *invoked*)
                              (begin
                                (parameterize ((ex:*phase* level))
                                  (eval `(,(ex:library->symbol 'invoke name))))
                                (set! *invoked* (cons name *invoked*))))))
                    ((execute)
                     (and (= level 0)
                          (eval `(,(ex:library->symbol 'invoke name)))))))))) 
    
    (import-libraries* imports level session))
  
  ;; Returns ((<rename-identifier> <identifier> <level> ...) ...)
  
  (define (ex:scan-exports clause)
    (and (pair? clause)
         (ex:free=? (car clause) 'export)
         (list? (cdr clause)))
    (let ((exports (apply append
                          (map ex:scan-export-spec (cdr clause)))))
      (ex:check-set? exports
                     (lambda (x y)
                       (eq? (ex:id-name (car x))
                            (ex:id-name (car y))))
                     (lambda (dup) (ex:syntax-violation 'export "Duplicate export" clause dup)))
      exports))
  
  (define (ex:scan-export-spec spec)
    (let ((levels `(0))               ;; Will be ignored in current implementation, but keep data
          (export-sets (list spec)))  ;; structures and interfaces same in case FOR exports return.
      (map (lambda (rename-pair)
             (cons (car rename-pair)
                   (cons (cdr rename-pair)
                         levels)))
           (apply append (map ex:scan-export-set export-sets)))))
  
  (define (ex:scan-export-set set)
    (cond ((ex:identifier? set)
           (list (cons set set)))
          ((ex:rename-export-set? set)  
           (map (lambda (mapping)
                  (cons (cadr mapping)
                        (car mapping)))
                (cdr set)))
          (else
           (ex:syntax-violation 'export "Invalid export set" set))))
  
  (define (ex:scan-levels spec)
    (cond ((ex:for-spec? spec)
           (let ((levels
                  (map (lambda (level)
                         (cond ((ex:free=? level 'run)    0)
                               ((ex:free=? level 'expand) 1)
                               ((ex:meta-spec? level)     (cadr level))
                               (else (ex:syntax-violation 'for "Invalid level in for spec" spec level))))
                       (cddr spec))))
             (ex:check-set? levels
                            =
                            (lambda (dup) (ex:syntax-violation 'for "Repeated level in for spec" spec dup)))
             levels))
          (else '(0))))
  
  ;; Returns (values ((<library reference> <level> ...) ....)
  ;;                 ((<local name> . <binding>) ...))
  ;; with no repeats.
  
  (define (ex:scan-imports clause)
    (or (and (pair? clause)
             (ex:free=? (car clause) 'import)
             (list?  (cdr clause)))
        (ex:syntax-violation 'import "Invalid import clause" clause))
    (ex:scan-import-specs (cdr clause)))
  
  (define (ex:scan-import-specs all-specs)
    (let loop ((specs all-specs)
               (imported-libraries '())
               (imports '()))
      (if (null? specs)
          (values imported-libraries (ex:unify-imports imports))
          (let-values (((library-ref levels more-imports) (ex:scan-import-spec (car specs))))
            (loop (cdr specs)
                  ;; library-ref = #f if primitives spec
                  (if library-ref
                      (cons (cons library-ref levels)
                            imported-libraries)
                      imported-libraries)
                  (append more-imports imports))))))
  
  ;; Returns (values <library reference> | #f
  ;;                 (<level> ...)
  ;;                 ((<local name> . <binding>) ...)
  ;; where <level> ::= <integer>
  ;; #f is returned for library name in case of primitives.
  
  (define (ex:scan-import-spec spec)
    (let ((levels (ex:scan-levels spec)))
      (let loop ((import-set (if (ex:for-spec? spec)
                                 (cadr spec)
                                 spec))
                 (adjuster (lambda (set) set)))
        
        ;; Extension for importing unadorned primitives:
        
        (cond ((ex:primitive-set? import-set)
               (values #f
                       levels
                       (map (lambda (mapping)
                              (cons (car mapping) (ex:make-binding 'variable (cdr mapping) levels #f '())))
                            (adjuster (map (lambda (name) (cons name name))
                                           (ex:syntax->datum (cdr import-set)))))))
              ((or (ex:only-set?   import-set)
                   (ex:except-set? import-set)
                   (ex:prefix-set? import-set)
                   (ex:rename-set? import-set))
               (loop (cadr import-set)
                     (let ((args (ex:syntax->datum (cddr import-set))))
                       (define (check-presence names mappings from)
                         (for-each (lambda (name)
                                     (or (assq name mappings)
                                         (ex:syntax-violation from 
                                                              (string-append "Identifier not in set: " 
                                                                             (ex:list->string (map car mappings) " "))
                                                              import-set
                                                              name)))
                                   names))
                       (ex:compose adjuster
                                   (cond
                                     ((ex:only-set? import-set)
                                      (lambda (mappings)
                                        (check-presence args mappings 'only)
                                        (filter (lambda (mapping)
                                                  (memq (car mapping) args))
                                                mappings)))
                                     ((ex:except-set? import-set)
                                      (lambda (mappings)
                                        (check-presence args mappings 'except)
                                        (filter (lambda (mapping)
                                                  (not (memq (car mapping) args)))
                                                mappings)))
                                     ((ex:prefix-set? import-set)
                                      (lambda (mappings)
                                        (map (lambda (mapping)
                                               (cons (string->symbol
                                                      (string-append (symbol->string (car args))
                                                                     (symbol->string (car mapping))))
                                                     (cdr mapping)))
                                             mappings)))
                                     ((ex:rename-set? import-set)
                                      (lambda (mappings)
                                        (check-presence (map car args) mappings 'rename)
                                        (map (lambda (mapping)
                                               (cons (cond ((assq (car mapping) args) => cadr)
                                                           (else (car mapping)))
                                                     (cdr mapping)))
                                             mappings)))
                                     (else (ex:syntax-violation 'import "Invalid import set" import-set)))))))
              ((ex:library-ref import-set)
               => (lambda (library-ref)
                    (let* ((exports (eval (ex:library->symbol 'exports (ex:syntax->datum library-ref))))
                           (imports
                            (map (lambda (mapping)
                                   (cons (car mapping)
                                         (let ((binding (cdr (assq (cdr mapping) exports))))
                                           (ex:make-binding (ex:binding-type binding)
                                                            (ex:binding-name binding)
                                                            (ex:compose-levels levels (ex:binding-levels binding))
                                                            (ex:binding-content binding)
                                                            (ex:binding-library binding)))))
                                 (adjuster (map (lambda (name) (cons name name))
                                                (map car exports))))))
                      (values (ex:syntax->datum library-ref)
                              levels
                              imports))))
              (else (ex:syntax-violation 'import "Invalid import set" import-set))))))
  
  (define (ex:compose-levels levels levels*)
    (apply ex:unionv
           (map (lambda (level)
                  (map (lambda (level*)
                         (+ level level*))
                       levels*))
                levels)))
  
  ;; Argument is of the form ((<local name> <binding>) ...)
  ;; where combinations (<local name> (binding-name <binding>)) may be repeated.
  ;; Return value is of the same format but with no repeats and
  ;; where union of (binding-levels <binding>)s has been taken for any original repeats.
  ;; An error is signaled if same <local name> occurs with <binding>s
  ;; whose (binding-name <binding>)s are different.
  
  (define (ex:unify-imports imports)
    (let ((seen '()))
      (let loop ((imports imports))
        (if (null? imports)
            seen
            (let* ((mapping (car imports))
                   (probe (assq (car mapping) seen)))
              (if probe
                  (begin
                    (or (eq? (ex:binding-name (cdr mapping))
                             (ex:binding-name (cdr probe)))
                        (ex:syntax-violation
                         'import
                         (string-append "Different bindings for identifier imported from libraries ("
                                        (ex:list->string (ex:binding-library (cdr mapping)) " ")
                                        ") and ("
                                        (ex:list->string (ex:binding-library (cdr probe)) " ") ")")
                         (car mapping)))
                    (set-cdr! probe
                              (ex:make-binding (ex:binding-type (cdr probe)) 
                                               (ex:binding-name (cdr probe))
                                               (ex:unionv (ex:binding-levels (cdr probe))
                                                          (ex:binding-levels (cdr mapping)))
                                               (ex:binding-content (cdr probe))
                                               (ex:binding-library (cdr probe)))))
                  (set! seen (cons mapping seen)))
              (loop (cdr imports)))))))
  
  (define (ex:for-spec? spec)
    (and (list? spec)
         (>= (length spec) 2)
         (ex:free=? (car spec) 'for)))
  
  (define (ex:meta-spec? level)
    (and (list? level)
         (= (length level) 2)
         (ex:free=? (car level) 'meta)
         (integer? (cadr level))))
  
  (define (ex:only-set? set)
    (and (list? set)
         (>= (length set) 2)
         (ex:free=? (car set) 'only)
         (for-all ex:identifier? (cddr set))))
  
  (define (ex:except-set? set)
    (and (list? set)
         (>= (length set) 2)
         (ex:free=? (car set) 'except)
         (for-all ex:identifier? (cddr set))))
  
  (define (ex:prefix-set? set)
    (and (list? set)
         (>= (length set) 2)
         (ex:free=? (car set) 'prefix)
         (= (length set) 3)
         (ex:identifier? (caddr set))))
  
  (define (ex:rename-set? set)
    (and (list? set)
         (>= (length set) 2)
         (ex:free=? (car set) 'rename)
         (ex:rename-list? (cddr set))))
  
  (define (ex:primitive-set? set)
    (and (list? set)
         (pair? set)
         (ex:free=? (car set) 'primitives)
         (for-all ex:identifier? (cdr set))))
  
  (define (ex:rename-export-set? set)
    (and (list? set)
         (>= (length set) 1)
         (ex:free=? (car set) 'rename)
         (ex:rename-list? (cdr set))))
  
  (define (ex:rename-list? ls)
    (for-all (lambda (e)
               (and (list? e)
                    (= (length e) 2)
                    (for-all ex:identifier? e)))
             ls))
  
  (define (ex:library-name e)
    (ex:library-ref-helper e ex:version?))
  
  (define (ex:library-ref e)  
    (ex:library-ref-helper
     (if (and (list? e)
              (= (length e) 2)
              (ex:free=? (car e) 'library))
         (cadr e)
         e)
     ex:version-reference?))
  
  (define (ex:library-ref-helper e version?)
    (or (and (list? e)
             (pair? e)
             (let ((re (reverse e)))
               (and (for-all ex:identifier? (cdr re))
                    (if (ex:identifier? (car re))
                        e
                        (and (version? (car re))
                             (reverse (cdr re)))))))
        (ex:syntax-violation 'library "Invalid library name" e)))
  
  (define (ex:version? e)
    (and (list? e)
         (for-all ex:subversion? e)))
  
  (define (ex:subversion? x)
    (and (integer? x)
         (>= x 0)))
  
  (define (ex:version-reference? e)
    (and (list? e)
         (or (for-all ex:subversion-reference? e)
             (and (pair? e) 
                  (for-all ex:version-reference? (cdr e))
                  (or (ex:free=? (car e) 'and)
                      (ex:free=? (car e) 'or)
                      (and (ex:free=? (car e) 'not)
                           (= (length e) 2)))))))
  
  (define (ex:subversion-reference? e)
    (or (ex:subversion? e)
        (ex:subversion-condition? e)))
  
  (define (ex:subversion-condition? e)
    (and (list? e)
         (pair? e) 
         (cond 
           ((or (ex:free=? (car e) '>=)
                (ex:free=? (car e) '<=))
            (and (= (length e) 2)
                 (ex:subversion? (cadr e))))
           ((or (ex:free=? (car e) 'and)
                (ex:free=? (car e) 'or))
            (for-all ex:subversion-reference? (cdr e)))
           ((ex:free=? (car e) 'not)
            (and (= (length e) 2)
                 (ex:subversion-reference? (cadr e))))
           (else #f))))
  
  ;;;==========================================================================
  ;;;
  ;;; Debugging facilities:
  ;;;
  ;;;==========================================================================
  
  ;; Debugging information displayed by syntax-violation.
  
  (define ex:*trace* (make-parameter '()))
  
  (define (ex:trace term thunk)
    (parameterize ((ex:*trace* (cons term (ex:*trace*))))
      (thunk)))
  
  (define (ex:syntax-violation who message form . maybe-subform)
    (newline)
    (display "Syntax violation: ")
    (let ((who (if who
                   who
                   (cond ((ex:identifier? form)
                          (ex:syntax->datum form))
                         ((and (list? form)
                               (ex:identifier? (car form)))
                          (ex:syntax->datum (car form)))
                         (else ""))))
          (subform (cond ((null? maybe-subform) #f)
                         ((and (pair? maybe-subform)
                               (null? (cdr maybe-subform)))
                          (car maybe-subform))
                         (else (assertion-violation 'syntax-violation
                                                    "Invalid subform in syntax violation"
                                                    maybe-subform)))))
      (display who)
      (newline)
      (newline)
      (display message)
      (newline)
      (newline)
      (if subform
          (begin (display "Subform: ")
                 (pretty-print (ex:syntax-debug subform))
                 (newline)))
      (display "Form: ")
      (pretty-print (ex:syntax-debug form))
      (newline)
      (display "Trace: ")
      (newline)
      (newline)
      (for-each (lambda (exp)
                  (display "  ")
                  (pretty-print (ex:syntax-debug exp))
                  (newline))
                (ex:*trace*))
      (error 'syntax-violation "Integrate with host error handling here")))
  
  (define (ex:syntax-debug exp)
    (ex:sexp-map (lambda (leaf)
                   (cond ((ex:identifier? leaf)
                          (ex:id-name leaf))
                         (else leaf)))
                 exp))
  
  ;;;==========================================================================
  ;;;
  ;;;  Eval and environment:
  ;;;
  ;;;==========================================================================
  
  (define ex:eval-template
    (ex:make-identifier 'eval-template
                        '()
                        '()         
                        0
                        ;; Needs a secret dummy library so expander will 
                        ;; not confuse source library of this with call
                        ;; site library.  
                        `(,(ex:generate-guid 'secret-eval-library))))
  
  (define-record-type ex:r6rs-environment (ex:make-r6rs-environment imported-libraries imports)
    ex:r6rs-environment?
    (imported-libraries ex:r6rs-environment-imported-libraries)
    (imports            ex:r6rs-environment-imports))
  
  (define (ex:environment . import-specs)
    (parameterize ((ex:*usage-env* (ex:make-unit-env)))
      (ex:env-import! ex:eval-template ex:library-language (ex:*usage-env*))
      (let-values (((imported-libraries imports)
                    (ex:scan-import-specs
                     (map (lambda (spec)
                            (ex:datum->syntax ex:eval-template spec))
                          import-specs))))
        (ex:make-r6rs-environment imported-libraries imports))))
  
  (define (ex:eval exp env)
    (parameterize ((ex:*usage-env* (ex:make-unit-env)))
      (ex:env-import! ex:eval-template (ex:r6rs-environment-imports env) (ex:*usage-env*))
      (let ((exp (ex:datum->syntax ex:eval-template exp)))
        (ex:import-libraries (ex:r6rs-environment-imported-libraries env) 0 'compile)
        (ex:import-libraries (ex:r6rs-environment-imported-libraries env) 0 'execute)
        (eval (ex:expand-begin
               ;; wrap in expression begin so no definition can occur as required
               ;; by r6rs
               `(,(ex:rename 'macro 'begin) ,exp)))))) 

  ;;;==========================================================================
  ;;;
  ;;; Library reflection:
  ;;;
  ;;;=========================================================================

  (define (ex:environment-bindings r6rs-env)
    (map format-mapping
         (ex:r6rs-environment-imports r6rs-env)))

  (define (format-mapping mapping)
    `((name ,(car mapping))
      (type ,(ex:binding-type (cdr mapping)))
      (from ,(ex:binding-library (cdr mapping)))
      (levels ,(ex:binding-levels (cdr mapping)))))
  
  ;;;=====================================================================
  ;;;
  ;;; Utilities:
  ;;;
  ;;;=====================================================================
  
  (define (ex:flatten l)
    (cond ((null? l) l)
          ((pair? l) (cons (car l)
                           (ex:flatten (cdr l))))
          (else (list l))))
  
  (define (ex:sexp-map f s)
    (cond ((null? s) '())
          ((pair? s) (cons (ex:sexp-map f (car s))
                           (ex:sexp-map f (cdr s))))
          ((vector? s)
           (apply vector (ex:sexp-map f (vector->list s))))
          (else (f s))))
  
  (define (ex:sexp-replace pred? value sexp)
    (ex:sexp-map (lambda (leaf)
                   (if (pred? leaf)
                       value
                       leaf))
                 sexp))
  
  (define (ex:dotted-memp proc ls)
    (cond ((null? ls) #f)
          ((pair? ls) (if (proc (car ls))
                          ls
                          (ex:dotted-memp proc (cdr ls))))
          (else (and (proc ls)
                     ls))))
  
  (define (ex:dotted-map f lst)
    (cond ((null? lst) '())
          ((pair? lst) (cons (f (car lst))
                             (ex:dotted-map f (cdr lst))))
          (else (f lst))))
  
  ;; Returns 0 also for non-list a la SRFI-1 protest.
  
  (define (ex:dotted-length dl)
    (cond ((null? dl) 0)
          ((pair? dl) (+ 1 (ex:dotted-length (cdr dl))))
          (else 0)))
  
  (define (ex:dotted-butlast ls n)
    (let recurse ((ls ls)
                  (length-left (ex:dotted-length ls)))
      (cond ((< length-left n) (assertion-violation 'dotted-butlast "List too short" ls n))
            ((= length-left n) '())
            (else
             (cons (car ls)
                   (recurse (cdr ls)
                            (- length-left 1)))))))
  
  (define (ex:dotted-last ls n)
    (let recurse ((ls ls)
                  (length-left (ex:dotted-length ls)))
      (cond ((< length-left n) (assertion-violation 'dotted-last "List too short" ls n))
            ((= length-left n) ls)
            (else
             (recurse (cdr ls)
                      (- length-left 1))))))
  
  (define (ex:map-while f lst k)
    (cond ((null? lst) (k '() '()))
          ((pair? lst)
           (let ((head (f (car lst))))
             (if head
                 (ex:map-while f
                               (cdr lst)
                               (lambda (answer rest)
                                 (k (cons head answer)
                                    rest)))
                 (k '() lst))))
          (else  (k '() lst))))
  
  (define (ex:check-set? ls = fail)
    (or (null? ls)
        (if (memp (lambda (x)
                    (= x (car ls)))
                  (cdr ls))
            (fail (car ls))
            (ex:check-set? (cdr ls) = fail))))
  
  (define (ex:unionv . sets)
    (cond ((null? sets) '())
          ((null? (car sets))
           (apply ex:unionv (cdr sets)))
          (else
           (let ((rest (apply ex:unionv
                              (cdr (car sets))
                              (cdr sets))))
             (if (memv (car (car sets)) rest)
                 rest
                 (cons (car (car sets)) rest))))))
  
  (define (ex:amputate-tail list tail)
    (cond ((null? list)    '())
          ((eq? list tail) '())
          (else
           (cons (car list)
                 (ex:amputate-tail (cdr list) tail)))))

  (define (ex:list->string e separator)
    (define (tostring x)
      (cond ((symbol? x)
             (symbol->string x))
            ((number? x)
             (number->string x))
            (else
             (assertion-violation 'ex:list->string "Invalid argument" e))))
    (if (null? e) 
        ""
        (string-append
         (tostring (car e))
         (apply string-append
                (map (lambda (x)
                       (string-append separator (tostring x)))
                     (cdr e))))))
  
  (define (ex:compose f g)
    (lambda (x) (f (g x))))
  
  (define (ex:check x p? from)
    (or (p? x)
        (ex:syntax-violation from "Invalid argument" x)))
  
  (define (ex:invalid-form exp)
    (ex:syntax-violation #f "Invalid form" exp))
  
  (define ex:syntax-error (ex:make-expander ex:invalid-form))
  
  (define ex:unspecified
    (let ((x (if #f #f)))
      (lambda () x)))
  
  ;;;============================================================================
  ;;;
  ;;; REPL integration:
  ;;;
  ;;;============================================================================
  
  (define (ex:repl exps)
    (for-each (lambda (exp)
                (for-each (lambda (exp)
                            (for-each (lambda (result)
                                        (display result)
                                        (newline))
                                      (call-with-values
                                       (lambda ()
                                         (eval exp))
                                       list)))
                          (ex:expand-toplevel-sequence (list exp))))
              exps))
  
  ;; Restores parameters to a consistent state
  ;; in case they were left inconsistent by an error.
  
  (define (ex:reset-toplevel!)
    (ex:*trace*           '())
    (ex:*current-library* '())
    (ex:*phase*           0)
    (ex:*used*            (list '()))
    (ex:*color*           (ex:generate-color))
    (ex:*usage-env*       (ex:*toplevel-env*)))
  
  (define (ex:expand-toplevel-sequence forms)
    (ex:reset-toplevel!)
    (ex:scan-sequence 'toplevel
                      ex:make-toplevel-mapping
                      (ex:source->syntax forms)
                      (lambda (forms syntax-definitions bound-variables)
                        (ex:emit-body forms 'define))))
  
  ;;;==========================================================================
  ;;;
  ;;;  Load and expand-file:
  ;;;
  ;;;==========================================================================
  
  ;; This may be used as a front end for the compiler:
  ;; The dependencies must list the already expanded files
  ;; containing libraries to be imported.
  
  (define (ex:expand-file filename target-filename . dependencies)
    (for-each load dependencies)
    (ex:write-file (ex:expand-toplevel-sequence (ex:normalize (ex:read-file filename)))
                   target-filename))
  
  ;; Keeps (<library> ...) the same.
  ;; Converts (<library> ... . <toplevel program>)
  ;; to (<library> ... (program . <toplevel program>))
  
  (define (ex:normalize exps)
    (define (error)
      (let ((newline (string #\newline)))
        (ex:syntax-violation
         'expand-file
         (string-append
          "File should be of the form:" newline
          "      <library>*" newline
          "    | <library>* <toplevel program>")
         exps)))
    (let loop ((exps exps)
               (normalized '()))
      (if (null? exps)
          (reverse normalized)
          (if (pair? (car exps))
              (case (caar exps)
                ((library)
                 (loop (cdr exps)
                       (cons (car exps) normalized)))
                ((import)
                 (loop '()
                       (cons (cons 'program exps)
                             normalized)))
                (else (error)))
              (error)))))
  
  (define ex:read-file
    (lambda (fn)
      (let ((p (open-input-file fn)))
        (let f ((x (read p)))
          (if (eof-object? x)
              (begin (close-input-port p) '())
              (cons x
                    (f (read p))))))))
  
  (define ex:write-file
    (lambda (exps fn)
      (if (file-exists? fn)
          (delete-file fn))
      (let ((p (open-output-file fn)))
        (for-each (lambda (exp)
                    (write exp p)
                    (newline p)
                    (newline p))
                  exps)
        (close-output-port p))))

  ;;;==========================================================================
  ;;;
  ;;; Toplevel bootstrap:
  ;;;
  ;;;==========================================================================
  
  (define ex:toplevel-template
    (ex:make-identifier 'toplevel-template
                        '()
                        '()
                        0
                        #f))
  
  (define (ex:source->syntax datum)
    (ex:datum->syntax ex:toplevel-template datum))
  
  (define ex:*toplevel-env* (make-parameter (ex:make-unit-env)))
  (define ex:*usage-env*    (make-parameter (ex:*toplevel-env*)))
  
  (define ex:library-language
    (map (lambda (x)
           (cons (car x) (ex:make-binding 'macro (car x) '(0) (cdr x) '())))
         `((program    . ,ex:syntax-error)
           (library    . ,ex:syntax-error)
           (export     . ,ex:syntax-error)
           (import     . ,ex:syntax-error)
           (for        . ,ex:syntax-error)
           (run        . ,ex:syntax-error)
           (expand     . ,ex:syntax-error)
           (meta       . ,ex:syntax-error)
           (only       . ,ex:syntax-error)
           (except     . ,ex:syntax-error)
           (prefix     . ,ex:syntax-error)
           (rename     . ,ex:syntax-error)
           (primitives . ,ex:syntax-error)
           (>=         . ,ex:syntax-error)
           (<=         . ,ex:syntax-error)
           (and        . ,ex:syntax-error)
           (or         . ,ex:syntax-error)
           (not        . ,ex:syntax-error)
           (>=         . ,ex:syntax-error))))
  
  ;; For bootstrap library below.
  
  (define ex:primitive-macros
    `((lambda        . ,(ex:make-expander ex:expand-lambda))
      (if            . ,(ex:make-expander ex:expand-if))
      (set!          . ,(ex:make-expander ex:expand-set!))
      (begin         . ,(ex:make-expander ex:expand-begin))
      (syntax        . ,(ex:make-expander ex:expand-syntax))
      (quote         . ,(ex:make-expander ex:expand-quote))
      (let-syntax    . ,(ex:make-expander ex:expand-local-syntax))
      (letrec-syntax . ,(ex:make-expander ex:expand-local-syntax))
      (syntax-case   . ,(ex:make-expander ex:expand-syntax-case))
      (and           . ,(ex:make-expander ex:expand-and))
      (or            . ,(ex:make-expander ex:expand-or))
      (define        . ,ex:syntax-error)
      (define-syntax . ,ex:syntax-error)
      (_             . ,ex:syntax-error)
      (...           . ,ex:syntax-error)))
  
  ;; Initial toplevel environment:
  
  (ex:env-import! ex:toplevel-template ex:library-language (ex:*toplevel-env*))
  
  ;;==========================================================================
  ;;
  ;; Exports:
  ;;
  ;;==========================================================================
  
  (set! $ex:make-variable-transformer ex:make-variable-transformer)
  (set! $ex:identifier?               ex:identifier?)
  (set! $ex:bound-identifier=?        ex:bound-identifier=?)
  (set! $ex:free-identifier=?         ex:free-identifier=?)
  (set! $ex:generate-temporaries      ex:generate-temporaries)
  (set! $ex:datum->syntax             ex:datum->syntax)
  (set! $ex:syntax->datum             ex:syntax->datum)
  (set! $ex:environment               ex:environment)
  (set! $ex:environment-bindings      ex:environment-bindings)
  (set! $ex:eval                      ex:eval)
  (set! $ex:syntax-violation          ex:syntax-violation)
  
  (set! $ex:expand-file               ex:expand-file)
  (set! $ex:repl                      ex:repl)
  
  (set! $ex:invalid-form              ex:invalid-form)
  (set! $ex:register-macro!           ex:register-macro!)
  (set! $ex:extend-table-of-envs!     ex:extend-table-of-envs!)
  (set! $ex:import-libraries          ex:import-libraries)
  (set! $ex:syntax-rename             ex:syntax-rename)
  (set! $ex:map-while                 ex:map-while)
  (set! $ex:dotted-length             ex:dotted-length)
  (set! $ex:dotted-butlast            ex:dotted-butlast)
  (set! $ex:dotted-last               ex:dotted-last)
  (set! $ex:unspecified               ex:unspecified)  
  
  ) ; Expander

;;===================================================================
;;
;; Bootstrap library containing macros defined in this expander.
;;
;;=================================================================== 

(define ~core.primitive-macros~envs '())
(define ~core.primitive-macros~exports
  (map (lambda (name)
         (cons name (ex:make-binding 'macro name '(0) #f '())))
       (map car ex:primitive-macros)))
(define ~core.primitive-macros~imports '())
(define ~core.primitive-macros~visit
  (lambda ()
    (for-each (lambda (mapping)
                (ex:register-macro! (car mapping) (cdr mapping)))
              ex:primitive-macros)))
(define ~core.primitive-macros~invoke
  (lambda () ($ex:unspecified)))

;; DID - Immutability checks:
;;       It is now a syntax violation if an explicitly exported variable appears on the 
;;       left-hand side of a set! expression, either in the exporting or importing libraries.
;;       It is also a syntax violation if a variable appears on the left-hand side of a set!
;;       expression in any code produced by an exported macro outside of the library in which 
;;       the variable is defined. 
;;       It is also a syntax violation if a reference to an assigned variable appears in any 
;;       code produced by an exported macro outside of the library in which the variable is 
;;       defined.
;; DID - Added library (rnrs eval reflection (6)) for reflection facilities:
;;       (environment-bindings <environment>) lists the bindings in an environment
;;       Very useful for development and dsbugging of libraries
;;       See the examples in examples.scm file
;; DID - replaced implementation of QUASIQUOTE with optimised Dybvig portable syntax-case version
;; DID - an unbound identifier at toplevel will now match unbound literal in library
;; DID - improved some error messages to state source libraries of bindings
;; DID - Larceny compat updated to v0.94
;; DID - corrected free occurrence of FIRST in ex:unify-imports that prevented an error message
;; DID - removed caching of imported macros
;; DID - added missing version (6) to library (rnrs mutable-strings)
;; DID - only, except, prefix, and rename now enforce the constraints stated in the r6rs draft



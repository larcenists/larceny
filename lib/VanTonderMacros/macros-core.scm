;;;===============================================================================
;;;
;;; R6RS Macros and R6RS libraries:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;   January 15, 2007 
;;;
;;;===============================================================================

(require 'define-record)
(require 'time)
(define (larcenyenv-getstarttime)
  (call-with-values current-utc-time
    (lambda (secs msecs)
      (+ (* secs 1000) msecs))))
(define larcenyenv-getpid
  (foreign-procedure "getpid" '() 'int))
(define larcenyenv-gethostname
  (lambda () 
    (cond ((getenv "HOST"))
          (else (error 'larcenyenv-gethostname 
                       "Set HOST env variable!")))))
                   
(read-square-bracket-as-paren #t)

;; Direct exports: 

(define $make-variable-transformer #f)
(define $identifier?               #f)
(define $bound-identifier=?        #f)
(define $free-identifier=?         #f)
(define $generate-temporaries      #f) 
(define $datum->syntax             #f)
(define $syntax->datum             #f)                
(define $environment               #f)
(define $r6rs-eval                 #f)

;; System exports:

(define $r6rs-expand-toplevel-expressions #f)
(define $r6rs-expand-file          #f)
(define $r6rs-load                 #f)
(define $r6rs-load-program         #f)
(define $repl                      #f)

;; Indirect exports: 

(define $invalid-form              #f) 
(define $uncompress                #f)
(define $register-macro!           #f) 
(define $import-libraries          #f) 
(define $syntax-rename             #f)    
(define $map-while                 #f) 
(define $dotted-length             #f)  
(define $dotted-butlast            #f)  
(define $dotted-last               #f)  

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
        (else (contract-violation 'memp "Invalid argument" ls))))

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

(define unspecified
  (let ((x (if #f #f)))
    (lambda () x)))

;; Start of expander:

(let-syntax ((define-struct (syntax-rules () ((_ ARGS ...) (define-record ARGS ...))))
             (define (syntax-rules (make-parameter) 
                       ((_ ID (make-parameter ARG))
                        (define ID (make-parameter 'ID ARG)))
                       ((_ ARGS ...)
                        (define ARGS ...)))))
  
  ;;;===============================================================================
  ;;;
  ;;; Hooks:
  ;;;
  ;;;===============================================================================
  
  ;; For compiler and REPL integration, see for example the procedures
  ;;
  ;;   - REPL
  ;;   - R6RS-LOAD
  ;;   - R6RS-LOAD-PROGRAM
  
  ;; IMPORTANT:
  ;; ---------
  ;; Generate-guid must return a fresh symbol that has a globally
  ;; unique external representation and is read-write invariant.  
  ;; This must be done before attempting to use this expander
  ;; for incremental expansion or separate compilation.
  
  (define generate-guid
    (let ((timestring (number->string (larcenyenv-getstarttime)))
          (pidstring  (number->string (larcenyenv-getpid)))
          (hostname   (larcenyenv-gethostname)))
      (lambda (symbol)
        (gensym
         (string-append (symbol->string symbol)
                        "@start" timestring
                        "@host"  hostname
                        "@pid"   pidstring
                        )))))
  
  ;; Used to generate user program toplevel and
  ;; library names.
  ;; Must be disjoint from all source symbols.
  
  (define (free-name  symbol) (string->symbol (string-append ";" (symbol->string symbol))))
  (define (free-name? symbol) (char=? (string-ref (symbol->string symbol) 0) #\;))
  
  ;;;==========================================================================
  ;;;
  ;;; Identifiers:
  ;;;
  ;;;==========================================================================
  
  ;; <name>             ::= <symbol>
  ;; <marks>            ::= (<mark> ...)
  ;; <transformer-envs> ::= (<env> ...)
  ;; <level-correction> ::= <integer>
  ;;
  ;; where <name>             : The symbolic name of the identifier in the source.
  ;;       <marks>            : Each time an introduced identifier is renamed, a fresh
  ;;                            mark gets prepended to its <marks>.
  ;;       <transformer-envs> : The environment (car <transformer-envs>) was the usage
  ;;                            environment valid during expansion of the (syntax id*) expression
  ;;                            introducing this identifier, while (cdr <transformer-envs>)
  ;;                            are in turn the <transformer-envs> of id*.
  ;;       <level-correction> : Integer that keeps track of shifts in meta-levels
  ;;                            between introduction and usage sites of identifier.
  
  (define-struct identifier (name
                             marks
                             transformer-envs
                             level-correction))
  
  (define (bound-identifier=? x y)
    (check x identifier? 'bound-identifier=?)
    (check y identifier? 'bound-identifier=?)
    (and (eq? (identifier-name x)
              (identifier-name y))
         (equal? (identifier-marks x)
                 (identifier-marks y))))
  
  ;; Since the denotation of a displaced (out of phase) identifier is 
  ;; a fresh value, a displaced identifier is not free-identifier=?
  ;; to any identifier, including itself. 
  
  (define (free-identifier=? x y)
    (check x identifier? 'free-identifier=?)            
    (check y identifier? 'free-identifier=?)
    (eq? (denotation x)
         (denotation y))) 
  
  (define (free=? x symbol)
    (and (identifier? x)
         (eq? (denotation x) symbol)))
  
  ;; Returns <mark> ::= globally unique symbol 
  
  (define (generate-mark)
    (generate-guid 'm))
  
  (define *mark* (make-parameter (generate-mark)))
  
  ;; The meta-level for the current expansion step:
  
  (define *level* (make-parameter 0)) 
  
  (define (source-level id)
    (- (*level*)
       (identifier-level-correction id)))
  
  ;; <binding>      ::= (<binding name> <level> ...)
  ;; <binding name> ::= <symbol>
  ;; <level>        ::= <integer>
  
  (define make-binding   cons)
  (define binding-name   car)
  (define binding-levels cdr)
  
  ;; Looks up binding first in usage environment and then
  ;; in attached transformer environments.
  ;; Returns <binding>.
  
  (define (lookup-binding id)   
    (let ((name  (identifier-name id))
          (marks (identifier-marks id)))
      (let loop ((env  (*usage-env*))
                 (envs (identifier-transformer-envs id))
                 (marks marks))
        (or (env-lookup (cons name marks) env #f)
            (and (pair? envs)
                 (loop (car envs)
                       (cdr envs)
                       (cdr marks)))))))
  
  ;; Returns <binding name> | (displaced . <identifier>)
  ;; where (displaced . <identifier>) is not eq? to any
  ;; previous denotation.  See free-identifier=? for why.
  
  (define (denotation id)
    (let ((binding (lookup-binding id))
          (level (source-level id))) 
      (let ((denotation       
             (if binding
                 (if (memv level (binding-levels binding))
                     (binding-name binding)
                     (cons 'displaced id))
                 (if (zero? level)
                     (free-name (identifier-name id))   
                     (cons 'displaced id)))))
        (if (symbol? denotation) 
            (*used* (cons (cons (cons id denotation) 
                                (car (*used*))) 
                          (cdr (*used*)))))   
        denotation)))
  
  ;; For avoiding giving lexically invalid semantics to Scheme expressions
  ;; according to the semantics described in readme.
  
  (define *used* (make-parameter (list '())))
  
  ;; Returns <binding name>
  
  (define (usage-denotation id)
    (let ((denotation (denotation id)))
      (cond ((pair? denotation)
             (displaced-error id))  
            ((and (*error-if-free?*) (free-name? denotation))
             (free-error id))
            (else denotation))))
  
  (define *error-if-free?* (make-parameter #f))
  
  ;; An environment entry for an identifier binding.
  ;; Returns ((<name> . <marks>) . <binding>)
  
  (define (make-entry name marks binding)  
    (cons (cons name marks) binding))
  
  ;; Generates a local binding entry at the current meta-level
  ;; that can be added to the usage environment.
  ;; Returns <entry>.   
  
  (define (make-local-entry id)
    (make-entry (identifier-name id)
                (identifier-marks id)
                (make-binding (generate-guid (identifier-name id))
                              (list (source-level id)))))
  
  ;; Toplevel binding forms use as binding name the free name 
  ;; so that source-level forward references will work.
  ;; If identifier is macro-generated, bind it with a fresh name.  
  ;; This ensures that generated toplevel defines are not visible 
  ;; from toplevel source code, thus approximating the behaviour 
  ;; of generated internal definitions.  
  ;; Returns <entry>.
  
  (define (make-toplevel-entry id)
    (if (null? (identifier-marks id)) 
        (make-entry (identifier-name id)
                    (identifier-marks id)      
                    (make-binding (free-name (identifier-name id))
                                  (list (source-level id))))
        (make-local-entry id)))
  
  ;; Errors:
  
  (define (free-error id)
    (syntax-violation "unbound reference"
                      "Identifier must be bound"
                      id))
  
  (define (displaced-error id)
    (syntax-violation
     "invalid reference"
     (let ((probe (lookup-binding id)))
       (if probe
           (string-append "Attempt to use " (symbol->string (syntax->datum id))
                          " at invalid meta level " (number->string (source-level id))
                          ".  Binding is only available at meta levels: "
                          (apply string-append 
                                 (map (lambda (level) (string-append (number->string level) " "))
                                      (binding-levels probe))))
           (string-append "No binding available for " (symbol->string (syntax->datum id))
                          " at meta level " (number->string (source-level id))))) 
     id))
  
  ;;;=========================================================================
  ;;;
  ;;; Environments: 
  ;;;
  ;;;=========================================================================
  
  ;; An environment consists of a sequence of frames that
  ;; can be destructively extended.  
  ;;
  ;; <env>   ::= (<frame> ... <frame0>)            
  ;; <frame> ::= (box ((<key> . <object>) ...)) 
  ;; <key>   ::= a scheme value comparable with equal?
  
  (define (make-unit-env) (list (make-frame '())))
  
  ;; Adds a new frame containing entries to env.
  
  (define (env-extend entries env)
    (cons (make-frame entries) env))
  
  ;; Destructively extends the leftmost frame in env.
  
  (define (env-extend! entries env)  
    (frame-extend! entries (car env)))
  
  ;; Returns <object> | default
  
  (define (env-lookup key env default)
    (cond ((null? env) default)
          ((frame-lookup key (car env)) => cdr)
          (else                            
           (env-lookup key (cdr env) default))))
  
  (define (make-frame entries) (box entries))
  
  ;; Is id already bound in leftmost frame?
  
  (define (duplicate id env)
    (assoc (cons (identifier-name id)
                 (identifier-marks id))
           (unbox (car env))))
  
  (define (frame-extend! entries frame)  
    (set-box! frame (append entries (unbox frame))))
  
  (define (frame-lookup key frame)  
    (assoc key (unbox frame)))
  
  (define box      list)
  (define unbox    car)
  (define set-box! set-car!)
  
  ;;;=========================================================================
  ;;;
  ;;; Syntax-reflect and syntax-rename:
  ;;;
  ;;; This is the basic building block of the implicit renaming mechanism for
  ;;; maintaining hygiene.  Syntax-reflect generates the expanded code for 
  ;;; (syntax id), including the expand-time environment in the
  ;;; external representation.  It expands to syntax-rename, which performs 
  ;;; the implicit renaming when this expanded code is eventually run. 
  ;;; The level computations perform the adjustment of levels in the presence
  ;;; of libraries, where meta-levels may be shifted.    
  ;;;
  ;;;=========================================================================
  
  (define (syntax-reflect id)                     
    `($syntax-rename ',(identifier-name id)                            
                     ',(identifier-marks id)
                     ',(reflect-envs id)
                     ;; transformer-expand-time corrected level
                     ,(- (*level*) (identifier-level-correction id) 1))) 
  
  (define (syntax-rename name marks reflected-transformer-envs expand-time-corrected-level)
    (make-identifier name 
                     (cons (*mark*) marks)
                     (reify-envs reflected-transformer-envs)
                     ;; transformer-runtime level-correction
                     (- (*level*) expand-time-corrected-level)))
  
  ;;;=====================================================================
  ;;;
  ;;; Capture and sexp <-> syntax conversions:
  ;;;
  ;;;=====================================================================
  
  (define (datum->syntax tid datum)
    (check tid identifier? 'datum->syntax)
    (sexp-map (lambda (leaf)
                (cond ((const? leaf)  leaf)
                      ((symbol? leaf) (make-identifier leaf
                                                       (identifier-marks tid)
                                                       (identifier-transformer-envs tid)
                                                       (identifier-level-correction tid)))
                      (else (contract-violation 'datum->syntax "Invalid datum" leaf))))
              datum))
  
  (define (syntax->datum exp)
    (sexp-map (lambda (leaf) 
                (cond ((const? leaf)      leaf)
                      ((identifier? leaf) (identifier-name leaf))
                      (else 
                       (contract-violation 'syntax->datum "Invalid syntax object" leaf))))
              exp))
  
  ;; Fresh identifiers:
  
  (define (generate-temporaries ls)
    (check ls list? 'generate-temporaries)
    (map (lambda (ignore)
           (rename (generate-guid 'gen)))       
         ls))
  
  ;; For use internally as in the explicit renaming system.
  
  (define (rename symbol)    
    (make-identifier symbol
                     (list (*mark*))
                     (list (env-extend (list (make-entry symbol
                                                         '()
                                                         (make-binding symbol '(0)))) 
                                       (make-unit-env)))
                     (*level*)))
  
  ;;;=======================================================================
  ;;;
  ;;; Reflecting and reifying transformer environments for
  ;;; inclusion in expanded syntax expressions in object code.
  ;;;
  ;;;=======================================================================
  
  ;; Table of the form ((<key> . <transformer-envs>) ...)
  
  (define *reflected-envs* (make-parameter '()))
  
  ;; Returns a single-symbol representation      
  ;; that can be included in object code.
  
  (define (reflect-envs id) 
    (let ((key (generate-guid 'env)))                 
      (*reflected-envs*
       (cons (cons key
                   (cons (*usage-env*)
                         (identifier-transformer-envs id)))
             (*reflected-envs*)))                
      key))
  
  ;; The inverse of the above.
  
  (define (reify-envs reflected-envs)
    (cond ((assq reflected-envs (*reflected-envs*)) => cdr) 
          (else (error 'reify-envs "Internal error" reflected-envs))))
  
  (define (extend-reflected-envs! envs)
    (*reflected-envs* 
     (append envs (*reflected-envs*))))
  
  ;; Returns a mark delimiting the environments currently present
  ;; in the reflected environment table.  
  
  (define (current-reflected-envs-mark)
    (*reflected-envs*))
  
  ;; Returns only relevant reflected environments for
  ;; inclusion in object library.  
  ;; This avoids exponentially growing object code when
  ;; imports are chained.
  
  (define (compress-reflected-envs stop-mark)
    (compress
     (let loop ((tenvs (*reflected-envs*))
                (entries '()))
       (if (eq? tenvs stop-mark)
           entries 
           (loop (cdr tenvs)
                 (cons (car tenvs) entries))))))
  
  ;; Replaces shared nodes in environments by #(<integer>), where
  ;; <integer> denotes another compressed environment. 
  ;; Returns (<compressed datum> (<integer> . <compressed datum>) ...)
  ;; This version is aware of the structure of argument because a prior
  ;; generic version was too slow.  
  
  (define (compress reflected-entries) 
    (let ((count 0)
          (nodes '()))
      (let loop ((reflected-entries reflected-entries))
        (if (pair? reflected-entries)
            (let ((envs (cdr (car reflected-entries))))
              (for-each (lambda (env)
                          (let loop ((env env))
                            (if (pair? env)
                                (let ((probe (assq env nodes)))
                                  (if probe 
                                      (begin 
                                        (set-cdr! probe count)
                                        (set! count (+ 1 count)))
                                      (begin 
                                        (set! nodes (cons (cons env #f) 
                                                          nodes))
                                        (loop (cdr env))))))))
                        envs)
              (loop (cdr reflected-entries)))))
      (let ((nodes (filter cdr nodes)))
        (define (abbreviate env)
          (if (pair? env)
              (let ((probe (assq env nodes)))
                (if probe 
                    (vector (cdr probe))
                    (cons (car env)
                          (abbreviate (cdr env)))))
              '()))
        (cons (map (lambda (reflected-entry)
                     (cons (car reflected-entry)
                           (map abbreviate (cdr reflected-entry)))) 
                   reflected-entries)
              (map (lambda (entry)
                     (cons (cdr entry)
                           (cons (car (car entry))
                                 (abbreviate (cdr (car entry))))))
                   nodes)))))

  (define (uncompress c) 
    (let ((datum (car c))
          (table (cdr c)))
      (define (reconstruct env)
        (cond ((vector? env) (reconstruct (cdr (assq (vector-ref env 0) table))))
              ((pair? env)   (cons (car env)
                                   (reconstruct (cdr env))))
              (else env)))
      (map (lambda (reflected-entry)
             (cons (car reflected-entry)
                   (map reconstruct (cdr reflected-entry)))) 
           datum)))
  
  ;;;=========================================================================
  ;;;
  ;;; Macros:
  ;;;
  ;;;=========================================================================
  
  ;; Expanders are system macros that fully expand
  ;; their arguments to core Scheme, while
  ;; transformers and variable transformers are 
  ;; user macros.  
  
  ;; <type> ::= expander | transformer | variable-transformer
  
  (define-struct macro (type proc))
  
  (define (make-expander proc)             (make-macro 'expander proc))
  (define (make-transformer proc)          (make-macro 'transformer proc))
  (define (make-variable-transformer proc) (make-macro 'variable-transformer proc))
  
  (define (make-user-macro proc-or-macro)
    (if (procedure? proc-or-macro)
        (make-transformer proc-or-macro)
        proc-or-macro))
  
  (define *macro-toplevel-env* (make-parameter (make-unit-env))) 
  (define *macro-usage-env*    (make-parameter (*macro-toplevel-env*)))
  
  ;; Returns <macro> | #f
  
  (define (macro-use t)            
    (let ((key (if (pair? t)
                   (car t)
                   t)))
      (and (identifier? key)
           (env-lookup (denotation key) (*macro-usage-env*) #f))))
  
  ;; Registering macro:
  
  (define (register-macro! denotation level proc)
    (env-extend! (list (cons denotation (make-user-macro proc)))
                 (*macro-usage-env*)))
  
  ;;;=========================================================================
  ;;;
  ;;; Expander dispatch:
  ;;;
  ;;;=========================================================================
  
  (define (expand t)
    (stacktrace t
                (lambda ()
                  (cond ((macro-use t) => (lambda (macro) 
                                            (*mark* (generate-mark))
                                            (let ((expanded-once ((macro-proc macro) t)))
                                              (case (macro-type macro)
                                                ((expander) expanded-once)
                                                (else      (expand expanded-once))))))
                        ((identifier? t)   (usage-denotation t))
                        ((list? t)         (map expand t))
                        ((const? t)        t)
                        (else              (syntax-violation #f "Invalid syntax object" t))))))
  
  ;; Only expands while t is a user macro invocation.
  ;; Used by expand-lambda to detect internal definitions.
  
  (define (head-expand t)
    (stacktrace t
                (lambda ()
                  (cond
                    ((macro-use t) => (lambda (macro)
                                        (*mark* (generate-mark))
                                        (case (macro-type macro)
                                          ((expander) t)  
                                          (else          
                                           (head-expand ((macro-proc macro) t)))))) 
                    (else t)))))
  
  (define (const? t)
    (or (null?    t)
        (boolean? t)
        (number?  t)
        (string?  t)
        (char?    t)))
  
  ;;;=========================================================================
  ;;;
  ;;; Quote, if, set!, begin, let-syntax, letrec-syntax:
  ;;;
  ;;;=========================================================================
  
  (define (expand-quote exp)
    (or (and (list? exp)
             (= (length exp) 2))
        (invalid-form exp))
    (syntax->datum exp))
  
  (define (expand-if exp)
    (or (and (list? exp)
             (<= 3 (length exp) 4))
        (invalid-form exp))
    `(if ,(expand (cadr exp))
         ,(expand (caddr exp))
         ,@(if (= (length exp) 4)
               (list (expand (cadddr exp)))
               `())))
  
  (define (expand-set! exp)
    (or (and (list? exp)
             (= (length exp) 3)
             (identifier? (cadr exp)))
        (invalid-form exp))
    (cond ((macro-use (cadr exp)) 
           => (lambda (macro) 
                (case (macro-type macro)
                  ((variable-transformer)
                   (expand ((macro-proc macro) exp)))
                  (else
                   (syntax-violation 'set! "Syntax being set! is not a variable transformer." 
                                     exp)))))
          (else `(set! ,(usage-denotation (cadr exp))     
                       ,(expand (caddr exp))))))
  
  ;; Expression begin.
  
  (define (expand-begin exp)
    (or (and (list? exp)
             (not (null? (cdr exp))))
        (invalid-form exp))
    (scan-sequence 'expression-sequence
                   (*usage-env*)             
                   (*macro-usage-env*) 
                   #f
                   (cdr exp)
                   (lambda (forms no-syntax-definitions no-bound-variables)
                     `(begin ,@(map cdr forms)))))
  
  ;; Expression let(rec)-syntax:
  
  (define (expand-local-syntax t)
    (expand-begin `(,(rename 'begin) ,t)))
  
  ;; And and or must be primitive, since they are also part of the library 
  ;; language, which is primitive.
  
  (define (expand-and exp)
    (or (list? exp)
        (invalid-form exp))
    (cond ((null? (cdr exp))  #t)
          ((null? (cddr exp)) (expand (cadr exp)))
          (else 
           `(if ,(expand (cadr exp))
                ,(expand `(,(rename 'and) ,@(cddr exp)))
                #f))))
  
  (define (expand-or exp)
    (or (list? exp)
        (invalid-form exp))
    (cond ((null? (cdr exp))  #f)
          ((null? (cddr exp)) (expand (cadr exp)))
          (else 
           `(let ((x ,(expand (cadr exp))))
              (if x x ,(expand `(,(rename 'or) ,@(cddr exp)))))))) 
  
  ;;;=========================================================================
  ;;;
  ;;; Lambda:
  ;;;
  ;;;=========================================================================
  
  (define (expand-lambda exp)
    (or (and (pair?    exp)
             (pair?    (cdr exp))
             (formals? (cadr exp))
             (list?    (cddr exp)))
        (invalid-form exp))
    (let ((formals (cadr exp))
          (body    (cddr exp)))
      (parameterize ((*usage-env* (env-extend (map make-local-entry (flatten formals)) (*usage-env*))))
        (let ((formals (dotted-map denotation formals)))
          (parameterize ((*usage-env*       (env-extend '() (*usage-env*)))   ; new scope
                         (*macro-usage-env* (env-extend '() (*macro-usage-env*)))) 
            (scan-sequence 'lambda 
                           (*usage-env*) 
                           (*macro-usage-env*) 
                           make-local-entry
                           body 
                           (lambda (forms syntax-definitions bound-variables)
                             `(lambda ,formals
                                ((lambda ,bound-variables
                                   ,@(emit-body forms 'set!))
                                 ,@(map (lambda (ignore) `(unspecified))
                                        bound-variables))))))))))
  
  (define (formals? s)
    (or (null? s)
        (identifier? s)
        (and (pair? s)
             (identifier? (car s))
             (formals? (cdr s))
             (not (dotted-memp (lambda (x) 
                                 (bound-identifier=? x (car s)))
                               (cdr s))))))
  
  ;;=========================================================================
  ;;;
  ;;; Bodies and sequences:
  ;;;
  ;;;=========================================================================
  
  ;; R6RS splicing of internal let-syntax and letrec-syntax (and only
  ;; this) requires that we control the bindings visible in each
  ;; expression of the body separately.  This is done by attaching
  ;; any extra bindings that should be visible in the expression
  ;; (over and above the usual bindings) to the expression.
  ;; We call the resulting data structure a wrap. 
  ;; Wraps are only used internally in processing of bodies.
  
  (define (make-wrap usage-diff macros-diff exp)
    (list usage-diff macros-diff exp))
  
  (define wrap-usage-diff  car)
  (define wrap-macros-diff cadr)
  (define wrap-exp         caddr)
  
  ;; Makes the additional bindings visible and then applies the operation
  ;; to the expression in the wrap.  Here the global fluid parameters become
  ;; a bit inelegant, and I may convert them to ordinary arguments in
  ;; future.  
  
  (define (do-wrap operation w . args)                                  
    (parameterize ((*usage-env*       (env-extend (wrap-usage-diff w)  (*usage-env*)))
                   (*macro-usage-env* (env-extend (wrap-macros-diff w) (*macro-usage-env*))))
      (apply operation (wrap-exp w) args)))
  
  ;; Copy bindings from w to expression exp.  
  
  (define (copy-wrap w exp)
    (make-wrap (wrap-usage-diff w) 
               (wrap-macros-diff w)
               exp))
  
  ;; The continuation k is evaluated in the body environment.  This is
  ;; used for example by expand-library to obtain the correct bindings of
  ;; exported identifiers.  
  ;; Common-env is shared whose lefmost frame is updated destructively 
  ;; as bindings become known:
  ;;
  ;; <body-type> ::= toplevel | library | program | lambda | expression-sequence
  
  (define (scan-sequence body-type common-env common-macro-env binder body-forms k)
    
    ;; Each <form> ::= (<symbol> . <wrap>)             (definition whose rhs is to be expanded)
    ;;              |  (#t       . <wrap>)             (expression to be expanded)
    ;;              |  (#f       . <s-expression>)     (expression already expanded)
    
    (define (expand-forms forms)
      (map (lambda (form)
             (cons (car form)
                   (if (car form)
                       (do-wrap expand (cdr form))
                       (cdr form))))
           forms))
    
    (let ()
      (*used* (cons '() (*used*)))  ; start new used scope 
      
      (let loop ((ws              (map (lambda (e) (make-wrap '() '() e)) body-forms))
                 (forms           '())
                 (syntax-defs     '())
                 (bound-variables '()))               
        (if (null? ws)
            (begin 
              (check-expression-body body-type forms body-forms)
              (*used* (cons (append (car (*used*))    ; merge used with parent scope 
                                    (cadr (*used*))) 
                            (cddr (*used*))))
              (k (reverse (expand-forms forms))
                 (reverse syntax-defs)
                 bound-variables)) 
            (let* ((w  (copy-wrap (car ws) (do-wrap head-expand (car ws))))  
                   (ws (cdr ws))
                   (type (do-wrap (lambda (t) 
                                    (and (pair? t)
                                         (identifier? (car t))
                                         (denotation (car t))))
                                  w)))
              (check-expression-sequence body-type type w)
              (case type
                ((import)
                 (check-toplevel body-type type w)
                 (let-values (((imported-libraries imports) (do-wrap scan-imports w)))
                   (parameterize ((*macro-usage-env* common-macro-env))  ; scope for visit
                     (import-libraries imported-libraries 0 'compile))
                   (env-import! (car (wrap-exp w)) imports common-env)
                   (loop ws
                         (cons (cons #f `($import-libraries ',imported-libraries 0 'execute)) 
                               forms)
                         syntax-defs
                         bound-variables)))  
                ((program)     
                 (check-toplevel body-type type w)
                 (loop ws
                       (cons (cons #f (do-wrap expand-program w)) forms)
                       syntax-defs
                       bound-variables))
                ((library) 
                 (check-toplevel body-type type w)
                 (loop ws
                       (cons (cons #f (do-wrap expand-library w)) forms)
                       syntax-defs
                       bound-variables))
                ((define)
                 (let-values (((id rhs) (do-wrap parse-definition w)))
                   (check-duplicate id common-env body-type w)
                   (check-used      id w)
                   (check-definition-follows-expression body-type forms 'define w) 
                   (env-extend! (list (binder id)) common-env)
                   (loop ws              
                         (cons (cons (denotation id) (copy-wrap w rhs))  
                               forms)
                         syntax-defs
                         (cons (denotation id) bound-variables))))
                ((define-syntax)   
                 (let-values (((id rhs) (do-wrap parse-definition w)))
                   (check-duplicate id common-env body-type w)
                   (check-used      id w)
                   (check-definition-follows-expression body-type forms 'define-syntax w) 
                   (env-extend! (list (binder id)) common-env)
                   (let ((rhs (parameterize ((*level* (+ 1 (*level*))))
                                (do-wrap expand (copy-wrap w rhs)))))
                     (env-extend! (list (cons (denotation id)           
                                              (make-user-macro (eval rhs)))) 
                                  common-macro-env)
                     (loop ws 
                           forms
                           (cons (cons (denotation id) rhs)  
                                 syntax-defs)
                           bound-variables))))      
                ((begin)
                 (loop (append (map (lambda (exp)
                                      (copy-wrap w exp))
                                    (cdr (wrap-exp w)))
                               ws)
                       forms
                       syntax-defs
                       bound-variables))
                ((let-syntax letrec-syntax)
                 (let-values (((formals vals body) (do-wrap parse-local-syntax w)))
                   (let* ((entries    (map make-local-entry formals))
                          (usage-diff (append entries (wrap-usage-diff w)))
                          (rhs-env    (env-extend usage-diff (*usage-env*))) 
                          (macros
                           (map (lambda (val)
                                  (eval (do-wrap (lambda (e)
                                                   (parameterize ((*level* (+ 1 (*level*))))
                                                     (case type
                                                       ((let-syntax) (expand e))
                                                       ((letrec-syntax)
                                                        (parameterize ((*usage-env* rhs-env))
                                                          (expand e))))))
                                                 (copy-wrap w val))))
                                vals))
                          (macros-diff
                           (append (map (lambda (entry macro)
                                          (cons (binding-name (cdr entry))
                                                (make-user-macro macro)))
                                        entries
                                        macros)
                                   (wrap-macros-diff w))))
                     (loop (cons (make-wrap usage-diff
                                            macros-diff
                                            `(,(rename 'begin) . ,body))
                                 ws)
                           forms
                           syntax-defs 
                           bound-variables))))
                (else
                 (loop ws
                       (cons (cons #t w) forms)
                       syntax-defs 
                       bound-variables))))))))
  
  (define (emit-body body-forms define-or-set)
    (map (lambda (body-form)
           (if (symbol? (car body-form))
               `(,define-or-set ,(car body-form) ,(cdr body-form))
               (cdr body-form)))
         body-forms))
  
  (define (check-expression-sequence body-type type w)
    (and (eq? body-type 'expression-sequence)
         (memq type '(import program library declare define define-syntax))
         (syntax-violation type "Invalid form in expression sequence"
                           (wrap-exp w))))
  
  (define (check-toplevel body-type from w)
    (and (not (eq? body-type 'toplevel))
         (syntax-violation from "Expression may only occur at toplevel"
                           (wrap-exp w))))
  
  (define (check-definition-follows-expression body-type forms from w)
    (and (not (memq body-type `(toplevel program)))
         (not (null? forms))
         (not (symbol? (car (car forms))))
         (syntax-violation from "Definitions may not follow expressions in a body"
                           (wrap-exp w))))
  
  (define (check-duplicate id env body-type w)  
    (and (not (eq? body-type 'toplevel))
         (duplicate id env)
         (syntax-violation 'definition (string-append "Duplicate binding of " 
                                                      (symbol->string (syntax->datum id))
                                                      " in body")
                           (wrap-exp w))))
  
  (define (check-used id w)
    (let* ((already-used (car (*used*)))  ; only current scope
           (denotation (denotation id)))  ; this changes *used* and must follow previous
      (if (memp (lambda (entry)
                  (and (eq? denotation (cdr entry))
                       (bound-identifier=? id (car entry))))
                already-used)
          (syntax-violation 'definition (string-append "Redefinition of identifier " 
                                                       (symbol->string (syntax->datum id))
                                                       " that has already been used during expansion of body")
                            (wrap-exp w)))))
  
  (define (check-expression-body body-type forms body-forms)
    (and (memq body-type '(lambda program))
         (or (null? forms) 
             (symbol? (caar forms)))
         (syntax-violation body-type "Body must be nonempty and end with an expression" body-forms)))
  
  (define (parse-definition t)        
    (or (and (pair? t)
             (pair? (cdr t)))
        (syntax-violation #f "Invalid definition format" t))
    (let ((k    (car t))
          (head (cadr t))
          (body (cddr t)))
      (cond ((and (identifier? head)
                  (list? body)
                  (<= (length body) 1))
             (values head (if (null? body)
                              `(,(rename 'unspecified))
                              (car body))))
            ((and (pair? head)
                  (identifier? (car head))
                  (formals? (cdr head)))
             (values (car head)
                     `(,(rename 'lambda) ,(cdr head) . ,body)))
            (else (syntax-violation #f "Invalid definition format" t)))))
  
  (define (parse-local-syntax t)            
    (or (and (pair? t)
             (pair? (cdr t))
             (list? (cadr t))
             (list? (cddr t))
             (for-all (lambda (binding)
                       (and (pair? binding)
                            (identifier? (car binding))
                            (pair? (cdr binding))
                            (null? (cddr binding))))
                     (cadr t)))
        (syntax-violation #f "Invalid form" t))
    (let ((formals (map car (cadr t)))
          (exps    (map cadr (cadr t)))
          (body    (cddr t)))
      (or (formals? formals)
          (syntax-violation #f "Duplicate binding" t))
      (values formals
              exps
              body)))
  
  ;;;=========================================================================
  ;;;
  ;;; Syntax-case:
  ;;;
  ;;;=========================================================================
  
  (define *pvar-env* (make-parameter (make-unit-env)))   
  
  (define (expand-syntax-case exp)
    (if (and (list? exp)
             (>= (length exp) 3))
        (let ((literals (caddr exp))
              (clauses (cdddr exp)))
          (if (and (list? literals)
                   (for-all identifier? literals))
              (let ((input (generate-guid 'input)))
                `(let ((,input ,(expand (cadr exp))))
                   ,(process-clauses clauses input literals)))
              (syntax-violation 'syntax-case "Invalid literals list" exp literals)))
        (invalid-form exp)))
  
  (define (process-clauses clauses input literals)
    
    (define (process-match input pattern sk fk)
      (cond 
        ((not (symbol? input)) (let ((temp (generate-guid 'temp)))
                                 `(let ((,temp ,input))
                                    ,(process-match temp pattern sk fk))))
        ((and (identifier? pattern) 
              (memp (lambda (x) 
                      (bound-identifier=? x pattern))
                    literals))
         `(if (and ($identifier? ,input)
                   ($free-identifier=? ,input ,(syntax-reflect pattern)))
              ,sk
              ,fk))
        ((ellipses? pattern)       (syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
        ((null? pattern)           `(if (null? ,input) ,sk ,fk))
        ((const? pattern)          `(if (equal? ,input ',pattern) ,sk ,fk))
        ((wildcard? pattern)       sk)
        ((identifier? pattern)     `(let ((,(denotation pattern) ,input)) ,sk))    
        ((segment-pattern? pattern)          
         (let ((tail-pattern (cddr pattern)))
           (if (null? tail-pattern)
               (let ((mapped-pvars (map denotation (map car (pattern-vars (car pattern) 0))))) 
                 (if (identifier? (car pattern))                         ; +++
                     `(if (list? ,input)                                 ; +++
                          (let ((,(denotation (car pattern)) ,input))    ; +++     
                            ,sk)                                         ; +++
                          ,fk)                                           ; +++
                     (let ((columns (generate-guid 'cols))
                           (rest    (generate-guid 'rest)))
                       `($map-while (lambda (,input)
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
               (let ((tail-length (dotted-length tail-pattern)))
                 `(if (>= ($dotted-length ,input) ,tail-length) 
                      ,(process-match `($dotted-butlast ,input ,tail-length) 
                                      `(,(car pattern) ,(cadr pattern))
                                      (process-match `($dotted-last ,input ,tail-length)
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
        (else (syntax-violation 'syntax-case "Invalid pattern" pattern))))
    
    (define (pattern-vars pattern level)     
      (cond 
        ((identifier? pattern)      (if (or (ellipses? pattern)
                                            (wildcard? pattern) 
                                            (memp (lambda (x) 
                                                    (bound-identifier=? x pattern))
                                                  literals))
                                        '()
                                        (list (cons pattern level)))) 
        ((segment-pattern? pattern) (append (pattern-vars (car pattern) (+ level 1))
                                            (pattern-vars (cddr pattern) level)))
        ((pair? pattern)            (append (pattern-vars (car pattern) level)
                                            (pattern-vars (cdr pattern) level)))
        ((vector? pattern)          (pattern-vars (vector->list pattern) level))
        (else                       '())))
    
    (define (process-clause clause input fk)
      (or (and (list? clause)
               (>= (length clause) 2))
          (syntax-violation 'syntax-case "Invalid clause" clause))
      (let* ((pattern  (car clause))
             (template (cdr clause))
             (pvars    (pattern-vars pattern 0)))
        (or (set? (map car pvars) bound-identifier=?)
            (syntax-violation 'syntax-case "Repeated pattern variable" clause (map car pvars)))
        (let ((entries (map make-local-entry (map car pvars))))
          (parameterize ((*usage-env* (env-extend entries (*usage-env*)))
                         (*pvar-env*  (env-extend (map (lambda (entry pvar)   
                                                         (cons (binding-name (cdr entry))
                                                               (cdr pvar)))
                                                       entries
                                                       pvars)
                                                  (*pvar-env*)))) 
            (process-match input
                           pattern
                           (cond ((null? (cdr template))
                                  (expand (car template)))
                                 ((null? (cddr template))
                                  `(if ,(expand (car template))
                                       ,(expand (cadr template))
                                       ,fk))   
                                 (else (syntax-violation 'syntax-case "Invalid clause" clause)))
                           fk)))))
    
    ;; process-clauses
    
    (if (null? clauses)
        `($invalid-form ,input)
        (let ((fail  (generate-guid 'fail)))
          `(let ((,fail (lambda () ,(process-clauses (cdr clauses) input literals))))
             ,(process-clause (car clauses) input `(,fail))))))
  
  (define (wildcard? x) (free=? x '_))
  
  ;; Ellipsis utilities:
  
  (define (ellipses? x) (free=? x '...))
  
  (define (segment-pattern? pattern)
    (and (segment-template? pattern)
         (or (for-all (lambda (p) 
                       (not (ellipses? p)))
                     (flatten (cddr pattern)))
             (syntax-violation 'syntax-case "Invalid segment pattern" pattern))))
  
  (define (segment-template? pattern)
    (and (pair? pattern)
         (pair? (cdr pattern))
         (identifier? (cadr pattern))
         (ellipses? (cadr pattern))))
  
  ;; Count the number of `...'s in PATTERN.
  
  (define (segment-depth pattern)
    (if (segment-template? pattern)
        (+ 1 (segment-depth (cdr pattern)))
        0))
  
  ;; Get whatever is after the `...'s in PATTERN.
  
  (define (segment-tail pattern)
    (let loop ((pattern (cdr pattern)))
      (if (and (pair? pattern)
               (identifier? (car pattern))
               (ellipses? (car pattern)))   
          (loop (cdr pattern))
          pattern)))
  
  ;; Ellipses-quote:
  
  (define (ellipses-quote? template)
    (and (pair? template)
         (ellipses? (car template))
         (pair? (cdr template))
         (null? (cddr template))))
  
  ;;;=========================================================================
  ;;;
  ;;; Syntax:
  ;;;
  ;;;=========================================================================
  
  (define (expand-syntax form) 
    (or (and (pair? form)
             (pair? (cdr form))
             (null? (cddr form)))
        (invalid-form form))
    (process-template (cadr form) 0 #f))
  
  (define (process-template template dim quote-ellipses)
    (cond ((and (ellipses? template)
                (not quote-ellipses))
           (syntax-violation 'syntax "Invalid occurrence of ellipses in syntax template" template))
          ((identifier? template)
           (let ((denotation (denotation template)))
             (cond ((env-lookup denotation (*pvar-env*) #f)  
                    => (lambda (pdim)
                         (if (<= pdim dim)
                             denotation 
                             (syntax-violation 'syntax "Template dimension error (too few ...'s?)"
                                               template))))
                   (else (syntax-reflect template)))))
          ((ellipses-quote? template)
           (process-template (cadr template) dim #t))
          ((and (segment-template? template)
                (not quote-ellipses))
           (let* ((depth (segment-depth template))
                  (seg-dim (+ dim depth))
                  (vars
                   (map (lambda (id) 
                          (usage-denotation id)) 
                        (free-meta-variables (car template) seg-dim '()))))
             (if (null? vars)
                 (syntax-violation 'syntax "too many ...'s" template)
                 (let* ((x (process-template (car template) seg-dim quote-ellipses))
                        (gen (if (equal? (list x) vars)   ; +++
                                 x                        ; +++
                                 `(map (lambda ,vars ,x)
                                       ,@vars)))
                        (gen (do ((d depth (- d 1))
                                  (gen gen `(apply append ,gen)))
                               ((= d 1)
                                gen))))
                   (if (null? (segment-tail template))   
                       gen                                ; +++
                       `(append ,gen ,(process-template (segment-tail template) dim quote-ellipses)))))))
          ((pair? template)
           `(cons ,(process-template (car template) dim quote-ellipses)
                  ,(process-template (cdr template) dim quote-ellipses)))
          ((vector? template)
           `(list->vector ,(process-template (vector->list template) dim quote-ellipses)))
          (else
           `(quote ,(expand template)))))
  
  ;; Return a list of meta-variables of given higher dim
  
  (define (free-meta-variables template dim free)
    (cond ((identifier? template)
           (if (and (not (memp (lambda (x) 
                                 (bound-identifier=? x template))
                               free))
                    (let ((pdim (env-lookup (denotation template) (*pvar-env*) #f)))
                      (and pdim
                           (>= pdim dim))))
               (cons template free)
               free))
          ((segment-template? template)
           (free-meta-variables (car template) dim
                                (free-meta-variables (cddr template) dim free)))
          ((pair? template)
           (free-meta-variables (car template) dim
                                (free-meta-variables (cdr template) dim free)))
          (else free)))
  
  ;;;==========================================================================       
  ;;;
  ;;; Libraries:
  ;;;
  ;;;==========================================================================
  
  (define (expand-program t) 
    (expand-library-or-program 
     `(,(car t) (,(datum->syntax (car t) (generate-guid 'program))) 
        (,(datum->syntax (car t) 'export)) . ,(cdr t))
     'program))
  
  (define (expand-library t) 
    (expand-library-or-program t 'library))
  
  ;; <library-type> ::= library | program
  
  (define (expand-library-or-program t library-type)
    (or (and (list? t)              
             (>= (length t) 4))       
        (syntax-violation 'library "Invalid number of clauses in library" t))
    
    (let* ((keyword (car t))
           (name    (scan-library-name (cadr t))))
      
      (let-values (((exports)                    (scan-exports (caddr t))) 
                   ((imported-libraries imports) (scan-imports (cadddr t))) 
                   ((body)                       (cddddr t)))
        
        ;; Make sure we start with a clean compilation environment,
        ;; and that we restore any global state afterwards.
        ;; Make sure macros registered when visiting
        ;; imported libraries are removed once we are done.
        
        (parameterize ((*visited*         '())
                       (*invoked*         '())
                       (*imported*        '())
                       (*error-if-free?*  #t)
                       (*reflected-envs*  '())   
                       (*pvar-env*        (make-unit-env))
                       (*usage-env*       (make-unit-env)) 
                       (*macro-usage-env* (env-extend '() primitive-macro-env)))
          
          (import-libraries imported-libraries 0 'compile)
          (env-import! keyword imports (*usage-env*))
          
          ;; Obtain a mark so that compress-reflected-envs will know
          ;; which reflected environments are created for use by this
          ;; library and should be included in the object code.
          
          (let ((stop-mark (current-reflected-envs-mark)))  ; +++
            
            (scan-sequence library-type 
                           (*usage-env*)
                           (*macro-usage-env*)              
                           make-local-entry
                           body
                           (lambda (forms syntax-definitions bound-variables)
                             
                             ;; This has to be done here, when all bindings are established.
                             (let* ((exports (map (lambda (entry)
                                                    (cons (identifier-name (car entry))
                                                          (let ((binding (lookup-binding (cadr entry))))
                                                            (or binding
                                                                (syntax-violation 'library "Unbound export" t 
                                                                                  (car entry))))))
                                                  exports))       
                                    (expanded-library
                                     (case library-type
                                       ((program) `(begin 
                                                     ($import-libraries ',imported-libraries 0 'execute)
                                                     ,@(emit-body forms 'define)))
                                       ((library)
                                        (let ((level (generate-guid 'level)))
                                          `(begin
                                             (define ,(name-for 'envs    name) ($uncompress ',(compress-reflected-envs stop-mark)))
                                             (define ,(name-for 'exports name) ',exports) 
                                             (define ,(name-for 'imports name) ',imported-libraries)
                                             
                                             (define (,(name-for 'visit name) ,level) 
                                               ,@(map (lambda (def)
                                                        `($register-macro! ',(car def) 0 ,(cdr def)))
                                                      syntax-definitions)
                                               (unspecified))
                                             
                                             ,@(map (lambda (var)
                                                      `(define ,var (unspecified)))
                                                    bound-variables)
                                             
                                             (define (,(name-for 'invoke name) ,level)                                           
                                               ,@(emit-body forms 'set!)
                                               (unspecified))))))))
                                     
                               ;; Make library available for further expansion.
                               (if (eq? library-type 'library) 
                                   (eval expanded-library))
                               
                               expanded-library))))))))
    
  (define *visited*  (make-parameter '())) 
  (define *invoked*  (make-parameter '())) 
  (define *imported* (make-parameter '()))
  
  (define (env-import! keyword imports env)
    (env-extend! (map (lambda (import) 
                        (make-entry (car import)
                                    (identifier-marks keyword)
                                    (cdr import)))
                      imports)
                 env))
  
  ;; session ::= compile | execute
  
  (define (import-libraries imports level session)
    (if (not (null? imports))
        (let* ((import (car imports))
               (name   (car import))
               (levels (cdr import)))
          (if (null? levels)
              (import-libraries (cdr imports) level session)
              (begin
                (import-library name (+ level (car levels)) session)
                (import-libraries (cons (cons name (cdr levels))
                                        (cdr imports))
                                  level
                                  session))))))
  
  (define (import-library name level session)
    (and (not (member (cons name level) (*imported*)))
         (let ((imports (eval (name-for 'imports name))))
           ;; Do this first so accidental cyclic dependencies will not hang
           (*imported* (cons (cons name level) (*imported*)))
           (import-libraries imports level session)
           (and (>= level 0)
                (case session 
                  ((compile)
                   (and (>= level 0)
                        (let ((visited (member name (*visited*))))
                          (or visited 
                              (begin
                                (extend-reflected-envs! (eval (name-for 'envs name)))
                                (parameterize ((*level* level))
                                  (eval `(,(name-for 'visit name) ,level)))
                                (*visited* (cons name (*visited*)))))))
                   (and (>= level 1)
                        (let ((invoked (member name (*invoked*))))
                          (or invoked 
                              (begin 
                                (parameterize ((*level* level))
                                  (eval `(,(name-for 'invoke name) ,level)))
                                (*invoked* (cons name (*invoked*))))))))
                  ((execute)
                   (and (= level 0)
                        (eval `(,(name-for 'invoke name) ,level)))))))))
 
  ;; Returns ((<rename-identifier> <identifier> <level> ...) ...)
  
  (define (scan-exports clause) 
    (and (pair? clause)
         (free=? (car clause) 'export) 
         (list? (cdr clause))) 
    (let ((exports (apply append 
                          (map scan-export-spec (cdr clause)))))
      (or (set? exports 
                (lambda (x y) 
                  (eq? (identifier-name (car x))
                       (identifier-name (car y)))))
          (syntax-violation 'export "Duplicate export" clause))
      exports))
  
  (define (scan-export-spec spec)
    (let ((levels `(0))               ;; Will be ignored in current implementation, but keep data   
          (export-sets (list spec)))  ;; structures and interfaces same in case FOR exports return.
      (map (lambda (rename-pair)
             (cons (car rename-pair)
                   (cons (cdr rename-pair)
                         levels)))
           (apply append (map scan-export-set export-sets)))))
  
  (define (scan-export-set set)
    (cond ((identifier? set)
           (list (cons set set)))
          ((rename-export-set? set)
           (map (lambda (entry)
                  (cons (cadr entry)
                        (car entry)))
                (cdr set)))
          (else
           (syntax-violation 'export "Invalid export set" set))))
  
  (define (scan-levels spec) 
    (cond ((for-spec? spec) 
           (let ((levels
                  (map (lambda (level)
                         (cond ((free=? level 'run)    0)
                               ((free=? level 'expand) 1)
                               ((meta-spec? level)     (cadr level))
                               (else (syntax-violation 'for "Invalid level in for spec" spec level))))    
                       (cddr spec))))
             (if (set? levels =)
                 levels
                 (syntax-violation 'for "Repeated level in for spec" spec))))
          (else '(0))))
  
  ;; Returns (values ((<library reference> <level> ...) ....)
  ;;                 ((<local name> . <binding>) ...))
  ;; with no repeats.  
  
  (define (scan-imports clause)
    (or (and (pair? clause)
             (free=? (car clause) 'import) 
             (list?  (cdr clause)))
        (syntax-violation 'import "Invalid import clause" clause))
    (scan-import-specs (cdr clause)))
  
  (define (scan-import-specs all-specs)
    (let loop ((specs all-specs)
               (imported-libraries '())
               (imports '()))
      (if (null? specs)
          (values imported-libraries (unify-imports imports))
          (let-values (((library-ref levels more-imports) (scan-import-spec (car specs))))
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
  
  (define (scan-import-spec spec)
    (let ((levels (scan-levels spec)))
      (let loop ((import-set (if (for-spec? spec) 
                                 (cadr spec) 
                                 spec))
                 (renamer (lambda (x) x)))
        
        ;; Extension for importing unadorned primitives:
        
        (cond ((primitive-set? import-set)
               (values #f
                       levels 
                       ;; renamer will return <symbol> | #f
                       (filter car
                               (map (lambda (name)
                                      (cons name
                                            (make-binding name levels)))
                                    (syntax->datum (cadr import-set))))))
              ((and (list? import-set)
                    (>= (length import-set) 2)
                    (or (only-set? import-set)
                        (except-set? import-set)
                        (prefix-set? import-set)
                        (rename-set? import-set)))
               (loop (cadr import-set)
                     (compose renamer 
                              ;; Remember to correctly propagate if x is #f
                              (lambda (x)
                                (cond
                                  ((only-set? import-set)
                                   (and (memq x (syntax->datum (cddr import-set)))
                                        x)) 
                                  ((except-set? import-set)
                                   (and (not (memq x (syntax->datum (cddr import-set))))
                                        x)) 
                                  ((prefix-set? import-set)
                                   (and x
                                        (string->symbol
                                         (string-append (symbol->string (syntax->datum (caddr import-set)))
                                                        (symbol->string x)))))
                                  ((rename-set? import-set)
                                   (let ((renames (syntax->datum (cddr import-set))))
                                     (cond ((assq x renames) => cadr)
                                           (else x)))) 
                                  (else (syntax-violation 'import "Invalid import set" import-set))))))) 
              ((library-ref? import-set)
               (let* ((exports (eval (name-for 'exports (syntax->datum import-set))))
                      (imports 
                       ;; renamer will return <symbol> | #f
                       (filter car
                               (map (lambda (export)
                                      (cons (renamer (car export))         
                                            (make-binding (binding-name (cdr export))
                                                          (compose-levels levels 
                                                                          (binding-levels (cdr export))))))
                                    exports)))
                      (all-import-levels (apply unionv
                                                (map (lambda (import)
                                                       (binding-levels (cdr import)))
                                                     imports))))
                 (values (syntax->datum import-set)
                         levels
                         imports)))
              (else (syntax-violation 'import "Invalid import set" import-set))))))
  
  (define (compose-levels levels levels*)
    (apply unionv 
           (map (lambda (level)
                  (map (lambda (level*)
                         (+ level level*))
                       levels*))
                levels)))
  
  ;; Argument is of the form ((<local name> <binding>) ...)
  ;; where combinations (<local name> (binding-symbol <binding>)) may be repeated.
  ;; Return value is of the same format but with no repeats and
  ;; where union of (binding-levels <binding>)s has been taken for any original repeats.
  ;; An error is signaled if same <local> occurs with <binding>s
  ;; whose (binding-symbol <binding>)s are different.  
  
  (define (unify-imports imports)           
    (if (null? imports)                   
        '()
        (let ((first (car imports))
              (rest  (unify-imports (cdr imports))))
          (let loop ((rest rest)
                     (seen '()))
            (cond ((null? rest)
                   (cons first seen))
                  ((eq? (car first) (caar rest))
                   (or (eq? (binding-name (cdr first))
                            (binding-name (cdar rest)))
                       (syntax-violation 'import "Same name imported from different libraries"
                                         (car first)))
                   (cons (cons (car first)
                               (make-binding (binding-name (cdr first))
                                             (unionv (binding-levels (cdr first))
                                                     (binding-levels (cdar rest)))))
                         (append (cdr rest) seen)))
                  (else
                   (loop (cdr rest)
                         (cons (car rest) seen))))))))  
  
  (define (for-spec? spec)
    (and (list? spec)
         (>= (length spec) 3)
         (free=? (car spec) 'for)))
  
  (define (meta-spec? level)
    (and (list? level)
         (= (length level) 2)
         (free=? (car level) 'meta)
         (integer? (cadr level))))
  
  (define (only-set? set)
    (and (free=? (car set) 'only)
         (for-all identifier? (cddr set))))
  
  (define (except-set? set)
    (and (free=? (car set) 'except)
         (for-all identifier? (cddr set))))
  
  (define (prefix-set? set)
    (and (free=? (car set) 'prefix)
         (= (length set) 3)
         (identifier? (caddr set))))
  
  (define (rename-set? set)
    (and (free=? (car set) 'rename)
         (rename-list? (cddr set))))
  
  (define (primitive-set? set)
    (and (list? set)
         (= (length set) 2)
         (free=? (car set) 'primitives)
         (list (cadr set))
         (for-all identifier? (cadr set))))
  
  (define (rename-export-set? set)
    (and (list? set)
         (>= (length set) 1)
         (free=? (car set) 'rename)
         (rename-list? (cdr set))))
  
  (define (rename-list? ls)
    (for-all (lambda (e)
              (and (list? e)
                   (= (length e) 2)
                   (for-all identifier? e)))
            ls))
  
  (define (scan-library-name e)
    (or (library-name? e)
        (syntax-violation 'library "Invalid library name" e))
    (syntax->datum e))
  
  (define (library-name? e)
    (and (list? e)
         (let ((e (reverse e)))
           (and (for-all identifier? (cdr e))
                (or (identifier? (car e))
                    (and (list? (car e))
                         (for-all subversion? (car e))))))))

  (define (subversion? x)
    (and (integer? x)
         (>= x 0)))
  
  (define (library-name->string e)
    (string-append (symbol->string (car e))
                   (apply string-append 
                          (map (lambda (e)
                                 (string-append "."
                                                (symbol->string e)))
                               (let ((re (reverse (cdr e))))
                                 (if (and (pair? re)
                                          (list? (car re)))
                                     (reverse (cdr re))
                                     (cdr e)))))))
  
  (define (library-ref? e)
    (and (list? e)
         (let ((e (reverse e)))
           (and (for-all identifier? (cdr e))
                (or (identifier? (car e))
                    (and (list? (car e))
                         (for-all subversion-reference?
                                  (car e))))))))
  
  (define (subversion-reference? e)
    (or (subversion? e)
        (subversion-condition? e)))
  
  (define (subversion-condition? e)
    (and (list? e)
         (pair? e)
         (identifier? (car e))
         (case (denotation (car e))
           ((>= <=)  
            (and (= (length e) 2)
                 (subversion? (cadr e))))
           ((and or) 
            (for-all subversion-condition? (cdr e)))
           ((not)    
            (and (= (length e) 2)
                 (subversion-condition? (cadr e))))
           (else #f))))
  
  (define library-ref->string library-name->string) 
  
  (define (name-for command name)
    (free-name (string->symbol (string-append (library-ref->string name) ";"
                                              (symbol->string command)))))
  
  ;;;==========================================================================
  ;;;
  ;;; Debugging facilities:
  ;;;
  ;;;==========================================================================
  
  ;; Debugging information displayed by syntax-violation.
  
  (define *backtrace* (make-parameter '()))
  
  (define (stacktrace term thunk)
    (parameterize ((*backtrace* (cons term (*backtrace*))))
      (thunk)))
  
  (define (syntax-violation who message form . maybe-subform)
    (newline)
    (display "Syntax violation: ")
    (let ((who (if who 
                   who
                   (cond ((identifier? form) 
                          (syntax->datum form))
                         ((and (list? form)
                               (identifier? (car form)))
                          (syntax->datum (car form)))
                         (else ""))))
          (subform (cond ((null? maybe-subform) #f)
                         ((and (pair? maybe-subform)
                               (null? (cdr maybe-subform)))
                          (car maybe-subform))
                         (else (contract-violation 'syntax-violation "Invalid subform in syntax violation"
                                                   maybe-subform))))) 
      (display who) 
      (newline)
      (newline) 
      (display message)
      (newline)
      (newline)
      (if subform
          (begin (display "Subform: ") 
                 (pretty-print (syntax-debug subform))
                 (newline)))
      (display "Form: ")
      (pretty-print (syntax-debug form))
      (newline)
      (display "Backtrace: ")
      (newline)
      (newline)
      (for-each (lambda (exp)
                  (display "  ")
                  (pretty-print (syntax-debug exp))
                  (newline))
                (*backtrace*))
      (error 'syntax-violation "Put call to host error handling here")))
  
  (define (syntax-debug exp)
    (sexp-map (lambda (leaf)
                (cond ((identifier? leaf) 
                       (identifier-name leaf))
                      (else leaf)))
              exp))
  
  ;;;=====================================================================
  ;;;
  ;;; Utilities:
  ;;;
  ;;;=====================================================================
  
  (define (flatten l)
    (cond ((null? l) l)
          ((pair? l) (cons (car l)
                           (flatten (cdr l))))
          (else (list l))))
  
  (define (sexp-map f s)
    (cond ((null? s) '())
          ((pair? s) (cons (sexp-map f (car s))
                           (sexp-map f (cdr s))))
          ((vector? s)
           (apply vector (sexp-map f (vector->list s))))
          (else (f s))))
  
  (define (dotted-memp proc ls)
    (cond ((null? ls) #f)
          ((pair? ls) (if (proc (car ls))
                          ls
                          (dotted-memp proc (cdr ls))))
          (else (and (proc ls) 
                     ls))))
  
  (define (dotted-map f lst)
    (cond ((null? lst) '())
          ((pair? lst) (cons (f (car lst))
                             (dotted-map f (cdr lst))))
          (else (f lst))))
  
  ;; Returns 0 also for non-list a la SRFI-1 protest.
  
  (define (dotted-length dl)
    (cond ((null? dl) 0)
          ((pair? dl) (+ 1 (dotted-length (cdr dl))))
          (else 0)))
  
  (define (dotted-butlast ls n)
    (let recurse ((ls ls)
                  (length-left (dotted-length ls)))
      (cond ((< length-left n) (contract-violation 'dotted-butlast "List too short" ls n))
            ((= length-left n) '())
            (else
             (cons (car ls) 
                   (recurse (cdr ls)
                            (- length-left 1)))))))
  
  (define (dotted-last ls n)
    (let recurse ((ls ls)
                  (length-left (dotted-length ls)))
      (cond ((< length-left n) (contract-violation 'dotted-last "List too short" ls n))
            ((= length-left n) ls)
            (else  
             (recurse (cdr ls)
                      (- length-left 1))))))
  
  (define (map-while f lst k)
    (cond ((null? lst) (k '() '()))
          ((pair? lst)
           (let ((head (f (car lst))))
             (if head
                 (map-while f
                            (cdr lst)
                            (lambda (answer rest)
                              (k (cons head answer)
                                 rest)))
                 (k '() lst))))
          (else  (k '() lst))))
    
  (define (set? ls =)
    (or (null? ls)
        (and (not (memp (lambda (x) 
                          (= x (car ls))) 
                        (cdr ls)))
             (set? (cdr ls) =))))
  
  (define (unionv . sets)
    (cond ((null? sets) '())
          ((null? (car sets)) 
           (apply unionv (cdr sets)))
          (else 
           (let ((rest (apply unionv 
                              (cdr (car sets)) 
                              (cdr sets))))
             (if (memv (car (car sets)) rest)
                 rest
                 (cons (car (car sets)) rest))))))
  
  (define (compose f g) 
    (lambda (x) (f (g x))))
  
  (define (check x p? from)
    (or (p? x)
        (syntax-violation from "Invalid argument" x)))   
  
  (define (invalid-form exp)
    (syntax-violation #f "Invalid form" exp))
  
  (define syntax-error (make-expander invalid-form))
  
  ;;;==========================================================================
  ;;;
  ;;;  Eval and environment:
  ;;;
  ;;;==========================================================================
  
  (define eval-template
    (make-identifier 'eval-template
                     '() 
                     '() 
                     0))
  
  (define-struct r6rs-environment (imported-libraries imports))
  
  (define (environment . import-specs)
    (parameterize ((*usage-env* (make-unit-env)))                          
      (env-import! eval-template library-language (*usage-env*)) 
      (let-values (((imported-libraries imports)
                    (scan-import-specs
                     (map (lambda (spec) 
                            (datum->syntax eval-template spec))
                          import-specs))))
        (make-r6rs-environment imported-libraries imports))))
  
  (define (r6rs-eval exp env)
    (parameterize ((*usage-env* (make-unit-env)))
      (env-import! eval-template (r6rs-environment-imports env) (*usage-env*)) 
      (let ((exp (datum->syntax eval-template exp)))
        (import-libraries (r6rs-environment-imported-libraries env) 0 'compile)
        (import-libraries (r6rs-environment-imported-libraries env) 0 'execute)
        (eval (expand exp)))))
  
  ;;;==========================================================================
  ;;;
  ;;; Toplevel bootstrap:
  ;;;
  ;;;==========================================================================
  
  (define toplevel-template
    (make-identifier 'toplevel-template
                     '()                  
                     '() 
                     0))
  
  (define (source->syntax datum)
    (datum->syntax toplevel-template datum))
  
  (define *toplevel-env* (make-parameter (make-unit-env)))
  (define *usage-env*    (make-parameter (*toplevel-env*)))    
  
  (define library-language
    (map (lambda (name)
           (cons name (make-binding name '(0)))) 
         '(library 
           program
           import
           export
           for  
           run               
           expand
           meta
           only
           except
           prefix
           rename
           >=
           <=
           and 
           or 
           not
           primitives)))
  
  ;; These are the macros that may be used in libraries and programs.
  
  (define primitive-macros     
    `((lambda        . ,(make-expander expand-lambda))
      (if            . ,(make-expander expand-if))
      (set!          . ,(make-expander expand-set!))
      (begin         . ,(make-expander expand-begin))  
      (syntax        . ,(make-expander expand-syntax))
      (quote         . ,(make-expander expand-quote))
      (let-syntax    . ,(make-expander expand-local-syntax))   
      (letrec-syntax . ,(make-expander expand-local-syntax))
      (syntax-case   . ,(make-expander expand-syntax-case))
      (and           . ,(make-expander expand-and))
      (or            . ,(make-expander expand-or))
      (define        . ,syntax-error) 
      (define-syntax . ,syntax-error) 
      (_             . ,syntax-error)
      (...           . ,syntax-error)
      (declare       . ,syntax-error)
      (unsafe        . ,syntax-error) 
      (safe          . ,syntax-error) 
      (fast          . ,syntax-error) 
      (small         . ,syntax-error) 
      (debug         . ,syntax-error)))
  
  (define primitive-macro-env (env-extend primitive-macros (make-unit-env))) 
  
  ;; Includes library language.
  
  (define toplevel-primitive-macros
    (append primitive-macros
            `((program       . ,syntax-error)
              (library       . ,syntax-error)  
              (import        . ,syntax-error) 
              (for           . ,syntax-error)
              (run           . ,syntax-error) 
              (expand        . ,syntax-error) 
              (meta          . ,syntax-error) 
              (only          . ,syntax-error) 
              (except        . ,syntax-error) 
              (prefix        . ,syntax-error) 
              (rename        . ,syntax-error)
              (primitives    . ,syntax-error)
              ;; >=  already bound to primitive   
              ;; <=  ditto
              ;; not ditto
              ;; and ditto
              ;; or  ditto
              )))
  
  ;;;============================================================================
  ;;;
  ;;; REPL integration:
  ;;;
  ;;;============================================================================
  
  (define (repl exps)
    (for-each (lambda (exp)
                (for-each (lambda (exp)
                            (for-each (lambda (result)
                                        (display result)
                                        (newline))
                                      (call-with-values
                                       (lambda ()
                                         (*imported* '())
                                         (*invoked* '())
                                         (eval exp))
                                       list)))
                          (expand-toplevel-sequence (list exp))))
              exps))
  
  (define (reset-toplevel!)
    (*backtrace*       '())
    (*error-if-free?*  #f)
    (*level*           0)
    (*used*            (list '()))
    (*mark*            (generate-mark))
    (*pvar-env*        (make-unit-env))
    (*usage-env*       (*toplevel-env*))
    (*macro-usage-env* (*macro-toplevel-env*))
    (*visited*         '())
    (*invoked*         '())
    (*imported*        '()))
  
  (define (expand-toplevel-sequence forms)
    (reset-toplevel!)
    (expand-toplevel-sequence-more forms))
    
  (define (expand-toplevel-sequence-more forms)
    (scan-sequence 'toplevel
                   (*toplevel-env*)        
                   (*macro-toplevel-env*)  
                   make-toplevel-entry
                   (source->syntax forms)
                   (lambda (forms syntax-definitions bound-variables) 
                     (emit-body forms 'define))))
  
  ;;;==========================================================================
  ;;;
  ;;;  Load and expand-file:
  ;;;
  ;;;==========================================================================
  
  ;; This may be used as a front end for the compiler:
  
  (define (expand-file filename)
    (expand-toplevel-sequence (read-file filename)))
  
  ;; For libraries and toplevel forms.
  
  (define (r6rs-load filename)
    (for-each eval (expand-file filename)))
  
  ;; For program source files.
  
  (define (r6rs-load-program filename)
    (for-each eval (expand-toplevel-sequence (list (cons 'program (read-file filename))))))
  
  (define read-file
    (lambda (fn)
      (let ((p (open-input-file fn)))
        (let f ((x (read p)))
          (if (eof-object? x)
              (begin (close-input-port p) '())
              (cons x
                    (f (read p))))))))
  
  ;; Initial toplevel environments:
  
  (env-import! toplevel-template library-language (*toplevel-env*))
  (env-extend! toplevel-primitive-macros (*macro-toplevel-env*))
  
  ;; Exports:
  
  (set! $make-variable-transformer make-variable-transformer)
  (set! $identifier?               identifier?)
  (set! $bound-identifier=?        bound-identifier=?)
  (set! $free-identifier=?         free-identifier=?)
  (set! $generate-temporaries      generate-temporaries) 
  (set! $datum->syntax             datum->syntax)
  (set! $syntax->datum             syntax->datum)              
  (set! $environment               environment)
  (set! $r6rs-eval                 r6rs-eval)
  (set! $invalid-form              invalid-form)   
  (set! $uncompress                uncompress)
  (set! $register-macro!           register-macro!)
  (set! $import-libraries          import-libraries) 
  (set! $syntax-rename             syntax-rename)
  (set! $map-while                 map-while)
  (set! $dotted-length             dotted-length)
  (set! $dotted-butlast            dotted-butlast)
  (set! $dotted-last               dotted-last)
  (set! $r6rs-expand-file          expand-file)
  (set! $r6rs-load                 r6rs-load)  
  (set! $r6rs-load-program         r6rs-load-program)
  (set! $repl                      repl)
  (set! $r6rs-expand-toplevel-expressions expand-toplevel-sequence)
  (set! $r6rs-expand-more-toplevel-expressions expand-toplevel-sequence-more)
  
  ) ; Expander

;;;==========================================================================
;;;
;;;  Make derived libraries available:
;;;
;;;==========================================================================

;; This expands and loads the core libraries composing r6rs.
;; In production, instead of doing this, just include the result 
;; of compiling (expand-file "macros-derived.scm")
;; This should only be done after generate-guid above has been
;; suitably redefined so as to allow separate compilation.  

(define-syntax include-derived-defns
  (transformer 
   (lambda (exp ren cmp)
     (cons (ren 'begin)
           ($r6rs-expand-file (cadr exp))))))
;(define macros-derived-expanded ($r6rs-expand-file "macros-derived.scm"))
;(include-derived-defns "macros-derived-alt.scm")
(include-derived-defns "macros-derived.scm")
;($r6rs-load "macros-derived.scm")

    
(define (r6rs-repl)
  (parameterize 
    ((repl-evaluator 
      (let ((init-call #t)) 
        (lambda (expr env) 
          (let ((expand-init 
                 (lambda () 
                   (set! init-call #f)
                   ($r6rs-expand-toplevel-expressions (list expr))))
                (expand-more
                 (lambda () 
                   ($r6rs-expand-more-toplevel-expressions (list expr)))))
            (eval (car ((if init-call expand-init expand-more)))))))))
    (repl)))

;;;=================================================================================
;;;
;;; R6RS Macros and R6RS libraries:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;=================================================================================

;;; As allowed by that copyright statement, William D Clinger has made
;;; these modifications:
;;;
;;; This expander has been extended to support R7RS define-library and its
;;; friends.  Search for [R7RS].
;;;
;;; The definitions of repl, syntax-violation, and expand-library-or-program
;;; have been modified for Larceny.  Search for [Larceny].

;;;
;;;=================================================================================
;;;
;;; PORTING COMMENTS:
;;;
;;;=================================================================================
;;;
;;; The file compat-*.scm has to be loaded before loading this expander.
;;;
;;; Compat-*.scm should supply whatever is missing from your implementation of
;;; the following.
;;;
;;; NOTE: A purely r5rs approximation is provided that can be used
;;;       as a customization template.
;;;
;;;  - Procedures assertion-violation, memp, filter, for-all, pretty-print,
;;;    file-exists? and delete-file.
;;;  - Procedures make-record-type-descriptor, make-record-constructor-descriptor,
;;;    record-constructor, record-predicate and record-accessor.
;;;  - Procedure (ex:unique-token) that provides a numeric GUID string once per run.
;;;  - Single-character string ex:guid-prefix.  No builtin may start with this.
;;;  - Single-character string ex:free-prefix.  No builtin may start with this.
;;;  - Value ex:undefined representing the letrec black hole value.
;;;  - Symbol ex:undefined-set! representing the corresponding setter.
;;;
;;; HOOKS:
;;; ------
;;;
;;; For compiler and REPL integration, see the procedures
;;;
;;;   - ex:repl              : Use this as REPL evaluator.  See description below.
;;;   - ex:expand-file       : Use this to expand a file containing libraries and/or
;;;                            toplevel programs before loading into an r5rs-type system
;;;                            or feeding result to an r5rs-type compiler.
;;;                            Suitable for separate compilation.
;;;   - ex:run-r6rs-sequence : Evaluates a sequence of forms of the format
;;;                            <library>* | <library>* <toplevel program>.
;;;                            The <toplevel program> environment is separate from the 
;;;                            interactive REPL environment and does not persist
;;;                            between invocations of run-r6rs-sequence.  
;;;                            For importing and evaluating stuff in the persistent 
;;;                            interactive environment, ex:REPL should be used instead.
;;;   - ex:run-r6rs-program  : Same as ex:run-r6rs-sequence, except that it reads the 
;;;                            input from a file.
;;;   - ex:expand-r5rs-file  : For expanding r5rs-like toplevel files in a given environment.
;;;                            Mainly provided so this expander can expand itself, but may
;;;                            have other uses.  See the documentation below where the
;;;                            procedure is defined.  See also the note below on
;;;                            metacircularity.
;;;
;;; COMPILATION:
;;; ------------
;;;
;;; Example compilation scripts can be seen in examples.scm.
;;; The expander expands everything to r5rs toplevel definitions
;;; and expressions, so the expanded code should be compilable
;;; with an r5rs compiler.
;;;
;;; REPL:
;;; -----
;;;
;;; Example REPL interaction can be seen in examples.scm.
;;;
;;; The REPL goes beyond r6rs to allow incremental development in
;;; a toplevel environment.
;;; The developer can freely change, replace and make new toplevel
;;; definitions, evaluate toplevel expressions, enter libraries and
;;; <toplevel programs> at the prompt, as well as import libraries
;;; into the toplevel environment.
;;;    
;;; EX:REPL evaluates a sequence of library definitions, commands, and top-level 
;;; import forms in the interactive environment.  The semantics for 
;;; evaluating libraries in and importing bindings into the interactive 
;;; environment is consistent with the ERR5RS proposal at
;;; http://scheme-punks.cyber-rush.org/wiki/index.php?title=ERR5RS:Libraries.
;;; Bindings in the interactive environment persist between invocations 
;;; of REPL. 
;;;
;;; An example session where I do all these things is in examples.scm.
;;; All an integrator would need to do is to automate the call to
;;; ex:repl in the development system so users don't have to type
;;; (ex:repl '( <code> )) at each prompt.
;;;
;;; FORMAT OF EXPANDED CODE:
;;; ------------------------
;;;
;;; We expand internal and library definitions, as well as letrec and letrec*
;;; completely to lambda and set! (or more accurately, whatever ex:undefined-set!
;;; is set to).  This seems to be the preferred input format for Larceny.
;;; It would be very easy to abstract or change, but we don't bother for now
;;; until implementors other than Larceny show a serious interest.
;;;
;;; METACIRCULARITY AND BOOTSTRAPPING:
;;; ----------------------------------
;;;
;;; This section is mostly of interest for r5rs non-compliant systems.
;;;
;;; The expander relies on r5rs (or r6rs) syntax-rules and letrec-syntax
;;; and should run in a correct r5rs system, but if you don't have 
;;; r5rs macros, you may bootstrap it by expanding the expander itself
;;; first on an R5RS system.
;;; Here is how to do it:
;;;
;;;   (load "compat-mzscheme.scm")   ; for example bootstrapping from mzscheme 
;;;   (load "runtime.scm")
;;;   (load "expander.scm")
;;;   (ex:expand-file "standard-libraries.scm" "standard-libraries.exp")
;;;   (ex:expand-r5rs-file "expander.scm" "expander.exp" (ex:environment '(rnrs base)))
;;; 
;;; The expanded (.exp) files are vanilla Scheme and can then be run on the target
;;; system as follows:
;;;
;;;   (load "compat-chez.scm")       ; for example
;;;   (load "runtime.scm")
;;;   (load "standard-libraries.exp")
;;;   (load "expander.exp")
;;;
;;; SIZE OF OBJECT CODE:
;;; --------------------
;;;
;;; The minimal runtime prerequisites has been separated into a small
;;; include file runtime.scm, which is all that needs to be present for
;;; executing an expanded program that does not contain runtime
;;; uses the exports of (rnrs syntax-case) or (rnrs eval).
;;; See examples.scm for demonstrations of this.
;;;
;;; Expanded libraries may contain identifier environment information
;;; and visit code that could adversely affect the runtime binary size.
;;; This is not a big problem, for several reasons:
;;; First, note that this information is only present in libraries that
;;; define macros.
;;; Second, the size of the environments saved in the object code can
;;; usually be reduced dramatically by using 'only' imports.
;;; Third, the environments, as well as the visit code, can be discarded
;;; completely from the runtime image of a fully expanded program not
;;; using (rnrs syntax-case) or (rnrs eval) at runtime.  It is very
;;; easy to write a little build script that does this.
;;;
;;; The only reason for including this information now in the object code
;;; of a library is to support separate compilation, so one can expand a
;;; library in one session and use macros from the /expanded/ library to
;;; expand another library or program in a new session.  The customization
;;; to get rid of separate compilation, if desired, would be trivial.

;;=================================================================================
;;
;; IMPORTS:
;;
;;=================================================================================
;;
;; The include file runtime.scm has to be loaded before loading this expander
;;
;;=================================================================================
;;
;; EXPORTS:
;;
;;=================================================================================

;; Direct exports:

(define ex:make-variable-transformer #f)
(define ex:identifier?               #f)
(define ex:bound-identifier=?        #f)
(define ex:free-identifier=?         #f)
(define ex:generate-temporaries      #f)
(define ex:datum->syntax             #f)
(define ex:syntax->datum             #f)
(define ex:environment               #f)
(define ex:environment-bindings      #f)
(define ex:eval                      #f)
(define ex:load                      #f)
(define ex:syntax-violation          #f)

;; System exports:

(define ex:expand-file               #f)
(define ex:repl                      #f)
(define ex:expand-r5rs-file          #f)
(define ex:run-r6rs-sequence         #f)
(define ex:run-r6rs-program          #f)

;; Indirect exports:

(define ex:invalid-form              #f)
(define ex:register-macro!           #f)
(define ex:syntax-rename             #f)
(define ex:map-while                 #f)
(define ex:dotted-length             #f)
(define ex:dotted-butlast            #f)
(define ex:dotted-last               #f)
(define ex:uncompress                #f)
(define ex:free=?                    #f)

(letrec-syntax
    ;; Not everyone has the same parameter API:

    ((fluid-let
      (syntax-rules ()
        ((fluid-let () be ...)
         (begin be ...))
        ((fluid-let ((p0 e0) (p e) ...) be ...)
         (let ((saved p0))
           (set! p0 e0)
           (call-with-values (lambda ()
                               (fluid-let ((p e) ...) be ...))
             (lambda results
               (set! p0 saved)
               (apply values results)))))))

     ;; A trivial but extremely useful s-expression matcher.
     ;; Implements a subset of Wright's matcher's patterns.
     ;; Includes additional (syntax id) pattern that matches
     ;; if input is identifier? and free=? to 'id.

     (match
      (syntax-rules ()
        ((match (op arg ...) clause ...)
         (let ((x (op arg ...)))
           (match x clause ...)))
        ((match x)
         (ex:invalid-form x))
        ((match x (pat e ...) clause ...)
         (matcher "base" pat "done" x (e ...) (lambda () (match x clause ...))))))

     (matcher
      (syntax-rules (- ___ ? syntax)
        ((matcher "base" () k arg ...)
         (matcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg ...))
        ((matcher "base" - k arg ...)
         (matcher k (lambda (x sk fk) (sk)) () arg ...))
        ((matcher "base" (syntax id) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (ex:free=? x 'id) (sk) (fk)))
                  ()
                  arg ...))
        ((matcher "base" (? pred? p) k arg ...)
         (matcher "base" p "predicate" pred? k arg ...))
        ((matcher "predicate" code vars pred? k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (pred? x)
                        (code x sk fk)
                        (fk)))
                  vars
                  arg ...))
        ((matcher "base" (p1 ___ tailp ...) k arg ...)
         (matcher "base" p1 "ellipses" (tailp ...) k arg ...))
        ((matcher "ellipses" code vars (tailp ...) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (let loop ((x x)
                               (result '()))
                      (define (match-tail)
                        (match x
                          ((tailp ...)
                           (apply sk (if (null? result)
                                         (map (lambda (ignore) '()) 'vars)
                                         (apply map list (reverse result)))))
                          (- (fk))))
                      (cond ((null? x) (match-tail))
                            ((pair? x)
                             (code (car x)
                                   (lambda car-vars
                                     (loop (cdr x) (cons car-vars result)))
                                   match-tail))
                            (else (fk)))))
                  vars
                  arg ...))
        ((matcher "base" (p1 . p2) k arg ...)
         (matcher "base" p1 "pair" p2 k arg ...))
        ((matcher "pair" car-code car-vars p2 k arg ...)
         (matcher "base" p2 "pair-done" car-code car-vars k arg ...))
        ((matcher "pair-done" cdr-code (cdr-var ...) car-code (car-var ...) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (pair? x)
                        (car-code (car x)
                                  (lambda (car-var ...)
                                    (cdr-code (cdr x)
                                              (lambda (cdr-var ...)
                                                (sk car-var ... cdr-var ...))
                                              fk))
                                  fk)
                        (fk)))
                  (car-var ... cdr-var ...)
                  arg ...))
        ((matcher "base" #(p ___) k arg ...)
         (matcher "base" (p ___) "vector" k arg ...))
        ((matcher "vector" list-code vars k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (vector? x)
                        (list-code (vector->list x)
                                   sk
                                   fk)
                        (fk)))
                  vars
                  arg ...))
        ((matcher "base" id k arg ...)
         (matcher k (lambda (x sk fk) (sk x)) (id) arg ...))
        ((matcher "done" code vars x (e ...) fk)
         (code x (lambda vars e ...) fk)))))

  (let* (;;==========================================================================
         ;;
         ;; Dynamic parameters:
         ;;
         ;;==========================================================================

         ;; toplevel REPL bindings to be initialized later
         (*toplevel-env*     #f)
         ;; current lexical environment to be initialized later
         (*usage-env*        #f)
         ;; current phase
         (*phase*            0)
         ;; current color for painting identifiers upon renaming to be initialized
         (*color*            #f)
         ;; global table mapping <binding name> of keyword to <macro> object
         (*macro-table*      '())
         ;; maps <symbolic key> of reflected environment to actual <environment>
         (*env-table*        '())
         ;; current library name as list of symbols or '() for toplevel
         (*current-library*  '())
         ;; car of this records bindings already referenced in current body
         ;; for detecting when later definitions may violate lexical scope
         (*used*             (list '()))
         ;; history trace for error reporting
         (*trace*            '())
         ;; whether expanded library introduces identifiers via syntax
         ;; expressions - if not, save lots of space by not including
         ;; env-table in object code
         (*syntax-reflected* #f)
         ;; what counts as an ellipsis; see standard-ellipsis below    ; [R7RS]
         (*ellipsis* #f)                                               ; [R7RS]

         ;;==========================================================================
         ;;
         ;; Identifiers:
         ;;
         ;;==========================================================================

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
         ;;   <transformer-envs> : List of reflected transformer environments.
         ;;                        The environment (env-reify (car <transformer-envs>)) was the
         ;;                        usage environment valid during expansion of any (syntax id)
         ;;                        expression whose evaluation introduced this identifier, while
         ;;                        (cdr <transformer-envs>) are in turn the reflected
         ;;                        <transformer-envs> of the original id.
         ;;   <displacement>     : Integer that keeps track of shifts in phases
         ;;                        between transformer and usage sites of identifier.
         ;;   <maybe-library>    : Library name if identifier was introduced by evaluation of
         ;;                        a (syntax ...) expression, otherwise #f.
         ;;                        The empty name '() is used for toplevel.

         (:identifier
          (make-record-type-descriptor 'identifier #f #f #f #f
                                       '#((immutable name)
                                          (immutable colors)
                                          (immutable transformer-envs)
                                          (immutable displacement)
                                          (immutable maybe-library))))
         (make-identifier
          (record-constructor (make-record-constructor-descriptor :identifier #f #f))))

    ;; We sequenced stuff in the let* above because r5rs internal
    ;; definitions use letrec semantics and cannot be used for sequencing.

    (define identifier?         (record-predicate :identifier))
    (define id-name             (record-accessor :identifier 0))
    (define id-colors           (record-accessor :identifier 1))
    (define id-transformer-envs (record-accessor :identifier 2))
    (define id-displacement     (record-accessor :identifier 3))
    (define id-maybe-library    (record-accessor :identifier 4))

    (define standard-ellipsis (make-identifier '... '() '() 0 #f))     ; [R7RS]

    (define (id-library id)
      (or (id-maybe-library id)
          *current-library*))

    (define (bound-identifier=? x y)
      (check x identifier? 'bound-identifier=?)
      (check y identifier? 'bound-identifier=?)
      (and (eq? (id-name x)
                (id-name y))
           (equal? (id-colors x)
                   (id-colors y))))

    ;; As required by r6rs, when this returns, the result is #t
    ;; if and only if the two identifiers resolve to the same binding.
    ;; It also treats unbound identifiers specially.
    ;; As allowed by R6RS, included phase checking of arguments.
    ;; An out of phase error is raised if the comparison succeeds but
    ;; either argument is out of phase.  This is sufficient to ensure
    ;; that literals such as ... in syntax-case are used in the correct phase.
    ;; For more dicussion on this choice, see the readme and the examples file.

    (define (free-identifier=? x y)
      (check x identifier? 'free-identifier=?)
      (check y identifier? 'free-identifier=?)
      (let  ((bx (binding x))
             (by (binding y)))
        (let ((result (if bx
                          (and by
                               (eq? (binding-name bx)
                                    (binding-name by)))
                          (and (not by)
                               (eq? (id-name x)
                                    (id-name y))))))
          (and result
               bx
               (begin (check-binding-level x bx)
                      (check-binding-level y by)))
          ;; A later definition in the same body can only change
          ;; #t to #f, so only record usage in that case.
          (and result
               (register-use! x bx)
               (register-use! y by))
          result)))

    ;; For internal use

    (define (free=? x symbol)
      (and (identifier? x)
           (let  ((bx (binding x)))
             (let ((result
                    (and bx
                         (eq? (binding-name bx) symbol))))
               (and result
                    bx
                    (check-binding-level x bx))
               (and result
                    (register-use! x bx))
               result))))

    ;;==========================================================================
    ;;
    ;; Infrastructure for generated names:
    ;;
    ;;==========================================================================

    ;; Generate-guid returns a fresh symbol that has a globally
    ;; unique external representation and is read-write invariant.
    ;; Your local gensym will probably not satisfy both conditions.
    ;; Prefix makes it disjoint from all builtins.
    ;; Uniqueness is important for incremental and separate expansion.

    (define generate-guid
      (let ((token (ex:unique-token))
            (ticks 0))
        (lambda (symbol)
          (set! ticks (+ ticks 1))
          (string->symbol
           (string-append ex:guid-prefix
                          (symbol->string symbol)
                          "~"
                          token
                          "~"
                          (number->string ticks))))))

    ;; Used to generate user program toplevel names.
    ;; Prefix makes it disjoint from all builtins.
    ;; Prefix makes it disjoint from output of generate-guid.
    ;; Must be read-write invariant.

    (define (make-free-name symbol)
      (string->symbol (string-append ex:free-prefix (symbol->string symbol))))

    ;;=========================================================================
    ;;
    ;; Colors to paint identifiers with:
    ;;
    ;;=========================================================================

    ;; Returns <color> ::= globally unique symbol

    (define (generate-color)
      (generate-guid 'c))

    ;;=========================================================================
    ;;
    ;; Bindings:
    ;;
    ;;=========================================================================

    ;; <binding> ::= (variable         <binding-name> (<level> ...) <mutable?>  <library-name>)
    ;;            |  (macro            <binding-name> (<level> ...) #f          <library-name>)
    ;;            |  (pattern-variable <binding-name> (<level> ...) <dimension> <library-name>)
    ;;            |  #f  (out of context binding from another library)
    ;; <mutable> ::= #t | #f
    ;; <dimension> ::= 0 | 1 | 2 | ...
    ;; <binding-name> ::= <symbol> uniquely identifying binding.
    ;; <binding-name> is used for free-identifier=? comparison.
    ;; For variable and pattern variable bindings, it is the same
    ;; as the symbol emitted for the binding in the object code.
    ;; For macro bindings, it is the key for looking up the transformer
    ;; in the global macro table.

    (define (make-binding type name levels content library)
      (list type name levels content library))

    (define (binding-type b)           (car b))
    (define (binding-name b)           (cadr b))
    (define (binding-levels b)         (caddr b))
    (define (binding-mutable? b)       (cadddr b))
    (define (binding-dimension b)      (cadddr b))
    (define (binding-library b)        (car (cddddr b)))
    (define (binding-mutable-set! b x) (set-car! (cdddr b) x))

    ;; Looks up binding first in usage environment and
    ;; then in attached transformer environments.
    ;; Toplevel forward references are treated specially.
    ;; Returns <binding> | #f if unbound.

    (define (binding id)
      (let ((name (id-name id)))
        (let loop ((env    *usage-env*)
                   (envs   (id-transformer-envs id))
                   (colors (id-colors id)))
          (or (env-lookup (cons name colors) env)
              (and (pair? envs)
                   (loop (env-reify (car envs))
                         (cdr envs)
                         (cdr colors)))))))

    ;;=========================================================================
    ;;
    ;; Mapping in environment: ((<name> <color> ...) . <binding>)
    ;;
    ;;=========================================================================

    ;; Generates a local mapping at the current meta-level
    ;; that can be added to the usage environment.

    (define (make-local-mapping type id content)
      (cons (cons (id-name id)
                  (id-colors id))
            (make-binding type
                          (generate-guid (id-name id))
                          (list (source-level id))
                          content
                          *current-library*)))

    ;; Toplevel binding forms use as binding name the free name
    ;; so that source-level forward references will work in REPL.
    ;; If identifier is macro-generated, bind it with a fresh name.
    ;; This ensures that generated toplevel defines are not visible
    ;; from toplevel source code, thus approximating the behaviour
    ;; of generated internal definitions.

    (define (make-toplevel-mapping type id content)
      (if (null? (id-colors id))
          (cons (cons (id-name id)
                      (id-colors id))
                (make-binding type
                              (make-free-name (id-name id))
                              '(0)
                              content
                              *current-library*))
          (make-local-mapping type id content)))

    ;;=========================================================================
    ;;
    ;; Infrastructure for binding levels:
    ;;
    ;;=========================================================================

    (define (source-level id)
      (- *phase* (id-displacement id)))

    (define (check-binding-level id binding)
      (if binding
          (or (memv (source-level id)
                    (binding-levels binding))
              #t ; FIXME: no for / meta / run / expand in R7RS         ; [R7RS]
              (syntax-violation
               "invalid reference"
               (string-append "Attempt to use binding of " (symbol->string (id-name id))
                              " in library (" (list->string (id-library id) " ")
                              ") at invalid level " (number->string (source-level id))
                              ".  Binding is only available at levels: "
                              (list->string (binding-levels binding) " "))
               id))
          (or (and (null? (id-library id))
                   (= *phase* 0))
              (syntax-violation
               "invalid reference"
               (string-append "No binding available for " (symbol->string (id-name id))
                              " in library (" (list->string (id-library id) " ") ")")

               id))))

    ;;=========================================================================
    ;;
    ;; Environments:
    ;;
    ;;=========================================================================

    ;; An environment is a list of frames.
    ;;
    ;;   <environment> ::= (<frame> ...)
    ;;   <frame>       ::= (list ((<key> . <value>) ...))
    ;;
    ;; Keys must be comparable with equal? and unique in each frame.
    ;; Frames can be added, or the leftmost frame can be destructively
    ;; updated in the case of binding constructs such as bodies where
    ;; definitions are incrementally discovered.

    (define (make-null-env) '())
    (define (make-unit-env) (env-extend '() (make-null-env)))

    ;; Adds a new frame containing mappings to env.

    (define (env-extend mappings env)
      (cons (list mappings) env))

    ;; Destructively extends the leftmost frame in env.

    (define (env-extend! mappings env)
      (let ((frame (car env)))
        (set-car! frame (append mappings (car frame)))))

    ;; Returns <object> | #f

    (define (env-lookup key env)
      (and (pair? env)
           (or (let ((probe (assoc key (caar env))))
                 (and probe
                      (or (cdr probe)
                          (syntax-violation
                           #f "Out of context reference to identifier" (car key)))))
               (env-lookup key (cdr env)))))

    ;; Is id already bound in leftmost frame?

    (define (duplicate? id env)
      (assoc (cons (id-name id)
                   (id-colors id))
             (caar env)))

    ;; Returns a single-symbol <key> representing an
    ;; environment that can be included in object code.

    (define (env-reflect env)
      (cond ((and (not (null? *env-table*))      ; +++
                  (eq? env (cdar *env-table*)))  ; +++
             (caar *env-table*))                 ; +++
            (else
             (let ((key (generate-guid 'env)))
               (set! *env-table*
                     (cons (cons key env)
                           *env-table*))
               key))))

    ;; The inverse of the above.

    (define (env-reify key-or-env)
      (if (symbol? key-or-env)
          (cdr (assq key-or-env *env-table*))
          key-or-env))

    ;; This makes a much smaller external representation of an
    ;; environment table by factoring shared structure.

    ;; Pattern variable bindings can never be
    ;; used in client, so don't waste space.
    ;; Should really do the same with all local
    ;; bindings, but there are usually much less
    ;; of them, so don't bother for now.
    ;;
    ;; Suppressed pattern variable bindings aren't saved in compiled files,
    ;; so those bindings won't be present when a compiled file is loaded,
    ;; so missing bindings must be special-cased below.  See bug #740.

    (define (compress env-table)
      (let ((frame-table '())
            (count 0))
        (for-each (lambda (entry)
                    (for-each (lambda (frame)
                                (if (not (assq frame frame-table))
                                    (begin
                                      (set! frame-table
                                            (cons (cons frame count)
                                                  frame-table))
                                      (set! count (+ 1 count)))))
                              (cdr entry)))
                  env-table)
        (cons (map (lambda (env-entry)
                     (cons (car env-entry)
                           (map (lambda (frame)
                                  (cdr (assq frame frame-table)))
                                (cdr env-entry))))
                   env-table)
              (map (lambda (frame-entry)
                     (cons (cdr frame-entry)
                           (list (map (lambda (mapping)
                                        (cons (car mapping)
                                              (let ((binding
                                                     ;; see bug #740
                                                     (or (cdr mapping)
                                                         '(pattern-variable))))
                                                (case (binding-type binding)
                                                  ((pattern-variable) #f) ; +++
                                                  (else binding)))))
                                      (caar frame-entry)))))
                   frame-table))))

    (define (uncompress compressed-env-table)
      (map (lambda (env-entry)
             (cons (car env-entry)
                   (map (lambda (frame-abbrev)
                          (cdr (assv frame-abbrev (cdr compressed-env-table))))
                        (cdr env-entry))))
           (car compressed-env-table)))

    ;;=========================================================================
    ;;
    ;; Syntax-reflect and syntax-rename:
    ;;
    ;; This is the basic building block of the implicit renaming mechanism for
    ;; maintaining hygiene.  Syntax-reflect generates the expanded code for
    ;; (syntax id), including the expand-time environment in the
    ;; external representation.  It expands to syntax-rename, which performs
    ;; the implicit renaming when this expanded code is eventually run.
    ;; The displacement computations calculate the difference between the
    ;; usage phase and the transformer phase.
    ;;
    ;;=========================================================================

    (define (syntax-reflect id)
      (set! *syntax-reflected* #t)
      `(ex:syntax-rename ',(id-name id)
                         ',(id-colors id)
                         ',(cons (env-reflect *usage-env*)
                                 (id-transformer-envs id))
                         ,(- (- *phase* (id-displacement id)) 1)
                         ',(id-library id)))

    (define (syntax-rename name colors transformer-envs transformer-phase source-library)
      (make-identifier name
                       (cons *color* colors)
                       transformer-envs
                       (- *phase* transformer-phase)
                       source-library))

    ;;=====================================================================
    ;;
    ;; Capture and sexp <-> syntax conversions:
    ;;
    ;;=====================================================================

    (define (datum->syntax tid datum)
      (check tid identifier? 'datum->syntax)
      (sexp-map (lambda (leaf)
                  (cond ((symbol? leaf)
                         (make-identifier leaf
                                          (id-colors tid)
                                          (id-transformer-envs tid)
                                          (id-displacement tid)
                                          (id-maybe-library tid)))
                        (else leaf)))
                datum))

    (define (syntax->datum exp)
      (sexp-map (lambda (leaf)
                  (cond ((identifier? leaf) (id-name leaf))
                        ((symbol? leaf)
                         (assertion-violation 'syntax->datum "A symbol is not a valid syntax object" leaf))
                        (else leaf)))
                exp))

    ;; Fresh identifiers:

    (define (generate-temporaries ls)
      (check ls list? 'generate-temporaries)
      (map (lambda (ignore)
             (make-identifier 'temp
                              (list (generate-color))
                              (list (make-null-env))
                              *phase*
                              #f))
           ls))

    ;; For use internally as in the explicit renaming system.

    (define (rename type symbol)
      (make-identifier symbol
                       (list *color*)
                       (list (env-extend
                              (list (cons (cons symbol '())
                                          (make-binding type symbol '(0) #f '())))
                              (make-null-env)))
                       *phase*
                       #f))

    ;;=========================================================================
    ;;
    ;; Macro objects:
    ;;
    ;;=========================================================================

    ;; Expanders are system macros that fully expand
    ;; their arguments to core Scheme, while
    ;; transformers and variable transformers are
    ;; user macros.

    ;; <type> ::= expander | transformer | variable-transformer

    (define (make-macro type proc)
      (list type proc))
    (define macro-type car)
    (define macro-proc cadr)

    (define (make-expander proc)             (make-macro 'expander proc))
    (define (make-transformer proc)          (make-macro 'transformer proc))
    (define (make-variable-transformer proc) (make-macro 'variable-transformer proc))

    (define (make-user-macro procedure-or-macro)
      (if (procedure? procedure-or-macro)
          (make-transformer procedure-or-macro)
          procedure-or-macro))

    ;; Returns <macro>.

    (define (binding->macro binding t)
      (cond ((assq (binding-name binding) *macro-table*) => cdr)
            (else
             (syntax-violation
              #f "Undefined variable or reference to macro keyword out of context" t))))

    ;; Registering macro.

    (define (register-macro! binding-name procedure-or-macro)
      (set! *macro-table* (cons (cons binding-name (make-user-macro procedure-or-macro))
                                *macro-table*)))

    ;; Calls a macro with a new color.

    (define (invoke-macro macro t)
      (set! *color* (generate-color))
      ((macro-proc macro) t))

    ;;=========================================================================
    ;;
    ;; Expander dispatch:
    ;;
    ;;=========================================================================

    (define (expand t)
      (fluid-let ((*trace* (cons t *trace*)))
        (let ((binding (operator-binding t)))
          (cond (binding (case (binding-type binding)
                           ((macro)
                            (let ((macro (binding->macro binding t)))
                              (let ((expanded-once (invoke-macro macro t)))
                                (case (macro-type macro)
                                  ((expander) expanded-once)
                                  (else
                                   (expand expanded-once))))))
                           ((variable)
                            (check-implicit-import-of-mutable binding t)
                            (if (list? t)
                                (cons (binding-name binding)
                                      (map expand (cdr t)))
                                (binding-name binding)))
                           ((pattern-variable)
                            (syntax-violation #f "Pattern variable used outside syntax template" t))))
                ((list? t)       (map expand t))
                ((identifier? t) (make-free-name (id-name t)))
                ((pair? t)       (syntax-violation #f "Invalid procedure call syntax" t))
                ((symbol? t)     (syntax-violation #f "Symbol may not appear in syntax object" t))
                ((vector? t)                                           ; [R7RS]
                 (syntax->datum t))                                    ; [R7RS]
                (else t)))))

    ;; Only expands while t is a user macro invocation.
    ;; Used by expand-lambda to detect internal definitions.
    ;; Also used by scan-sequence.

    (define (head-expand t)
      (fluid-let ((*trace* (cons t *trace*)))
        (let ((binding (operator-binding t)))
          (cond (binding (case (binding-type binding)
                           ((macro)
                            (let ((macro (binding->macro binding t)))
                              (case (macro-type macro)
                                ((expander) (values t binding))
                                (else
                                 (head-expand (invoke-macro macro t))))))
                           (else (values t binding))))
                (else (values t binding))))))

    ;; Returns binding of identifier in operator position | #f if none.
    ;; Singleton identifiers are also considered operators here for
    ;; the purpose of discovering identifier macros and variables.
    ;; Checks level and registers as a use.

    (define (operator-binding t)
      (let ((operator (if (pair? t) (car t) t)))
        (and (identifier? operator)
             (let ((binding (binding operator)))
               (check-binding-level operator binding)
               (register-use! operator binding)
               binding))))

    ;; We cannot implicitly import a mutable variable.

    (define (check-implicit-import-of-mutable binding t)
      (or (equal? (binding-library binding) *current-library*)
          (not (binding-mutable? binding))
          (syntax-violation
           #f
           (string-append "Attempt to implicitly import variable that is mutable in library ("
                          (list->string (binding-library binding) " ") ")")
           t)))

    ;;=========================================================================
    ;;
    ;; Quote, if, set!, expression begin, expression let[rec]-syntax, and, or:
    ;;
    ;;=========================================================================

    (define (expand-quote exp)
      (match exp
        ((- datum) (syntax->datum exp))))

    (define (expand-if exp)
      (match exp
        ((- e1 e2 e3) `(if ,(expand e1) ,(expand e2) ,(expand e3)))
        ((- e1 e2)    `(if ,(expand e1) ,(expand e2)))))

    (define (expand-set! exp)
      (match exp
        ((- (? identifier? id) e)
         (let ((binding (binding id)))
           (if (not binding)
               (syntax-violation
                'set!
                "Assignment to undefined variable" exp id))
           (check-binding-level id binding)
           (register-use! id binding)
           (case (binding-type binding)
             ((macro)
              (let ((macro (binding->macro binding id)))
                (case (macro-type macro)
                  ((variable-transformer)
                   (expand (invoke-macro macro exp)))
                  (else
                   (syntax-violation
                    'set! "Keyword being set! is not a variable transformer" exp id)))))
             ((variable)
              (or (eq? (binding-library binding) *current-library*)
                  (syntax-violation
                   'set! "Directly or indirectly imported variable cannot be assigned" exp id))
              (binding-mutable-set! binding #t)
              `(set! ,(binding-name binding)
                     ,(expand e)))
             ((pattern-variable)
              (syntax-violation 'set! "Pattern variable used outside syntax template" exp id)))))))

    ;; Expression begin.
    ;; Begin forms created by R7RS include forms are recognized        ; [R7RS]
    ;; here so nested include forms will work properly.
    ;; See lib/R7RS/includer.sch and keep this in sync with that.

    (define (expand-begin exp)
      (match exp
#;                                    ; FIXME
        ((- (? char? c) forms ___)             ; start of new code for ; [R7RS]
         (if (and (char=? c #\I)
                  (larceny:included-forms? `(begin ,@(cdr exp))))
             (call-with-values
              larceny:source-path-of-included-forms
              (lambda (dirname basename)
                (let* ((paths (current-require-path))
                       (paths (if (member dirname paths)
                                  paths
                                  (cons dirname paths)))
                       (forms (larceny:included-forms-as-list form)))
                  (parameterize ((current-require-path paths))
                   (pretty-print (list 'expand-begin exp forms paths))
                   (expand-begin `(begin ,@forms))))))
             (expand-begin `(begin ,@forms))))   ; end of new code for ; [R7RS]

        ((- exps ___)
         (scan-sequence 'expression-sequence
                        #f
                        exps
                        '()                                            ; [R7RS]
                        '()                                            ; [R7RS]
                        (lambda (forms
                                 no-syntax-definitions
                                 no-bound-variables
                                 no-exports                            ; [R7RS]
                                 no-imports)                           ; [R7RS]
                          `(begin ,@(map cdr forms)))))))

    ;; Expression let(rec)-syntax:

    (define (expand-local-syntax exp)
      (expand-begin `(,(rename 'macro 'begin) ,exp)))

    ;; Define and and or as primitives  so we can import them into the repl
    ;; toplevel without spoiling the and and or of the library language.

    (define (expand-and exp)
      (match exp
        ((and) #t)
        ((and e) (expand e))
        ((and e es ___)
         `(if ,(expand e)
              ,(expand `(,and ,@es))
              #f))))

    (define (expand-or exp)
      (match exp
        ((or) #f)
        ((or e) (expand e))
        ((or e es ___)
         `(let ((x ,(expand e)))
            (if x x ,(expand `(,or ,@es)))))))

    ;;=========================================================================
    ;;
    ;; Cond-expand:
    ;;
    ;;=========================================================================

    (define (expand-cond-expand body-type type form)
      (check-cond-expand body-type type form)
      (let ((form2 (cons (car form)
                         (map (lambda (clause)
                                (cons (syntax->datum (car clause))
                                      (cdr clause)))
                              (cdr form)))))
        (larceny:cond-expand form2)))

    (define (cond-expand-expander form)
      (let ((exps (expand-cond-expand 'expression-sequence 'expression form)))
        (expand-begin `(,(rename 'macro 'begin) ,@exps))))

    ;;=========================================================================
    ;;
    ;; Lambda:
    ;;
    ;;=========================================================================

    (define (expand-lambda exp)
      (match exp
        ((- (? formals? formals) body ___)
         (fluid-let ((*usage-env*
                      (env-extend (map (lambda (formal)
                                         (make-local-mapping 'variable formal #f))
                                       (flatten formals))
                                  *usage-env*)))
           (let ((formals (dotted-map (lambda (formal) (binding-name (binding formal))) formals)))
             ;; Scan-sequence expects the caller to have prepared
             ;; the frame to which to destructively add bindings.
             ;; Lambda bodies need a fresh frame.
             (fluid-let ((*usage-env* (env-extend '() *usage-env*)))
               (scan-sequence 'lambda
                              make-local-mapping
                              body
                              '()                                      ; [R7RS]
                              '()                                      ; [R7RS]
                              (lambda (forms
                                       syntax-definitions
                                       bound-variables
                                       no-exports                      ; [R7RS]
                                       no-imports)                     ; [R7RS]
                                `(lambda ,formals
                                   ,@(if (null? bound-variables)                ; +++
                                         (emit-body forms ex:undefined-set!)    ; +++
                                         `(((lambda ,bound-variables
                                              ,@(emit-body forms ex:undefined-set!))
                                            ,@(map (lambda (ignore) `ex:undefined)
                                                   bound-variables)))))))))))))

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
    ;;
    ;; Bodies and sequences:
    ;;
    ;;=========================================================================

    ;; R6RS splicing of internal let-syntax and letrec-syntax
    ;; requires that we remember the bindings visible in each
    ;; form for later expansion of deferred right hand sides
    ;; and expressions.  This is done by attaching
    ;; the environment to the expression.
    ;; We call the resulting data structure a wrap.
    ;; Wraps are only used internally in processing of bodies,
    ;; and are never seen by user macros.

    (define (make-wrap env exp)
      (cons env exp))
    (define wrap-env car)
    (define wrap-exp cdr)

    ;; The continuation k is evaluated in the body environment.  This is
    ;; used for example by expand-library to obtain the correct bindings of
    ;; exported identifiers.
    ;;
    ;; <body-type> ::= toplevel | library | program | lambda | expression-sequence
    ;;               | define-library                                  ; [R7RS]
    ;;
    ;; All but TOPLEVEL are as in r6rs or r7rs.                        ; [R7RS]
    ;; TOPLEVEL is meant for the REPL.
    ;; At TOPLEVEL, we may have a sequence of expressions, definitions, macros,
    ;; import declarations, libraries and programs wrapped in (program ---).
    ;; Redefinitions are allowed at toplevel.
    ;;
    ;; R7RS define-library allows export declarations to appear        ; [R7RS]
    ;; at top level anywhere within the define-library form, so
    ;; exports was added as a fourth argument to both scan-sequence
    ;; and to its continuation k.
    ;;
    ;; R7RS define-library also allows import declarations to appear   ; [R7RS]
    ;; at top level anywhere within the define-library form, so
    ;; imports was added as a fifth argument to both scan-sequence
    ;; and to its continuation k.

    (define (scan-sequence body-type
                           make-map
                           body-forms
                           exports
                           imported-libraries
                           k)

      ;; Each <form> ::= (<symbol | #f> #t <wrap>)   (deferred rhs)
      ;;              |  (<symbol | #f> #f <s-expr>) (undeferred rhs)
      ;; Returns ((<symbol | #f> . <s-expr>) ...)

      (define (expand-deferred forms)
        (map (lambda (form)
               (cons (car form)
                     (let ((deferred? (cadr form))
                           (exp       (caddr form)))
                       (if deferred?
                           (fluid-let ((*usage-env* (wrap-env exp)))
                             (expand (wrap-exp exp)))
                           exp))))
             forms))
      (let ((common-env *usage-env*))

        ;; For the R7RS, this loop needs to be slightly recursive.     ; [R7RS]
        ;; It takes a list of wrapped body forms, and returns
        ;; five values: the accumulated forms, syntax-defs,
        ;; bound-variables, exports, and imported libraries,
        ;; all in reverse order.

        ;; The R7RS define-library syntax does not allow definitions   ; [R7RS]
        ;; or expressions (other than begin) outside of a begin.  To
        ;; enforce that restriction, the loop maintains a flag
        ;; indicating whether definitions/expressions are allowed.

        (define (loop ws                                               ; [R7RS]
                      forms syntax-defs bound-variables
                      exports imported-libraries
                      defs-okay?)
          (cond
           ((null? ws)
            (values forms                                              ; [R7RS]
                    syntax-defs                                        ; [R7RS]
                    bound-variables                                    ; [R7RS]
                    exports                                            ; [R7RS]
                    imported-libraries))                               ; [R7RS]
           (else
            (fluid-let ((*usage-env* (wrap-env (car ws))))
              (call-with-values
                  (lambda () (head-expand (wrap-exp (car ws))))
                (lambda (form operator-binding)
                  (let ((type (and operator-binding
                                   (binding-name operator-binding))))
                    (check-expression-sequence body-type type form)
                    (check-toplevel            body-type type form)
                    (check-r7rs-library        body-type type form defs-okay?)

                    (case type

                      ;; The next few cases are new for the R7RS.      ; [R7RS]

                      ((include include-ci)
                       (match form
                        ((keyword filenames ___)
                         (let* ((includer (if (eq? type 'include)
                                              larceny:include
                                              larceny:include-ci))
                                (form (includer filenames))
                                (form (datum->syntax keyword form))
                                (wrap (make-wrap *usage-env* form)))
                           (call-with-values
                            (lambda ()
                              (loop (list wrap)
                                    forms
                                    syntax-defs
                                    bound-variables
                                    exports
                                    imported-libraries
                                    defs-okay?))
                            (lambda (forms
                                     syntax-defs
                                     bound-variables
                                     exports
                                     imported-libraries)
                              (loop (cdr ws)
                                    forms
                                    syntax-defs
                                    bound-variables
                                    exports
                                    imported-libraries
                                    defs-okay?)))))))
                      ((include-library-declarations)
                       (match form
                        ((keyword filenames ___)
                         (let* ((decls-as-begin
                                 (larceny:include-library-declarations
                                  filenames))
                                (decls-as-list (cdr decls-as-begin))
                                (decls (datum->syntax keyword decls-as-list))
                                (wraps (map (lambda (decl)
                                              (make-wrap *usage-env* decl))
                                            decls)))
                           (call-with-values
                            (lambda ()
                              (loop wraps
                                    forms
                                    syntax-defs
                                    bound-variables
                                    exports
                                    imported-libraries
                                    defs-okay?))
                            (lambda (forms
                                     syntax-defs
                                     bound-variables
                                     exports
                                     imported-libraries)
                              (loop (cdr ws)
                                    forms
                                    syntax-defs
                                    bound-variables
                                    exports
                                    imported-libraries
                                    defs-okay?)))))))
                      ((cond-expand)
                       (let* ((decls (expand-cond-expand body-type type form))
                              (wraps (map (lambda (decl)
                                            (make-wrap *usage-env* decl))
                                          decls)))
                         (loop (append wraps (cdr ws))
                               forms
                               syntax-defs
                               bound-variables
                               exports
                               imported-libraries
                               defs-okay?)))
                      ((export)
                       (match form
                        ((- sets ___)
                         (let* ((new-exports (scan-exports sets body-type))
                                (new-exports
                                 (filter-exports new-exports exports))
                                (exports (append new-exports exports)))
                           (loop (cdr ws)
                                 forms
                                 syntax-defs
                                 bound-variables
                                 exports
                                 imported-libraries
                                 defs-okay?)))))

                      ;; The begin case was rewritten for R7RS.        ; [R7RS]

                      ((begin)
                       (or (list? form)
                           (invalid-form form))

                       (let ()

                         ;; Returns five values to which the continue
                         ;; procedure can be applied.

                         (define (process-begin-body form defs-okay?)
                           (loop (map (lambda (exp)
                                        (make-wrap *usage-env* exp))
                                      (cdr form))
                                 forms
                                 syntax-defs
                                 bound-variables
                                 exports
                                 imported-libraries
                                 defs-okay?))

                         ;; Given the values returned by process-begin-body,
                         ;; continues the loop.

                         (define (continue forms
                                           syntax-defs
                                           bound-variables
                                           exports
                                           imported-libraries)
                            (loop (cdr ws)
                                  forms
                                  syntax-defs
                                  bound-variables
                                  exports
                                  imported-libraries
                                  defs-okay?))

                         ;; If it was an ordinary begin (not a begin
                         ;; produced by include-library-declarations),
                         ;; then definitions and expressions are okay.

                         (define (usual-action)
                           (call-with-values
                            (lambda () (process-begin-body form #t))
                            continue))

                         (define (parameterized-action paths form)
                           (call-with-values
                            (lambda ()
                             (parameterize ((current-require-path paths))
                              (process-begin-body form defs-okay?)))
                            continue))

                         (define (extend-search-path)
                           (call-with-values
                            (lambda ()
                              (larceny:source-path-of-included-forms form))
                            (lambda (dirname basename)
                              (let* ((paths (current-require-path))
                                     (paths (if (member dirname paths)
                                                paths
                                                (cons dirname paths)))
                                     (begin-forms
                                      (larceny:included-forms-as-list form))
                                     (form
                                      `(,(car form) ,@begin-forms)))
                                (parameterized-action paths form)))))

                         (match form
                          ((keyword (? char? c) begin-forms ___)
                           (if (and (char=? c #\I)
                                    (larceny:included-forms?
                                     `(begin ,@(cdr form))))
                               (extend-search-path)
                               (usual-action)))
                          (- (usual-action)))))

                      ;; The other cases required only minor changes   ; [R7RS]
                      ;; for R7RS.

                      ((import)
                       (match form
                         ((- specs ___)
                          (call-with-values
                           (lambda () (scan-imports specs))
                           (lambda (new-imported-libraries imports)
                             (import-libraries-for-expand
                              new-imported-libraries
                              (map not new-imported-libraries)
                              0)
                             (env-import! (car form) imports common-env)
                             (loop (cdr ws)
                                   (if (memq body-type                 ; [R7RS]
                                             '(program toplevel))
                                       (cons (list
                                              #f
                                              #f
                                              `(ex:import-libraries-for-run
                                                ',new-imported-libraries
                                                ',(current-builds
                                                   new-imported-libraries)
                                                0))
                                             forms)
                                       forms)
                                   syntax-defs
                                   bound-variables
                                   exports                             ; [R7RS]
                                   (union2 new-imported-libraries      ; [R7RS]
                                           imported-libraries)         ; [R7RS]
                                   defs-okay?))))))                    ; [R7RS]
                      ((program)
                       (loop (cdr ws)
                             (cons (list #f #f (expand-program form)) forms)
                             syntax-defs
                             bound-variables
                             exports                                   ; [R7RS]
                             imported-libraries                        ; [R7RS]
                             defs-okay?))                              ; [R7RS]
                      ((library)
                       (loop (cdr ws)
                             (cons (list #f #f (expand-library form)) forms)
                             syntax-defs
                             bound-variables
                             exports                                   ; [R7RS]
                             imported-libraries                        ; [R7RS]
                             defs-okay?))
                      ((define-library)                                ; [R7RS]
                       (loop (cdr ws)
                             (cons (list #f #f (expand-define-library form))
                                   forms)
                             syntax-defs
                             bound-variables
                             exports
                             imported-libraries
                             defs-okay?))
                      ((define)
                       (call-with-values
                        (lambda () (parse-definition form #f))
                        (lambda (id rhs)
                          (check-valid-definition id
                                                  common-env
                                                  body-type form forms type)
                          (env-extend! (list (make-map 'variable id #f))
                                       common-env)
                          (loop (cdr ws)
                                (cons (list (binding-name (binding id))
                                            #t
                                            (make-wrap *usage-env* rhs))
                                      forms)
                                syntax-defs
                                (cons (binding-name (binding id))
                                      bound-variables)
                                exports                                ; [R7RS]
                                imported-libraries                     ; [R7RS]
                                defs-okay?))))                         ; [R7RS]
                      ((define-syntax)
                       (call-with-values
                           (lambda () (parse-definition form #t))
                         (lambda (id rhs)
                           (check-valid-definition id
                                                   common-env
                                                   body-type form forms type)
                           (let ((mapping (make-map 'macro id #f)))
                             (env-extend! (list mapping) common-env)
                             (let ((rhs (fluid-let ((*phase* (+ 1 *phase*)))
                                          (expand rhs))))
                               (register-macro!
                                (binding-name (cdr mapping))
                                (make-user-macro
                                 (eval rhs (interaction-environment))))
                               (loop (cdr ws)
                                     forms
                                     (cons (cons (binding-name (binding id))
                                                 rhs)
                                           syntax-defs)
                                     bound-variables
                                     exports                           ; [R7RS]
                                     imported-libraries                ; [R7RS]
                                     defs-okay?))))))                  ; [R7RS]
                      ((let-syntax letrec-syntax)
                       (call-with-values
                        (lambda () (parse-local-syntax form))
                        (lambda (formals rhs body)
                          (let* ((original-env *usage-env*)
                                 (usage-diff
                                  (map (lambda (formal)
                                         (make-local-mapping 'macro formal #f))
                                       formals))
                                 (extended-env
                                  (env-extend usage-diff original-env))
                                 (rhs-expanded
                                  (fluid-let ((*phase* (+ 1 *phase*))
                                              (*usage-env*
                                               (case type
                                                 ((let-syntax)
                                                  original-env)
                                                 ((letrec-syntax)
                                                  extended-env))))
                                    (map expand rhs)))
                                 (macros
                                  (map (lambda (e)
                                         (eval e (interaction-environment)))
                                       rhs-expanded)))
                            (for-each (lambda (mapping macro)
                                        (register-macro!
                                         (binding-name (cdr mapping))
                                         (make-user-macro macro)))
                                      usage-diff
                                      macros)
                            (loop (append (map (lambda (form)
                                                 (make-wrap extended-env form))
                                               body)
                                          (cdr ws))
                                  forms
                                  syntax-defs
                                  bound-variables
                                  exports                             ; [R7RS]
                                  imported-libraries                  ; [R7RS]
                                  defs-okay?)))))                     ; [R7RS]
                      (else
                       (loop (cdr ws)
                             (cons (list #f #t (make-wrap *usage-env* form))
                                   forms)
                             syntax-defs
                             bound-variables
                             exports                                   ; [R7RS]
                             imported-libraries                        ; [R7RS]
                             defs-okay?))))))))))                      ; [R7RS]

        ;; Add new frame for keeping track of bindings used
        ;; so we can detect redefinitions violating lexical scope.

        (add-fresh-used-frame!)

        (call-with-values                                              ; [R7RS]
         (lambda ()                                                    ; [R7RS]
           (loop (map (lambda (e) (make-wrap common-env e))
                      body-forms)
                 '()                     ; no forms accumulated yet
                 '()                     ; nor syntax-defs
                 '()                     ; nor bound-variables
                 exports                 ; but maybe some exports      ; [R7RS]
                 imported-libraries      ; and maybe some libraries    ; [R7RS]
                 (not (memq body-type                                  ; [R7RS]
                            '(expression-sequence define-library)))))  ; [R7RS]
         (lambda (forms                                                ; [R7RS]
                  syntax-defs                                          ; [R7RS]
                  bound-variables                                      ; [R7RS]
                  exports                                              ; [R7RS]
                  imported-libraries)                                  ; [R7RS]
           (check-expression-body body-type forms body-forms)

           ;; Add denotations used in this frame to those of parent.
           ;; This is just for the optional reporting of shadowing errors.

           (merge-used-with-parent-frame!)

           (k (reverse (expand-deferred forms))
              (reverse syntax-defs)
              bound-variables
              exports
              imported-libraries)))))

    (define (emit-body body-forms define-or-set)
      (map (lambda (body-form)
             (if (symbol? (car body-form))
                 `(,define-or-set ,(car body-form) ,(cdr body-form))
                 (cdr body-form)))
           body-forms))

    (define (parse-definition exp syntax-def?)
      (match exp
        ((- (? identifier? id))
         (values id (rename 'variable 'ex:unspecified)))
        ((- (? identifier? id) e)
         (values id e))
        ((- ((? identifier? id) . (? formals? formals)) body ___)
         (and syntax-def?
              (invalid-form exp))
         (values id `(,(rename 'macro 'lambda) ,formals ,@body)))))

    (define (parse-local-syntax t)
      (match t
        ((- ((x e) ___) body ___)
         (or (formals? x)
             (invalid-form t))
         (values x e body))))

    ;;; R7RS library declarations are of these types.                  ; [R7RS]
    ;;; FIXME: some of these might be listed unnecessarily.

    (define r7rs-library-declaration-types                             ; [R7RS]
      '(export import begin
        include include-ci include-library-declarations
        cond-expand))

    (define (check-cond-expand body-type type form)                    ; [R7RS]
      (and (list? form)
           (pair? form)
           (pair? (cdr form))
           (not (null? (filter (lambda (clause)
                                 (not (and (list? clause)
                                           (pair? clause))))
                               (cdr form))))
           (syntax-violation type
                             "Invalid cond-expand syntax"
                             form)))

    (define (check-expression-sequence body-type type form)
#;
      (if (memq 'define-library (list body-type type))                  ; FIXME
          (begin (display "check-expression-sequence: ")
                 (write (list body-type type))
                 (newline)
                 (pretty-print form)
                 (newline)))
      (and (eq? body-type 'expression-sequence)
           (memq type '(import program library define define-syntax
                        define-library))                               ; [R7RS]
           (syntax-violation type "Invalid form in expression sequence" form)))

    ;; New for R7RS.                                                   ; [R7RS]

    (define (check-r7rs-library body-type type form defs-okay?)        ; [R7RS]
#;
      (if (memq 'define-library (list body-type type))                  ; FIXME
          (begin (display "check-r7rs-library: ")
                 (write (list body-type type))
                 (newline)))
      (and (eq? body-type 'define-library)
           (not (memq type r7rs-library-declaration-types))
           (not defs-okay?)
           (syntax-violation type
                             "Invalid define-library declaration"
                             form)))             ; end of new code for ; [R7RS]

    (define (check-toplevel body-type type form)
      (and (not (eq? body-type 'toplevel))
           (not (and (eq? type 'import)                                ; [R7RS]
                     (or (and (eq? body-type 'program)                 ; [R7RS]
                              (not (eq? (larceny:execution-mode)       ; [R7RS]
                                        'r6rs)))                       ; [R7RS]
                         (eq? body-type 'define-library))))            ; [R7RS]
           (memq type '(import program library
                        define-library))                               ; [R7RS]
           (syntax-violation type
                             "Expression may only occur at toplevel"
                             form)))

    (define (check-valid-definition id common-env body-type form forms type)
#;
      (if (memq 'define-library (list body-type type))                  ; FIXME
          (begin (display "check-valid-definition: ")
                 (write (list body-type type))
                 (newline)))
      (and (not (eq? body-type 'toplevel))
           (duplicate? id common-env)
           (eq? (larceny:execution-mode) 'r6rs)
           (syntax-violation type "Redefinition of identifier in body" form id))
      (check-used id body-type form)
      (and (not (memq body-type `(toplevel program define-library)))    ; FIXME
           (not (null? forms))
           (not (symbol? (car (car forms))))
           (syntax-violation type
                             "Definitions may not follow expressions in a body"
                             form)))

    (define (check-expression-body body-type forms body-forms)
#;
      (if (memq 'define-library (list body-type))                       ; FIXME
          (begin (display "check-expression-body: ")
                 (write (list body-type))
                 (newline)))
      (and (eq? body-type 'lambda)
           (or (null? forms)
               (symbol? (caar forms)))
           (syntax-violation body-type "Body must be nonempty and end with an expression" body-forms)))

    ;;=========================================================================
    ;;
    ;; Syntax-case:
    ;;
    ;;=========================================================================

    (define (expand-syntax-case exp)
      (match exp
        ((- e ((? identifier? literals) ___) clauses ___)
         (expand-syntax-case2 e standard-ellipsis literals clauses))   ; [R7RS]
        ((- e (? identifier? ellipsis) ((? identifier? literals) ___)  ; [R7RS]
            clauses ___)                                               ; [R7RS]
         (expand-syntax-case2 e ellipsis literals clauses))))          ; [R7RS]

    ;; [R7RS]  Some rewriting was needed to support the R7RS ellipsis feature.

    (define (expand-syntax-case2 e ellipsis literals clauses)          ; [R7RS]
      (fluid-let ((*ellipsis* ellipsis))                               ; [R7RS]
        (let ((input (generate-guid 'input)))
          `(let ((,input ,(expand e)))
             ,(process-clauses clauses input literals)))))

    (define (ellipsis? x)                                              ; [R7RS]
      (and (identifier? x)                                             ; [R7RS]
           (free-identifier=? x *ellipsis*)))                          ; [R7RS]

    (define (process-clauses clauses input literals)

      (define (literal? pattern)
        (and (identifier? pattern)
             (memp (lambda (x)
                     (bound-identifier=? x pattern))
                   literals)))

      (define (process-match input pattern sk fk)
        (if (not (symbol? input))
            (let ((temp (generate-guid 'temp)))
              `(let ((,temp ,input))
                 ,(process-match temp pattern sk fk)))
            (match pattern
              ((? literal? id)    `(if (and (ex:identifier? ,input)
                                            (ex:free-identifier=? ,input ,(syntax-reflect id)))
                                       ,sk
                                       ,fk))
              ((syntax _)         sk)
              ((? ellipsis? :::)  (syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
              (()                 `(if (null? ,input) ,sk ,fk))
              ((? identifier? id) `(let ((,(binding-name (binding id)) ,input)) ,sk))
              ((p (? literal? id) . tail)
               `(if (pair? ,input)
                    ,(process-match `(car ,input)
                                    p
                                    (process-match `(cdr ,input)
                                                   (cdr pattern)
                                                   sk fk)
                                    fk)
                    ,fk))
              ((p (? ellipsis? :::))
               (let ((mapped-pvars (map (lambda (pvar) (binding-name (binding pvar)))
                                        (map car (pattern-vars p 0)))))
                 (if (and (identifier? p)                                   ; +++
                          (= (length mapped-pvars) 1))                      ; +++
                     `(if (list? ,input)                                    ; +++
                          (let ((,(car mapped-pvars) ,input))               ; +++
                            ,sk)                                            ; +++
                          ,fk)                                              ; +++
                     (let ((columns (generate-guid 'cols))
                           (rest    (generate-guid 'rest)))
                       `(ex:map-while (lambda (,input)
                                        ,(process-match input
                                                        p
                                                        `(list ,@mapped-pvars)
                                                        #f))
                                      ,input
                                      (lambda (,columns ,rest)
                                        (if (null? ,rest)
                                            (apply (lambda ,mapped-pvars ,sk)
                                                   (if (null? ,columns)
                                                       ',(map (lambda (ignore) '()) mapped-pvars)
                                                       (apply map list ,columns)))
                                            ,fk)))))))
              ((p (? ellipsis? :::) . tail)
               (let ((tail-length (dotted-length tail)))
                 `(if (>= (ex:dotted-length ,input) ,tail-length)
                      ,(process-match `(ex:dotted-butlast ,input ,tail-length)
                                      `(,p ,(cadr pattern))
                                      (process-match `(ex:dotted-last ,input ,tail-length)
                                                     tail
                                                     sk
                                                     fk)
                                      fk)
                      ,fk)))
              ((p1 . p2)
               `(if (pair? ,input)
                    ,(process-match `(car ,input)
                                    p1
                                    (process-match `(cdr ,input) p2 sk fk)
                                    fk)
                    ,fk))
              (#(ps ___)
               `(if (vector? ,input)
                    ,(process-match `(vector->list ,input)
                                    ps
                                    sk
                                    fk)
                    ,fk))
              ((? symbol? -)
               (syntax-violation 'syntax-case "Symbol object may not appear in pattern" pattern))
              (other
               `(if (equal? ,input ',other) ,sk ,fk)))))

      (define (pattern-vars pattern level)
        (match pattern
          ((? literal? -)               '())
          ((p (? literal? -) . tail)    (append (pattern-vars p level)
                                                (pattern-vars tail level)))
          ((p (? ellipsis? :::) . tail) (append (pattern-vars p (+ level 1))
                                                (pattern-vars tail level)))
          ((p1 . p2)                    (append (pattern-vars p1 level)
                                                (pattern-vars p2 level)))
          (#(ps ___)                    (pattern-vars ps level))
          ((? ellipsis? :::)            '())
          ((syntax _)                   '())
          ((? identifier? id)           (list (cons id level)))
          (-                            '())))

      (define (process-clause clause input fk)
        (match clause
          ((pattern . rest)
           (let ((pvars    (pattern-vars pattern 0)))
             (check-set? (map car pvars)
                         bound-identifier=?
                         (lambda (dup)
                           (syntax-violation 'syntax-case "Repeated pattern variable" clause dup)))
             (let ((mappings (map (lambda (pvar)
                                    (make-local-mapping 'pattern-variable (car pvar) (cdr pvar)))
                                  pvars)))
               (fluid-let ((*usage-env* (env-extend mappings *usage-env*)))
                 (process-match input
                                pattern
                                (match rest
                                  ((template)
                                   (expand template))
                                  ((fender template)
                                   `(if ,(expand fender)
                                        ,(expand template)
                                        ,fk))
                                  (- (syntax-violation 'syntax-case "Invalid clause" clause)))
                                fk)))))))

      ;; process-clauses

      (match clauses
        (()
         `(ex:invalid-form ,input))
        ((clause clauses ___)
         (let ((fail  (generate-guid 'fail)))
           `(let ((,fail (lambda () ,(process-clauses clauses input literals))))
              ,(process-clause clause input `(,fail)))))))

    ;;=========================================================================
    ;;
    ;; Syntax:
    ;;
    ;;=========================================================================

    (define (expand-syntax form)
      (match form
        ((- template)
         (process-template template 0 #f))))

    (define (process-template template dim ellipses-quoted?)
      (match template
        ((? ellipsis? :::)
         (if (not ellipses-quoted?)
             (syntax-violation 'syntax "Invalid occurrence of ellipses in syntax template" template))
         (syntax-reflect template))
        ((? identifier? id)
         (let ((binding (binding id)))
           (cond ((and binding
                       (eq? (binding-type binding) 'pattern-variable)
                       (binding-dimension binding))
                  => (lambda (pdim)
                       (if (<= pdim dim)
                           (begin
                             (check-binding-level id binding)
                             (register-use! id binding)
                             (binding-name binding))
                           (syntax-violation 'syntax "Template dimension error (too few ...'s?)" id))))
                 (else
                  (syntax-reflect id)))))
        (((? ellipsis? :::) p)                     ; Andre van Tonder gave us
         (if ellipses-quoted?                      ; this patch for ticket #637
             `(list ,(process-template (car template) dim #t)
                    ,(process-template p dim #t))
             (process-template p dim #t)))
        ((? (lambda (_) (not ellipses-quoted?))
            (t (? ellipsis? :::) . tail))
         (let* ((head (segment-head template)) 
                (vars
                 (map (lambda (mapping)
                        (let ((id      (car mapping))
                              (binding (cdr mapping)))
                          (check-binding-level id binding)
                          (register-use! id binding)
                          (binding-name binding)))
                      (free-meta-variables head (+ dim 1) '() 0))))
           (if (null? vars)
               (syntax-violation 'syntax "Too many ...'s" template)
               (let* ((x (process-template head (+ dim 1) ellipses-quoted?))
                      (gen (if (equal? (list x) vars)   ; +++
                               x                        ; +++
                               (if (= (length vars) 1) 
                                   `(map (lambda ,vars ,x)
                                         ,@vars)
                                   `(if (= ,@(map (lambda (var) 
                                                    `(length ,var))
                                                  vars))
                                        (map (lambda ,vars ,x)
                                             ,@vars)
                                        (ex:syntax-violation 
                                         'syntax 
                                         "Pattern variables denoting lists of unequal length preceding ellipses"
                                         ',(syntax->datum template) 
                                         (list ,@vars))))))
                      (gen (if (> (segment-depth template) 1)
                               `(apply append ,gen)
                               gen)))
                 (if (null? (segment-tail template))   ; +++
                     gen                               ; +++
                     `(append ,gen ,(process-template (segment-tail template) dim ellipses-quoted?)))))))
        ((t1 . t2)
         `(cons ,(process-template t1 dim ellipses-quoted?)
                ,(process-template t2 dim ellipses-quoted?)))
        (#(ts ___)
         `(list->vector ,(process-template ts dim ellipses-quoted?)))
        (other
         `(quote ,(expand other)))))
    
    (define (free-meta-variables template dim free deeper)
      (match template
        ((? identifier? id)
         (if (memp (lambda (x) (bound-identifier=? (car x) id)) free)
             free
             (let ((binding (binding id)))
               (if (and binding
                        (eq? (binding-type binding) 'pattern-variable)
                        (let ((pdim (binding-dimension binding)))
                          (and (> pdim 0) 
                               (not (>= deeper pdim))
                               (<= (- pdim deeper) 
                                   dim))))
                   (cons (cons id binding) free)
                   free))))
        ((t (? ellipsis? :::) . rest)
         (free-meta-variables t 
                              dim 
                              (free-meta-variables (segment-tail template) dim free deeper)
                              (+ deeper (segment-depth template))))  
        ((t1 . t2)
         (free-meta-variables t1 dim (free-meta-variables t2 dim free deeper) deeper))
        (#(ts ___) 
         (free-meta-variables ts dim free deeper))
        (- free)))
 
    ;; Count the number of `...'s in PATTERN.

    (define (segment-depth pattern)
      (match pattern
        ((p (? ellipsis? :::) . rest)
         (+ 1 (segment-depth (cdr pattern))))
        (- 0)))
      
    ;; All but the last ellipses
    
    (define (segment-head pattern)
      (let ((head
             (let recur ((pattern pattern))
               (match pattern
                 ((h (? ellipsis? :::) (? ellipsis? ::) . rest)
                  (cons h (recur (cdr pattern))))
                 ((h (? ellipsis? :::) . rest)
                  (list h))))))
        (match head 
          ((h (? ellipsis? :::) . rest)
           head)
          (- (car head)))))   

    ;; Get whatever is after the `...'s in PATTERN.

    (define (segment-tail pattern)
      (let loop ((pattern (cdr pattern)))
        (match pattern
          (((? ellipsis? :::) . tail)
           (loop tail))
          (- pattern))))

    ;;=========================================================================
    ;;
    ;; Detecting violations of lexical scope.
    ;;
    ;;=========================================================================

    ;; This is r6rs-optional.
    ;; For avoiding giving lexically invalid semantics to body
    ;; sequences according to the following semantics described in r6rs:
    ;; A definition in the sequence of forms must not define any
    ;; identifier whose binding is used to determine the meaning of the
    ;; undeferred portions of the definition or any definition that precedes
    ;; it in the sequence of forms.
    ;; This implementation treats a possble violation of the restriction
    ;; as a syntax violation.

    ;; The parameter *used* keeps track of bindings used so we can
    ;; detect redefinitions violating lexical scope in body sequences.
    ;; The car of *used* contains bindings used in current frame.

    (define (add-fresh-used-frame!)
      (set! *used* (cons '() *used*)))

    (define (register-use! id binding)
      (set! *used* (cons (cons (cons id binding)
                               (car *used*))
                         (cdr *used*))))

    (define (merge-used-with-parent-frame!)
      (set! *used* (cons (append (car  *used*)
                                 (cadr *used*))
                         (cddr *used*))))

    (define (check-used id body-type form)
      (and (not (eq? body-type 'toplevel))
           ;; The car contains bindings for current frame and nested frames
           (let* ((already-used (car *used*))
                  ;; This destructively changes *used* and must follow previous
                  (binding (binding id)))
             (if (memp (lambda (mapping)
                         (and (eq? binding (cdr mapping))
                              (bound-identifier=? id (car mapping))))
                       already-used)
                 (syntax-violation
                  'definition
                  "Definition of identifier that may have already affected meaning of undeferred portions of body"
                  form
                  id)))))

    ;;==========================================================================
    ;;
    ;; Libraries:
    ;;
    ;;==========================================================================

    (define (expand-program t)
      (match t
        ((program import-clause forms ___)
         (expand-library-or-program
          `(,program (,(datum->syntax program (generate-guid 'program)))
                     (,(datum->syntax program 'export))
                     ,import-clause
                     ,@forms)
          'program))))

    (define (expand-library t)
      (expand-library-or-program t 'library))

    ;; New for R7RS.
    ;; Creates empty export and import declaration as necessary
    ;; to make a define-library start out like an R6RS library.

    (define (expand-define-library t)                                  ; [R7RS]
      (match t
        ((keyword name ((syntax export) sets ___)
                       ((syntax import) specs ___) body-forms ___)
         (expand-library-or-program t 'define-library))
        ((keyword name ((syntax export) sets ___) body-forms ___)
         (expand-define-library
          `(,keyword ,name
                     ,(caddr t)                         ; export declaration
                     (,(datum->syntax keyword 'import)) ; empty import decl
                     ,@(cdddr t))))
        ((keyword name ((syntax import) specs ___) body-forms ___)
         (expand-define-library
          `(,keyword ,name
                     (,(datum->syntax keyword 'export)) ; empty export decl
                     ,@(cddr t))))
        ((keyword name body-forms ___)
         (expand-define-library
          `(,keyword ,name
            (,(datum->syntax keyword 'export))          ; empty export decl
            (,(datum->syntax keyword 'import))          ; empty import decl
            ,@(cddr t))))))

    ;; <library-type> ::= define-library | library | program           ; [R7RS]

    (define (expand-library-or-program t library-type)
      (match t
        ((keyword name ((syntax export) sets ___)
                       ((syntax import) specs ___) body-forms ___)
         (let ((name (syntax->datum
                      (scan-library-name name library-type))))         ; [R7RS]
           (let ((exports (scan-exports sets library-type)))           ; [R7RS]
             (call-with-values
                 (lambda () (scan-imports specs))
               (lambda (imported-libraries imports)
                 (fluid-let ((*usage-env*        (make-unit-env))
                             (*current-library*  name)
                             (*syntax-reflected* #f))       ; +++ space

                   (if (or (eq? library-type 'define-library)          ; [R7RS]
                           (and (not (eq? (larceny:execution-mode)     ; [R7RS]
                                          'r6rs))                      ; [R7RS]
                                (eq? library-type 'program)))          ; [R7RS]
                       (env-import! keyword                            ; [R7RS]
                                    (make-r7rs-library-language)       ; [R7RS]
                                    *usage-env*))                      ; [R7RS]
                   (env-import! keyword imports *usage-env*)
                   (import-libraries-for-expand
                    imported-libraries
                    (map not imported-libraries)
                    0)

                   (let ((initial-env-table *env-table*))   ; +++ space
                     (scan-sequence
                      library-type
                      make-local-mapping
                      body-forms
                      exports                                          ; [R7RS]
                      imported-libraries                               ; [R7RS]
                      (lambda (forms
                               syntax-definitions
                               bound-variables
                               exports                                 ; [R7RS]
                               imported-libraries)                     ; [R7RS]
                        (let* ((exports
                                (map (lambda (mapping)
                                       (cons (id-name (car mapping))
                                             (let ((binding
                                                    (binding (cadr mapping))))
                                               (or binding
                                                   (syntax-violation
                                                    'library
                                                    "Unbound export"
                                                    t
                                                    (cadr mapping)))
                                               (if (binding-mutable? binding)
                                                   (syntax-violation
                                                    'library
                                                    "Attempt to export mutable variable"
                                                    t
                                                    (cadr mapping)))
                                               binding)))
                                     exports))
                               (expanded-library
                                (case library-type
                                 ((program)
                                  `(begin
                                    #\P ; [Larceny]
                                    (ex:import-libraries-for-run
                                     ',imported-libraries
                                     ',(current-builds imported-libraries)
                                     0)
                                    ,@(emit-body forms 'define)))
                                 ((library define-library)             ; [R7RS]
                                  `(begin
                                    #\L ; [Larceny]
                                    ,@(map (lambda (var)
                                             `(define ,var ex:unspecified))
                                           bound-variables)
                                    (ex:register-library!
                                     (ex:make-library
                                      ',name
                                      ;; Store as thunk so it is not
                                      ;; unnecesarily uncompressed at runtime
                                      (lambda ()
                                        ,(if *syntax-reflected*     ; +++ space
                                             `(ex:uncompress        ; +++ space
                                               ',(compress (drop-tail
                                                            *env-table*
                                                            initial-env-table)))
                                             `'()))                 ; +++ space
                                      ',exports
                                      ',imported-libraries
                                      ',(current-builds imported-libraries)
                                      ;; visit
                                      (lambda ()
                                        ,@(map (lambda (def)
                                                 `(ex:register-macro!
                                                   ',(car def)
                                                   ,(cdr def)))
                                               syntax-definitions)
                                        (values))
                                      ;; invoke
                                      (lambda ()
                                        ,@(map (lambda (var)
                                                 `(set! ,var ex:undefined))
                                               bound-variables)
                                        ,@(emit-body forms ex:undefined-set!)
                                        (values))
                                      ;; build
                                      ',(generate-guid 'build)))
                                    (values))))))
                          ;; Register library for any further expansion.
                          ;; FIXME: expand-file shouldn't do this
                          ;; [Larceny]
                          (if (and (memq library-type
                                         '(library  define-library))   ; [R7RS]
                                   (not (larceny:r6rs-expand-only)))
                              (eval expanded-library (interaction-environment)))
                          expanded-library))))))))))))

    (define (env-import! keyword imports env)
      (env-extend! (map (lambda (import)
                          (cons (cons (car import)
                                      (id-colors keyword))
                                (cdr import)))
                        imports)
                   env))

    (define (current-builds imported-libraries)
      (map (lambda (lib-entry)
             (ex:library-build (ex:lookup-library (car lib-entry))))
           imported-libraries))

    (define (import-libraries-for-expand imports builds phase)
      (ex:import-libraries-for
       imports
       builds
       phase
       (lambda (library phase imported)
         (if (and (>= phase 0)
                  (not (ex:library-visited? library)))
             (begin
               (set! *env-table*
                     (append ((ex:library-envs library)) *env-table*))
               ((ex:library-visiter library))
               (ex:library-visited?-set! library #t)))
         (if (and (>= phase 1)
                  (not (ex:library-invoked? library)))
             (begin 
               ((ex:library-invoker library))
               (ex:library-invoked?-set! library #t))))
       'expand))

    ;; Returns ((<rename-identifier> <identifier> <level> ...) ...)
    ;; The rename syntax for R7RS is different from that of R6RS.      ; [R7RS]

    (define (scan-exports specs body-type)                             ; [R7RS]
      (let ((exports
             (apply append
                    (map (lambda (export-spec)
                           (scan-export-spec export-spec body-type))   ; [R7RS]
                         specs))))
        (check-set? exports
                    (lambda (x y)
                      (eq? (id-name (car x))
                           (id-name (car y))))
                    (lambda (dup)
                      (syntax-violation 'export "Duplicate export" specs dup)))
        exports))

    ;; The following code will allow either syntax in top-level        ; [R7RS]
    ;; programs and read/eval/print loops, but will enforce the
    ;; correct syntax for R7RS define-library and R6RS library.

    (define (scan-export-spec spec body-type)                          ; [R7RS]
      (define (complain)
        (syntax-violation 'export "Invalid export spec" spec))
      (match spec
        ((? identifier? x)
         `((,x ,x 0)))
        (((syntax rename) (? identifier? x) (? identifier? y))         ; [R7RS]
         (if (eq? body-type 'library)                                  ; [R7RS]
             (complain))                                               ; [R7RS]
         `((,y ,x 0)))                                                 ; [R7RS]
        (((syntax rename) ((? identifier? xs) (? identifier? ys)) ___)
         (if (eq? body-type 'define-library)
             (complain))
         (map (lambda (x y) `(,y ,x 0)) xs ys))
        (- (complain))))

    ;; Returns list of all new-exports that aren't already in exports. ; [R7RS]

    (define (filter-exports new-exports exports)        ; new code for ; [R7RS]
      (let ((names (map (lambda (export)
                          (id-name (car export)))      ; see scan-exports above
                        exports)))
        (filter (lambda (export)
                  (not (memq (id-name (car export)) names)))
                new-exports)))                   ; end of new code for ; [R7RS]

    ;; Returns
    ;;    (values ((<library reference> <level> ...) ....)
    ;;            ((<local name> . <binding>) ...))
    ;; with no repeats.

    (define (scan-imports specs)
      (let loop ((specs specs)
                 (imported-libraries '())
                 (imports '()))
        (if (null? specs)
            (values imported-libraries (unify-imports imports))
            (call-with-values
                (lambda () (scan-import-spec (car specs)))
              (lambda (library-ref levels more-imports)
                (loop (cdr specs)
                      ;; library-ref = #f if primitives spec
                      ;; FIXME: that's not true; see below
                      (if library-ref
                          (cons (cons library-ref levels)
                                imported-libraries)
                          imported-libraries)
                      (append more-imports imports)))))))

    ;; Returns (values <library reference> | #f
    ;;                 (<level> ...)
    ;;                 ((<local name> . <binding>) ...)
    ;; where <level> ::= <integer>
    ;; FIXME: For primitives, (larceny PRIMITIVES) is the library name.

    (define (scan-import-spec spec)

      (call-with-values
          (lambda () (scan-levels spec))
        (lambda (levels import-set)
          (let loop ((import-set import-set)
                     (adjuster (lambda (set) set)))

            (define (check-presence names mappings from)
              (for-each (lambda (name)
                          (or (assq name mappings)
                              (syntax-violation from
                                                (string-append "Identifier not in set: "
                                                               (list->string (map car mappings) " "))
                                                import-set
                                                name)))
                        names))

            (match import-set
              ;; FIXME: (primitives foo bar) should be a legal library name
              (((syntax primitives) (? identifier? xs) ___)
               (values #f
                       levels
                       (map (lambda (mapping)
                              (cons (car mapping)
                                    (make-binding 'variable
                                                  (cdr mapping)
                                                  levels
                                                  #f
                                                  '(larceny PRIMITIVES))))
                            (adjuster (map (lambda (name) (cons name name))
                                           (syntax->datum xs))))))
              (((syntax only) set (? identifier? xs) ___)
               (let ((args (syntax->datum xs)))
                 (loop set
                       (compose adjuster (lambda (mappings)
                                           (check-presence args mappings 'only)
                                           (filter (lambda (mapping)
                                                     (memq (car mapping) args))
                                                   mappings))))))
              (((syntax except) set (? identifier? xs) ___)
               (let ((args (syntax->datum xs)))
                 (loop set
                       (compose adjuster (lambda (mappings)
                                           (check-presence args mappings 'except)
                                           (filter (lambda (mapping)
                                                     (not (memq (car mapping) args)))
                                                   mappings))))))
              (((syntax prefix) set (? identifier? pre))
               (loop set
                     (compose adjuster (lambda (mappings)
                                         (map (lambda (mapping)
                                                (cons (string->symbol
                                                       (string-append
                                                        (symbol->string (syntax->datum pre))
                                                        (symbol->string (car mapping))))
                                                      (cdr mapping)))
                                              mappings)))))
              (((syntax rename) set ((? identifier? xs) (? identifier? ys)) ___)
               (let ((args (syntax->datum (cddr import-set))))
                 (loop set
                       (compose adjuster
                                (lambda (mappings)
                                  (check-presence (map car args) mappings 'rename)
                                  (map (lambda (mapping)
                                         (cons (cond ((assq (car mapping) args) => cadr)
                                                     (else (car mapping)))
                                               (cdr mapping)))
                                       mappings))))))
              ;; Library names could begin with these (ticket #773).
;             (((syntax primitives) . -) (invalid-form import-set))
;             (((syntax only)       . -) (invalid-form import-set))
;             (((syntax except)     . -) (invalid-form import-set))
;             (((syntax prefix)     . -) (invalid-form import-set))
;             (((syntax rename)     . -) (invalid-form import-set))
              (-
               (let ((library-ref (library-ref import-set)))
                 (if library-ref
                     (let* ((library (ex:lookup-library (syntax->datum library-ref)))
                            (exports (ex:library-exports library))
                            (imports
                             (map (lambda (mapping)
                                    (cons (car mapping)
                                          (let ((binding (cdr (assq (cdr mapping) exports))))
                                            (make-binding (binding-type binding)
                                                          (binding-name binding)
                                                          (compose-levels levels (binding-levels binding))
                                                          (binding-mutable? binding)
                                                          (binding-library binding)))))
                                  (adjuster (map (lambda (name) (cons name name))
                                                 (map car exports))))))
                       (values (syntax->datum library-ref)
                               levels
                               imports))
                     (syntax-violation 'import "Invalid import set" import-set)))))))))

    (define (scan-levels spec)
      (match spec
        (((syntax for) set levels ___)
         (let ((levels
                (map (lambda (level)
                       (match level
                         ((syntax run)                   0)
                         ((syntax expand)                1)
                         (((syntax meta) (? integer? n)) n)
                         (- (syntax-violation 'for "Invalid level in for spec" spec level))))
                     levels)))
           (check-set? levels = (lambda (dup) (syntax-violation 'for "Repeated level in for spec" spec dup)))
           (values levels set)))
        (- (values '(0) spec))))

    (define (compose-levels levels levels*)
      (apply unionv
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

    (define (unify-imports imports)
      (let ((seen '()))
        (let loop ((imports imports))
          (if (null? imports)
              seen
              (let* ((mapping (car imports))
                     (probe (assq (car mapping) seen)))
                (if probe
                    (begin
                      (or (eq? (binding-name (cdr mapping))
                               (binding-name (cdr probe)))
                          (syntax-violation
                           'import
                           (string-append "Different bindings for identifier imported from libraries ("
                                          (list->string (binding-library (cdr mapping)) " ")
                                          ") and ("
                                          (list->string (binding-library (cdr probe)) " ") ")")
                           (car mapping)))
                      (set-cdr! probe
                                (make-binding (binding-type (cdr probe))
                                              (binding-name (cdr probe))
                                              (unionv (binding-levels (cdr probe))
                                                      (binding-levels (cdr mapping)))
                                              (binding-mutable? (cdr probe))
                                              (binding-library (cdr probe)))))
                    (set! seen (cons mapping seen)))
                (loop (cdr imports)))))))

    (define (scan-library-name e library-type)                         ; [R7RS]
      (library-ref-helper e version? library-type))                    ; [R7RS]

    (define (library-ref e)
      (library-ref-helper
       (match e
         (((syntax library) name) name)
         (((syntax library) . -)  (invalid-form e))
         (- e))
       version-reference?
       'define-library))                                               ; [R7RS]

    (define (library-ref-helper e version? library-type)               ; [R7RS]
      (define (complain)
        (syntax-violation 'library "Invalid library reference" e))
      (match e
        (((? identifier? ids) ___)                ids)
        (((? identifier? ids) ___ (? version? -)) ids)
        (((? library-name-component? ids) ___)                         ; [R7RS]
         (if (eq? library-type 'define-library)                        ; [R7RS]
             ids                                                       ; [R7RS]
             (complain)))                                              ; [R7RS]
        (- (complain))))

    (define (library-name-component? e)                                ; [R7RS]
      (or (identifier? e)                                              ; [R7RS]
          (and (exact-integer? e)                                      ; [R7RS]
               (<= 0 e))))                                             ; [R7RS]

    (define (version? e)
      (and (list? e)
           (for-all subversion? e)))

    (define (subversion? x)
      (and (integer? x)
           (>= x 0)))

    (define (version-reference? e)
      (match e
        (((syntax and) (? version-reference? -) ___) #t)
        (((syntax or)  (? version-reference? -) ___) #t)
        (((syntax not) (? version-reference? -))     #t)
        (((? subversion-reference? -) ___)           #t)
        (-                                           #f)))

    (define (subversion-reference? e)
      (or (subversion? e)
          (subversion-condition? e)))

    (define (subversion-condition? e)
      (match e
        (((syntax >=)  (? subversion? -))               #t)
        (((syntax <=)  (? subversion? -))               #t)
        (((syntax not) (? subversion? -))               #t)
        (((syntax and) (? subversion-reference? -) ___) #t)
        (((syntax or)  (? subversion-reference? -) ___) #t)
        (-                                              #f)))

    ;;==========================================================================
    ;;
    ;; Debugging facilities:
    ;;
    ;;==========================================================================

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
                           (else (assertion-violation 'syntax-violation
                                                      "Invalid subform in syntax violation"
                                                      maybe-subform))))
            (larceny:plength 7) ; [Larceny]
            (larceny:plevel 5)) ; [Larceny]
        (display who)
        (newline)
        (newline)
        (display message)
        (newline)
        (newline)
        (if subform
            (begin (display "Subform: ")
                   (parameterize ((print-length larceny:plength)    ; [Larceny]
                                  (print-level larceny:plevel))
                     (pretty-print (syntax-debug subform)))
                   (newline)))
        (display "Form: ")
        (pretty-print (syntax-debug form))
        (newline)
        (display "Trace: ")
        (newline)
        (newline)
        (for-each (lambda (exp)
                    (display "  ")
                    (parameterize ((print-length larceny:plength)   ; [Larceny]
                                   (print-level larceny:plevel))
                      (pretty-print (syntax-debug exp)))
                    (newline))
                  *trace*)
        ;(error 'syntax-violation "Integrate with host error handling here")

        ; [Larceny]

        (let* ((form (syntax-debug form))
               (subform (if subform (syntax-debug subform) #f))
               (c1 (make-who-condition who))
               (c2 (make-message-condition message))
               (c3 (make-syntax-violation form subform))
               (c (if who
                      (condition c1 c2 c3)
                      (condition c2 c3))))
          (raise c))))

    (define (syntax-debug exp)
      (sexp-map (lambda (leaf)
                  (cond ((identifier? leaf)
                         (id-name leaf))
                        (else leaf)))
                exp))

    ;;==========================================================================
    ;;
    ;;  Eval and environment:
    ;;
    ;;==========================================================================

    (define eval-template
      (make-identifier 'eval-template
                       '()
                       '()
                       0
                       `(anonymous)))

    ;; The R7RS adds a mutable interaction environment, so             ; [R7RS]
    ;; a boolean flag indicates whether the environment is mutable.

    (define (make-r7rs-environment imported-libraries mutable? env)    ; [R7RS]
      (cons imported-libraries (cons mutable? env)))                   ; [R7RS]
    (define (make-r6rs-environment imported-libraries env)
      (make-r7rs-environment imported-libraries #f env))               ; [R7RS]
    (define r6rs-environment-imported-libraries car)
    (define r7rs-environment-is-mutable?        cadr)                  ; [R7RS]
    (define r6rs-environment-env                cddr)                  ; [R7RS]

    ;; FIXME
    ;;
    ;; The R7RS standard library (scheme repl) exposes the             ; [R7RS]
    ;; interaction environment.

    (define (r7rs-interaction-environment)                             ; [R7RS]
      (make-r7rs-environment '() #t *usage-env*))                      ; [R7RS]

    (define (environment . import-specs)
      (fluid-let ((*usage-env* (make-unit-env)))
        (env-import! eval-template (make-library-language) *usage-env*)
        (call-with-values
            (lambda () 
              (fluid-let ((*phase* 0))
                (scan-imports
                 (map (lambda (spec)
                        (datum->syntax eval-template spec))
                      import-specs))))
          (lambda (imported-libraries imports)
            (make-r6rs-environment imported-libraries
                                   (let ((env (make-unit-env)))
                                     (env-import! eval-template imports env)
                                     env))))))

    ;; FIXME: the mutable case assumes there's only one mutable        ; [R7RS]
    ;; environment, and it's the R7RS interaction environment.
    ;; That should work for now, but it might break in R7RS large.

    (define (r6rs-eval exp env)
      (fluid-let ((*usage-env* (r6rs-environment-env env)))
        (let ((exp (datum->syntax eval-template exp))
              (imported-libraries (r6rs-environment-imported-libraries env)))
          (import-libraries-for-expand (r6rs-environment-imported-libraries env)
                                       (map not imported-libraries)
                                       0)
          (ex:import-libraries-for-run (r6rs-environment-imported-libraries env)
                                       (map not imported-libraries)
                                       0)
          (if (r7rs-environment-is-mutable? env)                       ; [R7RS]
              (repl (list exp))                                        ; [R7RS]
              (eval (expand-begin
                     ;; wrap in expression begin so no definition
                     ;; can occur as required by r6rs
                     `(,(rename 'macro 'begin) ,exp))
                    (interaction-environment))))))

    ;;==========================================================================
    ;;
    ;; Library reflection:
    ;;
    ;;=========================================================================

    (define (environment-bindings r6rs-env)
      (map format-mapping
           (caar (r6rs-environment-env r6rs-env))))

    (define (format-mapping mapping)
      `((name ,(caar mapping))
        (type ,(binding-type (cdr mapping)))
        (from ,(binding-library (cdr mapping)))
        (levels ,(binding-levels (cdr mapping)))))

    ;;=====================================================================
    ;;
    ;; Utilities:
    ;;
    ;;=====================================================================

    (define (flatten l)
      (cond ((null? l) l)
            ((pair? l) (cons (car l)
                             (flatten (cdr l))))
            (else (list l))))

    ;; In R7RS Scheme, circular constants can be quoted.               ; [R7RS]
    ;; FIXME: Unclear whether all shared structure must be preserved.  ; [R7RS]

    (define (sexp-map f s)                                             ; [R7RS]
      (cond ((object-is-circular? s)                                   ; [R7RS]
             (larceny:object-map f s))                                 ; [R7RS]
            (else                                                      ; [R7RS]
             (sexp-map-simple f s))))                                  ; [R7RS]
             
    (define (sexp-map-simple f s)                                      ; [R7RS]
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
        (cond ((< length-left n) (assertion-violation 'dotted-butlast "List too short" ls n))
              ((= length-left n) '())
              (else
               (cons (car ls)
                     (recurse (cdr ls)
                              (- length-left 1)))))))

    (define (dotted-last ls n)
      (let recurse ((ls ls)
                    (length-left (dotted-length ls)))
        (cond ((< length-left n) (assertion-violation 'dotted-last "List too short" ls n))
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

    (define (check-set? ls = fail)
      (or (null? ls)
          (if (memp (lambda (x)
                      (= x (car ls)))
                    (cdr ls))
              (fail (car ls))
              (check-set? (cdr ls) = fail))))

    (define (unionv . sets)
      (cond ((null? sets) '())
            ((null? (car sets))
             (apply unionv (cdr sets)))
            (else
             (let ((rest (apply unionv
                                (cdr (car sets))
                                (cdr sets))))
               (if (memv (car (car sets)) rest) ; note use of memv
                   rest
                   (cons (car (car sets)) rest))))))
    
    (define (union2 set1 set2)                                         ; [R7RS]
      (cond ((null? set1)
             set2)
            ((member (car set1) set2)    ; note use of member
             (union2 (cdr set1) set2))
            (else
             (union2 (cdr set1) (cons (car set1) set2)))))
    
    (define (drop-tail list tail)
      (cond ((null? list)    '())
            ((eq? list tail) '())
            (else
             (cons (car list)
                   (drop-tail (cdr list) tail)))))

    (define (list->string e separator)
      (define (tostring x)
        (cond ((symbol? x)
               (symbol->string x))
              ((number? x)
               (number->string x))
              (else
               (assertion-violation 'list->string "Invalid argument" e))))
      (if (null? e)
          ""
          (string-append
           (tostring (car e))
           (apply string-append
                  (map (lambda (x)
                         (string-append separator (tostring x)))
                       (cdr e))))))

    (define (compose f g)
      (lambda (x) (f (g x))))

    (define (check x p? from)
      (or (p? x)
          (syntax-violation from "Invalid argument" x)))

    (define (invalid-form exp)
      (syntax-violation #f "Invalid form" exp))

    ;;============================================================================
    ;;
    ;; REPL integration:
    ;;
    ;;============================================================================

    ;; Evaluates a sequence of library definitions, commands, and top-level 
    ;; import forms in the interactive environment.  The semantics for 
    ;; evaluating libraries in and importing bindings into the interactive 
    ;; environment is consistent with the ERR5RS proposal at
    ;; http://scheme-punks.cyber-rush.org/wiki/index.php?title=ERR5RS:Libraries.
    ;; Bindings in the interactive environment persist between invocations 
    ;; of REPL.
    
    ;; Modified for Larceny, mainly so Larceny's repl will print
    ;; the results.
#|
    (define (repl exps)
      (with-toplevel-parameters
       (lambda ()
         (for-each (lambda (exp)
                     (for-each (lambda (exp)
                                 (for-each (lambda (result)
                                             (display result)
                                             (newline))
                                           (call-with-values
                                            (lambda ()
                                              (eval exp (interaction-environment)))
                                            list)))
                               (expand-toplevel-sequence (list exp))))
                   exps))))
|#

    ;; Larceny-specific version.                                    ; [Larceny]

    (define (repl exps)
      (define (eval1 exp)
        (eval exp (interaction-environment)))
      (define (inner-loop exps)
        (cond ((null? exps)
               (if #f #f))
              ((null? (cdr exps))
               (eval1 (car exps)))
              (else
               (eval1 (car exps))
               (inner-loop (cdr exps)))))
      (define (outer-loop exps)
        (cond ((null? exps)
               (if #f #f))
              ((null? (cdr exps))
               (inner-loop (expand-toplevel-sequence exps)))
              (else
               (inner-loop (expand-toplevel-sequence (list (car exps))))
               (outer-loop (cdr exps)))))
      (with-toplevel-parameters
       (lambda ()
        (outer-loop exps))))
    
    ;; Evaluates a sequence of forms of the format
    ;; <library>* | <library>* <toplevel program>.
    ;; The <toplevel program> environment is separate from the 
    ;; interactive REPL environment and does not persist
    ;; between invocations of run-r6rs-sequence.  
    ;; For importing and evaluating stuff in the persistent 
    ;; interactive environment, see REPL above.

    ;; NOTE: expand-toplevel-sequence calls eval on every library
    ;; in the sequence.  (It calls scan-sequence, which calls
    ;; expand-library, which calls expand-library-or-program,
    ;; which calls eval on the expanded library.)  That's why
    ;; run-r6rs-sequence calls eval only on the expressions
    ;; obtained by expanding the program, if present.
    ;; See ticket #655.

    (define (run-r6rs-sequence forms)
      (with-toplevel-parameters
       (lambda ()
         (let* ((forms (normalize forms)))
           (call-with-values
            (lambda ()
              (partition (lambda (form)
                           (and (pair? form)
                                (eq? 'program (car form))))
                         forms))
            (lambda (pgms libs)
              (expand-toplevel-sequence libs)
              (if (not (null? pgms))
                  (let* ((exps (expand-toplevel-sequence pgms)))
                    (for-each (lambda (exp)
                                (eval exp (interaction-environment)))
                              exps)))))))))

    (define (run-r6rs-program filename)
      (run-r6rs-sequence (read-file filename)))

    ;; Puts parameters to a consistent state for the toplevel
    ;; Old state is restored afterwards so that things will be
    ;; reentrant. 

    (define with-toplevel-parameters
      (lambda (thunk)
        (fluid-let ((*trace*            '())
                    (*current-library*  '())
                    (*phase*            0)
                    (*used*             (list '()))
                    (*color*            (generate-color))
                    (*usage-env*        *toplevel-env*)
                    (*syntax-reflected* #f))
          (thunk))))
    
    (define (expand-toplevel-sequence forms)
      (scan-sequence 'toplevel
                     make-toplevel-mapping
                     (source->syntax forms)
                     '()                                               ; [R7RS]
                     '()                                               ; [R7RS]
                     (lambda (forms
                              syntax-definitions
                              bound-variables
                              no-exports                               ; [R7RS]
                              no-imports)                              ; [R7RS]
                       (emit-body forms 'define))))

    ;; R7RS load takes an optional second argument.                    ; [R7RS]
    ;; We take some care to make this reentrant so that 
    ;; it can be used to recursively load libraries while
    ;; expanding a client library or program.
    ;;
    ;; FIXME: It should be possible to use (r6rs-eval exp env)
    ;; even if no second argument is given, defaulting to the
    ;; interaction environment defined by (scheme repl).  By
    ;; the way, that R7RS environment is very different from
    ;; the R5RS (interaction-environment) used following macro
    ;; expansion.
    
    (define (r6rs-load filename . rest)                                ; [R7RS]
      (let ((env (and (pair? rest) (car rest))))                       ; [R7RS]
        (with-toplevel-parameters
         (lambda ()
           (for-each (lambda (exp)
                       (if env                                         ; [R7RS]
                           (r6rs-eval exp env)                         ; [R7RS]
                           (for-each (lambda (exp)
                                       (eval exp (interaction-environment)))
                                     (expand-toplevel-sequence (list exp)))))
                     (read-file filename))))))
      
    ;; This may be used as a front end for the compiler.
    ;; It expands a file consisting of a possibly empty sequence
    ;; of libraries optionally followed by a <toplevel program>.
    ;; The result is a sequence of vanilla r5rs-like toplevel
    ;; definitions and expressions.

    (define (expand-file filename target-filename)
      (with-toplevel-parameters
       (lambda ()
         (write-file (expand-toplevel-sequence (normalize (read-file filename)))
                     target-filename))))

    ;; This approximates the common r5rs behaviour of
    ;; expanding a toplevel file but treating unbound identifiers
    ;; as bare symbols that may refer to variables in the built-in toplevel
    ;; environment.  The environment argument should import at least the
    ;; macros necessary to expand the file.
    ;; This is provided mainly to be able to self-expand this expander
    ;; metacircularly (see the relevant note at the top of this file).
    ;; In contrast, expand-file strictly isolates a <toplevel program>
    ;; environment from the builtin environment and strictly disallows
    ;; unbound identifiers.
    ;; The resulting file will need the include file runtime.scm
    ;; and the appropriate libraries that constitute the env argument
    ;; to be preloaded before it can be run.

    (define (expand-r5rs-file filename target-filename r6rs-env)
      (with-toplevel-parameters
       (lambda ()
         (fluid-let ((make-free-name (lambda (symbol) symbol))
                     (*usage-env*    (r6rs-environment-env r6rs-env))
                     (*macro-table*  *macro-table*))
           (let ((imported-libraries (r6rs-environment-imported-libraries r6rs-env)))
             (import-libraries-for-expand (r6rs-environment-imported-libraries r6rs-env) (map not imported-libraries) 0)
             (write-file (cons `(ex:import-libraries-for-run ',(r6rs-environment-imported-libraries r6rs-env)
                                                             ',(current-builds imported-libraries)
                                                             0)
                               (expand-toplevel-sequence (read-file filename)))
                         target-filename))))))
       
    ;; Keeps (<library> ...) the same.
    ;; Converts (<library> ... . <toplevel program>)
    ;; to (<library> ... (program . <toplevel program>))

    (define (normalize exps)
      (define (error)
        (let ((newline (string #\newline)))
          (syntax-violation
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
                  ((library define-library)                            ; [R7RS]
                   (loop (cdr exps)
                         (cons (car exps) normalized)))
                  ((import)
                   (loop '()
                         (cons (cons 'program exps)
                               normalized)))
                  (else (error)))
                (error)))))

    (define (read-file fn)
      (let ((p (open-input-file fn)))
        (let f ((x (read p)))
          (if (eof-object? x)
              (begin (close-input-port p) '())
              (cons x
                    (f (read p)))))))

    (define (write-file exps fn)
      (if (file-exists? fn)
          (delete-file fn))
      (let ((p (open-output-file fn)))
        (for-each (lambda (exp)
                    (write exp p)
                    (newline p))
                  exps)
        (close-output-port p)))

    ;;==========================================================================
    ;;
    ;; Toplevel bootstrap:
    ;;
    ;;==========================================================================

    (define toplevel-template
      (make-identifier 'toplevel-template
                       '()
                       '()
                       0
                       #f))

    (define (source->syntax datum)
      (datum->syntax toplevel-template datum))

    ;;===================================================================
    ;;
    ;; Language for bootstrapping the REPL session and (environment ---):
    ;;
    ;;===================================================================

    ;; [R7RS]  These definitions have been refactored for R7RS.

    (define library-language-names:common
      `(program library define-library
                export import
                only except prefix rename primitives))

    (define library-language-names:r7rs-only
      `(begin cond-expand                                              ; [R7RS]
        include include-ci include-library-declarations))              ; [R7RS]

    (define library-language-names:r6rs-only
      `(for run expand meta
        >= <= and or not))

    (define library-language-names
      (append library-language-names:common
              library-language-names:r7rs-only
              library-language-names:r6rs-only))

    (define r7rs-library-language-names
      (append library-language-names:common
              library-language-names:r7rs-only))

    (define r6rs-library-language-names
      (append library-language-names:common
              library-language-names:r6rs-only))

    (define (make-a-specific-library-language names)
      (map (lambda (name)
             (cons name (make-binding 'macro name '(0) #f '())))
           names))

    ;; FIXME: unused

    (define (make-minimal-library-language)
      (make-a-specific-library-language library-language-names:common))

    (define (make-library-language)
      (make-a-specific-library-language library-language-names))

    (define (make-r7rs-library-language)
      (make-a-specific-library-language r7rs-library-language-names))

    ;; FIXME: unused

    (define (make-r6rs-library-language)
      (make-a-specific-library-language r6rs-library-language-names))

    ;; [R7RS]  End of refactored definitions for R7RS.

    ;;===================================================================
    ;;
    ;; Bootstrap library containing macros defined in this expander.
    ;;
    ;;===================================================================

    (set! *ellipsis* standard-ellipsis)                                ; [R7RS]

    (ex:register-library!
     (let ((primitive-macro-mapping
            `((lambda        . ,expand-lambda)
              (if            . ,expand-if)
              (set!          . ,expand-set!)
              (begin         . ,expand-begin)
              (syntax        . ,expand-syntax)
              (quote         . ,expand-quote)
              (let-syntax    . ,expand-local-syntax)
              (letrec-syntax . ,expand-local-syntax)
              (syntax-case   . ,expand-syntax-case)
              (and           . ,expand-and)
              (or            . ,expand-or)
              (define        . ,invalid-form)
              (define-syntax . ,invalid-form)
              (_             . ,invalid-form)
              (...           . ,invalid-form))))
       (ex:make-library
        '(core primitive-macros)
        ;; envs
        (lambda () '())
        ;; exports
        (map (lambda (mapping)
               (cons (car mapping) (make-binding 'macro (car mapping) '(0) #f '())))
             primitive-macro-mapping)
        ;; imported-libraries
        '()
        ;; builds
        '()
        ;; visit
        (lambda ()
          (for-each (lambda (mapping)
                      (register-macro! (car mapping) (make-expander (cdr mapping))))
                    primitive-macro-mapping)
          (values))
        ;; invoke
        (lambda () (values))
        ;; build
        'system)))

    ;; Initial environments:

    (set! *toplevel-env* (make-unit-env))
    (set! *usage-env*    *toplevel-env*)

    ;; Import only the minimal library language into the toplevel:

    (env-import! toplevel-template (make-library-language) *toplevel-env*)
    (register-macro! 'define-library (make-expander invalid-form))     ; [R7RS]
    (register-macro! 'library (make-expander invalid-form))
    (register-macro! 'program (make-expander invalid-form))
    (register-macro! 'import  (make-expander invalid-form))
    (register-macro! 'export  (make-expander invalid-form))            ; [R7RS]
    (register-macro! 'include (make-expander invalid-form))            ; [R7RS]
    (register-macro! 'include-ci (make-expander invalid-form))         ; [R7RS]
    (register-macro! 'include-library-declarations                     ; [R7RS]
                     (make-expander invalid-form))                     ; [R7RS]
    (register-macro! 'begin (make-expander invalid-form))              ; [R7RS]
    (register-macro! 'cond-expand                                      ; [R7RS]
                     (make-expander cond-expand-expander))             ; [R7RS]

    ;;==========================================================================
    ;;
    ;; Exports:
    ;;
    ;;==========================================================================

    (set! ex:make-variable-transformer make-variable-transformer)
    (set! ex:identifier?               identifier?)
    (set! ex:bound-identifier=?        bound-identifier=?)
    (set! ex:free-identifier=?         free-identifier=?)
    (set! ex:generate-temporaries      generate-temporaries)
    (set! ex:datum->syntax             datum->syntax)
    (set! ex:syntax->datum             syntax->datum)
    (set! ex:environment               environment)
    (set! ex:environment-bindings      environment-bindings)
    (set! ex:eval                      r6rs-eval)
    (set! ex:load                      r6rs-load)
    (set! ex:syntax-violation          syntax-violation)
    
    (set! ex:expand-file               expand-file)
    (set! ex:repl                      repl)
    (set! ex:expand-r5rs-file          expand-r5rs-file)
    (set! ex:run-r6rs-sequence         run-r6rs-sequence)
    (set! ex:run-r6rs-program          run-r6rs-program)
    (set! ex:interaction-environment   r7rs-interaction-environment)   ; [R7RS]

    (set! ex:invalid-form              invalid-form)
    (set! ex:register-macro!           register-macro!)
    (set! ex:syntax-rename             syntax-rename)
    (set! ex:map-while                 map-while)
    (set! ex:dotted-length             dotted-length)
    (set! ex:dotted-butlast            dotted-butlast)
    (set! ex:dotted-last               dotted-last)
    (set! ex:uncompress                uncompress)
    (set! ex:free=?                    free=?)

    ) ; let
  ) ; letrec-syntax



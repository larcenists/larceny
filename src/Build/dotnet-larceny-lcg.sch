;; This script depends on having the compiler (driver-larceny.sch version),
;; Util/seal-twobit.sch, Asm/IL-LCG/pass5p2.sch, and the Debugger/ files 
;; loaded.
;;
;; It seals off the Twobit compiler internals, installs the debugger, 
;; and replaces the evaluator with one that does CompileOnEval.
 
;; These will be prerequisites of this file at some point in future.

(define (prereq name thunk) (run-benchmark name thunk))
(define (prereq:load name file) (prereq name (lambda () (load file))))
;(define (prereq name thunk) 'nop)
;(define (prereq:load name file) 'nop)

(prereq "DOTNET"
        (lambda () 
          (load "src/Build/dotnet.sch")
          (larceny-setup "Larceny" 'win32 'little)))

(prereq "COMPILER"
        (lambda () 
          (load-compiler 'release)
          ))
(prereq:load "LINK-LOP" "src/Asm/Shared/link-lop.sch")
(prereq "IL-LCG" 
        (lambda () 
          (if #f 
              (peephole-optimization #f) ;; not there yet
              (load "src/Asm/IL-LCG/peepopt.sch"))
          (load "src/Asm/IL-LCG/dotnet-ffi-lcg.sch")
          (load "src/Asm/IL-LCG/pass5p2.sch")))
(prereq:load "TOPLEVEL" 
             (param-filename 'common-source "toplevel.sch"))
(prereq:load "LEVELTGT"
             (param-filename 'source "Arch" "IL" "toplevel-target.sch"))
(prereq:load "SEAL2BIT" 
             "src/Build/seal-twobit.sch")

(define ($$trace x) #f)                 ; Some code uses this

(define toplevel-macro-expand #f)       ; A hack for the benefit of 
                                        ; init-toplevel-environment

(define (displn x) (display x) (newline))

(let ()        
  ;; much of this was cut-and-pasted from iasn-larceny-heap (which in
  ;; turn was largely cut-and-pasted from the sparc version of that
  ;; file).
  ;; 
  ;; There is much trickery that Felix has to puzzle through every
  ;; time he tries to make sense of it.

  ;; snapshot current values of some globals in
  ;; lexical environment procedure below
  (let ((interaction-environment interaction-environment)
        (compile-expression compile-expression)
        (link-lop-segment link-lop-segment)
        (evaluator evaluator)        
        (macro-expand-expression macro-expand-expression) ;; XXX copied from iasn-larceny-heap
        )        

    (define twobit
      (lambda (expr . rest)
        (let ((env (if (null? rest)
                       (interaction-environment)
                       (car rest))))
          ((link-lop-segment (compile-expression expr env) env)))))
  
    (evaluator twobit))

  ;; Now, *before* the newly installed evaluator gets a chance to do
  ;; anything, we need to do some cleanup after ourselves.

  (displn "Install twobit's macro expander as the interpreter's ditto")
  
  (macro-expander (lambda (form environment)
                    (let ((switches (compiler-switches 'get)))
                      (dynamic-wind
                          (lambda ()
                            (compiler-switches 'standard))
                          (lambda ()
                            (twobit-expand
                             form
                             (environment-syntax-environment environment)))
                          (lambda ()
                            (compiler-switches 'set! switches))))))

  ;; Kids, don't try this at home
  
  (vector-like-set! (interaction-environment) 
                    4
                    (the-usual-syntactic-environment))
  )

(let ((proc-names 
       (append 
        
        ;; ;; From Debugger/debug.sch
        ;; '(install-debugger)
        
        ;; From Lib/MzScheme/dotnet.sch and Lib/MzScheme/init.sch
        ;; Javadot support.  Need the procedure to enable it,
        ;; as well as the supporting syntax definition for
        ;; (.javadot FORM); see eval expression below.
        ;; (See dotnet-ffi group as well)
        '(
          dot-javadot-syntax-definition
          )

        ;; Exports
        ;; Felix is just copying everything from lib/MzScheme/init.sch, to ensure that a missing item will not cause the windows.sch demo to fail.  
        ;; XXX Remove items that we should not be exporting!  e.g. PLT-isms
        '(
        weird-printer)

        ;; Miscellaneous
        '(
        add1
        arity-at-least?
        arity-at-least-value
        arity-plus
        constantly
        false
        getarg
        getarg*
        getargs
        get-serial-number
        identity
        sub1
        void
        void?
        make-arity-at-least
        %nary->fixed-arity
        )
        
        ;; (uncommented)
        '(
        %instance)

        ;; instance
        '(
        allocate-instance-state-vector
        instance?
        instance/class
        instance/procedure
        instance/ref
        instance/set!
        instance/serial-number
        instance/update!
        instance/replace!
        %make-instance
        %make-instance*
        %make-entity
        %make-entity*
        set-instance-class-to-self!
        uninitialized-entity-procedure)

        ;; class
        '(
        %class-cpl
        %class-default-initargs
        %class-direct-default-initargs
        %class-direct-slots
        %class-direct-supers
        %class-direct-additional-initargs
        %class-field-initializers
        %class-getters-n-setters
        %class-initializers
        %class-name
        %class-nfields
        %class-slots
        %class-effective-valid-initargs
        %generic-app-cache
        %generic-arity
        %generic-combination
        %generic-methods
        %generic-name
        %generic-singletons-list
        %method-arity
        %method-name
        %method-procedure
        %method-qualifier
        %method-specializers
        %set-class-cpl!
        %set-class-default-initargs!
        %set-class-direct-additional-initargs!
        %set-class-direct-default-initargs!
        %set-class-direct-slots!
        %set-class-direct-supers!
        %set-class-field-initializers!
        %set-class-getters-n-setters!
        %set-class-initializers!
        %set-class-name!
        %set-class-nfields!
        %set-class-slots!
        %set-class-effective-valid-initargs!
        %set-generic-app-cache!
        %set-generic-arity!
        %set-generic-combination!
        %set-generic-methods!
        %set-generic-name!
        %set-generic-singletons-list!
        %set-method-arity!
        %set-method-name!
        %set-method-procedure!
        %set-method-qualifier!
        %set-method-specializers!
        %update-class-effective-valid-initargs!
        *default-object-class*
        <builtin>
        <class>
        <entity-class>
        <function>
        <generic>
        <method>
        <object>
        <primitive-class>
        <procedure-class>
        <top>
        builtin?
        check-initargs
        class-cpl
        class-default-initargs
        class-direct-default-initargs
        class-direct-slots
        class-direct-supers
        class-direct-additional-initargs
        class-field-initializers
        class-getters-n-setters
        class-initializers
        class-name
        class-name-no-angles
        class-nfields
        class-of
        class-predicate
        class-slots
        class-effective-valid-initargs
        class?
        generic-arity
        generic-combination
        generic-methods
        generic-name
        generic?
        instance-of?
        instances-of?
        make
        method-arity
        method-name
        method-procedure
        method-qualifier
        method-specializers
        method:compute-apply-method
        method?
        no-applicable-method
        no-next-method
        nullable
        nullable-value
        nullable?
        object?
        record-type->class
        same-method-signature?
        singleton
        singleton-value
        singleton?
        slot-bound?
        slot-exists?
        slot-makunbound ; sic
        slot-missing
        slot-ref
        slot-set!
        slot-unbound
        slot-update!
        slot-value
        slot-value-if-bound
        struct-type->class
        subclass?
        subclasses-of?
        )

        ;; generic
        '(
        *default-class-class*
        *default-entityclass-class*
        *default-generic-class*
        *default-method-class*
        *make-safely*
        <assignment>
        <begin>
        <bignum>
        <boolean>
;        <box>
;        <break-exn>
        <bytevector>
        <bytevector-like>
        <call>
        <char>
;        <compiled-expression>
        <code-object>
        <complex>
        <conditional>
        <constant>
;        <custodian>
        <end-of-file>
        <exact-complex>
        <exact-integer>
        <exact-rational>
        <exact-real>
        <exact>
;        <exn>
        <fixnum>
        <flonum>
;        <foreign-array>
;        <foreign-boolean>
;        <foreign-enum>
;        <foreign-int32>
;        <foreign-null>
;        <foreign-object>
;        <foreign-string>
        <hash-table>
;        <identifier-syntax>
;        <immutable-nonempty-list>
;        <immutable-pair>
;        <immutable-string>
;        <immutable>
;        <improper-list>
        <inexact-complex>
        <inexact-integer>
        <inexact-rational>
        <inexact-real>
        <inexact>
        <input-port>
;        <input-stream-port>
;        <inspector>
        <integer>
        <interned-symbol>
        <interpreted-expression>
        <interpreted-primitive>
        <interpreted-procedure>
        <lambda>
        <list>
        <namespace>
;        <non-break-exn>
        <nonempty-list>
        <null>
        <number>
        <output-port>
;        <output-stream-port>
        <pair>
;        <parameter>
        <port>
;        <primitive-procedure>
        <primitive-structure>
        <procedure>
;        <promise>
;        <pseudo-random-generator>
        <rational>
        <ratnum>
        <real>
        <rectnum>
        <record>
;        <regexp>
;        <security-guard>
;        <semaphore>
        <sequence>
;        <stream-port>
        <string>
        <struct-type>
;        <subprocess>
        <symbol>
;        <syntax>
;        <tcp-listener>
;        <thread>
        <uninterned-symbol>
        <unknown-primitive>
        <variable>
        <vector>
        <vector-like>
        <void>
;        <weak-box>
;        <will-executor>
        add-method
        allocate-instance
        compute-apply-generic
        compute-apply-method
        compute-apply-methods
        compute-cpl
        compute-default-initargs
        compute-getter-and-setter
        compute-method-more-specific?
        compute-method-more-specific-by-class?
        compute-methods
        compute-methods-by-class
        compute-slots
        extend-generic
        generic-+-combination
        generic-and-combination
        generic-append!-combination
        generic-append-combination
        generic-begin-combination
        generic-combination-cons
        generic-combination-control
        generic-getter
        generic-list-combination
        generic-max-combination
        generic-min-combination
        generic-or-combination
        generic-setter
        generic-updater
        getter-method
        initialize-instance
        initialize-generic-accessors
        make-class
        make-generic
        make-method
        make-generic-combination
        rec-allocate-instance
        rec-initialize
        require-initarg
        setter-method
        updater-method
        )

        ;; gprint
        '(
        named-object-printer-method
        print-object
        print-unreadable-object
        )

        
        ;; dotnet-ffi
        '(
        *dotnet-noise-level*
        allocate-clr-array
        clr-app-domain/%current-domain ;; PROCEDURE
        clr-app-domain/%get-assemblies ;; PROCEDURE
        clr-array->list
        clr-array/length
        clr-assembly/%get-type         ;; PROCEDURE
        clr-binding-flags/instance
        clr-binding-flags/non-public
        clr-binding-flags/public
        clr-binding-flags/static
        clr-convert/%change-type       ;; PROCEDURE
        clr-enum/%get-names            ;; PROCEDURE
        clr-enum/%get-values           ;; PROCEDURE
        clr-enum/get-names
        clr-enum/get-values
        clr-enum/to-object
        clr-field-info/%get-value      ;; PROCEDURE
        clr-fieldinfo/%field-type
        clr-fieldinfo/is-init-only?
        clr-fieldinfo/is-literal?
        clr-fieldinfo/is-static?
        clr-guid/%new-guid             ;; PROCEDURE
        clr-member-type/constructor
        clr-member-type/custom
        clr-member-type/event
        clr-member-type/field
        clr-member-type/method
        clr-member-type/nested-type
        clr-member-type/property
        clr-member-type/type-info
        clr-memberinfo/%declaring-type
        clr-memberinfo/%name
        clr-memberinfo/%reflected-type
        clr-memberinfo/member-type
        clr-memberinfo/name
        clr-methodbase/%get-parameters
        clr-methodbase/is-public?
        clr-methodbase/is-static?
        clr-methodinfo/%return-type
        clr-methodinfo/contains-generic-parameters?
        clr-parameterinfo/%default-value
        clr-parameterinfo/%parameter-type
        clr-parameterinfo/is-optional?
        clr-propertyinfo/%get-get-method
        clr-propertyinfo/%get-index-parameters
        clr-propertyinfo/%property-type
        clr-propertyinfo/can-read?
        clr-propertyinfo/can-write?
        clr-type-handle/scheme-rt-ffi
        clr-type-handle/system-appdomain
        clr-type-handle/system-array
        clr-type-handle/system-boolean
        clr-type-handle/system-byte
        clr-type-handle/system-byte-array
        clr-type-handle/system-char
        clr-type-handle/system-convert
        clr-type-handle/system-double
        clr-type-handle/system-enum
        clr-type-handle/system-guid
        clr-type-handle/system-int16
        clr-type-handle/system-int32
        clr-type-handle/system-int64
        clr-type-handle/system-object
        clr-type-handle/system-reflection-assembly
        clr-type-handle/system-reflection-bindingflags
        clr-type-handle/system-reflection-constructorinfo
        clr-type-handle/system-reflection-emit-constructorbuilder
        clr-type-handle/system-reflection-emit-methodbuilder
        clr-type-handle/system-reflection-fieldinfo
        clr-type-handle/system-reflection-memberinfo
        clr-type-handle/system-reflection-membertypes
        clr-type-handle/system-reflection-methodbase
        clr-type-handle/system-reflection-methodinfo
        clr-type-handle/system-reflection-parameterinfo
        clr-type-handle/system-reflection-propertyinfo
        clr-type-handle/system-sbyte
        clr-type-handle/system-single
        clr-type-handle/system-string
        clr-type-handle/system-type
        clr-type-handle/system-uint16
        clr-type-handle/system-uint32
        clr-type-handle/system-uint64
        clr-type-handle/system-void
        clr-type/%assembly
        clr-type/%assembly-qualified-name
        clr-type/%attributes
        clr-type/%base-type
        clr-type/%full-name
        clr-type/%get-custom-attributes
        clr-type/%get-element-type
        clr-type/%get-interfaces
        clr-type/%get-members
        clr-type/contains-generic-parameters?
        clr-type/get-custom-attributes
        clr-type/is-enum?
        clr-type/is-generic?
        clr-type/is-special-name?
        clr-type/is-generic-parameter?
        clr-type/is-generic-type?

        clr/%type-as-string
        clr/bool->foreign
        clr/false
        clr/flonum->foreign-double
        clr/flonum->foreign-single
        clr/foreign->bool
        clr/foreign->char
        clr/foreign->int
        clr/foreign->schemeobject
        clr/foreign->string
        clr/foreign->symbol
        clr/foreign-double->flonum
        clr/foreign-single->flonum
        clr/byte->foreign
        clr/int->foreign
        clr/char->foreign
        clr/null
        clr/null?
        clr/parse-enum
        clr/string->foreign
        clr/symbol->foreign
        clr/true
        clr/type-not-found
        dotnet-message
        find-clr-type
        map-clr-array
        clr/%foreign-aset
        )

        ;; dotnet
        `(
        <clr-arity-overload>
        <clr-generic>
        <clr-instance-field-getter>
        <clr-instance-field-setter>
        <clr-method>
        <clr-static-field-getter>
        <clr-static-field-setter>
        System.Object
        System.RuntimeType
        System.Type
        argument-marshaler
        argument-specializer
        clr-arity-overload?
        clr-dynamic-cast
        clr-object->clr-instance
        clr-object/clr-handle
        clr-object/potential-types
        clr/StudlyName
        clr/default-marshal-in
        clr/default-marshal-out
        clr/find-class
        clr/find-constructor
        clr/find-generic
        clr/find-instance-field-getter
        clr/find-instance-field-setter
        clr/find-static-field-getter
        clr/find-static-field-setter
        clr/find-static-method
        clr/specific-method
        enable-dotnet!
        enum/enumerates
        enum/has-flags-attribute?
        enum/value
        get-arity-vector
        list-clr-classes
        return-marshaler
        wrap-clr-object
        %foreign?
        )


        '(
        %clr-array?
        %clr-double?
        %clr-enum?
        %clr-char?
        %clr-int32?
        %clr-single?
        %clr-string?
        %clr-type?
        clr/%add-event-handler
        clr/%clr-version
        clr/%eq?
        clr/%ffi-version
        clr/%field-ref
        clr/%field-set!
        clr/%flonum->foreign-double
        clr/%flonum->foreign-single
        clr/%foreign->bytes
        clr/%foreign->int
        clr/%foreign->object
        clr/%foreign->schemeobject
        clr/%foreign->string
        clr/%foreign->void
        clr/%foreign-aref
        clr/%foreign-box
        clr/%foreign-double->flonum
        clr/%foreign-single->flonum
        clr/%get-constructor
        clr/%get-field
        clr/%get-method
        clr/%get-property
        clr/%get-type
        clr/%invoke
        clr/%invoke-constructor
        clr/%isa?
        clr/%null?
        clr/%number->foreign-byte
        clr/%number->foreign-int16
        clr/%number->foreign-int32
        clr/%number->foreign-sbyte
        clr/%number->foreign-uint16
        clr/%number->foreign-uint32
        clr/%object-type
        clr/%procedure->message-filter
        clr/%property-ref
        clr/%property-ref-bool
        clr/%property-ref-int
        clr/%property-ref-intptr-int
        clr/%property-ref-window
        clr/%property-set!
        clr/%string->foreign
        clr/%bytes->foreign
        clr/%to-string
        clr/%load-assembly
        )
        
        standard-proc-names))
      )

  (seal-twobit proc-names))


;;; Load a bunch of useful things.  
;;; FIXME: Some of these files could usefully be loaded in private namespaces.

;;; FIXME: Common Larceny doesn't support heap dumping,
;;; so we can't just load files here and dump the heap.
;;; As a temporary expedient, it's easiest just to copy
;;; certain files into this file:

(define auxiliary-syntax-definitions
  '(begin

;;; FIXME: copied from lib/Base/macros.sch

(define-syntax bound?
  (syntax-rules ()
    ((bound? x)
     (bound? x (interaction-environment)))
    ((bound? ?x ?env)
     (let ((env ?env)
           (name (quote ?x)))
       (or (environment-variable? env name)
           (environment-macro? env name))))))

(define-syntax time
  (syntax-rules ()
    ((time ?expr)
     (run-with-stats (lambda () ?expr)))))

;;; End of copied text.

)) ; end of auxiliary-syntax-definitions


;;; Improve some definitions

(define (procedure-documentation-string p)
  (let ((e (procedure-expression p)))
    (if (and (list? e)
             (> (length e) 2)
             (string? (caddr e))
             (not (null? (cdddr e))))
        (caddr e)
        #f)))

;;; Set parameters to their defaults.

(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

;;; Eval the syntax definitions.

(eval dot-javadot-syntax-definition)
(eval auxiliary-syntax-definitions)

;;; Install debugger.

(install-debugger)
(define install-debugger)

;;; Install pretty printer as default printer.

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))


; It's necessary to set the interaction environment so that any uses of 
; EVAL in the loaded file will reference the correct environment.

(define new-load-eval
  (lambda (expr env)
    (let ((old-env (interaction-environment)))
      (define (literal? x)
        (or (procedure? x) (number? x) (string? x)))
      (dynamic-wind 
          (lambda ()
            (interaction-environment env))
          (lambda ()
            ;; Filter out procedure literals (.fasl files)
            ;; Keep in sync w/ dump-fasl in Asm/IL/dumpheap-extra.sch
            (if (and (pair? expr) 
                     (pair? (car expr))
                     (eq? '@common-patch-procedure (caar expr))
                     (every? literal? (cdar expr)))
                (let ((proc
                       (apply (eval '@common-patch-procedure env)
                              (cdar expr))))
                  (proc))
                (eval/clr expr env)))
          (lambda ()
            (if (eq? (interaction-environment) env)
                (interaction-environment old-env)))))))

(load-evaluator new-load-eval)

; eof

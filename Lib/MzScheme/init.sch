;; $Id$
;;
;; Initialization of MzScheme system.
;; First we need to get larceny going, but without starting the
;; main program. So we redefine go.
;;
;; Files executed after this file are able to use basic larceny
;; functionality, but should not rely on oblist or the reader.

(define (go symlist argv)
  ($$trace "In MzScheme's redefinition of 'go'")
  ($$trace "  installing oblist")
  (oblist-set! symlist)
  ($$trace "  installing reader")
  (install-reader)
  ($$trace "  initializing MzScheme subsystems")
  (for-each (lambda (p) (p)) *mzscheme-subsystem-init-procedures*)
  (set! scheme-entry #f)
  ($$trace "  jumping to 'main'")
  (main argv))

;; *mzscheme-subsystem-init* : (listof (-> void))
;; A list of thunks to be called before starting the main program.
;; Will be called in order given (order matters!)
(define *mzscheme-subsystem-init-procedures* '())

;; larceny-go : -> void
;; Initializes basic Larceny support.
(define (larceny-go)
  ($$trace "Initializing Larceny kernel")
  (install-millicode-support))

(larceny-go)

;; Lists exported names. Make sure you also put the files in
;; Lib/makefile.scm in the dotnet-mzscheme-files list.

(define (export-name name to-environment get-value)
  (let ((probe (syntactic-environment-get usual-syntactic-environment name)))
    (if probe
        (environment-set-macro! to-environment name (usual-syntax name))
        (environment-set! to-environment name (get-value)))))

(define-syntax export
  (syntax-rules ()
    ((_ name ...)
     (set! *larceny-environment-extensions*
           (cons (lambda (env)
                   (export-name 'name env (lambda () name)) ...)
                 *larceny-environment-extensions*)))))

(define (export-syntax . macro-defs)
  (set! *interactive-eval-list*
        (append macro-defs *interactive-eval-list*)))

;; Exports
(export weird-printer)

;; Miscellaneous
(export add1
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

(export %instance)

;; instance
(export allocate-instance-state-vector
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
(export %class-cpl
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
(export *default-class-class*
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
(export named-object-printer-method
        print-object
        print-unreadable-object
        )

;; dotnet-ffi
(export %clr-array?
        %clr-double?
        %clr-enum?
        %clr-int32?
        %clr-single?
        %clr-string?
        %clr-type?
        %foreign?
        *dotnet-noise-level*
        allocate-clr-array
        clr-app-domain/%current-domain
        clr-app-domain/%get-assemblies
        clr-array->list
        clr-array/length
        clr-assembly/%get-type
        clr-binding-flags/instance
        clr-binding-flags/non-public
        clr-binding-flags/public
        clr-binding-flags/static
        clr-convert/%change-type
        clr-enum/%get-names
        clr-enum/%get-values
        clr-enum/get-names
        clr-enum/get-values
        clr-enum/to-object
        clr-field-info/%get-value
        clr-fieldinfo/%field-type
        clr-fieldinfo/is-init-only?
        clr-fieldinfo/is-literal?
        clr-fieldinfo/is-static?
        clr-guid/%new-guid
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
        clr/%foreign-aset
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
        clr/%to-string
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
        clr/int->foreign
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
        )

;; dotnet
(export
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
        )

;; mzscheme-style hashtables
(export hash-table?
        make-hash-table
        hash-table-count
        hash-table-get
        hash-table-put!
        hash-table-remove!
        hash-table-map
        hash-table-for-each)

;; Envaux
(export
        expansion-environment
        <environment-auxinfo>
        current-module-name
        env/extend-reflected!
        env/extend-syntax!
        env/lookup-syntax
        env/module-environment
        env/reflect!
        env/reflected-environments
        env/reify
        env/syntax-environment
        env/embedded-syntax-environment
        make-namespace
        set-env/module-environment!
        set-env/reflected-environments!
        set-env/syntax-environment!
        set-env/embedded-syntax-environment!
        )

;; Compress
(export compress-envs
        uncompress-envs)

;; Identifier
(export *current-meta-rename*
        *no-color*
        *source-color*
        *source-stack*
        *syntax-noise-level*
        <identifier>
        alist-cons
        alist-ref
        bind!
        bind-lexical!
        bind-toplevel!
        binding-name
        bound-identifier=?
        close-environment
        color
        const?
        datum->syntax-object
        datum->syntax-object0
        free-identifier=?
        formals?
        generate-color
        identifier?
        import!
        lexically-bound?
        literal-identifier=?
        make-fluid-identifier
        make-meta-renaming-procedure
        make-renaming-procedure
        map-in-order
        quasi
        scan-let
        symbolic-name
        syntax-object->datum
        syntax-debug
        syntax-trace
        syntax-error
        unbind!
        )

;; MzScheme magic
(export #%app #%datum #%top)

;; inspectors
(export make-inspector
        inspector?
        current-inspector)

;; records
(export *record-type-type*
        make-record-type
        record-type-descriptor?
        record-type-field-names
        record-type-name
        record-type-extends?
        record-type-parent

        record?
        record-constructor
        record-predicate
        record-accessor
        record-updater
        record-type-descriptor

        record-indexer
        record-mutator)

;; structs
(export make-struct-type
        make-struct-type-property
        make-struct-field-accessor
        make-struct-field-mutator

        ;make-wrapped-waitable
        ;make-nack-guard-waitable
        ;make-poll-guard-waitable

        struct?
        struct-type?
        struct-type-property?

        ;struct-info
        struct-type-info
        ;struct->vector

        struct-mutator-procedure?
        struct-accessor-procedure?
        struct-predicate-procedure?
        struct-constructor-procedure?
        )

(define (begin-exports env)
  ($$trace "Exporting environment extensions."))

(set! *larceny-environment-extensions*
      (cons begin-exports *larceny-environment-extensions*))


(export-syntax
 '(define-syntax .javadot
    (transformer
     ((lambda ()

        (define (leading? char string)
          ;; Return #t if the string begins with CHAR.
          (and (>= (string-length string) 1)
               (char=? (string-ref string 0) char)))

        (define (trailing? char string)
          ;; Return #t if the string ends with CHAR.
          (let ((length (string-length string)))
            (and (>= length 1)
                 (char=? (string-ref string (- length 1)) char))))

        (define (trailing2? penultimate ultimate string)
          ;; Return #t if the string ends with the two characters.
          (let ((length (string-length string)))
            (and (> length 2)
                 (char=? (string-ref string (- length 2)) penultimate)
                 (char=? (string-ref string (- length 1)) ultimate))))

        (define (leading-dot? string)
          ;; Return #T if the `string' has a leading dot.
          (leading? #\. string))

        (define (trailing-dollar? string)
          ;; Return #T if the string has a trailing dollar.
          (trailing? #\$ string))

        (define (trailing-dot? string)
          (trailing? #\. string))

        (define (trailing-sharp? string)
          (trailing? #\# string))

        (define (trailing-dot-sharp? string)
          (trailing2? #\. #\# string))

        (define (trailing-dollar-sharp? string)
          (trailing2? #\$ #\# string))

        (define (dot-dollar? string)
          ;; Return #T if the symbol has a leading dot and a trailing
          ;; dollar sign.
          (and (leading-dot? string)
               (trailing-dollar? string)))

        (define (dot-dollar-sharp? string)
          ;; Return #T if the symbol has a leading dot and a trailing
          ;; dollar and sharp sign.
          (and (leading-dot? string)
               (trailing-dollar-sharp? string)))

        (define (set-dollar-excl? string)
          ;; Return #T if the symbol begins with SET-.  and ends
          ;; with $!
          ;; These symbols are created from the SETF! macro.
          ;; Kinda gross.
          (let ((length (string-length string)))
            (and (> length 6)
                 (char=? (string-ref string 0) #\s)
                 (char=? (string-ref string 1) #\e)
                 (char=? (string-ref string 2) #\t)
                 (char=? (string-ref string 3) #\-)
                 (char=? (string-ref string (- length 2)) #\$)
                 (char=? (string-ref string (- length 1)) #\!))))

        (define (set-dollar-sharp-excl? string)
          ;; Return #T if the string begins with SET-.  and ends
          ;; with $#!
          ;; These strings are created from the SETF! macro.
          ;; Kinda gross.
          (let ((length (string-length string)))
            (and (> length 7)
                 (char=? (string-ref string 0) #\s)
                 (char=? (string-ref string 1) #\e)
                 (char=? (string-ref string 2) #\t)
                 (char=? (string-ref string 3) #\-)
                 (char=? (string-ref string (- length 3)) #\$)
                 (char=? (string-ref string (- length 2)) #\#)
                 (char=? (string-ref string (- length 1)) #\!))))

        (define (set-dot-dollar-excl? string)
          ;; Return #T if the symbol begins with SET-.  and ends
          ;; with $!
          ;; These symbols are created from the SETF! macro.
          ;; Kinda gross.
          (let ((length (string-length string)))
            (and (> length 7)
                 (char=? (string-ref string 0) #\s)
                 (char=? (string-ref string 1) #\e)
                 (char=? (string-ref string 2) #\t)
                 (char=? (string-ref string 3) #\-)
                 (char=? (string-ref string 4) #\.)
                 (char=? (string-ref string (- length 2)) #\$)
                 (char=? (string-ref string (- length 1)) #\!))))

        (define (set-dot-dollar-sharp-excl? string)
          ;; Return #T if the string begins with SET-.  and ends
          ;; with $#!
          ;; These strings are created from the SETF! macro.
          ;; Kinda gross.
          (let ((length (string-length string)))
            (and (> length 8)
                 (char=? (string-ref string 0) #\s)
                 (char=? (string-ref string 1) #\e)
                 (char=? (string-ref string 2) #\t)
                 (char=? (string-ref string 3) #\-)
                 (char=? (string-ref string 4) #\.)
                 (char=? (string-ref string (- length 3)) #\$)
                 (char=? (string-ref string (- length 2)) #\#)
                 (char=? (string-ref string (- length 1)) #\!))))

        (define (trailing-suffix? suffix other)
          (let ((suffix-length (string-length suffix))
                (other-length (string-length other)))
            (and (> other-length suffix-length)
                 (let loop ((suffix-scan 0)
                            (other-scan (- other-length suffix-length)))
                   (cond ((= suffix-scan suffix-length) #t)
                         ((char=? (char-downcase (string-ref suffix suffix-scan))
                                  (char-downcase (string-ref other other-scan)))
                          (loop (+ suffix-scan 1) (+ other-scan 1)))
                         (else #f))))))

        (define (trailing-dot-class? string)
          (trailing-suffix? (javadot-type-suffix) string))

        (define (trailing-dot-generic? string)
          (trailing-suffix? (javadot-generic-suffix) string))

        (define (split-on-dots string)
          (let loop ((scan 0)
                     (start 0)
                     (accum '()))
            (cond ((>= scan (string-length string))
                   (reverse (cons (substring string start scan) accum)))
                  ((char=? (string-ref string scan) #\.)
                   (loop (+ scan 1)
                         (+ scan 1)
                         (cons (substring string start scan) accum)))
                  (else (loop (+ scan 1) start accum)))))

        (define (embedded-dot? string)
          ;; return #T if the `string' has embedded dots
          ;; and no leading and trailing dots.
          (let ((length (string-length string)))
            (and (> length 2)
                 (not (char=? (string-ref string 0) #\.))
                 (not (char=? (string-ref string (- length 1)) #\.))
                 (let loop ((scan 1))
                   (cond ((>= scan length) #f)
                         ((char=? (string-ref string scan) #\.) #t)
                         (else (loop (+ scan 1))))))))

        (define (trim prefix string suffix)
          ;; return the string without the prefix and suffix
          (substring string
                     (string-length prefix)
                     (- (string-length string)
                        (string-length suffix))))

        (define (transform exp rename compare)
          (let* ((exp (cadr exp))
                 (text (symbol->string exp)))
            (cond ((dot-dollar? text)
                   `(,(rename 'clr/find-instance-field-getter)
                     #f ;; public only
                     ,@(map (lambda (fragment)
                              (list (rename 'quote) (string->symbol fragment)))
                            (split-on-dots (trim "." text "$")))))

                  ((dot-dollar-sharp? text)
                   `(,(rename 'clr/find-instance-field-getter)
                     #t ;; allow private
                     ,@(map (lambda (fragment)
                              (list (rename 'quote) (string->symbol fragment)))
                            (split-on-dots (trim "." text "$#")))))

                  ((set-dot-dollar-excl? text)
                   `(,(rename 'clr/find-instance-field-setter)
                     #f ;; public only
                     ,@(map (lambda (fragment)
                              (list (rename 'quote) (string->symbol fragment)))
                            (split-on-dots (trim "set-." text "$!")))))

                  ((set-dot-dollar-sharp-excl? text)
                   `(,(rename 'clr/find-instance-field-setter)
                     #t ;; allow private
                     ,@(map (lambda (fragment)
                              (list (rename 'quote) (string->symbol fragment)))
                            (split-on-dots (trim "set-." text "$#!")))))

                  ((leading-dot? text)
                   (if (trailing-sharp? text)
                       `(,(rename 'clr/find-generic)
                         #t ;; allow private
                         (,(rename 'quote)
                          ,(string->symbol (trim "." text "#"))))
                       `(,(rename 'clr/find-generic)
                         #f ;; public only
                         (,(rename 'quote)
                          ,(string->symbol (trim "." text ""))))))

                  ((trailing-dot-sharp? text)
                   `(,(rename 'clr/find-constructor)
                     #t ;; allow private
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text ".#")))))

                  ((trailing-dot? text)
                   `(,(rename 'clr/find-constructor)
                     #f ;; public only
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text ".")))))

                  ((and (set-dollar-sharp-excl? text) (embedded-dot? text))
                   `(,(rename 'clr/find-static-field-setter)
                     #t ;; allow private
                     (,(rename 'quote)
                      ,(string->symbol (trim "set-" text "$#!")))))

                  ((and (set-dollar-excl? text) (embedded-dot? text))
                   `(,(rename 'clr/find-static-field-setter)
                     #f ;; public only
                     (,(rename 'quote)
                      ,(string->symbol (trim "set-" text "$!")))))

                  ((and (trailing-dollar-sharp? text) (embedded-dot? text))
                   `(,(rename 'clr/find-static-field-getter)
                     #t ;; allow private
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text "$#")))))

                  ((and (trailing-dollar? text) (embedded-dot? text))
                   `(,(rename 'clr/find-static-field-getter)
                     #f ;; public only
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text "$")))))

                  ((trailing-dot-class? text)
                   `(,(rename 'clr/find-class)
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text (javadot-type-suffix))))))

                  ((trailing-dot-generic? text)
                   `(,(rename 'clr/find-parameterized-class)
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text (javadot-generic-suffix))))))

                  ((embedded-dot? text)
                   (if (trailing-sharp? text)
                       `(,(rename 'clr/find-static-method)
                         #t
                         (,(rename 'quote)
                          ,(string->symbol (trim "" text "#"))))
                       `(,(rename 'clr/find-static-method)
                         #f
                         (,(rename 'quote) ,(string->symbol text)))))

                  (else (error "Bad javadot identifier?" exp)))))

        transform)))))

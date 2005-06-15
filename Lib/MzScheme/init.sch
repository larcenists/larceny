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

(define-syntax export
  (syntax-rules ()
    ((_ name ...)
     (set! *larceny-environment-extensions*
           (cons (lambda (env) (environment-set! env 'name name) ...)
                 *larceny-environment-extensions*)))))

(define (export-syntax . macro-defs)
  (set! *interactive-eval-list*
        (append macro-defs *interactive-eval-list*)))

;; Exports
(export weird-printer)

;; Miscellaneous
(export add1
        arg-check
        arity-at-least?
        arity-at-least-value
        arity-plus
        constantly
        false
        generate-id
        getarg
        getarg*
        getargs
        get-serial-number
        identity
        sub1
        vmap
        vfor-each
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
        compute-methods
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
        more-specific?
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

;; continuation marks
(export call-with-continuation-mark
        current-continuation-marks
        continuation-marks
        continuation-marks/structure
        continuation-mark-set->list)

;; mzscheme-style hashtables
(export hash-table?
        make-hash-table
        hash-table-count
        hash-table-get
        hash-table-put!
        hash-table-remove!
        hash-table-map
        hash-table-for-each)

;; upcoming mzscheme objects
;(export <box>
;        <bucket-table-with-home>
;        <bucket-table>
;        <bucket-with-flags>
;        <bucket-with-home>
;        <bucket>
;        <scheme-small-object>
;        bucket-table/for-each
;        bucket-table/size
;        bucket/flags
;        bucket/home
;        bucket/id
;        bucket/key
;        bucket/val
;        box/val
;        box?
;        get-bucket
;        make-default-bucket
;        ptr-val
;        scheme-add-to-table!
;        scheme-bucket-from-table
;        scheme-bucket-or-null-from-table
;        scheme-lookup-in-table
;        set-bucket/flags!
;        set-bucket/home!
;        set-bucket/id!
;        set-bucket/val!
;        set-box/val!
;        set-ptr-val!
;        update-bucket/flags!

;        module-renames/plus-kernel?
;        set-module-renames/plus-kernel?!
;        module-renames/phase
;        module-renames/plus-kernel-nominal-source
;        set-module-renames/plus-kernel-nominal-source!
;        module-renames/ht
;        module-renames/marked-names
;        set-module-renames/marked-names!
;        <module-renames>
;        module-renames?
;        <normal-module-renames>
;        normal-module-renames?
;        <marked-module-renames>
;        marked-module-renames?
;        <toplevel-module-renames>
;        toplevel-module-renames?
;        module-renames/put!
;        module-renames/get

;        <stx-srcloc>
;        stx/flags
;        stx/val

;        set-stx/val!
;        stx/srcloc
;        stx/wraps
;        set-stx/wraps!
;        stx/certs
;        set-stx/certs!
;        stx/props
;        set-stx/props!
;        stx/modinfo-cache
;        set-stx/modinfo-cache!
;        stx/lazy-prefix
;        set-stx/lazy-prefix!
;        <stx>
;        stx?
;        scheme-stx-null?
;        scheme-stx-pair?
;        scheme-stx-symbol?
;        scheme-stx-sym

;        modidx/path
;        set-modidx/path!
;        modidx/base
;        set-modidx/base!
;        modidx/resolved
;        set-modidx/resolved!
;        modidx/shift-cache
;        set-modidx/shift-cache!
;        modidx/cache-next
;        set-modidx/cache-next!
;        <modidx>
;        modidx?

;        module-variable/modidx
;        set-module-variable/modidx!
;        module-variable/sym
;        set-module-variable/sym!
;        module-variable/insp
;        set-module-variable/insp!
;        module-variable/pos
;        set-module-variable/pos!
;        module-variable/mod-phase
;        set-module-variable/mod-phase!

;        <module-variable>
;        module-variable?

;        prefix/num-toplevels
;        set-prefix/num-toplevels!
;        prefix/num-stxes
;        set-prefix/num-stxes!
;        prefix/toplevels
;        set-prefix/toplevels!
;        prefix/stxes
;        set-prefix/stxes!
;        <comp-prefix>
;        <resolve-prefix>

;        <local>
;        local/position
;        toplevel/depth
;        toplevel/position
;        <toplevel>
;        <compiled-toplevel>

;        compile-expand-info/certs
;        set-compile-expand-info/certs!
;        compile-expand-info/value-name
;        set-compile-expand-info/value-name!
;        <compile-expand-info>
;        compile-info/dont-mark-local-use
;        set-compile-info/dont-mark-local-use!
;        compile-info/max-let-depth
;        set-compile-info/max-let-depth!
;        compile-info/resolve-module-ids
;        set-compile-info/resolve-module-ids!
;        <compile-info>
;        compile-info?
;        expand-info/depth
;        set-expand-info/depth!
;        <expand-info>
;        expand-info?

;        )

;; wrap-pos
;(export <wrap-chunk>
;        <wrap-pos>
;        do-wrap-pos-inc!
;        set-wrap-chunk/a!
;        set-wrap-chunk/len!
;        set-wrap-pos-end!
;        wrap-chunk/a
;        wrap-chunk/len
;        wrap-chunk?
;        wrap-pos/a
;        wrap-pos/copy
;        wrap-pos/end?
;        wraps->wrap-pos

;        do-wrap-pos-revinit
;        )

;; graph and placeholder resolution
;(export <placeholder>
;        placeholder/value
;        placeholder?
;        resolve-placeholders
;        set-placeholder/value!
;        setup-datum-graph
;        )

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
                 (text (symbol->string (javadot-symbol->symbol exp))))
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

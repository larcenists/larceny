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
  ($$trace "  initalizing MzScheme subsystems")
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

;; Miscellaneous
(export arity-at-least?
        arity-at-least-value
        arity-plus
        getarg
        getarg*
        getargs
        get-serial-number
        identity
        make-arity-at-least)

(export %instance)

;; instance
(export instance?
        instance/class
        instance/procedure
        instance/ref
        instance/set!
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
        %class-field-initializers
        %class-getters-n-setters
        %class-initializers
        %class-name
        %class-nfields
        %class-slots
        %class-valid-initargs
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
        %set-class-direct-default-initargs!
        %set-class-direct-slots!
        %set-class-direct-supers!
        %set-class-field-initializers!
        %set-class-getters-n-setters!
        %set-class-initializers!
        %set-class-name!
        %set-class-nfields!
        %set-class-slots!
        %set-class-valid-initargs!
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
        *default-object-class*
        <builtin>
        <class>
        <entity-class>
        <function>
        <generic>
        <method>
        <object>
        <opaque-struct>
        <primitive-class>
        <procedure-class>
        <struct>
        <top>
        builtin?
        check-initargs
        class-cpl
        class-default-initargs
        class-direct-default-initargs
        class-direct-slots
        class-direct-supers
        class-field-initializers
        class-getters-n-setters
        class-initializers
        class-name
        class-nfields
        class-of
        class-slots
        class-valid-initargs
        class?
        function?
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
        slot-value
        struct-type->class
        subclass?
        )

;; generic
(export *default-class-class*
        *default-entityclass-class*
        *default-generic-class*
        *default-method-class*
        *make-safely*
        <boolean>
        <box>
        <break-exn>
        <char>
        <compiled-expression>
        <complex>
        <custodian>
        <end-of-file>
        <exact-complex>
        <exact-integer>
        <exact-rational>
        <exact-real>
        <exact>
        <exn>
        <foreign-array>
        <foreign-boolean>
        <foreign-enum>
        <foreign-int32>
        <foreign-null>
        <foreign-object>
        <foreign-string>
        <hash-table>
        <identifier-syntax>
        <immutable-nonempty-list>
        <immutable-pair>
        <immutable-string>
        <immutable>
        <improper-list>
        <inexact-complex>
        <inexact-integer>
        <inexact-rational>
        <inexact-real>
        <inexact>
        <input-port>
        <input-stream-port>
        <inspector>
        <integer>
        <list>
        <namespace>
        <non-break-exn>
        <nonempty-list>
        <null>
        <number>
        <output-port>
        <output-stream-port>
        <pair>
        <parameter>
        <port>
        <primitive-procedure>
        <procedure>
        <promise>
        <pseudo-random-generator>
        <rational>
        <real>
        <regexp>
        <security-guard>
        <semaphore>
        <sequence>
        <stream-port>
        <string>
        <struct-type>
        <subprocess>
        <symbol>
        <syntax>
        <tcp-listener>
        <thread>
        <unknown-primitive>
        <vector>
        <void>
        <weak-box>
        <will-executor>
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
        generic-+-combination
        generic-and-combination
        generic-append!-combination
        generic-append-combination
        generic-begin-combination
        generic-combination-cons
        generic-combination-control
        generic-list-combination
        generic-max-combination
        generic-min-combination
        generic-or-combination
        initialize-instance
        make-generic-combination
        more-specific?
        print-object
        rec-allocate-instance
        rec-initialize
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
        hash-table-get
        hash-table-put!
        hash-table-remove!
        hash-table-map
        hash-table-for-each)

;; inspectors
(export make-inspector
        inspector?
        current-inspector)

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

        (define (trailing-dot-class? string)
          (let ((length (string-length string)))
            (and (> length 6)
                 (char=? (string-ref string (- length 6)) #\.)
                 (char=? (string-ref string (- length 5)) #\c)
                 (char=? (string-ref string (- length 4)) #\l)
                 (char=? (string-ref string (- length 3)) #\a)
                 (char=? (string-ref string (- length 2)) #\s)
                 (char=? (string-ref string (- length 1)) #\s))))

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

                  ((trailing-dollar-sharp? text)
                   `(,(rename 'clr/find-static-field)
                     #t ;; allow private
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text "$#")))))

                  ((trailing-dollar? text)
                   `(,(rename 'clr/find-static-field)
                     #f ;; public only
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text "$")))))

                  ((trailing-dot-class? text)
                   `(,(rename 'clr/find-class)
                     (,(rename 'quote)
                      ,(string->symbol (trim "" text ".class")))))

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

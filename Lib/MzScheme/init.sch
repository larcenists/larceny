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
        ;struct-type-info
        ;struct->vector

        struct-mutator-procedure?
        struct-accessor-procedure?
        struct-predicate-procedure?
        struct-constructor-procedure?
        )

(export-syntax
 '(define-syntax .javadot
    (transformer
     (lambda (exp rename compare)
       (let ((exp (cadr exp)))
         (display "inside .javadot: ") (write exp) (newline)
         (list (rename 'dotnet-mumble) (javadot-symbol->symbol exp)))))))

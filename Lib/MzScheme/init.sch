;; $Id$
;; Lists exported names. Make sure you also put the files in 
;; Lib/makefile.scm in the dotnet-mzscheme-files list.

(define-syntax export
  (syntax-rules ()
    ((_ name ...)
     (let ((old initialize-larceny-environment-target-specific))
       (set! initialize-larceny-environment-target-specific
             (lambda (larc)
               (environment-set! larc 'name name) ...
               (old larc)))))))

;; Exports

;; continuation marks
(export call-with-continuation-mark
        current-continuation-marks
        continuation-marks
        continuation-marks/structure
        continuation-mark-set->list)

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


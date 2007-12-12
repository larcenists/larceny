;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records, syntactic layer.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records syntactic)

  (export define-record-type)

  (import (for (core primitives) run expand)
          (for (rnrs base) run expand)
          (for (rnrs lists) run expand)
          (err5rs records procedural))

  (define-syntax define-record-type
    (syntax-rules ()
     ((_ (type-name parent) constructor-spec predicate-spec . field-specs)
      (define-record-type-helper0
       type-name parent constructor-spec predicate-spec . field-specs))
     ((_ type-name constructor-spec predicate-spec . field-specs)
      (define-record-type-helper0
       type-name #f constructor-spec predicate-spec . field-specs))))

  (define-syntax define-record-type-helper0
    (lambda (x)
      (define (complain)
        (syntax-violation 'define-record-type "illegal syntax" x))
      (syntax-case x ()
       ((_ tname pname constructor-spec predicate-spec . field-specs)
        (let* ((type-name (syntax->datum #'tname))
               (parent (syntax->datum #'pname))
               (cspec (syntax->datum #'constructor-spec))
               (pspec (syntax->datum #'predicate-spec))
               (fspecs (syntax->datum #'field-specs))
               (type-name-string
                (begin (if (not (symbol? type-name))
                           (complain))
                       (symbol->string type-name)))
               (constructor-name
                (cond ((eq? cspec #f)
                       #f)
                      ((eq? cspec #t)
                       (string->symbol
                        (string-append "make-" type-name-string)))
                      ((symbol? cspec)
                       cspec)
                      ((pair? cspec)
                       (car cspec))
                      (else (complain))))
               (constructor-args
                (cond ((pair? cspec)
                       (if (not (for-all symbol? cspec))
                           (complain)
                           (list->vector (cdr cspec))))
                      (else #f)))
               (predicate-name
                (cond ((eq? pspec #f)
                       #f)
                      ((eq? pspec #t)
                       (string->symbol
                        (string-append type-name-string "?")))
                      ((symbol? pspec)
                       pspec)
                      (else (complain))))
               (field-specs
                (map (lambda (fspec)
                       (cond ((symbol? fspec)
                              (list 'immutable
                                    fspec
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string fspec)))))
                             ((not (pair? fspec))
                              (complain))
                             ((not (list? fspec))
                              (complain))
                             ((not (for-all symbol? fspec))
                              (complain))
                             ((null? (cdr fspec))
                              (list 'mutable
                                    (car fspec)
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string (car fspec))))
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string (car fspec))
                                      "-set!"))))
                             ((null? (cddr fspec))
                              (list 'immutable
                                    (car fspec)
                                    (cadr fspec)))
                             ((null? (cdddr fspec))
                              (cons 'mutable fspec))
                             (else (complain))))
                     fspecs))
  
               (fields (list->vector (map cadr field-specs)))
  
               (accessor-fields
                (map (lambda (x) (list (caddr x) (cadr x)))
                     (filter (lambda (x) (>= (length x) 3))
                             field-specs)))
  
               (mutator-fields
                (map (lambda (x) (list (cadddr x) (cadr x)))
                     (filter (lambda (x) (= (length x) 4))
                             field-specs))))
  
          (datum->syntax
           #'tname
           `(,#'define-record-type-helper
             ,type-name ,fields ,parent
             ,(if constructor-args
                  (list constructor-name constructor-args)
                  constructor-name)
             ,predicate-name
             ,accessor-fields ,mutator-fields)))))))
  
  (define-syntax define-record-type-helper
    (syntax-rules ()
  
     ((_ type-name fields parent #f predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (define-record-type-helper
       type-name fields parent ignored predicate
       ((accessor field) ...) ((mutator mutable-field) ...)))
  
     ((_ type-name fields parent constructor #f
         ((accessor field) ...) ((mutator mutable-field) ...))
      (define-record-type-helper
       type-name fields parent constructor ignored
       ((accessor field) ...) ((mutator mutable-field) ...)))
  
     ((_ type-name fields parent (constructor args) predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (begin (define type-name (make-rtd 'type-name 'fields parent))
             (define constructor (rtd-constructor type-name 'args))
             (define predicate (rtd-predicate type-name))
             (define accessor (rtd-accessor type-name 'field))
             ...
             (define mutator (rtd-mutator type-name 'mutable-field))
             ...))
  
     ((_ type-name fields parent constructor predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (begin (define type-name (make-rtd 'type-name 'fields parent))
             (define constructor (rtd-constructor type-name))
             (define predicate (rtd-predicate type-name))
             (define accessor (rtd-accessor type-name 'field))
             ...
             (define mutator (rtd-mutator type-name 'mutable-field))
             ...))))

  ) ; err5rs records syntactic


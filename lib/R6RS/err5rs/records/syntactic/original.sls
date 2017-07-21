(library (err5rs records syntactic original)

  (export define-record-type)

  (import (for (core primitives) run expand)
          (for (except (rnrs base)
                       let-syntax
                       letrec-syntax)
               run expand)
          (for (rnrs lists) run expand)
          (for (only (rnrs syntax-case) quasisyntax unsyntax) run expand)
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

      ;; Fix for ticket #633, based on similar code for
      ;; (rnrs records syntactic).

      (define (construct-record-type-definition
               tname
               fields
               parent
               constructor-name
               constructor-args
               predicate-name
               accessor-fields
               mutator-fields)
        (let ()

          (define (frob x)
            (cond ((identifier? x)
                   x)
                  ((pair? x)
                   (cons (frob (car x)) (frob (cdr x))))
                  (else
                   (datum->syntax tname x))))

          #`(#,(frob #'define-record-type-helper)
             #,(frob tname)
             #,(frob fields)
             #,(frob parent)
             #,(if constructor-args
                   (list (frob constructor-name) (frob constructor-args))
                   (frob constructor-name))
             #,(frob predicate-name)
             #,(frob accessor-fields)
             #,(frob mutator-fields))))

      ; Searches for a clause beginning with the given symbol,
      ; returning the entire clause (as a syntax object) if found
      ; or #f if no such clause is found.

      (define (clauses-assq sym clauses)
        (syntax-case clauses ()
         (((x1 x2 ...) y ...)
          (if (and (identifier? #'x1)
                   (eq? sym (syntax->datum #'x1)))
              #'(x1 x2 ...)
              (clauses-assq sym #'(y ...))))
         ((y0 y1 y2 ...)
          (clauses-assq sym #'(y1 y2 ...)))
         (x
          #f)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object for its first element.

      (define (syntax-car x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'x0)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object obtained by omitting the first
      ; element of that list.

      (define (syntax-cdr x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'(x1 ...))))

      ; Given a syntax object that represents a list,
      ; returns a list of syntax objects representing its elements.

      (define (syntax-list x)
        (syntax-case x ()
         (()
          '())
         ((x0 x1 ...)
          (cons #'x0 (syntax-list #'(x1 ...))))))

      ; Given a syntax object that represents a list,
      ; returns a vector of syntax objects representing its elements.

      (define (syntax-list->vector x)
        (syntax-case x ()
         ((x0 ...)
          (list->vector (syntax-list #'(x0 ...))))))

      (define (complain)
        (syntax-violation 'define-record-type "illegal syntax" x))

      (syntax-case x ()
       ((_ tname pname constructor-spec predicate-spec . field-specs)
        (let* ((type-name (syntax->datum #'tname))
               (parent (syntax->datum #'pname))
               (cspec (syntax->datum #'constructor-spec))
               (pspec (syntax->datum #'predicate-spec))
               (fspecs (syntax-list #'field-specs))
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
                       #'constructor-spec)
                      ((pair? cspec)
                       (syntax-car #'constructor-spec))
                      (else (complain))))
               (constructor-args
                (cond ((pair? cspec)
                       (if (not (for-all symbol? cspec))
                           (complain)
                           (syntax-list->vector
                            (syntax-cdr #'constructor-spec))))
                      (else #f)))
               (predicate-name
                (cond ((eq? pspec #f)
                       #f)
                      ((eq? pspec #t)
                       (string->symbol
                        (string-append type-name-string "?")))
                      ((symbol? pspec)
                       #'predicate-spec)
                      (else (complain))))

               ;; field-specs is a list of lists of these forms:
               ;;     (immutable <identifier> <symbol>)
               ;;     (immutable <identifier> <identifier>)
               ;;     (mutable   <identifier> <symbol> <symbol>)
               ;;     (mutable   <identifier> <identifier> <identifier>)
  
               (field-specs
                (map (lambda (fspec)
                       (cond ((identifier? fspec)
                              (list 'immutable
                                    fspec
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string
                                       (syntax->datum fspec))))))
                             ((not (pair? (syntax->datum fspec)))
                              (complain))
                             ((not (list? (syntax->datum fspec)))
                              (complain))
                             ((not (for-all identifier? (syntax-list fspec)))
                              (complain))
                             ((null? (cdr (syntax-list fspec)))
                              (let* ((field-name (syntax-car fspec))
                                     (field-name-as-string
                                      (symbol->string
                                       (syntax->datum field-name))))
                                (list 'mutable
                                      (syntax-car fspec)
                                      (string->symbol
                                       (string-append
                                        type-name-string
                                        "-"
                                        field-name-as-string))
                                      (string->symbol
                                       (string-append
                                        type-name-string
                                        "-"
                                        field-name-as-string
                                        "-set!")))))
                             ((null? (syntax-cdr (syntax-cdr fspec)))
                              (list 'immutable
                                    (syntax-car fspec)
                                    (syntax-car (syntax-cdr fspec))))
                             ((null? (syntax-cdr
                                      (syntax-cdr (syntax-cdr fspec))))
                              (cons 'mutable fspec))
                             (else (complain))))
                     fspecs))

               ;; fields is a vector of list of the forms
               ;;     (immutable <identifier>)
               ;;     (mutable   <identifier>)

               (fields (list->vector
                        (map (lambda (x)
                               (let ((field-name (syntax-car (syntax-cdr x)))
                                     (mutable? (= 4 (length (syntax-list x)))))
                                 (list (if mutable? 'mutable 'immutable)
                                       field-name)))
                             field-specs)))
  
               ;; accessor-fields is a list of lists of these forms:
               ;;     (<symbol> <identifier>)
               ;;     (<identifier> <identifier>)
               ;; where the first element of each list names an accessor

               (accessor-fields
                (map (lambda (x)
                       (let ((x (syntax-list x)))
                         (list (caddr x) (cadr x))))
                     (filter (lambda (x) (>= (length (syntax-list x)) 3))
                             field-specs)))
  
               ;; mutator-fields is a list of lists of these forms:
               ;;     (<symbol> <identifier>)
               ;;     (<identifier> <identifier>)
               ;; where the first element of each list names a mutator

               (mutator-fields
                (map (lambda (x)
                       (let ((x (syntax-list x)))
                         (list (cadddr x) (cadr x))))
                     (filter (lambda (x) (= (length (syntax-list x)) 4))
                             field-specs))))
  
          (construct-record-type-definition
           #'tname
           fields
           parent
           constructor-name
           constructor-args
           predicate-name
           accessor-fields
           mutator-fields))))))
  
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

  ) ; err5rs records syntactic original

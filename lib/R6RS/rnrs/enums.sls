(library (rnrs enums)

  (export make-enumeration
          enum-set-universe
          enum-set-indexer
          enum-set-constructor
          enum-set->list
          enum-set-member?
          enum-set-subset?
          enum-set=?
          enum-set-union
          enum-set-intersection
          enum-set-difference
          enum-set-complement
          enum-set-projection
          define-enumeration)

  (import
   (for (rnrs base)        run expand)
   (for (rnrs syntax-case) run expand)
   (for (rnrs lists)       run expand)
   (primitives
    make-enumeration
    enum-set-universe
    enum-set-indexer
    enum-set-constructor
    enum-set->list
    enum-set-member?
    enum-set-subset?
    enum-set=?
    enum-set-union
    enum-set-intersection
    enum-set-difference
    enum-set-complement
    enum-set-projection))

  (define-syntax define-enumeration
    (syntax-rules ()
     ((_ type-name (symbol1 ...) set-constructor-syntax)
      (begin (define-syntax type-name
               (lambda (x)
                 (define (complain)
                   (syntax-violation 'type-name "illegal symbol" x))
                 (syntax-case x ()
                  ((_ y)
                   (let ((sym1 (syntax->datum #'y)))
                     (if (memq sym1 '(symbol1 ...))
                         #''y
                         (complain)))))))
             (define hidden-name (make-enumeration '(symbol1 ...)))
             (define-syntax set-constructor-syntax
               (syntax-rules ()
                ((_ sym1 (... ...))
                 ((enum-set-constructor hidden-name)
                  (list (type-name sym1) (... ...))))))))))

  ) ; rnrs enums



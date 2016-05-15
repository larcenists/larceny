(library (rnrs control (6))
  (export when unless do case-lambda)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand)
          (for (only (core derived) case else) run)
          (for (primitives not map length assertion-violation = >= apply
                           eq? make-case-lambda)
            expand run) )
  
  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))
  
  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))
  
  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
        ((_ ((var init . step) ...) (e0 e1 ...) c ...)
         (with-syntax (((step ...)
                        (map (lambda (v s)
                               (syntax-case s ()
                                 (()  v)
                                 ((e) (syntax e))
                                 (_   (syntax-violation
                                       'do "Invalid step" orig-x s))))
                             (syntax (var ...))
                             (syntax (step ...)))))
           (syntax-case (syntax (e1 ...)) ()
             (()          (syntax (let do ((var init) ...)
                                    (if (not e0)
                                        (begin c ... (do step ...))))))
             ((e1 e2 ...) (syntax (let do ((var init) ...)
                                    (if e0
                                        (begin e1 e2 ...)
                                        (begin c ... (do step ...))))))))))))
  
  ;; Bummed for Larceny.
  ;; FIXME: reverts to the original general case if any clause
  ;;     uses a rest argument
  ;;  or has more than 3 formal parameters
  ;;         (the 3 is determined by ARM version's REG0 through REG5)

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ clause1)
       (case-lambda-for-general-case clause1))
      ((_ clause1 clause2 ...)
       (case-lambda-prepass () clause1 clause2 ...))))

  (define-syntax case-lambda-prepass
    (syntax-rules ()
      ((_ (c1 ...) (() b1 b2 ...) clause2 ...)
       (case-lambda-prepass (c1 ... (() b1 b2 ...)) clause2 ...))
      ((_ (c1 ...) ((x1) b1 b2 ...) clause2 ...)
       (case-lambda-prepass (c1 ... ((x1) b1 b2 ...)) clause2 ...))
      ((_ (c1 ...) ((x1 x2) b1 b2 ...) clause2 ...)
       (case-lambda-prepass (c1 ... ((x1 x2) b1 b2 ...)) clause2 ...))
      ((_ (c1 ...) ((x1 x2 x3) b1 b2 ...) clause2 ...)
       (case-lambda-prepass (c1 ... ((x1 x2 x3) b1 b2 ...)) clause2 ...))
      ((_ (c1 ...) clause1 clause2 ...)
       (case-lambda-for-general-case c1 ... clause1 clause2 ...))
      ((_ (c1 ...))
       (make-case-lambda
        (lambda (x1 x2 x3 n)
          (case-lambda-dispatch (x1 x2 x3 n) c1 ...))))))

  ;; FIXME: this is temporary, just for testing the concept
#;
  (define (make-case-lambda proc)
    (lambda args
      (let ((n (length args)))
        (case n
         ((0) (proc 0 0 0 0 n))
         ((1) (proc (car args) 0 0 0 n))
         ((2) (proc (car args) (cadr args) 0 0 n))
         ((3) (proc (car args) (cadr args) (caddr args) 0 n))
         (else
          (assertion-violation 'case-lambda "bug in case-lambda" n))))))

  (define-syntax case-lambda-dispatch
    (syntax-rules ()
      ((_ (x1 x2 x3 n) (() b1 b2 ...) c2 ...)
       (if (eq? n 0)
           (let () b1 b2 ...)
           (case-lambda-dispatch (x1 x2 x3 n) c2 ...)))
      ((_ (x1 x2 x3 n) ((y1) b1 b2 ...) c2 ...)
       (if (eq? n 1)
           (let ((y1 x1)) b1 b2 ...)
           (case-lambda-dispatch (x1 x2 x3 n) c2 ...)))
      ((_ (x1 x2 x3 n) ((y1 y2) b1 b2 ...) c2 ...)
       (if (eq? n 2)
           (let ((y1 x1) (y2 x2)) b1 b2 ...)
           (case-lambda-dispatch (x1 x2 x3 n) c2 ...)))
      ((_ (x1 x2 x3 n) ((y1 y2 y3) b1 b2 ...) c2 ...)
       (if (eq? n 3)
           (let ((y1 x1) (y2 x2) (y3 x3)) b1 b2 ...)
           (case-lambda-dispatch (x1 x2 x3 n) c2 ...)))
      ((_ (x1 x2 x3 n))
       (assertion-violation #f "unexpected number of arguments"))))

  (define-syntax case-lambda-for-general-case
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
         (let ((n (length args)))
           (case-lambda-help args n
                             (fmls b1 b2 ...) ...))))))
  
  (define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (assertion-violation #f "unexpected number of arguments"))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (= n (length '(x ...)))
           (apply (lambda (x ...) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (>= n (length '(x1 x2 ...)))
           (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                  args)
           (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args))))
  
  ) ; rnrs control                                      


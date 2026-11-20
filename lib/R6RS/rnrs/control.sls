(library (rnrs control (6))
  (export when unless do case-lambda)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand)
          (for (only (core derived) case else) run)
          (for (primitives not map case-lambda:make)
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
  
  ;; A case-lambda of several clauses is a procedure whose code
  ;; dispatches on the number of arguments to one of the clauses,
  ;; which are ordinary procedures, without consing (see
  ;; .make-case-lambda in src/Lib/Common/make-case-lambda.sch).
  ;; (case-lambda:make is .make-case-lambda: an identifier cannot
  ;; begin with a dot in R6RS, whose reader reads this file to find
  ;; the library in it.)

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (case-lambda:make '(fmls ...) (lambda fmls b1 b2 ...) ...))))
  
  ) ; rnrs control                                      


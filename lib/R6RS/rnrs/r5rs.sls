(library (rnrs r5rs (6))
  
  (export null-environment scheme-report-environment delay force
          exact->inexact inexact->exact quotient remainder modulo)
  
  (import (primitives exact->inexact inexact->exact quotient remainder modulo)
          (rnrs eval)
          (rnrs base)
          (rnrs control))
  
  (define (scheme-report-environment n)
    (unless (= n 5)
      (assertion-violation 'scheme-report-environment
                           "Argument should be 5" n))
    (environment '(r5rs)))
  
  (define null-environment
    (let ((null-env
           (environment '(only (rnrs base)
                           begin if lambda quote set! and or
                           define define-syntax let-syntax letrec-syntax 
                           let let* letrec
                           case cond else =>
                           quasiquote unquote unquote-splicing
                           syntax-rules ...)
                        '(only (rnrs control) do))))
      (lambda (n)
        (unless (= n 5)
          (assertion-violation 'null-environment "Argument should be 5" n))
        null-env)))
  
  (define force
    (lambda (object)
      (object)))
  
  (define-syntax delay
    (syntax-rules ()
      ((delay expression)
       (make-promise (lambda () expression)))))
  
  (define make-promise
    (lambda (proc)
      (let ((result-ready? #f)
            (result #f))
        (lambda ()
          (if result-ready?
              result
              (let ((x (proc)))
                (if result-ready?
                    result
                    (begin (set! result-ready? #t)
                           (set! result x)
                           result))))))))
  ) ; rnrs r5rs


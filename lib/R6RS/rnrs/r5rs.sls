(library (rnrs r5rs (6))
  
  (export null-environment scheme-report-environment delay force
          exact->inexact inexact->exact quotient remainder modulo)
  
  (import (primitives exact->inexact inexact->exact quotient remainder modulo)
          (rnrs eval)
          (rnrs base)
          (rnrs control)
          (only (larceny r7rs promises)
                delay force))
  
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

  ) ; rnrs r5rs


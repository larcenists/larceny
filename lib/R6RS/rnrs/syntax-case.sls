(library (rnrs syntax-case (6))
  
  (export make-variable-transformer
          identifier? bound-identifier=? free-identifier=?
          generate-temporaries datum->syntax syntax->datum 
          syntax-violation syntax syntax-case quasisyntax 
          unsyntax unsyntax-splicing with-syntax 
          _ ...)
  
  (import (except (core primitives) syntax-violation)
          (core with-syntax)  
          (core quasisyntax)
          (except (rnrs base)
                  let-syntax
                  letrec-syntax)
          (rnrs exceptions)
          (rnrs conditions))

  ;; This R6RS-conforming version of syntax-violation
  ;; is not the same as the version used by the macro
  ;; expander itself.

  (define (syntax-violation who msg form . rest)
    (if (not (and (or (eq? who #f)
                      (string? who)
                      (symbol? who))
                  (string? msg)))
        (assertion-violation 'syntax-violation who msg))
    (let* ((who (or who
                    (and (identifier? form)
                         (syntax->datum form))
                    (and (pair? form)
                         (identifier? (car form))
                         (syntax->datum (car form)))))
           (maybe-subform (if (null? rest) #f (car rest)))
           (c1 (make-who-condition who))
           (c2 (make-message-condition msg))
           (c3 (make-syntax-violation form maybe-subform))
           (c (if who
                  (condition c1 c2 c3)
                  (condition c2 c3))))
      (raise c)))
  
  ) ;; rnrs syntax-case


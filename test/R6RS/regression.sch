; Regression tests for R6RS mode.

(import (rnrs))

; Regression test for ticket:515

(define-syntax foo
  (lambda (x)
    (syntax-case x ()
     [(_ (Var ...) (Var2 ...) ...)
      #'(quote
        (([Var ... Var2 ...]) ...))])))

(if (not (equal? (foo (1 2 3) (a b c) (d e f))
                 '(((1 2 3 a b c)) ((1 2 3 d e f)))))
    (assertion-violation #f "ticket:515"))


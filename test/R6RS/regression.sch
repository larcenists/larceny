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

; Regression test for ticket:536

(if (not (equal? (cons* 1 2 3) '(1 2 . 3)))
    (assertion-violation #f "ticket:536"))

; Regression tests for ticket:539

(file-options no-create)
(file-options no-fail)
(file-options no-truncate)
(let ((opts (file-options no-create no-fail no-truncate)))
  (if (not (and (enum-set-member? 'no-create opts)
                (enum-set-member? 'no-fail opts)
                (enum-set-member? 'no-truncate opts)))
      (assertion-violation #f "ticket:539")))

; Regression tests for ticket:540

(if (not (and (eq? 'none (buffer-mode none))
              (eq? 'line (buffer-mode line))
              (eq? 'block (buffer-mode block))))
    (assertion-violation #f "ticket:540"))

; Regression test for ticket:549

(define-condition-type &wibble &condition
    make-wibble
    wibble?
    (wobble wibble-wobble))

(let ((w (make-wibble 549)))
  (if (not (and (wibble? w)
                (= 549 (wibble-wobble w))))
      (assertion-violation #f "ticket:549")))


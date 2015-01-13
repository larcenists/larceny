; Code generator for Scheme.

(define (generate-scheme-parser . args)
  (set! parser-language 'scheme)
  (apply generate-parser
         (lambda args #t)
         generate-scheme-for-nonterminal
         (lambda () #t)
         args))

; Given a nonterminal, the list of right hand sides of productions
; for that nonterminal, the list of director sets for that nonterminal,
; and the list of names of procedures that create abstract syntax trees
; for those productions,
; returns the code for a procedure that parses that nonterminal.

(define (generate-scheme-for-nonterminal nonterminal rhss dsets creators)
  `(define (,(generate-parser-procname nonterminal))
           (case (next-token)
             ,@(map (lambda (rhs dset creator)
                      (init-temp-generator)
                      (list (map lookup-token-kind dset)
                            (generate-code-for-production
                             nonterminal rhs creator)))
                    rhss
                    dsets
                    creators)
             (else (parse-error ',nonterminal
                                  ',(sorted-union
                                     (map (lambda (dset)
                                            (map lookup-token-kind dset))
                                          dsets)))))))

; Given a nonterminal, a rhs for that nonterminal
; the director set for that nonterminal,
; and the name of the procedure that creates abstract syntax trees
; for that nonterminal,
; returns code to parse that rhs and return an abstract syntax tree.
;
; If the right hand side of the production begins with a terminal,
; then the dispatch on the lookahead token has already confirmed that
; that terminal is the lookahead token.

(define (generate-code-for-production nonterminal rhs creator)
  (if (and (not (null? rhs))
           (string? (car rhs)))
      `(begin (consume-token!)
              ,(generate-code-for-production1
                nonterminal (cdr rhs) creator '()))
      (generate-code-for-production1 nonterminal rhs creator '())))

(define (generate-code-for-production1 nonterminal rhs creator trees)
  (cond ((null? rhs)
         (cons creator
               (if (actions-take-arguments)
                   (reverse trees)
                   '())))
        ((string? (car rhs))
         (let ((kind (lookup-token-kind (car rhs))))
           `(if (eq? (next-token) ',kind)
                (begin (consume-token!)
                       ,(generate-code-for-production1
                         nonterminal
                         (cdr rhs)
                         creator
                         trees))
                (parse-error ',nonterminal '(,kind)))))
        (else (let ((temp (generate-temp)))
                `(let ((,temp (,(generate-parser-procname (car rhs)))))
                      ,(generate-code-for-production1
                        nonterminal
                        (cdr rhs)
                        creator
                        (cons temp trees)))))))

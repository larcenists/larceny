(define rewrite
   ((lambda ()
       ((lambda (rewrite-error
                 atom?
                 hyg-list
                 hyg-cons
                 hyg-append
                 hyg-list->vector
                 special-forms-list
                 definition?
                 lambda?
                 constant?
                 rewrite-expr
                 rewrite-define
                 rewrite-top-level-define
                 rewrite-lambda
                 rewrite-and
                 rewrite-or
                 rewrite-let
                 rewrite-letrec
                 rewrite-let*
                 rewrite-cond
                 rewrite-case
                 rewrite-do
                 rewrite-quasiquotation
                 rewrite)
           (begin (set! rewrite-error
                     (lambda msgs
                        (begin (begin (display '"Error:")
                                      (for-each
                                         (lambda (x)
                                            (begin (begin (display '" ")
                                                          (display x))))
                                         msgs)
                                      (newline)
                                      (error '() '"")))))
                  (set! atom? (lambda (x) (begin (begin (not (pair? x))))))
                  (set! hyg-list '%list)
                  (set! hyg-cons '%cons)
                  (set! hyg-append '%append)
                  (set! hyg-list->vector '%list->vector)
                  (set! special-forms-list '#f)
                  (set! definition?
                     (lambda (expr)
                        (begin (begin (if (pair? expr)
                                          (eq? (car expr) 'define)
                                          '#f)))))
                  (set! lambda?
                     (lambda (expr)
                        (begin (begin (if (pair? expr)
                                          (eq? (car expr) 'lambda)
                                          '#f)))))
                  (set! constant?
                     (lambda (expr)
                        (begin (begin (if (if (pair? expr)
                                              (eq? (car expr) 'quote)
                                              '#f)
                                          '#t
                                          (if (number? expr)
                                              '#t
                                              (if (boolean? expr)
                                                  '#t
                                                  (if (char? expr)
                                                      '#t
                                                      (if (string? expr)
                                                          '#t
                                                          (if (procedure?
                                                                 expr)
                                                              '#t
                                                              (if (null?
                                                                     expr)
                                                                  '#t
                                                                  (eq? expr
                                                                       (unspecified)))))))))))))
                  (set! rewrite-expr
                     (lambda (expr)
                        (begin ((lambda (quote-expr)
                                   (begin (begin (set! quote-expr
                                                    (lambda (expr)
                                                       (begin (begin (begin (if (not (if (pair?
                                                                                            expr)
                                                                                         (eq? (car expr)
                                                                                              'quote)
                                                                                         '#f))
                                                                                (list 'quote
                                                                                      expr)
                                                                                expr))))))
                                                 (if (constant? expr)
                                                     (begin (quote-expr
                                                               expr))
                                                     (if (symbol? expr)
                                                         (begin expr)
                                                         (if (lambda? expr)
                                                             (begin (rewrite-lambda
                                                                       expr))
                                                             (if (pair?
                                                                    expr)
                                                                 (begin ((lambda (form)
                                                                            (begin (begin (if form
                                                                                              (rewrite-expr
                                                                                                 ((cdr form)
                                                                                                  expr))
                                                                                              (map rewrite-expr
                                                                                                   expr)))))
                                                                         (assq (car expr)
                                                                               special-forms-list)))
                                                                 (begin (rewrite-error
                                                                           '"Strange expression"
                                                                           expr)))))))))
                                '#f))))
                  (set! rewrite-define
                     (lambda (expr)
                        (begin (begin (if (pair? (cadr expr))
                                          (%cons
                                             'define
                                             (%cons
                                                (caadr expr)
                                                (%cons
                                                   (rewrite-lambda
                                                      (%cons
                                                         'lambda
                                                         (%cons
                                                            (cdadr expr)
                                                            (%append
                                                               (cddr expr)
                                                               '()))))
                                                   '())))
                                          (%cons
                                             'define
                                             (%cons
                                                (cadr expr)
                                                (%cons
                                                   (rewrite-expr
                                                      (caddr expr))
                                                   '()))))))))
                  (set! rewrite-top-level-define rewrite-define)
                  (set! rewrite-lambda
                     (lambda (expr)
                        (begin ((lambda (collect-defines
                                         rewrite-internal-defines)
                                   (begin (begin (set! collect-defines
                                                    (lambda (body defs)
                                                       (begin (begin (begin (if (if (null?
                                                                                       body)
                                                                                    '#t
                                                                                    (not (definition?
                                                                                            (car body))))
                                                                                (cons (reverse
                                                                                         defs)
                                                                                      body)
                                                                                (collect-defines
                                                                                   (cdr body)
                                                                                   (cons (car body)
                                                                                         defs))))))))
                                                 (set! rewrite-internal-defines
                                                    (lambda (defs body)
                                                       (begin (begin (begin (%cons
                                                                               'letrec
                                                                               (%cons
                                                                                  (map (lambda (d)
                                                                                          (begin (begin (begin (cdr (rewrite-define
                                                                                                                       d))))))
                                                                                       defs)
                                                                                  (%append
                                                                                     body
                                                                                     '()))))))))
                                                 ((lambda (result)
                                                     (begin (begin ((lambda (defs)
                                                                       (begin (begin ((lambda (body)
                                                                                         (begin (begin (begin (if (null?
                                                                                                                     defs)
                                                                                                                  (%cons
                                                                                                                     'lambda
                                                                                                                     (%cons
                                                                                                                        (cadr expr)
                                                                                                                        (%cons
                                                                                                                           (rewrite-expr
                                                                                                                              (cons 'begin
                                                                                                                                    body))
                                                                                                                           '())))
                                                                                                                  (%cons
                                                                                                                     'lambda
                                                                                                                     (%cons
                                                                                                                        (cadr expr)
                                                                                                                        (%cons
                                                                                                                           (rewrite-expr
                                                                                                                              (rewrite-internal-defines
                                                                                                                                 defs
                                                                                                                                 body))
                                                                                                                           '()))))))))
                                                                                      (cdr result)))))
                                                                    (car result)))))
                                                  (collect-defines
                                                     (cddr expr)
                                                     '())))))
                                '#f
                                '#f))))
                  (set! rewrite-and
                     (lambda (expr)
                        (begin (begin (if (null? (cdr expr))
                                          (begin '#t)
                                          (if (null? (cddr expr))
                                              (begin (cadr expr))
                                              (begin (%cons
                                                        'if
                                                        (%cons
                                                           (cadr expr)
                                                           (%cons
                                                              (%cons
                                                                 'and
                                                                 (%append
                                                                    (cddr expr)
                                                                    '()))
                                                              (%cons
                                                                 '#f
                                                                 '())))))))))))
                  (set! rewrite-or
                     (lambda (expr)
                        (begin (begin (if (null? (cdr expr))
                                          (begin '#f)
                                          (if (null? (cddr expr))
                                              (begin (cadr expr))
                                              (begin (%cons
                                                        'if
                                                        (%cons
                                                           (cadr expr)
                                                           (%cons
                                                              '#t
                                                              (%cons
                                                                 (%cons
                                                                    'or
                                                                    (%append
                                                                       (cddr expr)
                                                                       '()))
                                                                 '())))))))))))
                  (set! rewrite-let
                     (lambda (expr)
                        (begin ((lambda (named-let?
                                         nlet-bindings
                                         let-bindings
                                         nlet-body
                                         let-body
                                         collect-vars
                                         collect-inits
                                         named-let-name)
                                   (begin (begin (set! named-let?
                                                    (lambda (expr)
                                                       (begin (begin (begin (symbol?
                                                                               (cadr expr)))))))
                                                 (set! nlet-bindings caddr)
                                                 (set! let-bindings cadr)
                                                 (set! nlet-body cdddr)
                                                 (set! let-body cddr)
                                                 (set! collect-vars
                                                    (lambda (bindings)
                                                       (begin (begin (begin (map car
                                                                                 bindings))))))
                                                 (set! collect-inits
                                                    (lambda (bindings)
                                                       (begin (begin (begin (map cadr
                                                                                 bindings))))))
                                                 (set! named-let-name cadr)
                                                 ((lambda (bindings body)
                                                     (begin (begin ((lambda (vars
                                                                             inits)
                                                                       (begin (begin (if (named-let?
                                                                                            expr)
                                                                                         ((lambda (id)
                                                                                             (begin (begin (%cons
                                                                                                              (%cons
                                                                                                                 'lambda
                                                                                                                 (%cons
                                                                                                                    (%cons
                                                                                                                       id
                                                                                                                       '())
                                                                                                                    (%cons
                                                                                                                       (%cons
                                                                                                                          'set!
                                                                                                                          (%cons
                                                                                                                             id
                                                                                                                             (%cons
                                                                                                                                (%cons
                                                                                                                                   'lambda
                                                                                                                                   (%cons
                                                                                                                                      vars
                                                                                                                                      (%append
                                                                                                                                         body
                                                                                                                                         '())))
                                                                                                                                '())))
                                                                                                                       (%cons
                                                                                                                          (%cons
                                                                                                                             id
                                                                                                                             (%append
                                                                                                                                inits
                                                                                                                                '()))
                                                                                                                          '()))))
                                                                                                              (%cons
                                                                                                                 (%list
                                                                                                                    'quote
                                                                                                                    '())
                                                                                                                 '())))))
                                                                                          (named-let-name
                                                                                             expr))
                                                                                         (%cons
                                                                                            (%cons
                                                                                               'lambda
                                                                                               (%cons
                                                                                                  vars
                                                                                                  (%append
                                                                                                     body
                                                                                                     '())))
                                                                                            (%append
                                                                                               inits
                                                                                               '()))))))
                                                                    (collect-vars
                                                                       bindings)
                                                                    (collect-inits
                                                                       bindings)))))
                                                  (if (named-let? expr)
                                                      (nlet-bindings expr)
                                                      (let-bindings expr))
                                                  (if (named-let? expr)
                                                      (nlet-body expr)
                                                      (let-body expr))))))
                                '#f
                                '#f
                                '#f
                                '#f
                                '#f
                                '#f
                                '#f
                                '#f))))
                  (set! rewrite-letrec
                     (lambda (expr)
                        (begin ((lambda (collect-vars collect-inits)
                                   (begin (begin (set! collect-vars
                                                    (lambda ()
                                                       (begin (begin (begin (map car
                                                                                 (cadr expr)))))))
                                                 (set! collect-inits
                                                    (lambda ()
                                                       (begin (begin (begin (map cadr
                                                                                 (cadr expr)))))))
                                                 ((lambda (vars inits)
                                                     (begin (begin (%cons
                                                                      (%cons
                                                                         'lambda
                                                                         (%cons
                                                                            vars
                                                                            (%append
                                                                               (map (lambda (x
                                                                                             y)
                                                                                       (begin (begin (%cons
                                                                                                        'set!
                                                                                                        (%cons
                                                                                                           x
                                                                                                           (%cons
                                                                                                              y
                                                                                                              '()))))))
                                                                                    vars
                                                                                    inits)
                                                                               (%append
                                                                                  (cddr expr)
                                                                                  '()))))
                                                                      (%append
                                                                         (map (lambda (x)
                                                                                 (begin (begin '#f)))
                                                                              vars)
                                                                         '())))))
                                                  (collect-vars)
                                                  (collect-inits)))))
                                '#f
                                '#f))))
                  (set! rewrite-let*
                     (lambda (expr)
                        (begin (begin ((lambda (bindings)
                                          (begin (begin (if (null?
                                                               bindings)
                                                            (%cons
                                                               'begin
                                                               (%append
                                                                  (cddr expr)
                                                                  '()))
                                                            (%cons
                                                               'let
                                                               (%cons
                                                                  (%cons
                                                                     (car bindings)
                                                                     '())
                                                                  (%cons
                                                                     (%cons
                                                                        'let*
                                                                        (%cons
                                                                           (cdr bindings)
                                                                           (%append
                                                                              (cddr expr)
                                                                              '())))
                                                                     '())))))))
                                       (cadr expr))))))
                  (set! rewrite-cond
                     (lambda (expr)
                        (begin (begin (if (null? (cdr expr))
                                          'unspecified
                                          ((lambda (test
                                                    e-sequence
                                                    clause2)
                                              (begin (begin (if (eq? test
                                                                     'else)
                                                                (begin (%cons
                                                                          'begin
                                                                          (%append
                                                                             e-sequence
                                                                             '())))
                                                                (if (null?
                                                                       e-sequence)
                                                                    (begin (%cons
                                                                              'or
                                                                              (%cons
                                                                                 test
                                                                                 (%cons
                                                                                    (%cons
                                                                                       'cond
                                                                                       (%append
                                                                                          clause2
                                                                                          '()))
                                                                                    '()))))
                                                                    (begin (%cons
                                                                              'if
                                                                              (%cons
                                                                                 test
                                                                                 (%cons
                                                                                    (%cons
                                                                                       'begin
                                                                                       (%append
                                                                                          e-sequence
                                                                                          '()))
                                                                                    (%cons
                                                                                       (%cons
                                                                                          'cond
                                                                                          (%append
                                                                                             clause2
                                                                                             '()))
                                                                                       '()))))))))))
                                           (caadr expr)
                                           (cdadr expr)
                                           (cddr expr)))))))
                  (set! rewrite-case
                     (lambda (l)
                        (begin (begin ((lambda (s)
                                          (begin (begin (%cons
                                                           'let
                                                           (%cons
                                                              (%cons
                                                                 (%cons
                                                                    s
                                                                    (%cons
                                                                       (cadr l)
                                                                       '()))
                                                                 '())
                                                              (%cons
                                                                 (%cons
                                                                    'cond
                                                                    (%append
                                                                       ((lambda (loop)
                                                                           (begin (begin (set! loop
                                                                                            (lambda (q
                                                                                                     r)
                                                                                               (begin (begin (if (null?
                                                                                                                    q)
                                                                                                                 (begin (reverse
                                                                                                                           r))
                                                                                                                 (if (eq? (caar q)
                                                                                                                          'else)
                                                                                                                     (begin (reverse
                                                                                                                               (cons (car q)
                                                                                                                                     r)))
                                                                                                                     (begin (loop (cdr q)
                                                                                                                                  (cons ((lambda (x)
                                                                                                                                            (begin (begin (cons (cons 'or
                                                                                                                                                                      (map (lambda (z)
                                                                                                                                                                              (begin (begin (%cons
                                                                                                                                                                                               'eqv?
                                                                                                                                                                                               (%cons
                                                                                                                                                                                                  s
                                                                                                                                                                                                  (%cons
                                                                                                                                                                                                     (%list
                                                                                                                                                                                                        'quote
                                                                                                                                                                                                        ',z)
                                                                                                                                                                                                     '()))))))
                                                                                                                                                                           (car x)))
                                                                                                                                                                (cdr x)))))
                                                                                                                                         (car q))
                                                                                                                                        r)))))))))
                                                                                         (loop (cddr l)
                                                                                               '()))))
                                                                        '())
                                                                       '()))
                                                                 '()))))))
                                       (gensym '"T"))))))
                  (set! rewrite-do
                     (lambda (expr)
                        (begin (begin (if (if (not (list? expr))
                                              '#t
                                              (if (< (length expr) '3)
                                                  '#t
                                                  (if (not (list?
                                                              (cadr expr)))
                                                      '#t
                                                      (not (if (list?
                                                                  (caddr
                                                                     expr))
                                                               (pair?
                                                                  (caddr
                                                                     expr))
                                                               '#f)))))
                                          (rewrite-error
                                             '"Malformed do expression"
                                             expr))
                                      ((lambda (loop bindings test)
                                          (begin (begin (%cons
                                                           'letrec
                                                           (%cons
                                                              (%cons
                                                                 (%cons
                                                                    loop
                                                                    (%cons
                                                                       (%cons
                                                                          'lambda
                                                                          (%cons
                                                                             (map car
                                                                                  bindings)
                                                                             (%cons
                                                                                (%cons
                                                                                   'if
                                                                                   (%cons
                                                                                      (car test)
                                                                                      (%cons
                                                                                         (%cons
                                                                                            'begin
                                                                                            (%append
                                                                                               (cdr test)
                                                                                               '()))
                                                                                         (%cons
                                                                                            (%cons
                                                                                               'begin
                                                                                               (%cons
                                                                                                  (%cons
                                                                                                     'begin
                                                                                                     (%append
                                                                                                        (cdddr
                                                                                                           expr)
                                                                                                        '()))
                                                                                                  (%cons
                                                                                                     (%cons
                                                                                                        loop
                                                                                                        (%append
                                                                                                           (map caddr
                                                                                                                bindings)
                                                                                                           '()))
                                                                                                     '())))
                                                                                            '()))))
                                                                                '())))
                                                                       '()))
                                                                 '())
                                                              (%cons
                                                                 (%cons
                                                                    loop
                                                                    (%append
                                                                       (map cadr
                                                                            bindings)
                                                                       '()))
                                                                 '()))))))
                                       (gensym '"DO")
                                       (map (lambda (x)
                                               (begin (begin (if (atom? x)
                                                                 (begin (rewrite-error
                                                                           '"Malformed do expression"
                                                                           expr))
                                                                 (if (atom?
                                                                        (cdr x))
                                                                     (begin (list (car x)
                                                                                  '()
                                                                                  (car x)))
                                                                     (if (atom?
                                                                            (cddr x))
                                                                         (begin (list (car x)
                                                                                      (cadr x)
                                                                                      (car x)))
                                                                         (begin x)))))))
                                            (cadr expr))
                                       (caddr expr))))))
                  (set! rewrite-quasiquotation
                     (lambda (expr)
                        (begin ((lambda (r)
                                   (begin (begin (set! r
                                                    (lambda (e l)
                                                       (begin (begin (begin (if (vector?
                                                                                   (cadr e))
                                                                                (begin ((lambda (v)
                                                                                           (begin (begin (begin (if (zero?
                                                                                                                       l)
                                                                                                                    (list hyg-list->vector
                                                                                                                          (r (list 'quasiquote
                                                                                                                                   (vector->list
                                                                                                                                      v))
                                                                                                                             l))
                                                                                                                    (list hyg-list
                                                                                                                          'quasiquote
                                                                                                                          (list hyg-list->vector
                                                                                                                                (r (list 'quasiquote
                                                                                                                                         (vector->list
                                                                                                                                            v))
                                                                                                                                   l))))))))
                                                                                        (cadr e)))
                                                                                (if (pair?
                                                                                       (cadr e))
                                                                                    (begin (if (eq? (car (cadr e))
                                                                                                    'unquote)
                                                                                               (begin ((lambda (x)
                                                                                                          (begin (begin (begin (if (zero?
                                                                                                                                      l)
                                                                                                                                   x
                                                                                                                                   (list hyg-list
                                                                                                                                         ''unquote
                                                                                                                                         (r (list 'quasiquote
                                                                                                                                                  x)
                                                                                                                                            (- l
                                                                                                                                               '1))))))))
                                                                                                       (cadr (cadr e))))
                                                                                               (if (eq? (car (cadr e))
                                                                                                        'quasiquote)
                                                                                                   (begin ((lambda (x)
                                                                                                              (begin (begin (begin (list hyg-list
                                                                                                                                         ''quasiquote
                                                                                                                                         (r (list 'quasiquote
                                                                                                                                                  x)
                                                                                                                                            (+ l
                                                                                                                                               '1)))))))
                                                                                                           (cadr (cadr e))))
                                                                                                   (if (if (pair?
                                                                                                              (car (cadr e)))
                                                                                                           (eq? (caar (cadr e))
                                                                                                                'unquote-splicing)
                                                                                                           '#f)
                                                                                                       (begin ((lambda (x
                                                                                                                        y)
                                                                                                                  (begin (begin (begin (if (zero?
                                                                                                                                              l)
                                                                                                                                           (list hyg-append
                                                                                                                                                 x
                                                                                                                                                 (r (list 'quasiquote
                                                                                                                                                          y)
                                                                                                                                                    '0))
                                                                                                                                           (list hyg-list
                                                                                                                                                 ''quasiquote
                                                                                                                                                 (list hyg-list
                                                                                                                                                       (list hyg-list
                                                                                                                                                             ''unquote-splicing
                                                                                                                                                             (r (list 'quasiquote
                                                                                                                                                                      x)
                                                                                                                                                                (- l
                                                                                                                                                                   '1)))
                                                                                                                                                       (r (list 'quasiquote
                                                                                                                                                                y)
                                                                                                                                                          l))))))))
                                                                                                               (cadr (car (cadr e)))
                                                                                                               (cdr (cadr e))))
                                                                                                       (if (eq? (car (cadr e))
                                                                                                                'quote)
                                                                                                           (begin (list hyg-list
                                                                                                                        ''quote
                                                                                                                        (cadr e)))
                                                                                                           (begin ((lambda (x
                                                                                                                            y)
                                                                                                                      (begin (begin (begin (list hyg-cons
                                                                                                                                                 (r (list 'quasiquote
                                                                                                                                                          x)
                                                                                                                                                    l)
                                                                                                                                                 (r (list 'quasiquote
                                                                                                                                                          y)
                                                                                                                                                    l))))))
                                                                                                                   (car (cadr e))
                                                                                                                   (cdr (cadr e)))))))))
                                                                                    (begin ((lambda (x)
                                                                                               (begin (begin (begin (list 'quote
                                                                                                                          x)))))
                                                                                            (cadr e))))))))))
                                                 (r expr '0))))
                                '#f))))
                  (set! rewrite
                     (lambda (expr)
                        (begin (begin (if (definition? expr)
                                          (rewrite-top-level-define expr)
                                          (rewrite-expr expr))))))
                  (set! special-forms-list
                     (list (cons 'or rewrite-or)
                           (cons 'and rewrite-and)
                           (cons 'let rewrite-let)
                           (cons 'letrec rewrite-letrec)
                           (cons 'let* rewrite-let*)
                           (cons 'case rewrite-case)
                           (cons 'cond rewrite-cond)
                           (cons 'do rewrite-do)
                           (cons 'quasiquote rewrite-quasiquotation)))
                  rewrite))
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f
        '#f))))

(define rewrite-file
   (lambda (infilename outfilename)
      (begin (delete-file outfilename)
             (call-with-input-file
                infilename
                (lambda (in)
                   (begin (call-with-output-file
                             outfilename
                             (lambda (out)
                                (begin ((lambda (loop)
                                           (begin (set! loop
                                                     (lambda (item)
                                                        (begin (if (eof-object?
                                                                      item)
                                                                   '#t
                                                                   (begin (write
                                                                             (rewrite
                                                                                item)
                                                                             out)
                                                                          (loop (read in)))))))
                                                  (loop (read in))))
                                        '()))))))))))


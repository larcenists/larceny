; Copyright 2007 William D Clinger
;
; $Id$
;
; Interlibrary optimization.
;
; This pass converts the output of Andre van Tonder's macro expander
; into a form that Twobit will optimize more effectively.
;
; FIXME:  This isn't fully implemented yet, but it does improve
; interprocedural optimization of libraries and top-level programs
; that doesn't actually involve any interlibrary optimization.

(define (pass0 exp)
  (if (and (integrate-procedures)
           (global-optimization))
      (pass0-optimize exp)
      exp))

; When van Tonder's expander (as modified for Larceny)
; is run on a top-level program,
; its output currently looks something like
;
; (begin #\P
;        (ex:import-libraries-for-run
;          (quote (((rnrs io simple) 0) ((rnrs base) 0)))
;          (quote (\x0;build~1194538684~2675
;                   \x0;build~1194538684~1399))
;          0)
;        <def-or-exp>
;        ...)
;
; where each <def-or-exp> is an R5RS top-level definition
; or expression that has been macro-expanded into core forms.
;
; When van Tonder's expander is run on a library,
; its output currently looks something like
;
; (begin
;  #\L
;  (define <mangled1> ex:unspecified) ...           ; for each defined variable
;  (ex:register-library!
;    (ex:make-library
;      '(local foo)                                             ; name
;      (lambda () '())                                          ; envs
;      '((aa variable \x0;aa~1dwErK~9 (0) #f (local foo))       ; exports
;        (ff variable \x0;ff~1dwErK~11 (0) #f (local foo)))
;      '(((rnrs base) 0))                                       ; imports
;      '(\x0;build~1az74b~1398)                                 ; builds
;      (lambda () (values))                                     ; visiter [sic]
;      (lambda ()                                               ; invoker
;        (set! <mangled1> ex:undefined) ...         ; for each defined variable
;        (set! <mangled1> <expr1>) ...              ; for each defined variable
;        (values))
;      '\x0;build~1dwErK~19))                                   ; build
;  (values))
;
; where each <mangled1> is an R5RS global variable whose name
; is the mangled form of a variable defined within the library
; and each <expr1> is an R5RS expression that has been macro-expanded
; into core forms.

; If the expression to be compiled has one of the peculiar forms
; outlined above, then it is rewritten by a special-purpose rule.

(define (pass0-optimize exp)
  (cond ((and (pair? exp)
              (eq? (car exp) 'begin)
              (pair? (cdr exp))
              (eq? (cadr exp) #\P)
              (pair? (cddr exp))
              (let ((form1 (caddr exp)))
                (and (pair? form1)
                     (eq? (car form1)
                          'ex:import-libraries-for-run))))
         (pass0-optimize-toplevel (cons 'begin (cddr exp))))
        ((and (pair? exp)
              (eq? (car exp) 'begin)
              (pair? (cdr exp))
              (eq? (cadr exp) #\L)
              (list? exp)
              (>= (length (cddr exp)) 2)
              (let* ((forms (reverse (cddr exp))))
                (and (equal? (car forms) '(values))
                     (pair? (cadr forms))
                     (eq? (car (cadr forms))
                          'ex:register-library!)
                     (pair? (cdr (cadr forms)))
                     (list? (cadr (cadr forms)))
                     (= 9 (length (cadr (cadr forms))))
                     (eq? (car (cadr (cadr forms)))
                          'ex:make-library))))
         (pass0-optimize-library (cons 'begin (cddr exp))))
        (else
         exp)))

; Wrapping those definitions and expressions inside a let
; may allow Twobit to generate better code.
;
; FIXME: (gensym) should return an unforgeable symbol,
; but there is no such thing anymore.

(define (pass0-optimize-toplevel exp)
  (define (gensym)
    (set! counter (+ counter 16))
    (string->symbol
     (string-append "ignored0"
                    (number->string counter 16))))
  (define counter #xf0000)
  (let* ((form1 (cadr exp))
         (forms (cddr exp))
         (forms (map (lambda (form)
                       (if (and (pair? form)
                                (eq? (car form) 'define))
                           form
                           (list 'define (gensym) form)))
                     forms))
         (forms (pass0-sort-definitions forms '())))
    `(begin ,form1
            ((lambda ()
               ,@forms
               0)))))

; Wrapping a library's definitions and expressions inside a let
; may allow Twobit to generate better code.
; If the library exports a variable it defines, then that variable's
; value must be copied into the global variable whose name is the
; mangled form of the variable's name.
; Copying works because variables defined by a library cannot be
; assigned outside the library.
;
; FIXME:  This rewriting is done only if the library's exports
; consist entirely of variables.  Exporting a macro will disable
; the optimization.

(define (pass0-optimize-library exp)
  (define (gensym)
    (set! counter (+ counter 16))
    (string->symbol
     (string-append "t0"
                    (number->string counter 16))))
  (define counter #xf0000)
  (let* ((revforms (reverse (cdr exp)))
         (last-form (car revforms))
         (main-form (cadr revforms))
         (defns (reverse (cddr revforms)))  ; (define <mangled1> #!unspecified)
         (defined (map cadr defns))
         (main-form-arg (cadr main-form))   ; (ex:make-library ...)
         (pieces (cdr main-form-arg))
         (name    (list-ref pieces 0))      ; quoted list
         (envs    (list-ref pieces 1))      ; lambda expression (thunk)
         (exports (list-ref pieces 2))      ; quoted alist
         (imports (list-ref pieces 3))      ; quoted list of mangled names
         (builds  (list-ref pieces 4))      ; quoted list
         (visitor (list-ref pieces 5))      ; lambda expression (thunk)
         (invoker (list-ref pieces 6))      ; lambda expression (thunk)
         (build   (list-ref pieces 7))      ; quoted symbol
         (exported-entries (cadr exports))  ; without the quote
         (exported-globals (map caddr exported-entries))  ; mangled names
         (exported-kinds (map cadr exported-entries))
         (invoker-forms (cddr invoker)))
    (assert (every? (lambda (x) (eq? x 'define))
                    (map car defns)))
    (assert (every? (lambda (x) (eq? x 'ex:unspecified))
                    (map caddr defns)))
    (if (not (every? (lambda (kind) (eq? kind 'variable))
                     exported-kinds))
        exp

        ;; Separate the body of the visitor thunk into
        ;;   preliminary forms:
        ;;     (set! <mangled1> ex:undefined) ...   ; for each defined variable
        ;;   forms:
        ;;     (set! <mangled1> <expr1>) ...        ; for each defined variable
        ;;     (values))

        (do ((defined-vars defined (cdr defined-vars))
             (forms invoker-forms (cdr forms))
             (preliminary-forms '() (cons (car forms) preliminary-forms)))
            ((null? defined-vars)
             (let* ((preliminary-forms (reverse preliminary-forms))
                    (assignments (filter (lambda (form)
                                           (and (pair? form)
                                                (eq? (car form) 'set!)))
                                         forms))
                    (assignments
                     (pass0-sort-definitions assignments exported-globals))
                    (exported-assignments
                     (filter (lambda (assignment)
                               (memq (cadr assignment) exported-globals))
                             assignments))
                    (value-forms
                     (map (lambda (form)
                            (let ((var (cadr form))
                                  (exp (caddr form)))
                              (if (and (pair? exp)
                                       (eq? (car exp) 'lambda)
                                       (list? (cadr exp)))
                                  `(lambda ,(cadr exp)
                                     (,var ,@(cadr exp)))
                                  var)))
                          exported-assignments))
                    (temps (map (lambda (x) (gensym))
                                exported-assignments))
                    (global-assignments
                     (map (lambda (var temp) `(set! ,var ,temp))
                          (map cadr exported-assignments)
                          temps))
                    (undefs (map (lambda (x) '(undefined))
                                 defined)))
               (assert (equal? defined (map cadr preliminary-forms)))
               (assert (equal? defined (reverse (map cadr assignments))))
               (let ((result
                      `(begin ,@defns
                              (ex:register-library!
                               (ex:make-library
                                ,name
                                ,envs
                                ,exports
                                ,imports
                                ,builds
                                ,visitor
                                (lambda ()
                                  ,@preliminary-forms
                                  (apply
                                   (lambda (,@temps)
                                     ,@global-assignments
                                     (values))
                                   ((lambda (,@defined)
                                      ,@forms
                                      (list ,@value-forms))
                                    ,@undefs)))
                                ,build))
                              ,last-form)))
                 result)))))))

; Twobit may generate better code if the definitions are
; sorted as follows (see also finalize-body in expand.sch):
;
;     procedure definitions (with a fixed number of arguments)
;     procedure definitions (with a rest argument, not exported)
;     trivial definitions (whose right hand side is a constant)
;     procedure definitions (with a rest argument, exported)
;     other definitions (in their original order)
;
; Sorting into that order cannot break a correct R6RS program,
; but it may eliminate a violation of the letrec* restriction
; (which Larceny doesn't enforce anyway).

(define (pass0-sort-definitions defs exported-names)
  (define (loop defs procs0 procs1 procs2 trivs others)
    (if (null? defs)
        (append (reverse procs0)
                (reverse procs1)
                (reverse trivs)
                (reverse procs2)
                (reverse others))
        (let* ((def (car defs))
               (defs (cdr defs))
               (rhs (cadr def)))
          (if (not (pair? rhs))
              (if (symbol? rhs)
                  (loop defs procs0 procs1 procs2 trivs (cons def others))
                  (loop defs procs0 procs1 procs2 (cons def trivs) others))
              (let ((var (car def))
                    (keyword (car rhs)))
                (cond ((eq? keyword 'lambda)
                       (cond ((list? (cadr rhs))
                              (loop defs
                                    (cons def procs0)
                                    procs1
                                    procs2
                                    trivs
                                    others))
                             ((memq var exported-names)
                              (loop defs
                                    procs0
                                    procs1
                                    (cons def procs2)
                                    trivs
                                    others))
                             (else
                              (loop defs
                                    procs0
                                    (cons def procs1)
                                    procs2
                                    trivs
                                    others))))
                      ((eq? keyword 'quote)
                       (loop defs
                             procs0
                             procs1
                             procs2
                             (cons def trivs)
                             others))
                      (else
                       (loop defs
                             procs0
                             procs1
                             procs2
                             trivs
                             (cons def others)))))))))
    (loop defs '() '() '() '() '()))

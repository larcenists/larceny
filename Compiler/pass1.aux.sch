; Copyright 1991 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 24 June 1995 / lth
;
; A naive implementation of macros and procedure-specific source code
; transformations.  Most of this should be replaced by R4RS hygienic
; macros.

;***************************************************************
;
; Each definition in this section should be overridden by an assignment
; in a target-specific file.
;
; If a lambda expression has more than @maxargs-with-rest-arg@ required
; arguments followed by a rest argument, then the macro expander will
; rewrite the lambda expression as a lambda expression with only one
; argument (a rest argument) whose body is a LET that binds the arguments
; of the original lambda expression.

(define @maxargs-with-rest-arg@
  1000000)                              ; infinity

(define (prim-entry name) #f)           ; no integrable procedures
(define (prim-arity name) 0)            ; all of which take 0 arguments
(define (prim-opcodename name) name)    ; and go by their source names

; End of definitions to be overridden by target-specific assignments.
;
;***************************************************************

; Procedure-specific source code transformations.
; The transformer is passed a source code expression and a predicate
; and returns one of:
;
;    the original source code expression
;    a new source code expression to use in place of the original
;    #f to indicate that the procedure is being called
;      with an incorrect number of arguments or
;      with an incorrect operand
;
; The original source code expression is guaranteed to be a list whose
; car is the name associated with the transformer.
; The predicate takes an identifier (a symbol) and returns true iff
; that identifier is bound to something other than its global binding.
;
; Since the procedures and their transformations are target-specific,
; they are defined in another file, in the Target subdirectory.

(define @integrable@ '())

(define (integrable? name)
  (and (integrate-usual-procedures)
       (or (assq name @integrable@)
           (prim-entry name))))

(define define-inline)

(define (define-inline name transformer)
  (if (assq name @integrable@)
      (begin (set! @integrable@
                   (remq! (assq name @integrable@)
                          @integrable@))
             (define-inline name transformer))
      (begin (set! @integrable@
                   (cons (list name transformer)
                         @integrable@))
             name)))

(define (inline-error exp) (static-error 9 exp))


; Macros for cross-compilation.
; Naive macros, should be replaced by hygienic macros.

(define @special-forms@
  '(quote lambda if set! begin))

(define @macros@ '())

(define (install-macro keyword transformer)
  (if (assq keyword @macros@)
      (begin (set! @special-forms@ (remq! keyword @special-forms@))
             (set! @macros@ (remq! (assq keyword @macros@) @macros@))
             (install-macro keyword transformer))
      (begin (set! @macros@ (cons (list keyword transformer) @macros@))
             (set! @special-forms@ (cons keyword @special-forms@))
             keyword)))

(define define-macro install-macro)

;(define *undefined-expression* ''*undefined*)
;(define *unspecified-expression* ''*unspecified*)

;(define-macro 'undefined
;  (lambda (l) *undefined-expression*))

;(define-macro 'unspecified
;  (lambda (l) *unspecified-expression*))

(define-macro 'cond
  (lambda (l)
    (if (null? (cdr l))
        '(unspecified)
        (if (memq (car (car (cdr l))) '(#t else))
            (cons 'begin (cdr (car (cdr l))))
            (if (= (length (car (cdr l))) 1)
                (list 'or
                      (car (car (cdr l)))
                      (cons 'cond (cdr (cdr l))))
                (if (eq? (car (cdr (car (cdr l)))) '=>)
                    (if (= (length (car (cdr l))) 3)
                        (list '(lambda (test-result thunk2 thunk3)
                                       (if test-result
                                           ((thunk2) test-result)
                                           (thunk3)))
                              (car (car (cdr l)))
                              (list 'lambda
                                    '()
                                    (car (cdr (cdr (car (cdr l))))))
                              (list 'lambda
                                    '()
                                    (cons 'cond (cdr (cdr l)))))
                        (error "Malformed cond clause" l))
                    (list 'if
                          (car (car (cdr l)))
                          (cons 'begin (cdr (car (cdr l))))
                          (cons 'cond (cdr (cdr l))))))))))

(define-macro 'case
  (lambda (l)
    (let* ((tag (if (symbol? (cadr l)) (cadr l) (gensym "t")))
           (body (cons
                  'cond
                  (map
                   (lambda (x)
                     (cond ((eq? (car x) 'else) x)
                           ((and (pair? (car x)) (= (length (car x)) 1))
                            (cons (list 'eqv?
                                        tag
                                        (list 'quote (car (car x))))
                                  (cdr x)))
                           ((pair? (car x))
                            (cons (list 'memv
                                        tag
                                        (list 'quote (car x)))
                                  (cdr x)))
                           (else (cons (list 'eqv?
                                             tag
                                             (list 'quote (car x)))
                                       (cdr x)))))
                   (cddr l)))))
      (if (symbol? (cadr l))
          body
          (list 'let (list (list tag (cadr l))) body)))))

(define-macro 'and
  (lambda (l)
    (cond ((null? (cdr l)) '#t)
          ((null? (cddr l)) (cadr l))
          (else `(if ,(cadr l)
                     (and ,@(cddr l))
                     #f)))))

(define-macro 'or
  (lambda (l)
    (cond ((null? (cdr l)) '#f)
          ((null? (cddr l)) (cadr l))
          (else (let ((temp (gensym "T")))
                  `(let ((,temp ,(cadr l)))
                        (if ,temp ,temp (or ,@(cddr l)))))))))

(define-macro 'let
  (lambda (l)
    (if (and (not (pair? (cadr l)))
             (not (null? (cadr l))))
        (let ((f (cadr l))
              (bindings (caddr l))
              (vars (map car (caddr l)))
              (body (cdddr l)))
          ; named let
          (if (not (memq f vars))
              (list 'let
                    bindings
                    (list 'letrec
                          (list (list f
                                      (cons 'lambda
                                            (cons vars body))))
                          (cons f vars)))                     
              (cons (list 'letrec
                          (list (list (cadr l)
                                      (cons 'lambda
                                            (cons (map car (caddr l))
                                                  (cdddr l)))))
                          (cadr l))
                    (map (lambda (x) (cadr x)) (caddr l)))))
        ; standard let
        (cons (cons 'lambda               ; standard let
                    (cons (map car (cadr l))
                          (cddr l)))
              (map (lambda (x) (cadr x)) (cadr l))))))

(define-macro 'let*
  (lambda (l)
    (if (null? (cadr l))
        (cons 'let (cdr l))
        `(let (,(caadr l))
              (let* ,(cdadr l) ,@(cddr l))))))

(define-macro 'letrec
  ; Sorts letrec bindings into a good order for
  ; single assignment analysis and single assignment elimination.
  (let ((good? (lambda (x y)
                 (let ((x.rhs (cadr x))
                       (y.rhs (cadr y)))
                   (let ((x-is-lambda? (and (pair? x.rhs)
                                            (eq? (car x.rhs) 'lambda)))
                         (y-is-lambda? (and (pair? y.rhs)
                                            (eq? (car y.rhs) 'lambda)))
                         (x-is-trivial? (or (not (pair? x.rhs))
                                            (eq? (car x.rhs) 'quote)))
                         (y-is-trivial? (or (not (pair? y.rhs))
                                            (eq? (car y.rhs) 'quote))))
                     (or x-is-lambda?
                         (and x-is-trivial? (not y-is-lambda?))))))))                            
    (lambda (l)
      (let ((bindings (twobit-sort good? (cadr l)))
            (body (cddr l)))
        (cons (list 'lambda
                    (map car bindings)
                    (cons 'begin
                          (map (lambda (x) (cons 'set! x)) bindings))
                    (cons 'let (cons '() body)))
              (map (lambda (x) '(undefined)) bindings))))))

(define-macro 'do
  (let ((oops (lambda (l) 
                (error "Malformed do expression" l))))
    (lambda (l)
      (if (not (list? l)) (oops l))
      (if (< (length l) 3) (oops l))
      (if (not (list? (cadr l))) (oops l))
      (if (not (and (list? (caddr l)) (pair? (caddr l))))
          (oops l))
      (let
        ((loop (gensym "DO"))
         (bindings (map (lambda (x)
                          (cond ((not (pair? x)) (oops l))
                                ((not (pair? (cdr x)))
                                 (list (car x) '() (car x)))
                                ((not (pair? (cddr x)))
                                 (list (car x) (cadr x) (car x)))
                                (else x)))
                        (cadr l)))
         (test (caddr l)))
        (list 'letrec
              (list (list loop
                          (list 'lambda
                                (map car bindings)
                                (list 'if
                                      (car test)
                                      (cons 'begin (cdr test))
                                      (list 'begin
                                            (cons 'begin (cdddr l))
                                            (cons loop (map caddr bindings)))))))
              (cons loop (map cadr bindings)))))))

(define-macro 'delay
  (lambda (l)
    `(.make-promise (lambda () ,(cadr l)))))

;; Rewrites quasiquotations. Painfully.
;; A stop-gap measure only.
;;
;; Notation:
;;
;;   0 <= m <= infty
;;   1 <= n <= infty
;;   v is a vector
;;   Caps indicate literals. Lower case indicates actual code.
;;   Aliases: QQ = quasiquote
;;            UNQ = unquote
;;            UNQS = unquote-splicing
;;            L->V = list->vector
;;            v->l = vector->list
;;
;; Rewrite Rules (apply matching top-down, from the outside and in):
;;
;;   (r (QQ v) 0) => (L->V (r (QQ (v->l v)) 0)))
;;   (r (QQ v) n) => (LIST (QUOTE QQ) (L->V (r (QQ (v->l v)) n))))
;;   (r (QQ (UNQ x) 0)) => x
;;   (r (QQ (UNQ x) n)) => (LIST (QUOTE UNQ) (r (QQ x) (- n 1)))
;;   (r (QQ (QQ x) m)) => (LIST (QUOTE QQ) (r (QQ x) (+ m 1)))
;;   (r (QQ ((UNQS x) . y)) 0) => (APPEND x (r (QQ y) 0))
;;   (r (QQ ((UNQS x) . y)) n) => (LIST (QUOTE QQ)
;;                                      (LIST (LIST (QUOTE UNQS) 
;;                                                  (r (QQ x) (- n 1)))
;;                                                  (r (QQ y) n)))
;;;   (r (QQ (QUOTE x)) m) => (LIST (QUOTE QUOTE) (QUOTE x))
;;   (r (QQ (x . y) m)) => (CONS (r (QQ x) m) (r (QQ y) m))
;;   (r (QQ x) m) => (QUOTE x)
;;
;; Notes:
;;
;;   Not terribly robust.
;;   One could write a set of rules which would expand to more 
;;   efficient code, using literals wherever possible. Here, we 
;;   almost always expand to runnable code, resulting in slower
;;   and larger code. 
;;
;; Assumes existence of top-level names .LIST, .CONS, .LIST->VECTOR, 
;; and .APPEND.

(define-macro 'quasiquote
  (lambda (expr)
    
    (define hyg-list->vector '.list->vector)
    (define hyg-list '.list)
    (define hyg-cons '.cons)
    (define hyg-append '.append)
    
    (define (r e l)
      (cond ((vector? (cadr e))
             (let ((v (cadr e)))
               (if (zero? l)
                   (list hyg-list->vector
                         (r (list 'QUASIQUOTE (vector->list v)) l))
                   (list hyg-list
                         'QUASIQUOTE
                         (list hyg-list->vector 
                               (r (list 'QUASIQUOTE (vector->list v)) l))))))
            ((pair? (cadr e))
             (cond ((eq? (car (cadr e)) 'UNQUOTE)
                    (let ((x (cadr (cadr e))))
                      (if (zero? l)
                          x
                          (list hyg-list '(QUOTE UNQUOTE)
                                         (r (list 'QUASIQUOTE x) (- l 1))))))
                   ((eq? (car (cadr e)) 'QUASIQUOTE)
                    (let ((x (cadr (cadr e))))
                      (list hyg-list 
                            '(QUOTE QUASIQUOTE)
                            (r (list 'QUASIQUOTE x) (+ l 1)))))
                   ((and (pair? (car (cadr e)))
                         (eq? (caar (cadr e)) 'UNQUOTE-SPLICING))
                    (let ((x (cadr (car (cadr e))))
                          (y (cdr (cadr e))))
                      (if (zero? l)
                          (list hyg-append x (r (list 'QUASIQUOTE y) 0))
                          (list hyg-list
                                '(QUOTE quasiquote)
                                (list hyg-list
                                      (list hyg-list 
                                            '(QUOTE UNQUOTE-SPLICING)
                                            (r (list 'QUASIQUOTE x) (- l 1)))
                                      (r (list 'QUASIQUOTE y) l))))))
;                   ((eq? (car (cadr e)) 'QUOTE)
;                    (list hyg-list '(QUOTE QUOTE) (cadr e)))
                   (else
                    (let ((x (car (cadr e)))
                          (y (cdr (cadr e))))
                      (list hyg-cons 
                            (r (list 'QUASIQUOTE x) l) 
                            (r (list 'QUASIQUOTE y) l))))))
            (else
             (let ((x (cadr e)))
               (list 'QUOTE x)))))
    
    (r expr 0)))

(define-macro 'unquote
  (lambda (expr)
    (error "Invalid context for UNQUOTE special form.")))

(define-macro 'unquote-splicing
  (lambda (expr)
    (error "Invalid context for UNQUOTE-SPLICING special form.")))


; For testing.

; MAKE-READABLE strips the referencing information
; and replaces (begin I) by I.

(define (make-readable exp)
  (case (car exp)
    ((quote)    exp)
    ((lambda)   `(lambda ,(lambda.args exp)
                         ,@(map (lambda (def)
                                  `(define ,(def.lhs def)
                                           ,(make-readable (def.rhs def))))
                                (lambda.defs exp))
                           ,(make-readable (lambda.body exp))))
    ((set!)     `(set! ,(assignment.lhs exp)
                       ,(make-readable (assignment.rhs exp))))
    ((if)       `(if ,(make-readable (if.test exp))
                     ,(make-readable (if.then exp))
                     ,(make-readable (if.else exp))))
    ((begin)    (if (variable? exp)
                    (variable.name exp)
                    `(begin ,@(map make-readable (begin.exprs exp)))))
    (else       `(,(make-readable (call.proc exp))
                  ,@(map make-readable (call.args exp))))))

; MAKE-UNREADABLE does the reverse.
; It assumes there are no internal definitions.

(define (make-unreadable exp)
  (cond ((symbol? exp) (list 'begin exp))
        ((pair? exp)
         (case (car exp)
           ((quote) exp)
           ((lambda) (list 'lambda
                           (cadr exp)
                           '(begin)
                           (list '() '() '() '())
                           (make-unreadable (cons 'begin (cddr exp)))))
           ((set!) (list 'set! (cadr exp) (make-unreadable (caddr exp))))
           ((if) (list 'if
                       (make-unreadable (cadr exp))
                       (make-unreadable (caddr exp))
                       (if (= (length exp) 3)
                           '(unspecified)
                           (make-unreadable (cadddr exp)))))
           ((begin) (if (= (length exp) 2)
                        (make-unreadable (cadr exp))
                        (cons 'begin (map make-unreadable (cdr exp)))))
           (else (map make-unreadable exp))))
        (else (list 'quote exp))))

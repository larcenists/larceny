; Copyright 1991 Lightship Software
;
; 29 August 1991
;
; Integrable procedures and procedure-specific source code transformations.
; Every integrable procedure that takes a varying number of arguments must
; supply a transformation procedure to map calls into the fixed arity
; required by the MacScheme machine instructions.

; The maximum number of fixed arguments that may be followed by a rest
; argument.  This limitation is removed by the macro expander.

(define @maxargs-with-rest-arg@ 30)

; The table of integrable procedures.
; Each entry is a list of the following items:
;
;    procedure name
;    arity
;    procedure name to be used by the disassembler
;    predicate for immediate operands (or #f)
;    primop code in the MacScheme machine

(define (prim-entry name)
  (assq name $usual-integrable-procedures$))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry)
  (car (cddddr entry)))

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

(define-inline 'abs
  (lambda (exp env)
    (cond ((not (= 2 (length exp)))
           (inline-error exp))
          ((and (not (assq '< env))
                (not (assq '-- env)))
           (m-scan `(let ((temp ,(cadr exp)))
                         (if (< temp 0)
                             (-- temp)
                             temp))
                   env))
          (else (make-call (make-variable 'abs)
                           (m-scan (cadr exp) env))))))

(define-inline 'negative?
  (lambda (exp env)
    (cond ((not (= 2 (length exp)))
           (inline-error exp))
          (else (make-call (make-variable '<)
                           (list (m-scan (cadr exp) env)
                                 (make-constant 0)))))))

(define-inline 'positive?
  (lambda (exp env)
    (cond ((not (= 2 (length exp)))
           (inline-error exp))
          (else (make-call (make-variable '>)
                           (list (m-scan (cadr exp) env)
                                 (make-constant 0)))))))

(define-inline 'list
  (lambda (exp env)
    (cond ((null? (cdr exp)) (make-constant '()))
          (else (make-call (make-variable 'cons)
                           (list (m-scan (cadr exp) env)
                                 (m-scan `(list ,@(cddr exp)) env)))))))

(define-inline 'cadddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (make-call
                               (make-variable 'cdr)
                               (list (make-call
                                      (make-variable 'cdr)
                                      (list (m-scan (cadr exp) env)))))))))))))

(define-inline 'cdddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'cdr)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (make-call
                               (make-variable 'cdr)
                               (list (m-scan (cadr exp) env)))))))))))

(define-inline 'caddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (make-call
                               (make-variable 'cdr)
                               (list (m-scan (cadr exp) env)))))))))))

(define-inline 'cddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'cdr)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'cdar
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'cdr)
                 (list (make-call
                        (make-variable 'car)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'cadr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'caar
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'car)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'make-vector
  (lambda (exp env)
    (cond ((= 2 (length exp))
           (m-scan `(make-vector ,(cadr exp) '()) env))
          ((= 3 (length exp))
           (make-call (make-variable 'make-vector)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (inline-error exp)))))

(define-inline 'make-string
  (lambda (exp env)
    (cond ((= 2 (length exp))
           (m-scan `(make-string ,(cadr exp) #\space) env))
          ((= 3 (length exp))
           (make-call (make-variable 'make-string)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (inline-error exp)))))

(define-inline 'integer->char
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          ((and (fixnum? (cadr exp))
                (<= 0 (cadr exp) 255))
           (make-constant (integer->char (cadr exp))))
          (else (make-call (make-variable 'integer->char)
                           (list (m-scan (cadr exp) env)))))))

(define-inline 'char->integer
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          ((char? (cadr exp))
           (make-constant (char->integer (cadr exp))))
          (else (make-call (make-variable 'char->integer)
                           (list (m-scan (cadr exp) env)))))))

(define-inline '=
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '=)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (= ,(cadr exp) ,TEMP)
                                     (= ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '<
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '<)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (< ,(cadr exp) ,TEMP)
                                     (< ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '>
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '>)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (> ,(cadr exp) ,TEMP)
                                     (> ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '<=
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '<=)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (<= ,(cadr exp) ,TEMP)
                                     (<= ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '>=
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '>=)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (>= ,(cadr exp) ,TEMP)
                                     (>= ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '+
  (lambda (exp env)
    (define (fold args val)
      (cond ((null? args) (make-constant val))
            ((and (constant? (car args))
                  (fixnum? (constant.value (car args)))
                  (fixnum? (+ val (constant.value (car args)))))
             (fold (cdr args) (+ val (constant.value (car args)))))
            ((and (null? (cdr args))
                  (zero? val))
             (car args))
            ((null? (cdr args))
             (make-call (make-variable '+)
                        (list (car args) (make-constant val))))
            ((and (constant? (cadr args))
                  (fixnum? (constant.value (cadr args)))
                  (fixnum? (+ val (constant.value (cadr args)))))
             (fold (cons (car args) (cddr args))
                   (+ val (constant.value (cadr args)))))
            (else (fold (cons (make-call (make-variable '+)
                                         (list (car args) (cadr args)))
                              (cddr args))
                        val))))
    (fold (map (lambda (x) (m-scan x env)) (cdr exp)) 0)))

(define-inline '*
  (lambda (exp env)
    (define (fold args val)
      (cond ((null? args) (make-constant val))
            ((and (constant? (car args))
                  (fixnum? (constant.value (car args)))
                  (fixnum? (* val (constant.value (car args)))))
             (fold (cdr args) (* val (constant.value (car args)))))
            ((and (null? (cdr args))
                  (= 1 val))
             (car args))
            ((null? (cdr args))
             (make-call (make-variable '*)
                        (list (car args) (make-constant val))))
            ((and (constant? (cadr args))
                  (fixnum? (constant.value (cadr args)))
                  (fixnum? (* val (constant.value (cadr args)))))
             (fold (cons (car args) (cddr args))
                   (* val (constant.value (cadr args)))))
            (else (fold (cons (make-call (make-variable '*)
                                         (list (car args) (cadr args)))
                              (cddr args))
                        val))))
    (fold (map (lambda (x) (m-scan x env)) (cdr exp)) 1)))

(define-inline '-
  (lambda (exp env)
    (define (fold args val)
      (cond ((and (null? (cdr args))
                  (zero? val))
             (car args))
            ((null? (cdr args))
             (make-call (make-variable '-)
                        (list (car args) (make-constant val))))
            ((and (constant? (cadr args))
                  (fixnum? (constant.value (cadr args)))
                  (fixnum? (+ val (constant.value (cadr args)))))
             (fold (cons (car args) (cddr args))
                   (+ val (constant.value (cadr args)))))
            (else (fold (cons (make-call (make-variable '-)
                                         (list (car args) (cadr args)))
                              (cddr args))
                        val))))
    (cond ((null? (cdr exp)) (inline-error exp))
          ((null? (cdr (cdr exp)))
           (make-call (make-variable '--)
                      (list (m-scan (cadr exp) env))))
          (else (fold (map (lambda (x) (m-scan x env)) (cdr exp)) 0)))))

(define-inline '/
  (lambda (exp env)
    (cond ((null? (cdr exp))
           (inline-error exp))
          ((null? (cddr exp))
           (m-scan `(/ 1 ,(cadr exp)) env))
          ((null? (cdddr exp))
           (make-call (make-variable '/)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (m-scan `(/ (/ ,(cadr exp) ,(caddr exp)) ,@(cdddr exp))
                        env)))))

(define-inline 'eq?
  (lambda (exp env)
    (cond ((not (= 3 (length exp)))
           (inline-error exp))
          ((fixnum? (cadr exp))
           (m-scan `(eq? ,(caddr exp) ,(cadr exp)) env))
          (else (make-call (make-variable 'eq?)
                           (list (m-scan (cadr exp) env)
                                 (m-scan (caddr exp) env)))))))

(define-inline 'eqv?
  (lambda (exp env)
    (cond ((not (= 3 (length exp)))
           (inline-error exp))
          (else (make-call
                 (let ((arg1 (cadr exp))
                       (arg2 (caddr exp)))
                   (if (or (boolean? arg1)
                           (boolean? arg2)
                           (fixnum? arg1)
                           (fixnum? arg2)
                           (char? arg1)
                           (char? arg2)
                           (and (pair? arg1)
                                (eq? (car arg1) 'quote)
                                (pair? (cdr arg1))
                                (let ((x (cadr arg1)))
                                  (or (boolean? x)
                                      (fixnum? x)
                                      (char? x)
                                      (null? x)
                                      (symbol? x))))
                           (and (pair? arg2)
                                (eq? (car arg2) 'quote)
                                (pair? (cdr arg2))
                                (let ((x (cadr arg2)))
                                  (or (boolean? x)
                                      (fixnum? x)
                                      (char? x)
                                      (null? x)
                                      (symbol? x)))))
                       (make-variable 'eq?)
                       (make-variable 'eqv?)))
                 (list (m-scan (cadr exp) env)
                       (m-scan (caddr exp) env)))))))



; Macros for cross-compilation.

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

(for-each (lambda (x) (install-macro (car x) (cadr x)))
          **macros**)

(define-macro undefined
  (lambda (l) `',hash-bang-unspecified))

(define-macro or
  (lambda (l)
    (cond ((null? (cdr l)) '#f)
          ((null? (cddr l)) (cadr l))
          (else (let ((temp (gensym "T")))
                  `(let ((,temp ,(cadr l)))
                        (if ,temp ,temp (or ,@(cddr l)))))))))

(define-macro cond
  (lambda (l)
    (if (null? (cdr l))
        `',hash-bang-unspecified
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

; This generates pretty terrible code for named let.
; Ought to fix it someday.

(define-macro let
  (lambda (l)
    (if (and (atom? (cadr l))
             (not (null? (cadr l))))
        (cons (list 'letrec                  ; named let
                    (list (list (cadr l)
                                (cons 'lambda
                                      (cons (map car (caddr l))
                                            (cdddr l)))))
                    (cadr l))
              (map (lambda (x) (cadr x)) (caddr l)))
        (cons (cons 'lambda               ; standard let
                    (cons (map car (cadr l))
                          (cddr l)))
              (map (lambda (x) (cadr x)) (cadr l))))))

(define-macro letrec
  (lambda (l)
    (let ((bindings (sort (cadr l)
                          (lambda (x y)
                            (and (pair? (cadr x))
                                 (eq? (car (cadr x)) 'lambda)))))
          (body (cddr l)))
      (cons (list 'lambda
                  (map car bindings)
                  (cons 'begin
                        (map (lambda (x) (cons 'set! x)) bindings))
                  (cons 'let (cons '() body)))
            (map (lambda (x) '(undefined)) bindings)))))

(define-macro do
  (let ((oops (lambda (l) 
                (error "Malformed do expression" l))))
    (lambda (l)
      (if (not (proper-list? l)) (oops l))
      (if (<? (length l) 3) (oops l))
      (if (not (proper-list? (cadr l))) (oops l))
      (if (not (and (proper-list? (caddr l)) (pair? (caddr l))))
          (oops l))
      (let
        ((loop (gensym "DO"))
         (bindings (map (lambda (x)
                          (cond ((atom? x) (oops l))
                                ((atom? (cdr x))
                                 (list (car x) '() (car x)))
                                ((atom? (cddr x))
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
                           '(() ())
                           (make-unreadable (cons 'begin (cddr exp)))))
           ((set!) (list 'set! (cadr exp) (make-unreadable (caddr exp))))
           ((if) (list 'if
                       (make-unreadable (cadr exp))
                       (make-unreadable (caddr exp))
                       (if (= (length exp) 3)
                           (list 'quote hash-bang-unspecified)
                           (make-unreadable (cadddr exp)))))
           ((begin) (if (= (length exp) 2)
                        (make-unreadable (cadr exp))
                        (cons 'begin (map make-unreadable (cdr exp)))))
           (else (map make-unreadable exp))))
        (else (list 'quote exp))))

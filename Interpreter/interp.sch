; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny's interpreter.
;
; Description
;   INTERPRET takes an expression and a top-level environment and
;   evaluates the expression in the environment.  It returns the
;   result of the evaluation, and may change the environment argument.
;
;   INTERPRET accepts full R4RS Scheme, but requires a procedure
;   stored in the parameter MACRO-EXPANDER that converts full Scheme
;   to its core form with all constants quoted.
;
; Technique
;   The interpreter works by preprocessing the source code into a
;   procedure that takes a run-time environment as its only argument.
;   The procedure in turn calls procedures representing
;   subexpressions, and so on, so all steps in the interpretation are
;   translated into direct function calls, with roughly one call per
;   subexpression.  This technique strongly resembles the use of
;   threaded code.
;
;   By judiciously translating common cases specially and using some
;   caching (using local transformations only), much interpretive
;   overhead is avoided.  A number of further optimizations are
;   possible; see the to-do list.
;
;   Run-time lexical environments are lists of vectors, in inside-out
;   lexical order.  Slot 0 of every rib is the procedure: this makes
;   the environment self-describing, which aids debugging.
;
;   See also Feeley and LaPalme, "Using closures for code generation",
;   Journal of Programming Languages, 1989.
;
; Bugs
;   It's possible that there should be some restrictions here for
;   certain to-level environment arguments; for example, some
;   environments are immutable (cf the Report).  We can detect
;   immutable arguments at preprocessing time and generate code for
;   setglbl that signals an error if executed (and perhaps a warning
;   during preprocessing).
;
; Note
;   The ADT returned by the expander stored in MACRO-EXPANDER is
;   defined in Twobit/pass2.aux.sch.

($$trace "interpret")

;;; This entry point interprets full R4RS Scheme represented as list
;;; structure.  The macro expander is used to convert this to the core
;;; Scheme ADT as defined in Twobit/pass2.aux.sch.  The resulting core
;;; Scheme code is then preprocessed and executed.

(define (interpret expr env)
  (let ((expanded-form ((macro-expander) expr env)))
    (interpret-code-object expanded-form env)))

;;; This entry point interprets core Scheme code as specified in
;;; Twobit/pass2.aux.sch
(define (interpret-code-object code-object env)
  ((interpret/preprocess code-object
                         '()
                         (lambda (sym)
                           (environment-get-cell env sym))
                         #f)
   '()))

(define (interpret/preprocess expr env find-global proc-doc)
  (cond ((variable? expr)
         (let ((address (interpret/var-address (variable.name expr) env)))
           (if address
               (interpret/lexical (car address) (cdr address))
               (interpret/global (variable.name expr) find-global
                                 expr proc-doc))))
        ((constant? expr)
         (interpret/const (constant.value expr) expr proc-doc))
        ((assignment? expr)
         (let* ((lhs (assignment.lhs expr))
                (address (interpret/var-address lhs env))
                (rhs (interpret/preprocess (assignment.rhs expr) env
                                           find-global
                                           proc-doc)))
           (if address
               (interpret/setlex (car address) (cdr address) rhs)
               (interpret/setglbl lhs rhs find-global))))
        ((lambda? expr)
         (interpret/make-proc expr env find-global expr))
        ((begin? expr)
         (interpret/sequence
          (map (lambda (x)
                 (interpret/preprocess x env find-global proc-doc))
               (begin.exprs expr))
          expr
          proc-doc))
        ((conditional? expr)
         (let ((test (interpret/preprocess (if.test expr) env
                                           find-global proc-doc))
               (*then (interpret/preprocess (if.then expr) env
                                            find-global proc-doc))
               (*else (interpret/preprocess (if.else expr) env
                                            find-global proc-doc)))
           (interpret/if test *then *else expr proc-doc)))
        ((call? expr)
         (interpret/make-call expr env find-global expr proc-doc))
        (else
         (error "INTERPRET/PREPROCESS: unknown expression: " expr)
         #t)))


; Closure creation.  Special cases handled:
;  - procedures of 0..4 arguments.
;  - varargs procedures.

(define (interpret/make-proc expr env find-global src)

  (define (listify x)
    (cond ((pair? x) (cons (car x) (listify (cdr x))))
          ((null? x) x)
          (else (list x))))

  (define (fixed-args x n)
    (if (pair? x)
        (fixed-args (cdr x) (+ n 1))
        n))

  (let* ((args  (lambda.args expr))
         (body  (lambda.body expr))
         (doc   (lambda.doc expr))
         (src   (if doc (doc.code doc) #f))
         (nenv  (interpret/extend-env env (listify args)))
         (exprs (interpret/preprocess body nenv find-global doc)))
    (cond ((pair? args)
           (let ((tail0 (cdr args)))
             (cond ((pair? tail0)
                    (let ((tail1 (cdr tail0)))
                      (cond ((pair? tail1)
                             (let ((tail2 (cdr tail1)))
                               (cond ((pair? tail2)
                                      (let ((tail3 (cdr tail2)))
                                        (cond ((pair? tail3)
                                               (if (list? tail3)
                                                   (interpret/lambda-n (length args) exprs doc src)
                                                   (interpret/lambda-dot (fixed-args args 0) exprs doc src)))
                                              ((null? tail3) (interpret/lambda4 exprs doc src))
                                              (else (interpret/lambda-dot (fixed-args args 0) exprs doc src)))))
                                     ((null? tail2) (interpret/lambda3 exprs doc src))
                                     (else (interpret/lambda-dot (fixed-args args 0) exprs doc src)))))
                            ((null? tail1) (interpret/lambda2 exprs doc src))
                            (else (interpret/lambda-dot (fixed-args args 0) exprs doc src)))))
                   ((null? tail0) (interpret/lambda1 exprs doc src))
                   (else (interpret/lambda-dot (fixed-args args 0) exprs doc src)))))
          ((null? args) (interpret/lambda0 exprs doc src))
          (else (interpret/lambda-dot (fixed-args args 0) exprs doc src)))))

; Procedure call.  Special cases handled:
;  - primitive: (op a b ...)
;  - short: 0..4 arguments

(define (interpret/make-call expr env find-global src doc)
  (let ((n (length (call.args expr))))
    (cond ((<= n 4)
           (interpret/invoke-short (call.proc expr) (call.args expr) n env find-global src doc))
          (else
           (interpret/invoke-n (call.proc expr) (call.args expr) env find-global src doc)))))

(define (interpret/extend-env env names)
  (cons names env))

(define (interpret/var-address name env)
  (let r-loop ((env env) (i 0))
    (if (null? env)
        #f
        (let a-loop ((rib (car env)) (j 1))   ; slot 0 is procedure
          (cond ((null? rib)
                 (r-loop (cdr env) (+ i 1)))
                ((eq? (car rib) name)
                 (cons i j))
                (else
                 (a-loop (cdr rib) (+ j 1))))))))

(define (interpret/global name find-global src proc-doc)
  (let ((cell (find-global name)))
    (interpreted-expression
     (lambda (env)
       (let ((v (car cell)))
         (if (eq? v (undefined))
             (begin
               (error "Reference to undefined global variable \"" name "\".")
               #t)
             v)))
     (cons src proc-doc))))

(define (interpret/setglbl name expr find-global)
  (let ((cell (find-global name)))
    (lambda (env)
      (set-car! cell (expr env)))))

; Unroll loop for the closest ribs.
; Go deep because we actually hit the default case fairly often.
(define (interpret/lexical rib offset)
  (case rib
    ((0) (interpret/lexical0 offset))
    ((1) (interpret/lexical1 offset))
    ((2) (interpret/lexical2 offset))
    ((3) (interpret/lexical3 offset))
    ((4) (interpret/lexical4 offset))
    ((5) (interpret/lexical5 offset))
    ((6) (interpret/lexical6 offset))
    ((7) (interpret/lexical7 offset))
    (else (interpret/lexical-n rib offset))))

(define (interpret/lexical0 offset)
  (lambda (env)
    (vector-ref (car env) offset)))

(define (interpret/lexical1 offset)
  (lambda (env)
    (vector-ref (cadr env) offset)))

(define (interpret/lexical2 offset)
  (lambda (env)
    (vector-ref (caddr env) offset)))

(define (interpret/lexical3 offset)
  (lambda (env)
    (vector-ref (cadddr env) offset)))

(define (interpret/lexical4 offset)
  (lambda (env)
    (vector-ref (car (cddddr env)) offset)))

(define (interpret/lexical5 offset)
  (lambda (env)
    (vector-ref (cadr (cddddr env)) offset)))

(define (interpret/lexical6 offset)
  (lambda (env)
    (vector-ref (caddr (cddddr env)) offset)))

(define (interpret/lexical7 offset)
  (lambda (env)
    (vector-ref (cadddr (cddddr env)) offset)))

(define (interpret/lexical-n rib offset)
  (lambda (env0)
    (let loop ((rib rib) (env env0))
      (if (= rib 0)
          (vector-ref (car env) offset)
          (loop (- rib 1) (cdr env))))))

(define (interpret/setlex rib offset expr)
  (lambda (env0)
    (let loop ((rib rib) (env env0))
      (if (= rib 0)
          (vector-set! (car env) offset (expr env0))
          (loop (- rib 1) (cdr env))))))

(define (interpret/const c src doc)
  (interpreted-expression
   (lambda (env)
     c)
   (cons src doc)))

(define (interpret/if test consequent alternate src proc-doc)
  (interpreted-expression
   (lambda (env)
     (if (test env) (consequent env) (alternate env)))
   (cons src proc-doc)))

; Special cases: 1..4 expressions.

(define (interpret/sequence exprs src proc-doc)
  (let ((doc (cons src proc-doc)))
    (case (length exprs)
      ((1) (car exprs))
      ((2) (interpret/sequence2 (car exprs) (cadr exprs) doc))
      ((3) (interpret/sequence3 (car exprs) (cadr exprs) (caddr exprs) doc))
      ((4) (interpret/sequence4 (car exprs) (cadr exprs) (caddr exprs)
                                (cadddr exprs) doc))
      (else (interpret/sequence-n exprs doc)))))

(define (interpret/sequence2 a b doc)
  (interpreted-expression
   (lambda (env)
     (a env) (b env))
   doc))

(define (interpret/sequence3 a b c doc)
  (interpreted-expression
   (lambda (env)
     (a env) (b env) (c env))
   doc))

(define (interpret/sequence4 a b c d doc)
  (interpreted-expression
   (lambda (env)
     (a env) (b env) (c env) (d env))
   doc))

(define (interpret/sequence-n exprs doc)
  (interpreted-expression
   (lambda (env)
     (let loop ((exprs exprs))
       (cond ((null? (cdr exprs))
              ((car exprs) env))
             (else
              ((car exprs) env)
              (loop (cdr exprs))))))
   doc))

(define (interpret/invoke-prim1 name a find-global)
  ((interpret/primitive name 1) a (interpret/prim-orig name)
                                (find-global name)))

(define (interpret/invoke-prim2 name a b find-global)
  ((interpret/primitive name 2) a b (interpret/prim-orig name)
                                (find-global name)))

; Calls that take 0..4 arguments.

(define (interpret/invoke-short rator rands n env find-global src proc-doc)
  (let ((proc (interpret/preprocess rator env find-global proc-doc))
        (args (map (lambda (rand)
                     (interpret/preprocess rand env find-global proc-doc))
                   rands)))

    (define (prim? op)
      (and (symbol? op)
           (not (interpret/var-address op env)) ; No longer necessary, I think
           (interpret/primitive? op n)))

    (let ((op (if (variable? rator)
                  (variable.name rator)
                  rator))
          (doc (cons src proc-doc)))
      (case n
        ((0) (interpret/invoke0 proc doc))
        ((1) (if (prim? op)
                 (interpret/invoke-prim1 op (car args) find-global)
                 (interpret/invoke1 proc (car args) doc)))
        ((2) (if (prim? op)
                 (interpret/invoke-prim2 op (car args) (cadr args) find-global)
                 (interpret/invoke2 proc (car args) (cadr args) doc)))
        ((3) (interpret/invoke3 proc (car args) (cadr args) (caddr args) doc))
        ((4) (interpret/invoke4 proc (car args) (cadr args) (caddr args)
                                (cadddr args) doc))
        (else
         (error "impossible case in interpret/invoke-short: " n op))))))


(define (interpret/invoke0 proc doc)
  (interpreted-expression
   (lambda (env)
     (let ((proc (proc env)))
       (proc)))
   doc))

(define (interpret/invoke1 proc a doc)
  (interpreted-expression
   (lambda (env)
     (let ((proc (proc env))
           (arg0 (a env)))
       (proc arg0)))
   doc))

(define (interpret/invoke2 proc a b doc)
  (interpreted-expression
   (lambda (env)
     (let ((proc (proc env))
           (arg0 (a env))
           (arg1 (b env)))
       (proc arg0 arg1)))
   doc))

(define (interpret/invoke3 proc a b c doc)
  (interpreted-expression
   (lambda (env)
     (let ((proc (proc env))
           (arg0 (a env))
           (arg1 (b env))
           (arg2 (c env)))
       (proc arg0 arg1 arg2)))
   doc))

(define (interpret/invoke4 proc a b c d doc)
  (interpreted-expression
   (lambda (env)
     (let ((proc (proc env))
           (arg0 (a env))
           (arg1 (b env))
           (arg2 (c env))
           (arg3 (d env)))
       (proc arg0 arg1 arg2 arg3)))
   doc))

(define (interpret/invoke-n rator rands env find-global src doc)
  (let ((proc (interpret/preprocess rator env find-global doc))
        (args (map (lambda (rand)
                     (interpret/preprocess rand env find-global doc))
                   rands)))

    (interpreted-expression
     (lambda (env)
       (let ((proc (proc env))
             (args (map (lambda (p) (p env)) args)))
         (apply proc args)))
     (cons src doc))))

; Closure creation.

(define (interpret/lambda0 body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           0
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self (interpreted-procedure
                       doc
                       (lambda ()
                         (body (cons (vector self) env))))))
         self))
     src)))

(define (interpret/lambda1 body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           1
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self (interpreted-procedure
                       doc
                       (lambda (a)
                         (body (cons (vector self a) env))))))
         self))
     src)))

(define (interpret/lambda2 body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           2
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self (interpreted-procedure
                       doc
                       (lambda (a b)
                         (body (cons (vector self a b) env))))))
         self))
     src)))

(define (interpret/lambda3 body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           3
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self (interpreted-procedure
                       doc
                       (lambda (a b c)
                         (body (cons (vector self a b c) env))))))
         self))
     src)))

(define (interpret/lambda4 body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           4
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self (interpreted-procedure
                       doc
                       (lambda (a b c d)
                         (body (cons (vector self a b c d) env))))))
         self))
     src)))

(define (interpret/lambda-n n body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           n
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self (interpreted-procedure
                       doc
                       (lambda args
                         (if (length<? args n)
                             (interpret/too-few self n (length args) 'exact)
                             (body (cons (list->vector (cons self args)) env)))))))
         self))
     src)))

; `n' is the number of fixed arguments.

(define (interpret/lambda-dot n body doc src)
  (let ((doc (or doc
                 (make-doc #f
                           (exact->inexact n)
                           #f
                           src
                           #f
                           #f))))
    (interpreted-expression
     (lambda (env)
       (letrec ((self
                 (interpreted-procedure
                  doc
                  (lambda args
                    (let ((v (make-vector (+ n 2) (unspecified)))
                          (limit (+ n 1)))
                      (vector-set! v 0 self)
                      (let loop ((argnum  1)
                                 (argtail args))
                        (cond ((= argnum limit)
                               (vector-set! v argnum argtail)
                               (body (cons v env)))
                              ((pair? argtail)
                               (vector-set! v argnum (car argtail))
                               (loop (+ argnum 1) (cdr argtail)))
                              (else (interpret/too-few self n (length args) 'inexact)))))))))
         self))
     src)))

(define (interpret/too-few proc required got exact?)
  (error "Too few arguments to procedure "
         (procedure-name proc)
         "; got "
         got
         ", wanted "
         (if exact? "" "at least ")
         required
         "."))

; Debugger support

; Interpreted-procedure, interpreted-expression, and
; interpreted-primitive can always return their procedure argument
; without any harm to the interpreter; only debugging will be affected.

; Interpreted-procedure takes a standard documentation structure and any
; procedure (currently anything) and returns a new procedure that is
; identical to the old except that the it has typetag 0 and is one
; element longer.  The new, last element contains the pair
; ($eval-lambda . <doc>) where <doc> is the documentation structure.
; This procedure is on the critical path in the interpreter and should
; do no more work than absolutely necessary.

(define (interpreted-procedure doc proc)
  (let* ((l (procedure-length proc))
         (p (make-procedure (+ l 1))))
    (do ((i 0 (+ i 1)))
        ((= i l))
      (procedure-set! p i (procedure-ref proc i)))
    (procedure-set! p l (cons '$eval-lambda doc))
    (typetag-set! p 0)
    p))

(define (interpreted-procedure? x)
  (and (procedure? x)
       (zero? (typetag x))
       (let ((last (procedure-ref x (- (procedure-length x) 1))))
         (and (pair? last)
              (eq? (car last) '$eval-lambda)))))

(define (interpreted-procedure-documentation proc)
  (cdr (procedure-ref proc (- (procedure-length proc) 1))))


; Interpreted-expression takes any procedure and a pair consisting of
; the source code for the expression and the enclosing procedure's
; documentation slot and returns a new procedure that is identical to
; the old except that the it is one element longer and has typetag 0.
;
; The new, last element contains the pair ($evalproc . <doc>) where <doc>
; is (<source> <documentation>)

(define (interpreted-expression proc doc)
  (let* ((l (procedure-length proc))
         (p (make-procedure (+ l 1))))
    (do ((i 0 (+ i 1)))
        ((= i l))
      (procedure-set! p i (procedure-ref proc i)))
    (procedure-set! p l (cons '$evalproc doc))
    (typetag-set! p 0)
    p))

(define (interpreted-expression? x)
  (and (procedure? x)
       (zero? (typetag x))
       (let ((last (procedure-ref x (- (procedure-length x) 1))))
         (and (pair? last)
              (eq? (car last) '$evalproc)))))


; Interpreted-primitive takes a name (a symbol), a number of arguments
; (a fixnum), and a procedure, and returns a new procedure that is
; identical to the old except that it is one element longer and has
; typetag 0.
; The new, last element contains the list ($evalprim <name> <argc>).

(define (interpreted-primitive name argc proc)
  (let* ((l (procedure-length proc))
         (p (make-procedure (+ l 1))))
    (do ((i 0 (+ i 1)))
        ((= i l))
      (procedure-set! p i (procedure-ref proc i)))
    (procedure-set! p l (list '$evalprim name argc))
    (typetag-set! p 0)
    p))

(define (interpreted-primitive? x)
  (and (procedure? x)
       (zero? (typetag x))
       (let ((last (procedure-ref x (- (procedure-length x) 1))))
         (and (pair? last)
              (eq? (car last) '$evalprim)))))


; Augments the definition in Lib/procinfo.sch to deal with interpreted
; procedures.

(define procedure-documentation
  (let ((procedure-documentation procedure-documentation))
    (lambda (proc . rest)
      (cond ((interpreted-procedure? proc)
             (interpreted-procedure-documentation proc))
            ((interpreted-expression? proc)
             (cdr (interpreted-procedure-documentation proc)))
            ((interpreted-primitive? proc)
             (let ((x (interpreted-procedure-documentation proc)))
               (vector (car x) #f (cadr x))))
            (else
             (apply procedure-documentation proc rest))))))

(define (interpreted-expression-source proc)
  (if (interpreted-expression? proc)
      (make-readable (car (interpreted-procedure-documentation proc)))
      (begin (error "Not an expression: " proc)
             #t)))

; eof

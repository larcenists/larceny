; Copyright 1998 William D Clinger.
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
; 7 June 1999.
;
; Conversion to A-normal form, with heuristics for
; choosing a good order of evaluation.
;
; This pass operates as a source-to-source transformation on
; expressions written in the subset of Scheme described by the
; following grammar, where the input and output expressions
; satisfy certain additional invariants described below.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F G <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the input only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  For each lambda expression, the associated G is a subset of F
;      that contains every identifier that occurs free within some
;      inner lambda expression that escapes, and possibly a few that
;      don't.  (Assignment-elimination does not calculate G exactly.)
;   *  Variables named IGNORED are neither referenced nor assigned.
;
; Invariants that hold for the output only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  R, F, and G are garbage.
;   *  There are no sequential expressions.
;   *  The output is an expression E with syntax
;
; E  -->  A
;      |  (L)
;      |  (L A)
;
; A  -->  W
;      |  L
;      |  (W_0 W_1 ...)
;      |  (set! I W)
;      |  (if W E1 E2)
;
; W  -->  (quote K)
;      |  (begin I)
;
; In other words:
; An expression is a LET* such that the rhs of every binding is
;     a conditional with the test already evaluated, or
;     an expression that can be evaluated in one step
;         (treating function calls as a single step)
;
; A-normal form corresponds to the control flow graph for a lambda
; expression.

; Algorithm: repeated use of these rules:
;
; (E0 E1 ...)                              ((lambda (T0 T1 ...) (T0 T1 ...))
;                                           E0 E1 ...)
; (set! I E)                               ((lambda (T) (set! I T)) E)
; (if E0 E1 E2)                            ((lambda (T) (if T E1 E2)) E0)
; (begin E0 E1 E2 ...)                     ((lambda (T) (begin E1 E2 ...)) E0)
;
; ((lambda (I1 I2 I3 ...) E)               ((lambda (I1)
;  E1 E2 E3)                                  ((lambda (I2 I3 ...) E)
;                                              E2 E3))
;                                           E1)
;
; ((lambda (I2) E)                         ((lambda (I1)
;  ((lambda (I1) E2)                          ((lambda (I2) E)
;   E1))                                       E2)
;                                           E1)
;
; In other words:
; Introduce a temporary name for every expression except:
;     tail expressions
;     the alternatives of a non-tail conditional
; Convert every LET into a LET*.
; Get rid of LET* on the right hand side of a binding.

; Given an expression E in the representation output by pass 2,
; returns an A-normal form for E in that representation.
; Except for quoted values, the A-normal form does not share
; mutable structure with the original expression E.
;
; OPTIONAL ARGUMENTS:
;
; The following optional arguments can be combined in either
; order.
;
; If you call A-normal-form on a form that has already been
; converted to A-normal form, then the same temporaries will
; be generated twice.  An optional argument lets you specify
; a different prefix for temporaries the second time around.
; Example:
;
; (A-normal-form (A-normal-form E ".T")
;                ".U")
;
; An optional threshold n tells A-normal-form to give up by
; returning #f if the A-normal form would contain more than
; n new temporary variables.

; This is the declaration that is used to indicate A-normal form.

(define A-normal-form-declaration (list 'anf))

; Expressions larger than this threshold are definitely complicated.

(define *anf-complication* 100)

; Expressions larger than this will not be converted to A-normal form.
; (One million corresponds to about 200,000 lines of dense source code.)

(define anf-infinity 1000000)

; If an input expression is larger than this, then its ANF size
; will be printed during compilation.

(define anf-large 50000)

(define (A-normal-form E . rest)
  
  (define (A-normal-form E)
    (anf-make-let* (anf E '() '())))
  
  ; Given an expression E as output by pass 2,
  ; a list of surrounding LET* bindings,
  ; and an ordered list of likely register variables,
  ; return a non-empty list of LET* bindings
  ; whose first binding associates a dummy variable
  ; with an A-expression giving the value for E.
  
  (define (anf E bindings regvars)
    (cond
     ((constant? E)    (anf-bind-dummy E bindings))
     ((variable? E)    (anf-bind-dummy E bindings))
     ((begin? E)       (anf-sequential E bindings regvars))
     ((lambda? E)      (anf-lambda E bindings regvars))
     ((assignment? E)  (anf-assignment E bindings regvars))
     ((conditional? E) (anf-conditional E bindings regvars))
     ((call? E)        (anf-call E bindings regvars))
     (else (twobit-bug "unrecognized expression" E))))

  (define anf:dummy (string->symbol "RESULT"))
  
  (define (anf-bind-dummy E bindings)
    (cons (list anf:dummy E)
          bindings))
  
  ; Unlike anf-bind-dummy, anf-bind-name and anf-bind convert
  ; their expression argument to A-normal form.
  ; Don't change anf-bind to call anf-bind-name, because that
  ; would name the temporaries in an aesthetically bad order.
  
  (define (anf-bind-name name E bindings regvars)
    (let ((bindings (anf E bindings regvars)))
      (cons (list name (cadr (car bindings)))
            (cdr bindings))))
  
  (define (anf-bind E bindings regvars)
    (let ((bindings (anf E bindings regvars)))
      (cons (list (newtemp) (cadr (car bindings)))
            (cdr bindings))))
  
  (define (anf-result bindings)
    (make-variable (car (car bindings))))
  
  (define (anf-make-let* bindings)
    (define (loop bindings body)
      (if (null? bindings)
          body
          (let ((T1 (car (car bindings)))
                (E1 (cadr (car bindings))))
            (loop (cdr bindings)
                  (make-call (make-lambda (list T1)
                                          '()
                                          '()
                                          '()
                                          '()
                                          (list A-normal-form-declaration)
                                          '()
                                          body)
                             (list E1))))))
    (loop (cdr bindings)
          (cadr (car bindings))))                                  
  
  (define (anf-sequential E bindings regvars)
    (do ((bindings bindings
                   (anf-bind (car exprs) bindings regvars))
         (exprs (begin.exprs E)
                (cdr exprs)))
        ((null? (cdr exprs))
         (anf (car exprs) bindings regvars))))
  
  ; Heuristic: the formal parameters of an escaping lambda or
  ; known local procedure are kept in REG1, REG2, et cetera.
  
  (define (anf-lambda L bindings regvars)
    (anf-bind-dummy
     (make-lambda (lambda.args L)
                  (map (lambda (def)
                         (make-definition
                          (def.lhs def)
                          (A-normal-form (def.rhs def))))
                       (lambda.defs L))
                  '()
                  '()
                  '()
                  (cons A-normal-form-declaration
                        (lambda.decls L))
                  (lambda.doc L)
                  (anf-make-let*
                   (anf (lambda.body L)
                        '()
                        (make-null-terminated (lambda.args L)))))
     bindings))
  
  (define (anf-assignment E bindings regvars)
    (let ((I (assignment.lhs E))
          (E1 (assignment.rhs E)))
      (if (variable? E1)
          (anf-bind-dummy E bindings)
          (let* ((bindings (anf-bind E1 bindings regvars))
                 (T1 (anf-result bindings)))
            (anf-bind-dummy (make-assignment I T1) bindings)))))
  
  (define (anf-conditional E bindings regvars)
    (let ((E0 (if.test E))
          (E1 (if.then E))
          (E2 (if.else E)))
      (if (variable? E0)
          (let ((E1 (anf-make-let* (anf E1 '() regvars)))
                (E2 (anf-make-let* (anf E2 '() regvars))))
            (anf-bind-dummy
             (make-conditional E0 E1 E2)
             bindings))
          (let* ((bindings (anf-bind E0 bindings regvars))
                 (E1 (anf-make-let* (anf E1 '() regvars)))
                 (E2 (anf-make-let* (anf E2 '() regvars))))
            (anf-bind-dummy
             (make-conditional (anf-result bindings) E1 E2)
             bindings)))))
  
  (define (anf-call E bindings regvars)
    (let* ((proc (call.proc E))
           (args (call.args E)))
      
      ; Evaluates the exprs and returns both a list of bindings and
      ; a list of the temporaries that name the results of the exprs.
      ; If rename-always? is true, then temporaries are generated even
      ; for constants and temporaries.
      
      (define (loop exprs bindings names rename-always?)
        (if (null? exprs)
            (values bindings (reverse names))
            (let ((E (car exprs)))
              (if (or rename-always?
                      (not (or (constant? E)
                               (variable? E))))
                  (let* ((bindings
                          (anf-bind (car exprs) bindings regvars)))
                    (loop (cdr exprs)
                          bindings
                          (cons (anf-result bindings) names)
                          rename-always?))
                  (loop (cdr exprs)
                        bindings
                        (cons E names)
                        rename-always?)))))
      
      ; Evaluates the exprs, binding them to the vars, and returns
      ; a list of bindings.
      ;
      ; Although LET variables are likely to be kept in registers,
      ; trying to guess which register will be allocated is likely
      ; to do more harm than good.
      
      (define (let-loop exprs bindings regvars vars)
        (if (null? exprs)
            (if (null? (lambda.defs proc))
                (anf (lambda.body proc)
                     bindings
                     regvars)
                (let ((bindings
                       (anf-bind
                        (make-lambda '()
                                     (lambda.defs proc)
                                     '()
                                     '()
                                     '()
                                     (cons A-normal-form-declaration
                                           (lambda.decls proc))
                                     (lambda.doc proc)
                                     (lambda.body proc))
                        bindings
                        '())))
                  (anf-bind-dummy
                   (make-call (anf-result bindings) '())
                   bindings)))
            (let-loop (cdr exprs)
              (anf-bind-name (car vars)
                             (car exprs)
                             bindings
                             regvars)
              regvars
              (cdr vars))))
      
      (cond ((lambda? proc)
             (let ((formals (lambda.args proc)))
               (if (list? formals)
                   (let* ((pi (anf-order-of-evaluation args regvars #f))
                          (exprs (permute args pi))
                          (names (permute (lambda.args proc) pi)))
                     (let-loop (reverse exprs) bindings regvars (reverse names)))
                   (anf-call (normalize-let E) bindings regvars))))
            
            ((not (variable? proc))
             (let ((pi (anf-order-of-evaluation args regvars #f)))
               (call-with-values
                (lambda () (loop (permute args pi) bindings '() #t))
                (lambda (bindings names)
                  (let ((bindings (anf-bind proc bindings regvars)))
                    (anf-bind-dummy
                     (make-call (anf-result bindings)
                                (unpermute names pi))
                     bindings))))))
            
            ((prim-entry (variable.name proc))
             (let ((pi (anf-order-of-evaluation args regvars #t)))
               (call-with-values
                (lambda () (loop (permute args pi) bindings '() #t))
                (lambda (bindings names)
                  (anf-bind-dummy
                   (make-call proc (unpermute names pi))
                   bindings)))))
            
            ((memq (variable.name proc) regvars)
             (let* ((exprs (cons proc args))
                    (pi (anf-order-of-evaluation
                         exprs
                         (cons name:IGNORED regvars)
                         #f)))
               (call-with-values
                (lambda () (loop (permute exprs pi) bindings '() #t))
                (lambda (bindings names)
                  (let ((names (unpermute names pi)))
                    (anf-bind-dummy
                     (make-call (car names) (cdr names))
                     bindings))))))
            
            (else
             (let ((pi (anf-order-of-evaluation args regvars #f)))
               (call-with-values
                (lambda () (loop (permute args pi) bindings '() #t))
                (lambda (bindings names)
                  (anf-bind-dummy
                   (make-call proc (unpermute names pi))
                   bindings))))))))
  
  ; Given a list of expressions, a list of likely register contents,
  ; and a switch telling whether these are arguments for a primop
  ; or something else (such as the arguments for a real call),
  ; try to choose a good order in which to evaluate the expressions.
  ;
  ; Heuristic:  If none of the expressions is a call to a non-primop,
  ; then parallel assignment optimization gives a good order if the
  ; regvars are right, and should do no worse than a random order if
  ; the regvars are wrong.
  ;
  ; Heuristic:  If the expressions are arguments to a primop, and
  ; none are a call to a non-primop, then the register contents
  ; are irrelevant, and the first argument should be evaluated last.
  ;
  ; Heuristic:  If one or more of the expressions is a call to a
  ; non-primop, then the following should be a good order:
  ;
  ;     expressions that are neither a constant, variable, or a call
  ;     calls to non-primops
  ;     constants and variables
  
  (define (anf-order-of-evaluation exprs regvars for-primop?)
    (define (ordering targets exprs alist)
      (let ((para
             (parallel-assignment targets alist exprs)))
        (or para
            ; Evaluate left to right until a parallel assignment is found.
            (cons (car targets)
                  (ordering (cdr targets)
                            (cdr exprs)
                            alist)))))
    (if (parallel-assignment-optimization)
        (cond ((null? exprs) '())
              ((null? (cdr exprs)) '(0))
              (else
               (let* ((contains-call? #f)
                      (vexprs (list->vector exprs))
                      (vindexes (list->vector
                                 (iota (vector-length vexprs))))
                      (contains-call? #f)
                      (categories
                       (list->vector
                        (map (lambda (E)
                               (cond ((constant? E)
                                      2)
                                     ((variable? E)
                                      2)
                                     ((complicated? E)
                                      (set! contains-call? #t)
                                      1)
                                     (else
                                      0)))
                             exprs))))
                 (cond (contains-call?
                        (twobit-sort (lambda (i j)
                                       (< (vector-ref categories i)
                                          (vector-ref categories j)))
                                     (iota (length exprs))))
                       (for-primop?
                        (reverse (iota (length exprs))))
                       (else
                        (let ((targets (iota (length exprs))))
                          (define (pairup regvars targets)
                            (if (or (null? targets)
                                    (null? regvars))
                                '()
                                (cons (cons (car regvars)
                                            (car targets))
                                      (pairup (cdr regvars)
                                              (cdr targets)))))
                          (ordering targets
                                    exprs
                                    (pairup regvars targets))))))))
        (iota (length exprs))))
  
  (define (permute things pi)
    (let ((v (list->vector things)))
      (map (lambda (i) (vector-ref v i))
           pi)))
  
  (define (unpermute things pi)
    (let* ((v0 (list->vector things))
           (v1 (make-vector (vector-length v0))))
      (do ((pi pi (cdr pi))
           (k 0 (+ k 1)))
          ((null? pi)
           (vector->list v1))
          (vector-set! v1 (car pi) (vector-ref v0 k)))))
  
  ; Given a call whose procedure is a lambda expression that has
  ; a rest argument, return a genuine let expression.
  
  (define (normalize-let exp)
    (let* ((L (call.proc exp)))
      (let loop ((formals (lambda.args L))
                 (args (call.args exp))
                 (newformals '())
                 (newargs '()))
        (cond ((null? formals)
               (if (null? args)
                   (begin (lambda.args-set! L (reverse newformals))
                          (call.args-set! exp (reverse newargs))
                          exp)
                   (begin (normalize-let-error exp)
                          (loop (list (newtemp))
                                args
                                newformals
                                newargs))))
              ((pair? formals)
               (if (pair? args)
                   (loop (cdr formals)
                         (cdr args)
                         (cons (car formals) newformals)
                         (cons (car args) newargs))
                   (begin (normalize-let-error exp)
                          (loop formals
                                (cons (make-constant 0)
                                      args)
                                newformals
                                newargs))))
              (else
               (loop (list formals)
                     (list (make-call-to-LIST args))
                     newformals
                     newargs))))))
  
  (define (normalize-let-error exp)
    (twobit-error "Wrong number of arguments to lambda expression"
                  (make-readable exp #t)))
  
  ; For heuristic use only.
  ; An expression is complicated unless it can probably be evaluated
  ; without saving and restoring any registers, even if it occurs in
  ; a non-tail position.
  
  (define (complicated? exp)
    ; Let's not spend all day on this.
    (let ((budget *anf-complication*))
      (define (complicated? exp)
        (set! budget (- budget 1))
        (if (zero? budget)
            #t
            (cond
              ((constant? exp)    #f)
              ((lambda? exp)      #f)
              ((assignment? exp)  (complicated? (assignment.rhs exp)))
              ((conditional? exp) (or (complicated? (if.test exp))
                                      (complicated? (if.then exp))
                                      (complicated? (if.else exp))))
              ((variable? exp)    #f)
              ((begin? exp)       (some? complicated?
                                         (begin.exprs exp)))
              ((call? exp)
               (let ((proc (call.proc exp)))
                 (if (cond ((variable? proc)
                            (prim-entry (variable.name proc)))
                           ((lambda? proc)
                            (not (complicated? (lambda.body proc))))
                           (else #f))
                     (some? complicated?
                            (call.args exp))
                     #t)))
              (else (twobit-bug "Unrecognized expression" exp)))))
      (complicated? exp)))
  
  ; New temporaries.
  
  (define (newtemp)
    (set! temp-counter (+ temp-counter 1))
    (if (> temp-counter temp-threshold)
        (return))
    (string->symbol
     (string-append temp-prefix
                    (number->string temp-counter))))
  
  (define temp-prefix
    (let ()
      (define (default-prefix)
        (string-append renaming-prefix "T"))
      (cond ((or (null? rest)
                 (and (null? (cdr rest))
                      (not (string? (car rest)))))
             (default-prefix))
            ((string? (car rest))
             (car rest))
            ((and (pair? (cdr rest))
                  (string? (cadr rest)))
             (cadr rest))
            (else
             (default-prefix)))))

  (define temp-threshold
    (cond ((null? rest) anf-infinity)
          ((number? (car rest))
           (car rest))
          ((null? (cdr rest))
           anf-infinity)
          ((number? (cadr rest))
           (cadr rest))
          (else
           anf-infinity)))
  
  (define temp-counter 0)

  (define return (lambda () #f))

  (call-with-current-continuation
   (lambda (k)
     (set! return (lambda () (k #f)))
     (let ((anf (A-normal-form E)))
       (if (> temp-counter anf-large)
           (twobit-warn (string-append "ANF size: "
                                       (number->string temp-counter))))
       anf))))

; Copyright 1991 William D Clinger.
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
; 14 September 2000.
;
; Second pass of the Twobit compiler:
;   single assignment analysis, local source transformations,
;   assignment elimination, and lambda lifting.
; The code for assignment elimination and lambda lifting
; are in a separate file.
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
;   *  There are no internal definitions.
;   *  No identifier containing an upper case letter is bound anywhere.
;      (Change the "name:..." variables if upper case is preferred.)
;   *  No identifier is bound in more than one place.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.
;
; Invariants that hold for the output only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  For each lambda expression, the associated G is a subset of F
;      that contains every identifier that occurs free within some
;      inner lambda expression that escapes, and possibly a few that
;      don't.  (Assignment-elimination does not calculate G exactly.)
;   *  Variables named IGNORED are neither referenced nor assigned.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.

(define (pass2 exp)
  (simplify exp (make-notepad #f)))

; Given an expression and a "notepad" data structure that conveys
; inherited attributes, performs the appropriate optimizations and
; destructively modifies the notepad to record various attributes
; that it synthesizes while traversing the expression.  In particular,
; any nested lambda expressions and any variable references will be
; noted in the notepad.

(define (simplify exp notepad)
  (cond ((constant? exp) exp)
        ((lambda? exp) (simplify-lambda exp notepad))
        ((assignment? exp) (simplify-assignment exp notepad))
        ((conditional? exp) (simplify-conditional exp notepad))
        ((variable? exp) (notepad-var-add! notepad (variable.name exp)) exp)
        ((begin? exp) (simplify-sequential exp notepad))
        ((call? exp) (simplify-call exp notepad))
        (else (error "Unrecognized expression" exp))))

;(define (simplify exp notepad)
;  (case (car exp)
;    ((quote)    exp)
;    ((lambda)   (simplify-lambda exp notepad))
;    ((set!)     (simplify-assignment exp notepad))
;    ((if)       (simplify-conditional exp notepad))
;    ((begin)    (if (variable? exp)
;                    (begin (notepad-var-add! notepad (variable.name exp))
;                           exp)
;                    (simplify-sequential exp notepad)))
;    (else       (simplify-call exp notepad))))

; Most optimization occurs here.
; The  right hand sides of internal definitions are simplified,
; as is the body.
; Internal definitions of enclosed lambda expressions may
; then be lifted to this one.
; Single assignment analysis creates internal definitions.
; Single assignment elimination converts single assignments
; to bindings where possible, and renames arguments whose value
; is ignored.
; Assignment elimination then replaces all remaining assigned
; variables by heap-allocated cells.

(define (simplify-lambda exp notepad)
  (notepad-lambda-add! notepad exp)
  (let ((defs (lambda.defs exp))
        (body (lambda.body exp))
        (newnotepad (make-notepad exp)))
    (for-each (lambda (def)
                (simplify-lambda (def.rhs def) newnotepad))
              defs)
    (lambda.body-set! exp (simplify body newnotepad))
    (lambda.F-set! exp (notepad-free-variables newnotepad))
    (lambda.G-set! exp (notepad-captured-variables newnotepad))
    (single-assignment-analysis exp newnotepad)
    (let ((known-lambdas (notepad.nonescaping newnotepad)))
      (for-each (lambda (L)
                  (if (memq L known-lambdas)
                      (lambda-lifting L exp)
                      (lambda-lifting L L)))
                (notepad.lambdas newnotepad))))
  (single-assignment-elimination exp notepad)
  (assignment-elimination exp)
  (if (not (notepad.parent notepad))
      ; This is an outermost lambda expression.
      (lambda-lifting exp exp))
  exp)

; SIMPLIFY-ASSIGNMENT performs this transformation:
;
;    (set! I (begin ... E))
; -> (begin ... (set! I E))

(define (simplify-assignment exp notepad)
  (notepad-var-add! notepad (assignment.lhs exp))
  (let ((rhs (simplify (assignment.rhs exp) notepad)))
    (cond ((begin? rhs)
           (let ((exprs (reverse (begin.exprs rhs))))
             (assignment.rhs-set! exp (car exprs))
             (post-simplify-begin
              (make-begin (reverse (cons exp (cdr exprs))))
              notepad)))
          (else (assignment.rhs-set! exp rhs) exp))))

(define (simplify-sequential exp notepad)
  (let ((exprs (map (lambda (exp) (simplify exp notepad))
                    (begin.exprs exp))))
    (begin.exprs-set! exp exprs)
    (post-simplify-begin exp notepad)))

; Given (BEGIN E0 E1 E2 ...) where the E_i are simplified expressions,
; flattens any nested BEGINs and removes trivial expressions that
; don't appear in the last position.  The second argument is used only
; if a lambda expression is removed.
; This procedure is careful to return E instead of (BEGIN E).
; Fairly harmless bug: a variable reference removed by this procedure
; may remain on the notepad when it shouldn't.

(define (post-simplify-begin exp notepad)
  (let ((unspecified-expression (make-unspecified)))
    ; (flatten exprs '()) returns the flattened exprs in reverse order.
    (define (flatten exprs flattened)
      (cond ((null? exprs) flattened)
            ((begin? (car exprs))
             (flatten (cdr exprs)
                      (flatten (begin.exprs (car exprs)) flattened)))
            (else (flatten (cdr exprs) (cons (car exprs) flattened)))))
    (define (filter exprs filtered)
      (if (null? exprs)
          filtered
          (let ((exp (car exprs)))
            (cond ((constant? exp) (filter (cdr exprs) filtered))
                  ((variable? exp) (filter (cdr exprs) filtered))
                  ((lambda? exp)
                   (notepad.lambdas-set!
                    notepad
                    (remq exp (notepad.lambdas notepad)))
                   (filter (cdr exprs) filtered))
                  ((equal? exp unspecified-expression)
                   (filter (cdr exprs) filtered))
                  (else (filter (cdr exprs) (cons exp filtered)))))))
    (let ((exprs (flatten (begin.exprs exp) '())))
      (begin.exprs-set! exp (filter (cdr exprs) (list (car exprs))))
      (if (null? (cdr (begin.exprs exp)))
          (car (begin.exprs exp))
          exp))))

; SIMPLIFY-CALL simplifies subexpressions
; and calls SIMPLIFY-LET to take care of LET transformations.
; It also performs constant folding (mainly to improve inlining,
; since pass 3 does a better job of constant folding).

(define (simplify-call exp notepad)
  (call.args-set! exp (map (lambda (arg) (simplify arg notepad))
                           (call.args exp)))
  (if (lambda? (call.proc exp))
      (let ((exp (simplify-let exp notepad)))
        (if (call? exp)
            (post-simplify-call exp notepad)
            exp))
      (begin (call.proc-set! exp
                             (simplify (call.proc exp) notepad))
             (post-simplify-call exp notepad))))

; Performs this transformation:
;
;    ((begin ... L) E ...)
; -> (begin ... (L E ...))
;
; Also performs constant folding, mainly to improve inlining,
; since pass 3 does a better job of constant folding.

(define (post-simplify-call exp notepad)
  (let ((proc (call.proc exp)))
    (cond ((lambda? proc)
           (post-simplify-let exp notepad))
          ((begin? proc)
           (let* ((revexprs (reverse (begin.exprs proc)))
                  (exp2 (car revexprs)))
             (if (lambda? exp2)
                 (let ((exp2
                        (post-simplify-let (make-call exp2 (call.args exp))
                                           notepad)))
                   (begin.exprs-set! proc (reverse (cons exp2 (cdr revexprs))))
                   (post-simplify-begin proc notepad))
                 exp)))
          ((variable? proc)
           (let* ((procname (variable.name proc))
                  (args (call.args exp))
                  (entry
                   (and (not (null? args))
                        (constant? (car args))
                        (every? constant? args)
                        (let ((entry (constant-folding-entry procname)))
                          (and entry
                               (let ((predicates
                                      (constant-folding-predicates entry)))
                                 (and (= (length args)
                                         (length predicates))
                                      (let loop ((args args)
                                                 (predicates predicates))
                                        (cond ((null? args) entry)
                                              (((car predicates)
                                                (constant.value
                                                 (car args)))
                                               (loop (cdr args)
                                                     (cdr predicates)))
                                              (else #f))))))))))
             (if entry
                 (make-constant (apply (constant-folding-folder entry)
                                       (map constant.value args)))
                 exp)))
          (else exp))))

; Cleans up the output of let optimization by performing
; these transformations:
;
;    ((lambda (I_1 ... I_k . I_rest) ---) E1 ... Ek Ek+1 ...)
; -> ((lambda (I_1 ... I_k I_rest) ---) E1 ... Ek (LIST Ek+1 ...))
;
;    ((lambda () (begin) (quote ...) E))
; -> E
;
;    ((lambda (I1)
;       (begin)
;       (quote ((I1 ((begin I1)) () ())))
;       (begin I1))
;     E1)
;
; -> E1
;
;    ((lambda (IGNORED I2 ...) ---) E1 E2 ...)
; -> (begin E1 ((lambda (I2 ...) ---) E2 ...))
;
; Note that these transformations do not change the body
; of the lambda expression, nor do they depend upon the
; referencing information (which may be out of date after
; simplification).

(define (post-simplify-let exp notepad)
  (define proc (call.proc exp))

  ; Look for ignored or rest arguments or wrong number of arguments.

  (define (loop1 formals actuals rev-formals rev-actuals rev-for-effect)
    (cond ((null? formals)
           (if (not (null? actuals))
               (pass2-error p2error:wna (make-readable exp #f)))
           (return1 rev-formals rev-actuals rev-for-effect))
          ((symbol? formals)
           (return1 (cons formals rev-formals)
                    (cons (make-call-to-LIST actuals) rev-actuals)
                    rev-for-effect))
          ((null? actuals)
           (pass2-error p2error:wna (make-readable exp #f))
           (return1 rev-formals rev-actuals rev-for-effect))
          ((ignored? (car formals))
           (loop1 (cdr formals)
                  (cdr actuals)
                  rev-formals
                  rev-actuals
                  (cons (car actuals) rev-for-effect)))
          (else (loop1 (cdr formals)
                       (cdr actuals)
                       (cons (car formals) rev-formals)
                       (cons (car actuals) rev-actuals)
                       rev-for-effect))))

  (define (return1 rev-formals rev-actuals rev-for-effect)
    (let ((formals (reverse rev-formals))
          (actuals (reverse rev-actuals))
          (for-effect (reverse rev-for-effect)))
      (lambda.args-set! proc formals)
      (call.args-set! exp actuals)
      (cond ((and (null? formals)
                  (null? actuals)
                  (null? (lambda.defs proc)))
             (return2 (lambda.body proc) for-effect))
            ((and (not (null? formals))
                  (null? (cdr formals))
                  (variable? (lambda.body proc))
                  (eq? (car formals) (variable.name (lambda.body proc)))
                  (null? (lambda.defs proc)))
             (return2 (car actuals) for-effect))
            (else
             (return2 exp for-effect)))))

  (define (return2 exp for-effect)
      (if (null? for-effect)
          exp
          (post-simplify-begin (make-begin (append for-effect (list exp)))
                               notepad)))

  (loop1 (lambda.args proc) (call.args exp) '() '() '()))

; SIMPLIFY-LET performs these transformations:
;
;    ((lambda (I_1 ... I_k . I_rest) ---) E1 ... Ek Ek+1 ...)
; -> ((lambda (I_1 ... I_k I_rest) ---) E1 ... Ek (LIST Ek+1 ...))
;
;    ((lambda (I1 I2 ...) (begin D ...) (quote ...) E) L ...)
; -> ((lambda (I2 ...) (begin (define I1 L) D ...) (quote ...) E) ...)
;
; provided I1 is not assigned and each reference to I1 is in call position.
;
;    ((lambda (I1)
;       (begin)
;       (quote ((I1 ((begin I1)) () ())))
;       (begin I1))
;     E1)
;
; -> E1
;
;    ((lambda (I1)
;       (begin)
;       (quote ((I1 ((begin I1)) () ())))
;       (if (begin I1) E2 E3))
;     E1)
;
; -> (if E1 E2 E3)
;
; (Together with SIMPLIFY-CASE, this cleans up the output of the OR
; macro and enables certain control optimizations.)
;
;    ((lambda (I1 I2 ...)
;       (begin D ...)
;       (quote (... (I <references> () <calls>) ...) ...)
;       E)
;     K ...)
; -> ((lambda (I2 ...)
;       (begin D' ...)
;       (quote (... ...) ...)
;       E')
;     ...)
;
; where D' ... and E' ... are obtained from D ... and E ...
; by replacing all references to I1 by K.  This transformation
; applies if K is a constant that can be duplicated without changing
; its EQV? behavior.
;
;    ((lambda () (begin) (quote ...) E)) -> E
;
;    ((lambda (IGNORED I2 ...) ---) E1 E2 ...)
; -> (begin E1 ((lambda (I2 ...) ---) E2 ...))
;
; (Single assignment analysis, performed by the simplifier for lambda
; expressions, detects unused arguments and replaces them in the argument
; list by the special identifier IGNORED.)
;
; The exp argument is a call whose arguments have already been simplified
; and whose proc is a lambda expression that hasn't been simplified.

(define (simplify-let exp notepad)
  (define proc (call.proc exp))

  ; Loop1 operates before simplification of the lambda body.

  (define (loop1 formals actuals processed-formals processed-actuals)
    (cond ((null? formals)
           (if (not (null? actuals))
               (pass2-error p2error:wna (make-readable exp #t)))
           (return1 processed-formals processed-actuals))
          ((symbol? formals)
           (return1 (cons formals processed-formals)
                    (cons (make-call-to-LIST actuals) processed-actuals)))
          ((null? actuals)
           (pass2-error p2error:wna (make-readable exp #t))
           (return1 processed-formals
                    processed-actuals))
          ((and (lambda? (car actuals))
                (let ((Rinfo (R-lookup (lambda.R proc) (car formals))))
                  (and (null? (R-entry.assignments Rinfo))
                       (= (length (R-entry.references Rinfo))
                          (length (R-entry.calls Rinfo))))))
           (let ((I (car formals))
                 (L (car actuals)))
             (notepad-nonescaping-add! notepad L)
             (lambda.defs-set! proc
               (cons (make-definition I L)
                     (lambda.defs proc)))
             (standardize-known-calls L
                                      (R-entry.calls
                                       (R-lookup (lambda.R proc) I)))
             (lambda.F-set! proc (union (lambda.F proc)
                                        (free-variables L)))
             (lambda.G-set! proc (union (lambda.G proc) (lambda.G L))))
           (loop1 (cdr formals)
                  (cdr actuals)
                  processed-formals
                  processed-actuals))
          ((and (constant? (car actuals))
                (let ((x (constant.value (car actuals))))
                  (or (boolean? x)
                      (number? x)
                      (symbol? x)
                      (char? x))))
           (let* ((I (car formals))
                  (Rinfo (R-lookup (lambda.R proc) I)))
             (if (null? (R-entry.assignments Rinfo))
                 (begin
                  (for-each (lambda (ref)
                              (variable-set! ref (car actuals)))
                            (R-entry.references Rinfo))
                  (lambda.R-set! proc (remq Rinfo (lambda.R proc)))
                  (lambda.F-set! proc (remq I (lambda.F proc)))
                  (lambda.G-set! proc (remq I (lambda.G proc)))
                  (loop1 (cdr formals)
                         (cdr actuals)
                         processed-formals
                         processed-actuals))
                 (loop1 (cdr formals)
                        (cdr actuals)
                        (cons (car formals) processed-formals)
                        (cons (car actuals) processed-actuals)))))
          (else (loop1 (cdr formals)
                       (cdr actuals)
                       (cons (car formals) processed-formals)
                       (cons (car actuals) processed-actuals)))))

  (define (return1 rev-formals rev-actuals)
    (let ((formals (reverse rev-formals))
          (actuals (reverse rev-actuals)))
      (lambda.args-set! proc formals)
      (if (and (not (null? formals))
               (null? (cdr formals))
               (let* ((x (car formals))
                      (R (lambda.R proc))
                      (refs (references R x)))
                 (and (= 1 (length refs))
                      (null? (assignments R x)))))
          ; exactly one formal, which is referenced exactly once in body
          ; the actual has already been simplified, but the lambda hasn't
          (let ((x (car formals))
                (body (lambda.body proc)))
            (cond ((and (variable? body)
                        (eq? x (variable.name body)))
                   (car actuals))
                  ((and (conditional? body)
                        (let ((B0 (if.test body)))
                          (variable? B0)
                          (eq? x (variable.name B0))))
                   (if.test-set! body (car actuals))
                   (if (control-optimization)
                       (simplify-case body notepad)
                       (let ((e1 (simplify (if.then body) notepad))
                             (e2 (simplify (if.else body) notepad)))
                         (if.then-set! body e1)
                         (if.else-set! body e2))))
                  (else
                   (return1-finish formals actuals))))
          (return1-finish formals actuals))))

  (define (return1-finish formals actuals)
    (simplify-lambda proc notepad)
    (loop2 formals actuals '() '() '()))

  ; Loop2 operates after simplification of the lambda body.

  (define (loop2 formals actuals processed-formals processed-actuals for-effect)
    (cond ((null? formals)
           (return2 processed-formals processed-actuals for-effect))
          ((ignored? (car formals))
           (loop2 (cdr formals)
                  (cdr actuals)
                  processed-formals
                  processed-actuals
                  (cons (car actuals) for-effect)))
          (else (loop2 (cdr formals)
                       (cdr actuals)
                       (cons (car formals) processed-formals)
                       (cons (car actuals) processed-actuals)
                       for-effect))))

  (define (return2 rev-formals rev-actuals rev-for-effect)
    (let ((formals (reverse rev-formals))
          (actuals (reverse rev-actuals))
          (for-effect (reverse rev-for-effect)))
      (lambda.args-set! proc formals)
      (call.args-set! exp actuals)
      (let ((exp (if (and (null? actuals)
                          (or (null? (lambda.defs proc))
                              (and (notepad.parent notepad)
                                   (POLICY:LIFT? proc
                                                 (notepad.parent notepad)
                                                 (map (lambda (def) '())
                                                      (lambda.defs proc))))))
                     (begin (for-each (lambda (I)
                                        (notepad-var-add! notepad I))
                                      (lambda.F proc))
                            (if (not (null? (lambda.defs proc)))
                                (let ((parent (notepad.parent notepad))
                                      (defs (lambda.defs proc))
                                      (R (lambda.R proc)))
                                  (lambda.defs-set!
                                    parent
                                    (append defs (lambda.defs parent)))
                                  (lambda.defs-set! proc '())
                                  (lambda.R-set!
                                    parent
                                    (append (map (lambda (def)
                                                   (R-lookup R (def.lhs def)))
                                                 defs)
                                            (lambda.R parent)))))
                            (lambda.body proc))
                     exp)))
        (if (null? for-effect)
            exp
            (post-simplify-begin (make-begin (append for-effect (list exp)))
                                 notepad)))))

  (notepad-nonescaping-add! notepad proc)
  (loop1 (lambda.args proc) (call.args exp) '() '()))

; Single assignment analysis performs the transformation
;
;    (lambda (... I ...)
;      (begin D ...)
;      (quote (... (I <references> ((set! I L)) <calls>) ...) ...)
;      (begin (set! I L) E1 ...))
; -> (lambda (... IGNORED ...)
;      (begin (define I L) D ...)
;      (quote (... (I <references> () <calls>) ...) ...)
;      (begin E1 ...))
;
; For best results, pass 1 should sort internal definitions and LETRECs so
; that procedure definitions/bindings come first.
;
; This procedure operates by side effect.

(define (single-assignment-analysis L notepad)
  (let ((formals (lambda.args L))
        (defs (lambda.defs L))
        (R (lambda.R L))
        (body (lambda.body L)))
    (define (finish! exprs escapees)
      (begin.exprs-set! body
                        (append (reverse escapees)
                                exprs))
      (lambda.body-set! L (post-simplify-begin body '())))
    (if (begin? body)
        (let loop ((exprs (begin.exprs body))
                   (escapees '()))
          (let ((first (car exprs)))
            (if (and (assignment? first)
                     (not (null? (cdr exprs))))
                (let ((I (assignment.lhs first))
                      (rhs (assignment.rhs first)))
                  (if (and (lambda? rhs)
                           (local? R I)
                           (= 1 (length (assignments R I))))
                      (if (= (length (calls R I))
                             (length (references R I)))
                          (begin (notepad-nonescaping-add! notepad rhs)
                                 (flag-as-ignored I L)
                                 (lambda.defs-set! L
                                   (cons (make-definition I rhs)
                                         (lambda.defs L)))
                                 (assignments-set! R I '())
                                 (standardize-known-calls
                                  rhs
                                  (R-entry.calls (R-lookup R I)))
                                 (loop (cdr exprs) escapees))
                          (loop (cdr exprs)
                                (cons (car exprs) escapees)))
                      (finish! exprs escapees)))
                (finish! exprs escapees)))))))

(define (standardize-known-calls L calls)
  (let ((formals (lambda.args L)))
    (cond ((not (list? formals))
           (let* ((newformals (make-null-terminated formals))
                  (n (- (length newformals) 1)))
             (lambda.args-set! L newformals)
             (for-each (lambda (call)
                         (if (>= (length (call.args call)) n)
                             (call.args-set!
                              call
                              (append (list-head (call.args call) n)
                                      (list
                                       (make-call-to-LIST
                                        (list-tail (call.args call) n)))))
                             (pass2-error p2error:wna
                                          (make-readable call #t))))
                       calls)))
          (else (let ((n (length formals)))
                  (for-each (lambda (call)
                              (if (not (= (length (call.args call)) n))
                                  (pass2-error p2error:wna call)))
                            calls))))))

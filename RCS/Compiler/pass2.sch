; Copyright 1991 Lightship Software, Incorporated.
; 28 August 1991
;
; Second pass of the Scheme 313 compiler:
;   single assignment analysis, assignment elimination,
;   lambda lifting, and various local source transformations.
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
;           (quote <info>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F))
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
; <info>  -->  (R F)
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
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
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  Variables named IGNORED are neither referenced nor assigned.

(define (pass2 exp)
  (simplify exp (make-notepad #f)))

; Given an expression and a "notepad" data structure that conveys
; inherited attributes, performs the appropriate optimizations and
; destructively modifies the notepad to record various attributes
; that it synthesizes while traversing the expression.  In particular,
; any nested lambda expressions and any variable references will be
; noted in the notepad.

(define (simplify exp notepad)
  (case (car exp)
    ((quote)    exp)
    ((lambda)   (simplify-lambda exp notepad))
    ((set!)     (simplify-assignment exp notepad))
    ((if)       (simplify-conditional exp notepad))
    ((begin)    (if (variable? exp)
                    (begin (notepad-var-add! notepad (variable.name exp))
                           exp)
                    (simplify-sequential exp notepad)))
    (else       (simplify-call exp notepad))))

; Most optimization occurs here.
; Single assignment analysis creates internal definitions
; and renames arguments whose value is ignored.
; Assignment elimination follows single assignment analysis.
; The  right hand sides of internal definitions are then
; simplified, as is the expression in the body.
; Internal definitions may then be lifted to the enclosing
; lambda expression.

(define (simplify-lambda exp notepad)
  (notepad-lambda-add! notepad exp)
  (single-assignment-analysis exp)
  (assignment-elimination exp)
  (let ((defs (lambda.defs exp))
        (body (lambda.body exp))
        (newnotepad (make-notepad exp)))
    (for-each (lambda (def)
                (simplify-lambda (def.rhs def) newnotepad))
              defs)
    (lambda.body-set! exp (simplify body newnotepad))
    (lambda.F-set! exp (notepad-free-variables newnotepad))
    (lambda-lifting exp (notepad.parent notepad))
    exp))

(define (simplify-assignment exp notepad)
  (assignment.rhs-set! exp (simplify (assignment.rhs exp) notepad))
  exp)

; Some source transformations on IF expressions:
;
; (if '#f E1 E2)                E2
; (if 'K  E1 E2)                E1                          K != #f
; (if (if B0 '#f '#f) E1 E2)    (begin B0 E2)
; (if (if B0 '#f 'K ) E1 E2)    (if B0 E2 E1)               K != #f
; (if (if B0 'K  '#f) E1 E2)    (if B0 E1 E2)               K != #f
; (if (if B0 'K1 'K2) E1 E2)    (begin B0 E1)               K1, K2 != #f
; (if (begin ... B0) E1 E2)     (begin ... (if B0 E1 E2))
; (if (not E0) E1 E2)           (if E0 E2 E1)               not is integrable

(define (simplify-conditional exp notepad)
  (let loop ((test (simplify (if.test exp) notepad)))
    (if.test-set! exp test)
    (cond ((constant? test)
           (simplify (if (constant.value test)
                         (if.then exp)
                         (if.else exp))
                     notepad))
          ((and (conditional? test)
                (constant? (if.then test))
                (constant? (if.else test)))
           (cond ((and (constant.value (if.then test))
                       (constant.value (if.else test)))
                  (post-simplify-begin
                   (list 'begin (if.test test)
                                (simplify (if.then exp) notepad))
                   notepad))
                 ((and (not (constant.value (if.then test)))
                       (not (constant.value (if.then test))))
                  (post-simplify-begin
                   (list 'begin (if.test test)
                                (simplify (if.else exp) notepad))
                   notepad))
                 (else (if.then-set! exp (simplify (if.then test) notepad))
                       (if.else-set! exp (simplify (if.else test) notepad))
                       (if (not (constant.value (if.then test)))
                           (let ((temp (if.then exp)))
                             (if.then-set! exp (if.else exp))
                             (if.else-set! exp temp)))
                       exp)))
          ((begin? test)
           (let ((exprs (reverse (begin.exprs test))))
             (if.test-set! exp (car exprs))
             (if.then-set! exp (simplify (if.then test) notepad))
             (if.else-set! exp (simplify (if.else test) notepad))
             (post-simplify-begin
              (cons 'begin
                    (reverse (cons exp (cdr exprs))))
              notepad)))
          ((and (call? test)
                (variable? (call.proc test))
                (eq? (variable.name (call.proc test)) 'NOT)
                (integrable? 'NOT)
                (= (length (call.args test)) 1))
           (let ((temp (if.then exp)))
             (if.then-set! exp (if.else exp))
             (if.else-set! exp temp))
           (loop (car (call.args test))))
          (else (if.then-set! exp (simplify (if.then exp) notepad))
                (if.else-set! exp (simplify (if.else exp) notepad))
                exp))))

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
                 (notepad.lambdas-set! notepad
                                       (remq exp (notepad.lambdas notepad)))
                 (filter (cdr exprs) filtered))
                (else (filter (cdr exprs) (cons exp filtered)))))))
  (let ((exprs (flatten (begin.exprs exp) '())))
    (begin.exprs-set! exp (filter (cdr exprs) (list (car exprs))))
    (if (null? (cdr (begin.exprs exp)))
        (car (begin.exprs exp))
        exp)))

(define (simplify-call exp notepad)
  (call.proc-set! exp (simplify (call.proc exp) notepad))
  (call.args-set! exp (map (lambda (arg) (simplify arg notepad))
                           (call.args exp)))
  (if (lambda? (call.proc exp))
      (simplify-let exp notepad)
      exp))

; SIMPLIFY-LET performs these transformations:
;
;    ((lambda () (begin) (quote R) E)) -> E
;
;    ((lambda (I_1 ... I_k . I_rest) ---) E1 ... Ek Ek+1 ...)
; -> ((lambda (I_1 ... I_k I_rest) ---) E1 ... Ek (CONS Ek+1 ...))
;
;    ((lambda (IGNORED I2 ...) ---) E1 E2 ...)
; -> (begin E1 ((lambda (I2 ...) ---) E2 ...))
;
; (Single assignment analysis, performed by the simplifier for lambda
; expressions, detects unused arguments and replaces them in the argument
; list by the special identifier IGNORED.)

(define (simplify-let exp notepad)
  (define proc (call.proc exp))
  (define (loop formals actuals processed-formals processed-actuals for-effect)
    (cond ((null? formals)
           (if (not (null? actuals))
               (pass2-error p2error:wna exp))
           (return processed-formals processed-actuals for-effect))
          ((symbol? formals)
           (if (ignored? formals)
               (return processed-formals
                       processed-actuals
                       (append actuals for-effect))
               (return (cons formals processed-formals)
                       (cons (make-call-to-LIST actuals) processed-actuals)
                       for-effect)))
          ((ignored? (car formals))
           (if (null? actuals)
               (pass2-error p2error:wna exp))
           (loop (cdr formals)
                 (cdr actuals)
                 processed-formals
                 processed-actuals
                 (cons (car actuals) for-effect)))
          (else (if (null? actuals)
                    (pass2-error p2error:wna exp))
                (loop (cdr formals)
                      (cdr actuals)
                      (cons (car formals) processed-formals)
                      (cons (car actuals) processed-actuals)
                      for-effect))))
  (define (return formals actuals for-effect)
    (lambda.args-set! proc (reverse formals))
    (call.args-set! exp (reverse actuals))
    (let ((exp (if (and (null? (lambda.defs proc))
                        (null? actuals))
                   (begin (for-each (lambda (I)
                                      (notepad-var-add! notepad I))
                                    (lambda.F proc))
                          (lambda.body proc))
                   exp)))
      (if (null? for-effect)
          exp
          (post-simplify-begin (make-begin (append for-effect (list exp)))
                               notepad))))
  (loop (lambda.args proc) (call.args exp) '() '() '()))

; Single assignment analysis performs the transformation
;
;    (lambda (... I ...)
;      (begin D ...)
;      (quote ... (I <references> ((set! I L)) #t) ...)
;      (begin (set! I L) E1 ...))
; -> (lambda (... IGNORED ...)
;      (begin (define I L) D ...)
;      (quote ... (I <references> () #t) ...)
;      (begin E1 ...))
;
; For best results, pass 1 should sort internal definitions and LETRECs so
; that procedure definitions/bindings come first.
;
; This procedure operates by side effect.

(define (single-assignment-analysis L)
  (let ((formals (lambda.args L))
        (defs (lambda.defs L))
        (R (lambda.R L))
        (body (lambda.body L)))
    (if (begin? body)
        (let ((first (car (begin.exprs body))))
          (if (assignment? first)
              (let ((I (assignment.lhs first))
                    (rhs (assignment.rhs first)))
                (if (and (lambda? rhs)
                         (local? R I)
                         (= 1 (length (assignments R I)))
                         (= (length (calls R I))
                            (length (references R I))))
                    (begin (flag-as-ignored I L)
                           (lambda.defs-set! L
                             (cons (make-definition I rhs)
                                   (lambda.defs L)))
                           (assignments-set! R I '())
                           (begin.exprs-set! body (cdr (begin.exprs body)))
                           (lambda.body-set! L (post-simplify-begin body '()))
                           (single-assignment-analysis L)))))))))

; Assignment elimination replaces variables that appear on the left
; hand side of an assignment by data structures.  This is necessary
; to avoid some nasty complications with lambda lifting.
;
; This procedure operates by side effect.

(define (assignment-elimination L)
  (let ((formals (lambda.args L))
        (R (lambda.R L)))
    
    ; Given a list of entries, return those for assigned variables.
    
    (define (loop entries assigned)
      (cond ((null? entries)
             (if (not (null? assigned))
                 (eliminate assigned)))
            ((not (null? (R-entry.assignments (car entries))))
             (loop (cdr entries) (cons (car entries) assigned)))
            ((null? (R-entry.references (car entries)))
             (flag-as-ignored (R-entry.name (car entries)) L)
             (loop (cdr entries) assigned))
            (else (loop (cdr entries) assigned))))
    
    ; Given a list of entries for assigned variables I1 ...,
    ; remove the assignments by replacing the body by a LET of the form
    ; ((LAMBDA (V1 ...) ...) (MAKE-CELL I1) ...), by replacing references
    ; by calls to CELL-REF, and by replacing assignments by calls to
    ; CELL-SET!.
    
    (define (eliminate assigned)
      (let ((augmented-entries
             (map (lambda (entry)
                    (list (generate-new-name (R-entry.name entry)) entry))
                  assigned))
            (defs (lambda.defs L))
            (R (lambda.R L)))
        (for-each cellify! augmented-entries)
        (let ((newbody
               (make-call
                (make-lambda (map car augmented-entries)
                             defs
                             (union (map (lambda (def)
                                           (R-entry R (def.lhs def)))
                                         defs)
                                    (map new-reference-info augmented-entries))
                             (union (list name:CELL-REF name:CELL-SET!)
                                    (map car augmented-entries)
                                    (difference (lambda.F L)
                                                (map R-entry.name assigned)))
                             (lambda.decls L)
                             (lambda.doc L)
                             (lambda.body L))
                (map (lambda (name)
                       (make-call (make-variable name:MAKE-CELL)
                                  (list (make-variable name))))
                     (map R-entry.name assigned)))))
          (lambda.F-set! L (union (list name:MAKE-CELL name:CELL-REF name:CELL-SET!)
                                  (difference (lambda.F L)
                                              (map def.lhs (lambda.defs L)))))
          (lambda.defs-set! L '())
          (for-each update-old-reference-info!
                    (map (lambda (arg)
                           (car (call.args arg)))
                         (call.args newbody)))
          (lambda.body-set! L newbody))))
    
    (define (generate-new-name name)
      (string->symbol (string-append "CELL:" (symbol->string name))))
    
    ; In addition to replacing references and assignments involving the
    ; old variable by calls to CELL-REF and CELL-SET! on the new, CELLIFY!
    ; uses the old entry to collect the referencing information for the
    ; new variable.
    
    (define (cellify! augmented-entry)
      (let ((newname (car augmented-entry))
            (entry (cadr augmented-entry)))
        (do ((refs (R-entry.references entry)
                   (cdr refs)))
            ((null? refs))
            (let* ((reference (car refs))
                   (newref (make-variable newname)))
              (set-car! reference (make-variable name:CELL-REF))
              (set-car! (cdr reference) newref)
              (set-car! refs newref)))
        (do ((assigns (R-entry.assignments entry)
                      (cdr assigns)))
            ((null? assigns))
            (let* ((assignment (car assigns))
                   (newref (make-variable newname)))
              (set-car! assignment (make-variable name:CELL-SET!))
              (set-car! (cdr assignment) newref)
              (R-entry.references-set! entry
                                       (cons newref
                                             (R-entry.references entry)))))
        (R-entry.assignments-set! entry '())))
    
    ; This procedure creates a brand new entry for a new variable, extracting
    ; the references stored in the old entry by CELLIFY!.
    
    (define (new-reference-info augmented-entry)
      (make-R-entry (car augmented-entry)
                    (R-entry.references (cadr augmented-entry))
                    '()
                    '()))
    
    ; This procedure updates the old entry to reflect the fact that it is
    ; now referenced once and never assigned.
    
    (define (update-old-reference-info! ref)
      (references-set! R (variable.name ref) (list ref))
      (assignments-set! R (variable.name ref) '())
      (calls-set! R (variable.name ref) '()))
    
    (loop R '())))

; Lambda lifting raises internal definitions to outer scopes to avoid
; having to choose between creating a closure or losing tail recursion.
; If L is not #f, then L2 is a lambda expression nested within L.
; Any internal definitions that occur within L2 may be lifted to L
; by adding extra arguments to the defined procedure and to all calls to it.
; Lambda lifting is not a clear win, because the extra arguments could
; easily become more expensive than creating a closure and referring
; to the non-local arguments through the closure.  The heuristics used
; to decide whether to lift a group of internal definitions are isolated
; within the POLICY:LIFT? procedure.

(define (lambda-lifting L2 L)
  
  ; The call to sort is optional.  It gets the added arguments into
  ; the same order they appear in the formals list, which is an
  ; advantage for register targeting.
  
  (define (lift L2 L args-to-add)
    (let ((formals (make-null-terminated (lambda.args L2))))
      (do ((defs (lambda.defs L2) (cdr defs))
           (args-to-add args-to-add (cdr args-to-add)))
          ((null? defs))
          (let* ((def (car defs))
                 (entry (R-lookup (lambda.R L2) (def.lhs def)))
                 (calls (R-entry.calls entry))
                 (added (sort (car args-to-add)
                              (lambda (x y)
                                (let ((xx (memq x formals))
                                      (yy (memq y formals)))
                                  (if (and xx yy)
                                      (> (length xx) (length yy))
                                      #t)))))
                 (L3 (def.rhs def)))
            ; The flow equation guarantees that these added arguments
            ; will occur free by the time this round of lifting is done.
            (lambda.F-set! L3 (union added (lambda.F L3)))
            (lambda.args-set! L3 (append added (lambda.args L3)))
            (for-each (lambda (call)
                        (let ((newargs (map make-variable added)))
                          ; The referencing information is made obsolete here!
                          (call.args-set! call
                                          (append newargs (call.args call)))))
                      calls)
            (lambda.R-set! L (cons entry (lambda.R L)))
            (lambda.R-set! L2 (remq entry (lambda.R L2)))))
      (lambda.defs-set! L (append (lambda.defs L2) (lambda.defs L)))
      (lambda.defs-set! L2 '())))
  
  (if L
      (if (not (null? (lambda.defs L2)))
          (let ((args-to-add (compute-added-arguments
                              (lambda.defs L2)
                              (make-null-terminated (lambda.args L2)))))
            (if (POLICY:LIFT? L2 L args-to-add)
                (lift L2 L args-to-add))))))

; Given a list of definitions ((define f1 ...) ...) and a set of formals
; N over which the definitions may be lifted, returns a list of the
; subsets of N that need to be added to each procedure definition
; as new arguments.
;
; Algorithm: Let F_i be the variables that occur free in the body of
; the lambda expression associated with f_i.  Construct the call graph.
; Solve the flow equations
;
;     A_i = (F_i /\ N) \/ (\/ {A_j | A_i calls A_j})
;
; where /\ is intersection and \/ is union.

(define (compute-added-arguments defs formals)
  (let ((procs (map def.lhs defs))
        (freevars (map lambda.F (map def.rhs defs))))
    (let ((callgraph (map (lambda (names)
                            (map (lambda (name)
                                   (position name procs))
                                 (intersection names procs)))
                          freevars))
          (added_0 (map (lambda (names)
                          (intersection names formals))
                        freevars)))
      (vector->list
       (compute-fixedpoint
        (make-vector (length procs) '())
        (list->vector (map (lambda (term0 indexes)
                             (lambda (approximations)
                               (union term0
                                      (apply union
                                             (map (lambda (i)
                                                    (vector-ref approximations i))
                                                  indexes)))))
                           added_0
                           callgraph))
        set-equal?)))))

(define (position x l)
  (cond ((eq? x (car l)) 0)
        (else (+ 1 (position x (cdr l))))))

; Given a vector of starting approximations,
; a vector of functions that compute a next approximation
; as a function of the vector of approximations,
; and an equality predicate,
; returns a vector of fixed points.

(define (compute-fixedpoint v functions equiv?)
  (define (loop i flag)
    (if (negative? i)
        (if flag
            (loop (- (vector-length v) 1) #f)
            v)
        (let ((next_i ((vector-ref functions i) v)))
          (if (equiv? next_i (vector-ref v i))
              (loop (- i 1) flag)
              (begin (vector-set! v i next_i)
                     (loop (- i 1) #t))))))
  (loop (- (vector-length v) 1) #f))

; Hey, it's a prototype.
;
; Reasonable heuristics to use eventually, on the assumption
; that simple code generators will create a closure for any
; lambda expression that contains internal definitions:
;
;   Don't lift if it means adding too many arguments.
;   Don't lift large groups of definitions.
;   In questionable cases it is better to lift to an outer
;     lambda expression that already contains internal
;     definitions than to one that doesn't.

(define (POLICY:LIFT? L2 L args-to-add) #t)

; To do:
;   *  Add number-of-argument checking and rest-list elimination
;      to single-assignment-analysis.

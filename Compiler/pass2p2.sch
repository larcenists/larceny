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
; 17 May 1995.
;
; Second pass of the Twobit compiler, part 2:
;   single assignment elimination, assignment elimination,
;   and lambda lifting.
;
; See part 1 for further documentation.

; Single assignment elimination performs the transformation
;
;    (lambda (... I1 ... In ...)
;      (begin D ...)
;      (begin (set! I1 E1)
;             ...
;             (set! In En)
;             E ...))
; -> (lambda (... IGNORED ... IGNORED ...)
;      (let* ((I1 E1) ... (In En))
;        (begin D ...)
;        (begin E ...)))
;
; provided for each k:
;
;    1.  Ik does not occur in E1, ..., Ek.
;    2.  Either E1 through Ek contain no procedure calls
;        or Ik is not referenced by an escaping lambda expression.
;    3.  Ik is assigned only once.
;
; I doubt whether the third condition is really necessary, but
; dropping it would involve a more complex calculation of the
; revised referencing information.
;
; A more precise description of the transformation:
;
;    (lambda (... I1 ... In ...)
;      (begin (define F1 L1) ...)
;      (quote (... (I1 <references> ((set! I1 E1)) <calls>) ...
;                  (In <references> ((set! In En)) <calls>)
;                  (F1 <references> () <calls>) ...) ...)
;      (begin (set! I1 E1) ... (set! In En) E ...))
; -> (lambda (... IGNORED ... IGNORED ...)
;      (begin)
;      (quote (...) ...)
;      ((lambda (I1)
;         (begin)
;         (quote ((I1 <references> () <calls>)) ...)
;         ...
;           ((lambda (In)
;              (begin (define F1 L1) ...)
;              (quote (... (In <references> () <calls>)
;                          (F1 <references> () <calls>) ...) ...)
;              (begin E ...))
;            En)
;         ...)
;       E1))
;
; For best results, pass 1 should sort internal definitions and LETRECs
; so that procedure definitions/bindings come first, followed by
; definitions/bindings whose right hand side contains no calls,
; followed by definitions/bindings of variables that do not escape,
; followed by all other definitions/bindings.
;
; Pass 1 can't tell which variables escape, however.  Pass 2 can't tell
; which variables escape either until all enclosed lambda expressions
; have been simplified and the first transformation above has been
; performed.  That is why single assignment analysis precedes single
; assignment elimination.  As implemented here, an assignment that does
; not satisfy the conditions above will prevent the transformation from
; being applied to any subsequent assignments.
;
; This procedure operates by side effect.

(define (single-assignment-elimination L notepad)
  
  (if (begin? (lambda.body L))
      
      (let* ((formals (make-null-terminated (lambda.args L)))
             (defined (map def.lhs (lambda.defs L)))
             (escaping (intersection formals
                                     (notepad-captured-variables notepad)))
             (R (lambda.R L)))
        
        ; Given:
        ;    exprs that remain in the body;
        ;    assigns that will be replaced by let* variables;
        ;    call-has-occurred?, a boolean;
        ;    free variables of the assigns;
        ; Performs the transformation described above.
        
        (define (loop exprs assigns call-has-occurred? free)
          (cond ((null? (cdr exprs))
                 (return exprs assigns))
                ((assignment? (car exprs))
                 (let ((I1 (assignment.lhs (car exprs)))
                       (E1 (assignment.rhs (car exprs))))
                   (if (and (memq I1 formals)
                            (= (length (assignments R I1)) 1)
                            (not (and call-has-occurred?
                                      (memq I1 escaping))))
                       (let* ((free-in-E1 (free-variables E1))
                              (newfree (union free-in-E1 free)))
                         (if (or (memq I1 newfree)
                                 (not
                                  (empty-set?
                                   (intersection free-in-E1 defined))))
                             (return exprs assigns)
                             (loop (cdr exprs)
                                   (cons (car exprs) assigns)
                                   (or call-has-occurred?
                                       (might-return-twice? E1))
                                   newfree)))
                       (return exprs assigns))))
                (else (return exprs assigns))))
        
        (define (return exprs assigns)
          (if (not (null? assigns))
              (let ((I (assignment.lhs (car assigns)))
                    (E (assignment.rhs (car assigns)))
                    (defs (lambda.defs L))
                    (F (lambda.F L))
                    (G (lambda.G L)))
                (flag-as-ignored I L)
                (assignments-set! R I '())
                (let ((L2 (make-lambda (list I)
                                       defs
                                       (cons (R-entry R I)
                                             (map (lambda (def)
                                                    (R-entry R (def.lhs def)))
                                                  defs))
                                       F
                                       G
                                       (lambda.decls L)
                                       (lambda.doc L)
                                       (make-begin exprs))))
                  (lambda.defs-set! L '())
                  (for-each (lambda (entry)
                              (lambda.R-set! L (remq entry R)))
                            (lambda.R L2))
                  (return-loop (cdr assigns) (make-call L2 (list E)))))))
        
        (define (return-loop assigns body)
          (if (null? assigns)
              (let ((L3 (call.proc body)))
                (lambda.body-set! L body)
                (lambda-lifting L3 L))
              (let* ((I (assignment.lhs (car assigns)))
                     (E (assignment.rhs (car assigns)))
                     (L3 (call.proc body))
                     (F (remq I (lambda.F L3)))
                     (G (remq I (lambda.G L3))))
                (flag-as-ignored I L)
                (assignments-set! R I '())
                (let ((L2 (make-lambda (list I)
                                       '()
                                       (list (R-entry R I))
                                       F
                                       G
                                       (lambda.decls L)
                                       (lambda.doc L)
                                       body)))
                  (lambda.R-set! L (remq (R-entry R I) R))
                  (lambda-lifting L3 L2)
                  (return-loop (cdr assigns) (make-call L2 (list E)))))))
        
        (loop (begin.exprs (lambda.body L)) '() #f '())))
  
  L)

; Temporary definitions.

(define (free-variables exp)
  (case (car exp)
    ((quote)    '())
    ((lambda)   (difference (lambda.F exp)
                            (make-null-terminated (lambda.args exp))))
    ((set!)     (union (list (assignment.lhs exp))
                       (free-variables (assignment.rhs exp))))
    ((if)       (union (free-variables (if.test exp))
                       (free-variables (if.then exp))
                       (free-variables (if.else exp))))
    ((begin)    (if (variable? exp)
                    (list (variable.name exp))
                    (apply union (map free-variables (begin.exprs exp)))))
    (else       (apply union (map free-variables exp)))))

(define (might-return-twice? exp)
  (case (car exp)
    ((quote)    #f)
    ((lambda)   #f)
    ((set!)     (might-return-twice? (assignment.rhs exp)))
    ((if)       (or (might-return-twice? (if.test exp))
                    (might-return-twice? (if.then exp))
                    (might-return-twice? (if.else exp))))
    ((begin)    (if (variable? exp)
                    #f
                    (some? might-return-twice? (begin.exprs exp))))
    (else       #t)))


; Assignment elimination replaces variables that appear on the left
; hand side of an assignment by data structures.  This is necessary
; to avoid some nasty complications with lambda lifting.
;
; This procedure operates by side effect.

(define (assignment-elimination L)
  (let ((R (lambda.R L)))
    
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
      (let* ((oldnames (map R-entry.name assigned))
             (newnames (map generate-new-name oldnames)))
        (let ((augmented-entries (map list newnames assigned))
              (renaming-alist (map cons oldnames newnames))
              (defs (lambda.defs L)))
          (for-each cellify! augmented-entries)
          (for-each (lambda (def)
                      (do ((free (lambda.F (def.rhs def)) (cdr free)))
                          ((null? free))
                          (let ((z (assq (car free) renaming-alist)))
                            (if z
                                (set-car! free (cdr z))))))
                    defs)
          (let ((newbody
                 (make-call
                  (make-lambda (map car augmented-entries)
                               defs
                               (union (map (lambda (def)
                                             (R-entry R (def.lhs def)))
                                           defs)
                                      (map new-reference-info augmented-entries))
                               (union (list name:CELL-REF name:CELL-SET!)
                                      newnames
                                      (difference (lambda.F L) oldnames))
                               (union (list name:CELL-REF name:CELL-SET!)
                                      newnames
                                      (difference (lambda.G L) oldnames))
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
            (lambda.body-set! L newbody)
            (lambda-lifting (call.proc newbody) L)))))
    
    (define (generate-new-name name)
      (string->symbol (string-append cell-prefix (symbol->string name))))
    
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

; L2 can be the same as L, so the order of side effects is critical.

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
                 (added (twobit-sort (lambda (x y)
                                       (let ((xx (memq x formals))
                                             (yy (memq y formals)))
                                         (if (and xx yy)
                                             (> (length xx) (length yy))
                                             #t)))
                                     (car args-to-add)))
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
            (lambda.R-set! L2 (remq entry (lambda.R L2)))
            (lambda.R-set! L (cons entry (lambda.R L)))
            ))
      (if (not (eq? L2 L))
          (begin
           (lambda.defs-set! L (append (lambda.defs L2) (lambda.defs L)))
           (lambda.defs-set! L2 '())))))
  
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
; that simple code generators will create a closure upon entry
; to any lambda expression that contains internal definitions:
;
;   Don't lift if it means adding too many arguments.
;   Don't lift large groups of definitions.
;   In questionable cases it is better to lift to an outer
;     lambda expression that already contains internal
;     definitions than to one that doesn't.
;   It is better not to lift if the body contains a lambda
;     expression that has to be closed anyway.  This is the
;     only heuristic used below.

(define (POLICY:LIFT? L2 L args-to-add)
  (not (lambda? (lambda.body L2))))

; Interprocedural representation analysis.

; Given the callgraph for an expression in A-normal form,
; returns a hashtable mapping local variables to representation types.

(define (representation-analysis exp)
  (let* ((debugging? #f)
         (integrate-usual? (integrate-usual-procedures))
         (known (make-hashtable symbol-hash assq))
         (types (make-hashtable symbol-hash assq))
         (g (callgraph exp))
         (schedule (list (callgraphnode.code (car g))))
         (changed? #f)
         (mutate? #f))
    
    ; known is a hashtable that maps the name of a known local procedure
    ; to a list of the form (tv1 ... tvN), where tv1, ..., tvN
    ; are type variables that stand for the representation types of its
    ; arguments.  The type variable that stands for the representation
    ; type of the result of the procedure has the same name as the
    ; procedure itself.
    
    ; types is a hashtable that maps local variables and the names
    ; of known local procedures to an approximation of their
    ; representation type.
    ; For a known local procedure, the representation type is for the
    ; result of the procedure, not the procedure itself.
    
    ; schedule is a stack of work that needs to be done.
    ; Each entry in the stack is either an escaping lambda expression
    ; or the name of a known local procedure.
    
    (define (schedule! job)
      (if (not (memq job schedule))
          (set! schedule (cons job schedule))))
    
    ; Schedules a known local procedure.
    
    (define (schedule-known-procedure! name)
      ; Mark every known procedure that can actually be called.
      (callgraphnode.info! (assq name g) #t)
      (schedule! name))
    
    ; Schedule all code that calls the given known local procedure.
    
    (define (schedule-callers! name)
      (for-each (lambda (node)
                  (if (or (memq name (callgraphnode.tailcalls node))
                          (memq name (callgraphnode.nontailcalls node)))
                      (let ((caller (callgraphnode.name node)))
                        (if caller
                            (schedule! caller)
                            (schedule! (callgraphnode.code node))))))
                g))
    
    ; Returns true iff the given known procedure is known to be callable.
    
    (define (known-procedure-is-callable? name)
      (callgraphnode.info (assq name g)))
    
    ; Sets CHANGED? to #t and returns #t if the type variable's
    ; approximation has changed; otherwise returns #f.
    
    (define (update-typevar! tv type)
      (let* ((type0 (hashtable-get types tv))
             (type0 (or type0
                        (begin (hashtable-put! types tv rep:bottom)
                               rep:bottom)))
             (type1 (representation-union type0 type)))
        (if (eq? type0 type1)
            #f
            (begin (hashtable-put! types tv type1)
                   (set! changed? #t)
                   (if (and debugging? mutate?)
                       (begin (display "******** Changing type of ")
                              (display tv)
                              (display " from ")
                              (display (rep->symbol type0))
                              (display " to ")
                              (display (rep->symbol type1))
                              (newline)))
                   #t))))
    
    (define (lookup-code name)
      (callgraphnode.code (assq name g)))
    
    ; Given: a type variable, expression, and a set of constraints.
    ; Side effects:
    ;     Update the representation types of all variables that are
    ;         bound within the expression.
    ;     Update the representation types of all arguments to known
    ;         local procedures that are called within the expression.
    ;     If the representation type of an argument to a known local
    ;         procedure changes, then schedule that procedure's code
    ;         for analysis.
    ;     Update the constraint set to reflect the constraints that
    ;         hold following execution of the expression.
    ;     If mutate? is true, then transform the expression to rely
    ;         on the representation types that have been inferred.
    ; Return: type of the expression under the current assumptions
    ;     and constraints.
    
    (define (analyze exp constraints)
      
      (if (and #f debugging?)
          (begin (display "Analyzing: ")
                 (newline)
                 (pretty-print (make-readable exp #t))
                 (newline)))
      
      (case (car exp)
        
        ((quote)
         (representation-of-value (constant.value exp)))
        
        ((begin)
         (let* ((name (variable.name exp)))
           (representation-typeof name types constraints)))
        
        ((lambda)
         (schedule! exp)
         rep:procedure)
        
        ((set!)
         (analyze (assignment.rhs exp) constraints)
         (constraints-kill! constraints available:killer:globals)
         rep:object)
        
        ((if)
         (let* ((E0 (if.test exp))
                (E1 (if.then exp))
                (E2 (if.else exp))
                (type0 (analyze E0 constraints)))
           (if mutate?
               (cond ((representation-subtype? type0 rep:true)
                      (if.test-set! exp (make-constant #t)))
                     ((representation-subtype? type0 rep:false)
                      (if.test-set! exp (make-constant #f)))))
           (cond ((representation-subtype? type0 rep:true)
                  (analyze E1 constraints))
                 ((representation-subtype? type0 rep:false)
                  (analyze E2 constraints))
                 ((variable? E0)
                  (let* ((T0 (variable.name E0))
                         (ignored (analyze E0 constraints))
                         (constraints1 (copy-constraints-table constraints))
                         (constraints2 (copy-constraints-table constraints)))
                    (constraints-add! types
                                      constraints1
                                      (make-type-constraint
                                       T0 rep:true available:killer:immortal))
                    (constraints-add! types
                                      constraints2
                                      (make-type-constraint
                                       T0 rep:false available:killer:immortal))
                    (let* ((type1 (analyze E1 constraints1))
                           (type2 (analyze E2 constraints2))
                           (type (representation-union type1 type2)))
                      (constraints-intersect! constraints
                                              constraints1
                                              constraints2)
                      type)))
                 (else
                  (representation-error "Bad ANF" (make-readable exp #t))))))
        
        (else
         (let ((proc (call.proc exp))
               (args (call.args exp)))
           (cond ((lambda? proc)
                  (cond ((null? args)
                         (analyze-let0 exp constraints))
                        ((null? (cdr args))
                         (analyze-let1 exp constraints))
                        (else
                         (error "Compiler bug: pass3rep"))))
                 ((variable? proc)
                  (let* ((procname (variable.name proc)))
                    (cond ((hashtable-get known procname)
                           =>
                           (lambda (vars)
                             (analyze-known-call exp constraints vars)))
                          (integrate-usual?
                           (let ((entry (prim-entry procname)))
                             (if entry
                                 (analyze-primop-call exp constraints entry)
                                 (analyze-unknown-call exp constraints))))
                          (else
                           (analyze-unknown-call exp constraints)))))
                 (else
                  (analyze-unknown-call exp constraints)))))))
    
    (define (analyze-let0 exp constraints)
      (let ((proc (call.proc exp)))
        (if (null? (lambda.args proc))
            (analyze (lambda.body exp) constraints)
            (analyze-unknown-call exp constraints))))
    
    (define (analyze-let1 exp constraints)
      (let* ((proc (call.proc exp))
             (vars (lambda.args proc)))
        (if (and (pair? vars)
                 (null? (cdr vars)))
            (let* ((T1 (car vars))
                   (E1 (car (call.args exp))))
              (if (and integrate-usual? (call? E1))
                  (let ((proc (call.proc E1))
                        (args (call.args E1)))
                    (if (variable? proc)
                        (let* ((op (variable.name proc))
                               (entry (prim-entry op))
                               (K1 (if entry
                                       (prim-lives-until entry)
                                       available:killer:dead)))
                          (if (not (= K1 available:killer:dead))
                              ; Must copy the call to avoid problems
                              ; with side effects when mutate? is true.
                              (constraints-add!
                               types
                               constraints
                               (make-constraint T1
                                                (make-call proc args)
                                                K1)))))))
              (update-typevar! T1 (analyze E1 constraints))
              (analyze (lambda.body proc) constraints))
            (analyze-unknown-call exp constraints))))
    
    (define (analyze-primop-call exp constraints entry)
      (let* ((op (prim-opcodename entry))
             (args (call.args exp))
             (argtypes (map (lambda (arg) (analyze arg constraints))
                            args))
             (type (rep-result? op argtypes)))
        (constraints-kill! constraints (prim-kills entry))
        (cond ((and (eq? op 'check!)
                    (variable? (car args)))
               (let ((varname (variable.name (car args))))
                 (if (and mutate?
                          (representation-subtype? (car argtypes) rep:true))
                     (call.args-set! exp
                                     (cons (make-constant #t) (cdr args))))
                 (constraints-add! types
                                   constraints
                                   (make-type-constraint
                                    varname
                                    rep:true
                                    available:killer:immortal))))
              ((and mutate? (rep-specific? op argtypes))
               =>
               (lambda (newop)
                 (call.proc-set! exp (make-variable newop)))))
        (or type rep:object)))
    
    (define (analyze-known-call exp constraints vars)
      (let* ((procname (variable.name (call.proc exp)))
             (args (call.args exp))
             (argtypes (map (lambda (arg) (analyze arg constraints))
                            args)))
        (if (not (known-procedure-is-callable? procname))
            (schedule-known-procedure! procname))
        (for-each (lambda (var type)
                    (if (update-typevar! var type)
                        (schedule-known-procedure! procname)))
                  vars
                  argtypes)
        ; FIXME: We aren't analyzing the effects of known local procedures.
        (constraints-kill! constraints available:killer:all)
        (hashtable-get types procname)))
    
    (define (analyze-unknown-call exp constraints)
      (analyze (call.proc exp) constraints)
      (for-each (lambda (arg) (analyze arg constraints))
                (call.args exp))
      (constraints-kill! constraints available:killer:all)
      rep:object)
    
    (define (analyze-known-local-procedure name)
      (if debugging?
          (begin (display "Analyzing ")
                 (display name)
                 (newline)))
      (let ((L (lookup-code name))
            (constraints (make-constraints-table)))
        (let ((type (analyze (lambda.body L) constraints)))
          (if (update-typevar! name type)
              (schedule-callers! name))
          type)))
    
    (define (analyze-unknown-lambda L)
      (if debugging?
          (begin (display "Analyzing escaping lambda expression")
                 (newline)))
      (let ((vars (make-null-terminated (lambda.args L))))
        (for-each (lambda (var)
                    (hashtable-put! types var rep:object))
                  vars)
        (analyze (lambda.body L)
                 (make-constraints-table))))
    
    ; For debugging.
    
    (define (display-types)
      (hashtable-for-each (lambda (f vars)
                            (write f)
                            (display " : returns ")
                            (write (rep->symbol (hashtable-get types f)))
                            (newline)
                            (for-each (lambda (x)
                                        (display "  ")
                                        (write x)
                                        (display ": ")
                                        (write (rep->symbol
                                                (hashtable-get types x)))
                                        (newline))
                                      vars))
                          known))
    
    (define (display-all-types)
      (let* ((vars (hashtable-map (lambda (x type) x) types))
             (vars (twobit-sort (lambda (var1 var2)
                                  (string<=? (symbol->string var1)
                                             (symbol->string var2)))
                                vars)))
        (for-each (lambda (x)
                    (write x)
                    (display ": ")
                    (write (rep->symbol
                            (hashtable-get types x)))
                    (newline))
                  vars)))
    '
    (if debugging?
        (begin (pretty-print (make-readable (car schedule) #t))
               (newline)))
    (if debugging?
        (view-callgraph g))
    
    (for-each (lambda (node)
                (let* ((name (callgraphnode.name node))
                       (code (callgraphnode.code node))
                       (vars (make-null-terminated (lambda.args code)))
                       (known? (symbol? name))
                       (rep (if known? rep:bottom rep:object)))
                  (callgraphnode.info! node #f)
                  (if known?
                      (begin (hashtable-put! known name vars)
                             (hashtable-put! types name rep)))
                  (for-each (lambda (var)
                              (hashtable-put! types var rep))
                            vars)))
              g)
    
    (let loop ()
      (cond ((not (null? schedule))
             (let ((job (car schedule)))
               (set! schedule (cdr schedule))
               (if (symbol? job)
                   (analyze-known-local-procedure job)
                   (analyze-unknown-lambda job))
               (loop)))
            (changed?
             (set! changed? #f)
             (set! schedule (list (callgraphnode.code (car g))))
             (if debugging?
                 (begin (display-all-types) (newline)))
             (loop))))
    
    (if debugging?
        (display-types))
    
    (set! mutate? #t)
    
    ; We don't want to analyze known procedures that haven't been marked.
    
    (set! schedule
          (cons (callgraphnode.code (car g))
                (map callgraphnode.name
                     (filter (lambda (node)
                               (let* ((name (callgraphnode.name node))
                                      (known? (symbol? name))
                                      (marked?
                                       (known-procedure-is-callable? name)))
                                 (callgraphnode.info! node #f)
                                 (and known? marked?)))
                             g))))
    (let loop ()
      (if (not (null? schedule))
          (let ((job (car schedule)))
            (set! schedule (cdr schedule))
            (if (symbol? job)
                (analyze-known-local-procedure job)
                (analyze-unknown-lambda job))
            (loop))))
    
    (if changed?
        (error "Compiler bug in representation inference"))
    
    (if debugging?
        (pretty-print (make-readable (callgraphnode.code (car g)) #t)))
    
    exp))

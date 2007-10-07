; Copyright 1999 William D Clinger.
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
; Intraprocedural common subexpression elimination, constant propagation,
; copy propagation, dead code elimination, and register targeting.
;
; (intraprocedural-commoning E 'commoning)
;
;     Given an A-normal form E (alpha-converted, with correct free
;     variables and referencing information), returns an optimized
;     A-normal form with correct free variables but incorrect referencing
;     information.
;
; (intraprocedural-commoning E 'target-registers)
;
;     Given an A-normal form E (alpha-converted, with correct free
;     variables and referencing information), returns an A-normal form
;     with correct free variables but incorrect referencing information,
;     and in which MacScheme machine register names are used as temporary
;     variables.  The result is alpha-converted except for register names.
;
; (intraprocedural-commoning E 'commoning 'target-registers)
; (intraprocedural-commoning E)
;
;     Given an A-normal form as described above, returns an optimized
;     form in which register names are used as temporary variables.

; Semantics of .check!:
;
; (.check! b exn x ...) faults with code exn and arguments x ...
; if b is #f.

; The list of argument registers.
; This can't go in pass3commoning.aux.sch because that file must be
; loaded before the target-specific file that defines *nregs*.

(define argument-registers
  (do ((n (- *nregs* 2) (- n 1))
       (regs '()
             (cons (string->symbol
                    (string-append ".REG" (number->string n)))
                   regs)))
      ((zero? n)
       regs)))

(define (intraprocedural-commoning E . flags)
  
  (define target-registers? (or (null? flags) (memq 'target-registers flags)))
  (define commoning? (or (null? flags) (memq 'commoning flags)))
  
  (define debugging? #f)
  
  (call-with-current-continuation
   (lambda (return)
     
     (define (error . stuff)
       (display "Bug detected during intraprocedural optimization")
       (newline)
       (for-each (lambda (s)
                   (display s) (newline))
                 stuff)
       (return (make-constant #f)))
     
     ; Given an expression, an environment, the available expressions,
     ; and an ordered list of likely register variables (used heuristically),
     ; returns the transformed expression and its set of free variables.
     
     (define (scan-body E env available regvars)
       
       ; The local variables are those that are bound by a LET within
       ; this procedure.  The formals of a lambda expression and the
       ; known local procedures are counted as non-global, not local,
       ; because there is no let-binding for a formal that can be
       ; renamed during register targeting.
       ; For each local variable, we keep track of how many times it
       ; is referenced.  This information is not accurate until we
       ; are backing out of the recursion, and does not have to be.
       
       (define local-variables (make-oldstyle-hashtable symbol-hash assq))
       
       (define (local-variable? sym)
         (hashtable-get local-variables sym))
       
       (define (local-variable-not-used? sym)
         (= 0 (hashtable-fetch local-variables sym -1)))
       
       (define (local-variable-used-once? sym)
         (= 1 (hashtable-fetch local-variables sym 0)))
       
       (define (record-local-variable! sym)
         (hashtable-put! local-variables sym 0))
       
       (define (used-local-variable! sym)
         (adjust-local-variable! sym 1))
       
       (define (adjust-local-variable! sym n)
         (let ((m (hashtable-get local-variables sym)))
           (if debugging?
               (if (and m (> m 0))
                   (begin (write (list sym (+ m n)))
                          (newline))))
           (if m
               (hashtable-put! local-variables
                               sym
                               (+ m n)))))
       
       (define (closed-over-local-variable! sym)
         ; Set its reference count to infinity so it won't be optimized away.
         ; FIXME:  One million isn't infinity.
         (hashtable-put! local-variables sym 1000000))
       
       (define (used-variable! sym)
         (used-local-variable! sym))
       
       (define (abandon-expression! E)
         (cond ((variable? E)
                (adjust-local-variable! (variable.name E) -1))
               ((conditional? E)
                (abandon-expression! (if.test E))
                (abandon-expression! (if.then E))
                (abandon-expression! (if.else E)))
               ((call? E)
                (for-each (lambda (exp)
                            (if (variable? exp)
                                (let ((name (variable.name exp)))
                                  (if (local-variable? name)
                                      (adjust-local-variable! name -1)))))
                          (cons (call.proc E)
                                (call.args E))))))
       
       ; Environments are represented as hashtrees.
       
       (define (make-empty-environment)
         (make-hashtree symbol-hash assq))
       
       (define (environment-extend env sym)
         (hashtree-put env sym #t))
       
       (define (environment-extend* env symbols)
         (if (null? symbols)
             env
             (environment-extend* (hashtree-put env (car symbols) #t)
                                  (cdr symbols))))
       
       (define (environment-lookup env sym)
         (hashtree-get env sym))
       
       (define (global? x)
         (cond ((local-variable? x)
                #f)
               ((environment-lookup env x)
                #f)
               (else
                #t)))
       
       ;
       
       (define (available-add! available T E)
         (cond ((constant? E)
                (available-extend! available T E available:killer:immortal))
               ((variable? E)
                (available-extend! available
                                   T
                                   E
                                   (if (global? (variable.name E))
                                       available:killer:globals
                                       available:killer:immortal)))
               (else
                (let ((entry (prim-call E)))
                  (if entry
                      (let ((killer (prim-lives-until entry)))
                        (if (not (eq? killer available:killer:dead))
                            (do ((args (call.args E) (cdr args))
                                 (k killer
                                    (let ((arg (car args)))
                                      (if (and (variable? arg)
                                               (global? (variable.name arg)))
                                          available:killer:globals
                                          k))))
                                ((null? args)
                                 (available-extend!
                                  available
                                  T
                                  E
                                  (fxlogior killer k)))))))))))
       
       ; Given an expression E,
       ; an environment containing all variables that are in scope,
       ; and a table of available expressions,
       ; returns multiple values:
       ;   the transformed E
       ;   the free variables of E
       ;   the register bindings to be inserted; each binding has the form
       ;     (R x (begin R)), where (begin R) is a reference to R.
       ; 
       ; Side effects E.
       
       (define (scan E env available)
         (if (not (call? E))
             (scan-rhs E env available)
             (let ((proc (call.proc E)))
               (if (not (lambda? proc))
                   (scan-rhs E env available)
                   (let ((vars (lambda.args proc)))
                     (cond ((null? vars)
                            (scan-let0 E env available))
                           ((null? (cdr vars))
                            (scan-binding E env available))
                           (else
                            (error (make-readable E)))))))))
       
       ; E has the form of (let ((T1 E1)) E0).
       
       (define (scan-binding E env available)
         (let* ((L (call.proc E))
                (T1 (car (lambda.args L)))
                (E1 (car (call.args E)))
                (E0 (lambda.body L)))
           (record-local-variable! T1)
           (call-with-values
            (lambda () (scan-rhs E1 env available))
            (lambda (E1 F1 regbindings1)
              (available-add! available T1 E1)
              (let* ((env (let ((formals
                                 (make-null-terminated (lambda.args L))))
                            (environment-extend*
                             (environment-extend* env formals)
                             (map def.lhs (lambda.defs L)))))
                     (Fdefs (scan-defs L env available)))
                (call-with-values
                 (lambda () (scan E0 env available))
                 (lambda (E0 F0 regbindings0)
                   (lambda.body-set! L E0)
                   (if target-registers?
                       (scan-binding-phase2
                        L T1 E0 E1 F0 F1 Fdefs regbindings0 regbindings1)
                       (scan-binding-phase3
                        L E0 E1 (union F0 Fdefs)
                                F1 regbindings0 regbindings1)))))))))
       
       ; Given the lambda expression for a let expression that binds
       ; a single variable T1, the transformed body E0 and right hand side E1,
       ; their sets of free variables F0 and F1, the set of free variables
       ; for the internal definitions of L, and the sets of register
       ; bindings that need to be wrapped around E0 and E1, returns the
       ; transformed let expression, its free variables, and register
       ; bindings.
       ;
       ; This phase is concerned exclusively with register bindings,
       ; and is bypassed unless the target-registers flag is specified.
       
       (define (scan-binding-phase2
                L T1 E0 E1 F0 F1 Fdefs regbindings0 regbindings1)
         
         ; T1 can't be a register because we haven't
         ; yet inserted register bindings that high up.
         
         ; Classify the register bindings that need to wrapped around E0:
         ;     1.  those that have T1 as their rhs
         ;     2.  those whose lhs is a register that is likely to hold
         ;         a variable that occurs free in E1
         ;     3.  all others
         
         (define (phase2a)
           (do ((rvars regvars (cdr rvars))
                (regs argument-registers (cdr regs))
                (regs1 '() (if (memq (car rvars) F1)
                               (cons (car regs) regs1)
                               regs1)))
               ((or (null? rvars)
                    (null? regs))
                ; regs1 is the set of registers that are live for E1
                
                (let loop ((regbindings regbindings0)
                           (rb1 '())
                           (rb2 '())
                           (rb3 '()))
                  (if (null? regbindings)
                      (phase2b rb1 rb2 rb3)
                      (let* ((binding (car regbindings))
                             (regbindings (cdr regbindings))
                             (lhs (regbinding.lhs binding))
                             (rhs (regbinding.rhs binding)))
                        (cond ((eq? rhs T1)
                               (loop regbindings
                                     (cons binding rb1)
                                     rb2
                                     rb3))
                              ((memq lhs regs1)
                               (loop regbindings
                                     rb1
                                     (cons binding rb2)
                                     rb3))
                              (else
                               (loop regbindings
                                     rb1
                                     rb2
                                     (cons binding rb3))))))))))
         
         ; Determine which categories of register bindings should be
         ; wrapped around E0.
         ; Always wrap the register bindings in category 2.
         ; If E1 is a conditional or a real call, then wrap category 3.
         ; If T1 might be used more than once, then wrap category 1.
         
         (define (phase2b rb1 rb2 rb3)
           (if (or (conditional? E1)
                   (real-call? E1))
               (phase2c (append rb2 rb3) rb1 '())
               (phase2c rb2 rb1 rb3)))
         
         (define (phase2c towrap rb1 regbindings0)
           (cond ((and (not (null? rb1))
                       (local-variable-used-once? T1))
                  (phase2d towrap rb1 regbindings0))
                 (else
                  (phase2e (append rb1 towrap) regbindings0))))
         
         ; T1 is used only once, and there is a register binding (R T1).
         ; Change T1 to R.
         
         (define (phase2d towrap regbindings-T1 regbindings0)
           (if (not (null? (cdr regbindings-T1)))
               (error "incorrect number of uses" T1))
           (let* ((regbinding (car regbindings-T1))
                  (R (regbinding.lhs regbinding)))
             (lambda.args-set! L (list R))
             (phase2e towrap regbindings0)))
         
         ; Wrap the selected register bindings around E0.
         
         (define (phase2e towrap regbindings0)
           (call-with-values
            (lambda ()
              (wrap-with-register-bindings towrap E0 F0))
            (lambda (E0 F0)
              (let ((F (union Fdefs F0)))
                (scan-binding-phase3
                 L E0 E1 F F1 regbindings0 regbindings1)))))
         
         (phase2a))
       
       ; This phase, with arguments as above, constructs the result.
       
       (define (scan-binding-phase3 L E0 E1 F F1 regbindings0 regbindings1)
         (let* ((args (lambda.args L))
                (T1 (car args))
                (free (union F1 (difference F args)))
                (simple-let? (simple-lambda? L))
                (regbindings 
                 
                 ; At least one of regbindings0 and regbindings1
                 ; is the empty list.
                 
                 (cond ((null? regbindings0)
                        regbindings1)
                       ((null? regbindings1)
                        regbindings0)
                       (else
                        (error 'scan-binding 'regbindings)))))
           (lambda.body-set! L E0)
           (lambda.F-set! L F)
           (lambda.G-set! L F)
           (cond ((and simple-let?
                       (not (memq T1 F))
                       (no-side-effects? E1))
                  (abandon-expression! E1)
                  (values E0 F regbindings0))
                 ((and target-registers?
                       simple-let?
                       (local-variable-used-once? T1))
                  (post-simplify-anf L T1 E0 E1 free regbindings #f))
                 (else
                  (values (make-call L (list E1))
                          free
                          regbindings)))))
       
       (define (scan-let0 E env available)
         (let ((L (call.proc E)))
           (if (simple-lambda? L)
               (scan (lambda.body L) env available)
               (let ((T1 (make-variable name:IGNORED)))
                 (lambda.args-set! L (list T1))
                 (call-with-values
                  (lambda () (scan (make-call L (list (make-constant 0)))
                                   env
                                   available))
                  (lambda (E F regbindings)
                    (lambda.args-set! L '())
                    (values (make-call L '())
                            F
                            regbindings)))))))
       
       ; Optimizes the internal definitions of L and returns their
       ; free variables.
       
       (define (scan-defs L env available)
         (let loop ((defs (lambda.defs L))
                    (newdefs '())
                    (Fdefs '()))
           (if (null? defs)
               (begin (lambda.defs-set! L (reverse newdefs))
                      Fdefs)
               (let ((def (car defs)))
                 (call-with-values
                  (lambda ()
                    (let* ((Ldef (def.rhs def))
                           (Lformals (make-null-terminated (lambda.args Ldef)))
                           (Lenv (environment-extend*
                                  (environment-extend* env Lformals)
                                  (map def.lhs (lambda.defs Ldef)))))
                      (scan Ldef Lenv available)))
                  (lambda (rhs Frhs empty)
                    (if (not (null? empty))
                        (error 'scan-binding 'def))
                    (loop (cdr defs)
                          (cons (make-definition (def.lhs def) rhs)
                                newdefs)
                          (union Frhs Fdefs))))))))
       
       ; Given the right-hand side of a let-binding, an environment,
       ; and a table of available expressions, returns the transformed
       ; expression, its free variables, and the register bindings that
       ; need to be wrapped around it.
       
       (define (scan-rhs E env available)
         
         (cond
          ((constant? E)
           (values E (empty-set) '()))
          
          ((variable? E)
           (let* ((name (variable.name E))
                  (Enew (and commoning?
                             (if (global? name)
                                 (let ((T (available-expression
                                           available E)))
                                   (if T
                                       (make-variable T)
                                       #f))
                                 (available-variable available name)))))
             (if Enew
                 (scan-rhs Enew env available)
                 (begin (used-variable! name)
                        (values E (list name) '())))))
          
          ((lambda? E)
           (let* ((formals (make-null-terminated (lambda.args E)))
                  (env (environment-extend*
                        (environment-extend* env formals)
                        (map def.lhs (lambda.defs E))))
                  (Fdefs (scan-defs E env available)))
             (call-with-values
              (lambda ()
                (let ((available (copy-available-table available)))
                  (available-kill! available available:killer:all)
                  (scan-body (lambda.body E)
                             env
                             available
                             formals)))
              (lambda (E0 F0 regbindings0)
                (call-with-values
                 (lambda ()
                   (wrap-with-register-bindings regbindings0 E0 F0))
                 (lambda (E0 F0)
                   (lambda.body-set! E E0)
                   (let ((F (union Fdefs F0)))
                     (for-each (lambda (x)
                                 (closed-over-local-variable! x))
                               F)
                     (lambda.F-set! E F)
                     (lambda.G-set! E F)
                     (values E
                             (difference F
                                         (make-null-terminated
                                          (lambda.args E)))
                             '()))))))))
          
          ((conditional? E)
           (let ((E0 (if.test E))
                 (E1 (if.then E))
                 (E2 (if.else E)))
             (if (constant? E0)
                 ; FIXME: E1 and E2 might not be a legal rhs,
                 ; so we can't just return the simplified E1 or E2.
                 (let ((E1 (if (constant.value E0) E1 E2)))
                   (call-with-values
                    (lambda () (scan E1 env available))
                    (lambda (E1 F1 regbindings1)
                      (cond ((or (not (call? E1))
                                 (not (lambda? (call.proc E1))))
                             (values E1 F1 regbindings1))
                            (else
                             ; FIXME: Must return a valid rhs.
                             (values (make-conditional
                                      (make-constant #t)
                                      E1
                                      (make-constant 0))
                                     F1
                                     regbindings1))))))
                 (call-with-values
                  (lambda () (scan E0 env available))
                  (lambda (E0 F0 regbindings0)
                    (if (not (null? regbindings0))
                        (error 'scan-rhs 'if))
                    (if (not (eq? E0 (if.test E)))
                        (scan-rhs (make-conditional E0 E1 E2)
                                  env available)
                        (let ((available1
                               (copy-available-table available))
                              (available2
                               (copy-available-table available)))
                          (if (variable? E0)
                              (let ((T0 (variable.name E0)))
                                (available-add!
                                 available2 T0 (make-constant #f)))
                              (error (make-readable E #t)))
                          (call-with-values
                           (lambda () (scan E1 env available1))
                           (lambda (E1 F1 regbindings1)
                             (call-with-values
                              (lambda ()
                                (wrap-with-register-bindings
                                 regbindings1 E1 F1))
                              (lambda (E1 F1)
                                (call-with-values
                                 (lambda () (scan E2 env available2))
                                 (lambda (E2 F2 regbindings2)
                                   (call-with-values
                                    (lambda ()
                                      (wrap-with-register-bindings
                                       regbindings2 E2 F2))
                                    (lambda (E2 F2)
                                      (let ((E (make-conditional
                                                E0 E1 E2))
                                            (F (union F0 F1 F2)))
                                        (available-intersect!
                                         available
                                         available1
                                         available2)
                                        (values E F '())))))))))))))))))
          
          
          ((assignment? E)
           (call-with-values
            (lambda () (scan-rhs (assignment.rhs E) env available))
            (lambda (E1 F1 regbindings1)
              (if (not (null? regbindings1))
                  (error 'scan-rhs 'set!))
              (available-kill! available available:killer:globals)
              (values (make-assignment (assignment.lhs E) E1)
                      (union (list (assignment.lhs E)) F1)
                      '()))))
          
          ((begin? E)
           ; Shouldn't occur in A-normal form.
           (error 'scan-rhs 'begin))
          
          ((real-call? E)
           (let* ((E0 (call.proc E))
                  (args (call.args E))
                  (regcontents (append regvars
                                       (map (lambda (x) #f) args))))
             (let loop ((args args)
                        (regs argument-registers)
                        (regcontents regcontents)
                        (newargs '())
                        (regbindings '())
                        (F (if (variable? E0)
                               (let ((f (variable.name E0)))
                                 (used-variable! f)
                                 (list f))
                               (empty-set))))
               (cond ((null? args)
                      (available-kill! available available:killer:all)
                      (values (make-call E0 (reverse newargs))
                              F
                              regbindings))
                     ((null? regs)
                      (let ((arg (car args)))
                        (loop (cdr args)
                              '()
                              (cdr regcontents)
                              (cons arg newargs)
                              regbindings
                              (if (variable? arg)
                                  (let ((name (variable.name arg)))
                                    (used-variable! name)
                                    (union (list name) F))
                                  F))))
                     ((and commoning?
                           (variable? (car args))
                           (available-variable
                            available
                            (variable.name (car args))))
                      (let* ((name (variable.name (car args)))
                             (Enew (available-variable available name)))
                        (loop (cons Enew (cdr args))
                              regs regcontents newargs regbindings F)))
                     ((and target-registers?
                           (variable? (car args))
                           (let ((x (variable.name (car args))))
                             ; We haven't yet recorded this use.
                             (or (local-variable-not-used? x)
                                 (and (memq x regvars)
                                      (not (eq? x (car regcontents)))))))
                      (let* ((x (variable.name (car args)))
                             (R (car regs))
                             (newarg (make-variable R)))
                        (used-variable! x)
                        (loop (cdr args)
                              (cdr regs)
                              (cdr regcontents)
                              (cons newarg newargs)
                              (cons (make-regbinding R x newarg)
                                    regbindings)
                              (union (list R) F))))
                     (else
                      (let ((E1 (car args)))
                        (loop (cdr args)
                              (cdr regs)
                              (cdr regcontents)
                              (cons E1 newargs)
                              regbindings
                              (if (variable? E1)
                                  (let ((name (variable.name E1)))
                                    (used-variable! name)
                                    (union (list name) F))
                                  F))))))))
          
          ((call? E)
           ; Must be a call to a primop.
           (let* ((E0 (call.proc E))
                  (f0 (variable.name E0)))
             (let loop ((args (call.args E))
                        (newargs '())
                        (F (list f0)))
               (cond ((null? args)
                      (let* ((E (make-call E0 (reverse newargs)))
                             (T (and commoning?
                                     (available-expression
                                      available E))))
                        (if T
                            (begin (abandon-expression! E)
                                   (scan-rhs (make-variable T) env available))
                            (begin
                             (available-kill!
                              available
                              (prim-kills (prim-entry f0)))
                             (cond ((eq? f0 name:CHECK!)
                                    (let ((x (car (call.args E))))
                                      (cond ((not (runtime-safety-checking))
                                             (abandon-expression! E)
                                             ;(values x '() '())
                                             (scan-rhs x env available))
                                            ((variable? x)
                                             (available-add!
                                              available
                                              (variable.name x)
                                              (make-constant #t))
                                             (values E F '()))
                                            ((constant.value x)
                                             (abandon-expression! E)
                                             (values x '() '()))
                                            (else
                                             (declaration-error E)
                                             (values E F '())))))
                                   (else
                                    (values E F '())))))))
                     ((variable? (car args))
                      (let* ((E1 (car args))
                             (x (variable.name E1))
                             (Enew
                              (and commoning?
                                   (available-variable available x))))
                        (if Enew
                            ; All of the arguments are constants or
                            ; variables, so if the variable is replaced
                            ; here it will be replaced throughout the call.
                            (loop (cons Enew (cdr args))
                                  newargs
                                  (remq x F))
                            (begin
                             (used-variable! x)
                             (loop (cdr args)
                                   (cons (car args) newargs)
                                   (union (list x) F))))))
                     (else
                      (loop (cdr args)
                            (cons (car args) newargs)
                            F))))))
          
          (else
           (error 'scan-rhs (make-readable E)))))
       
       (call-with-values
        (lambda () (scan E env available))
        (lambda (E F regbindings)
          (call-with-values
           (lambda () (wrap-with-register-bindings regbindings E F))
           (lambda (E F)
             (values E F '()))))))
     
     (call-with-values
      (lambda ()
        (scan-body E
                   (make-hashtree symbol-hash assq)
                   (make-available-table)
                   '()))
      (lambda (E F regbindings)
        (if (not (null? regbindings))
            (error 'scan-body))
        E)))))

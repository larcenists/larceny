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
; 14 April 1999.
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

; FIXME: an expression like (vector-ref v i) implies the availability
; of (vector? v) and (index? i).  The relationship between representation
; inference and commoning needs to be sorted out.

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
  
  ; A real call is a call whose procedure expression is
  ; neither a lambda expression nor a primop.
  
  (define (real-call? E)
    (and (call? E)
         (let ((proc (call.proc E)))
           (and (not (lambda? proc))
                (or (not (variable? proc))
                    (let ((f (variable.name proc)))
                      (or (not (integrate-usual-procedures))
                          (not (prim-entry f)))))))))
  
  (define (prim-call E)
    (and (call? E)
         (let ((proc (call.proc E)))
           (and (variable? proc)
                (integrate-usual-procedures)
                (prim-entry (variable.name proc))))))
  
  (define (no-side-effects? E)
    (or (constant? E)
        (variable? E)
        (lambda? E)
        (and (conditional? E)
             (no-side-effects? (if.test E))
             (no-side-effects? (if.then E))
             (no-side-effects? (if.else E)))
        (and (call? E)
             (let ((proc (call.proc E)))
               (and (variable? proc)
                    (integrate-usual-procedures)
                    (let ((entry (prim-entry (variable.name proc))))
                      (and entry
                           (not (eq? available:killer:dead
                                     (prim-lives-until entry))))))))))
  
  (define (make-regbinding lhs rhs use)
    (list lhs rhs use))
  
  (define (regbinding.lhs x) (car x))
  (define (regbinding.rhs x) (cadr x))
  (define (regbinding.use x) (caddr x))
  
  (define (wrap-with-register-bindings regbindings E F)
    (if (null? regbindings)
        (values E F)
        (let* ((regbinding (car regbindings))
               (R (regbinding.lhs regbinding))
               (x (regbinding.rhs regbinding)))
          (wrap-with-register-bindings
           (cdr regbindings)
           (make-call (make-lambda (list R) '() '() F F '() #f E)
                      (list (make-variable x)))
           (union (list x)
                  (difference F (list R)))))))
  
  ; Returns two values:
  ;   the subset of regbindings that have x as their right hand side
  ;   the rest of regbindings
  
  (define (register-bindings regbindings x)
    (define (loop regbindings to-x others)
      (cond ((null? regbindings)
             (values to-x others))
            ((eq? x (regbinding.rhs (car regbindings)))
             (loop (cdr regbindings)
                   (cons (car regbindings) to-x)
                   others))
            (else
             (loop (cdr regbindings)
                   to-x
                   (cons (car regbindings) others)))))
    (loop regbindings '() '()))
  
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
       
       (define local-variables '())
       (define local-variables-used-once '())
       
       (define (global? x)
         (if (memq x local-variables)
             #f
             (let loop ((env env))
               (cond ((null? env)
                      #t)
                     ((assq x (lambda.R (car env)))
                      #f)
                     (else
                      (loop (cdr env)))))))
       
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
                                  (logior killer k)))))))))))
       
       ; Given an expression E,
       ; a list of lambda expressions env,
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
                (E0 (lambda.body L))
                (R (lambda.R L)))
           (set! local-variables (cons T1 local-variables))
           (if (= 1 (length (references R T1)))
               (set! local-variables-used-once
                     (cons T1 local-variables-used-once)))
           (call-with-values
            (lambda () (scan-rhs E1 env available))
            (lambda (E1 F1 regbindings1)
              (available-add! available T1 E1)
              (let* ((env (cons L env))
                     (Fdefs (scan-defs L env available)))
                (call-with-values
                 (lambda () (scan E0 env available))
                 (lambda (E0 F0 regbindings0)
                   (lambda.body-set! L E0)
                   (if target-registers?
                       (scan-binding-phase2
                        L T1 E0 E1 F0 F1 Fdefs regbindings0 regbindings1)
                       (scan-binding-phase3
                        L E0 E1 (union F0 Fdefs) F1
                                                 regbindings0 regbindings1)))))))))
       
       ; Given the lambda expression for a let expression that binds
       ; a single variable T1, the transformed body E0 and right hand side E1,
       ; their sets of free variables F0 and F1, the set of free variables
       ; for the internal definitions of L, and the sets of register
       ; bindings that need to be wrapped around E0 and E1, returns the
       ; transformed let expression, its free variables, and register
       ; bindings.
       ;
       ; This phase is concerned exclusively with register bindings,
       ; and is bypassed when the target-registers flag is not specified.
       
       (define (scan-binding-phase2
                L T1 E0 E1 F0 F1 Fdefs regbindings0 regbindings1)
         
         ; T1 can't be a register because we haven't
         ; yet inserted register bindings that high up.
         ; Find the register bindings that have T1 as rhs.
         
         (call-with-values
          (lambda () (register-bindings regbindings0 T1))
          (lambda (regbindings-T1 regbindings0)
            (call-with-values
             (lambda ()
               (if (or (conditional? E1)
                       (real-call? E1))
                   (call-with-values
                    (lambda ()
                      (wrap-with-register-bindings regbindings0 E0 F0))
                    (lambda (E0 F0)
                      (values E0 F0 '())))
                   (values E0 F0 regbindings0)))
             (lambda (E0 F0 regbindings0)
               
               ; If E1 is a conditional or a real call,
               ; then regbindings0 is now empty (see above).
               
               (call-with-values
                (lambda ()
                  (cond
                   ((null? regbindings-T1)
                    ; nothing to do in this case
                    (values E0 F0))
                   ((memq T1 local-variables-used-once)
                    (if (not (null? (cdr regbindings-T1)))
                        (error "incorrect number of uses" T1))
                    (let* ((regbinding (car regbindings-T1))
                           (R (regbinding.lhs regbinding))
                           (Ruse (regbinding.use regbinding)))
                      (lambda.args-set! L (list R))
                      (values E0 F0)))
                   (else
                    (wrap-with-register-bindings regbindings-T1 E0 F0))))
                (lambda (E0 F0)
                  (let ((F (union Fdefs F0)))
                    (scan-binding-phase3
                     L E0 E1 F F1 regbindings0 regbindings1)))))))))
       
       ; This phase, with arguments as above, constructs the result.
       
       (define (scan-binding-phase3 L E0 E1 F F1 regbindings0 regbindings1)
         (let* ((args (lambda.args L))
                (T1 (car args))
                (free (union F1 (difference F args)))
                (simple-let? (and (null? (lambda.defs L))
                                  (null? (lambda.decls L))))
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
                  (values E0 F regbindings0))
                 ((and target-registers?
                       simple-let?
                       (memq T1 local-variables-used-once))
                  (post-simplify-anf L T1 E0 E1 free regbindings #f))
                 (else
                  (values (make-call L (list E1))
                          free
                          regbindings)))))
       
       (define (scan-let0 E env available)
         (let ((L (call.proc E)))
           (if (and (null? (lambda.defs L))
                    (null? (lambda.decls L)))
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
                    (let ((Ldef (def.rhs def)))
                      (scan Ldef (cons Ldef env) available)))
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
                 (begin (if (variable? Enew)
                            (set! local-variables-used-once
                                  (remq (variable.name Enew)
                                        local-variables-used-once)))
                        (scan-rhs Enew env available))
                 (values E (list name) '()))))
          
          ((lambda? E)
           (let* ((env (cons E env))
                  (Fdefs (scan-defs E env available))
                  (locals local-variables)
                  (locals-used-once local-variables-used-once))
             (call-with-values
              (lambda ()
                (let ((available (copy-available-table available)))
                  (available-kill! available available:killer:all)
                  (scan-body (lambda.body E)
                             (cons E env)
                             available
                             (make-null-terminated (lambda.args E)))))
              (lambda (E0 F0 regbindings0)
                (call-with-values
                 (lambda ()
                   (wrap-with-register-bindings regbindings0 E0 F0))
                 (lambda (E0 F0)
                   (lambda.body-set! E E0)
                   (let ((F (union Fdefs F0)))
                     ; I don't believe the comment below, but I'll
                     ; try it out on some tests before I delete the
                     ; code.
                     (if (not (and (eq? local-variables locals)
                                   (eq? local-variables-used-once
                                        locals-used-once)))
                         (error 'local-variables
                                local-variables
                                locals
                                local-variables-used-once
                                locals-used-once))
                     ; Restoring these two variables isn't necessary
                     ; for correctness, but improves efficiency.
                     (set! local-variables locals)
                     (set! locals-used-once local-variables-used-once)
                     ; If you restore local-variables-used-once, then
                     ; you must update it to remain correct.
                     (for-each
                      (lambda (x)
                        (set! local-variables-used-once
                              (remq x local-variables-used-once)))
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
                               (list (variable.name E0))
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
                                  (union (list (variable.name arg))
                                         F)
                                  F))))
                     ((and commoning?
                           (variable? (car args))
                           (available-variable
                            available
                            (variable.name (car args))))
                      (let* ((name (variable.name (car args)))
                             (Enew (available-variable available name)))
                        (if (variable? Enew)
                            (set! local-variables-used-once
                                  (remq (variable.name Enew)
                                        local-variables-used-once)))
                        (loop (cons Enew (cdr args))
                              regs regcontents newargs regbindings F)))
                     ((and target-registers?
                           (variable? (car args))
                           (let ((x (variable.name (car args))))
                             (or (memq x local-variables-used-once)
                                 (and (memq x regvars)
                                      (not (eq? x (car regcontents)))))))
                      (let* ((x (variable.name (car args)))
                             (R (car regs))
                             (newarg (make-variable R)))
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
                                  (union (list (variable.name E1))
                                         F)
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
                            (begin
                             (set! local-variables-used-once
                                   (remq T
                                         local-variables-used-once))
                             (values (make-variable T)
                                     (list T)
                                     '()))
                            (begin
                             (available-kill!
                              available
                              (prim-kills (prim-entry f0)))
                             (values E F '())))))
                     ((variable? (car args))
                      (let* ((E1 (car args))
                             (x (variable.name E1))
                             (Enew
                              (and commoning?
                                   (available-variable available x))))
                        (if Enew
                            (begin
                             (if (variable? Enew)
                                 (set! local-variables-used-once
                                       (remq (variable.name Enew)
                                             local-variables-used-once)))
                             (loop (cons Enew (cdr args))
                                   newargs
                                   F))
                            (loop (cdr args)
                                  (cons (car args) newargs)
                                  (union (list x) F)))))
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
      (lambda () (scan-body E '() (make-available-table) '()))
      (lambda (E F regbindings)
        (if (not (null? regbindings))
            (error 'scan-body))
        E)))))

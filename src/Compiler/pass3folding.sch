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
; Interprocedural constant propagation and folding.
;
; Constant propagation must converge before constant folding can be
; performed.  Constant folding creates more constants that can be
; propagated, so these two optimizations must be iterated, but it
; is safe to stop at any time.
;
; Abstract interpretation for constant folding.
;
; The abstract values are
;     bottom    (represented here by #f)
;     constants (represented by quoted literals)
;     top       (represented here by #t)
;
; Let [[ E ]] be the abstract interpretation of E over that domain
; of abstract values, with respect to some arbitrary set of abstract
; values for local variables.
;
; If a is a global variable or a formal parameter of an escaping
; lambda expression, then [[ a ]] = #t.
;
; If x is the ith formal parameter of a known local procedure f,
; then [[ x ]] = \join_{(f E1 ... En)} [[ Ei ]].
;
; [[ K ]] = K
; [[ L ]] = #t
; [[ (begin E1 ... En) ]] = [[ En ]]
; [[ (set! I E) ]] = #f
;
; If [[ E0 ]] = #t, then [[ (if E0 E1 E2) ]] = [[ E1 ]] \join [[ E2 ]]
; else if [[ E0 ]] = K, then [[ (if E0 E1 E2) ]] = [[ E1 ]]
;                         or [[ (if E0 E1 E2) ]] = [[ E2 ]]
;                       depending upon K
; else [[ (if E0 E1 E2) ]] = #f
;
; If f is a known local procedure with body E,
;     then [[ (f E1 ... En) ]] = [[ E ]]
;
; If g is a foldable integrable procedure, then:
; if there is some i for which [[ Ei ]] = #t,
;     then [[ (g E1 ... En) ]] = #t
; else if [[ E1 ]] = K1, ..., [[ En ]] = Kn,
;     then [[ (g E1 ... En) ]] = (g K1 ... Kn)
; else [[ (g E1 ... En) ]] = #f
;
; Symbolic representations of abstract values.
; (Can be thought of as mappings from abstract environments to
; abstract values.)
;
; <symbolic>     ::=  #t  |  ( <expressions> )
; <expressions>  ::=  <empty>  |  <expression> <expressions>

; Parameter to limit constant propagation and folding.
; This parameter can be tuned later.

(define *constant-propagation-limit* 5)

; Given an expression as output by pass 2, performs constant
; propagation and folding.

(define (constant-propagation exp)
  (define (constant-propagation exp i)
    (if (< i *constant-propagation-limit*)
        (begin
         ;(display "Performing constant propagation and folding...")
         ;(newline)
         (let* ((g (callgraph exp))
                (L (callgraphnode.code (car g)))
                (variables (constant-propagation-using-callgraph g))
                (changed? (constant-folding! L variables)))
           (if changed?
               (constant-propagation (lambda.body L) (+ i 1))
               (lambda.body L))))))
  (constant-propagation exp 0))

; Given a callgraph, returns a hashtable of abstract values for
; all local variables.

(define (constant-propagation-using-callgraph g)
  (let ((debugging? #f)
        (folding? #t)
        (known (make-oldstyle-hashtable))
        (variables (make-oldstyle-hashtable))
        (counter 0))
    
    ; Computes joins of abstract values.
    
    (define (join x y)
      (cond ((boolean? x)
             (if x #t y))
            ((boolean? y)
             (join y x))
            ((equal? x y)
             x)
            (else #t)))
    
    ; Given a <symbolic> and a vector of abstract values,
    ; evaluates the <symbolic> and returns its abstract value.
    
    (define (aeval rep env)
      (cond ((eq? rep #t)
             #t)
            ((null? rep)
             #f)
            ((null? (cdr rep))
             (aeval1 (car rep) env))
            (else
             (join (aeval1 (car rep) env)
                   (aeval (cdr rep) env)))))
    
    (define (aeval1 exp env)

      (cond

        ((constant? exp)
         exp)
 
        ((lambda? exp)
         #t)
 
        ((assignment? exp)
         #f)
 
        ((variable? exp) (let* ((name (variable.name exp))
                                (i (hashtable-get variables name)))
                           (if i
                               (vector-ref env i)
                               #t)))
 
        ((begin? exp) (aeval1-error))
 
        ((conditional? exp)
         (let* ((val0 (aeval1 (if.test exp) env))
                (val1 (aeval1 (if.then exp) env))
                (val2 (aeval1 (if.else exp) env)))
           (cond ((eq? val0 #t)
                  (join val1 val2))
                 ((pair? val0)
                  (if (constant.value val0)
                      val1
                      val2))
                 (else
                  #f))))
 
        ((call? exp)
 
         (do ((exprs (reverse (call.args exp)) (cdr exprs))
              (vals '() (cons (aeval1 (car exprs) env) vals)))
             ((null? exprs)
              (let ((proc (call.proc exp)))
                (cond ((variable? proc)
                       (let* ((procname (variable.name proc))
                              (procnode (hashtable-get known procname))
                              (entry (if folding?
                                         (constant-folding-entry procname)
                                         #f)))
                         (cond (procnode
                                (vector-ref env
                                            (hashtable-get variables
                                                           procname)))
                               (entry
                                ; FIXME: No constant folding
                                #t)
                               (else (aeval1-error)))))
                      (else
                       (aeval1-error)))))))
        (else (twobit-bug "unrecognized expression." exp))))

    (define (aeval1-error)
      (twobit-bug "Compiler bug: constant propagation (aeval1)"))
    
    ; Combines two <symbolic>s.
    
    (define (combine-symbolic rep1 rep2)
      (cond ((eq? rep1 #t) #t)
            ((eq? rep2 #t) #t)
            (else
             (append rep1 rep2))))
    
    ; Given an expression, returns a <symbolic> that represents
    ; a list of expressions whose abstract values can be joined
    ; to obtain the abstract value of the given expression.
    ; As a side effect, enters local variables into variables.
    
    (define (collect! exp)

      (cond

        ((constant? exp)
         (list exp))
 
        ((lambda? exp)
         #t)
 
        ((assignment? exp)
         (collect! (assignment.rhs exp))
         '())
 
        ((variable? exp) (list exp))
 
        ((begin? exp)
         (do ((exprs (begin.exprs exp) (cdr exprs)))
             ((null? (cdr exprs))
              (collect! (car exprs)))
           (collect! (car exprs))))
 
        ((conditional? exp)
         (collect! (if.test exp))
         (collect! (if.then exp))
         (collect! (if.else exp))
         #t)
 
        ((call? exp)
         (do ((exprs (reverse (call.args exp)) (cdr exprs))
              (reps '() (cons (collect! (car exprs)) reps)))
             ((null? exprs)
              (let ((proc (call.proc exp)))
                (define (put-args! args reps)
                  (cond ((pair? args)
                         (let ((v (car args))
                               (rep (car reps)))
                           (hashtable-put! variables v rep)
                           (put-args! (cdr args) (cdr reps))))
                        ((symbol? args)
                         (hashtable-put! variables args #t))
                        (else #f)))
                (cond ((variable? proc)
                       (let* ((procname (variable.name proc))
                              (procnode (hashtable-get known procname))
                              (entry (if folding?
                                         (constant-folding-entry procname)
                                         #f)))
                         (cond (procnode
                                (for-each (lambda (v rep)
                                            (hashtable-put!
                                             variables
                                             v
                                             (combine-symbolic
                                              rep (hashtable-get variables v))))
                                          (lambda.args
                                            (callgraphnode.code procnode))
                                          reps)
                                (list (make-variable procname)))
                               (entry
                                ; FIXME: No constant folding
                                #t)
                               (else #t))))
                      ((lambda? proc)
                       (put-args! (lambda.args proc) reps)
                       (collect! (lambda.body proc)))
                      (else
                       (collect! proc)
                       #t))))))
        (else
         (twobit-bug "unrecognized expression" exp))))

    (for-each (lambda (node)
                (let* ((name (callgraphnode.name node))
                       (code (callgraphnode.code node))
                       (known? (symbol? name))
                       (rep (if known? '() #t)))
                  (if known?
                      (hashtable-put! known name node))
                  (if (lambda? code)
                      (for-each (lambda (var)
                                  (hashtable-put! variables var rep))
                                (make-null-terminated (lambda.args code))))))
              g)
    
    (for-each (lambda (node)
                (let ((name (callgraphnode.name node))
                      (code (callgraphnode.code node)))
                  (cond ((symbol? name)
                         (hashtable-put! variables
                                         name
                                         (collect! (lambda.body code))))
                        (else
                         (collect! (lambda.body code))))))
              g)
    
    (if (and #f debugging?)
        (begin
         (hashtable-for-each (lambda (v rep)
                               (write v)
                               (display ": ")
                               (write rep)
                               (newline))
                             variables)
         
         (display "----------------------------------------")
         (newline)))
    
    ;(trace aeval aeval1)
    
    (let* ((n (hashtable-size variables))
           (vars (hashtable-map (lambda (v rep) v) variables))
           (reps (map (lambda (v) (hashtable-get variables v)) vars))
           (init (make-vector n #f))
           (next (make-vector n)))
      (do ((i 0 (+ i 1))
           (vars vars (cdr vars))
           (reps reps (cdr reps)))
          ((= i n))
          (hashtable-put! variables (car vars) i)
          (vector-set! next
                       i
                       (let ((rep (car reps)))
                         (lambda (env)
                           (aeval rep env)))))
      (compute-fixedpoint init next equal?)
      (for-each (lambda (v)
                  (let* ((i (hashtable-get variables v))
                         (aval (vector-ref init i)))
                    (hashtable-put! variables v aval)
                    (if (and debugging?
                             (not (eq? aval #t)))
                        (begin (write v)
                               (display ": ")
                               (write aval)
                               (newline)))))
                vars)
      variables)))

; Given a lambda expression, performs constant propagation, folding,
; and simplifications by side effect, using the abstract values in the
; hash table of variables.
; Returns #t if any new constants were created by constant folding,
; otherwise returns #f.

(define (constant-folding! L variables)
  (let ((debugging? #f)
        (msg1 "    Propagating constant value for ")
        (msg2 "    Folding: ")
        (msg3 " ==> ")
        (folding? #t)
        (changed? #f))
    
    ; Given a known lambda expression L, its original formal parameters,
    ; and a list of all calls to L, deletes arguments that are now
    ; ignored because of constant propagation.
    
    (define (delete-ignored-args! L formals0 calls)
      (let ((formals1 (lambda.args L)))
        (for-each (lambda (call)
                    (do ((formals0 formals0 (cdr formals0))
                         (formals1 formals1 (cdr formals1))
                         (args (call.args call)
                               (cdr args))
                         (newargs '()
                                  (if (and (eq? (car formals1) name:IGNORED)
                                           (pair?
                                            (hashtable-get variables
                                                           (car formals0))))
                                      newargs
                                      (cons (car args) newargs))))
                        ((null? formals0)
                         (call.args-set! call (reverse newargs)))))
                  calls)
        (do ((formals0 formals0 (cdr formals0))
             (formals1 formals1 (cdr formals1))
             (formals2 '()
                       (if (and (not (eq? (car formals0)
                                          (car formals1)))
                                (eq? (car formals1) name:IGNORED)
                                (pair?
                                 (hashtable-get variables
                                                (car formals0))))
                           formals2
                           (cons (car formals1) formals2))))
            ((null? formals0)
             (lambda.args-set! L (reverse formals2))))))
    
    (define (fold! exp)
      
      (cond

        ((constant? exp) exp)
 
        ((lambda? exp)
         (let ((Rinfo (lambda.R exp))
               (known (map def.lhs (lambda.defs exp))))
           (for-each (lambda (entry)
                       (let* ((v (R-entry.name entry))
                              (aval (hashtable-fetch variables v #t)))
                         (if (and (pair? aval)
                                  (not (memq v known)))
                             (let ((x (constant.value aval)))
                               (if (or (boolean? x)
                                       (null? x)
                                       (symbol? x)
                                       (number? x)
                                       (char? x)
                                       (and (vector? x)
                                            (zero? (vector-length x))))
                                   (let ((refs (R-entry.references entry)))
                                     (for-each (lambda (ref)
                                                 (variable-set! ref aval))
                                               refs)
                                     ; Do not try to use Rinfo in place of
                                     ; (lambda.R exp) below!
                                     (lambda.R-set!
                                       exp
                                       (remq entry (lambda.R exp)))
                                     (flag-as-ignored v exp)
                                     (if debugging?
                                         (begin (display msg1)
                                                (write v)
                                                (display ": ")
                                                (write aval)
                                                (newline)))))))))
                     Rinfo)
           (for-each (lambda (def)
                       (let* ((name (def.lhs def))
                              (rhs (def.rhs def))
                              (entry (R-lookup Rinfo name))
                              (calls (R-entry.calls entry)))
                         (if (null? calls)
                             (begin (lambda.defs-set!
                                      exp
                                      (remq def (lambda.defs exp)))
                                    ; Do not try to use Rinfo in place of
                                    ; (lambda.R exp) below!
                                    (lambda.R-set!
                                      exp
                                      (remq entry (lambda.R exp))))
                             (let* ((formals0 (append (lambda.args rhs) '()))
                                    (L (fold! rhs))
                                    (formals1 (lambda.args L)))
                               (if (not (equal? formals0 formals1))
                                   (delete-ignored-args! L formals0 calls))))))
                     (lambda.defs exp))
           (lambda.body-set!
             exp
             (fold! (lambda.body exp)))
           exp))
 
        ((assignment? exp)
         (assignment.rhs-set! exp (fold! (assignment.rhs exp)))
         exp)
 
        ((variable? exp) exp)
 
        ((begin? exp)
         (post-simplify-begin (make-begin (map fold! (begin.exprs exp)))
                              (make-notepad #f)))
 
        ((conditional? exp)
         (let ((exp0 (fold! (if.test exp)))
               (exp1 (fold! (if.then exp)))
               (exp2 (fold! (if.else exp))))
           (if (constant? exp0)
               (let ((newexp (if (constant.value exp0)
                                 exp1
                                 exp2)))
                 (if debugging?
                     (begin (display msg2)
                            (write (make-readable exp))
                            (display msg3)
                            (write (make-readable newexp))
                            (newline)))
                 (set! changed? #t)
                 newexp)
               (make-conditional exp0 exp1 exp2))))
 
        ((call? exp)
         (let ((args (map fold! (call.args exp)))
               (proc (fold! (call.proc exp))))
           (cond ((and folding?
                       (variable? proc)
                       (every? constant? args)
                       (let ((entry
                              (constant-folding-entry (variable.name proc))))
                         (and entry
                              (let ((preds
                                     (constant-folding-predicates entry)))
                                (and (= (length args) (length preds))
                                     (every?
                                      (lambda (x) x)
                                      (map (lambda (f v) (f v))
                                           (constant-folding-predicates entry)
                                           (map constant.value args))))))))
                  (set! changed? #t)
                  (let ((result
                         (make-constant
                          (apply (constant-folding-folder
                                  (constant-folding-entry
                                   (variable.name proc)))
                                 (map constant.value args)))))
                    (if debugging?
                        (begin (display msg2)
                               (write (make-readable (make-call proc args)))
                               (display msg3)
                               (write result)
                               (newline)))
                    result))
                 ((and (lambda? proc)
                       (list? (lambda.args proc)))
                  ; FIXME: Folding should be done even if there is
                  ; a rest argument.
                  (let loop ((formals (reverse (lambda.args proc)))
                             (actuals (reverse args))
                             (processed-formals '())
                             (processed-actuals '())
                             (for-effect '()))
                    (cond ((null? formals)
                           (lambda.args-set! proc processed-formals)
                           (call.args-set! exp processed-actuals)
                           (let ((call (if (and (null? processed-formals)
                                                (null? (lambda.defs proc)))
                                           (lambda.body proc)
                                           exp)))
                             (if (null? for-effect)
                                 call
                                 (post-simplify-begin
                                  (make-begin
                                   (reverse (cons call for-effect)))
                                  (make-notepad #f)))))
                          ((ignored? (car formals))
                           (loop (cdr formals)
                                 (cdr actuals)
                                 processed-formals
                                 processed-actuals
                                 (cons (car actuals) for-effect)))
                          (else
                           (loop (cdr formals)
                                 (cdr actuals)
                                 (cons (car formals) processed-formals)
                                 (cons (car actuals) processed-actuals)
                                 for-effect)))))
                 (else
                  (call.proc-set! exp proc)
                  (call.args-set! exp args)
                  exp))))
         (else (twobit-bug "unrecognized expression" exp))))

    (fold! L)
    changed?))

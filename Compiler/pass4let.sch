; Copyright 1991 William Clinger (cg-let and cg-let-body)
; Copyright 1999 William Clinger (everything else)
;
; 10 June 1999.

; Generates code for a let expression.

(define (cg-let output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (vars (lambda.args proc))
         (n (length vars))
         (free (lambda.F proc))
         (live (cgframe-livevars frame)))
    (if (and (null? (lambda.defs proc))
             (= n 1))
        (cg-let1 output exp target regs frame env tail?)
        (let* ((args (call.args exp))
               (temps (newtemps n))
               (alist (map cons temps vars)))
          (for-each (lambda (arg t)
                      (let ((r (choose-register regs frame)))
                        (cg0 output arg r regs frame env #f)
                        (cgreg-bind! regs r t)
                        (gen-store! output frame r t)))
                    args
                    temps)
          (cgreg-rename! regs alist)
          (cgframe-rename! frame alist)
          (cg-let-release! free live regs frame tail?)
          (cg-let-body output proc target regs frame env tail?)))))

; Given the free variables of a let body, and the variables that are
; live after the let expression, and the usual regs, frame, and tail?
; arguments, releases any registers and frame slots that don't need
; to be preserved across the body of the let.

(define (cg-let-release! free live regs frame tail?)
  ; The tail case is easy because there are no live temporaries,
  ; and there are no free variables in the context.
  ; The non-tail case assumes A-normal form.
  (cond (tail?
         (let ((keepers (cons (cgreg-lookup-reg regs 0) free)))
           (cgreg-release-except! regs keepers)
           (cgframe-release-except! frame keepers)))
        (live
         (let ((keepers (cons (cgreg-lookup-reg regs 0)
                              (union live free))))
           (cgreg-release-except! regs keepers)
           (cgframe-release-except! frame keepers)))))

; Generates code for the body of a let.

(define (cg-let-body output L target regs frame env tail?)
  (let ((vars (lambda.args L))
        (free (lambda.F L))
        (live (cgframe-livevars frame)))
    (let ((r (cg-body output L target regs frame env tail?)))
      (for-each (lambda (v)
                  (let ((entry (cgreg-lookup regs v)))
                    (if entry
                        (cgreg-release! regs (entry.regnum entry)))
                    (cgframe-release! frame v)))
                vars)
      (if (and (not target)
               (not (eq? r 'result))
               (not (cgreg-lookup-reg regs r)))
          (cg-move output frame regs r 'result)
          r))))

; Generates code for a let expression that binds exactly one variable
; and has no internal definitions.  These let expressions are very
; common in A-normal form, and there are many special cases with
; respect to register allocation and order of evaluation.

(define (cg-let1 output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (v (car (lambda.args proc)))
         (arg (car (call.args exp)))
         (free (lambda.F proc))
         (live (cgframe-livevars frame))
         (body (lambda.body proc)))
    
    (define (evaluate-into-register r)
      (cg0 output arg r regs frame env #f)
      (cgreg-bind! regs r v)
      (gen-store! output frame r v)
      r)
    
    (define (release-registers!)
      (cgframe-livevars-set! frame live)
      (cg-let-release! free live regs frame tail?))
    
    (define (finish)
      (release-registers!)
      (cg-let-body output proc target regs frame env tail?))
    
    (if live
        (cgframe-livevars-set! frame (union live free)))
    
    (cond ((assq v *regnames*)
           (evaluate-into-register (cdr (assq v *regnames*)))
           (finish))
          ((not (memq v free))
           (cg0 output arg #f regs frame env #f)
           (finish))
          (live
           (cg0 output arg 'result regs frame env #f)
           (release-registers!)
           (cg-let1-result output exp target regs frame env tail?))
          (else
           (evaluate-into-register (choose-register regs frame))
           (finish)))))

; Given a let expression that binds one variable whose value has already
; been evaluated into the result register, generates code for the rest
; of the let expression.
; The main difficulty is an unfortunate interaction between A-normal
; form and the MacScheme machine architecture:  We don't want to move
; a value from the result register into a general register if it has
; only one use and can remain in the result register until that use.

(define (cg-let1-result output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (v (car (lambda.args proc)))
         (free (lambda.F proc))
         (live (cgframe-livevars frame))
         (body (lambda.body proc))
         (pattern (cg-let-used-once v body)))
    
    (define (move-to-register r)
      (gen! output $setreg r)
      (cgreg-bind! regs r v)
      (gen-store! output frame r v)
      r)
    
    (define (release-registers!)
      (cgframe-livevars-set! frame live)
      (cg-let-release! free live regs frame tail?))
    
    ; FIXME: The live variables must be correct in the frame.
    
    (case pattern
      ((if)
       (cg-if-result output body target regs frame env tail?))
      ((let-if)
       (if live
           (cgframe-livevars-set! frame (union live free)))
       (cg-if-result output
                     (car (call.args body))
                     'result regs frame env #f)
       (release-registers!)
       (cg-let1-result output body target regs frame env tail?))
      ((set!)
       (cg-assignment-result output
                             body target regs frame env tail?))
      ((let-set!)
       (cg-assignment-result output
                             (car (call.args body))
                             'result regs frame env #f)
       (cg-let1-result output body target regs frame env tail?))
      ((primop)
       (cg-primop-result output body target regs frame env tail?))
      ((let-primop)
       (cg-primop-result output
                         (car (call.args body))
                         'result regs frame env #f)
       (cg-let1-result output body target regs frame env tail?))
      ; FIXME
      ((_called)
       (cg-call-result output body target regs frame env tail?))
      ; FIXME
      ((_let-called)
       (cg-call-result output
                       (car (call.args body))
                       'result regs frame env #f)
       (cg-let1-result output body target regs frame env tail?))
      (else
       ; FIXME:  The first case was handled by cg-let1.
       (cond ((assq v *regnames*)
              (move-to-register (cdr (assq v *regnames*))))
             ((memq v free)
              (move-to-register (choose-register regs frame))))
       (cg-let-body output proc target regs frame env tail?)))))

; Given a call to a primop whose first argument has already been
; evaluated into the result register and whose remaining arguments
; consist of constants and variable references, generates code for
; the call.

(define (cg-primop-result output exp target regs frame env tail?)
  (let ((args (call.args exp))
        (entry (var-lookup (variable.name (call.proc exp)) regs frame env)))
    (if (= (entry.arity entry) (length args))
        (begin (case (entry.arity entry)
                 ((0) (gen! output $op1 (entry.op entry)))
                 ((1) (gen! output $op1 (entry.op entry)))
                 ((2) (cg-primop2-result! output entry args regs frame env))
                 ((3) (let ((rs (cg-result-args output args regs frame env)))
                        (gen! output
                              $op3 (entry.op entry) (car rs) (cadr rs))))
                 (else (error "Bug detected by cg-primop-result"
                              (make-readable exp))))
               (if tail?
                   (begin (gen-pop! output frame)
                          (gen! output $return)
                          'result)
                   (cg-move output frame regs 'result target)))
        (if (negative? (entry.arity entry))
            (cg-special-result output exp target regs frame env tail?)
            (error "Wrong number of arguments to integrable procedure"
                   (make-readable exp))))))

(define (cg-primop2-result! output entry args regs frame env)
  (let ((op (entry.op entry))
        (arg2 (cadr args)))
    (if (and (constant? arg2)
             (entry.imm entry)
             ((entry.imm entry) (constant.value arg2)))
        (gen! output $op2imm op (constant.value arg2))
        (let ((rs (cg-result-args output args regs frame env)))
          (gen! output $op2 op (car rs))))))

; Given a short list of constants and variable references to be evaluated
; into arbitrary general registers, evaluates them into registers without
; disturbing the result register and returns a list of the registers into
; which they are evaluated.  Before returning, any registers that were
; allocated by this routine are released.

(define (cg-result-args output args regs frame env)
  
  ; Given a list of unevaluated arguments,
  ; a longer list of disjoint general registers,
  ; the register that holds the first evaluated argument,
  ; a list of registers in reverse order that hold other arguments,
  ; and a list of registers to be released afterwards,
  ; generates code to evaluate the arguments,
  ; deallocates any registers that were evaluated to hold the arguments,
  ; and returns the list of registers that contain the arguments.
  
  (define (loop args registers rr rs temps)
    (if (null? args)
        (begin (if (not (eq? rr 'result))
                   (gen! output $reg rr))
               (for-each (lambda (r) (cgreg-release! regs r))
                         temps)
               (reverse rs))
        (let ((arg (car args)))
          (cond ((constant? arg)
                 (let ((r (car registers)))
                   (gen! output $const/setreg (constant.value arg) r)
                   (cgreg-bind! regs r #t)
                   (loop (cdr args)
                         (cdr registers)
                         rr
                         (cons r rs)
                         (cons r temps))))
                ((variable? arg)
                 (let* ((id (variable.name arg))
                        (entry (var-lookup id regs frame env)))
                   (case (entry.kind entry)
                     ((global integrable)
                      (if (eq? rr 'result)
                          (save-result! args registers rr rs temps)
                          (let ((r (car registers)))
                            (gen! output $global id)
                            (gen! output $setreg r)
                            (cgreg-bind! regs r id)
                            (loop (cdr args)
                                  (cdr registers)
                                  rr
                                  (cons r rs)
                                  (cons r temps)))))
                     ((lexical)
                      (if (eq? rr 'result)
                          (save-result! args registers rr rs temps)
                          (let ((m (entry.rib entry))
                                (n (entry.offset entry))
                                (r (car registers)))
                            (gen! output $lexical m n id)
                            (gen! output $setreg r)
                            (cgreg-bind! regs r id)
                            (loop (cdr args)
                                  (cdr registers)
                                  rr
                                  (cons r rs)
                                  (cons r temps)))))
                     ((procedure) (error "Bug in cg-variable" arg))
                     ((register)
                      (let ((r (entry.regnum entry)))
                        (loop (cdr args)
                              registers
                              rr
                              (cons r rs)
                              temps)))
                     ((frame)
                      (let ((r (car registers)))
                        (gen-load! output frame r id)
                        (cgreg-bind! regs r id)
                        (loop (cdr args)
                              (cdr registers)
                              rr
                              (cons r rs)
                              (cons r temps))))
                     (else (error "Bug in cg-result-args" arg)))))
                (else
                 (error "Bug in cg-result-args"))))))
  
  (define (save-result! args registers rr rs temps)
    (let ((r (car registers)))
      (gen! output $setreg r)
      (loop args
            (cdr registers)
            r
            rs
            temps)))
  
  (loop (cdr args)
        (choose-registers regs frame (length args))
        'result '() '()))

; Given a local variable T1 and an expression in A-normal form,
; cg-let-used-once returns a symbol if the local variable is used
; exactly once in the expression and the expression matches one of
; the patterns below.  Otherwise returns #f.  The symbol that is
; returned is the name of the pattern that is matched.
;
;     pattern                         symbol returned
; 
;     (if T1 ... ...)                 if
; 
;     (<primop> T1 ...)               primop
; 
;     (T1 ...)                        called
; 
;     (set! ... T1)                   set!
; 
;     (let ((T2 (if T1 ... ...)))     let-if
;       E3)
; 
;     (let ((T2 (<primop> T1 ...)))   let-primop
;       E3)
; 
;     (let ((T2 (T1 ...)))            let-called
;       E3)
; 
;     (let ((T2 (set! ... T1)))       let-set!
;       E3)
;
; This implementation sometimes returns #f incorrectly, but it always
; returns an answer in constant time (assuming A-normal form).

(define (cg-let-used-once T1 exp)
  (define budget 20)
  (define (cg-let-used-once T1 exp)
    (define (used? T1 exp)
      (set! budget (- budget 1))
      (cond ((negative? budget) #t)
            ((constant? exp) #f)
            ((variable? exp)
             (eq? T1 (variable.name exp)))
            ((lambda? exp)
             (memq T1 (lambda.F exp)))
            ((assignment? exp)
             (used? T1 (assignment.rhs exp)))
            ((call? exp)
             (or (used? T1 (call.proc exp))
                 (used-in-args? T1 (call.args exp))))
            ((conditional? exp)
             (or (used? T1 (if.test exp))
                 (used? T1 (if.then exp))
                 (used? T1 (if.else exp))))
            (else #t)))
    (define (used-in-args? T1 args)
      (if (null? args)
          #f
          (or (used? T1 (car args))
              (used-in-args? T1 (cdr args)))))
    (set! budget (- budget 1))
    (cond ((negative? budget) #f)
          ((call? exp)
           (let ((proc (call.proc exp))
                 (args (call.args exp)))
             (cond ((variable? proc)
                    (let ((f (variable.name proc)))
                      (cond ((eq? f T1)
                             (and (not (used-in-args? T1 args))
                                  'called))
                            ((and (integrable? f)
                                  (not (null? args))
                                  (variable? (car args))
                                  (eq? T1 (variable.name (car args))))
                             (and (not (used-in-args? T1 (cdr args)))
                                  'primop))
                            (else #f))))
                   ((lambda? proc)
                    (and (not (memq T1 (lambda.F proc)))
                         (not (null? args))
                         (null? (cdr args))
                         (case (cg-let-used-once T1 (car args))
                           ((if)       'let-if)
                           ((primop)   'let-primop)
                           ((called)   'let-called)
                           ((set!)     'let-set!)
                           (else       #f))))
                   (else #f))))
          ((conditional? exp)
           (let ((E0 (if.test exp)))
             (and (variable? E0)
                  (eq? T1 (variable.name E0))
                  (not (used? T1 (if.then exp)))
                  (not (used? T1 (if.else exp)))
                  'if)))
          ((assignment? exp)
           (let ((rhs (assignment.rhs exp)))
             (and (variable? rhs)
                  (eq? T1 (variable.name rhs))
                  'set!)))
          (else #f)))
  (cg-let-used-once T1 exp))

; Given the name of a let-body pattern, an expression that matches that
; pattern, and an expression to be substituted for the let variable,
; returns the transformed expression.

; FIXME: No longer used.

(define (cg-let-transform pattern exp E1)
  (case pattern
    ((if)
     (make-conditional E1 (if.then exp) (if.else exp)))
    ((primop)
     (make-call (call.proc exp)
                (cons E1 (cdr (call.args exp)))))
    ((called)
     (make-call E1 (call.args exp)))
    ((set!)
     (make-assignment (assignment.lhs exp) E1))
    ((let-if let-primop let-called let-set!)
     (make-call (call.proc exp)
                (list (cg-let-transform (case pattern
                                          ((let-if)     'if)
                                          ((let-primop) 'primop)
                                          ((let-called) 'called)
                                          ((let-set!)   'set!))
                                        (car (call.args exp))
                                        E1))))
    (else
     (error "Unrecognized pattern in cg-let-transform" pattern))))
; Copyright 1991 William Clinger
;
; 13 November 1998
;
; Fourth pass of the Twobit compiler:
;   code generation for the MacScheme machine.
;
; This pass operates on input expressions described by the
; following grammar and the invariants that follow it.
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
; Invariants that hold for the input
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  Every procedure defined by an internal definition takes a
;      fixed number of arguments.
;   *  Every call to a procedure defined by an internal definition
;      passes the correct number of arguments.
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
; 
; Stack frames are created by "save" instructions.
; A save instruction is generated
; 
;     *  at the beginning of each lambda body
;     *  at the beginning of the code for each arm of a conditional,
;        provided:
;          the conditional is in a tail position
;          the frames that were allocated by the save instructions
;            that dominate the arms of the conditional have not been
;            used (those save instructions will be eliminated during
;            assembly)
;
; The operand of a save instruction, and of its matching pop instructions,
; increases automatically as frame slots are allocated.
; 
; The code generated to return from a procedure is
; 
;         pop     n
;         return
; 
; The code generated for a tail call is
; 
;         pop     n
;         invoke  ...
;
; Invariant:  When the code generator reserves an argument register
; to hold a value, that value is named, and is stored into the current
; stack frame.  These store instructions are eliminated during assembly
; unless there is a matching load instruction.  If all of the instructions
; that store into a stack frame are eliminated, then the stack frame
; itself is eliminated.
; Exception:  An argument register may be used without naming or storing
; its value provided the register is not in use and no expressions are
; evaluated while it contains the unnamed and unstored value.


(define (pass4 exp integrable)
  (init-labels)
  (init-temps)
  (let ((output (make-assembly-stream))
        (frame (cgframe-initial))
        (regs (cgreg-initial))
        (t0 (newtemp)))
    (cgreg-bind! regs 0 t0)
    (gen-save! output frame t0)
    (cg0 output
         exp
         'result
         regs
         frame
         (cgenv-initial integrable)
         #t)
    (assembly-stream-code output)))

; Given:
;    an assembly stream into which instructions should be emitted
;    an expression
;    the target register
;      ('result, a register number, or '#f; tail position implies 'result)
;    a register environment [cgreg]
;    a stack-frame environment [cgframe]
;    a compile-time environment [cgenv]
;    a flag indicating whether the expression is in tail position
; Returns:
;    the target register ('result or a register number)
; Side effects:
;    may change the register and stack-frame environments
;    may increase the size of the stack frame, which changes previously
;       emitted instructions
;    writes instructions to the assembly stream

(define (cg0 output exp target regs frame env tail?)
  (case (car exp)
    ((quote)    (gen! output $const (constant.value exp))
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output frame regs 'result target)))
    ((lambda)   (cg-lambda output exp regs frame env)
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output frame regs 'result target)))
    ((set!)     (cg0 output (assignment.rhs exp) 'result regs frame env #f)
                (gen! output $setglbl (assignment.lhs exp))
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output frame regs 'result target)))
    ((if)       (cg-if output exp target regs frame env tail?))
    ((begin)    (if (variable? exp)
                    (cg-variable output exp target regs frame env tail?)
                    (cg-sequential output exp target regs frame env tail?)))
    (else       (cg-call output exp target regs frame env tail?))))

; Lambda expressions that evaluate to closures.
; This is hard because the MacScheme machine's lambda instruction
; closes over the values that are in argument registers 0 through r
; (where r can be larger than *nregs*).
; The set of free variables is calculated and then sorted to minimize
; register shuffling.
;
; Returns: nothing.

(define (cg-lambda output exp regs frame env)
  (let* ((args (lambda.args exp))
         (vars (make-null-terminated args))
         (free (difference (lambda.F exp) vars))
         (free (cg-sort-vars free regs frame env))
         (newenv (cgenv-extend env (cons #t free) '()))
         (newoutput (make-assembly-stream)))
    (gen! newoutput $.proc)
    (if (list? args)
        (gen! newoutput $args= (length args))
        (gen! newoutput $args>= (- (length vars) 1)))
    (cg-known-lambda newoutput exp newenv)
    (cg-eval-vars output free regs frame env)
    ; FIXME
    '
    (if (not (ignore-space-leaks))
        ; FIXME: Is this the right constant?
        (begin (gen! output $const #f)
               (gen! output $setreg 0)))
    (gen! output
          $lambda
          (assembly-stream-code newoutput)
          (length free)
          (lambda.doc exp))
    ; FIXME
    '
    (if (not (ignore-space-leaks))
        ; FIXME: This load forces a stack frame to be allocated.
        (gen-load! output frame 0 (cgreg-lookup-reg regs 0)))))

; Given a list of free variables, filters out the ones that
; need to be copied into a closure, and sorts them into an order
; that reduces register shuffling.  Returns a sorted version of
; the list in which the first element (element 0) should go
; into register 1, the second into register 2, and so on.

(define (cg-sort-vars free regs frame env)
  (let* ((free (filter (lambda (var)
                         (case (entry.kind
                                (var-lookup var regs frame env))
                           ((register frame)
                            #t)
                           ((lexical)
                            (not (ignore-space-leaks)))
                           (else #f)))
                       free))
         (n (length free))
         (m (min n (- *nregs* 1)))
         (vec (make-vector m #f)))
    (define (loop1 free free-notregister)
      (if (null? free)
          (loop2 0 free-notregister)
          (let* ((var (car free))
                 (entry (cgreg-lookup regs var)))
            (if entry
                (let ((r (entry.regnum entry)))
                  (if (<= r n)
                      (begin (vector-set! vec (- r 1) var)
                             (loop1 (cdr free)
                                    free-notregister))
                      (loop1 (cdr free)
                             (cons var free-notregister))))
                (loop1 (cdr free)
                       (cons var free-notregister))))))
    (define (loop2 i free)
      (cond ((null? free)
             (vector->list vec))
            ((= i m)
             (append (vector->list vec) free))
            ((vector-ref vec i)
             (loop2 (+ i 1) free))
            (else
             (vector-set! vec i (car free))
             (loop2 (+ i 1) (cdr free)))))
    (loop1 free '())))

; Fetches the given list of free variables into the corresponding
; registers in preparation for a $lambda or $lexes instruction.

(define (cg-eval-vars output free regs frame env)
  (let ((n (length free))
        (R-1 (- *nregs* 1)))
    (if (>= n R-1)
        (begin (gen! output $const '())
               (gen! output $setreg R-1)
               (cgreg-release! regs R-1)))
    (do ((r n (- r 1))
         (vars (reverse free) (cdr vars)))
        ((zero? r))
        (let* ((v (car vars))
               (entry (var-lookup v regs frame env)))
          (case (entry.kind entry)
            ((register)
             (let ((r1 (entry.regnum entry)))
               (if (not (eqv? r r1))
                   (if (< r R-1)
                       (begin (gen! output $movereg r1 r)
                              (cgreg-bind! regs r v))
                       (gen! output $reg r1 v)))))
            ((frame)
             (if (< r R-1)
                 (begin (gen-load! output frame r v)
                        (cgreg-bind! regs r v))
                 (gen-stack! output frame v)))
            ((lexical)
             (gen! output $lexical
                          (entry.rib entry)
                          (entry.offset entry)
                          v)
             (if (< r R-1)
                 (begin (gen! output $setreg r)
                        (cgreg-bind! regs r v)
                        (gen-store! output frame r v))))
            (else
             (error "Bug in cg-close-lambda")))
          (if (>= r R-1)
              (begin (gen! output $op2 $cons R-1)
                     (gen! output $setreg R-1)))))))

; Lambda expressions that appear on the rhs of a definition are
; compiled here.  They don't need an args= instruction at their head.
;
; Returns: nothing.

(define (cg-known-lambda output exp env)
  (let* ((vars (make-null-terminated (lambda.args exp)))
         (regs (cgreg-initial))
         (frame (cgframe-initial))
         (t0 (newtemp)))
    (cgreg-bind! regs 0 t0)
    (gen-save! output frame t0)
    (do ((r 1 (+ r 1))
         (vars vars (cdr vars)))
        ((or (null? vars)
             (= r *lastreg*))
         (if (not (null? vars))
             (begin (gen! output $movereg *lastreg* 1)
                    (cgreg-release! regs 1)
                    (do ((vars vars (cdr vars)))
                        ((null? vars))
                        (gen! output $reg 1)
                        (gen! output $op1 $car:pair)
                        (gen-setstk! output frame (car vars))
                        (gen! output $reg 1)
                        (gen! output $op1 $cdr:pair)
                        (gen! output $setreg 1)))))
        (cgreg-bind! regs r (car vars))
        (gen-store! output frame r (car vars)))
    (cg-body output
             exp
             'result
             regs
             frame
             env
             #t)))

; Compiles a let or lambda body.
; The arguments of the lambda expression L are already in
; registers or the stack frame, as specified by regs and frame.
;
; The problem here is that the free variables of an internal
; definition must be in a heap-allocated environment, so any
; such variables in registers must be copied to the heap.
;
; Returns: destination register.

(define (cg-body output L target regs frame env tail?)
  (let* ((exp (lambda.body L))
         (defs (lambda.defs L))
         (free (apply union
                      (map (lambda (def)
                             (let ((L (def.rhs def)))
                               (difference (lambda.F L)
                                           (lambda.args L))))
                           defs))))
    (cond ((or (null? defs) (constant? exp) (variable? exp))
           (cg0 output exp target regs frame env tail?))
          ((lambda? exp)
           (let* ((free (cg-sort-vars
                         (union free
                                (difference
                                 (lambda.F exp)
                                 (make-null-terminated (lambda.args exp))))
                         regs frame env))
                  (newenv1 (cgenv-extend env
                                         (cons #t free)
                                         (map def.lhs defs)))
                  (args (lambda.args exp))
                  (vars (make-null-terminated args))
                  (newoutput (make-assembly-stream)))
             (gen! newoutput $.proc)
             (if (list? args)
                 (gen! newoutput $args= (length args))
                 (gen! newoutput $args>= (- (length vars) 1)))
             (cg-known-lambda newoutput exp newenv1)
             (cg-defs newoutput defs newenv1)
             (cg-eval-vars output free regs frame env)
             (gen! output
                   $lambda
                   (assembly-stream-code newoutput)
                   (length free)
                   (lambda.doc exp))
             (if tail?
                 (begin (gen-pop! output frame)
                        (gen! output $return)
                        'result)
                 (cg-move output frame regs 'result target))))
          ((every? (lambda (def)
                     (every? (lambda (v)
                               (case (entry.kind
                                      (var-lookup v regs frame env))
                                 ((register frame) #f)
                                 (else #t)))
                             (let ((Ldef (def.rhs def)))
                               (difference (lambda.F Ldef)
                                           (lambda.args Ldef)))))
                   defs)
           (let* ((newenv (cgenv-bindprocs env (map def.lhs defs)))
                  (L (make-label))
                  (r (cg0 output exp target regs frame newenv tail?)))
             (if (not tail?)
                 (gen! output $skip L (cgreg-live regs r)))
             (cg-defs output defs newenv)
             (if (not tail?)
                 (gen! output $.label L))))
          (else
           (let ((free (cg-sort-vars free regs frame env)))
             (cg-eval-vars output free regs frame env)
             ; FIXME: Have to restore it too!
             '
             (if (not (ignore-space-leaks))
                 ; FIXME: Is this constant the right one?
                 (begin (gen! output $const #f)
                        (gen! output $setreg 0)))
             (let ((t0 (cgreg-lookup-reg regs 0))
                   (t1 (newtemp))
                   (newenv (cgenv-extend env
                                         (cons #t free)
                                         (map def.lhs defs)))
                   (L (make-label)))
               (gen! output $lexes (length free) free)
               (gen! output $setreg 0)
               (cgreg-bind! regs 0 t1)
               (if tail?
                   (begin (cgframe-release! frame t0)
                          (gen-store! output frame 0 t1)
                          (cg0 output exp 'result regs frame newenv #t)
                          (cg-defs output defs newenv)
                          'result)
                   (begin (gen-store! output frame 0 t1)
                          (cg0 output exp 'result regs frame newenv #f)
                          (gen! output $skip L (cgreg-tos regs))
                          (cg-defs output defs newenv)
                          (gen! output $.label L)
                          (gen-load! output frame 0 t0)
                          (cgreg-bind! regs 0 t0)
                          (cgframe-release! frame t1)
                          (cg-move output frame regs 'result target)))))))))

(define (cg-defs output defs env)
  (for-each (lambda (def)
              (gen! output $.align 4)
              (gen! output $.label
                           (entry.label
                            (cgenv-lookup env (def.lhs def))))
              (gen! output $.proc)
              (gen! output $.proc-doc (lambda.doc (def.rhs def)))
              (cg-known-lambda output
                               (def.rhs def)
                               env))
            defs))

(define (cg-if output exp target regs frame env tail?)
  (let ((L1 (make-label))
        (L2 (make-label)))
    (cg0 output (if.test exp) 'result regs frame env #f)
    (gen! output $branchf L1 (cgreg-tos regs))
    (let* ((regs2 (cgreg-copy regs))
           (frame1 (if (and tail?
                            (negative? (cgframe-size frame)))
                       (cgframe-initial)
                       frame))
           (frame2 (if (eq? frame frame1)
                       (cgframe-copy frame1)
                       (cgframe-initial)))
           (t0 (cgreg-lookup-reg regs 0)))
      (if (not (eq? frame frame1))
          (begin (gen-save! output frame1 t0)
                 (cg-saveregs output regs frame1)))
      (let ((r (cg0 output (if.then exp) target regs frame1 env tail?)))
        (if (not tail?)
            (gen! output $skip L2 (cgreg-live regs r)))
        (gen! output $.label L1)
        (if (not (eq? frame frame1))
            (begin (gen-save! output frame2 t0)
                   (cg-saveregs output regs2 frame2))
            (cgframe-update-stale! frame2))
        (cg0 output (if.else exp) r regs2 frame2 env tail?)
        (if (not tail?)
            (begin (gen! output $.label L2)
                   (cgreg-join! regs regs2)
                   (cgframe-join! frame1 frame2)))
        r))))

(define (cg-variable output exp target regs frame env tail?)
  (define (return id)
    (if tail?
        (begin (gen-pop! output frame)
               (gen! output $return)
               'result)
        (if (and target
                 (not (eq? 'result target)))
            (begin (gen! output $setreg target)
                   (cgreg-bind! regs target id)
                   (gen-store! output frame target id)
                   target)
            'result)))
  ; Same as return, but doesn't emit a store instruction.
  (define (return-nostore id)
    (if tail?
        (begin (gen-pop! output frame)
               (gen! output $return)
               'result)
        (if (and target
                 (not (eq? 'result target)))
            (begin (gen! output $setreg target)
                   (cgreg-bind! regs target id)
                   target)
            'result)))
  (let* ((id (variable.name exp))
         (entry (var-lookup id regs frame env)))
    (case (entry.kind entry)
      ((global integrable)
       (gen! output $global id)
       (return (newtemp)))
      ((lexical)
       (let ((m (entry.rib entry))
             (n (entry.offset entry)))
         (gen! output $lexical m n id)
         (if (or (zero? m)
                 (negative? (cgframe-size frame)))
             (return-nostore id)
             (return id))))
      ((procedure) (error "Bug in cg-variable" exp))
      ((register)
       (let ((r (entry.regnum entry)))
         (if (or tail?
                 (and target (not (eqv? target r))))
             (begin (gen! output $reg (entry.regnum entry) id)
                    (return-nostore id))
             r)))
      ((frame)
       (cond ((eq? target 'result)
              (gen-stack! output frame id)
              (return id))
             (target
              ; Must be non-tail.
              (gen-load! output frame target id)
              (cgreg-bind! regs target id)
              target)
             (else
              ; Must be non-tail.
              (let ((r (choose-register regs frame)))
                (gen-load! output frame r id)
                (cgreg-bind! regs r id)
                r))))
      (else (error "Bug in cg-variable" exp)))))

(define (cg-sequential output exp target regs frame env tail?)
  (cg-sequential-loop output (begin.exprs exp) target regs frame env tail?))

(define (cg-sequential-loop output exprs target regs frame env tail?)
  (cond ((null? exprs)
         (gen! output $const unspecified)
         (if tail?
             (begin (gen-pop! output frame)
                    (gen! output $return)
                    'result)
             (cg-move output frame regs 'result target)))
        ((null? (cdr exprs))
         (cg0 output (car exprs) target regs frame env tail?))
        (else (cg0 output (car exprs) #f regs frame env #f)
              (cg-sequential-loop output
                                  (cdr exprs)
                                  target regs frame env tail?))))

(define (cg-saveregs output regs frame)
  (do ((i 1 (+ i 1))
       (vars (cdr (cgreg-vars regs)) (cdr vars)))
      ((null? vars))
      (let ((t (car vars)))
        (if t
            (gen-store! output frame i t)))))

(define (cg-move output frame regs src dst)
  (define (bind dst)
    (let ((temp (newtemp)))
      (cgreg-bind! regs dst temp)
      (gen-store! output frame dst temp)
      dst))
  (cond ((not dst)
         src)
        ((eqv? src dst)
         dst)
        ((eq? dst 'result)
         (gen! output $reg src)
         dst)
        ((eq? src 'result)
         (gen! output $setreg dst)
         (bind dst))
        ((and (not (zero? src))
              (not (zero? dst)))
         (gen! output $movereg src dst)
         (bind dst))
        (else
         (gen! output $reg src)
         (gen! output $setreg dst)
         (bind dst))))

; On-the-fly register allocator.
; Tries to allocate:
;    a hardware register that isn't being used
;    a hardware register whose contents have already been spilled
;    a software register that isn't being used, unless a stack
;       frame has already been created, in which case it is better to use
;    a hardware register that is in use and hasn't yet been spilled
;
; All else equal, it is better to allocate a higher-numbered register
; because the lower-numbered registers are targets when arguments
; are being evaluated.
;
; Invariant:  Every register that is returned by this allocator
; is either not in use or has been spilled.

(define (choose-register regs frame)
  (car (choose-registers regs frame 1)))

(define (choose-registers regs frame n)
  
  ; Find unused hardware registers.
  (define (loop1 i n good)
    (cond ((zero? n)
           good)
          ((zero? i)
           (if (negative? (cgframe-size frame))
               (hardcase)
               (loop2 (- *nhwregs* 1) n good)))
          (else
           (if (cgreg-lookup-reg regs i)
               (loop1 (- i 1) n good)
               (loop1 (- i 1)
                      (- n 1)
                      (cons i good))))))
  
  ; Find already spilled hardware registers.
  (define (loop2 i n good)
    (cond ((zero? n)
           good)
          ((zero? i)
           (hardcase))
          (else
           (let ((t (cgreg-lookup-reg regs i)))
             (if (and t (cgframe-spilled? frame t))
                 (loop2 (- i 1)
                        (- n 1)
                        (cons i good))
                 (loop2 (- i 1) n good))))))
  
  ; This is ridiculous.
  ; Fortunately the correctness of the compiler is independent
  ; of the predicate used for this sort.
  
  (define (hardcase)
    (let* ((frame-exists? (not (negative? (cgframe-size frame))))
           (stufftosort
            (map (lambda (r)
                   (let* ((t (cgreg-lookup-reg regs r))
                          (spilled?
                           (and t
                                (cgframe-spilled? frame t))))
                     (list r t spilled?)))
                 (cdr (iota *nregs*))))
           (registers
            (twobit-sort
             (lambda (x1 x2)
               (let ((r1 (car x1))
                     (r2 (car x2))
                     (t1 (cadr x1))
                     (t2 (cadr x2)))
                 (cond ((< r1 *nhwregs*)
                        (cond ((not t1)                     #t)
                              ((< r2 *nhwregs*)
                               (cond ((not t2)              #f)
                                     ((caddr x1)            #t)
                                     ((caddr x2)            #f)
                                     (else                  #t)))
                              (frame-exists?                #t)
                              (t2                           #t)
                              (else                         #f)))
                       ((< r2 *nhwregs*)
                        (cond (frame-exists?                #f)
                              (t1                           #f)
                              (t2                           #t)
                              (else                         #f)))
                       (t1
                        (if (and (caddr x1)
                                 t2
                                 (not (caddr x2)))
                            #t
                            #f))
                       (else #t))))
             stufftosort)))
      ; FIXME: What was this for?
      '
      (for-each (lambda (register)
                  (let ((t (cadr register))
                        (spilled? (caddr register)))
                    (if (and t (not spilled?))
                        (cgframe-touch! frame t))))
                registers)
      (do ((sorted (map car registers) (cdr sorted))
           (rs '() (cons (car sorted) rs))
           (n n (- n 1)))
          ((zero? n)
           (reverse rs)))))
  
  (if (< n *nregs*)
      (loop1 (- *nhwregs* 1) n '())
      (error (string-append "Compiler bug: can't allocate "
                            (number->string n)
                            " registers on this target."))))

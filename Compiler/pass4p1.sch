; Copyright 1991 William Clinger
;
; 26 May 1995
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
; Stack frames.
; The assembler ignores instructions of the following forms:
; 
;         save    -1
;         restore -1
;         pop     -1
; 
; A "save -1" instruction is generated:
; 
;     *  at the beginning of each lambda body
;     *  at the beginning of the code for each arm of a conditional,
;        provided:
;          the conditional is in a tail position
;          the frame size is -1 after the test code has been generated
; 
; The operand of a save instruction, and the operands of its matching
; restore and pop instructions, are side effected when a non-tail call
; is compiled or when a frame temporary is allocated.
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


(define (pass4 exp integrable)
  (set! cg-label-counter 1000)
  (let ((output (make-assembly-stream))
        (frame (cgframe-initial)))
    (gen-save! output frame)
    (cg0 output
         exp
         'result
         (cgreg-initial)
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
;      contains size of frame, current top of frame
;    a compile-time environment [cgenv]
;    a flag indicating whether the expression is in tail position
; Returns:
;    the target register ('result or a register number)
; Side effects:
;    may increase the size of the stack frame
;    writes instructions to the assembly stream

(define (cg0 output exp target regs frame env tail?)
  (case (car exp)
    ((quote)    (gen! output $const (constant.value exp))
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output 'result target)))
    ((lambda)   (cg-lambda output exp regs frame env)
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output 'result target)))
    ((set!)     (cg0 output (assignment.rhs exp) 'result regs frame env #f)
                (gen! output $setglbl (assignment.lhs exp))
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output 'result target)))
    ((if)       (cg-if output exp target regs frame env tail?))
    ((begin)    (if (variable? exp)
                    (cg-variable output exp target regs frame env tail?)
                    (cg-sequential output exp target regs frame env tail?)))
    (else       (cg-call output exp target regs frame env tail?))))

; For the moment a lambda expression will close over all
; live registers, regardless of whether the registers contain
; variables or temporaries and regardless of whether the
; variables occur free in the lambda expression.
;
; Returns: nothing.

(define (cg-lambda output exp regs frame env)
  (let* ((args (lambda.args exp))
         (vars (make-null-terminated args))
         (regvars (cgreg-vars regs))
         (newenv (cgenv-extend env regvars '()))
         (newoutput (make-assembly-stream)))
    (gen! newoutput $.proc)
    (if (list? args)
        (gen! newoutput $args= (length args))
        (gen! newoutput $args>= (- (length vars) 1)))
    (cg-known-lambda newoutput exp newenv)
    (gen! output
          $lambda
          (assembly-stream-code newoutput)
          (cgreg-tos regs) 
	  ;regvars
	  (lambda.doc exp) ; @@ Lars
	  )))

; Lambda expressions that appear on the rhs of a definition are
; compiled here.  They don't need an args= instruction at their head.
;
; Returns: nothing.

(define (cg-known-lambda output exp env)
  (let* ((vars (make-null-terminated (lambda.args exp)))
         (defs (lambda.defs exp))
         (body (lambda.body exp))
         (frame (cgframe-initial)))
    (if (> (length vars) *fullregs*)
        (begin (gen! output $lexes (length vars) (cons #t vars))
               (gen! output $setreg 0)
               (gen-save! output frame)
               (cg-body output
                        body
                        defs
                        'result
                        (cgreg-initial)
                        frame
                        (cgenv-extend env
                                      (cons '#t vars)
                                      '())
                        #t))
        (begin (gen-save! output frame)
               (cg-body output
                        body
                        defs
                        'result
                        (cgreg-bindregs (cgreg-initial) vars)
                        frame
                        env
                        #t)))))

; Compiles a let or lambda body.
; The problem here is that the free variables of an internal
; definition must be in a heap-allocated environment, so any
; such variables in registers must be copied to the heap.
;
; Returns: destination register.

(define (cg-body output exp defs target regs frame env tail?)
  (cond ((or (null? defs) (constant? exp) (variable? exp))
         (cg0 output exp target regs frame env tail?))
        ((lambda? exp)
         (let* ((newenv1 (cgenv-extend env
                                       (cgreg-vars regs)
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
           (gen! output
                 $lambda
                 (assembly-stream-code newoutput)
                 (cgreg-tos regs)
                 ;(cgreg-vars regs)
		 (lambda.doc exp) ; @@ Lars
		 )
           (if tail?
               (begin (gen-pop! output frame)
                      (gen! output $return)
                      'result)
               (cg-move output 'result target))))
        ((zero? (cgreg-tos regs))
         (let ((newenv (cgenv-bindprocs env (map def.lhs defs)))
               (L (make-label)))
           (if tail?
               (begin
                (cg0 output exp 'result regs frame newenv #t)
                (cg-defs output defs newenv)
                'result)
               (begin
                (let ((r (cg0 output exp target regs frame newenv #f)))
                  (gen! output $skip L (cgreg-live regs r))
                  (cg-defs output defs newenv)
                  (gen! output $.label L)
                  (cg-move output r target))))))
        (else
         (let* ((k (cgreg-tos regs))
                (newenv (cgenv-extend env
                                      (cgreg-vars regs)
                                      (map def.lhs defs)))
                (L (make-label)))
           (if tail?
               (begin (gen! output $lexes k (cgreg-vars regs))
                      (gen! output $setreg 0)
                      (cg0 output exp 'result regs frame newenv #t)
                      (cg-defs output defs newenv)
                      'result)
               (call-with-values
                (lambda () (cgframe-newtemp frame))
                (lambda (t1 frame)
                  (gen! output $store 0 t1)
                  (gen! output $lexes k (cgreg-vars regs))
                  (gen! output $setreg 0)
                  (cg0 output exp 'result regs frame newenv #f)
                  (gen! output $skip L (cgreg-tos regs))
                  (cg-defs output defs newenv2)
                  (gen! output $.label L)
                  (gen! output $load 0 t1)
                  (cg-move output 'result target))))))))

(define (cg-defs output defs env)
  (for-each (lambda (def)
              (gen! output $.align 4)
              (gen! output $.label
                           (entry.label
                            (cgenv-lookup env (def.lhs def))))
              (gen! output $.proc)
              (cg-known-lambda output
                               (def.rhs def)
                               env))
            defs))

(define (cg-if output exp target regs frame env tail?)
  (let ((L1 (make-label))
        (L2 (make-label)))
    (cg0 output (if.test exp) 'result regs frame env #f)
    (gen! output $branchf L1 (cgreg-tos regs))
    (let ((newframe (if (and tail?
                             (negative? (cgframe-size frame)))
                        (cgframe-initial)
                        frame)))
      (if (not (eq? frame newframe))
          (gen-save! output newframe))
      (let ((r (cg0 output (if.then exp) target regs newframe env tail?)))
        (if (not tail?)
            (gen! output $skip L2 (cgreg-live regs r)))
        (gen! output $.label L1)
        (let ((newframe (if (and tail?
                                 (negative? (cgframe-size frame)))
                            (cgframe-initial)
                            frame)))
          (if (not (eq? frame newframe))
              (gen-save! output newframe))
          (cg0 output (if.else exp) r regs newframe env tail?))
        (if (not tail?)
            (gen! output $.label L2))
        r))))

(define (cg-variable output exp target regs frame env tail?)
  (define (return)
    (if tail?
        (begin (gen-pop! output frame)
               (gen! output $return)
               'result)
        (cg-move output 'result target)))
  (let* ((id (variable.name exp))
         (entry (var-lookup id regs frame env)))
    (case (entry.kind entry)
      ((global integrable)
       (gen! output $global id)
       (return))
      ((lexical)
       (gen! output $lexical (entry.rib entry) (entry.offset entry) id)
       (return))
      ((procedure) (error "Bug in cg-variable" exp))
      ((register)
       (let ((r (entry.regnum entry)))
         (if (and target (not (eqv? target r)))
             (begin (gen! output $reg (entry.regnum entry) id)
                    (return))
             r)))
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
             (cg-move output 'result target)))
        ((null? (cdr exprs))
         (cg0 output (car exprs) target regs frame env tail?))
        (else (cg0 output (car exprs) #f regs frame env #f)
              (cg-sequential-loop output
                                  (cdr exprs)
                                  target regs frame env tail?))))

(define (cg-call output exp target regs frame env tail?)
  (let ((proc (call.proc exp)))
    (cond ((lambda? proc)
           (cg-let output exp target regs frame env tail?))
          ((not (variable? proc))
           (cg-unknown-call output exp target regs frame env tail?))
          (else (let ((entry
                       (var-lookup (variable.name proc) regs frame env)))
                  (case (entry.kind entry)
                    ((global lexical register)
                     (cg-unknown-call output
                                      exp
                                      target regs frame env tail?))
                    ((integrable)
                     (cg-integrable-call output
                                         exp
                                         target regs frame env tail?))
                    ((procedure)
                     (cg-known-call output
                                    exp
                                    target regs frame env tail?))
                    (else (error "Bug in cg-call" exp))))))))

(define (cg-let output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (liveregs (cgreg-liveregs regs)))
    (cond ((>= n *lastreg*)
           (cg-unknown-call output exp target regs frame env tail?))
          ((>= (+ n liveregs) *nregs*)
           (cg-unknown-call output exp target regs frame env tail?))
          (else
           (cg-pushargs output args regs frame env)
           (cg-body output
                    (lambda.body proc)
                    (lambda.defs proc)
                    target
                    (cgreg-bindregs regs (lambda.args proc))
                    frame
                    env
                    tail?)))))

(define (cg-pushargs output args regs frame env)
  (if (not (null? args))
      (let* ((newregs (cgreg-push regs 1))
             (r (cgreg-tos newregs)))
        (cg0 output (car args) r regs frame env #f)
        (cg-pushargs output (cdr args) newregs frame env))))

(define (cg-unknown-call output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (L (make-label)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call output exp target regs frame env tail?))
          (else
           (call-with-values
            (lambda ()
              (cgframe-newtemps frame (if tail? 0 (+ (cgreg-tos regs) 1))))
            (lambda (temps frame)
              (if (not tail?)
                  (cg-saveregs output (cgreg-tos regs) temps))
              (if (and (variable? proc)
                       (not (eq? (entry.kind
                                  (var-lookup (variable.name proc)
                                              regs
                                              frame
                                              env))
                                 'register)))
                  (begin (cg-arguments output
                                       (iota1 n)
                                       args
                                       regs frame env)
                         (cg0 output proc 'result regs frame env #f)
                         (if tail?
                             (gen-pop! output frame)
                             (begin (cgframe-used! frame)
                                    (gen! output $setrtn L)))
                         (gen! output $invoke n))
                  (begin (cg-arguments output
                                       (iota1 (+ n 1))
                                       (append args (list proc))
                                       regs frame env)
                         (if tail?
                             (gen-pop! output frame)
                             (begin (cgframe-used! frame)
                                    (gen! output $setrtn L)))
                         (gen! output $reg (+ n 1))
                         (gen! output $invoke n)))
              (if tail?
                  'result
                  (begin (gen! output $.align 4)
                         (gen! output $.label L)
                         (gen! output $.cont)
                         (cg-restoreregs output (cgreg-tos regs) temps)
                         (cg-move output 'result target)))))))))

(define (cg-known-call output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (n (length args))
         (L (make-label)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call output exp target regs frame env tail?))
          (else
           (call-with-values
            (lambda ()
              (cgframe-newtemps frame (if tail? 0 (+ (cgreg-tos regs) 1))))
            (lambda (temps frame)
              (if (not tail?)
                  (cg-saveregs output (cgreg-tos regs) temps))
              (cg-arguments output (iota1 n) args regs frame env)
              (if tail?
                  (gen-pop! output frame)
                  (begin (cgframe-used! frame)
                         (gen! output $setrtn L)))
              (let* ((entry (cgenv-lookup env (variable.name (call.proc exp))))
                     (label (entry.label entry))
                     (m (entry.rib entry)))
                (if (zero? m)
                    (gen! output $branch label n)
                    (gen! output $jump m label n)))
              (if tail?
                  'result
                  (begin (gen! output $.align 4)
                         (gen! output $.label L)
                         (gen! output $.cont)
                         (cg-restoreregs output (cgreg-tos regs) temps)
                         (cg-move output 'result target)))))))))

; Any call can be compiled as follows, even if there are no free registers.
;
; Let T0, T1, ..., Tn be newly allocated stack temporaries.
;
;     <arg0>
;     setstk  T0
;     <arg1>             -|
;     setstk  T1          |
;     ...                 |- evaluate args into stack frame
;     <argn>              |
;     setstk  Tn         -|
;     const   ()
;     setreg  R-1
;     stack   Tn         -|
;     op2     cons,R-1    |
;     setreg  R-1         |
;     ...                 |- cons up overflow args
;     stack   T_{R-1}     |
;     op2     cons,R-1    |
;     setreg  R-1        -|
;     stack   T_{R-2}      -|
;     setreg  R-2           |
;     ...                   |- pop remaining args into registers
;     stack   T1            |
;     setreg  1            -|
;     stack   T0
;     invoke  n

(define (cg-big-call output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (R-1 (- *nregs* 1))
         (entry (if (variable? proc)
                    (let ((entry
                           (var-lookup (variable.name proc)
                                       regs frame env)))
                      (if (eq? (entry.kind entry) 'procedure)
                          entry
                          #f))
                    #f))
         (L (make-label)))
    (call-with-values
     (lambda ()
       (cgframe-newtemps frame (if tail? 0 (+ (cgreg-tos regs) 1))))
     (lambda (temps frame)
       (call-with-values
        (lambda ()
          (cgframe-newtemps frame n))
        (lambda (argslots frame)
          (call-with-values
           (lambda ()
             (cgframe-newtemps frame (if entry 0 1)))
           (lambda (procslots frame)
             (if (not tail?)
                 (cg-saveregs output (cgreg-tos regs) temps))
             (if (not entry)
                 (begin
                  (cg0 output proc 'result regs frame env #f)
                  (gen! output $setstk (car procslots))))
             (for-each (lambda (arg argslot)
                         (cg0 output arg 'result regs frame env #f)
                         (gen! output $setstk argslot))
                       args
                       argslots)
             (gen! output $const '())
             (gen! output $setreg R-1)
             (do ((i n (- i 1))
                  (slots (reverse argslots) (cdr slots)))
                 ((zero? i))
                 (if (< i R-1)
                     (gen! output $load i (car slots))
                     (begin (gen! output $stack (car slots))
                            (gen! output $op2 $cons R-1)
                            (gen! output $setreg R-1))))
             (if (not entry)
                 (gen! output $stack (car procslots)))
             (if tail?
                 (gen-pop! output frame)
                 (begin (cgframe-used! frame)
                        (gen! output $setrtn L)))
             (if entry
                 (let ((label (entry.label entry))
                       (m (entry.rib entry)))
                   (if (zero? m)
                       (gen! output $branch label n)
                       (gen! output $jump m label n)))
                 (gen! output $invoke n))
             (if (not tail?)
                 (begin (gen! output $.align 4)
                        (gen! output $.label L)
                        (gen! output $.cont)
                        (cg-restoreregs output (cgreg-tos regs) temps)
                        (cg-move output 'result target)))))))))))

(define (cg-integrable-call output exp target regs frame env tail?)
  (let ((args (call.args exp))
        (entry (var-lookup (variable.name (call.proc exp)) regs frame env)))
    (if (= (entry.arity entry) (length args))
        (begin (case (entry.arity entry)
                 ((0) (gen! output $op1 (entry.op entry)))
                 ((1) (cg0 output (car args) 'result regs frame env #f)
                      (gen! output $op1 (entry.op entry)))
                 ((2) (cg-integrable-call2 output
                                           entry
                                           args
                                           regs frame env))
                 ((3) (cg-integrable-call3 output
                                           entry
                                           args
                                           regs frame env))
                 (else (error "Bug detected by cg-integrable-call"
                              (make-readable exp))))
               (if tail?
                   (begin (gen-pop! output frame)
                          (gen! output $return)
                          'result)
                   (cg-move output 'result target)))
        (error "Wrong number of arguments to integrable procedure"
               (make-readable exp)))))

(define (cg-integrable-call2 output entry args regs frame env)
  (let ((op (entry.op entry)))
    (if (and (entry.imm entry)
             (constant? (cadr args))
             ((entry.imm entry) (constant.value (cadr args))))
        (begin (cg0 output (car args) 'result regs frame env #f)
               (gen! output $op2imm
                            op
                            (constant.value (cadr args))))
        (let ((reg2 (cg0 output (cadr args) #f regs frame env #f)))
          (cond ((not (eq? reg2 'result))
; Original
                 (cg0 output (car args) 'result regs frame env #f)
                 (gen! output $op2 op reg2))
; New
; The problem with the preceding code is that when (car args) is evaluated,
; there is no record that the register that holds the result from (cadr args)
; is live.  The following code fixes that, while trying to minimize 
; the register moves that must be done.
;		 (cond ((= (cgreg-tos regs) (- reg2 1))              ; @@ Lars
;			(cg0 output                                  ; @@ Lars
;			     (car args) 'result (cgreg-push1 regs)   ; @@ Lars
;			     frame env #f)                           ; @@ Lars
;                        (gen! output $op2 op reg2))                  ; @@ Lars
;		       ((>= (cgreg-tos regs) reg2)                   ; @@ Lars
;			(cg0 output                                  ; @@ Lars
;			     (car args) 'result regs frame env #f)   ; @@ Lars
;                        (gen! output $op2 op reg2))                  ; @@ Lars
;		       (else                                         ; @@ Lars
;			(let* ((regs (cgreg-push1 regs))             ; @@ Lars
;			       (r    (cgreg-tos regs)))              ; @@ Lars
;			  (gen! output $setreg r)                    ; @@ Lars
;			  (cg0 output                                ; @@ Lars
;			       (car args) 'result regs frame env #f) ; @@ Lars
;			  (gen! output $op2 op r)))))                ; @@ Lars
                ((not (= (cgreg-tos regs) *lastreg*))
                 (let* ((regs (cgreg-push regs 1))
                        (r (cgreg-tos regs)))
                   (gen! output $setreg r)
                   (cg0 output (car args) 'result regs frame env #f)
                   (gen! output $op2 op r)))
                (else
                 ; This generates poor code, but shouldn't happen often.
                 (call-with-values
                  (lambda () (cgframe-newtemps frame 2))
                  (lambda (temps frame)
                    (let ((s1 (car temps))
                          (t2 (cadr temps)))
                      (gen! output $setstk t2)
                      (cg0 output (car args) 'result regs frame env #f)
                      (gen! output $store 1 s1)
                      (gen! output $load 1 t2)
                      (gen! output $op2 op 1)
                      (gen! output $load 1 s1)))))))))
  'result)

(define (cg-integrable-call3 output entry args regs frame env)
  (define (evalarg arg regs)
    (let ((r (cg0 output arg #f regs frame env #f)))
      (if (eq? r 'result)
          (let* ((regs (cgreg-push regs 1))
                 (r (cgreg-tos regs)))
            (cg-move output 'result r)
            (values r regs))
          (values r regs))))
  (if (< (cgreg-tos regs) (- *lastreg* 1))
      (call-with-values
       (lambda () (evalarg (caddr args) regs))
       (lambda (r3 regs)
         (call-with-values
          (lambda () (evalarg (cadr args) regs))
          (lambda (r2 regs)
            (cg0 output (car args) 'result regs frame env #f)
            (gen! output $op3 (entry.op entry) r2 r3)))))
      (call-with-values
       (lambda () (cgframe-newtemps frame 4))
       (lambda (temps frame)
         (define (worstcase s1 s2 t2 t3)
           (cg0 output (caddr args) 'result regs frame env #f)
           (gen! output $setstk t3)
           (cg0 output (cadr args) 'result regs frame env #f)
           (gen! output $setstk t2)
           (cg0 output (car args) 'result regs frame env #f)
           (gen! output $store 1 s1)
           (gen! output $store 2 s2)
           (gen! output $load 1 t2)
           (gen! output $load 2 t3)
           (gen! output $op3 (entry.op entry) 1 2)
           (gen! output $load 1 s1)
           (gen! output $load 2 s2))
         (apply worstcase temps)))))

(define (cg-saveregs output k temps)
  (do ((i 0 (+ i 1))
       (temps temps (cdr temps)))
      ((> i k))
      (gen! output $store i (car temps))))

(define (cg-restoreregs output k temps)
  (do ((i 0 (+ i 1))
       (temps temps (cdr temps)))
      ((> i k))
      (gen! output $load i (car temps))))

(define (cg-move output src dst)
  (cond ((not dst)
         src)
        ((eqv? src dst)
         dst)
        ((eq? src 'result)
         (gen! output $setreg dst)
         dst)
        ((eq? dst 'result)
         (gen! output $reg src)
         dst)
        ((and (not (zero? src))
              (not (zero? dst)))
         (gen! output $movereg src dst))
        (else
         (gen! output $reg src)
         (gen! output $setreg dst)
         dst)))

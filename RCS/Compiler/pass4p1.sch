; Copyright 1991 Lightship Software, Incorporated.
;
; Fourth pass of the Scheme 313 compiler:
;   byte code generation.
;
; This pass operates on input expressions described by the
; following grammar and the invariants that follow it.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote <info>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote <info>)
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
; <info>  -->  (T F)
; T  -->  <see below>
; F  -->  (I ...)
;
; Invariants that hold for the input
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  Every procedure defined by an internal definition takes a
;      fixed number of arguments.  (Not implemented yet!!!!!)
;   *  Every call to a procedure defined by an internal definition
;      passes the correct number of arguments.  (Not implemented yet!!!!!)
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  Variables named IGNORED are neither referenced nor assigned.
;   *  The T field is garbage or can be ignored.

(define (pass4 exp integrable)
  (set! cg-label-counter 1000)
  (cg-make-linear (cg0 exp (cgenv-initial integrable) #t)))

(define (idisplay x)
  (display x) 
  (newline))


; Given an expression, a compile-time environment, and a flag
; indicating whether the expression is in tail-recursive position,
; returns a tree of MacScheme assembly instructions in reverse order.

(define (cg0 exp env tail?)
  (case (car exp)
    ((quote)    (cg-return (cg-ops (cg-op $const (constant.value exp)))
                           tail?))
    ((lambda)   (cg-return (cg-lambda exp env) tail?))
    ((set!)     (cg-return (cg-linearize
                            (cg0 (assignment.rhs exp) env #f)
                            (cg-ops (cg-op $setglbl (assignment.lhs exp))))
                           tail?))
    ((if)       (cg-if exp env tail?))
    ((begin)    (if (variable? exp)
                    (cg-variable exp env tail?)
                    (cg-sequential exp env tail?)))
    (else       (cg-call exp env tail?))))

; For the moment a lambda expression will close over all
; live registers, regardless of whether the registers contain
; variables or temporaries and regardless of whether the
; variables occur free in the lambda expression.

(define (cg-lambda exp env)
  (let* ((args (lambda.args exp))
         (vars (make-null-terminated args))
         (newenv (cgenv-pop
                  (cgenv-extend env (cgenv-regvars env) '())
                  (cgenv-tos env)))
         (code (cg-linearize
                (cg-ops (cg-op $.proc)
                        (if (list? args)
                            (cg-op $args= (length args))
                            (cg-op $args>= (- (length vars) 1))))
                (cg-known-lambda exp newenv))))
    (cg-ops (cg-op $lambda
                   (cg-make-linear code)
                   (cgenv-tos env)
                   (cgenv-regvars env)))))

; Lambda expressions that appear on the rhs of a definition are
; compiled here.  They don't need an args= instruction at their head.

(define (cg-known-lambda exp env)
  (let* ((vars (make-null-terminated (lambda.args exp)))
         (defs (lambda.defs exp))
         (body (lambda.body exp)))
    (if (> (length vars) *fullregs*)
        (cg-linearize
         (cg-ops (cg-op $lexes (length vars) (cons #t vars))
                 (cg-op $setreg 0))
         (cg-body body
                  defs
                  (cgenv-extend env
                                (cons '#t vars) '()) #t))
        (cg-body body defs (cgenv-bindregs env vars) #t))))

; Compiles a let or lambda body.
; The problem here is that the free variables of an internal
; definition must be in a heap-allocated environment, so any
; such variables in registers must be copied to the heap.

(define (cg-body exp defs env tail?)
  (cond ((or (null? defs) (constant? exp) (variable? exp))
         (cg0 exp env tail?))
        ((lambda? exp)
         (let* ((newenv1 (cgenv-pop
                          (cgenv-extend env
                                        (cgenv-regvars env)
                                        (map def.lhs defs))
                          (cgenv-tos env)))
                (args (lambda.args exp))
                (vars (make-null-terminated args))
                (code
                 (cg-linearize
                  (cg-ops (cg-op $.proc)
                          (if (list? args)
                              (cg-op $args= (length args))
                              (cg-op $args>= (- (length vars) 1))))
                  (cg-linearize
                   (cg-known-lambda exp newenv1)
                   (cg-defs defs newenv1)))))
           (cg-return (cg-ops (cg-op $lambda
                                     (cg-make-linear code)
                                     (cgenv-tos env)
                                     (cgenv-regvars env)))
                      tail?)))
        ((zero? (cgenv-tos env))
         (let ((newenv (cgenv-bindprocs env (map def.lhs defs)))
               (L (make-label)))
           (if tail?
               (cg-linearize
                (cg0 exp newenv #t)
                (cg-defs defs newenv))
               (cg-linearize
                (cg-linearize
                 (cg0 exp newenv #f)
                 (cg-ops (cg-op $skip L)))
                (cg-linearize
                 (cg-defs defs newenv)
                 (cg-ops (cg-op $.label L)))))))
        (else
         (let* ((k (cgenv-tos env))
                (newenv1 (cgenv-extend env
                                       (cgenv-regvars env)
                                       (map def.lhs defs)))
                (newenv2 (cgenv-pop newenv1 k))
                (L (make-label)))
           (if tail?
               (cg-linearize
                (cg-ops (cg-op $lexes k (cgenv-regvars env))
                        (cg-op $setreg 0))
                (cg-linearize
                 (cg0 exp newenv1 #t)
                 (cg-defs defs newenv2)))
               (cg-linearize
                (cg-ops (begin (if (> k 31) (idisplay "foo (1)!"))
			       (cg-op $save L k))
                        (cg-op $lexes k (cgenv-regvars env))
                        (cg-op $setreg 0))
                (cg-linearize
                 (cg-linearize
                  (cg0 exp newenv1 #f)
                  (cg-ops (cg-op $skip L)))
                 (cg-linearize
                  (cg-defs defs newenv2)
                  (cg-ops (cg-op $.align 4)
                          (cg-op $.label L)
                          (cg-op $restore k)
                          (cg-op $pop k))))))))))

(define (cg-defs defs env)
  (do ((code '()
             (cg-linearize
              code
              (cg-linearize
               (cg-ops (cg-op $.align 4)
                       (cg-op $.label
                              (entry.label
                               (cgenv-lookup env (def.lhs (car defs)))))
                       (cg-op $.proc))
               (cg-known-lambda
                (def.rhs (car defs))
                env))))
       (defs defs (cdr defs)))
      ((null? defs) code)))

(define (cg-if exp env tail?)
  (let ((code0 (cg0 (if.test exp) env #f))
        (code1 (cg0 (if.then exp) env tail?))
        (code2 (cg0 (if.else exp) env tail?))
        (L1 (make-label))
        (L2 (make-label)))
    (cg-linearize
     (cg-linearize
      code0
      (cg-ops (cg-op $branchf L1)))
     (cg-linearize
      (if tail?
          code1
          (cg-linearize
           code1
           (cg-ops (cg-op $skip L2))))
      (if tail?
          (cg-linearize (cg-ops (cg-op $.label L1)) code2)
          (cg-linearize
           (cg-linearize (cg-ops (cg-op $.label L1)) code2)
           (cg-ops (cg-op $.label L2))))))))

(define (cg-variable exp env tail?)
  (cg-return
   (let* ((id (variable.name exp))
          (entry (cgenv-lookup env id)))
     (case (entry.kind entry)
       ((global integrable) (cg-ops (cg-op $global id)))
       ((lexical) (cg-ops (cg-op $lexical (entry.rib entry)
                                          (entry.offset entry)
                                          id)))
       ((procedure) ???)
       ((register) (cg-ops (cg-op $reg (entry.regnum entry) id)))
       (else ???)))
   tail?))

(define (cg-sequential exp env tail?)
  (cg-sequential-loop (begin.exprs exp) env tail?))

(define (cg-sequential-loop exprs env tail?)
  (cond ((null? exprs)
         (cg-return (cg-op $const hash-bang-unspecified) tail?))
        ((null? (cdr exprs))
         (cg0 (car exprs) env tail?))
        (else (cg-linearize
               (cg0 (car exprs) env #f)
               (cg-sequential-loop (cdr exprs) env tail?)))))

(define (cg-call exp env tail?)
  (let ((proc (call.proc exp)))
    (cond ((lambda? proc)
           (cg-let exp env tail?))
          ((not (variable? proc))
           (cg-unknown-call exp env tail?))
          (else (let ((entry (cgenv-lookup env (variable.name proc))))
                  (case (entry.kind entry)
                    ((global lexical register)
                     (cg-unknown-call exp env tail?))
                    ((integrable)
                     (cg-integrable-call exp env tail?))
                    ((procedure)
                     (cg-known-call exp env tail?))
                    (else ???)))))))

(define (cg-let exp env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (liveregs (cgenv-liveregs env)))
    (cond ((>= n *lastreg*)
           (cg-unknown-call exp env tail?))
          ((>= (+ n liveregs) *nregs*)
           (cg-spill exp env tail?))
          (else
           (cg-linearize
            (cg-pushargs args env)
            (cg-body (lambda.body proc)
                     (lambda.defs proc)
                     (cgenv-bindregs env (lambda.args proc))
                     tail?))))))

(define (cg-unknown-call exp env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call exp env tail?))
          (else
           (let ((code
                  (if (and (variable? proc)
                           (not (eq? (entry.kind
                                      (cgenv-lookup env
                                                    (variable.name proc)))
                                     'register)))
                      (cg-linearize
                       (cg-arguments (iota1 n) args env)
                       (cg-linearize
                        (cg0 proc env #f)
                        (cg-ops (cg-op $invoke n))))
                      (cg-linearize
                       (cg-arguments (iota1 (+ n 1))
                                     (append args (list proc))
                                     env)
                       (cg-ops (cg-op $reg (+ n 1))
                               (cg-op $invoke n))))))
             (if tail?
                 code
                 (let ((L (make-label))
                       (k (cgenv-tos env)))
                   (cg-linearize
                    (cg-ops (begin (if (> k 31) (idisplay "foo (2)!"))
				   (cg-op $save L k)))
                    (cg-linearize
                     code
                     (cg-ops (cg-op $.align 4)
                             (cg-op $.label L)
                             (cg-op $.cont)
                             (cg-op $restore k)
                             (cg-op $pop k)))))))))))

(define (cg-known-call exp env tail?)
  (let ((n (length (call.args exp))))
    (cond ((>= n *lastreg*)
           (cg-big-call exp env tail?))
          ((not tail?)
           (let ((L (make-label))
                 (k (cgenv-tos env)))
             (cg-linearize
              (cg-ops (begin (if (> k 31) (idisplay "foo (3)!"))
			     (cg-op $save L k)))
              (cg-linearize
               (cg-known-call exp env #t)
               (cg-ops (cg-op $.align 4)
                       (cg-op $.label L)
                       (cg-op $.cont)
                       (cg-op $restore k)
                       (cg-op $pop k))))))
          (else
           (let* ((entry (cgenv-lookup env (variable.name (call.proc exp))))
                  (label (entry.label entry))
                  (m (entry.rib entry))
                  (args (call.args exp)))
             (cg-linearize
              (cg-arguments (iota1 n) args env)
              (cg-ops (if (zero? m)
                          (cg-op $branch label)
                          (cg-op $jump m label)))))))))

; Any call can be compiled as follows, even if there are no free registers:
;
;     save    L,n+1
;     <arg0>
;     setstk  n+1
;     <arg1>             -|
;     setstk  1           |
;     ...                 |- evaluate args into stack frame
;     <argn>              |
;     setstk  n          -|
;     const   ()
;     setreg  R-1
;     stack   n          -|
;     op2     cons,R-1    |
;     setreg  R-1         |
;     ...                 |- cons up overflow args
;     stack   R-1         |
;     op2     cons,R-1    |
;     setreg  R-1        -|
;     stack   R-2          -|
;     setreg  R-2           |
;     ...                   |- pop remaining args into registers
;     stack   1             |
;     setreg  1            -|
;     stack   n+1
; L:  pop     n+1
;     invoke  n

(define (cg-big-call exp env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (n+1 (+ n 1))
         (R-1 (- *nregs* 1))
         (entry (if (variable? proc)
                    (let ((entry
                           (cgenv-lookup env (variable.name proc))))
                      (if (eq? (entry.kind entry) 'procedure)
                          entry
                          #f))
                    #f))
         (k (cgenv-tos env)))
    (define (evalargs i args)
      (if (> i n)
          '()
          (cg-linearize
           (cons (cg-op $setstk i)
                 (cg0 (car args) env #f))
           (evalargs (+ i 1) (cdr args)))))
    (define (consup i)
      (if (<= i n)
          (cons (cg-op $setreg R-1)
                (cons (cg-op $op2 $cons R-1)
                      (cons (cg-op $stack i)
                            (consup (+ i 1)))))
          '()))
    (define (popargs i)
      (if (= i R-1)
          '()
          (cons (cg-op $setreg i)
                (cons (cg-op $stack i)
                      (popargs (+ i 1))))))
    (define (thecall)
      (let ((L (make-label)))
        (cg-linearize
         (cg-ops (begin (if (> n+1 31) (idisplay "foo (4)!"))
			(cg-op $save L n+1)))
         (cg-linearize
          (if entry
              '()
              (cons (cg-op $setstk n+1)
                    (cg0 proc env #f)))
          (cg-linearize
           (evalargs 1 args)
           (cg-linearize
            (cg-linearize
             (cg-ops (cg-op $const '())
                     (cg-op $setreg R-1))
             (consup *lastreg*))
            (cg-linearize
             (popargs 1)
             (if entry
                 (cg-ops (cg-op $.align 4)
                         (cg-op $.label L)
                         (cg-op $pop n+1)
                         (let ((m (entry.rib entry)))
                           (if (zero? m)
                               (cg-op $branch (entry.label entry))
                               (cg-op $jump m (entry.label entry)))))
                 (cg-ops (cg-op $stack n+1)
                         (cg-op $.align 4)
                         (cg-op $.label L)
                         (cg-op $pop n+1)
                         (cg-op $invoke n))))))))))
    (if tail?
        (thecall)
        (let ((L (make-label)))
          (cg-linearize
           (cg-ops (begin (if (> k 31) (idisplay "foo (5)!"))
			  (cg-op $save L k)))
           (cg-linearize
            (thecall)
            (cg-ops (cg-op $.align 4)
                    (cg-op $.label L)
                    (cg-op $.cont)
                    (cg-op $restore k)
                    (cg-op $pop k))))))))

(define (cg-integrable-call exp env tail?)
  (let ((args (call.args exp))
        (entry (cgenv-lookup env (variable.name (call.proc exp)))))
    (if (= (entry.arity entry) (length args))
        (cg-return
         (case (entry.arity entry)
           ((0) (cg-ops (cg-op $op1 (entry.op entry))))
           ((1) (cg-linearize
                 (cg0 (car args) env #f)
                 (cg-ops (cg-op $op1 (entry.op entry)))))
           ((2) (cg-integrable-call2 entry args env))
           ((3) (cg-integrable-call3 entry args env))
           (else ???))
         tail?)
        ???)))

(define (cg-integrable-call2 entry args env)
  (let ((reg2 (if (variable? (cadr args))
                  (let ((entry2
                         (cgenv-lookup env (variable.name (cadr args)))))
                    (if (eq? (entry.kind entry2) 'register)
                        (entry.regnum entry2)
                        #f))
                  #f)))
    (cond (reg2
           (cg-linearize
            (cg0 (car args) env #f)
            (cg-ops (cg-op $op2 (entry.op entry) reg2))))
          ((= (cgenv-tos env) *lastreg*)
           (let ((L (make-label)))
             (cg-linearize
              (cg-ops (cg-op $save L 2))
              (cg-linearize
               (cg-linearize
                (cg0 (cadr args) env #f)
                (cg-ops (cg-op $setstk 2)))
               (cg-linearize
                (cg0 (car args) env #f)
                (cg-ops (cg-op $load 2 1)
                        (cg-op $op2 (entry.op entry) 1)
                        (cg-op $.align 4)
                        (cg-op $.label L)
                        (cg-op $restore 1)
                        (cg-op $pop 2)))))))
          ((and (entry.imm entry)
                (constant? (cadr args))
                ((entry.imm entry) (constant.value (cadr args))))
           (cg-linearize
            (cg0 (car args) env #f)
            (cg-ops (cg-op $op2imm (entry.name entry) (constant.value (cadr args))))))
          (else
           (let ((newenv (cgenv-push env 1)))
             (cg-linearize
              (cg-linearize
               (cg0 (cadr args) env #f)
               (cg-ops (cg-op $setreg (cgenv-tos newenv))))
              (cg-linearize
               (cg0 (car args) newenv #f)
               (cg-ops (cg-op $op2 (entry.op entry) (cgenv-tos newenv))))))))))

; Any expression can be compiled by spilling registers to the heap.

(define (cg-spill exp env tail?)
  (let* ((k (cgenv-tos env))
         (newenv
          (cgenv-pop (cgenv-extend env (cgenv-regvars env) '())
                     k)))
    (if tail?
        (cg-linearize
         (cg-ops (cg-op $lexes k (cgenv-regvars env))
                 (cg-op $setreg 0))
         (cg0 exp newenv #t))
        (let ((L (make-label)))
          (cg-linearize
           (cg-ops (cg-op $save L 0)
                   (cg-op $lexes k (cgenv-regvars env))
                   (cg-op $setreg 0))
           (cg-linearize
            (cg0 exp newenv #f)
            (cg-ops (cg-op $.align 4)
                    (cg-op $.label L)
                    (cg-op $restore 0)
                    (cg-op $pop 0))))))))

(define (cg-integrable-call3 entry args env)
   (let ((reg2 (if (variable? (cadr args))
                   (let ((entry2
                          (cgenv-lookup env (variable.name (cadr args)))))
                     (if (eq? (entry.kind entry2) 'register)
                         (entry.regnum entry2)
                         #f))
                   #f))
         (reg3 (if (variable? (caddr args))
                   (let ((entry3
                          (cgenv-lookup env (variable.name (caddr args)))))
                     (if (eq? (entry.kind entry3) 'register)
                         (entry.regnum entry3)
                         #f))
                   #f)))
     (cond ((and reg2 reg3)
            (cg-linearize
             (cg0 (car args) env #f)
             (cg-ops (cg-op $op3 (entry.op entry) reg2 reg3))))
           ; wasting one register here if (or reg2 reg3)
           ((>= (cgenv-tos env) (- *lastreg* 1))
            (let ((L (make-label)))
              (cg-linearize
               (cg-ops (cg-op $save L 4))
               (cg-linearize
                (cg-linearize
                 (cg0 (caddr args) env #f)
                 (cg-ops (cg-op $setstk 4)))
                (cg-linearize
                 (cg-linearize
                  (cg0 (cadr args) env #f)
                  (cg-ops (cg-op $setstk 3)))
                 (cg-linearize
                  (cg0 (car args) env #f)
                  (cg-ops (cg-op $load 4 2)
                          (cg-op $load 3 1)
                          (cg-op $op3 (entry.op entry) 1 2)
                          (cg-op $.align 4)
                          (cg-op $.label L)
                          (cg-op $restore 2)
                          (cg-op $pop 4))))))))
           (reg2
            (let ((newenv (cgenv-push env 1)))
              (cg-linearize
               (cg-linearize
                (cg-linearize
                 (cg0 (caddr args) env #f)
                 (cg-ops (cg-op $setreg (cgenv-tos newenv))))
                (cg0 (car args) newenv #f))
               (cg-ops (cg-op $op3 (entry.op entry) reg2 (cgenv-tos newenv))))))
           (reg3
            (let ((newenv (cgenv-push env 1)))
              (cg-linearize
               (cg-linearize
                (cg-linearize
                 (cg0 (cadr args) env #f)
                 (cg-ops (cg-op $setreg (cgenv-tos newenv))))
                (cg0 (car args) newenv #f))
               (cg-ops (cg-op $op3 (entry.op entry) (cgenv-tos newenv) reg3)))))
           (else
            (let ((newenv1 (cgenv-push env 1))
                  (newenv2 (cgenv-push env 2)))
              (cg-linearize
               (cg-linearize
                (cg0 (caddr args) env #f)
                (cg-ops (cg-op $setreg (cgenv-tos newenv1))))
               (cg-linearize
                (cg-linearize
                 (cg0 (cadr args) newenv1 #f)
                 (cg-ops (cg-op $setreg (cgenv-tos newenv2))))
                (cg-linearize
                 (cg0 (car args) newenv2 #f)
                 (cg-ops (cg-op $op3 (entry.op entry)
                                     (cgenv-tos newenv2)
                                     (cgenv-tos newenv1)))))))))))

(define (cg-return tree tail?)
  (if tail?
      (cg-linearize
       tree
       (cg-ops (cg-op $return)))
      tree))

; Parallel assignment.

; Given a list of target registers, a list of expressions, and a
; compile-time environment, returns code to evaluate the expressions
; into the registers.
; Evaluates directly into the target registers if possible, otherwise
; pushes all arguments and then pops into the target registers.  It
; ought to degrade more gracefully.
; Another shortcoming is that it reserves all registers through the
; last target register while evaluating the arguments.

(define (cg-arguments regs args env)
  (cond ((null? regs) '())
        (else
         (let ((para (let* ((regvars (map (lambda (reg)
                                            (cgenv-lookup-reg env reg))
                                          regs)))
                       (parallel-assignment regs
                                            (map cons regvars regs)
                                            args))))
           (if para
               (do ((regs para (cdr regs))
                    (args (cg-permute args regs para) (cdr args))
                    (env env
                         (let ((reg (car regs))
                               (tos (cgenv-tos env)))
                           (if (> reg tos)
                               (cgenv-push env (- reg tos))
                               env)))
                    (code '()
                          (if (and (variable? (car args))
                                   (eq? (variable.name (car args))
                                        (cgenv-lookup-reg env (car regs))))
                              code
                              (cg-linearize
                               code
                               (cg-linearize
                                (cg0 (car args) env #f)
                                (cg-ops (cg-op $setreg (car regs))))))))
                   ((null? regs) code))
               (cg-linearize
                (cg-pushargs args env)
                (cg-popargs regs (+ (cgenv-tos env) 1))))))))

; Pushes arguments from left to right.

(define (cg-pushargs args env)
  (do ((code '()
             (cg-linearize
              code
              (cg-linearize
               (cg0 (car args) env #f)
               (cg-ops (cg-op $setreg (+ (cgenv-tos env) 1))))))
       (args args (cdr args))
       (env env (cgenv-push env 1)))
      ((null? args) code)))

; Given a list of n target registers, moves REGk through REG{k+n-1}
; to the target registers.

(define (cg-popargs regs k)
  (do ((regs regs (cdr regs))
       (k k (+ k 1))
       (code '()
             (cg-linearize
              code
              (cg-ops (cg-op $movereg k (car regs))))))
      ((null? regs) code)))

; Returns a permutation of the src list, permuted the same way the
; key list was permuted to obtain newkey.

(define (cg-permute src key newkey)
  (let ((alist (map cons key (iota (length key)))))
    (do ((newkey newkey (cdr newkey))
         (dest '()
               (cons (list-ref src (cdr (assq (car newkey) alist)))
                     dest)))
        ((null? newkey) (reverse dest)))))


; Copyright 1991 William Clinger
;
; $Id$
;
; 21 May 1999.

; Procedure calls.

(define (cg-call output exp target regs frame env tail?)
  (cg-call/cont output exp target regs frame env tail? #f))

; Given a procedure call exp, and some encoding of its
; continuation, generates code for the call and for its
; return point.

(define (cg-call/cont output exp target regs frame env tail? cont)
  (define (assert-normal-continuation!)
    (if cont
        (twobit-bug "cg-call/cont" cont)))
  (let ((proc (call.proc exp)))
    (cond ((and (lambda? proc)
                (list? (lambda.args proc)))
           (assert-normal-continuation!)
           (cg-let output exp target regs frame env tail?))
          ((not (variable? proc))
           (cg-unknown-call output exp target regs frame env tail? cont))
          (else (let ((entry
                       (var-lookup (variable.name proc) regs frame env)))
                  (case (entry.kind entry)
                    ((global lexical frame register)
                     (cg-unknown-call output
                                      exp
                                      target regs frame env tail? cont))
                    ((integrable)
                     (assert-normal-continuation!)
                     (cg-integrable-call output
                                         exp
                                         target regs frame env tail?))
                    ((procedure)
                     (cg-known-call output
                                    exp
                                    target regs frame env tail? cont))
                    (else (twobit-bug "cg-call" exp))))))))

(define (cg-unknown-call output exp target regs frame env tail? cont)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (L (make-label)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call output exp target regs frame env tail? cont))
          (else
           (let ((r0 (cgreg-lookup-reg regs 0)))
             (if (variable? proc)
                 (let ((entry (cgreg-lookup regs (variable.name proc))))
                   (if (and entry
                            (<= (entry.regnum entry) n))
                       (begin (cg-arguments output
                                            (iota1 (+ n 1))
                                            (append args (list proc))
                                            regs frame env)
                              (gen! output $reg (+ n 1)))
                       (begin (cg-arguments output
                                            (iota1 n)
                                            args
                                            regs frame env)
                              (cg0 output proc 'result regs frame env #f)))
                   (if tail?
                       (gen-pop! output frame)
                       (begin (cgframe-used! frame)
                              (gen! output $setrtn L)))
                   (gen! output $invoke n))
                 (begin (cg-arguments output
                                      (iota1 (+ n 1))
                                      (append args (list proc))
                                      regs frame env)
                        (gen! output $reg (+ n 1))
                        (if tail?
                            (gen-pop! output frame)
                            (begin (cgframe-used! frame)
                                   (gen! output $setrtn L)))
                        (gen! output $invoke n)))
             (if tail?
                 'result
                 (begin (gen! output $.align 4)
                        (gen! output $.label L)
                        (cg-return-point output cont regs frame r0)
                        (cg-move output frame regs 'result target))))))))

(define (cg-known-call output exp target regs frame env tail? cont)
  (let* ((args (call.args exp))
         (n (length args))
         (L (make-label)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call output exp target regs frame env tail? cont))
          (else
           (let ((r0 (cgreg-lookup-reg regs 0)))
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
                        (cg-return-point output cont regs frame r0)
                        (cg-move output frame regs 'result target))))))))

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

(define (cg-big-call output exp target regs frame env tail? cont)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (argslots (newtemps n))
         (procslot (newtemp))
         (r0 (cgreg-lookup-reg regs 0))
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
    (if (not entry)
        (begin
         (cg0 output proc 'result regs frame env #f)
         (gen-setstk! output frame procslot)))
    (for-each (lambda (arg argslot)
                (cg0 output arg 'result regs frame env #f)
                (gen-setstk! output frame argslot))
              args
              argslots)
    (cgreg-clear! regs)
    (gen! output $const '())
    (gen! output $setreg R-1)
    (do ((i n (- i 1))
         (slots (reverse argslots) (cdr slots)))
        ((zero? i))
        (if (< i R-1)
            (gen-load! output frame i (car slots))
            (begin (gen-stack! output frame (car slots))
                   (gen! output $op2 $cons R-1)
                   (gen! output $setreg R-1))))
    (if (not entry)
        (gen-stack! output frame procslot))
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
    (if tail?
        'result
        (begin (gen! output $.align 4)
               (gen! output $.label L)
               (cg-return-point output cont regs frame r0)
               (cg-move output frame regs 'result target)))))

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
                 (else (twobit-bug "cg-integrable-call"
                                   (make-readable exp))))
               (if tail?
                   (begin (gen-pop! output frame)
                          (gen! output $return)
                          'result)
                   (cg-move output frame regs 'result target)))
        (cond ((negative? (entry.arity entry))
               (cg-special output exp target regs frame env tail?))
              ((memq (variable.name (call.proc exp))
                     variable-arity-primops-that-allow-closed-calls)
               (cg-unknown-call output exp target regs frame env tail? #f))
              (else
               (twobit-error
                "Wrong number of arguments to integrable procedure"
                (make-readable exp))
               (cg-special output
                           (make-call-to-TRAP p2error:wna)
                           target regs frame env tail?))))))

(define (cg-integrable-call2 output entry args regs frame env)
  (let ((op (entry.op entry)))
    (if (and (entry.imm entry)
             (constant? (cadr args))
             ((entry.imm entry) (constant.value (cadr args))))
        (begin (cg0 output (car args) 'result regs frame env #f)
               (gen! output $op2imm
                            op
                            (constant.value (cadr args))))
        (let* ((reg2 (cg0 output (cadr args) #f regs frame env #f))
               (r2 (choose-register regs frame))
               (t2 (if (eq? reg2 'result)
                       (let ((t2 (newtemp)))
                         (gen! output $setreg r2)
                         (cgreg-bind! regs r2 t2)
                         (gen-store! output frame r2 t2)
                         t2)
                       (cgreg-lookup-reg regs reg2))))
          (cg0 output (car args) 'result regs frame env #f)
          (let* ((r2 (or (let ((entry (cgreg-lookup regs t2)))
                           (if entry
                               (entry.regnum entry)
                               #f))
                         (let ((r2 (choose-register regs frame)))
                           (cgreg-bind! regs r2 t2)
                           (gen-load! output frame r2 t2)
                           r2))))
            (gen! output $op2 (entry.op entry) r2)
            (if (eq? reg2 'result)
                (begin (cgreg-release! regs r2)
                       (cgframe-release! frame t2)))))))
  'result)

(define (cg-integrable-call3 output entry args regs frame env)
  (let* ((reg2 (cg0 output (cadr args) #f regs frame env #f))
         (r2 (choose-register regs frame))
         (t2 (if (eq? reg2 'result)
                 (let ((t2 (newtemp)))
                   (gen! output $setreg r2)
                   (cgreg-bind! regs r2 t2)
                   (gen-store! output frame r2 t2)
                   t2)
                 (cgreg-lookup-reg regs reg2)))
         (reg3 (cg0 output (caddr args) #f regs frame env #f))
         (spillregs (choose-registers regs frame 2))
         (t3 (if (eq? reg3 'result)
                 (let ((t3 (newtemp))
                       (r3 (if (eq? t2 (cgreg-lookup-reg
                                        regs (car spillregs)))
                               (cadr spillregs)
                               (car spillregs))))
                   (gen! output $setreg r3)
                   (cgreg-bind! regs r3 t3)
                   (gen-store! output frame r3 t3)
                   t3)
                 (cgreg-lookup-reg regs reg3))))
    (cg0 output (car args) 'result regs frame env #f)
    (let* ((spillregs (choose-registers regs frame 2))
           (r2 (or (let ((entry (cgreg-lookup regs t2)))
                           (if entry
                               (entry.regnum entry)
                               #f))
                   (let ((r2 (car spillregs)))
                     (cgreg-bind! regs r2 t2)
                     (gen-load! output frame r2 t2)
                     r2)))
           (r3 (or (let ((entry (cgreg-lookup regs t3)))
                           (if entry
                               (entry.regnum entry)
                               #f))
                   (let ((r3 (if (eq? r2 (car spillregs))
                                 (cadr spillregs)
                                 (car spillregs))))
                     (cgreg-bind! regs r3 t3)
                     (gen-load! output frame r3 t3)
                     r3))))
      (gen! output $op3 (entry.op entry) r2 r3)
      (if (eq? reg2 'result)
          (begin (cgreg-release! regs r2)
                 (cgframe-release! frame t2)))
      (if (eq? reg3 'result)
          (begin (cgreg-release! regs r3)
                 (cgframe-release! frame t3)))))
  'result)

; Given a short list of expressions that can be evaluated in any order,
; evaluates the first into the result register and the others into any
; register, and returns an ordered list of the registers that contain
; the arguments that follow the first.
; The number of expressions must be less than the number of argument
; registers.

(define (cg-primop-args output args regs frame env)
  
  ; Given a list of expressions to evaluate, a list of variables
  ; and temporary names for arguments that have already been
  ; evaluated, in reverse order, and a mask of booleans that
  ; indicate which temporaries should be released before returning,
  ; returns the correct result.
  
  (define (eval-loop args temps mask)
    (if (null? args)
        (eval-first-into-result temps mask)
        (let ((reg (cg0 output (car args) #f regs frame env #f)))
          (if (eq? reg 'result)
              (let* ((r (choose-register regs frame))
                     (t (newtemp)))
                (gen! output $setreg r)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t)
                (eval-loop (cdr args)
                           (cons t temps)
                           (cons #t mask)))
              (eval-loop (cdr args)
                         (cons (cgreg-lookup-reg regs reg) temps)
                         (cons #f mask))))))
  
  (define (eval-first-into-result temps mask)
    (cg0 output (car args) 'result regs frame env #f)
    (finish-loop (choose-registers regs frame (length temps))
                 temps
                 mask
                 '()))
  
  ; Given a sufficient number of disjoint registers, a list of
  ; variable and temporary names that may need to be loaded into
  ; registers, a mask of booleans that indicates which temporaries
  ; should be released, and a list of registers in forward order,
  ; returns the correct result.
  
  (define (finish-loop disjoint temps mask registers)
    (if (null? temps)
        registers
        (let* ((t (car temps))
               (entry (cgreg-lookup regs t)))
          (if entry
              (let ((r (entry.regnum entry)))
                (if (car mask)
                    (begin (cgreg-release! regs r)
                           (cgframe-release! frame t)))
                (finish-loop disjoint
                             (cdr temps)
                             (cdr mask)
                             (cons r registers)))
              (let ((r (car disjoint)))
                (if (memv r registers)
                    (finish-loop (cdr disjoint) temps mask registers)
                    (begin (gen-load! output frame r t)
                           (cgreg-bind! regs r t)
                           (if (car mask)
                               (begin (cgreg-release! regs r)
                                      (cgframe-release! frame t)))
                           (finish-loop disjoint
                                        (cdr temps)
                                        (cdr mask)
                                        (cons r registers)))))))))
  
  (if (< (length args) *nregs*)
      (eval-loop (cdr args) '() '())
      (twobit-bug "cg-primop-args" args)))


; Parallel assignment.

; Given a list of target registers, a list of expressions, and a
; compile-time environment, generates code to evaluate the expressions
; into the registers.
;
; Argument evaluation proceeds as follows:
;
; 1.  Evaluate all but one of the complicated arguments.
; 2.  Evaluate remaining arguments.
; 3.  Load spilled arguments from stack.

(define (cg-arguments output targets args regs frame env)
  
  ; Sorts the args and their targets into complicated and
  ; uncomplicated args and targets.
  ; Then it calls evalargs.
  
  (define (sortargs targets args targets1 args1 targets2 args2)
    (if (null? args)
        (evalargs targets1 args1 targets2 args2)
        (let ((target (car targets))
              (arg (car args))
              (targets (cdr targets))
              (args (cdr args)))
          (if (complicated? arg env)
              (sortargs targets
                        args
                        (cons target targets1)
                        (cons arg args1)
                        targets2
                        args2)
              (sortargs targets
                        args
                        targets1
                        args1
                        (cons target targets2)
                        (cons arg args2))))))
  
  ; Given the complicated args1 and their targets1,
  ; and the uncomplicated args2 and their targets2,
  ; evaluates all the arguments into their target registers.
  
  (define (evalargs targets1 args1 targets2 args2)
    (let* ((temps1 (newtemps (length targets1)))
           (temps2 (newtemps (length targets2))))
      (if (not (null? args1))
          (for-each (lambda (arg temp)
                      (cg0 output arg 'result regs frame env #f)
                      (gen-setstk! output frame temp))
                    (cdr args1)
                    (cdr temps1)))
      (if (not (null? args1))
          (evalargs0 (cons (car targets1) targets2)
                     (cons (car args1) args2)
                     (cons (car temps1) temps2))
          (evalargs0 targets2 args2 temps2))
      (for-each (lambda (r t)
                  (let ((temp (cgreg-lookup-reg regs r)))
                    (if (not (eq? temp t))
                        (let ((entry (var-lookup t regs frame env)))
                          (case (entry.kind entry)
                            ((register)
                             (gen! output $movereg (entry.regnum entry) r))
                            ((frame)
                             (gen-load! output frame r t)))
                          (cgreg-bind! regs r t)))
                    (cgframe-release! frame t)))
                (append targets1 targets2)
                (append temps1 temps2))))
  
  (define (evalargs0 targets args temps)
    (if (not (null? targets))
        (let ((para (let* ((regvars (map (lambda (reg)
                                           (cgreg-lookup-reg regs reg))
                                         targets)))
                      (parallel-assignment targets
                                           (map cons regvars targets)
                                           args))))
          (if para
              (let ((targets para)
                    (args (cg-permute args targets para))
                    (temps (cg-permute temps targets para)))
                (for-each (lambda (arg r t)
                            (cg0 output arg r regs frame env #f)
                            (cgreg-bind! regs r t)
                            (gen-store! output frame r t))
                          args
                          para
                          temps))
              (let ((r (choose-register regs frame))
                    (t (car temps)))
                (cg0 output (car args) r regs frame env #f)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t)
                (evalargs0 (cdr targets)
                           (cdr args)
                           (cdr temps)))))))

  (if (parallel-assignment-optimization)
      (sortargs (reverse targets) (reverse args) '() '() '() '())
      (cg-evalargs output targets args regs frame env)))

; Left-to-right evaluation of arguments directly into targets.

(define (cg-evalargs output targets args regs frame env)
  (let ((temps (newtemps (length targets))))
    (for-each (lambda (arg r t)
                (cg0 output arg r regs frame env #f)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t))
              args
              targets
              temps)
    (for-each (lambda (r t)
                (let ((temp (cgreg-lookup-reg regs r)))
                  (if (not (eq? temp t))
                      (begin (gen-load! output frame r t)
                             (cgreg-bind! regs r t)))
                  (cgframe-release! frame t)))
              targets
              temps)))

; For heuristic use only.
; An expression is complicated unless it can probably be evaluated
; without saving and restoring any registers, even if it occurs in
; a non-tail position.

(define (complicated? exp env)
  (cond
   ((constant? exp)    #f)
   ((lambda? exp)   #f)
   ((assignment? exp)     (complicated? (assignment.rhs exp) env))
   ((conditional? exp)       (or (complicated? (if.test exp) env)
                                 (complicated? (if.then exp) env)
                                 (complicated? (if.else exp) env)))
   ((variable? exp) #f)
   ((begin? exp) (some? (lambda (exp)
                          (complicated? exp env))
                        (begin.exprs exp)))
   ((call? exp)       (let ((proc (call.proc exp)))
                        (if (and (variable? proc)
                                 (let ((entry
                                        (cgenv-lookup env (variable.name proc))))
                                   (eq? (entry.kind entry) 'integrable)))
                            (some? (lambda (exp)
                                     (complicated? exp env))
                                   (call.args exp))
                            #t)))
   (else (twobit-bug "unrecognized expression" exp))))

; Returns a permutation of the src list, permuted the same way the
; key list was permuted to obtain newkey.

(define (cg-permute src key newkey)
  (let ((alist (map cons key (iota (length key)))))
    (do ((newkey newkey (cdr newkey))
         (dest '()
               (cons (list-ref src (cdr (assq (car newkey) alist)))
                     dest)))
        ((null? newkey) (reverse dest)))))

; Given a list of register numbers,
; an association list with entries of the form (name . regnum) giving
; the variable names by which those registers are known in code,
; and a list of expressions giving new values for those registers,
; returns an ordering of the register assignments that implements a
; parallel assignment if one can be found, otherwise returns #f.

(define parallel-assignment
 (lambda (regnums alist exps)
   (if (null? regnums)
       #t
       (let ((x (toposort (dependency-graph regnums alist exps))))
         (if x (reverse x) #f)))))

(define dependency-graph
 (lambda (regnums alist exps)
   (let ((names (map car alist)))
     (do ((regnums regnums (cdr regnums))
          (exps exps (cdr exps))
          (l '() (cons (cons (car regnums)
                             (map (lambda (var) (cdr (assq var alist)))
                                  (intersection (freevariables (car exps))
                                                names)))
                       l)))
         ((null? regnums) l)))))

; Given a nonempty graph represented as a list of the form
;     ((node1 . <list of nodes that node1 is less than or equal to>)
;      (node2 . <list of nodes that node2 is less than or equal to>)
;      ...)
; returns a topological sort of the nodes if one can be found,
; otherwise returns #f.

(define toposort
 (lambda (graph)
   (cond ((null? (cdr graph)) (list (caar graph)))
         (else (toposort2 graph '())))))

(define toposort2
  (letrec ((remove (lambda (x l)
                     (cond ((not (pair? l)) l)
                           ((equal? x (car l)) (remove x (cdr l)))
                           (else (cons (car l) (remove x (cdr l))))))))
    (lambda (totry tried)
      (cond ((null? totry) #f)
            ((or (null? (cdr (car totry)))
                 (and (null? (cddr (car totry)))
                      (eq? (cadr (car totry))
                           (car (car totry)))))
             (if (and (null? (cdr totry)) (null? tried))
                 (list (caar totry))
                 (let* ((node (caar totry))
                        (x (toposort2 (map (lambda (y)
                                             (cons (car y) (remove node (cdr y))))
                                           (append (cdr totry) tried))
                                      '())))
                   (if x
                       (cons node x)
                       #f))))
            (else (toposort2 (cdr totry) (cons (car totry) tried)))))))

(define iota (lambda (n) (iota2 n '())))

(define iota1 (lambda (n) (cdr (iota2 (+ n 1) '()))))

(define iota2
 (lambda (n l)
   (if (zero? n)
       l
       (let ((n (- n 1)))
         (iota2 n (cons n l))))))

(define (freevariables exp)
  (freevars2 exp '()))

(define (freevars2 exp env)
  (cond ((symbol? exp)
         (if (memq exp env) '() (list exp)))
        ((not (pair? exp)) '())
        ((constant? exp) '())
        ((lambda? exp)
         (difference (lambda.F exp)
                     (make-null-terminated (lambda.args exp))))
        ((conditional? exp) (apply-union
                             (list (freevars2 (if.test exp) env)
                                   (freevars2 (if.then exp) env)
                                   (freevars2 (if.else exp) env))))
        ((assignment? exp) (apply-union
                            (list (freevars2 (assignment.lhs exp) env)
                                  (freevars2 (assignment.rhs exp) env))))
        ((variable? exp) (list (variable.name exp)))
        ((begin? exp) (apply-union
                       (map (lambda (subexp)
                              (freevars2 subexp env))
                            (begin.exprs exp))))
        ((call? exp) (apply-union
                      (cons (freevars2 (call.proc exp) env)
                            (map (lambda (arg)
                                   (freevars2 arg env))
                                 (call.args exp)))))
        (else (twobit-bug "unrecognized expression" exp))))

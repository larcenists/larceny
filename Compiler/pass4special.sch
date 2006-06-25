; Copyright 1999 William Clinger
;
; Code for special primitives, used to generate runtime safety checks,
; efficient code for call-with-values, and other weird things.
;
; 4 June 1999.

(define (cg-special output exp target regs frame env tail?)
  (let ((name (variable.name (call.proc exp))))
    (cond ((eq? name name:CHECK!)
           (if (runtime-safety-checking)
               (cg-check output exp target regs frame env tail?)))
          ((eq? name name:VALUES)
           (cg-values output exp target regs frame env tail?))
          ((eq? name name:CALL-WITH-VALUES)
           (cg-call-with-values output exp target regs frame env tail?))
          (else
           (error "Compiler bug: cg-special" (make-readable exp))))))

(define (cg-special-result output exp target regs frame env tail?)
  (let ((name (variable.name (call.proc exp))))
    (cond ((eq? name name:CHECK!)
           (if (runtime-safety-checking)
               (cg-check-result output exp target regs frame env tail?)))
          ((eq? name name:VALUES)
           (cg-values-result output exp target regs frame env tail?))
          ((eq? name name:CALL-WITH-VALUES)
           (cg-call-with-values-result output exp target regs frame env tail?))
          (else
           (error "Compiler bug: cg-special-result" (make-readable exp))))))

(define (cg-check output exp target regs frame env tail?)
  (cg0 output (car (call.args exp)) 'result regs frame env #f)
  (cg-check-result output exp target regs frame env tail?))

(define (cg-check-result output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (nargs (length args))
         (valexps (cddr args)))
    (if (and (<= 2 nargs 5)
             (constant? (cadr args))
             (every? (lambda (exp)
                       (or (constant? exp)
                           (variable? exp)))
                     valexps))
        (let* ((exn (constant.value (cadr args)))
               (vars (filter variable? valexps))
               (rs (cg-result-args output
                                   (cons (car args) vars)
                                   regs frame env)))
          
          ; Construct the trap situation:
          ; the exception number followed by an ordered list of
          ; register numbers and constant expressions.
          
          (let loop ((registers rs)
                     (exps valexps)
                     (operands '()))
            (cond ((null? exps)
                   (let* ((situation (cons exn (reverse operands)))
                          (ht (assembly-stream-info output))
                          (L1 (or (hashtable-get ht situation)
                                  (let ((L1 (make-label)))
                                    (hashtable-put! ht situation L1)
                                    L1))))
                     (define (translate r)
                       (if (number? r) r 0))
                     (case (length operands)
                       ((0) (gen! output $check 0 0 0 L1))
                       ((1) (gen! output $check
                                         (translate (car operands))
                                         0 0 L1))
                       ((2) (gen! output $check
                                         (translate (car operands))
                                         (translate (cadr operands))
                                         0 L1))
                       ((3) (gen! output $check
                                         (translate (car operands))
                                         (translate (cadr operands))
                                         (translate (caddr operands))
                                         L1)))))
                  ((constant? (car exps))
                   (loop registers
                         (cdr exps)
                         (cons (car exps) operands)))
                  (else
                   (loop (cdr registers)
                         (cdr exps)
                         (cons (car registers) operands))))))
        (error "Compiler bug: runtime check" (make-readable exp)))))

; Given an assembly stream and the description of a trap as recorded
; by cg-check above, generates a non-continuable trap at that label for
; that trap, passing the operands to the exception handler.

(define (cg-trap output situation L1)
  (let* ((exn (car situation))
         (operands (cdr situation)))
    (gen! output $.label L1)
    (let ((liveregs (filter number? operands)))
      (define (loop operands registers r)
        (cond ((null? operands)
               (case (length registers)
                 ((0) (gen! output $trap 0 0 0 exn))
                 ((1) (gen! output $trap (car registers) 0 0 exn))
                 ((2) (gen! output $trap
                                   (car registers)
                                   (cadr registers)
                                   0
                                   exn))
                 ((3) (gen! output $trap
                                   (car registers)
                                   (cadr registers)
                                   (caddr registers)
                                   exn))
                 (else "Compiler bug: trap")))
              ((number? (car operands))
               (loop (cdr operands)
                     (cons (car operands) registers)
                     r))
              ((memv r liveregs)
               (loop operands registers (+ r 1)))
              (else
               (gen! output $const (constant.value (car operands)))
               (gen! output $setreg r)
               (loop (cdr operands)
                     (cons r registers)
                     (+ r 1)))))
      (loop (reverse operands) '() 1))))

; Given a short list of expressions that can be evaluated in any order,
; evaluates the first into the result register and the others into any
; register, and returns an ordered list of the registers that contain
; the arguments that follow the first.
; The number of expressions must be less than the number of argument
; registers.

; FIXME: No longer used.

(define (cg-check-args output args regs frame env)
  
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
      (error "Bug detected by cg-primop-args" args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Multiple values.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an encoding for the number of values that are accepted
; by a return point, generates code for the return point.
; The appropriate encodings for cont are
;
; #f -- the usual return point, which uses the result and
;         discards other values
; (,$args= n)  -- the return point requires exactly n values
;                 in registers 1 through n
; (,$args>= n) -- the return point requires n or more values
;                 in registers 1 through n
;
; r0 is the name of the stack slot from which to restore reg0.

(define (cg-return-point output cont regs frame r0)
  (cond ((not cont)
         (gen! output $.cont)
         (cgreg-clear! regs)
         (cgreg-bind! regs 0 r0)
         (gen-load! output frame 0 r0))
        ((eq? (car cont) $args=)
         (let ((n (cadr cont)))
           (gen! output $.cont)
           (cgreg-clear! regs)
           (cgreg-bind! regs 0 r0)
           (if (= n 1)
               (let ((L (make-label)))
                 (gen!      output $setreg 1)
                 (gen!      output $reg 0)
                 (gen-load! output frame 0 r0)
                 (gen!      output $op1 op:FIXNUM?)
                 (gen!      output $branchf L (cgreg-tos regs))
                 (gen!      output $const 0)      ; exception
                 (gen!      output $args= 1)
                 (gen!      output $.label L))
               (begin (gen!      output $setreg 1)
                      (gen!      output $reg 0)
                      (gen-load! output frame 0 r0)
                      (gen!      output $args= n)))))
        ((eq? (car cont) $args>=)
         (let ((n (cadr cont)))
           (gen! output $.cont)
           (cgreg-clear! regs)
           (cgreg-bind! regs 0 r0)
           (if (> n 1)
               (begin (gen!      output $setreg 1)
                      (gen!      output $reg 0)
                      (gen-load! output frame 0 r0)
                      (gen!      output $args>= n))
               (let ((t (newtemp))
                     (L (make-label)))
                 (gen-store!  output frame 0 t)
                 (gen-load!   output frame 0 r0)
                 (gen-stack!  output frame t)
                 (gen!        output $op1 op:FIXNUM?)
                 (gen!        output $branchf L (cgreg-tos regs))
                 (gen!        output $const 1)
                 (gen-setstk! output frame t)
                 (gen!        output $.label L)
                 (gen-stack!  output frame t)
                 (cgframe-release! frame t)
                 (gen! output $args>= n)))))
        (else
         (error "Compiler bug: cg-return-point" cont))))

; Given a call to values, generates optimized code for it.

(define (cg-values output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (n (length args)))
    (if (= n 1)
        (cg0 output (car args) target regs frame env tail?)
        (begin (cg-arguments output (iota1 n) args regs frame env)
               (cg-values-part2 output exp target regs frame env tail?)))))

; Given a call to values whose arguments have already been
; evaluated into registers 1 through n, generates optimized
; code for the call to values.

(define (cg-values-part2 output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (n (length args)))
    (cond ((not tail?)
           (if (= n 0)
               (begin (gen! output $op1 op:UNSPECIFIED)
                      (cg-move output frame regs 'result target))
               (cg-move output frame regs 1 target)))
          ((= n 1)
           (gen! output $reg 1)
           (gen-pop! output frame)
           (gen! output $return)
           'result)
          ((= n 0)
           (gen-pop! output frame)
           (gen! output $op1 op:UNSPECIFIED)
           (gen! output $setreg 1)
           (gen! output $const 0)
           (gen! output $setreg 0)
           (gen! output $reg 1)
           (gen! output $mvrtn)
           'result)
          (else
           (gen-pop! output frame)
           (gen! output $const n)
           (gen! output $setreg 0)
           (gen! output $reg 1)
           (gen! output $mvrtn)
           'result))))

; Given a call to values whose arguments consist entirely of
; constants and variables, and whose first argument has already
; been evaluated into the result register, generates optimized
; code for it.  (This is just a hack forced upon us by let
; optimization of A-normal forms.)

(define (cg-values-result output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (n (length args))
         (arg1 (car args))
         (t (newtemp)))
    (gen! output $setreg 1)
    (cgreg-bind! regs 1 t)
    (gen-store! output frame 1 t)
    (cg-arguments output (cdr (iota1 n)) (cdr args) regs frame env)
    (let ((entry (var-lookup t regs frame env)))
      (case (entry.kind entry)
        ((register)
         (let ((r (entry.regnum entry)))
           (if (not (= r 1))
               (gen! output $movereg r 1))))
        ((frame)
         (gen-load! output frame 1 t)))
      (cgreg-bind! regs 1 t)))
  (cg-values-part2 output exp target regs frame env tail?))

; Given an arbitrary expression, probably in A-normal form,
; return #t if and only it will end with a unique closed call.

(define (closed-call? exp regs frame env)
  (cond ((and #f (begin? exp))
         ; FIXME: This case is disabled because it would make
         ; simplify-call-with-values more complicated.
         ; This case cannot arise with A-normal form anyway.
         (closed-call? (car (reverse (begin.exprs exp))) regs frame env))
        ((call? exp)
         (let ((proc (call.proc exp)))
           (cond ((lambda? proc)
                  (closed-call? (lambda.body proc) regs frame env))
                 ((variable? proc)
                  (let ((entry
                         (var-lookup (variable.name proc)
                                     regs frame env)))
                    (case (entry.kind entry)
                      ((global lexical frame register procedure)
                       #t)
                      (else #f))))
                 (else #f))))
        (else #f)))

; Given a call to call-with-values whose thunk's body satisfies
; the closed-call? predicate, simplify-call-with-values
; returns an equivalent expression in which the thunk's body is
; a closed call with no intervening let expressions.
; This is accomplished by repeated application of the following
; transform:
;
;    (call-with-values
;     (lambda () ((lambda (x1 ... xn) E0) E1 ... En))
;     RCVR)
; -> ((lambda (x1 ... xn)
;       (call-with-values
;        (lambda () E0)
;        RCVR))
;     E1 ... En)

(define (simplify-call-with-values exp)
  (let* ((thunk (car (call.args exp)))
         (body (lambda.body thunk)))
    (if (and (call? body) (not (lambda? (call.proc body))))
        exp
        (let* ((Lx1...xn (call.proc body))
               (E1...En (call.args body))
               (E0 (lambda.body Lx1...xn)))
          (lambda.body-set! thunk E0)
          (lambda.body-set! Lx1...xn (simplify-call-with-values exp))
          body))))

; Given a let expression that binds a single variable,
; returns true if and only if it is of one of these forms:
;
; (let* ((x (lambda () (f ...)))    ; or something that ends in a closed call
;        (y (lambda (...) ...))
;        (z (call-with-values x y)))
;   ...)
;
; (let* ((x (lambda (...) ...))
;        (y (lambda () (f ...)))    ; or something that ends in a closed call
;        (z (call-with-values y x)))
;   ...)
;

(define (cg-let1-call-with-values? exp regs frame env)
  (let* ((proc (call.proc exp))
         (x (car (lambda.args proc)))
         (xexpr (car (call.args exp)))
         (body (lambda.body proc)))
    (and (lambda? xexpr)
         (null? (lambda.defs xexpr))
         (call? body)
         (integrate-procedures)
         (let ((bodyproc (call.proc body))
               (bodyargs (call.args body)))
           (and (lambda? bodyproc)
                (null? (lambda.defs bodyproc))
                (not (null? bodyargs))
                (null? (cdr bodyargs))
                (pair? (lambda.args bodyproc))
                (null? (cdr (lambda.args bodyproc)))
                (let* ((y (car (lambda.args bodyproc)))
                       (yexpr (car bodyargs))
                       (body (lambda.body bodyproc)))
                  (and (lambda? yexpr)
                       (null? (lambda.defs yexpr))
                       (call? body)
                       (let ((bodyproc (call.proc body))
                             (bodyargs (call.args body)))
                         (and (variable? bodyproc)
                              (eq? (variable.name bodyproc)
                                   name:CALL-WITH-VALUES)
                              (pair? bodyargs)
                              (pair? (cdr bodyargs))
                              (null? (cdr (cdr bodyargs)))
                              (variable? (car bodyargs))
                              (variable? (cadr bodyargs))
                              (let* ((arg1 (car bodyargs))
                                     (arg2 (cadr bodyargs))
                                     (id1 (variable.name arg1))
                                     (id2 (variable.name arg2)))
                                (or (and
                                     (eq? x id1)
                                     (eq? y id2)
                                     (null? (lambda.args xexpr))
                                     (closed-call? (lambda.body xexpr)
                                                   regs frame env))
                                    (and
                                     (eq? y id1)
                                     (eq? x id2)
                                     (null? (lambda.args yexpr))
                                     (closed-call? (lambda.body yexpr)
                                                   regs frame env)))))))))))))


; Generates code for let expressions of these two forms:
;
; (let* ((x (lambda () (f ...)))
;        (y (lambda (...) ...))
;        (z (call-with-values x y)))
;   ...)
;
; (let* ((x (lambda (...) ...))
;        (y (lambda () (f ...)))
;        (z (call-with-values y x)))
;   ...)

(define (cg-let1-call-with-values output exp target regs frame env tail?)
  (let* ((x (car (lambda.args (call.proc exp))))
         (xexpr (car (call.args exp)))
         (exp (lambda.body (call.proc exp)))
         (y (car (lambda.args (call.proc exp))))
         (yexpr (car (call.args exp)))
         (exp (lambda.body (call.proc exp)))
         (proc (call.proc exp))
         (args (call.args exp))
         (arg1 (car args))
         (arg2 (cadr args))
         (cwv (make-call proc
                         (if (eq? x (variable.name arg1))
                             (list xexpr yexpr)
                             (list yexpr xexpr))))
         (cwv (simplify-call-with-values cwv)))
    (if (eq? proc (call.proc cwv))
        (cg-call-with-values output cwv target regs frame env tail?)
        (cg0 output cwv target regs frame env tail?))))

; Generates code for procedure calls of the form
;
; (call-with-values (lambda () (f ...))
;                   (lambda (...) ...))

(define (cg-call-with-values output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (thunk (car args))
         (rcvr (cadr args)))
    (if (and #f (lambda? thunk) (lambda? rcvr)) ;FIXME
        (let* ((E1 (lambda.body thunk))
               (E2 (lambda.body rcvr))
               (vars (lambda.args rcvr))
               (varslist (make-null-terminated vars))
               (n (length varslist))
               (r0 (cgreg-lookup-reg regs 0)))
          (if (and (null? (lambda.args thunk))
                   (null? (lambda.defs thunk))
                   (null? (lambda.defs rcvr))
                   (< n *lastreg*) ; don't mess with complex cases
                   (call? E1)
                   (variable? (call.proc E1))
                   (closed-call? E1 regs frame env))
              (cond ((list? vars)
                     (cond ((= n 1)
                            (cg-let output
                                    (make-call rcvr (list E1))
                                    target regs frame env tail?))
                           (else
                            ; Must compile this as a closed call.
                            (cg-call/cont output E1 'result
                                          regs frame env #f
                                          (list $args= n))
                            (do ((vars vars (cdr vars))
                                 (r 1 (+ r 1)))
                                ((null? vars))
                              (cgreg-bind! regs r (car vars)))
                            (cg0 output E2 target regs frame env tail?))))
                    (else
                     (let ((vars varslist)
                           (n (- n 1)))
                       ; Must compile this as a closed call.
                       (cg-call/cont output E1 'result
                                     regs frame env #f
                                     (list $args>= n))
                       (do ((vars vars (cdr vars))
                            (r 1 (+ r 1)))
                           ((null? vars))
                         (cgreg-bind! regs r (car vars)))
                       (cg0 output E2 target regs frame env tail?))))
              (cg-unknown-call output exp target regs frame env tail? #f)))
    (cg-unknown-call output exp target regs frame env tail? #f))))

; Given a call to call-with-values whose arguments consist entirely
; of constants and variables, and whose first argument has already
; been evaluated into the result register, generates code for it.
; (This is just a hack forced upon us by let optimization of
; A-normal forms.)

(define (cg-call-with-values-result output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (arg1 (car args))
         (t (newtemp)))
    (cond ((constant? arg1)
           (cg0 output exp target regs frame env tail?))
          ((variable? arg1)
           (let ((v (variable.name arg1)))
             (gen! output $setreg 1)
             (cgreg-bind! regs 1 v)
             (gen-store! output frame 1 v)
             (cg0 output exp target regs frame env tail?)))
          (else
           (error "Compiler bug: call-with-values-result")))))


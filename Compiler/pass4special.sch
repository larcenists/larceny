; Copyright 1999 William Clinger
;
; Code for special primitives, used to generate runtime safety checks,
; efficient code for call-with-values, and other weird things.
;
; 24 April 1999.

(define (cg-special output exp target regs frame env tail?)
  (let ((name (variable.name (call.proc exp))))
    (cond ((eq? name name:CHECK!)
           (if (runtime-safety-checking)
               (cg-check output exp target regs frame env tail?)))
          (else
           (error "Compiler bug: cg-special" (make-readable exp))))))

(define (cg-check output exp target regs frame env tail?)
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
               (rs (cg-check-args output
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

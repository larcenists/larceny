; Miscellaneous trivial patches.

; FIXME:  What about lambda.doc ?

; Modifies sparc.imp.sch

; The number of argument registers that are represented by hardware
; registers.

(define *nhwregs* 8)

; Variable names that indicate register targets.

(define *regnames*
  (do ((alist '() (cons (cons (string->symbol
                               (string-append ".REG" (number->string r)))
                              r)
                        alist))
       (r (- *nhwregs* 1) (- r 1)))
      ((<= r 0)
       alist)))

; Improves the code generated for calls to LIST.

(define-inline 'list
  (lambda (exp env)
    (cond ((null? (cdr exp)) (make-constant '()))
          ((null? (cddr exp))
           (make-call (make-variable 'cons)
                      (list (m-scan (cadr exp) env)
                            (make-constant '()))))
          (else (make-call (make-variable 'list)
                           (map (lambda (x)
                                  (m-scan x env))
                                (cdr exp)))))))

; More miscellaneous.

(define $car:pair 'car)
(define $cdr:pair 'cdr)

; Modifies Asm/Sparc/pass5p2.sch
; Cosmetic patch to listify? output.

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (emit-constant->register as (operand2 instruction) $r.argreg2)
    (emit-primop.2arg! as
		       (operand1 instruction)
		       $r.argreg2)))

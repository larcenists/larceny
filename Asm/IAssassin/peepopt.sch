; Copyright 1998 Lars T Hansen.
;
; $Id: peepopt.sch 2543 2005-07-20 21:54:03Z pnkfelix $
;
; 12 December 2003.
;
; Asm/Intel/peepopt.sch -- MAL peephole optimizer, for the Intel assembler.
;
; The procedure `peep' is called on the as structure before every
; instruction is assembled.  It may replace the prefix of the instruction
; stream by some other instruction sequence.
;
; Invariant: if the peephole optimizer doesn't change anything, then 
;
;  (let ((x (as-source as)))
;    (peep as)
;    (eq? x (as-source as)))     => #t
;
; Note this still isn't right -- it should be integrated with pass5p2 --
; but it's a step in the right direction.

(define *peephole-table* (make-vector *number-of-mnemonics* #f))

(define (define-peephole n p)
  (vector-set! *peephole-table* n p)
  (unspecified))

(define (peep as)
  (let ((t0 (as-source as)))
    (if (not (null? t0))
        (let ((i1 (car t0)))
          (let ((p (vector-ref *peephole-table* (car i1))))
            (if p
                (let* ((t1 (if (null? t0) t0 (cdr t0)))
                       (i2 (if (null? t1) '(-1 0 0 0) (car t1)))
                       (t2 (if (null? t1) t1 (cdr t1)))
                       (i3 (if (null? t2) '(-1 0 0 0) (car t2)))
                       (t3 (if (null? t2) t2 (cdr t2))))
                  (p as i1 i2 i3 t1 t2 t3))))))))

(define-peephole $reg
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (reg-setreg as i1 i2 t2))
          ((= (car i2) $setglbl)
           (reg-setglbl as i1 i2 t2))
          ((= (car i2) $op1)
           (cond ((= (car i3) $setreg)
                  (reg-op1-setreg as i1 i2 i3 t2 t3))
                 ((= (car i3) $branchf)
                  (reg-op1-branchf as i1 i2 i3 t3))
                 (else
                  (reg-op1 as i1 i2 t2))))
          ((= (car i2) $op2)
           (cond ((= (car i3) $setreg)
                  (reg-op2-setreg as i1 i2 i3 t2 t3))
                 (else 
                  (reg-op2 as i1 i2 t2))))
          )))

(define-peephole $op1
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (cond ((not (= (car i3) $store))
                  ;; avoid interference w/ setreg-store optimization
                  (op1-setreg as i1 i2 t2))))
          ((= (car i2) $branchf)
           (op1-branchf as i1 i2 t2))
          )))

(define-peephole $op2
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (cond ((not (= (car i3) $store))
                  ;; avoid interference w/ setreg-store optimization
                  (op2-setreg as i1 i2 t2)))))))

(define-peephole $setreg
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $reg)
	   (setreg-reg as i1 i2 t2))
	  ((= (car i2) $store)
	   (setreg-store as i1 i2 t2)))))

(define-peephole $global
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (global-setreg as i1 i2 t2))
          ((= (car i2) $invoke)
	   (as-source! as (cons (list $global/invoke (cadr i1) (cadr i2))
				t2))))))

(define-peephole $const
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (const-setreg as i1 i2 t2))
          )))

(define-peephole $setrtn
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branch)
           (cond ((= (car i3) $.align) 
                  (if (not (null? t3)) 
                      (let ((i4 (car t3)) 
                            (t4 (cdr t3))) 
                        (cond ((= (car i4) $.label) 
                               (setrtn-branch as i1 i2 i3 i4 t4))))))))
          ((= (car i2) $jump)
           (cond ((= (car i3) $.align)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $.label)
                               (setrtn-jump   as i1 i2 i3 i4 t4))))))))
          ((= (car i2) $invoke)
           (cond ((= (car i3) $.align)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $.label)
                               (setrtn-invoke as i1 i2 i3 i4 t4)))))))))))

(define-peephole $branch
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $.align)
           (cond ((= (car i3) $.label)
                  (branch-and-label as i1 i2 i3 t3)))))))

; Reg-setreg is not restricted to hardware registers, as $movereg is 
; a standard instruction.

(define (reg-setreg as i:reg i:setreg tail)
  (let ((rs (operand1 i:reg))
        (rd (operand1 i:setreg)))
    (if (= rs rd)
        (as-source! as tail)
        (as-source! as (cons (list $movereg rs rd) tail)))))

(define (reg-op1-setreg as i:reg i:op1 i:setreg tail-1 tail) 
  (let ((rs (operand1 i:reg))
        (rd (operand1 i:setreg)))
    (cond 
     ((and (is_hwreg rs) (is_hwreg rd))
      (peep-reg/op1/setreg as (operand1 i:op1) rs rd tail))
     (else
      (reg-op1 as i:reg i:op1 tail-1)))))

(define (reg-op1 as i:reg i:op1 tail)
  (let ((rs (operand1 i:reg)))
    (cond
     ((is_hwreg rs)
      (peep-reg/op1/setreg as (operand1 i:op1) rs 'RESULT tail)))))

; Optimize
;   setreg n
;   reg n
; on the assumption that setreg does not destroy RESULT

(define (setreg-reg as i:setreg i:reg tail)
  (if (= (operand1 i:setreg) (operand1 i:reg))
      (as-source! as (cons i:setreg tail))))

; Ditto for
;   setreg n
;   store n, k

(define (setreg-store as i:setreg i:store tail)
  (if (= (operand1 i:setreg) (operand1 i:store))
      (as-source! as (cons i:setreg
			   (cons (list $setstk (operand2 i:store))
				 tail)))))

; Gets rid of spurious branch-to-next-instruction
;    (branch Lx k)
;    (.align y)
;    (.label Lx)
; => (.align y)
;    (.label Lx)

(define (branch-and-label as i:branch i:align i:label tail)
  (let ((branch-label (operand1 i:branch))
        (label        (operand1 i:label)))
    (if (= branch-label label)
        (as-source! as (cons i:align (cons i:label tail))))))

(define (setrtn-invoke as i:setrtn i:invoke i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (invoke-ops   (operand1 i:invoke))
        (label        (operand1 i:label)))
    (if (and ;; Very experimental; leaving spot to ease disabling
             (= return-label label))
        (as-source! as (cons (list $setrtn/invoke invoke-ops)
                             (cons i:align
                                   (cons i:label
                                         tail)))))))

; This allows the use of hardware 'call' instructions.
;    (setrtn Lx)
;    (branch Ly k)
;    (.align k)            Note alignment enforced by setrtn/branch
;    (.label Lx)
; => (setrtn/branch Ly k)
;    (.label Lx)

(define (setrtn-branch as i:setrtn i:branch i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (branch-ops   (cdr i:branch))
        (label        (operand1 i:label)))
    (if (= return-label label)
        (as-source! as (cons (cons $setrtn/branch branch-ops)
                             (cons i:label
                                   tail))))))

(define (setrtn-jump as i:setrtn i:jump i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (jump-ops   (cdr i:jump))
        (label        (operand1 i:label)))
    (cond ((= return-label label)
           (as-source! as (cons (cons $setrtn/jump jump-ops)
                                (cons i:label
                                      tail)))))))

(define (op1-setreg as i:op1 i:setreg tail)
  (let ((op (operand1 i:op1))
        (rd (operand1 i:setreg)))
    (if (is_hwreg rd)
        (peep-reg/op1/setreg as op 'RESULT rd tail))))

(define (peep-reg/op1/setreg as op rs rd tail)
  (define (the-reg n)
    (if (eq? n 'RESULT) 'RESULT (REG n)))
  (let ((op (case op
              ((unspecified 
                undefined
                eof-object
                pair?
                eof-object?
                car
                cdr
                car:pair
                cdr:pair
                not
                null?
                procedure?
                set-car!
                set-cdr!
                bytevector-like?
                vector-like?) op)
              (else #f))))
    (cond (op
           (as-source! as (cons (list $reg/op1/setreg op 
                                      (the-reg rs) (the-reg rd)) tail))))))

(define (global-setreg as i:global i:setreg tail)
  (let ((global (operand1 i:global))
        (rd     (operand1 i:setreg)))
    (cond ((is_hwreg rd)
           (as-source! as (cons (list $global/setreg global (REG rd)) tail))))))

(define (reg-setglbl as i:reg i:setglbl tail)
  (let ((rs     (operand1 i:reg))
        (global (operand1 i:setglbl)))
    (cond ((is_hwreg rs)
           (as-source! as (cons (list $reg/setglbl rs global) tail))))))

(define (reg-op2-setreg as i:reg i:op2 i:setreg tail-1 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op (operand1 i:op2))
        (rd (operand1 i:setreg)))
    (if (is_hwreg rs1)
        (if (is_hwreg rd)
            (peep-reg/op2/setreg as op rs1 rs2 rd tail)
            (peep-reg/op2/setreg as op rs1 rs2 'RESULT tail-1)))))

(define (reg-op2 as i:reg i:op2 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op  (operand1 i:op2)))
    (if (is_hwreg rs1)
        (peep-reg/op2/setreg as op rs1 rs2 'RESULT tail))))

(define (op2-setreg as i:op2 i:setreg tail)
  (let ((op  (operand1 i:op2))
        (rs2 (operand2 i:op2))
        (rd  (operand1 i:setreg)))
    (if (is_hwreg rd)
        (peep-reg/op2/setreg as op 'RESULT rs2 rd tail))))

(define (peep-reg/op2/setreg as op rs1 rs2 rd tail)
  (let ((op (case op
              ((eq? 
                set-car! 
                set-cdr!
                cons
                ) op)
              (else #f))))
    (cond (op
           (as-source! as (cons (list $reg/op2/setreg op rs1 rs2 rd) tail))))))

(define (reg-op1-branchf as i:reg i:op1 i:branchf tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1))
        (L  (operand1 i:branchf)))
    (if (is_hwreg rs)
        (peep-reg/op1/branchf as op rs L tail))))

(define (op1-branchf as i:op1 i:branchf tail)
  (let ((op (operand1 i:op1))
        (L  (operand1 i:branchf)))
    (peep-reg/op1/branchf as op 'RESULT L tail)))

(define (peep-reg/op1/branchf as op rs L tail)
  (let ((op (case op
              ((null?)       'internal:branchf-null?)
              ((eof-object?) 'internal:branchf-eof-object?)
              ((pair?)       'internal:branchf-pair?)
              ;;((fixnum?)     'internal:branchf-fixnum?)
              ;;((char?)       'internal:branchf-char?)
              ;;((fxzero?)     'internal:branchf-fxzero?)
              ;;((fxnegative?) 'internal:branchf-fxnegative?)
              ;;((fxpositive?) 'internal:branchf-fxpositive?)
              (else #f))))
    (cond (op
           (as-source! as (cons (list $reg/op1/branchf op rs L) tail))))))

(define (const-setreg as i:const i:setreg tail)
  (let ((c  (operand1 i:const))
        (rd (operand1 i:setreg)))
    (if (is_hwreg rd)
        (as-source! as (cons (list $const/setreg c rd) tail)))))

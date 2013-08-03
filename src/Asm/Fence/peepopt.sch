; Copyright 1998 Lars T Hansen.
;
; 25 July 2012
;
; Asm/Fence/peepopt.sch -- MAL peephole optimizer, for the Fence assembler.
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

; TODO
; Probably desirable:
;
;  - reg / op1 bytevector-like? / check [ already have the bytevector? case ]
;  - support bytevector-like? everywhere we support bytevector?
;  - support vector-like? everywhere we support vector?
;  - ditto for -ref, -set!, -length
;
; On Sparc but not here yet, probably desirable:
;
;  - const-op2                       (expands make-vector on known short lengths)
;
; On Sparc but not here yet, unknown whether they're wanted:
;
;  - reg-op1-check-reg-op1-setreg    (special case for vector-length and string-length)
;  - reg-op2-check-reg-op2imm-check  (folds into an unsigned comparison)
;
; On Sparc, not appropriate for Fence:
;
;  - reg-return                      (fills branch delay slot)
;  - const-return                    (fills branch delay slot)
;
; Also the Sparc version handles more primitives in some cases:
;
;  - char=?, char<?, etc for reg/op2/branchf, reg/op2imm/branchf
;  - --, fx--, fxpositive?, fxnegative?, fxzero? for reg/op1/setreg and reg/op1/branchf

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
                 ((= (car i3) $check)
                  (reg-op1-check as i1 i2 i3 t3))
                 (else
                  (reg-op1 as i1 i2 t2))))
          ((= (car i2) $op2)
           (cond ((= (car i3) $setreg)
                  (reg-op2-setreg as i1 i2 i3 t2 t3))
                 ((= (car i3) $branchf)
                  (reg-op2-branchf as i1 i2 i3 t3))
                 ((= (car i3) $check)
                  (reg-op2-check as i1 i2 i3 t3))
                 (else 
                  (reg-op2 as i1 i2 t2))))
          ((= (car i2) $op2imm)
           (cond ((= (car i3) $setreg)
                  (reg-op2imm-setreg as i1 i2 i3 t2 t3))
                 ((= (car i3) $branchf)
                  (reg-op2imm-branchf as i1 i2 i3 t3))
                 ((= (car i3) $check)
                  (reg-op2imm-check as i1 i2 i3 t3))
                 (else 
                  (reg-op2imm as i1 i2 t2))))
          ((= (car i2) $op3)
           (reg-op3 as i1 i2 t2))
          ((= (car i2) $branchf)
           (reg-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (reg-check as i1 i2 t2))
          )))

(define-peephole $op1
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (cond ((not (= (car i3) $store))
                  ;; avoid interference w/ setreg-store optimization
                  (op1-setreg as i1 i2 t2))))
          ((= (car i2) $branchf)
           (op1-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (op1-check as i1 i2 t2))
          )))

(define-peephole $op2
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (cond ((not (= (car i3) $store))
                  ;; avoid interference w/ setreg-store optimization
                  (op2-setreg as i1 i2 t2))))
          ((= (car i2) $branchf)
           (op2-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (op2-check as i1 i2 t2))
          )))

(define-peephole $op2imm
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (op2imm-setreg as i1 i2 t2))
          ((= (car i2) $branchf)
           (op2imm-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (op2imm-check as i1 i2 t2))
          )))

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
          ((= (car i2) $setrtn)
           (cond ((= (car i3) $invoke)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $.align)
                               (if (not (null? t4))
                                   (let ((i5 (car t4))
                                         (t5 (cdr t4)))
                                     (cond ((= (car i5) $.label)
                                            (global-setrtn-invoke as i1 i2 i3 i4 i5 t5))))))))))))
          ((= (car i2) $invoke)
           (as-source! as (cons (list $global/invoke (cadr i1) (cadr i2))
                                t2)))
          )))

(define-peephole $const
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (const-setreg as i1 i2 t2))
          ((= (car i2) $setglbl)
           (const-setglbl as i1 i2 t2))
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

(define-peephole $save
  (lambda (as i1 i2 i3 t1 t2 t3)
    (let loop ((instrs t1)
               (rev-stores '()))
      (if (eqv? $store (operand0 (car instrs)))
          (loop (cdr instrs) 
                (cons (car instrs) rev-stores))
          (if (not (null? rev-stores))
              (save-and-stores as i1 (reverse rev-stores) instrs))))))

(define-peephole $stack
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $op1)
           (cond ((= (car i3) $load)
                  (stack-op1-load as i1 i2 i3 t3))))
          ((= (car i2) $setreg)
           (stack-setreg as i1 i2 t2))
          )))

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
    (peep-reg/op1/setreg as (operand1 i:op1) rs rd tail)))

(define (reg-op1 as i:reg i:op1 tail)
  (let ((rs (operand1 i:reg)))
    (peep-reg/op1/setreg as (operand1 i:op1) rs $r.result tail)))

(define (reg-branchf as i:reg i:branchf tail)
  (let ((rs (operand1 i:reg))
        (l  (operand1 i:branchf)))
    (as-source! as (cons (list $reg/branchf rs l) tail))))

(define (reg-check as i:reg i:check tail)
  (let ((rs (operand1 i:reg))
        (l  (operand4 i:check))
        (liveregs (list (operand1 i:check)
                        (operand2 i:check)
                        (operand3 i:check))))
    (as-source! as (cons (list $reg/check rs l liveregs) tail))))

; Optimize
;   setreg n
;   reg n
; on the assumption that setreg does not destroy RESULT.

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
;    (branch lx k)
;    (.align y)
;    (.label lx)
; => (.align y)
;    (.label lx)

(define (branch-and-label as i:branch i:align i:label tail)
  (let ((branch-label (operand1 i:branch))
        (label        (operand1 i:label)))
    (if (= branch-label label)
        (as-source! as (cons i:align (cons i:label tail))))))

(define (setrtn-invoke as i:setrtn i:invoke i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (invoke-arg   (operand1 i:invoke))
        (label        (operand1 i:label))
        (alignment    (operand1 i:align)))
    (if (= return-label label)
        (as-source! as (cons (list $setrtn/invoke alignment invoke-arg)
                             (cons i:align
                                   (cons i:label
                                         tail)))))))

(define (global-setrtn-invoke as i:global i:setrtn i:invoke i:align i:label tail)
  (let ((global-name  (operand1 i:global))
        (return-label (operand1 i:setrtn))
        (invoke-arg   (operand1 i:invoke))
        (label        (operand1 i:label))
        (alignment    (operand1 i:align)))
    (if (= return-label label)
        (as-source! as (cons (list $global/setrtn/invoke global-name alignment invoke-arg)
                             (cons i:align
                                   (cons i:label
                                         tail)))))))

; This allows the use of hardware 'call' instructions.
;    (setrtn lx)
;    (branch ly k)
;    (.align a)            Note alignment enforced by setrtn/branch
;    (.label lx)
; => (setrtn/branch a ly k)
;    (.label lx)

(define (setrtn-branch as i:setrtn i:branch i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (branch-ops   (cdr i:branch))
        (label        (operand1 i:label))
        (alignment    (operand1 i:align)))
    (if (= return-label label)
        (as-source! as (cons (cons $setrtn/branch (cons alignment branch-ops))
                             (cons i:label
                                   tail))))))

(define (setrtn-jump as i:setrtn i:jump i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (jump-ops     (cdr i:jump))
        (label        (operand1 i:label))
        (alignment    (operand1 i:align)))
    (cond ((= return-label label)
           (as-source! as (cons (cons $setrtn/jump (cons alignment jump-ops))
                                (cons i:label
                                      tail)))))))

(define (op1-setreg as i:op1 i:setreg tail)
  (let ((op (operand1 i:op1))
        (rd (operand1 i:setreg)))
    (peep-reg/op1/setreg as op $r.result rd tail)))

(define (peep-reg/op1/setreg as op rs rd tail)
  (as-source! as (cons (list $reg/op1/setreg op rs rd) tail)))

(define (global-setreg as i:global i:setreg tail)
  (let ((global (operand1 i:global))
        (rd     (operand1 i:setreg)))
    (as-source! as (cons (list $global/setreg global rd) tail))))

(define (reg-setglbl as i:reg i:setglbl tail)
  (let ((rs     (operand1 i:reg))
        (global (operand1 i:setglbl)))
    (as-source! as (cons (list $reg/setglbl rs global) tail))))

(define (reg-op2-setreg as i:reg i:op2 i:setreg tail-1 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op (operand1 i:op2))
        (rd (operand1 i:setreg)))
    (peep-reg/op2/setreg as op rs1 rs2 rd tail)))

(define (reg-op2imm-setreg as i:reg i:op2imm i:setreg tail-1 tail)
  (let ((rs  (operand1 i:reg))
        (imm (operand2 i:op2imm))
        (op  (operand1 i:op2imm))
        (rd  (operand1 i:setreg)))
    (peep-reg/op2imm/setreg as op rs imm rd tail)))

(define (reg-op2 as i:reg i:op2 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op  (operand1 i:op2)))
    (peep-reg/op2/setreg as op rs1 rs2 $r.result tail)))

(define (reg-op2imm as i:reg i:op2imm tail)
  (let ((rs  (operand1 i:reg))
        (imm (operand2 i:op2imm))
        (op  (operand1 i:op2imm)))
    (peep-reg/op2imm/setreg as op rs imm $r.result tail)))

(define (op2-setreg as i:op2 i:setreg tail)
  (let ((op  (operand1 i:op2))
        (rs2 (operand2 i:op2))
        (rd  (operand1 i:setreg)))
    (peep-reg/op2/setreg as op $r.result rs2 rd tail)))

(define (op2imm-setreg as i:op2imm i:setreg tail)
  (let ((op  (operand1 i:op2imm))
        (imm (operand2 i:op2imm))
        (rd  (operand1 i:setreg)))
    (peep-reg/op2imm/setreg as op $r.result imm rd tail)))

(define (peep-reg/op2/setreg as op rs1 rs2 rd tail)
  (as-source! as (cons (list $reg/op2/setreg op rs1 rs2 rd) tail)))

(define (peep-reg/op2imm/setreg as op rs imm rd tail)
  (as-source! as (cons (list $reg/op2imm/setreg op rs imm rd) tail)))

(define (reg-op3 as i:reg i:op3 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op3))
        (rs3 (operand3 i:op3))
        (op  (operand1 i:op3)))
    (as-source! as (cons (list $reg/op3 op rs1 rs2 rs3) tail))))

(define (reg-op1-branchf as i:reg i:op1 i:branchf tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1))
        (l  (operand1 i:branchf)))
    (peep-reg/op1/branchf as op rs l tail)))

(define (op1-branchf as i:op1 i:branchf tail)
  (let ((op (operand1 i:op1))
        (l  (operand1 i:branchf)))
    (peep-reg/op1/branchf as op $r.result l tail)))

(define (reg-op2-branchf as i:reg i:op2 i:branchf tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op  (operand1 i:op2))
        (l   (operand1 i:branchf)))
    (peep-reg/op2/branchf as op rs1 rs2 l tail)))

(define (op2-branchf as i:op2 i:branchf tail)
  (let ((op  (operand1 i:op2))
        (rs2 (operand2 i:op2))
        (l   (operand1 i:branchf)))
    (peep-reg/op2/branchf as op $r.result rs2 l tail)))

(define (peep-reg/op2/branchf as op rs1 rs2 l tail)
  (if (case op
        ((eq? < <= = > >= <:fix:fix <=:fix:fix =:fix:fix >:fix:fix >=:fix:fix) #t)
        (else #f))
      (as-source! as
                  (cons (list $reg/op2/branchf op rs1 rs2 l)
                        tail))))

(define (reg-op2imm-branchf as i:reg i:op2imm i:branchf tail)
  (let ((rs  (operand1 i:reg))
        (imm (operand2 i:op2imm))
        (op  (operand1 i:op2imm))
        (l   (operand1 i:branchf)))
    (peep-reg/op2imm/branchf as op rs imm l tail)))

(define (op2imm-branchf as i:op2imm i:branchf tail)
  (let ((op  (operand1 i:op2imm))
        (imm (operand2 i:op2imm))
        (l   (operand1 i:branchf)))
    (peep-reg/op2imm/branchf as op $r.result imm l tail)))

(define (peep-reg/op2imm/branchf as op rs imm l tail)
  (if (case op
        ((eq? < <= = > >= <:fix:fix <=:fix:fix =:fix:fix >:fix:fix >=:fix:fix) #t)
        (else #f))
      (as-source! as
                  (cons (list $reg/op2imm/branchf op rs imm l)
                        tail))))

(define (reg-op1-check as i:reg i:op1 i:check tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1))
        (l  (operand4 i:check)))
    (peep-reg/op1/check as op rs l
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (op1-check as i:op1 i:check tail)
  (let ((op (operand1 i:op1))
        (l  (operand4 i:check)))
    (peep-reg/op1/check as op $r.result l
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (peep-reg/op1/check as op rs l1 liveregs tail)
  (if (case op
        ((fixnum? flonum? pair? vector? bytevector? structure? ustring?) #t)
        (else #f))
      (as-source! as
                  (cons (list $reg/op1/check op rs l1 liveregs)
                        tail))))

(define (reg-op2-check as i:reg i:op2 i:check tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (peep-reg/op2/check as op rs1 rs2 (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (reg-op2imm-check as i:reg i:op2 i:check tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (peep-reg/op2imm/check as op rs1 rs2 (operand4 i:check)
                           (list (operand1 i:check)
                                 (operand2 i:check)
                                 (operand3 i:check))
                           tail)))

(define (op2-check as i:op2 i:check tail)
  (let ((rs2 (operand2 i:op2))
        (op  (operand1 i:op2)))
    (peep-reg/op2/check as op $r.result rs2 (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (op2imm-check as i:op2 i:check tail)
  (let ((rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (peep-reg/op2imm/check as op $r.result rs2 (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (peep-reg/op2/check as op rs1 rs2 l1 liveregs tail)
  (if (case op
        ((=:fix:fix <:fix:fix <=:fix:fix >=:fix:fix >:fix:fix) #t)
        (else #f))
      (as-source! as
                  (cons (list $reg/op2/check op rs1 rs2 l1 liveregs)
                        tail))))

(define (peep-reg/op2imm/check as op rs1 rs2 l1 liveregs tail)
  (if (case op
        ((=:fix:fix <:fix:fix <=:fix:fix >=:fix:fix >:fix:fix) #t)
        (else #f))
      (as-source! as
                  (cons (list $reg/op2imm/check op rs1 rs2 l1 liveregs)
                        tail))))

(define (peep-reg/op1/branchf as op rs l tail)
  (if (case op
        ((null? eof-object? pair? zero? fixnum? flonum? vector? bytevector? structure?) #t)
        (else #f))
      (as-source! as (cons (list $reg/op1/branchf op rs l) tail))))

(define (const-setreg as i:const i:setreg tail)
  (let ((c  (operand1 i:const))
        (rd (operand1 i:setreg)))
    (as-source! as (cons (list $const/setreg c rd) tail))))

(define (const-setglbl as i:const i:setglbl tail)
  (let ((c (operand1 i:const))
        (g (operand1 i:setglbl)))
    (as-source! as (cons (list $const/setglbl c g) tail))))

(define (save-and-stores as i:save il:stores tail)
  (let ((save-n      (operand1 i:save))
        (store-regs  (map operand1 il:stores))
        (store-slots (map operand2 il:stores)))
    (as-source! as (cons (list $save/stores save-n store-regs store-slots) tail))))

;; stack n            stack n
;; op1   op     ==>   setreg m
;; load  m,n          reg m
;;                    op1 op
(define (stack-op1-load as i:stack i:op1 i:load tail)
  (let ((n (operand1 i:stack))
        (op (operand1 i:op1))
        (m (operand1 i:load)))
    (cond 
     ((eqv? n (operand2 i:load))
      ;; longer, but setreg/reg or reg/op1 will itself be optimized,
      ;; and avoids memory traffic of second stack load.
      (as-source! as (append (list (list $stack n)
                                   (list $setreg m)
                                   (list $reg m)
                                   (list $op1 op))
                             tail))))))

(define (stack-setreg as i:stack i:setreg tail)
  (let ((n  (operand1 i:stack))
        (rd (operand1 i:setreg)))
    (as-source! as (cons (list $load rd n) tail))))

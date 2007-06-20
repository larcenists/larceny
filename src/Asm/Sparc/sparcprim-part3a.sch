; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC code generation macros for primitives, part 3a:
;   helper procedures for scalars.


; LOGAND, LOGIOR, LOGXOR: logical operations on fixnums.
;
; Input:  Registers rs1 and rs2, both of which can be general registers.
;         In addition, rs1 can be RESULT, and rs2 can be ARGREG2.
; Output: Register dest, which can be a general register or RESULT.

(define (logical-op as rs1 rs2 dest op excode)

  (define (fail rs1 rs2 L0)
    (if (not (= rs1 $r.result))  (sparc.move as rs1 $r.result))
    (if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2))
    (sparc.set as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L0))

  (let ((L0  (new-label))
        (L1  (new-label)))
    (sparc.label     as L0)
    (let ((rs1 (force-hwreg! as rs1 $r.result))
          (rs2 (force-hwreg! as rs2 $r.argreg2))
          (u   (unsafe-code))
          (d   (hardware-mapped? dest)))
      (cond ((and u d)
             (op as rs1 rs2 dest))
            ((and u (not d))
             (op as rs1 rs2 $r.tmp0)
             (emit-store-reg! as $r.tmp0 dest))
            ((and (not u) d)
             (sparc.orr     as rs1 rs2 $r.tmp0)
             (sparc.btsti   as $r.tmp0 3)
             (sparc.bz.a    as L1)
             (op            as rs1 rs2 dest)
             (fail rs1 rs2 L0)
             (sparc.label   as L1))
            (else
             (sparc.orr     as rs1 rs2 $r.tmp0)
             (sparc.btsti   as $r.tmp0 3)
             (sparc.bz.a    as L1)
             (op            as rs1 rs2 $r.tmp0)
             (fail rs1 rs2 L0)
             (sparc.label   as L1)
             (emit-store-reg! as $r.tmp0 dest))))))


; LSH, RSHA, RSHL: Bitwise shifts on fixnums.
;
; Notes for future contemplation:
;   - The semantics do not match those of MIT Scheme or MacScheme: only 
;     positive shifts are allowed.
;   - The names do not match the fixnum-specific procedures of Chez Scheme
;     that have the same semantics: fxsll, fxsra, fxsrl.
;   - This code checks that the second argument is in range; if it did
;     not, then we could get a MOD for free.  Probably too hardware-dependent
;     to worry about.
;   - The range 0..31 for the shift count is curious given that the fixnum
;     is 30-bit.

(define (emit-shift-operation as exn rs1 rs2 rd)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
        (let ((L0 (new-label))
              (FAULT (new-label))
              (START (new-label)))
          (sparc.label as START)
          (sparc.btsti as rs1 3)          ; RS1 fixnum?
          (sparc.be.a  as L0)
          (sparc.andi  as rs2 #x7c $r.g0) ; RS2 fixnum and 0 <= RS2 < 32?
          (sparc.label as FAULT)
          (if (not (= rs1 $r.result))
              (sparc.move as rs1 $r.result))
          (if (not (= rs2 $r.argreg2))
              (emit-move2hwreg! as rs2 $r.argreg2))
          (sparc.set   as (thefixnum exn) $r.tmp0)
          (millicode-call/ret as $m.exception START)
          (sparc.label as L0)
          (sparc.bne   as FAULT)
          (sparc.srai  as rs2 2 $r.tmp1))
        (begin
          (sparc.srai  as rs2 2 $r.tmp1)))
    (cond ((= exn $ex.lsh)
           (sparc.sllr as rs1 $r.tmp1 rd))
          ((= exn $ex.rshl)
           (sparc.srlr  as rs1 $r.tmp1 rd)
           (sparc.andni as rd 3 rd))
          ((= exn $ex.rsha)
           (sparc.srar  as rs1 $r.tmp1 rd)
           (sparc.andni as rd 3 rd))
          (else ???))))


; Set result on condition code.
;
; The processor's zero bit has been affected by a previous instruction.
; If the bit is set, store #t in RESULT, otherwise store #f in RESULT.

(define (emit-set-boolean! as)
  (emit-set-boolean-reg! as $r.result))


; Set on condition code.
;
; The processor's zero bit has been affected by a previous instruction.
; If the bit is set, store #t in the processor register 'dest', otherwise
; store #f in 'dest'.

(define (emit-set-boolean-reg! as dest)
  (let ((L1 (new-label)))
    (sparc.set   as $imm.true dest)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false dest)
    (sparc.label as L1)))


; Representation predicate.

(define (emit-single-tagcheck->bool! as tag)
  (sparc.andi as $r.result $tag.tagmask $r.tmp0)
  (sparc.cmpi as $r.tmp0 tag)
  (emit-set-boolean! as))

(define (emit-single-tagcheck-assert! as tag1 excode reg2)
  (emit-single-tagcheck-assert-reg! as tag1 $r.result reg2 excode))

(define (emit-single-tagcheck-assert-reg! as tag1 reg reg2 excode)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as reg $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (fault-if-ne as excode #f #f reg reg2 L0)))

; Assert that a machine register has a fixnum in it.
; Returns the label of the fault code.

(define (emit-assert-fixnum! as reg excode)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label  as L0)
    (sparc.btsti  as reg 3)
    (fault-if-ne as excode #f #f reg #f L0)))

; Assert that RESULT has a character in it.
; Returns the label of the fault code.

(define (emit-assert-char! as excode fault-label)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as $r.result #xFF $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $imm.character)
    (fault-if-ne as excode #f fault-label #f #f L0)))

; Generate code for fault handling if the zero flag is not set.
; - excode is the nativeint exception code.
; - cont-label, if not #f, is the label to go to if there is no fault.
; - fault-label, if not #f, is the label of an existing fault handler.
; - reg1, if not #f, is the number of a register which must be
;   moved into RESULT before the fault handler is called.
; - reg2, if not #f, is the number of a register which must be moved
;   into ARGREG2 before the fault handler is called.
; - ret-label, if not #f, is the return address to be set up before calling
;   the fault handler.
;
; Ret-label and fault-label cannot simultaneously be non-#f; in this case
; the ret-label is ignored (since the existing fault handler most likely
; sets up the return in the desired manner).

(define (fault-if-ne as excode cont-label fault-label reg1 reg2 ret-label)
  (if fault-label
      (begin 
        (if (and reg2 (not (= reg2 $r.argreg2)))
            (emit-move2hwreg! as reg2 $r.argreg2))
        (sparc.bne as fault-label)
        (if (and reg1 (not (= reg1 $r.result)))
            (sparc.move as reg1 $r.result)
            (sparc.nop as))
        fault-label)
      (let ((FAULT (new-label))
            (L1    (new-label)))
        (sparc.be.a  as (or cont-label L1))
        (sparc.slot  as)
        (sparc.label as FAULT)
        (if (and reg1 (not (= reg1 $r.result)))
            (sparc.move as reg1 $r.result))
        (if (and reg2 (not (= reg2 $r.argreg2)))
            (emit-move2hwreg! as reg2 $r.argreg2))
        (sparc.set   as (thefixnum excode) $r.tmp0)
        (millicode-call/ret as $m.exception (or ret-label L1))
        (if (or (not cont-label) (not ret-label))
            (sparc.label as L1))
        FAULT)))

; This is more expensive than what is good for it (5 cycles in the usual case),
; but there does not seem to be a better way.

(define (emit-assert-positive-fixnum! as reg excode)
  (let ((L1 (new-label))
        (L2 (new-label))
        (L3 (new-label))) 
    (sparc.label   as L2)
    (sparc.tsubrcc as reg $r.g0 $r.g0)
    (sparc.bvc     as L1)
    (sparc.nop     as)
    (sparc.label   as L3)
    (if (not (= reg $r.result))
        (sparc.move as reg $r.result))
    (sparc.set     as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L2)
    (sparc.label   as L1)
    (sparc.bl      as L3)
    (sparc.nop     as)
    L3))


; Arithmetic comparison with boolean result.

(define (emit-cmp-primop! as branch_t.a generic r)
  (let ((Ltagok (new-label))
        (Lcont  (new-label))
        (r      (force-hwreg! as r $r.argreg2)))
    (sparc.tsubrcc as $r.result r $r.g0)
    (sparc.bvc.a   as Ltagok)
    (sparc.set     as $imm.false $r.result)
    (if (not (= r $r.argreg2))
        (sparc.move    as r $r.argreg2))
    (millicode-call/ret as generic Lcont)
    (sparc.label   as Ltagok)
    (branch_t.a    as Lcont)
    (sparc.set     as $imm.true $r.result)
    (sparc.label   as Lcont)))


; Arithmetic comparison and branch.
;
; This code does not use the chained branch trick (DCTI) that was documented
; in the Sparc v8 manual and deprecated in the v9 manual.  This code executes
; _much_ faster on the Ultra than the code using DCTI, even though it executes
; the same instructions.
;
; Parameters and preconditions.
;   Src1 is a general register, RESULT, ARGREG2, or ARGREG3.
;   Src2 is a general register, RESULT, ARGREG2, ARGREG3, or an immediate.
;   Src2 is an immediate iff src2isreg = #f.
;   Branch_f.a is a branch on condition code that branches if the condition
;     is not true.
;   Generic is the millicode table offset of the generic procedure.

(define (emit-bcmp-primop! as branch_f.a src1 src2 Lfalse generic src2isreg)
  (let ((Ltagok (new-label))
        (Ltrue  (new-label))
        (op2    (if src2isreg
                    (force-hwreg! as src2 $r.tmp1)
                    (thefixnum src2)))
        (sub   (if src2isreg sparc.tsubrcc sparc.tsubicc))
        (mov   (if src2isreg sparc.move sparc.set)))
    (sub         as src1 op2 $r.g0)
    (sparc.bvc.a as Ltagok)
    (sparc.slot  as)

    ; Not both fixnums.
    ; Must move src1 to result if src1 is not result.
    ; Must move src2 to argreg2 if src2 is not argreg2.

    (let ((move-res  (not (= src1 $r.result)))
          (move-arg2 (or (not src2isreg) (not (= op2 $r.argreg2)))))
      (if (and move-arg2 move-res)
          (mov     as op2 $r.argreg2))
      (sparc.jmpli as $r.millicode generic $r.o7)
      (cond (move-res   (sparc.move as src1 $r.result))
            (move-arg2  (mov        as op2 $r.argreg2))
            (else       (sparc.nop  as)))
      (sparc.cmpi  as $r.result $imm.false)
      (sparc.bne.a as Ltrue)
      (sparc.slot  as)
      (sparc.b     as Lfalse)
      (sparc.slot  as))

    (sparc.label as Ltagok)
    (branch_f.a   as Lfalse)
    (sparc.slot  as)
    (sparc.label as Ltrue)))


; Generic arithmetic for + and -.
; Some rules:
;   We have two HW registers src1 and dest.
;   If src2isreg is #t then src2 may be a HW reg or a SW reg
;   If src2isreg is #f then src2 is an immediate fixnum, not shifted.
;   Src1 and dest may be RESULT, but src2 may not.
;   Src2 may be ARGREG2, the others may not.
;
; FIXME! This is incomprehensible.

; New code below.

'(define (emit-arith-primop! as op invop generic src1 src2 dest src2isreg)
  (let ((L1  (new-label))
        (op2 (if src2isreg
                 (force-hwreg! as src2 $r.tmp1)
                 (thefixnum src2))))
    (if (and src2isreg (= op2 dest))
        (begin (op          as src1 op2 $r.tmp0)
               (sparc.bvc.a as L1)
               (sparc.move  as $r.tmp0 dest))
        (begin (op          as src1 op2 dest)
               (sparc.bvc.a as L1)
               (sparc.slot  as)
               (invop       as dest op2 dest)))
    (let ((n    (+ (if (not (= src1 $r.result)) 1 0)
                   (if (or (not src2isreg) (not (= op2 $r.argreg2))) 1 0)))
          (mov2 (if src2isreg sparc.move sparc.set)))
      (if (= n 2)
          (mov2 as op2 $r.argreg2))
      (sparc.jmpli as $r.millicode generic $r.o7)
      (cond ((= n 0) (sparc.nop  as))
            ((= n 1) (mov2       as op2 $r.argreg2))
            (else    (sparc.move as src1 $r.result)))
      ; Generic arithmetic leaves stuff in RESULT, must move to dest if
      ; dest is not RESULT.
      (if (not (= dest $r.result))
          (sparc.move as $r.result dest))
      (sparc.label as L1))))

; Comprehensible, but longer.
;
; Important to be careful not to clobber arguments, and not to leave garbage
; in rd, if millicode is called.
;
; op is the appropriate operation.
; invop is the appropriate inverse operation.
; RS1 can be any general hw register or RESULT.
; RS2/IMM can be any general register or ARGREG2 (op2isreg=#t), or 
;         an immediate (op2isreg=#f)
; RD can be any general hw register or RESULT.
;
; FIXME: split this into two procedures.

(define (emit-arith-primop! as op invop generic rs1 rs2/imm rd op2isreg)
  (let ((L1 (new-label)))
    (if op2isreg
        (let ((rs2 (force-hwreg! as rs2/imm $r.argreg2)))
          (cond ((or (= rs1 rs2 rd)
                     (and (= rs2 rd)
                          (= generic $m.subtract)))
                 (op          as rs1 rs2 $r.tmp0)
                 (sparc.bvc.a as L1)
                 (sparc.move  as $r.tmp0 rd))
                ((= rs1 rd)
                 (op          as rs1 rs2 rs1)
                 (sparc.bvc.a as L1)
                 (sparc.slot  as)
                 (invop       as rs1 rs2 rs1))
                ((= rs2 rd)
                 (op          as rs1 rs2 rs2)
                 (sparc.bvc.a as L1)
                 (sparc.slot  as)
                 (invop       as rs2 rs1 rs2))
                (else
                 (op          as rs1 rs2 rd)
                 (sparc.bvc.a as L1)
                 (sparc.slot  as)
                 (if (and (not (= rd $r.result)) (not (= rd $r.argreg2)))
                     (sparc.clr as rd))))
          (cond ((and (= rs1 $r.result) (= rs2 $r.argreg2))
                 ;; Could peephole the INVOP or CLR into the slot here.
                 (millicode-call/0arg as generic))
                ((= rs1 $r.result)
                 (millicode-call/1arg as generic rs2))
                ((= rs2 $r.argreg2)
                 (millicode-call/1arg-in-result as generic rs1))
                (else
                 (sparc.move as rs2 $r.argreg2)
                 (millicode-call/1arg-in-result as generic rs1))))
        (let ((imm (thefixnum rs2/imm)))
          (op          as rs1 imm rd)
          (sparc.bvc.a as L1)
          (sparc.slot  as)
          (invop       as rd imm rd)
          (if (not (= rs1 $r.result))
              (sparc.move as rs1 $r.result))
          (millicode-call/numarg-in-reg as generic imm $r.argreg2)))
    (if (not (= rd $r.result))
        (sparc.move as $r.result rd))
    (sparc.label as L1)))


; Important to be careful not to leave garbage in rd if millicode is called.

(define (emit-negate as rs rd)
  (let ((L1 (new-label)))
    (cond ((= rs rd)
           (sparc.tsubrcc as $r.g0 rs rs)
           (sparc.bvc.a   as L1)
           (sparc.slot    as)
           (if (= rs $r.result)
               (begin 
                 (sparc.jmpli as $r.millicode $m.negate $r.o7)
                 (sparc.subr  as $r.g0 $r.result $r.result))
               (begin
                 (sparc.subr  as $r.g0 rs rs)
                 (sparc.jmpli as $r.millicode $m.negate $r.o7)
                 (sparc.move  as rs $r.result))))
          (else
           (sparc.tsubrcc as $r.g0 rs rd)
           (sparc.bvc.a   as L1)
           (sparc.slot    as)
           (cond ((= rs $r.result)
                  (sparc.jmpli as $r.millicode $m.negate $r.o7)
                  (sparc.clr   as rd))
                 ((= rd $r.result)
                  (sparc.jmpli as $r.millicode $m.negate $r.o7)
                  (sparc.move  as rs $r.result))
                 (else
                  (sparc.clr   as rd)
                  (sparc.jmpli as $r.millicode $m.negate $r.o7)
                  (sparc.move  as rs $r.result)))))
    (if (not (= rd $r.result))
        (sparc.move as $r.result rd))
    (sparc.label   as L1)))

; Character comparison.

; r is a register or a character constant.

(define (emit-char-cmp as r btrue.a excode)
  (emit-charcmp! as (lambda ()
                      (let ((l2 (new-label)))
                        (sparc.set   as $imm.false $r.result)
                        (btrue.a     as L2)
                        (sparc.set   as $imm.true $r.result)
                        (sparc.label as L2)))
                 $r.result
                 r
                 excode))
 
; op1 is a hw register
; op2 is a register or a character constant

(define (emit-char-bcmp-primop! as bfalse.a op1 op2 L0 excode)
  (emit-charcmp! as (lambda ()
                      (bfalse.a   as L0)
                      (sparc.slot as))
                 op1
                 op2
                 excode))

; We check the tags of both to make sure they are characters.
; If so, then we can subtract one from the other (tag and all) and check the
; condition codes.  
;
; The branch-on-true instruction must have the annull bit set. (???)
;
; op1 is a hw register
; op2 is a register or a character constant.

(define (emit-charcmp! as tail op1 op2 excode)
  (let ((op2 (if (char? op2)
                 op2
                 (force-hwreg! as op2 $r.argreg2))))
    (cond ((not (unsafe-code))
           (let ((L0 (new-label))
                 (L1 (new-label))
                 (FAULT (new-label)))
             (sparc.label as L0)
             (cond ((char? op2)
                    (sparc.xori  as op1 $imm.character $r.tmp0)
                    (sparc.btsti as $r.tmp0 #xFF)
                    (sparc.srli  as op1 8 $r.tmp0)
                    (sparc.be.a  as L1)
                    (sparc.cmpi  as $r.tmp0 (char->integer op2)))
                   (else
                    (sparc.andi  as op1 #xFF $r.tmp0)
                    (sparc.andi  as op2 #xFF $r.tmp1)
                    (sparc.cmpr  as $r.tmp0 $r.tmp1)
                    (sparc.bne   as FAULT)
                    (sparc.cmpi  as $r.tmp0 $imm.character)
                    (sparc.be.a  as L1)
                    (sparc.cmpr  as op1 op2)))
             (sparc.label as FAULT)
             (if (not (eqv? op1 $r.result))
                 (sparc.move as op1 $r.result))
             (cond ((char? op2) 
                    (emit-immediate->register! as
                                               (char->immediate op2)
                                               $r.argreg2))
                   ((not (eqv? op2 $r.argreg2))
                    (sparc.move as op2 $r.argreg2)))
             (sparc.set   as (thefixnum excode) $r.tmp0)
             (millicode-call/ret as $m.exception L0)
             (sparc.label as L1)))
          ((not (char? op2))
           (sparc.cmpr as op1 op2))
          ((sparc-imm? (char->integer op2))
           (sparc.srli as op1 8 $r.tmp0)
           (sparc.cmpi as $r.tmp0 (char->integer op2)))
          (else
           (emit-immediate->register! as (char->immediate op2) $r.argreg2)
           (sparc.cmpi as op1 $r.argreg2)))
    (tail)))

; eof

; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC code generation macros for primitives, part 3:
;   helper procedures.


; SET-CAR!, SET-CDR!, CELL-SET!
;
; Input:  RS1: a hardware register; has pair pointer (tag check must be
;         performed by the caller).
;         RS2: any register; has value to store.
; Output: None.
;
; Having rs1 != RESULT is pretty silly with the current write barrier
; but will be less silly with the new barrier.

(define (emit-setcar/setcdr! as rs1 rs2 offs)
  (cond ((and (write-barrier) (hardware-mapped? rs2))
	 (sparc.sti as rs2 (- offs $tag.pair-tag) rs1)
	 (if (not (= rs1 $r.result))
	     (sparc.move as rs1 $r.result))
	 (millicode-call/1arg as $m.addtrans rs2))
	((write-barrier)
	 (emit-move2hwreg! as rs2 $r.argreg2)
	 (sparc.sti as $r.argreg2 (- offs $tag.pair-tag) rs1)
	 (millicode-call/1arg-in-result as $m.addtrans rs1))
	((hardware-mapped? rs2)
	 (sparc.sti as rs2 (- offs $tag.pair-tag) rs1))
	(else
	 (emit-move2hwreg! as rs2 $r.argreg2)
	 (sparc.sti as $r.argreg2 (- offs $tag.pair-tag) rs1))))


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
	  (sparc.btsti as rs1 3)	  ; RS1 fixnum?
	  (sparc.be.a  as L0)
	  (sparc.andi  as rs2 #x7c $r.g0) ; RS2 fixnum and 0 <= RS2 < 32?
	  (sparc.label as FAULT)
	  (if (not (= rs1 $r.result))
	      (sparc.mov as rs1 $r.result))
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
;
; RESULT has an object.  If the tag of RESULT is 'tag1' and the 
; header byte of the object is 'tag2' then set RESULT to #t, else
; set it to #f.

(define (emit-double-tagcheck->bool! as tag1 tag2)
  (let ((L1 (new-label)))
    (sparc.andi  as $r.result $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.ldbi  as $r.result (+ (- tag1) 3) $r.tmp0)
    (sparc.set   as $imm.true $r.result)
    (sparc.cmpi  as $r.tmp0 tag2)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.label as L1)))


; Check structure tag.
;
; RS1 has an object.  If the tag of RS1 is not 'tag1', or if the tag is 
; 'tag1' but the header byte of the object header is not 'tag2', then an
; exception with code 'excode' is signalled.  The exception call is set
; up to return to the first instruction of the emitted code.
;
; If RS1 is not RESULT then it is moved to RESULT before the exception 
; is signalled.
;
; If RS2/IMM is not #f, then it is a register or immediate that is moved
; to ARGREG2 before the exception is signalled; it is an immediate iff 
; imm? = #t.  
;
; RS1 must be a hardware register.
; RS2/IMM is a general register, ARGREG2, an immediate, or #f.
; RS3 is a general register, ARGREG3, or #f.
;
; The procedure returns the label of the fault address.  If the execution
; falls off the end of the emitted instruction sequence, then the following
; are true:
;  - the tag of the object in RS1 was 'tag1' and its header byte was 'tag2'
;  - the object header word is in TMP0.

(define (double-tagcheck-assert as tag1 tag2 rs1 rs2/imm rs3 excode imm?)
  (let ((L0    (new-label))
	(L1    (new-label))
	(FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as rs1 $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.be.a  as L1)
    (sparc.ldi   as rs1 (- tag1) $r.tmp0)
    (sparc.label as FAULT)
    (if (not (= rs1 $r.result))
	(sparc.move as rs1 $r.result))
    (if rs2/imm 
	(cond (imm?
	       (sparc.set as (thefixnum rs2/imm) $r.argreg2))
	      ((= rs2/imm $r.argreg2))
	      (else
	       (emit-move2hwreg! as rs2/imm $r.argreg2))))
    (if (and rs3 (not (= rs3 $r.argreg3)))
	(emit-move2hwreg! as rs3 $r.argreg3))
    (sparc.set   as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L0)
    (sparc.label as L1)
    (sparc.andi  as $r.tmp0 255 $r.tmp1)
    (sparc.cmpi  as $r.tmp1 tag2)
    (sparc.bne.a as FAULT)
    (sparc.slot  as)
    FAULT))

(define (emit-double-tagcheck-assert! as tag1 tag2 excode reg2)
  (double-tagcheck-assert as tag1 tag2 $r.result reg2 #f excode #f))

(define (emit-double-tagcheck-assert-reg/reg! as tag1 tag2 rs1 rs2 excode)
  (double-tagcheck-assert as tag1 tag2 rs1 rs2 #f excode #f))
  
(define (emit-double-tagcheck-assert-reg/imm! as tag1 tag2 rs1 imm excode)
  (double-tagcheck-assert as tag1 tag2 rs1 imm #f excode #t))


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
    (millicode-call/ret as $m.exception l2)
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


; Get the length of a vector or bytevector structure, with tag checking
; included.
;
; Input: RS and RD are both hardware registers.

(define (emit-get-length! as tag1 tag2 excode rs rd)
  (if (not (unsafe-code))
      (if tag2
	  (emit-double-tagcheck-assert-reg/reg! as tag1 tag2 rs rd excode)
	  (emit-single-tagcheck-assert-reg! as tag1 rs rd excode)))
  (sparc.ldi  as rs (- tag1) $r.tmp0)
  (sparc.srli as $r.tmp0 8 rd)
  (if (= tag1 $tag.bytevector-tag)
      (sparc.slli as rd 2 rd)))


; Allocate a bytevector, leave untagged pointer in RESULT.

(define (emit-allocate-bytevector as hdr preserved-result)

  ; Preserve the length field, then calculate the number of words
  ; to allocate.  The value `28' is an adjustment of 3 (for rounding 
  ; up) plus another 4 bytes for the header, all represented as a fixnum.

  (if (not preserved-result)
      (sparc.move as $r.result $r.argreg2))
  (sparc.addi as $r.result 28 $r.result)
  (sparc.andi as $r.result (asm:signed #xFFFFFFF0) $r.result)

  ; Allocate space

  (sparc.jmpli as $r.millicode $m.alloc-bv $r.o7)
  (sparc.srai  as $r.result 2 $r.result)
  
  ; Setup the header.

  (if (not preserved-result)
      (sparc.slli as $r.argreg2 6 $r.tmp0)
      (sparc.slli as preserved-result 6 $r.tmp0))
  (sparc.addi as $r.tmp0 hdr $r.tmp0)
  (sparc.sti  as $r.tmp0 0 $r.result))


; Given a nativeint count, a pointer to the first element of a 
; bytevector-like structure, and a byte value, fill the bytevector
; with the byte value.

(define (emit-bytevector-fill as r-bytecount r-pointer r-value)
  (let ((L2 (new-label))
	(L1 (new-label)))
    (sparc.label  as L2)
    (sparc.deccc  as r-bytecount)
    (sparc.bge.a  as L2)
    (sparc.stbr   as r-value r-bytecount r-pointer)
    (sparc.label  as L1)))


; BYTEVECTOR-REF, BYTEVECTOR-LIKE-REF, STRING-REF.
;
; The pointer in RS1 is known to be bytevector-like.  RS2 is the fixnum
; index into the structure.  Get the RS2'th element and place it in RD.
;
; RS1 and RD are hardware registers.
; RS2 is a general register or ARGREG2.
; 'fault' is defined iff (unsafe-code) = #f
; header is in TMP0 iff (unsafe-code) = #f and 'header-loaded?' = #t
; if 'charize?' is #t then store result as char, otherwise as fixnum.

(define (emit-bytevector-like-ref! as rs1 rs2 rd fault charize? header-loaded?)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
	(begin
	  ; check that index is fixnum
	  (sparc.btsti  as rs2 3)
	  (sparc.bne    as fault)
	  (if (not header-loaded?)
	      (sparc.ldi as rs1 (- $tag.bytevector-tag) $r.tmp0))
	  ; check length
	  (sparc.srai   as rs2 2 $r.tmp1)
	  (sparc.srli   as $r.tmp0 8 $r.tmp0)
	  (sparc.cmpr   as $r.tmp0 $r.tmp1)
	  (sparc.bleu as fault)
	  ; No NOP or SLOT -- the SUBI below goes into the slot.
	  )
	(begin
	  (sparc.srai   as rs2 2 $r.tmp1)))
    ; Pointer is in RS1.
    ; Shifted index is in TMP1.
    (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
    (sparc.ldbr as $r.tmp0 $r.tmp1 $r.tmp0)
    (if (not charize?)
	(sparc.slli as $r.tmp0 2 rd)
	(begin (sparc.slli as $r.tmp0 16 rd)
	       (sparc.ori  as rd $imm.character rd)))))

; As above, but RS2 is replaced by an immediate, IMM.
;
; The immediate, represented as a fixnum, is guaranteed fit in the 
; instruction's immediate field.

(define (emit-bytevector-like-ref/imm! as rs1 imm rd fault charize?
				       header-loaded?)
  (if (not (unsafe-code))
      (begin
	(if (not header-loaded?)
	    (sparc.ldi as rs1 (- $tag.bytevector-tag) $r.tmp0))
	; Range check.
	(sparc.srli   as $r.tmp0 8 $r.tmp0)
	(sparc.cmpi   as $r.tmp0 imm)
	(sparc.bleu.a as fault)
	(sparc.slot   as)))

  ; Pointer is in RS1.

  (let ((adjusted-offset (+ (- 4 $tag.bytevector-tag) imm)))
    (if (immediate-literal? adjusted-offset)
	(begin
	  (sparc.ldbi as rs1 adjusted-offset $r.tmp0))
	(begin
	  (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
	  (sparc.ldbr as $r.tmp0 imm $r.tmp0)))
    (if (not charize?)
	(sparc.slli as $r.tmp0 2 rd)
	(begin (sparc.slli as $r.tmp0 16 rd)
	       (sparc.ori  as rd $imm.character rd)))))


; BYTEVECTOR-SET!, BYTEVECTOR-LIKE-SET!
;
; Input:  RESULT -- a pointer to a bytevector-like structure.
;         TMP0   -- the header iff (unsafe-code) = #f and header-loaded? = #t
;         IDX    -- a register that holds the second argument
;         BYTE   -- a register that holds the third argument
; Output: Nothing.
;
; 'Fault' is the address of the error code iff (unsafe-code) = #f
;
; FIXME: 
;   - Argument values passed to error handler appear to be bogus 
;     (error message is very strange).
;   - There's no check that the value actually fits in a byte.
;   - Uses ARGREG3 and and TMP2.

(define (emit-bytevector-like-set! as idx byte fault header-loaded?)
  (let ((r1 (force-hwreg! as idx $r.tmp1))
	(r2 (force-hwreg! as byte $r.argreg3)))
    (if (not (unsafe-code))
	(begin
	  (if (not header-loaded?)
	      (sparc.ldi     as $r.result (- $tag.bytevector-tag) $r.tmp0))
	  ; Both index and byte must be fixnums.  
	  ; Can't use tsubcc because the computation may really overflow.
	  (sparc.orr     as r1 r2 $r.tmp2)
	  (sparc.btsti   as $r.tmp2 3)
	  (sparc.bnz     as fault)
	  ; No NOP -- next instruction is OK in slot.
	  ; Index must be in range.
	  (sparc.srli    as $r.tmp0 8 $r.tmp0)    ; limit - in slot
	  (sparc.srai    as r1 2 $r.tmp1)         ; index
	  (sparc.cmpr    as $r.tmp1 $r.tmp0)
	  (sparc.bgeu    as fault)
	  ; No NOP -- next instruction is OK in slot.
	  )
	(begin
	  (sparc.srai   as r1 2 $r.tmp1)))
    (sparc.srli as r2 2 $r.tmp0)
    ; Using ARGREG2 as the destination is OK because the resulting pointer
    ; value always looks like a fixnum.  By doing so, we avoid needing TMP2.
    (sparc.addi as $r.result (- 4 $tag.bytevector-tag) $r.argreg2)
    (sparc.stbr as $r.tmp0 $r.tmp1 $r.argreg2)))


; STRING-SET!

(define (emit-string-set! as rs1 rs2 rs3)
  (let* ((rs2 (force-hwreg! as rs2 $r.argreg2))
	 (rs3 (force-hwreg! as rs3 $r.argreg3))
	 (FAULT (if (not (unsafe-code))
		    (double-tagcheck-assert 
		     as 
		     $tag.bytevector-tag
		     (+ $imm.bytevector-header $tag.string-typetag)
		     rs1 rs2 rs3
		     $ex.sset
		     #f))))
    ; Header is in TMP0; TMP1 and TMP2 are free.
    (if (not (unsafe-code))
	(begin
	  ; RS2 must be a fixnum.
	  (sparc.btsti  as rs2 3)
	  (sparc.bne    as FAULT)
	  ; Index (in RS2) must be valid; header is in tmp0.
	  (sparc.srli   as $r.tmp0 8 $r.tmp0) ; limit
	  (sparc.srai   as rs2 2 $r.tmp1) ; index
	  (sparc.cmpr   as $r.tmp1 $r.tmp0)
	  (sparc.bgeu   as FAULT)
	  ; RS3 must be a character.
	  (sparc.andi   as rs3 #xFF $r.tmp0)
	  (sparc.cmpi   as $r.tmp0 $imm.character)
	  (sparc.bne    as FAULT)
	  ; No NOP -- the SRLI below goes in the slot
	  )
	(begin
	  (sparc.srai as rs2 2 $r.tmp1)))
    ; tmp1 has nativeint index. 
    ; rs3/argreg3 has character.
    ; tmp0 is garbage.
    (sparc.subi as $r.tmp1 (- $tag.bytevector-tag 4) $r.tmp1)
    (sparc.srli as rs3 16 $r.tmp0)
    (sparc.stbr as $r.tmp0 rs1 $r.tmp1)))


; VECTORS and PROCEDURES

; Allocate short vectors of known length; faster than the general case.
; FIXME: can also allocate in-line.

(define (make-vector-n as length r)
  (sparc.jmpli as $r.millicode $m.alloc $r.o7)
  (sparc.set  as (thefixnum (+ length 1)) $r.result)
  (emit-immediate->register! as (+ (* 256 (thefixnum length))
				   $imm.vector-header
				   $tag.vector-typetag)
			     $r.tmp0)
  (sparc.sti  as $r.tmp0 0 $r.result)
  (let ((dest (force-hwreg! as r $r.argreg2)))
    (do ((i 0 (+ i 1)))
	((= i length))
      (sparc.sti as dest (* (+ i 1) 4) $r.result)))
  (sparc.addi as $r.result $tag.vector-tag $r.result))


; emit-make-vector-like! assumes argreg3 is not destroyed by alloci.
; FIXME: bug: $ex.mkvl is not right if the operation is make-procedure
; or make-vector.

(define (emit-make-vector-like! as r hdr ptrtag)
  (let ((FAULT (emit-assert-positive-fixnum! as $r.result $ex.mkvl)))
    (sparc.move  as $r.result $r.argreg3)
    (sparc.addi  as $r.result 4 $r.result)
    (sparc.jmpli as $r.millicode $m.alloci $r.o7)
    (if (null? r)
	(sparc.set as $imm.null $r.argreg2)
	(emit-move2hwreg! as r $r.argreg2))
    (sparc.slli  as $r.argreg3 8 $r.tmp0)
    (sparc.addi  as $r.tmp0 hdr $r.tmp0)
    (sparc.sti   as $r.tmp0 0 $r.result)
    (sparc.addi  as $r.result ptrtag $r.result)))


; VECTOR-REF, VECTOR-LIKE-REF, PROCEDURE-REF
;
; FAULT is valid iff (unsafe-code) = #f
; Header is in TMP0 iff (unsafe-code) = #f and header-loaded? = #t.

(define (emit-vector-like-ref! as rs1 rs2 rd FAULT tag header-loaded? )
  (let ((index (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
	(begin
	  (if (not header-loaded?)
	      (sparc.ldi   as rs1 (- tag) $r.tmp0))
	  ; Index must be fixnum.
	  (sparc.btsti as index 3)
	  (sparc.bne   as FAULT)
	  ; Index must be within bounds.
	  (sparc.srai  as $r.tmp0 8 $r.tmp0)
	  (sparc.cmpr  as $r.tmp0 index)
	  (sparc.bleu  as FAULT)
	  ; No NOP; the following instruction is valid in the slot.
	  ))
    (sparc.addi as rs1 (- 4 tag) $r.tmp0)
    (sparc.ldr  as $r.tmp0 index rd)))


; VECTOR-REF/IMM, VECTOR-LIKE-REF/IMM, PROCEDURE-REF/IMM
;
; 'rs1' is a hardware register containing a vectorish pointer (to a
;       vector-like or procedure).
; 'imm' is a fixnum s.t. (immediate-literal? imm) => #t.
; 'rd' is a hardware register.
; 'FAULT' is the label of the error code iff (unsafe-code) => #f
; 'tag' is the tag of the pointer in rs1.
; 'header-loaded?' is #t iff the structure header word is in $r.tmp0.

(define (emit-vector-like-ref/imm! as rs1 imm rd FAULT tag header-loaded?)
  (if (not (unsafe-code))
      (begin
	(if (not header-loaded?) (sparc.ldi as rs1 (- tag) $r.tmp0))
	; Check bounds.
	(sparc.srai  as $r.tmp0 10 $r.tmp0)
	(sparc.cmpi  as $r.tmp0 imm)
	(sparc.bleu  as FAULT)
	(sparc.nop   as)))
  (let* ((offset (* imm 4))		          ; words->bytes
	 (adjusted-offset (+ (- 4 tag) offset)))
    (if (immediate-literal? adjusted-offset)
	(begin
	  (sparc.ldi as rs1 adjusted-offset rd))
	(begin
	  (sparc.addi as rs1 (- 4 tag) $r.tmp0)
	  (sparc.ldi  as $r.tmp0 offset rd)))))


; VECTOR-SET!, VECTOR-LIKE-SET!, PROCEDURE-SET!
;
; It is assumed that the pointer in RESULT is valid. We must check the index
; in register x for validity and then perform the side effect (by calling
; millicode). The tag is the pointer tag to be adjusted for.
;
; The use of vector-set is ok even if it is a procedure.

; fault is valid iff (unsafe-code) = #f
; header is in tmp0 iff (unsafe-code) = #f and header-loaded? = #t

(define (emit-vector-like-set! as rs1 rs2 rs3 fault tag header-loaded?)
  (let ((rs2 (force-hwreg! as rs2 $r.tmp1))
	(rs3 (force-hwreg! as rs3 $r.argreg2)))
    (if (not (unsafe-code))
	(begin 
	  (if (not header-loaded?)
	      (sparc.ldi as $r.result (- tag) $r.tmp0))
	  (sparc.btsti as rs2 3)
	  (sparc.bne   as fault)
	  (sparc.srai  as $r.tmp0 8 $r.tmp0)
	  (sparc.cmpr  as $r.tmp0 rs2)
	  (sparc.bleu  as fault)))
    ;; The ADDR goes in the delay slot of the preceding BLEU (if emitted).
    (sparc.addr as rs1 rs2 $r.tmp0)
    (cond ((not (write-barrier))
	   (sparc.sti  as rs3 (- 4 tag) $r.tmp0))
	  ((= rs1 $r.result)
	   (cond ((= rs3 $r.argreg2)
		  (sparc.jmpli as $r.millicode $m.addtrans $r.o7)
		  (sparc.sti  as rs3 (- 4 tag) $r.tmp0))
		 (else
		  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
		  (millicode-call/1arg as $m.addtrans rs3))))
	  (else
	   (cond ((= rs3 $r.argreg2)
		  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
		  (millicode-call/1arg-in-result as $m.addtrans rs1))
		 (else
		  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
		  (sparc.move as rs1 $r.result)
		  (millicode-call/1arg as $m.addtrans rs3)))))))

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

; We check the tags of both by xoring them and seeing if the low byte is 0.
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
		    (sparc.srli  as op1 16 $r.tmp0)
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
	  (else
	   (sparc.srli as op1 16 $r.tmp0)
	   (sparc.cmpi as $r.tmp0 (char->integer op2))))
    (tail)))

; eof

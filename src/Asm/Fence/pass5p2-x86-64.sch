; x86-64 overrides for the Cant primitives, after pass5p2-arm64.sch.
;
; Only hardware-backed MacScheme registers; the assignment is in
; src/Rts/fence-x86-64-regs.cfg.  In short:
;
;   rax       TMP1, assembler scratch, not exposed to pass5p2 (and the
;             low half of the dividend and product of idiv and imul)
;   rcx       TMP0 (this is $r.tmp in pass5p2)
;   rdx       TMP2, assembler scratch, not exposed to pass5p2 (and the
;             high half of the dividend of idiv)
;   rbx       GLOBALS (callee-saved in C, so it survives calls to C)
;   rsp       the C stack pointer; Scheme code calls millicode with CALL,
;             which pushes the return address, but changes it no other way
;   rbp       STKP
;   rsi       RESULT
;   rdi       SECOND
;   r8..r15   REG0..REG7
;
; The MacScheme floating registers F0..F3 are xmm0..xmm3, which are not
; callee-saved in C, so millicode saves them; xmm15 is a floating
; scratch register.
;
; Millicode is called through a table of function pointers in the
; globals array: the millicode code passed to cant.trap is the byte
; offset of the entry in the globals array.  The millicode pops the
; return address that CALL pushed.
;
; Scheme calls store a return address computed by LEA from RIP into
; the frame and jump; returns jump through the frame.  Return points
; are padded with NOPs to be four-byte aligned, so every return address
; looks like a fixnum to the garbage collector.
;
; The shared assembler (Asm/Shared/pass5p1.sch) makes one pass, so the
; size of a branch must be chosen before its target is known: forward
; branches always take a 32-bit displacement; backward branches take an
; 8-bit displacement when it reaches.  Displacements of 32 bits reach
; anywhere in a code vector, so there is no long mode.
;
; The encoder emits the encodings that are shortest or customary; it
; need not agree with GNU as byte for byte (test/Asm/x86-64 compares
; disassemblies).

(define (cant.label as L)
  (emit-label! as L))

(define (cant.end as)
  #t)

; Assumes that exceptions are non-continuable, as does the AArch64 back
; end.  An additional code is always passed to millicode in TMP0.
;
; The return address of the call is a return point too: millicode saves
; it (relative to the start of the code) in globals[G_RETADDR], which
; the garbage collector scans, so it must be aligned like the return
; points of Scheme calls.

(define (cant.trap as code name . rest)
  (if (not (null? rest))
      (x86-64.mov-ri as x86-64.TMP0 (car rest)))
  (let ((call (x86-64.call-m-bytes (x86-64.m x86-64.GLOBALS code))))
    (x86-64.alignment as $bytewidth.code-align (length call))
    (x86-64.emit as call)))

(define (cant.nop as)
  (x86-64.NOP as))

(define (cant.ldi as base offset rd)
  (x86-64.load as (x86-64.reg rd) (x86-64.m (x86-64.reg base) offset)))

(define (cant.ldbiz as base offset rd)
  (x86-64.load8z as (x86-64.reg rd) (x86-64.m (x86-64.reg base) offset)))

(define (cant.ldwiz as base offset rd)
  (x86-64.load32 as (x86-64.reg rd) (x86-64.m (x86-64.reg base) offset)))

(define (cant.sti as rs base offset)
  (x86-64.store as (x86-64.m (x86-64.reg base) offset) (x86-64.reg rs)))

(define (cant.stzi as base offset)
  (x86-64.store-imm as (x86-64.m (x86-64.reg base) offset) 0))

(define (cant.stbi as rs base offset)
  (x86-64.store8 as (x86-64.m (x86-64.reg base) offset) (x86-64.reg rs)))

(define (cant.stwi as rs base offset)
  (x86-64.store32 as (x86-64.m (x86-64.reg base) offset) (x86-64.reg rs)))

(define (cant.movi as imm rd)
  (x86-64.mov-ri as (x86-64.reg rd) imm))

(define (cant.mov as rs rd)
  (x86-64.mov-rr as (x86-64.reg rd) (x86-64.reg rs)))

(define (cant.ldrtn/return as offset)
  (x86-64.jmp-m as (x86-64.m x86-64.STKP offset)))

(define (cant.setrtn/strtn as L offset)
  (x86-64.lea-label as x86-64.TMP1 L 0)
  (x86-64.store as (x86-64.m x86-64.STKP offset) x86-64.TMP1))

; Leave in TMP1 the address of L within the code vector of REG0, or
; the start of that code vector if L is #f.

(define (x86-64.code-address as L)
  (let ((bias (- $bytewidth.wordsize $tag.bytevector-tag)))
    (x86-64.load as x86-64.TMP1
                 (x86-64.m (x86-64.reg $r.reg0) $p.codevector))
    (cond ((not L)
           (x86-64.add-ri as x86-64.TMP1 bias))
          ((label-value as L)
           (x86-64.add-ri as x86-64.TMP1 (+ bias (label-value as L))))
          (else
           (x86-64.add-label as x86-64.TMP1 L bias)))))

(define (cant.jump as L)
  (x86-64.code-address as L)
  (x86-64.jmp-r as x86-64.TMP1))

; Set the return address of the instruction after the jump, store it
; in the frame at byte offset "offset", and jump.  The sequence must
; be aligned so that the return point has the required alignment.
;
; If L is #f then it denotes the start of the code vector, otherwise
; it's a label within that code vector.

(define (cant.setrtn/strtn/jump as L align offset)
  (x86-64.code-address as L)
  (let* ((store (x86-64.store-bytes (x86-64.m x86-64.STKP offset) x86-64.TMP2))
         (jump  (x86-64.jmp-r-bytes x86-64.TMP1))
         (k     (+ (length store) (length jump))))
    (x86-64.alignment as align (+ 7 k))
    (x86-64.lea-rip as x86-64.TMP2 k)
    (x86-64.emit as store)
    (x86-64.emit as jump)
    (assert (zero? (remainder (here as) align)))))

; Set the return address of the instruction after the branch, store it
; in the frame at byte offset "offset", and branch.  The sequence must
; be aligned so that the return point has the required alignment.

(define (cant.setrtn/strtn/branch as L align offset)
  (let* ((store (x86-64.store-bytes (x86-64.m x86-64.STKP offset) x86-64.TMP2))
         (k     (+ (length store) 5)))
    (x86-64.alignment as align (+ 7 k))
    (x86-64.lea-rip as x86-64.TMP2 k)
    (x86-64.emit as store)
    (x86-64.jmp-label as L #t)
    (assert (zero? (remainder (here as) align)))))

(define (cant.ba as L)
  (x86-64.jmp-label as L #f))

(define (cant.bcc as condition r1 r2 L)
  (x86-64.bccx as condition (x86-64.reg r1) (x86-64.reg r2) #f L))

(define (cant.bcci as condition r imm L)
  (x86-64.bccx as condition (x86-64.reg r) imm #t L))

(define (cant.setcc as condition r1 r2 rd)
  (x86-64.setccx as condition (x86-64.reg rd) (x86-64.reg r1) (x86-64.reg r2) #f))

(define (cant.setcci as condition r imm rd)
  (x86-64.setccx as condition (x86-64.reg rd) (x86-64.reg r) imm #t))

(define (cant.addi as rs imm rd)
  (x86-64.addi as (x86-64.reg rd) (x86-64.reg rs) imm))

(define (cant.subi as rs imm rd)
  (x86-64.addi as (x86-64.reg rd) (x86-64.reg rs) (- imm)))

; The overflow-checking operations compute into TMP1 and move the
; result to rd only if there was no overflow, so rd is not updated on
; overflow.

(define (cant.addi/bnv as rs imm rd L)
  (x86-64.op-imm-into-tmp1 as x86-64.op.add (x86-64.reg rs) imm)
  (x86-64.move-if-no-overflow as (x86-64.reg rd) x86-64.cc.NO L))

(define (cant.subi/bnv as rs imm rd L)
  (x86-64.op-imm-into-tmp1 as x86-64.op.sub (x86-64.reg rs) imm)
  (x86-64.move-if-no-overflow as (x86-64.reg rd) x86-64.cc.NO L))

(define (cant.add/bnv as rs1 rs2 rd L)
  (x86-64.op-into-tmp1 as x86-64.op.add (x86-64.reg rs1) (x86-64.reg rs2))
  (x86-64.move-if-no-overflow as (x86-64.reg rd) x86-64.cc.NO L))

(define (cant.sub/bnv as rs1 rs2 rd L)
  (x86-64.op-into-tmp1 as x86-64.op.sub (x86-64.reg rs1) (x86-64.reg rs2))
  (x86-64.move-if-no-overflow as (x86-64.reg rd) x86-64.cc.NO L))

; The /bov forms (see pass5p2.sch) branch on overflow, so the common
; case falls through.  When rd is not an operand the result goes to rd
; directly: on overflow rd is garbage, but the slow paths use only the
; operands.

(define (cant.add/bov as rs1 rs2 rd L)
  (x86-64.op/bov as x86-64.op.add rs1 rs2 rd L))

(define (cant.sub/bov as rs1 rs2 rd L)
  (x86-64.op/bov as x86-64.op.sub rs1 rs2 rd L))

(define (cant.addi/bov as rs imm rd L)
  (x86-64.opi/bov as x86-64.op.add rs imm rd L))

(define (cant.subi/bov as rs imm rd L)
  (x86-64.opi/bov as x86-64.op.sub rs imm rd L))

(define (x86-64.op/bov as op rs1 rs2 rd L)
  (let ((rs1 (x86-64.reg rs1))
        (rs2 (x86-64.reg rs2))
        (rd  (x86-64.reg rd)))
    (if (or (= rd rs1) (= rd rs2))
        (begin
          (x86-64.op-into-tmp1 as op rs1 rs2)
          (x86-64.move-if-no-overflow as rd x86-64.cc.O L))
        (begin
          (x86-64.mov-rr as rd rs1)
          (x86-64.alu-rr as op rd rs2)
          (x86-64.jcc-label as x86-64.cc.O L)))))

(define (x86-64.opi/bov as op rs imm rd L)
  (let ((rs (x86-64.reg rs))
        (rd (x86-64.reg rd)))
    (if (or (= rd rs) (not (x86-64.s32? imm)))
        (begin
          (x86-64.op-imm-into-tmp1 as op rs imm)
          (x86-64.move-if-no-overflow as rd x86-64.cc.O L))
        (begin
          (x86-64.mov-rr as rd rs)
          (x86-64.alu-ri as op rd imm)
          (x86-64.jcc-label as x86-64.cc.O L)))))

; IMUL with two operands sets OF when the product does not fit in 64
; bits.

(define (cant.mul as rs1 rs2 rd)
  (let ((rs1 (x86-64.reg rs1))
        (rs2 (x86-64.reg rs2))
        (rd  (x86-64.reg rd)))
    (cond ((= rd rs1) (x86-64.imul-rr as rd rs2))
          ((= rd rs2) (x86-64.imul-rr as rd rs1))
          (else
           (x86-64.mov-rr as rd rs1)
           (x86-64.imul-rr as rd rs2)))))

(define (cant.mul/bnv as rs1 rs2 rd L)
  (x86-64.mov-rr  as x86-64.TMP1 (x86-64.reg rs1))
  (x86-64.imul-rr as x86-64.TMP1 (x86-64.reg rs2))
  (x86-64.move-if-no-overflow as (x86-64.reg rd) x86-64.cc.NO L))

(define (cant.mul/bov as rs1 rs2 rd L)
  (x86-64.mov-rr  as x86-64.TMP1 (x86-64.reg rs1))
  (x86-64.imul-rr as x86-64.TMP1 (x86-64.reg rs2))
  (x86-64.move-if-no-overflow as (x86-64.reg rd) x86-64.cc.O L))

; Quotient and remainder of two fixnums (64-bit targets only).  Tagged
; fixnums divide to the untagged quotient (4x/4y = x/y, truncated), and
; 4x - (x/y)*4y = 4(x rem y) is the tagged remainder.  Both branch to
; L, leaving rd unchanged, if rs2 is 0; the quotient also if it is not
; a fixnum (the most negative fixnum divided by -1).  IDIV does not
; trap: the divisor is a nonzero fixnum, so a multiple of 4.

(define (x86-64.fxdivide as rs1 rs2 L)
  (let ((rs1 (x86-64.reg rs1))
        (rs2 (x86-64.reg rs2)))
    (x86-64.test-rr   as rs2 rs2)
    (x86-64.jcc-label as x86-64.cc.E L)
    (x86-64.mov-rr    as x86-64.TMP1 rs1)
    (x86-64.CQO       as)
    (x86-64.idiv      as rs2)))

(define (cant.fxquotient/bov as rs1 rs2 rd L)
  (x86-64.fxdivide  as rs1 rs2 L)
  (x86-64.lea       as x86-64.TMP2 (x86-64.msib #f x86-64.TMP1 4 0))
  (x86-64.shift-ri  as x86-64.shift.sar x86-64.TMP2 2)
  (x86-64.alu-rr    as x86-64.op.cmp x86-64.TMP2 x86-64.TMP1)
  (x86-64.jcc-label as x86-64.cc.NE L)
  (x86-64.lea       as (x86-64.reg rd) (x86-64.msib #f x86-64.TMP1 4 0)))

(define (cant.fxremainder/bov as rs1 rs2 rd L)
  (x86-64.fxdivide  as rs1 rs2 L)
  (x86-64.mov-rr    as (x86-64.reg rd) x86-64.TMP2))

(define (cant.shli as rs imm rd)
  (x86-64.shifti as x86-64.shift.shl rs imm rd))

(define (cant.shri as rs imm rd)
  (x86-64.shifti as x86-64.shift.shr rs imm rd))

(define (cant.shrai as rs imm rd)
  (x86-64.shifti as x86-64.shift.sar rs imm rd))

(define (x86-64.shifti as kind rs imm rd)
  (assert (<= 0 imm 63))
  (let ((rs (x86-64.reg rs))
        (rd (x86-64.reg rd)))
    (x86-64.mov-rr as rd rs)
    (if (not (zero? imm))
        (x86-64.shift-ri as kind rd imm))))

(define (cant.andni as rs imm rd)
  (let ((rs (x86-64.reg rs))
        (rd (x86-64.reg rd))
        (m  (- -1 imm)))
    (if (x86-64.s32? m)
        (begin
          (x86-64.mov-rr as rd rs)
          (x86-64.alu-ri as x86-64.op.and rd m))
        (begin
          (x86-64.mov-ri as x86-64.TMP1 m)
          (x86-64.op3 as x86-64.op.and #t rs x86-64.TMP1 rd)))))

(define (cant.add as rs1 rs2 rd)
  (let ((rs1 (x86-64.reg rs1))
        (rs2 (x86-64.reg rs2))
        (rd  (x86-64.reg rd)))
    (if (or (= rd rs1) (= rd rs2))
        (x86-64.op3 as x86-64.op.add #t rs1 rs2 rd)
        (x86-64.lea as rd (x86-64.msib rs1 rs2 1 0)))))

; rd = rs1 + (rs2 << shift)

(define (cant.addshli as rs1 rs2 shift rd)
  (assert (<= 0 shift 3))
  (x86-64.lea as (x86-64.reg rd)
              (x86-64.msib (x86-64.reg rs1) (x86-64.reg rs2) (expt 2 shift) 0)))

(define (cant.sub as rs1 rs2 rd)
  (x86-64.op3 as x86-64.op.sub #f (x86-64.reg rs1) (x86-64.reg rs2) (x86-64.reg rd)))

(define (cant.and as rs1 rs2 rd)
  (x86-64.op3 as x86-64.op.and #t (x86-64.reg rs1) (x86-64.reg rs2) (x86-64.reg rd)))

(define (cant.or as rs1 rs2 rd)
  (x86-64.op3 as x86-64.op.or #t (x86-64.reg rs1) (x86-64.reg rs2) (x86-64.reg rd)))

(define (cant.xor as rs1 rs2 rd)
  (x86-64.op3 as x86-64.op.xor #t (x86-64.reg rs1) (x86-64.reg rs2) (x86-64.reg rd)))

; Shifts by a register count take the count in cl, which is TMP0.  The
; value is shifted in TMP1, and TMP0 is restored, so any of the
; operands may be TMP0.

(define (cant.shl as rs1 rs2 rd)
  (x86-64.shiftv as x86-64.shift.shl rs1 rs2 rd))

(define (cant.shr as rs1 rs2 rd)
  (x86-64.shiftv as x86-64.shift.shr rs1 rs2 rd))

(define (cant.shra as rs1 rs2 rd)
  (x86-64.shiftv as x86-64.shift.sar rs1 rs2 rd))

(define (x86-64.shiftv as kind rs1 rs2 rd)
  (x86-64.mov-rr    as x86-64.TMP1 (x86-64.reg rs1))
  (x86-64.mov-rr    as x86-64.TMP2 (x86-64.reg rs2))
  (x86-64.xchg      as x86-64.TMP0 x86-64.TMP2)
  (x86-64.shift-cl  as kind x86-64.TMP1)
  (x86-64.xchg      as x86-64.TMP0 x86-64.TMP2)
  (x86-64.mov-rr    as (x86-64.reg rd) x86-64.TMP1))

(define (cant.not as rs rd)
  (let ((rs (x86-64.reg rs))
        (rd (x86-64.reg rd)))
    (x86-64.mov-rr as rd rs)
    (x86-64.unary as x86-64.unary.not rd)))

; Floating operations.

(define (cant.fldi as base offset frd)
  (x86-64.sse as #xF2 #f #x10 (x86-64.freg frd)
              (x86-64.m (x86-64.reg base) offset)))

(define (cant.fsti as frs base offset)
  (x86-64.sse as #xF2 #f #x11 (x86-64.freg frs)
              (x86-64.m (x86-64.reg base) offset)))

(define (cant.fadd as frs1 frs2 frd)
  (x86-64.fop as #x58 #t frs1 frs2 frd))

(define (cant.fsub as frs1 frs2 frd)
  (x86-64.fop as #x5C #f frs1 frs2 frd))

(define (cant.fmul as frs1 frs2 frd)
  (x86-64.fop as #x59 #t frs1 frs2 frd))

(define (cant.fdiv as frs1 frs2 frd)
  (x86-64.fop as #x5E #f frs1 frs2 frd))

(define (x86-64.fop as op commutative? frs1 frs2 frd)
  (let ((frs1 (x86-64.freg frs1))
        (frs2 (x86-64.freg frs2))
        (frd  (x86-64.freg frd)))
    (cond ((= frd frs1)
           (x86-64.sse as #xF2 #f op frd frs2))
          ((and commutative? (= frd frs2))
           (x86-64.sse as #xF2 #f op frd frs1))
          ((= frd frs2)
           (x86-64.movapd as x86-64.FTMP frs1)
           (x86-64.sse    as #xF2 #f op x86-64.FTMP frs2)
           (x86-64.movapd as frd x86-64.FTMP))
          (else
           (x86-64.movapd as frd frs1)
           (x86-64.sse    as #xF2 #f op frd frs2)))))

; The value of the fixnum in rs, as a double.

(define (cant.fixnum->flonum as rs frd)
  (x86-64.mov-rr   as x86-64.TMP1 (x86-64.reg rs))
  (x86-64.shift-ri as x86-64.shift.sar x86-64.TMP1 2)
  (x86-64.sse      as #xF2 #t #x2A (x86-64.freg frd) x86-64.TMP1))

(define fence.inline-fixnum->flonum? #t)

; UCOMISD sets ZF, PF, and CF all to 1 for unordered operands.  Above
; and above-or-equal (CF=0) are false then, so less and less-or-equal
; compare the operands the other way around; equal also needs PF=0,
; and not-equal is true when PF=1.  All the conditions are false for
; NaNs except not-equal, as required by Scheme's = and < etc.

(define (cant.fsetcc as condition frs1 frs2 rd)
  (let ((a  (x86-64.freg frs1))
        (b  (x86-64.freg frs2))
        (rd (x86-64.reg rd)))
    (case condition
      ((equal)
       (x86-64.ucomisd as a b)
       (x86-64.setcc   as x86-64.cc.E x86-64.TMP1)
       (x86-64.setcc   as x86-64.cc.NP x86-64.TMP2)
       (x86-64.alu8-rr as x86-64.op.and x86-64.TMP1 x86-64.TMP2)
       (x86-64.boolean-from-tmp1 as rd))
      ((not-equal)
       (x86-64.ucomisd as a b)
       (x86-64.setcc   as x86-64.cc.NE x86-64.TMP1)
       (x86-64.setcc   as x86-64.cc.P x86-64.TMP2)
       (x86-64.alu8-rr as x86-64.op.or x86-64.TMP1 x86-64.TMP2)
       (x86-64.boolean-from-tmp1 as rd))
      ((less)
       (x86-64.ucomisd as b a)
       (x86-64.set-boolean as rd x86-64.cc.A))
      ((less-or-equal)
       (x86-64.ucomisd as b a)
       (x86-64.set-boolean as rd x86-64.cc.AE))
      ((greater)
       (x86-64.ucomisd as a b)
       (x86-64.set-boolean as rd x86-64.cc.A))
      ((greater-or-equal)
       (x86-64.ucomisd as a b)
       (x86-64.set-boolean as rd x86-64.cc.AE))
      (else (error 'fsetcc "Bad condition: " condition)))))

;;; Utilities

; Emit NOPs so that if 'bytes' bytes of code follows the NOPs then
; the end of those bytes will be aligned on an 'align' boundary.

(define (x86-64.alignment as align bytes)
  (let loop ()
    (if (not (zero? (remainder (+ (here as) bytes) align)))
        (begin
          (cant.nop as)
          (loop)))))

; Map a MAL register to an x86-64 hardware register.

(define x86-64.reg
  (let* ((offset (abs (min $r.tmp $r.globals $r.stkp $r.second $r.result)))
         (v      (make-vector (+ *nregs* offset) -1))
         (regs   (vector x86-64.REG0 x86-64.REG1 x86-64.REG2 x86-64.REG3
                         x86-64.REG4 x86-64.REG5 x86-64.REG6 x86-64.REG7)))
    (vector-set! v (+ offset $r.tmp) x86-64.TMP0)
    (vector-set! v (+ offset $r.globals) x86-64.GLOBALS)
    (vector-set! v (+ offset $r.stkp) x86-64.STKP)
    (vector-set! v (+ offset $r.second) x86-64.SECOND)
    (vector-set! v (+ offset $r.result) x86-64.RESULT)
    (do ((i 0 (+ i 1)))
        ((= i *nregs*))
      (vector-set! v (+ offset i $r.reg0) (vector-ref regs i)))
    (set! fence.records-registers? #t)
    (lambda (r)
      (if (>= r $r.reg0)
          (fence.note-register! (- r $r.reg0)))
      (vector-ref v (+ r offset)))))

; Floating registers F0..F3 are xmm0..xmm3.

(define (x86-64.freg x) x)

(define x86-64.FTMP 15)

(define x86-64.2^64 18446744073709551616)
(define x86-64.2^32 4294967296)

(define (x86-64.s8? v) (and (exact-integer? v) (<= -128 v 127)))
(define (x86-64.s32? v) (and (exact-integer? v) (<= -2147483648 v 2147483647)))
(define (x86-64.u32? v) (and (exact-integer? v) (<= 0 v 4294967295)))

; rd = rs + imm, for any 64-bit imm.

(define (x86-64.addi as rd rs imm)
  (cond ((zero? imm)
         (x86-64.mov-rr as rd rs))
        ((and (= rd rs) (x86-64.s32? imm))
         (x86-64.add-ri as rd imm))
        ((x86-64.s32? imm)
         (x86-64.lea as rd (x86-64.m rs imm)))
        (else
         (x86-64.mov-ri as x86-64.TMP1 imm)
         (x86-64.lea as rd (x86-64.msib rs x86-64.TMP1 1 0)))))

(define (x86-64.add-ri as rd imm)
  (x86-64.alu-ri as x86-64.op.add rd imm))

; TMP1 = rs op rs2, and TMP1 = rs op imm, setting the flags.

(define (x86-64.op-into-tmp1 as op rs1 rs2)
  (x86-64.mov-rr as x86-64.TMP1 rs1)
  (x86-64.alu-rr as op x86-64.TMP1 rs2))

(define (x86-64.op-imm-into-tmp1 as op rs imm)
  (x86-64.mov-rr as x86-64.TMP1 rs)
  (if (x86-64.s32? imm)
      (x86-64.alu-ri as op x86-64.TMP1 imm)
      (begin
        (x86-64.mov-ri as x86-64.TMP2 imm)
        (x86-64.alu-rr as op x86-64.TMP1 x86-64.TMP2))))

; After a flag-setting operation that left its result in TMP1: move
; TMP1 to rd if there was no overflow, then branch to L on condition
; cc (NO or O).  CMOV does not change the flags.

(define (x86-64.move-if-no-overflow as rd cc L)
  (x86-64.cmov      as x86-64.cc.NO rd x86-64.TMP1)
  (x86-64.jcc-label as cc L))

; rd = rs1 op rs2 for the two-operand ALU instructions.

(define (x86-64.op3 as op commutative? rs1 rs2 rd)
  (cond ((= rd rs1)
         (x86-64.alu-rr as op rd rs2))
        ((and commutative? (= rd rs2))
         (x86-64.alu-rr as op rd rs1))
        ((= rd rs2)
         (x86-64.mov-rr as x86-64.TMP1 rs1)
         (x86-64.alu-rr as op x86-64.TMP1 rs2)
         (x86-64.mov-rr as rd x86-64.TMP1))
        (else
         (x86-64.mov-rr as rd rs1)
         (x86-64.alu-rr as op rd rs2))))

(define (x86-64.bccx as condition r1 src2 src2-imm? L)
  (x86-64.generate-condition as condition r1 src2 src2-imm?)
  (x86-64.jcc-label as (x86-64.condition-code condition) L))

(define (x86-64.setccx as condition rd r1 src2 src2-imm?)
  (x86-64.generate-condition as condition r1 src2 src2-imm?)
  (x86-64.set-boolean as rd (x86-64.condition-code condition)))

; rd = cc ? #t : #f.  The representations of #t and #f differ only
; in bit 2.

(define (x86-64.set-boolean as rd cc)
  (x86-64.setcc as cc x86-64.TMP1)
  (x86-64.boolean-from-tmp1 as rd))

(define (x86-64.boolean-from-tmp1 as rd)
  (assert (= (- $imm.true $imm.false) 4))
  (x86-64.movzx8 as x86-64.TMP1 x86-64.TMP1)
  (x86-64.lea    as rd (x86-64.msib #f x86-64.TMP1 4 $imm.false)))

(define (x86-64.condition-code condition)
  (case condition
    ((equal low1-equal low2-equal low3-equal low8-equal) x86-64.cc.E)
    ((not-equal low1-not-equal low2-not-equal low3-not-equal low8-not-equal) x86-64.cc.NE)
    ((less) x86-64.cc.L)
    ((greater) x86-64.cc.G)
    ((less-or-equal) x86-64.cc.LE)
    ((greater-or-equal) x86-64.cc.GE)
    ((unsigned-less) x86-64.cc.B)
    ((unsigned-greater-or-equal) x86-64.cc.AE)
    (else (error 'x86-64.condition-code "Bad condition: " condition))))

(define (x86-64.generate-condition as condition r1 src2 src2-imm?)
  (let ((mask (case condition
                ((low1-equal low1-not-equal) 1)
                ((low2-equal low2-not-equal) 3)
                ((low3-equal low3-not-equal) 7)
                ((low8-equal low8-not-equal) 255)
                (else #f))))
    (cond ((and mask src2-imm? (= src2 0))
           (x86-64.test8-ri as r1 mask))
          (mask
           (x86-64.mov-rr as x86-64.TMP1 r1)
           (x86-64.alu-ri as x86-64.op.and x86-64.TMP1 mask)
           (cond ((not src2-imm?)
                  (x86-64.alu-rr as x86-64.op.cmp x86-64.TMP1 src2))
                 (else
                  ;; The precondition for using low<n>-{not-,}equal is
                  ;; that the immediate, if any, fits in n bits.
                  (assert (<= 0 src2 255))
                  (x86-64.alu-ri as x86-64.op.cmp x86-64.TMP1 src2))))
          ((not src2-imm?)
           (x86-64.alu-rr as x86-64.op.cmp r1 src2))
          ((and (= src2 0) (memq condition '(equal not-equal)))
           (x86-64.test-rr as r1 r1))
          ((x86-64.s32? src2)
           (x86-64.alu-ri as x86-64.op.cmp r1 src2))
          (else
           (x86-64.mov-ri as x86-64.TMP1 src2)
           (x86-64.alu-rr as x86-64.op.cmp r1 x86-64.TMP1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; x86-64 machine assembler.
;
; Registers are hardware register numbers: rax=0, rcx=1, rdx=2, rbx=3,
; rsp=4, rbp=5, rsi=6, rdi=7, r8..r15=8..15; xmm registers likewise.
; A memory operand, made by x86-64.m or x86-64.msib, is a vector of a
; base register (or #f), an index register (or #f), a scale (1, 2, 4,
; or 8), and a displacement that fits in 32 bits.
;
; The encoders named ...-bytes return the instruction as a list of
; bytes; the others emit it.  Byte values are computed with plain
; arithmetic, so the code works on hosts with 30-bit fixnums.

(define x86-64.cc.O  0)
(define x86-64.cc.NO 1)
(define x86-64.cc.B  2)
(define x86-64.cc.AE 3)
(define x86-64.cc.E  4)
(define x86-64.cc.NE 5)
(define x86-64.cc.BE 6)
(define x86-64.cc.A  7)
(define x86-64.cc.S  8)
(define x86-64.cc.NS 9)
(define x86-64.cc.P  10)
(define x86-64.cc.NP 11)
(define x86-64.cc.L  12)
(define x86-64.cc.GE 13)
(define x86-64.cc.LE 14)
(define x86-64.cc.G  15)

; ALU operations: the /digit of the immediate forms; the register form
; r/m64,r64 has opcode 8*digit+1.

(define x86-64.op.add 0)
(define x86-64.op.or  1)
(define x86-64.op.and 4)
(define x86-64.op.sub 5)
(define x86-64.op.xor 6)
(define x86-64.op.cmp 7)

(define x86-64.shift.shl 4)
(define x86-64.shift.shr 5)
(define x86-64.shift.sar 7)

(define x86-64.unary.not 2)
(define x86-64.unary.neg 3)

(define (x86-64.m base disp)
  (vector base #f 1 disp))

(define (x86-64.msib base index scale disp)
  (vector base index scale disp))

(define (x86-64.emit as bytes)
  (let ((bv (make-bytevector (length bytes))))
    (do ((i 0 (+ i 1))
         (bytes bytes (cdr bytes)))
        ((null? bytes))
      (bytevector-set! bv i (car bytes)))
    (emit! as bv)))

; The little-endian bytes of v, n bytes wide, v signed or unsigned.

(define (x86-64.le-bytes v n)
  (let loop ((v (if (negative? v) (+ v (expt 2 (* 8 n))) v)) (n n))
    (if (zero? n)
        '()
        (cons (remainder v 256) (loop (quotient v 256) (- n 1))))))

; Store the 32-bit value v, signed, at loc in the code vector cv.

(define (x86-64.patch32! cv loc v)
  (assert (x86-64.s32? v))
  (do ((i 0 (+ i 1))
       (bytes (x86-64.le-bytes v 4) (cdr bytes)))
      ((= i 4))
    (bytevector-set! cv (+ loc i) (car bytes))))

(define (x86-64.lo3 r) (remainder r 8))
(define (x86-64.hi1 r) (quotient r 8))

; ModRM, SIB, and displacement bytes for a register (or /digit) reg and
; an operand rm that is a register or a memory operand.  Returns the
; bytes and, as the REX.X and REX.B bits, the high bits of the index
; and base registers.

(define (x86-64.modrm reg rm k)
  (define (modrm mod reg rm) (+ (* 64 mod) (* 8 (x86-64.lo3 reg)) rm))
  (define (sib scale index base)
    (+ (* 64 (case scale ((1) 0) ((2) 1) ((4) 2) ((8) 3)
                (else (error 'x86-64.modrm "Bad scale " scale))))
       (* 8 index)
       base))
  (define (disp-mod base disp)
    (cond ((and (zero? disp) (not (= (x86-64.lo3 base) 5))) 0)
          ((x86-64.s8? disp) 1)
          (else 2)))
  (define (disp-bytes mod disp)
    (case mod
      ((0) '())
      ((1) (x86-64.le-bytes disp 1))
      (else (x86-64.le-bytes disp 4))))
  (if (not (vector? rm))
      (k (list (modrm 3 reg (x86-64.lo3 rm))) 0 (x86-64.hi1 rm))
      (let ((base  (vector-ref rm 0))
            (index (vector-ref rm 1))
            (scale (vector-ref rm 2))
            (disp  (vector-ref rm 3)))
        (assert (x86-64.s32? disp))
        (cond (index
               (assert (not (= index 4)))
               (if (not base)
                   (k (append (list (modrm 0 reg 4)
                                    (sib scale (x86-64.lo3 index) 5))
                              (x86-64.le-bytes disp 4))
                      (x86-64.hi1 index) 0)
                   (let ((mod (disp-mod base disp)))
                     (k (append (list (modrm mod reg 4)
                                      (sib scale (x86-64.lo3 index)
                                           (x86-64.lo3 base)))
                                (disp-bytes mod disp))
                        (x86-64.hi1 index) (x86-64.hi1 base)))))
              ((= (x86-64.lo3 base) 4)  ; rsp or r12: needs a SIB byte
               (let ((mod (disp-mod base disp)))
                 (k (append (list (modrm mod reg 4) #x24)
                            (disp-bytes mod disp))
                    0 (x86-64.hi1 base))))
              (else
               (let ((mod (disp-mod base disp)))
                 (k (cons (modrm mod reg (x86-64.lo3 base))
                          (disp-bytes mod disp))
                    0 (x86-64.hi1 base))))))))

; The general instruction: legacy prefixes, REX if needed, the opcode
; bytes, ModRM etc, and immediate bytes.  w is REX.W; byte-regs is a
; list of registers used as byte registers, which need a REX prefix to
; denote spl, bpl, sil, and dil rather than ah, ch, dh, and bh.

(define (x86-64.encode prefixes w byte-regs opcodes reg rm imm)
  (x86-64.modrm
   reg rm
   (lambda (modrm-bytes x b)
     (let* ((r   (x86-64.hi1 reg))
            (rex (+ #x40 (if w 8 0) (* 4 r) (* 2 x) b))
            (need-rex? (or (> rex #x40)
                           (let loop ((l byte-regs))
                             (and (pair? l)
                                  (or (<= 4 (car l) 7) (loop (cdr l))))))))
       (append prefixes
               (if need-rex? (list rex) '())
               opcodes
               modrm-bytes
               imm)))))

; As encode, for opcodes with the register in the low three bits.

(define (x86-64.encode+r w opcode r imm)
  (append (if (or w (> r 7))
              (list (+ #x40 (if w 8 0) (x86-64.hi1 r)))
              '())
          (list (+ opcode (x86-64.lo3 r)))
          imm))

;;; Moves, loads, and stores.

(define (x86-64.mov-rr as rd rs)
  (if (not (= rd rs))
      (x86-64.emit as (x86-64.encode '() #t '() '(#x89) rs rd '()))))

; rd = imm: a 32-bit move (zero-extending) when the value fits, a
; sign-extended 32-bit immediate, or a 64-bit immediate.  XOR for zero
; would change the flags.

(define (x86-64.mov-ri as rd imm)
  (let ((u (if (negative? imm) (+ x86-64.2^64 imm) imm)))
    (assert (<= 0 u (- x86-64.2^64 1)))
    (x86-64.emit
     as
     (cond ((x86-64.u32? imm)
            (x86-64.encode+r #f #xB8 rd (x86-64.le-bytes imm 4)))
           ((x86-64.s32? imm)
            (x86-64.encode '() #t '() '(#xC7) 0 rd (x86-64.le-bytes imm 4)))
           (else
            (x86-64.encode+r #t #xB8 rd (x86-64.le-bytes u 8)))))))

(define (x86-64.load as rd m)
  (x86-64.emit as (x86-64.encode '() #t '() '(#x8B) rd m '())))

(define (x86-64.load32 as rd m)            ; zero-extends
  (x86-64.emit as (x86-64.encode '() #f '() '(#x8B) rd m '())))

(define (x86-64.load8z as rd m)            ; zero-extends
  (x86-64.emit as (x86-64.encode '() #f '() '(#x0F #xB6) rd m '())))

(define (x86-64.store-bytes m rs)
  (x86-64.encode '() #t '() '(#x89) rs m '()))

(define (x86-64.store as m rs)
  (x86-64.emit as (x86-64.store-bytes m rs)))

(define (x86-64.store32 as m rs)
  (x86-64.emit as (x86-64.encode '() #f '() '(#x89) rs m '())))

(define (x86-64.store8 as m rs)
  (x86-64.emit as (x86-64.encode '() #f (list rs) '(#x88) rs m '())))

; A 32-bit immediate, sign-extended, into a 64-bit memory word.

(define (x86-64.store-imm as m imm)
  (assert (x86-64.s32? imm))
  (x86-64.emit as (x86-64.encode '() #t '() '(#xC7) 0 m (x86-64.le-bytes imm 4))))

(define (x86-64.lea as rd m)
  (x86-64.emit as (x86-64.encode '() #t '() '(#x8D) rd m '())))

(define (x86-64.xchg as r1 r2)
  (x86-64.emit as (x86-64.encode '() #t '() '(#x87) r2 r1 '())))

(define (x86-64.movzx8 as rd rs)
  (x86-64.emit as (x86-64.encode '() #f (list rs) '(#x0F #xB6) rd rs '())))

(define (x86-64.cmov as cc rd rs)
  (x86-64.emit as (x86-64.encode '() #t '() (list #x0F (+ #x40 cc)) rd rs '())))

(define (x86-64.setcc as cc r)
  (x86-64.emit as (x86-64.encode '() #f (list r) (list #x0F (+ #x90 cc)) 0 r '())))

;;; Arithmetic.

(define (x86-64.alu-rr as op rd rs)
  (x86-64.emit as (x86-64.encode '() #t '() (list (+ (* 8 op) 1)) rs rd '())))

(define (x86-64.alu8-rr as op rd rs)
  (x86-64.emit as (x86-64.encode '() #f (list rd rs) (list (* 8 op)) rs rd '())))

; rax (TMP1) has a shorter form for 32-bit immediates.

(define (x86-64.alu-ri as op rd imm)
  (assert (x86-64.s32? imm))
  (x86-64.emit
   as
   (cond ((x86-64.s8? imm)
          (x86-64.encode '() #t '() '(#x83) op rd (x86-64.le-bytes imm 1)))
         ((= rd 0)
          (cons #x48 (cons (+ (* 8 op) 5) (x86-64.le-bytes imm 4))))
         (else
          (x86-64.encode '() #t '() '(#x81) op rd (x86-64.le-bytes imm 4))))))

(define (x86-64.test-rr as r1 r2)
  (x86-64.emit as (x86-64.encode '() #t '() '(#x85) r2 r1 '())))

; Test the low byte of r against an 8-bit mask.

(define (x86-64.test8-ri as r imm)
  (assert (<= 0 imm 255))
  (x86-64.emit as (x86-64.encode '() #f (list r) '(#xF6) 0 r (list imm))))

(define (x86-64.shift-ri as kind rd n)
  (assert (<= 1 n 63))
  (x86-64.emit
   as
   (if (= n 1)
       (x86-64.encode '() #t '() '(#xD1) kind rd '())
       (x86-64.encode '() #t '() '(#xC1) kind rd (list n)))))

(define (x86-64.shift-cl as kind rd)
  (x86-64.emit as (x86-64.encode '() #t '() '(#xD3) kind rd '())))

(define (x86-64.unary as kind r)
  (x86-64.emit as (x86-64.encode '() #t '() '(#xF7) kind r '())))

(define (x86-64.imul-rr as rd rs)
  (x86-64.emit as (x86-64.encode '() #t '() '(#x0F #xAF) rd rs '())))

(define (x86-64.idiv as rs)
  (x86-64.emit as (x86-64.encode '() #t '() '(#xF7) 7 rs '())))

(define (x86-64.CQO as)
  (x86-64.emit as '(#x48 #x99)))

(define (x86-64.NOP as)
  (x86-64.emit as '(#x90)))

;;; Control transfers.

(define (x86-64.jmp-r-bytes r)
  (x86-64.encode '() #f '() '(#xFF) 4 r '()))

(define (x86-64.jmp-r as r)
  (x86-64.emit as (x86-64.jmp-r-bytes r)))

(define (x86-64.jmp-m as m)
  (x86-64.emit as (x86-64.encode '() #f '() '(#xFF) 4 m '())))

(define (x86-64.call-m-bytes m)
  (x86-64.encode '() #f '() '(#xFF) 2 m '()))

; Branches to label L.  The short form (8-bit displacement) is used
; when L is known and near, else the long form (32-bit displacement).
; The displacement is relative to the end of the instruction.  If
; long? is true the long form is used anyway, so that the size of the
; instruction is known in advance.

(define (x86-64.jmp-label as L long?)
  (let ((loc (here as)))
    (if (and (label-value as L)
             (not long?)
             (x86-64.s8? (- (label-value as L) (+ loc 2))))
        (x86-64.emit as (list #xEB (car (x86-64.le-bytes
                                         (- (label-value as L) (+ loc 2)) 1))))
        (x86-64.branch-long as L '(#xE9)))))

(define (x86-64.jcc-label as cc L)
  (let ((loc (here as)))
    (if (and (label-value as L)
             (x86-64.s8? (- (label-value as L) (+ loc 2))))
        (x86-64.emit as (list (+ #x70 cc)
                              (car (x86-64.le-bytes
                                    (- (label-value as L) (+ loc 2)) 1))))
        (x86-64.branch-long as L (list #x0F (+ #x80 cc))))))

; Emit the opcode bytes followed by the 32-bit displacement of L
; relative to the end of the instruction.

(define (x86-64.branch-long as L opcodes)
  (x86-64.rel32 as opcodes L 0))

; Emit bytes followed by a 32-bit field holding the offset of label L,
; plus adjust, from the end of the instruction (rel32 and RIP-relative
; addressing).

(define (x86-64.rel32 as bytes L adjust)
  (let* ((loc (here as))
         (end (+ loc (length bytes) 4)))
    (if (label-value as L)
        (x86-64.emit as (append bytes (x86-64.le-bytes
                                       (- (+ (label-value as L) adjust) end) 4)))
        (begin
          (emit-fixup-proc!
           as (lambda (cv fixup-loc)
                (x86-64.patch32! cv (- end 4)
                                 (- (+ (label-value as L) adjust) end))))
          (x86-64.emit as (append bytes '(0 0 0 0)))))))

; rd = the address of label L plus adjust (LEA from RIP).

(define (x86-64.lea-label as rd L adjust)
  (x86-64.rel32 as (x86-64.lea-rip-prefix rd) L adjust))

; rd = the address k bytes past the end of this instruction, which is
; seven bytes long.

(define (x86-64.lea-rip as rd k)
  (x86-64.emit as (append (x86-64.lea-rip-prefix rd) (x86-64.le-bytes k 4))))

(define (x86-64.lea-rip-prefix rd)
  (list (+ #x48 (* 4 (x86-64.hi1 rd))) #x8D (+ 5 (* 8 (x86-64.lo3 rd)))))

; rd += the offset of label L plus adjust (ADD with a 32-bit
; immediate).

(define (x86-64.add-label as rd L adjust)
  (let ((prefix (list (+ #x48 (x86-64.hi1 rd)) #x81 (+ #xC0 (x86-64.lo3 rd)))))
    (if (label-value as L)
        (x86-64.emit as (append prefix (x86-64.le-bytes (+ adjust (label-value as L)) 4)))
        (let ((loc (here as)))
          (emit-fixup-proc!
           as (lambda (cv fixup-loc)
                (x86-64.patch32! cv (+ loc 3) (+ adjust (label-value as L)))))
          (x86-64.emit as (append prefix '(0 0 0 0)))))))

;;; Scalar double-precision floating point (SSE2).

; prefix 0F op /r with the xmm register in reg and rm a memory operand,
; an xmm register, or (cvtsi2sd, w true) a general register.

(define (x86-64.sse as prefix w op reg rm)
  (x86-64.emit as (x86-64.encode (list prefix) w '() (list #x0F op) reg rm '())))

(define (x86-64.movapd as xd xs)
  (if (not (= xd xs))
      (x86-64.sse as #x66 #f #x28 xd xs)))

(define (x86-64.ucomisd as xa xb)
  (x86-64.sse as #x66 #f #x2E xa xb))

; The features of the code this assembler produces (see
; Lib/Common/code-features.sch).  The version number in the target
; changes when the calling convention or the millicode interface does.
; Both barriers are always emitted.

(define (assembler-lop-features)
  (make-code-features 'lop
                      '((target fence-x86-64 1)
                        (word-size 64))
                      '(write-barrier satb-barrier)))

; eof

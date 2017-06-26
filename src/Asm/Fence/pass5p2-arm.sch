; Copyright 2012 Lars T Hansen.
;
; 23 August 2012 / lth@acm.org
;
; ARM-32 overrides for the Cant primitives.
;
; This implementation supports ARMv7-A and ARMv6T2-A, and supporting
; ARMv6-A in general is a matter of dealing with the use of MOVT.
;
; The ARM mapping has only hardware-backed MacScheme registers.
;
; There are six ARM registers available for MacScheme registers; with
; a little work we can usually make two more available (LR can be used
; for TMP1 and SP can be used for STKP, freeing R12 and R3).  On some
; operating systems R9 is also available, but the ABI makes that
; platform-dependent.
;
; For floating point operations we assume that FPSCR.LEN = 000 and
; FPSCR.STRIDE = 00, so that eg VADD performs a single addition,
; not a vector addition.
;
; About ARMv6:
;
; - Supports only VFPv2
;   - no VMOV
; - Some instructions introduced in ARMv6T2
;   - BFC, BFI, MOVT, SBFX, and UBFX; others.
; - Some instructions introduced in ARMv7:
;   - PLI, SDIV, UDIV; others.  (SDIV and UDIV are anyway optional in ARMv7-A)
;
; About R9:
;
; - According to Apple documentation, r9 was used for TLS on iOS 2.x
;   but is a scratch register since iOS 3:
;   http://developer.apple.com/library/ios/documentation/Xcode/Conceptual/iPhoneOSABIReference/iPhoneOSABIReference.pdf
;
; - GCC will supposedly use R9 for a global data segment pointer when
;   compiling position-independent code.  Some GCC versions allow the
;   use of -mpic-register= with r9 or r10.
;
; - On platforms that use it for TLS or data segment, R9 might
;   actually always be available unless signal handlers require it to
;   have a fixed value.  This needs to be investigated.
;
; About R13 (SP):
;
; - Using SP may require a red zone for signal handlers, as on the
;   x86.
;
; About R14 (LR):
;
; - The current mapping uses LR only to pass the return address on
;   millicode calls so LR is available pretty much always.  It's not
;   clear whether ARM implementations use the LR contents for
;   predicting branch addresses and will be confused (ie slow down) if
;   we use it as a general temp.  From the available documentation it
;   does not appear that way.
;
; TODO:
;
; - Numerous instruction selection improvements, search for TODO below.
;
; - Potentially popular ARMv6 systems such as the Raspberry Pi are ARMv6
;   based, with an ARM1176JZFS chip.  This chip does not implement the
;   ARMv6T2 extensions.  Thus we should find a way of removing the use
;   of MOVT, and avoid BFC, BFI, SBFX, and UBFX (not currently used).
;   (A MOVWI/MOVTI pair can be replaced by a PC-relative load of a datum
;   in the instruction stream, among other things.)
;
; - The ARM "Cortex-A Series Programmer's Guide" has some scattered
;   performance advice that we should pay attention to.  Notably, the
;   chips have a return address cache that is triggered by certain
;   instruction sequences (that we're not currently using).
;
; - Some obvious improvements are possible in certain common
;   instruction sequences; to implement them requires a peephole
;   optimizer, see arm-optimizer.sch for a possible framework.
;
;      - the best timer check is five instructions, not six as now
;        (four instructions if mc_timer is at offset 0 in globals)
;      - the "bcc $+8 ; blx stkoflo ; b $-n" tail for stack frame
;        allocation can be done in two instructions using predication
;      - generic + and - can be implemented with optimistic
;        store / undo in many cases, saving one dynamic instruction


; MOVT was introduced in the ARMv6T2

(define *arm.MOVT* #t)

; Overrides procedures in pass5p2.sch.

(define (cant.label as L)
  (arm.emit-label as L)
  (emit-label! as L))

(define (cant.end as)
  ;; This is sufficent to flush the optimizer state, at the moment.
  (arm.begin-no-optimize as)
  (arm.end-no-optimize as))

; Assumes that exceptions are non-continuable.  That's not necessary,
; it just shrinks code size, but it's always been true, and Twobit
; generates code that assumes it.
;
; An additional code is always passed to millicode in TMP0.

(define (cant.trap as code name . rest)
  (let-values (((success? rotation imm) (arm.compress-immediate code)))
    (assert success?)
    (if (not (null? rest))
        (arm.setval as arm.TMP0 (car rest)))
    (if (or (= code $m.global-ex)
            (= code $m.argc-ex)
            (= code $m.invoke-ex)
            (= code $m.global-invoke-ex)
            (= code $m.exception))
        (arm.ADDI   as arm.PC arm.GLOBALS rotation imm)
        (begin
          (arm.ADDI as arm.TMP1 arm.GLOBALS rotation imm)
          (arm.BLX  as arm.TMP1)))))

(define (cant.nop as)
  (arm.ORR as 0 0 0))

(define (cant.ldi as base offset rd)
  (let ((base (arm.reg base))
        (rd   (arm.reg rd)))
    (if (< (abs offset) #x1000)
        (arm.LDRI as rd base offset)
        (begin
          (arm.setval as arm.TMP1 offset)
          (arm.LDR    as rd base arm.TMP1)))))

(define (cant.ldbiz as base offset rd)
  (let ((base (arm.reg base))
        (rd   (arm.reg rd)))
    (if (< (abs offset) #x1000)
        (arm.LDRBI as rd base offset)
        (begin
          (arm.setval as arm.TMP1 offset)
          (arm.LDRB   as rd base arm.TMP1)))))

(define (cant.sti as rs base offset)
  (let ((base (arm.reg base))
        (rs   (arm.reg rs)))
    (if (< (abs offset) #x1000)
        (arm.STRI as rs base offset)
        (begin
          (arm.setval as arm.TMP1 offset)
          (arm.STR    as rs base arm.TMP1)))))

(define (cant.stbi as rs base offset)
  (let ((base (arm.reg base))
        (rs   (arm.reg rs)))
    (if (< (abs offset) #x1000)
        (arm.STRBI as rs base offset)
        (begin
          (arm.setval as arm.TMP1 offset)
          (arm.STRB   as rs base arm.TMP1)))))

(define (cant.movi as imm rd)
  (arm.setval as (arm.reg rd) imm))

(define (cant.mov as rs rd)
  (arm.MOV as (arm.reg rd) (arm.reg rs)))

(define (cant.ldrtn/return as offset)
  (arm.begin-no-optimize as)
  (arm.LDRI as arm.PC arm.STKP offset)
  (arm.end-no-optimize as))

; Empirically it's not a big win to try to encode SETRTN and JUMP
; addresses cleverly (next three operations).  Now that the code's
; written it seems silly to remove it, but it's not known whether all
; paths have been well tested.

(define (cant.setrtn/strtn as L offset)

  ;; Compute the distance from the ADD to L.  When ADD reads PC
  ;; it reads as ADD+8, so we must subtract 8.
  (define (default-encoding)
    (arm.label->reg as L arm.TMP1 -8 #t)
    (arm.ADD        as arm.TMP1 arm.TMP1 arm.PC)
    (arm.STRI       as arm.TMP1 arm.STKP offset))

  (arm.begin-no-optimize as)
  (cond ((label-value as L)
         => (lambda (Lv)
              ;; Compute the distance from the ADDI to L.  The PC reads as ADDI+8
              ;; so adjust for that by -8.
              (let-values (((success? rotation imm) (arm.compress-immediate (- Lv (here as) 8))))
                (if success?
                    (begin
                      (arm.ADDI    as arm.TMP1 arm.PC rotation imm)
                      (arm.STRI    as arm.TMP1 arm.STKP offset))
                    (default-encoding)))))
        (else
         (default-encoding)))
  (arm.end-no-optimize as))

(define (cant.jump as L)
  (arm.LDRI as arm.TMP1 arm.REG0 $p.codevector)
  (assert (> $tag.bytevector-tag 0))   ; Or we must use ADDI instead of SUBI
  (cond ((or (not L) (eqv? (label-value as L) 0))
         (arm.SUBI as arm.PC arm.TMP1 0 (abs (- 4 $tag.bytevector-tag))))
        ((not (label-value as L))
         (arm.label->reg as L arm.TMP0 (- 4 $tag.bytevector-tag) #f)
         (arm.ADD   as arm.PC arm.TMP1 arm.TMP0))
        (else
         (let ((Lv (label-value as L)))
           (let-values (((success? rotation imm) (arm.compress-immediate (+ (- 4 $tag.bytevector-tag) Lv))))
             (if success?
                 (arm.ADDI as arm.PC arm.TMP1 rotation imm)
                 (begin
                   (arm.label->reg as L arm.TMP0 (- 4 $tag.bytevector-tag) #f)
                   (arm.ADD   as arm.PC arm.TMP1 arm.TMP0))))))))

; Set the return address of the instruction after the jump, store it
; in the frame at byte offset "offset", and jump.  The sequence must
; be aligned so that the return point has the required alignment.
;
; If L is #f then it denotes the start of the code vector, otherwise
; it's a label within that code vector.

(define (cant.setrtn/strtn/jump as L align offset)
  (arm.LDRI as arm.TMP1 arm.REG0 $p.codevector)
  (assert (> $tag.bytevector-tag 0))            ; Or we must use ADDI instead of SUBI
  (arm.begin-no-optimize as)
  (cond ((or (not L) (eqv? (label-value as L) 0))
         (arm.alignment as align 8)
         (arm.STRI as arm.PC arm.STKP offset)   ; PC reads as current+8
         (arm.SUBI as arm.PC arm.TMP1 0 (abs (- 4 $tag.bytevector-tag))))
        ((not (label-value as L))
         (arm.label->reg as L arm.TMP0 (- 4 $tag.bytevector-tag) #f)
         (arm.alignment as align 8)
         (arm.STRI  as arm.PC arm.STKP offset)   ; PC reads as current+8
         (arm.ADD   as arm.PC arm.TMP1 arm.TMP0))
        (else
         (let ((Lv (label-value as L))
               (a  (arm.compute-alignment as align 8)))
           (let-values (((success? rotation imm) (arm.compress-immediate (+ (- 4 $tag.bytevector-tag) Lv a))))
             (if success?
                 (begin
                   (arm.alignment as align 8)
                   (arm.STRI  as arm.PC arm.STKP offset)   ; PC reads as current+8
                   (arm.ADDI  as arm.PC arm.TMP1 rotation imm))
                 (begin
                   (arm.label->reg as L arm.TMP0 (- 4 $tag.bytevector-tag) #f)
                   (arm.alignment as align 8)
                   (arm.STRI  as arm.PC arm.STKP offset)   ; PC reads as current+8
                   (arm.ADD   as arm.PC arm.TMP1 arm.TMP0)))))))
  (arm.end-no-optimize as)
  (assert (zero? (remainder (here as) align))))

; Set the return address of the instruction after the jump, store it
; in the frame at byte offset "offset", and branch.  The sequence must
; be aligned so that the return point has the required alignment.

(define (cant.setrtn/strtn/branch as L align offset)
  (arm.begin-no-optimize as)
  (arm.alignment as align 8)
  (arm.STRI   as arm.PC arm.STKP offset) ; PC reads as current+8
  (arm.branch as arm.B L)                ; *Must* be one instruction
  (arm.end-no-optimize as)
  (assert (zero? (remainder (here as) align))))

(define (cant.ba as L)
  (arm.branch as arm.B L))

(define (cant.bcc as condition r1 r2 L)
  (arm.bccx as condition (arm.reg r1) (arm.reg r2) #f L))

(define (cant.bcci as condition r imm L)
  (arm.bccx as condition (arm.reg r) imm #t L))

(define (cant.setcc as condition r1 r2 rd)
  (arm.setccx as condition (arm.reg rd) (arm.reg r1) (arm.reg r2) #f))

(define (cant.setcci as condition r imm rd)
  (arm.setccx as condition (arm.reg rd) (arm.reg r) imm #t))

(define (cant.addi as rs imm rd)
  (arm.emit-addi as (arm.reg rd) (arm.reg rs) imm #f))

; TODO: Possible to avoid the MOV.VC in some cases, and to undo on the
; not-taken branch instead.  The static code size is the same but the
; fast path is one instruction shorter.  Ditto for add/bnv, sub/bnv,
; subi/bnv.

(define (cant.addi/bnv as rs imm rd L)
  (arm.emit-addi as arm.TMP1 (arm.reg rs) imm #t)
  (arm.MOV.VC    as (arm.reg rd) arm.TMP1)
  (arm.branch    as arm.B.VC L))

; TODO: better instruction selection here

(define (cant.subi as rs imm rd)
  (arm.setval as arm.TMP1 imm)
  (arm.SUB    as (arm.reg rd) (arm.reg rs) arm.TMP1))

; TODO: better instruction selection here

(define (cant.subi/bnv as rs imm rd L)
  (arm.setval as arm.TMP1 imm)
  (arm.SUBS   as arm.TMP1 (arm.reg rs) arm.TMP1)
  (arm.MOV.VC as (arm.reg rd) arm.TMP1)
  (arm.branch as arm.B.VC L))

(define (cant.shli as rs imm rd)
  (arm.LSLI as (arm.reg rd) (arm.reg rs) imm))

(define (cant.shri as rs imm rd)
  (arm.LSRI as (arm.reg rd) (arm.reg rs) imm))

(define (cant.shrai as rs imm rd)
  (arm.ASRI as (arm.reg rd) (arm.reg rs) imm))

; The only important cases here are when imm is quite small.

(define (cant.andni as rs imm rd)
  (let ((rs (arm.reg rs))
        (rd (arm.reg rd)))
    (let-values (((success? rotation imm2) (arm.compress-immediate imm)))
      (cond (success?
             (arm.BICI as rd rs rotation imm2))
            (else
             (arm.setval as arm.TMP1 imm)
             (arm.BIC    as rd rs arm.TMP1))))))

(define (cant.add as rs1 rs2 rd)
  (arm.ADD as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.add/bnv as rs1 rs2 rd L)
  (arm.ADDS   as arm.TMP1 (arm.reg rs1) (arm.reg rs2))
  (arm.MOV.VC as (arm.reg rd) arm.TMP1)
  (arm.branch as arm.B.VC L))

(define (cant.sub as rs1 rs2 rd)
  (arm.SUB as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.sub/bnv as rs1 rs2 rd L)
  (arm.SUBS   as arm.TMP1 (arm.reg rs1) (arm.reg rs2))
  (arm.MOV.VC as (arm.reg rd) arm.TMP1)
  (arm.branch as arm.B.VC L))

(define (cant.mul as rs1 rs2 rd)
  (arm.MUL as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

; arm.MULS does not set the overflow flag, nor does eg SMULLS.
;
; But SMULL can be used to generate a 64-bit result and we can then
; easily check for overflow:
;  overflow iff hi(x*y) != lo(x*y) >> 31
; where the shift is signed.
;
; TODO: This code is dumb because it performs the multiplication
; twice, but to avoid that we need a non-root spill location in
; globals for the first result.  We could use the dynamic frame
; link slot in the frame but that seems really hacky.

(define (cant.mul/bnv as rs1 rs2 rd L)
  (let ((rd  (arm.reg rd))
        (rs1 (arm.reg rs1))
        (rs2 (arm.reg rs2)))
    (arm.STRI   as arm.STKP arm.GLOBALS $g.stkp) ; Spill a safe register
    (arm.SMULL  as arm.TMP1 arm.STKP rs1 rs2)
    (arm.ASRI   as arm.STKP arm.STKP 31)
    (arm.CMP    as arm.TMP1 arm.STKP)
    (arm.LDRI   as arm.STKP arm.GLOBALS $g.stkp) ; Restore
    (arm.MUL.EQ as rd rs1 rs2)
    (arm.branch as arm.B.EQ L)))

(define (cant.and as rs1 rs2 rd)
  (arm.AND as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.or as rs1 rs2 rd)
  (arm.ORR as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.xor as rs1 rs2 rd)
  (arm.EOR as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.shl as rs1 rs2 rd)
  (arm.LSL as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.shr as rs1 rs2 rd)
  (arm.LSR as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.shra as rs1 rs2 rd)
  (arm.ASR as (arm.reg rd) (arm.reg rs1) (arm.reg rs2)))

(define (cant.not as rs rd)
  (arm.MVN as (arm.reg rd) (arm.reg rs)))

; Floating operations.  Here we get to assume only VFPv2, ie, no
; VMOV.  Floating registers are mapped directly to
; corresponding-numbered hardware registers.
;
; Floating load/store instructions allow for immediates but the low
; two bits must be zero, so generally they're useless for
; tag-adjusted loads.  But some stores are through untagged pointers,
; so catch that.

(define (cant.fldi as base offset frd)
  (arm.emit-addi as arm.TMP1 (arm.reg base) offset #f)
  (arm.VLDRI as (arm.freg frd) arm.TMP1 0))

(define (cant.fsti as frs base offset)
  (if (and (<= -1023 offset 1023)
           (zero? (remainder (abs offset) 4)))
      (arm.VSTRI as (arm.freg frs) (arm.reg base) offset)
      (begin
        (arm.emit-addi as arm.TMP1 (arm.reg base) offset #f)
        (arm.VSTRI as (arm.freg frs) arm.TMP1 0))))

(define (cant.fadd as frs1 frs2 frd)
  (arm.VADD as (arm.freg frd) (arm.freg frs1) (arm.freg frs2)))

(define (cant.fsub as frs1 frs2 frd)
  (arm.VSUB as (arm.freg frd) (arm.freg frs1) (arm.freg frs2)))

(define (cant.fmul as frs1 frs2 frd)
  (arm.VMUL as (arm.freg frd) (arm.freg frs1) (arm.freg frs2)))

(define (cant.fdiv as frs1 frs2 frd)
  (arm.VDIV as (arm.freg frd) (arm.freg frs1) (arm.freg frs2)))

;; ARM condition codes for floating point comparisons are unintuitive.

(define (cant.fsetcc as condition frs1 frs2 rd)
  (arm.VCMP      as (arm.freg frs1) (arm.freg frs2))
  (arm.VMRS.APSR as)
  (let ((rd (arm.reg rd)))
    (arm.MOVWI as rd $imm.false)
    ((case condition
       ((equal) arm.ADDI.EQ)
       ((not-equal) arm.ADDI.NE)           ; NE true for NaN
       ((less) arm.ADDI.MI)                ; LT true for NaN
       ((greater) arm.ADDI.GT)
       ((less-or-equal) arm.ADDI.LS)       ; LE true for NaN
       ((greater-or-equal) arm.ADDI.GE)
       (else (error fsetcc  "Bad condition: " condition)))
     as rd rd 0 (- $imm.true $imm.false))))

;;; Utilities

; Emit NOPs so that if 'bytes' bytes of code follows the NOPs then
; the end of those bytes will be aligned on an 'align' boundary.

(define (arm.alignment as align bytes)
  (let loop ()
    (if (not (zero? (remainder (+ (here as) bytes) align)))
        (begin
          (cant.nop as)
          (loop)))))

; Compute the number of bytes of alignment.  This is pretty hacky.

(define (arm.compute-alignment as align bytes)
  (let loop ((loc (here as)))
    (if (not (zero? (remainder (+ loc bytes) align)))
        (loop (+ loc 4))
        (- loc (here as)))))
  
; Emit a value into r representing the distance from the end of the
; sequence, or from zero, to the label L.
;
; When label->reg is used for JUMP and SETRTN when the label is not
; known it is probably possible to do better than two instructions, by
; looking ahead or speculating.  In practice the savings are probably
; very slight indeed.

(define (arm.label->reg as L r adjust pc-relative?)
  (if (label-value as L)
      (let* ((Lv  (label-value as L))
             (loc (here as))
             (v4  (if pc-relative?
                      (+ adjust (- Lv (+ loc 4))) ; 4 to skip over one MOVWI
                      (+ adjust Lv)))
             (v8  (if pc-relative?
                      (+ adjust (- Lv (+ loc 8))) ; 8 to skip over MOVWI/MOVTI
                      (+ adjust Lv))))
        (or (arm.setval1 as r v4)
            (arm.setval2 as r v8)))
      (begin
        (emit-fixup-proc! 
         as (lambda (cv loc)
              (let* ((Lv (label-value as L))
                     (bs (arm.bytes (if pc-relative?
                                        (+ adjust (- Lv (+ loc 8))) ; 8 to skip over the two instructions
                                        (+ adjust Lv)))))
                (arm.fixup-movi cv (+ loc 0) (vector-ref bs 2) (vector-ref bs 3))
                (arm.fixup-movi cv (+ loc 4) (vector-ref bs 0) (vector-ref bs 1)))))
        (arm.MOVWI as r 0)
        (arm.MOVTI as r 0))))

; Map a MAL register to an ARM hardware register.

(define arm.reg
  (let* ((offset (abs (min $r.tmp $r.globals $r.stkp $r.second $r.result)))
         (v      (make-vector (+ *nregs* offset) -1)))
    (vector-set! v (+ offset $r.tmp) arm.TMP0)
    (vector-set! v (+ offset $r.globals) arm.GLOBALS)
    (vector-set! v (+ offset $r.stkp) arm.STKP)
    (vector-set! v (+ offset $r.second) arm.SECOND)
    (vector-set! v (+ offset $r.result) arm.RESULT)
    (vector-set! v (+ offset 0 $r.reg0) arm.REG0)
    (vector-set! v (+ offset 1 $r.reg0) arm.REG1)
    (vector-set! v (+ offset 2 $r.reg0) arm.REG2)
    (vector-set! v (+ offset 3 $r.reg0) arm.REG3)
    (vector-set! v (+ offset 4 $r.reg0) arm.REG4)
    (vector-set! v (+ offset 5 $r.reg0) arm.REG5)
    (if (= 8 *nregs*)
	(begin
         (vector-set! v (+ offset 6 $r.reg0) arm.REG6)
         (vector-set! v (+ offset 7 $r.reg0) arm.REG7)))
    (lambda (r)
      (vector-ref v (+ r offset)))))

(define (arm.freg x)
  x)

; The bytes of v considered as a 32-bit unsigned value, in big-endian order.

(define (arm.bytes v)
  (let ((v (if (negative? v) (+ #x100000000 v) v)))
    (vector (quotient v 16777216)
            (remainder (quotient v 65536) 256)
            (remainder (quotient v 256) 256)
            (remainder v 256))))

; TODO: better instruction selection here, esp large negative vaues

(define (arm.emit-addi as rd rs imm set-flag?)
  (cond ((<= 0 imm #xFF) 
         ((if set-flag? arm.ADDIS arm.ADDI) as rd rs 0 imm))
        ((<= 0 (abs imm) #xFF)
         ((if set-flag? arm.SUBIS arm.SUBI) as rd rs 0 (abs imm)))
        (else
         (let-values (((success? rotation imm2) (arm.compress-immediate imm)))
           (cond (success?
                  ((if set-flag? arm.ADDIS arm.ADDI) as rd rs rotation imm2))
                 (else
                  (arm.setval as arm.TMP1 imm)
                  ((if set-flag? arm.ADDS arm.ADD) as rd rs arm.TMP1)))))))

; Returns (#t rotation value) if it's possible to encode it, or (#f 0 0) if not.
; When encoding is possible, rotation fits in four bits and value in eight bits.

(define (arm.compress-immediate imm)
  (let loop ((j imm) (rotation 16))
    (cond ((<= 0 j 255)
           (values #t (remainder rotation 16) j))
          ((= rotation 0)
           (values #f 0 0))
          (else
           (loop (+ (quotient j 4)
                    (* (remainder j 4) 1073741824))
                 (- rotation 1))))))

; Inject two bytes of immediates into a MOVWI or MOVTI instruction.

(define (arm.fixup-movi cv loc hi lo)
  (bytevector-set! cv (+ loc 0) lo)
  (bytevector-set! cv (+ loc 1) (+ (bytevector-ref cv (+ loc 1)) (remainder hi 16)))
  (bytevector-set! cv (+ loc 2) (+ (bytevector-ref cv (+ loc 2)) (quotient hi 16))))

; ARM operand order is dest, src, ... and we follow that for all
; utility functions.  In all cases the registers have been mapped to
; ARM hardware registers by arm.reg when we get into the utilities.

; Load an immediate into a register using one or two instructions.

(define (arm.setval as rd imm)
  (or (arm.setval1 as rd imm)
      (arm.setval2 as rd imm)))

; Load imm into a register and return #t if it can be done with one
; instruction, otherwise return #f.

(define (arm.setval1 as rd imm)
  (let ((imm (if (negative? imm) (+ #x100000000 imm) imm)))
    (if (<= 0 imm 65535)
        ;; 16-bit constant in low halfword
        (begin (arm.MOVWI as rd imm)
               #t)
        (let-values (((success? rotation imm2) (arm.compress-immediate imm)))
          (if success?
              ;; 8-bit constant expanded
              (begin (arm.MOVI as rd rotation imm2)
                     #t)
              (let-values (((success? rotation imm2) (arm.compress-immediate (- #xFFFFFFFF imm))))
                (if success?
                    ;; 8-bit constant expanded and complemented
                    (begin (arm.MVNI as rd rotation imm2)
                           #t)
                    #f)))))))

; Load imm into a register using two instructions.

(define (arm.setval2 as rd imm)
  (let ((imm (if (negative? imm) (+ #x100000000 imm) imm)))
    ;; Any other 32-bit constant
    (arm.MOVWI as rd (remainder imm 65536))
    (arm.MOVTI as rd (quotient imm 65536))))

(define (arm.bccx as condition r1 src2 src2-imm? L)
  (arm.generate-condition as condition r1 src2 src2-imm?)
  (arm.branch as (case condition
                   ((equal low1-equal low2-equal low3-equal low8-equal) arm.B.EQ)
                   ((not-equal low1-not-equal low2-not-equal low3-not-equal low8-not-equal) arm.B.NE)
                   ((less) arm.B.LT)
                   ((greater) arm.B.GT)
                   ((less-or-equal) arm.B.LE)
                   ((greater-or-equal) arm.B.GE)
                   ((unsigned-less) arm.B.HI) ; op1 < op2 === op2 > op1
                   (else (error "bccx: should not happen: branch condition: " condition)))
              L))

(define (arm.setccx as condition rd r1 src2 src2-imm?)
  (arm.generate-condition as condition r1 src2 src2-imm?)
  (arm.MOVWI as rd $imm.false)
  ((case condition
     ((equal low1-equal low2-equal low3-equal low8-equal) arm.ADDI.EQ)
     ((not-equal low1-not-equal low2-not-equal low3-not-equal low8-not-equal) arm.ADDI.NE)
     ((less) arm.ADDI.LT)
     ((greater) arm.ADDI.GT)
     ((less-or-equal) arm.ADDI.LE)
     ((greater-or-equal) arm.ADDI.GE)
     ((unsigned-less) arm.ADDI.HI)
     (else (error "setccx: should not happen: branch condition: " condition)))
   as rd rd 0 (- $imm.true $imm.false)))

(define (arm.generate-condition as condition r1 src2 src2-imm?)
  (let ((field-compare? (case condition
                          ((low1-equal low1-not-equal low2-equal low2-not-equal low3-equal low3-not-equal low8-equal low8-not-equal) #t)
                          (else #f))))
    (cond ((and field-compare? src2-imm? (= src2 0))
           (case condition
             ((low1-equal low1-not-equal) (arm.TSTI as r1 0 1))
             ((low2-equal low2-not-equal) (arm.TSTI as r1 0 3))
             ((low3-equal low3-not-equal) (arm.TSTI as r1 0 7))
             ((low8-equal low8-not-equal) (arm.TSTI as r1 0 255))
             (else (error "generate-condition: should not happen: field-compare #1: " condition))))
          (field-compare?
           (case condition
             ;; TODO: We can optimize the low1 case if src2 is an
             ;; immediate equal to 1, but only by reversing the
             ;; condition in the caller, I think.
             ((low1-equal low1-not-equal) (arm.ANDI as arm.TMP1 r1 0 1))
             ((low2-equal low2-not-equal) (arm.ANDI as arm.TMP1 r1 0 3))
             ((low3-equal low3-not-equal) (arm.ANDI as arm.TMP1 r1 0 7))
             ((low8-equal low8-not-equal) (arm.ANDI as arm.TMP1 r1 0 255))
             (else (error "generate-condition: should not happen: field-compare #2: " condition)))
           (cond ((not src2-imm?)
                  (arm.CMP as arm.TMP1 src2))
                 (else
                  ;; The assertion holds because the precondition for
                  ;; using low<n>-{not-,}equal is that the immediate,
                  ;; if any, fits in n bits, and we support only n<=8.
                  (assert (<= 0 src2 255))
                  (arm.CMPI as arm.TMP1 0 src2))))
          ((eq? condition 'unsigned-less)
           ;; Here we must reverse the operands for the caller to be able to
           ;; use available instructions.  Ergo tricky to use an immediate form.
           (if (not src2-imm?)
               (arm.CMP as src2 r1)
               (begin
                 (arm.setval as arm.TMP1 imm)
                 (arm.CMP as arm.TMP1 r1))))
          (else
           ;; TODO: We may be able to use CMN here for negative immediate src2
           ;; provided the test is equal/not-equal; uncertain about other cases.
           (if (not src2-imm?)
               (arm.CMP as r1 src2)
               (let-values (((success? rotation imm2) (arm.compress-immediate src2)))
                 (if success?
                     (arm.CMPI as r1 rotation imm2)
                     (begin
                       (arm.setval as arm.TMP1 src2)
                       (arm.CMP as r1 arm.TMP1)))))))))

; In a branch there is a 26-bit offset: the low two bits must be
; zero, and are shifted out before the number is represented.

(define (arm.branch as branch L)
  (branch as 0 L (lambda (cv loc)
                   (let ((offset (- (label-value as L) (+ loc 8))))
                     (assert (zero? (remainder offset 4)))
                     (let ((bs (arm.bytes (quotient offset 4))))
                       (bytevector-set! cv (+ loc 0) (vector-ref bs 3))
                       (bytevector-set! cv (+ loc 1) (vector-ref bs 2))
                       (bytevector-set! cv (+ loc 2) (vector-ref bs 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ARM machine assembler.
;
; In all cases the immediates /must/ fit in the instruction, there
; are some assertions but they may be removed in the future.  DW,
; LDRI, LDRBI, STRI, STRBI, and B admit negative immediates but
; otherwise immediates are always nonnegative (and zero-extend upon
; use).

(define (arm.emit-bytes as b3 b2 b1 b0)
  (let ((buf (make-bytevector 4)))
    ; Guard against a Heisenbug, for now
    (assert (and (exact? b3) (exact? b2) (exact? b1) (exact? b0)))
    (bytevector-set! buf 0 b0)
    (bytevector-set! buf 1 b1)
    (bytevector-set! buf 2 b2)
    (bytevector-set! buf 3 b3)
    (emit! as buf)))

; These emitter procedures are overridden by the simple optimizer in
; arm-optimizer.sch.

(define arm.emit-alu arm.emit-bytes)     ; No effect on flags
(define arm.emit-mem arm.emit-bytes)     ; Reads or writes memory
(define arm.emit-trap arm.emit-bytes)    ; Makes a millicode call
(define arm.emit-other arm.emit-bytes)   ; Everything else (unknown effect)

(define arm.emit-bcc-fwd                 ; Forward conditional branch
  (lambda (as fixup L b3 b2 b1 b0)
    (emit-fixup-proc! as fixup)
    (arm.emit-bytes as b3 b2 b1 b0)))

(define arm.emit-branch                 ; Unconditional branch
  (lambda (as fixup L b3 b2 b1 b0)
    (emit-fixup-proc! as fixup)
    (arm.emit-bytes as b3 b2 b1 b0)))

(define (arm.emit-label as L)            ; Label at this location
  #f)

(define (arm.begin-no-optimize as)       ; Signal that any optimization window ends here and that the next instructions cannot be changed
  #f)

(define (arm.end-no-optimize as)         ; Signal that it's OK to optimize again
  #f)

;;; End customizable emitters

(define arm-cond.EQ #b00000000)
(define arm-cond.NE #b00010000)
(define arm-cond.CS #b00100000)
(define arm-cond.CC #b00110000)
(define arm-cond.MI #b01000000)
(define arm-cond.PL #b01010000)
(define arm-cond.VS #b01100000)
(define arm-cond.VC #b01110000)
(define arm-cond.HI #b10000000)
(define arm-cond.LS #b10010000)
(define arm-cond.GE #b10100000)
(define arm-cond.LT #b10110000)
(define arm-cond.GT #b11000000)
(define arm-cond.LE #b11010000)
(define arm-cond.AL #b11100000)

(define (arm.DW as value)               ; "define word"
  (let ((bs (arm.bytes value)))
    (arm.emit-other as (vector-ref bs 0) (vector-ref bs 1) (vector-ref bs 2) (vector-ref bs 3))))
    
(define (arm.MOVWI as rd imm)           ; 16-bit immediate, clears high halfword
  (assert (<= 0 imm 65535))
  (arm.emit-alu as
                (+ arm-cond.AL #b00000011) 
                (+ #b00000000 (quotient imm 4096))              ; Bits 15..12
                (+ (* rd 16) (remainder (quotient imm 256) 16)) ; Bits 11..8
                (remainder imm 256)))                           ; Bits 7..0

(define (arm.MOVTI as rd imm)           ; 16-bit immediate, leaves low halfword alone
  (assert *arm.MOVT*)
  (assert (<= 0 imm 65535))
  (arm.emit-alu as
                (+ arm-cond.AL #b00000011) 
                (+ #b01000000 (quotient imm 4096))              ; Bits 15..12
                (+ (* rd 16) (remainder (quotient imm 256) 16)) ; Bits 11..8
                (remainder imm 256)))                           ; Bits 7..0

(define (arm.AND as rd rs1 rs2)
  (arm.emit-alu as arm-cond.AL rs1 (* rd 16) rs2))

(define (arm.BIC as rd rs1 rs2)
  (arm.emit-alu as (+ #b00000001 arm-cond.AL) (+ #b11000000 rs1) (* rd 16) rs2))

(define (arm.EOR as rd rs1 rs2)
  (arm.emit-alu as arm-cond.AL (+ #b00100000 rs1) (* rd 16) rs2))

(define (arm.SUB as rd rs1 rs2)
  (arm.emit-alu as arm-cond.AL (+ #b01000000 rs1) (* rd 16) rs2))

(define (arm.SUBS as rd rs1 rs2)
  (arm.emit-other as arm-cond.AL (+ #b01010000 rs1) (* rd 16) rs2))

(define (arm.ADD as rd rs1 rs2)
  (arm.emit-alu as arm-cond.AL (+ #b10000000 rs1) (* rd 16) rs2))

(define (arm.ADDS as rd rs1 rs2)
  (arm.emit-other as arm-cond.AL (+ #b10010000 rs1) (* rd 16) rs2))

(define (arm.CMP as rs1 rs2)
  (arm.emit-other as (+ arm-cond.AL #b00000001) (+ #b01010000 rs1) 0 rs2))

(define (arm.ORR as rd rs1 rs2)
  (arm.emit-alu as (+ arm-cond.AL #b00000001) (+ #b10000000 rs1) (* rd 16) rs2))

(define (arm.MOV as rd rs)
  (if (not (= rs rd))
      (arm.emit-alu as (+ arm-cond.AL #b00000001) #b10100000 (* rd 16) rs)))
  
(define (arm.MOV.VC as rd rs)
  (arm.emit-other as (+ arm-cond.VC #b00000001) #b10100000 (* rd 16) rs))
  
(define (arm.MVN as rd rs)
  (arm.emit-alu as (+ arm-cond.AL #b00000001) #b11100000 (* rd 16) rs))
  
(define (arm.MOVI as rd rotation imm)   ; 8-bit expanded immediate
  (arm.emit-alu as (+ arm-cond.AL #b00000011) #b10100000 (+ (* rd 16) rotation) imm))

(define (arm.MVNI as rd rotation imm)   ; 8 bit expanded and complemented immediate
  (arm.emit-alu as (+ arm-cond.AL #b00000011) #b11100000 (+ (* rd 16) rotation) imm))

(define (arm.BICI as rd rs rotation imm)
  (arm.emit-alu as (+ arm-cond.AL #b00000011) (+ #b11000000 rs) (+ (* rd 16) rotation) imm))

(define (arm.ANDI as rd rs rotation imm)
  (arm.emit-alu as (+ arm-cond.AL #b00000010) (+ #b00000000 rs) (+ (* rd 16) rotation) imm))

(define (arm.ANDSI as rd rs rotation imm)
  (arm.emit-other as (+ arm-cond.AL #b00000010) (+ #b00010000 rs) (+ (* rd 16) rotation) imm))

(define (arm.SUBI as rd rs rotation imm)
  (arm.emit-alu as (+ arm-cond.AL #b00000010) (+ #b01000000 rs) (+ (* rd 16) rotation) imm))

(define (arm.SUBIS as rd rs rotation imm)
  (arm.emit-other as (+ arm-cond.AL #b00000010) (+ #b01010000 rs) (+ (* rd 16) rotation) imm))

(define (arm.addicc name cond)
  (lambda (as rd rs rotation imm)
    ((if (= cond arm-cond.AL) arm.emit-alu arm.emit-other)
     as (+ cond #b00000010) (+ #b10000000 rs) (+ (* rd 16) rotation) imm)))

(define arm.ADDI    (arm.addicc "ADDI"    arm-cond.AL))
(define arm.ADDI.EQ (arm.addicc "ADDI.EQ" arm-cond.EQ))
(define arm.ADDI.NE (arm.addicc "ADDI.NE" arm-cond.NE))
(define arm.ADDI.LT (arm.addicc "ADDI.LT" arm-cond.LT))
(define arm.ADDI.GT (arm.addicc "ADDI.GT" arm-cond.GT))
(define arm.ADDI.LE (arm.addicc "ADDI.LE" arm-cond.LE))
(define arm.ADDI.GE (arm.addicc "ADDI.GE" arm-cond.GE))
(define arm.ADDI.MI (arm.addicc "ADDI.LT" arm-cond.MI))
(define arm.ADDI.LS (arm.addicc "ADDI.LT" arm-cond.LS))

(define (arm.ADDIS as rd rs rotation imm)
  (arm.emit-other as (+ arm-cond.AL #b00000010) (+ #b10010000 rs) (+ (* rd 16) rotation) imm))

(define (arm.TSTI as rs rotation imm)
  (arm.emit-other as (+ arm-cond.AL #b00000011) (+ #b00010000 rs) (+ 0 rotation) imm))

(define (arm.CMPI as rs rotation imm)
  (arm.emit-other as (+ arm-cond.AL #b00000011) (+ #b01010000 rs) (+ 0 rotation) imm))

; Multiply argument order is result, op2, op1 (but should not matter).

(define (arm.MUL as rd rs1 rs2)
  (arm.emit-alu as arm-cond.AL rd rs2 (+ #b10010000 rs1)))

(define (arm.MUL.EQ as rd rs1 rs2)
  (arm.emit-other as arm-cond.EQ rd rs2 (+ #b10010000 rs1)))

(define (arm.SMULL as rdhi rdlo rs1 rs2)
  (arm.emit-alu as arm-cond.AL (+ #b11000000 rdhi) (+ (* 16 rdlo) rs2) (+ #b10010000 rs1)))

; Note that reg-reg shifts are not laid out like add/subtract, etc:
; the register order is different: result, count, shiftee

(define (arm.LSL as rd rs1 rs2)
  (arm.emit-alu as (+ arm-cond.AL #b00000001) #b10100000 (+ (* rd 16) rs2) (+ #b00010000 rs1)))

(define (arm.LSR as rd rs1 rs2)
  (arm.emit-alu as (+ arm-cond.AL #b00000001) #b10100000 (+ (* rd 16) rs2) (+ #b00110000 rs1)))

(define (arm.ASR as rd rs1 rs2)
  (arm.emit-alu as (+ arm-cond.AL #b00000001) #b10100000 (+ (* rd 16) rs2) (+ #b01010000 rs1)))

(define (arm.LSLI as rd rs imm)
  (assert (<= 0 imm 31))
  (arm.emit-alu as (+ arm-cond.AL #b00000001) 
                #b10100000 
                (+ (* rd 16) (quotient imm 2))
                (+ #b00000000 (* 128 (remainder imm 2)) rs)))

(define (arm.LSRI as rd rs imm)
  (assert (<= 0 imm 31))
  (arm.emit-alu as (+ arm-cond.AL #b00000001) 
                #b10100000 
                (+ (* rd 16) (quotient imm 2))
                (+ #b00100000 (* 128 (remainder imm 2)) rs)))

(define (arm.ASRI as rd rs imm)
  (assert (<= 0 imm 31))
  (arm.emit-alu as (+ arm-cond.AL #b00000001) 
                #b10100000 
                (+ (* rd 16) (quotient imm 2))
                (+ #b01000000 (* 128 (remainder imm 2)) rs)))

; Load/store:
;
; The "P" bit (low bit of the MSB) must be set or the second operand
; (rs2 or immediate) is ignored.  P=0 is used for increment/decrement
; modes.
;
; Immediates are always zero-extended, if the "U" bit is 1 then the
; value is added, otherwise subtracted.

(define (arm.LDR as rd rs1 rs2)
  (arm.emit-mem as (+ arm-cond.AL #b00000111) (+ #b10010000 rs1) (* rd 16) rs2))

(define (arm.LDRI as rd rs imm)
  (assert (<= -4095 imm 4095))
  (let ((u-bit (if (negative? imm) #b00000000 #b10000000))
        (imm   (abs imm)))
    (arm.emit-mem as (+ arm-cond.AL #b00000101) (+ u-bit #b00010000 rs) (+ (* rd 16) (quotient imm 256)) (remainder imm 256))))

(define (arm.LDRB as rd rs1 rs2)
  (arm.emit-mem as (+ arm-cond.AL #b00000111) (+ #b11010000 rs1) (* rd 16) rs2))

(define (arm.LDRBI as rd rs imm)
  (assert (<= -4095 imm 4095))
  (let ((u-bit (if (negative? imm) #b00000000 #b10000000))
        (imm   (abs imm)))
    (arm.emit-mem as (+ arm-cond.AL #b00000101) (+ u-bit #b01010000 rs) (+ (* rd 16) (quotient imm 256)) (remainder imm 256))))

; ARM syntax for store:
;  "STR x, y, z"    means  "*(y+z) = x"
;  "STR x, y, imm"  means  "*(y+imm) = x"

(define (arm.STR as rval rbase ridx)
  (arm.emit-mem as (+ arm-cond.AL #b00000111) (+ #b10000000 rbase) (* rval 16) ridx))

(define (arm.STRI as rval rbase imm)
  (assert (<= -4095 imm 4095))
  (let ((u-bit (if (negative? imm) #b00000000 #b10000000))
        (imm   (abs imm)))
    (arm.emit-mem as (+ arm-cond.AL #b00000101) (+ u-bit #b00000000 rbase) (+ (* rval 16) (quotient imm 256)) (remainder imm 256))))

(define (arm.STRB as rval rbase ridx)
  (arm.emit-mem as (+ arm-cond.AL #b00000111) (+ #b11000000 rbase) (* rval 16) ridx))

(define (arm.STRBI as rval rbase imm)
  (assert (<= -4095 imm 4095))
  (let ((u-bit (if (negative? imm) #b00000000 #b10000000))
        (imm   (abs imm)))
    (arm.emit-mem as (+ arm-cond.AL #b00000101) (+ u-bit #b01000000 rbase) (+ (* rval 16) (quotient imm 256)) (remainder imm 256))))

(define (arm.bcc name cnd)
  (lambda (as offset L fixup)
    (let ((bs (arm.bytes offset)))
      (cond ((and (not (label-value as L)) (not (= cnd arm-cond.AL)))
             (arm.emit-bcc-fwd as fixup L (+ cnd #b00001010) (vector-ref bs 1) (vector-ref bs 2) (vector-ref bs 3)))
            ((= cnd arm-cond.AL) 
             (arm.emit-branch as fixup L (+ cnd #b00001010) (vector-ref bs 1) (vector-ref bs 2) (vector-ref bs 3)))
            (else
             (emit-fixup-proc! as fixup)
             (arm.emit-other as (+ cnd #b00001010) (vector-ref bs 1) (vector-ref bs 2) (vector-ref bs 3)))))))

(define arm.B    (arm.bcc "B"    arm-cond.AL))
(define arm.B.EQ (arm.bcc "B.EQ" arm-cond.EQ))
(define arm.B.NE (arm.bcc "B.NE" arm-cond.NE))
(define arm.B.VC (arm.bcc "B.VC" arm-cond.VC))
(define arm.B.HI (arm.bcc "B.HI" arm-cond.HI))
(define arm.B.LT (arm.bcc "B.LT" arm-cond.LT))
(define arm.B.GT (arm.bcc "B.GT" arm-cond.GT))
(define arm.B.LE (arm.bcc "B.LE" arm-cond.LE))
(define arm.B.GE (arm.bcc "B.GE" arm-cond.GE))

(define (arm.BLX as r)
  (arm.emit-trap as (+ arm-cond.AL #b00000001) #b00101111 #b11111111 (+ #b00110000 r)))

; A1 encoding: D is the high bit of frd.  Note the offset is a word
; offset and not useful for tag stripping as part of the load.

(define (arm.VLDRI as frd base offset)
  (assert (and (<= -1023 offset 1023) (zero? (remainder (abs offset) 4))))
  (let ((U      (if (negative? offset) #b00000000 #b10000000))
        (offset (quotient (abs offset) 4))
        (D      (fxlsh (fxlogand frd #b00010000) 2))
        (Vd     (fxlogand frd 15)))
    (arm.emit-alu as (+ arm-cond.AL #b00001101) (+ U D #b00010000 base) (+ (* Vd 16) #b00001011) offset)))

(define (arm.VSTRI as frs base offset)
  (assert (and (<= -1023 offset 1023) (zero? (remainder (abs offset) 4))))
  (let ((U      (if (negative? offset) #b00000000 #b10000000))
        (offset (quotient (abs offset) 4))
        (D      (fxlsh (fxlogand frs #b00010000) 2))
        (Vd     (fxlogand frs 15)))
    (arm.emit-alu as (+ arm-cond.AL #b00001101) (+ U D #b00000000 base) (+ (* Vd 16) #b00001011) offset)))

; For add, subtract, multiply and divide we want A2 encoding A2 with sz=1.

(define (arm.VADD as frd frs1 frs2)
  (arm.vector-alu-op as frd frs1 frs2 #b00110000 #b00000000))

(define (arm.VSUB as frd frs1 frs2)
  (arm.vector-alu-op as frd frs1 frs2 #b00110000 #b01000000))

(define (arm.VMUL as frd frs1 frs2)
  (arm.vector-alu-op as frd frs1 frs2 #b00100000 #b00000000))

(define (arm.VDIV as frd frs1 frs2)
  (arm.vector-alu-op as frd frs1 frs2 #b10000000 #b00000000))

; For compare the A1 and A2 encodings are the same except for the code
; chosen for Vn; we want the A1 encoding (Vn=4) because the A2
; encoding (Vn=5) is a compare with 0.0.

(define (arm.VCMP as frs1 frs2)
  (arm.vector-alu-op as frs1 4 frs2 #b10110000 #b01000000))

(define (arm.vector-alu-op as frd frs1 frs2 b2-bits b0-bits)
  (let ((D  (fxlsh (fxrshl frd 4) 6))
        (N  (fxlsh (fxrshl frs1 4) 7))
        (M  (fxlsh (fxrshl frs2 4) 5))
        (Vd (fxlogand frd 15))
        (Vn (fxlogand frs1 15))
        (Vm (fxlogand frs2 15)))
    (arm.emit-alu as
                  (+ arm-cond.AL #b00001110) 
                  (+ b2-bits D Vn)
                  (+ (* Vd 16) #b00001011) ; lsb: sz=1
                  (+ b0-bits N M Vm))))

(define (arm.VMRS.APSR as)
  (arm.emit-other as (+ #b00001110 arm-cond.AL) #b11110001 #b11111010 #b00010000))

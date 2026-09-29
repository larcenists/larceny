; Copyright 2012 Lars T Hansen.
;
; AArch64 (ARMv8-A, A64 instruction set) overrides for the Cant
; primitives.  Derived from pass5p2-arm.sch.
;
; The AArch64 mapping has only hardware-backed MacScheme registers;
; the assignment is in src/Rts/fence-arm64-regs.cfg.  In short:
;
;   x0        RESULT
;   x1        SECOND
;   x2..x15   REG0..REG13
;   x16       TMP1, assembler scratch, not exposed to pass5p2
;   x17       TMP2, assembler scratch, not exposed to pass5p2
;   x18       platform register, never touched
;   x19..x20  REG14..REG15
;   x21       TMP0 (this is $r.tmp in pass5p2)
;   x22       STKP
;   x23       GLOBALS
;   x24..x28  unused (callee-saved in C, so preserved for C)
;   x29       frame pointer, never touched
;   x30       link register, used only for millicode calls
;   sp        the C stack pointer, never touched by Scheme code
;
; The MacScheme floating registers F0..F3 are d8..d11, which are
; callee-saved in C.  Millicode need not save them.
;
; Millicode is called through a table of function pointers in the
; globals array: the millicode code passed to cant.trap is the byte
; offset of the entry in the globals array.
;
; Instructions are four bytes and four-byte aligned, so every return
; address looks like a fixnum to the garbage collector.
;
; Conditional branches reach +/-1MB and unconditional branches reach
; +/-128MB; larger code vectors are not supported.
;
; TODO:
;
; - Peephole optimization: the ARM-32 back end has a framework in
;   arm-optimizer.sch.
;
; Calls that set a return address use BL/BLR, returns use RET, and tail
; calls use B/BR, so that the CPU's return-address predictor sees
; matching calls and returns.  (The link register value is not used;
; the return address is in the frame.)

(define (cant.label as L)
  (emit-label! as L))

(define (cant.end as)
  #t)

; Assumes that exceptions are non-continuable, as does the ARM-32
; back end, but the call sequence is the same either way.
;
; An additional code is always passed to millicode in TMP0.

(define (cant.trap as code name . rest)
  (if (not (null? rest))
      (arm64.setval as arm64.TMP0 (car rest)))
  (arm64.load as 8 arm64.TMP2 arm64.GLOBALS code)
  (arm64.BLR  as arm64.TMP2))

(define (cant.nop as)
  (arm64.NOP as))

(define (cant.ldi as base offset rd)
  (arm64.load as 8 (arm64.reg rd) (arm64.reg base) offset))

(define (cant.ldbiz as base offset rd)
  (arm64.load as 1 (arm64.reg rd) (arm64.reg base) offset))

(define (cant.ldwiz as base offset rd)
  (arm64.load as 4 (arm64.reg rd) (arm64.reg base) offset))

(define (cant.sti as rs base offset)
  (arm64.store as 8 (arm64.reg rs) (arm64.reg base) offset))

(define (cant.stbi as rs base offset)
  (arm64.store as 1 (arm64.reg rs) (arm64.reg base) offset))

(define (cant.stwi as rs base offset)
  (arm64.store as 4 (arm64.reg rs) (arm64.reg base) offset))

(define (cant.movi as imm rd)
  (arm64.setval as (arm64.reg rd) imm))

(define (cant.mov as rs rd)
  (arm64.MOV as (arm64.reg rd) (arm64.reg rs)))

(define (cant.ldrtn/return as offset)
  (arm64.load as 8 arm64.TMP1 arm64.STKP offset)
  (arm64.RET  as arm64.TMP1))

(define (cant.setrtn/strtn as L offset)
  (arm64.ADR   as arm64.TMP1 L 0)
  (arm64.store as 8 arm64.TMP1 arm64.STKP offset))

; Leave in TMP1 the address of L within the code vector of REG0, or
; the start of that code vector if L is #f.  Destroys TMP2.

(define (arm64.code-address as L)
  (let ((bias (- $bytewidth.wordsize $tag.bytevector-tag)))
    (arm64.load as 8 arm64.TMP1 arm64.REG0 $p.codevector)
    (cond ((not L)
           (arm64.addi as arm64.TMP1 arm64.TMP1 bias))
          ((label-value as L)
           (arm64.addi as arm64.TMP1 arm64.TMP1 (+ bias (label-value as L))))
          (else
           (arm64.label->reg as L arm64.TMP2 bias)
           (arm64.ADD as arm64.TMP1 arm64.TMP1 arm64.TMP2)))))

(define (cant.jump as L)
  (arm64.code-address as L)
  (arm64.BR as arm64.TMP1))

; Set the return address of the instruction after the jump, store it
; in the frame at byte offset "offset", and jump.  The sequence must
; be aligned so that the return point has the required alignment.
;
; If L is #f then it denotes the start of the code vector, otherwise
; it's a label within that code vector.

(define (cant.setrtn/strtn/jump as L align offset)
  (arm64.code-address as L)
  (arm64.alignment as align 12)
  (arm64.ADR*  as arm64.TMP2 12)
  (arm64.store as 8 arm64.TMP2 arm64.STKP offset)
  (arm64.BLR   as arm64.TMP1)
  (assert (zero? (remainder (here as) align))))

; Set the return address of the instruction after the branch, store it
; in the frame at byte offset "offset", and branch.  The sequence must
; be aligned so that the return point has the required alignment.

(define (cant.setrtn/strtn/branch as L align offset)
  (arm64.alignment as align 12)
  (arm64.ADR*  as arm64.TMP2 12)
  (arm64.store as 8 arm64.TMP2 arm64.STKP offset)
  (arm64.BL    as L)
  (assert (zero? (remainder (here as) align))))

(define (cant.ba as L)
  (arm64.B as L))

(define (cant.bcc as condition r1 r2 L)
  (arm64.bccx as condition (arm64.reg r1) (arm64.reg r2) #f L))

(define (cant.bcci as condition r imm L)
  (arm64.bccx as condition (arm64.reg r) imm #t L))

(define (cant.setcc as condition r1 r2 rd)
  (arm64.setccx as condition (arm64.reg rd) (arm64.reg r1) (arm64.reg r2) #f))

(define (cant.setcci as condition r imm rd)
  (arm64.setccx as condition (arm64.reg rd) (arm64.reg r) imm #t))

(define (cant.addi as rs imm rd)
  (arm64.addi as (arm64.reg rd) (arm64.reg rs) imm))

; The overflow-checking operations compute into TMP1 and move the
; result to rd only if there was no overflow, so rd is not updated on
; overflow.

(define (cant.addi/bnv as rs imm rd L)
  (arm64.addi/flags as arm64.TMP1 (arm64.reg rs) imm)
  (arm64.move-if-no-overflow as (arm64.reg rd) L))

(define (cant.subi as rs imm rd)
  (arm64.addi as (arm64.reg rd) (arm64.reg rs) (- imm)))

(define (cant.subi/bnv as rs imm rd L)
  (arm64.subi/flags as arm64.TMP1 (arm64.reg rs) imm)
  (arm64.move-if-no-overflow as (arm64.reg rd) L))

(define (cant.shli as rs imm rd)
  (arm64.LSLI as (arm64.reg rd) (arm64.reg rs) imm))

(define (cant.shri as rs imm rd)
  (arm64.LSRI as (arm64.reg rd) (arm64.reg rs) imm))

(define (cant.shrai as rs imm rd)
  (arm64.ASRI as (arm64.reg rd) (arm64.reg rs) imm))

(define (cant.andni as rs imm rd)
  (let ((rs (arm64.reg rs))
        (rd (arm64.reg rd)))
    (cond ((arm64.logical-immediate (- arm64.2^64 1 (arm64.u64 imm)))
           => (lambda (enc)
                (arm64.ANDI as rd rs enc)))
          (else
           (arm64.setval as arm64.TMP1 imm)
           (arm64.BIC    as rd rs arm64.TMP1)))))

(define (cant.add as rs1 rs2 rd)
  (arm64.ADD as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.add/bnv as rs1 rs2 rd L)
  (arm64.ADDS as arm64.TMP1 (arm64.reg rs1) (arm64.reg rs2))
  (arm64.move-if-no-overflow as (arm64.reg rd) L))

(define (cant.sub as rs1 rs2 rd)
  (arm64.SUB as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.sub/bnv as rs1 rs2 rd L)
  (arm64.SUBS as arm64.TMP1 (arm64.reg rs1) (arm64.reg rs2))
  (arm64.move-if-no-overflow as (arm64.reg rd) L))

(define (cant.mul as rs1 rs2 rd)
  (arm64.MUL as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

; The product overflows iff the high 64 bits of the 128-bit product
; are not the sign extension of the low 64 bits.

(define (cant.mul/bnv as rs1 rs2 rd L)
  (let ((rs1 (arm64.reg rs1))
        (rs2 (arm64.reg rs2)))
    (arm64.MUL        as arm64.TMP1 rs1 rs2)
    (arm64.SMULH      as arm64.TMP2 rs1 rs2)
    (arm64.CMP/ASR    as arm64.TMP2 arm64.TMP1 63)
    (arm64.CSEL       as (arm64.reg rd) arm64.TMP1 (arm64.reg rd) arm64-cond.EQ)
    (arm64.Bcond      as arm64-cond.EQ L)))

(define (cant.and as rs1 rs2 rd)
  (arm64.AND as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.or as rs1 rs2 rd)
  (arm64.ORR as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.xor as rs1 rs2 rd)
  (arm64.EOR as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.shl as rs1 rs2 rd)
  (arm64.LSLV as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.shr as rs1 rs2 rd)
  (arm64.LSRV as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.shra as rs1 rs2 rd)
  (arm64.ASRV as (arm64.reg rd) (arm64.reg rs1) (arm64.reg rs2)))

(define (cant.not as rs rd)
  (arm64.MVN as (arm64.reg rd) (arm64.reg rs)))

; Floating operations.  Loads and stores through tagged pointers have
; offsets that are not multiples of 8, so they mostly use the
; unscaled forms.

(define (cant.fldi as base offset frd)
  (arm64.load as 'd (arm64.freg frd) (arm64.reg base) offset))

(define (cant.fsti as frs base offset)
  (arm64.store as 'd (arm64.freg frs) (arm64.reg base) offset))

(define (cant.fadd as frs1 frs2 frd)
  (arm64.FADD as (arm64.freg frd) (arm64.freg frs1) (arm64.freg frs2)))

(define (cant.fsub as frs1 frs2 frd)
  (arm64.FSUB as (arm64.freg frd) (arm64.freg frs1) (arm64.freg frs2)))

(define (cant.fmul as frs1 frs2 frd)
  (arm64.FMUL as (arm64.freg frd) (arm64.freg frs1) (arm64.freg frs2)))

(define (cant.fdiv as frs1 frs2 frd)
  (arm64.FDIV as (arm64.freg frd) (arm64.freg frs1) (arm64.freg frs2)))

; After FCMP an unordered result sets NZCV to 0011.  The conditions
; below are all false for unordered operands except NE, as required
; by Scheme's = and < etc on NaNs.

(define (cant.fsetcc as condition frs1 frs2 rd)
  (arm64.FCMP as (arm64.freg frs1) (arm64.freg frs2))
  (arm64.set-boolean as (arm64.reg rd)
                     (case condition
                       ((equal) arm64-cond.EQ)
                       ((not-equal) arm64-cond.NE)
                       ((less) arm64-cond.MI)
                       ((greater) arm64-cond.GT)
                       ((less-or-equal) arm64-cond.LS)
                       ((greater-or-equal) arm64-cond.GE)
                       (else (error 'fsetcc "Bad condition: " condition)))))

;;; Utilities

; Emit NOPs so that if 'bytes' bytes of code follows the NOPs then
; the end of those bytes will be aligned on an 'align' boundary.

(define (arm64.alignment as align bytes)
  (let loop ()
    (if (not (zero? (remainder (+ (here as) bytes) align)))
        (begin
          (cant.nop as)
          (loop)))))

; Map a MAL register to an AArch64 hardware register.

(define arm64.reg
  (let* ((offset (abs (min $r.tmp $r.globals $r.stkp $r.second $r.result)))
         (v      (make-vector (+ *nregs* offset) -1))
         (regs   (vector arm64.REG0 arm64.REG1 arm64.REG2 arm64.REG3
                         arm64.REG4 arm64.REG5 arm64.REG6 arm64.REG7
                         arm64.REG8 arm64.REG9 arm64.REG10 arm64.REG11
                         arm64.REG12 arm64.REG13 arm64.REG14 arm64.REG15)))
    (vector-set! v (+ offset $r.tmp) arm64.TMP0)
    (vector-set! v (+ offset $r.globals) arm64.GLOBALS)
    (vector-set! v (+ offset $r.stkp) arm64.STKP)
    (vector-set! v (+ offset $r.second) arm64.SECOND)
    (vector-set! v (+ offset $r.result) arm64.RESULT)
    (do ((i 0 (+ i 1)))
        ((= i *nregs*))
      (vector-set! v (+ offset i $r.reg0) (vector-ref regs i)))
    (lambda (r)
      (vector-ref v (+ r offset)))))

; Floating registers F0..F3 are d8..d11.

(define (arm64.freg x)
  (+ x 8))

(define arm64.2^64 18446744073709551616)
(define arm64.2^63  9223372036854775808)

; The value v as an unsigned 64-bit quantity.

(define (arm64.u64 v)
  (if (negative? v) (+ arm64.2^64 v) v))

; Emit a 32-bit instruction word.  The instruction is computed as a
; nonnegative exact integer from non-overlapping fields, so plain
; arithmetic suffices and the code works on hosts with 30-bit fixnums.

(define (arm64.emit as instr)
  (let ((buf (make-bytevector 4)))
    (bytevector-set! buf 0 (remainder instr 256))
    (bytevector-set! buf 1 (remainder (quotient instr 256) 256))
    (bytevector-set! buf 2 (remainder (quotient instr 65536) 256))
    (bytevector-set! buf 3 (quotient instr 16777216))
    (emit! as buf)))

; Add n to the instruction word at loc in the code vector cv.  The
; field being filled in must be zero.

(define (arm64.patch! cv loc n)
  (let* ((old (+ (bytevector-ref cv loc)
                 (* 256 (bytevector-ref cv (+ loc 1)))
                 (* 65536 (bytevector-ref cv (+ loc 2)))
                 (* 16777216 (bytevector-ref cv (+ loc 3)))))
         (new (+ old n)))
    (bytevector-set! cv (+ loc 0) (remainder new 256))
    (bytevector-set! cv (+ loc 1) (remainder (quotient new 256) 256))
    (bytevector-set! cv (+ loc 2) (remainder (quotient new 65536) 256))
    (bytevector-set! cv (+ loc 3) (quotient new 16777216))))

; Encode a signed value v in a field of n bits.

(define (arm64.field v n)
  (let ((m (expt 2 n)))
    (assert (and (<= (- (quotient m 2)) v) (< v (quotient m 2))))
    (if (negative? v) (+ m v) v)))

; Load an immediate into a register, using the shortest sequence we
; know how to generate.

(define (arm64.setval as rd imm)
  (let* ((u      (arm64.u64 imm))
         (chunks (arm64.chunks u))
         (zeros  (arm64.count 0 chunks))
         (ones   (arm64.count 65535 chunks)))
    (cond ((and (< zeros 3) (< ones 3) (arm64.logical-immediate u))
           => (lambda (enc)
                (arm64.ORRI as rd arm64.ZR enc)))
          ((> ones zeros)
           (arm64.setval-sequence as rd chunks 65535 arm64.MOVN
                                  (lambda (c) (- 65535 c))))
          (else
           (arm64.setval-sequence as rd chunks 0 arm64.MOVZ
                                  (lambda (c) c))))))

; Emit MOVZ or MOVN for the first chunk that is not 'fill', then MOVK
; for the remaining chunks that are not 'fill'.

(define (arm64.setval-sequence as rd chunks fill first-op first-value)
  (let loop ((i 0) (chunks chunks) (first? #t))
    (cond ((null? chunks)
           (if first?
               (first-op as rd (first-value fill) 0)))
          ((= (car chunks) fill)
           (loop (+ i 1) (cdr chunks) first?))
          (first?
           (first-op as rd (first-value (car chunks)) i)
           (loop (+ i 1) (cdr chunks) #f))
          (else
           (arm64.MOVK as rd (car chunks) i)
           (loop (+ i 1) (cdr chunks) #f)))))

; The four 16-bit chunks of a 64-bit value, least significant first.

(define (arm64.chunks u)
  (list (remainder u 65536)
        (remainder (quotient u 65536) 65536)
        (remainder (quotient u 4294967296) 65536)
        (quotient u 281474976710656)))

(define (arm64.count x l)
  (cond ((null? l) 0)
        ((= x (car l)) (+ 1 (arm64.count x (cdr l))))
        (else (arm64.count x (cdr l)))))

; Load into register r a value representing the offset of label L
; plus an adjustment.  When the label is not yet defined this is
; always two instructions, patched when the label is known; label
; offsets are nonnegative and less than 2^32.

(define (arm64.label->reg as L r adjust)
  (if (label-value as L)
      (arm64.setval as r (+ adjust (label-value as L)))
      (begin
        (emit-fixup-proc!
         as (lambda (cv loc)
              (let ((v (+ adjust (label-value as L))))
                (assert (<= 0 v 4294967295))
                (arm64.patch! cv loc (* 32 (remainder v 65536)))
                (arm64.patch! cv (+ loc 4) (* 32 (quotient v 65536))))))
        (arm64.MOVZ as r 0 0)
        (arm64.MOVK as r 0 1))))

; rd = rs + imm, for any 64-bit imm.  Uses TMP2 when the immediate
; does not fit, so neither rd nor rs can be TMP2 in that case.

(define (arm64.addi as rd rs imm)
  (arm64.addi-general as rd rs imm #f))

(define (arm64.addi/flags as rd rs imm)
  (arm64.addi-general as rd rs imm #t))

(define (arm64.subi/flags as rd rs imm)
  (cond ((<= 0 imm 4095)
         (arm64.SUBSI as rd rs imm))
        (else
         (arm64.setval as arm64.TMP2 imm)
         (arm64.SUBS   as rd rs arm64.TMP2))))

(define (arm64.addi-general as rd rs imm set-flags?)
  (cond ((and (zero? imm) (not set-flags?))
         (arm64.MOV as rd rs))
        ((<= 0 imm 4095)
         ((if set-flags? arm64.ADDSI arm64.ADDI) as rd rs imm))
        ((<= -4095 imm -1)
         ((if set-flags? arm64.SUBSI arm64.SUBI) as rd rs (- imm)))
        ((and (not set-flags?)
              (zero? (remainder imm 4096))
              (<= 0 (quotient imm 4096) 4095))
         (arm64.ADDI12 as rd rs (quotient imm 4096)))
        (else
         (assert (not (= rs arm64.TMP2)))
         (arm64.setval as arm64.TMP2 imm)
         ((if set-flags? arm64.ADDS arm64.ADD) as rd rs arm64.TMP2))))

; After a flag-setting operation that left its result in TMP1: move
; TMP1 to rd and branch to L if there was no overflow.

(define (arm64.move-if-no-overflow as rd L)
  (arm64.CSEL  as rd arm64.TMP1 rd arm64-cond.VC)
  (arm64.Bcond as arm64-cond.VC L))

; Loads and stores.  size is 8, 4, or 1 for integer loads (which are
; zero-extending), or d for a double-precision floating register.
;
; Use the scaled unsigned offset form when possible, then the
; unscaled signed 9-bit form, then a register index in TMP2.

(define (arm64.load as size rt base offset)
  (arm64.ldst as size rt base offset #t))

(define (arm64.store as size rt base offset)
  (arm64.ldst as size rt base offset #f))

(define (arm64.ldst as size rt base offset load?)
  (let* ((bytes (if (eq? size 'd) 8 size))
         (ops   (case size
                  ((8) (if load? arm64.op.LDRX arm64.op.STRX))
                  ((4) (if load? arm64.op.LDRW arm64.op.STRW))
                  ((1) (if load? arm64.op.LDRB arm64.op.STRB))
                  ((d) (if load? arm64.op.LDRD arm64.op.STRD))
                  (else (error 'arm64.ldst "Bad size " size)))))
    (cond ((and (>= offset 0)
                (zero? (remainder offset bytes))
                (< (quotient offset bytes) 4096))
           (arm64.emit as (+ (vector-ref ops 0)
                             (* 1024 (quotient offset bytes))
                             (* 32 base)
                             rt)))
          ((<= -256 offset 255)
           (arm64.emit as (+ (vector-ref ops 1)
                             (* 4096 (arm64.field offset 9))
                             (* 32 base)
                             rt)))
          (else
           (assert (not (= base arm64.TMP2)))
           (assert (or (eq? size 'd) (not (= rt arm64.TMP2))))
           (arm64.setval as arm64.TMP2 offset)
           (arm64.emit as (+ (vector-ref ops 2)
                             (* 65536 arm64.TMP2)
                             (* 32 base)
                             rt))))))

; Opcodes for: unsigned scaled offset, unscaled offset, register offset.

(define arm64.op.LDRX '#(#xF9400000 #xF8400000 #xF8606800))
(define arm64.op.STRX '#(#xF9000000 #xF8000000 #xF8206800))
(define arm64.op.LDRW '#(#xB9400000 #xB8400000 #xB8606800))
(define arm64.op.STRW '#(#xB9000000 #xB8000000 #xB8206800))
(define arm64.op.LDRB '#(#x39400000 #x38400000 #x38606800))
(define arm64.op.STRB '#(#x39000000 #x38000000 #x38206800))
(define arm64.op.LDRD '#(#xFD400000 #xFC400000 #xFC606800))
(define arm64.op.STRD '#(#xFD000000 #xFC000000 #xFC206800))

(define (arm64.bccx as condition r1 src2 src2-imm? L)
  (cond ((and src2-imm?
              (eqv? src2 0)
              (memq condition '(equal not-equal)))
         ((if (eq? condition 'equal) arm64.CBZ arm64.CBNZ) as r1 L))
        (else
         (arm64.generate-condition as condition r1 src2 src2-imm?)
         (arm64.Bcond as (arm64.condition-code condition) L))))

(define (arm64.setccx as condition rd r1 src2 src2-imm?)
  (arm64.generate-condition as condition r1 src2 src2-imm?)
  (arm64.set-boolean as rd (arm64.condition-code condition)))

; rd = cond ? #t : #f.  The representations of #t and #f differ only
; in bit 2.

(define (arm64.set-boolean as rd cond)
  (assert (= (- $imm.true $imm.false) 4))
  (arm64.CSET as arm64.TMP1 cond)
  (arm64.LSLI as arm64.TMP1 arm64.TMP1 2)
  (arm64.ADDI as rd arm64.TMP1 $imm.false))

(define (arm64.condition-code condition)
  (case condition
    ((equal low1-equal low2-equal low3-equal low8-equal) arm64-cond.EQ)
    ((not-equal low1-not-equal low2-not-equal low3-not-equal low8-not-equal) arm64-cond.NE)
    ((less) arm64-cond.LT)
    ((greater) arm64-cond.GT)
    ((less-or-equal) arm64-cond.LE)
    ((greater-or-equal) arm64-cond.GE)
    ((unsigned-less) arm64-cond.LO)
    (else (error 'arm64.condition-code "Bad condition: " condition))))

(define (arm64.generate-condition as condition r1 src2 src2-imm?)
  (let ((mask (case condition
                ((low1-equal low1-not-equal) 1)
                ((low2-equal low2-not-equal) 3)
                ((low3-equal low3-not-equal) 7)
                ((low8-equal low8-not-equal) 255)
                (else #f))))
    (cond ((and mask src2-imm? (= src2 0))
           (arm64.TSTI as r1 (arm64.logical-immediate mask)))
          (mask
           (arm64.ANDI as arm64.TMP1 r1 (arm64.logical-immediate mask))
           (cond ((not src2-imm?)
                  (arm64.CMP as arm64.TMP1 src2))
                 (else
                  ;; The precondition for using low<n>-{not-,}equal is
                  ;; that the immediate, if any, fits in n bits.
                  (assert (<= 0 src2 255))
                  (arm64.CMPI as arm64.TMP1 src2))))
          ((not src2-imm?)
           (arm64.CMP as r1 src2))
          ((<= 0 src2 4095)
           (arm64.CMPI as r1 src2))
          ((<= -4095 src2 -1)
           (arm64.CMNI as r1 (- src2)))
          (else
           (arm64.setval as arm64.TMP1 src2)
           (arm64.CMP as r1 arm64.TMP1)))))

; Encode a 64-bit value as a logical ("bitmask") immediate.  Returns
; #f if that is not possible, otherwise the 13-bit N:immr:imms field
; as an integer ready to be shifted into bit position 10.
;
; A bitmask immediate is a pattern of 2, 4, 8, 16, 32, or 64 bits,
; replicated to fill 64 bits, where the pattern is a rotated run of
; one or more ones that is not all ones.

(define (arm64.logical-immediate u)

  (define (pattern-size)
    (let loop ((size 64))
      (if (= size 2)
          2
          (let* ((half (quotient size 2))
                 (m    (expt 2 half))
                 (lo   (remainder u m))
                 (hi   (remainder (quotient u m) m)))
            (if (= lo hi)
                (loop half)
                size)))))

  (define (count-ones x)
    (if (zero? x)
        0
        (+ (remainder x 2) (count-ones (quotient x 2)))))

  ; Rotate the size-bit value x right by r bits.
  (define (ror x r size)
    (let ((m (expt 2 r)))
      (+ (quotient x m)
         (* (remainder x m) (expt 2 (- size r))))))

  (let* ((size (pattern-size))
         (elt  (remainder u (expt 2 size)))
         (ones (count-ones elt)))
    (if (or (zero? ones) (= ones size))
        #f
        (let ((run (- (expt 2 ones) 1)))
          (let loop ((r 0))
            (cond ((= r size) #f)
                  ((= (ror run r size) elt)
                   (let ((N    (if (= size 64) 1 0))
                         (imms (+ (remainder (- 128 (* 2 size)) 64)
                                  (- ones 1))))
                     (+ (* N 4096) (* r 64) imms)))
                  (else (loop (+ r 1)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; AArch64 machine assembler.
;
; Register operands are hardware register numbers; 31 denotes the
; zero register in all the operations below except ADDI, SUBI,
; ADDI12 (where it would denote SP, which we never use).

(define arm64.ZR 31)

(define arm64-cond.EQ 0)
(define arm64-cond.NE 1)
(define arm64-cond.HS 2)
(define arm64-cond.LO 3)
(define arm64-cond.MI 4)
(define arm64-cond.PL 5)
(define arm64-cond.VS 6)
(define arm64-cond.VC 7)
(define arm64-cond.HI 8)
(define arm64-cond.LS 9)
(define arm64-cond.GE 10)
(define arm64-cond.LT 11)
(define arm64-cond.GT 12)
(define arm64-cond.LE 13)

(define (arm64.rd-rn-rm base)
  (lambda (as rd rn rm)
    (arm64.emit as (+ base (* 65536 rm) (* 32 rn) rd))))

(define arm64.ADD  (arm64.rd-rn-rm #x8B000000))
(define arm64.ADDS (arm64.rd-rn-rm #xAB000000))
(define arm64.SUB  (arm64.rd-rn-rm #xCB000000))
(define arm64.SUBS (arm64.rd-rn-rm #xEB000000))
(define arm64.AND  (arm64.rd-rn-rm #x8A000000))
(define arm64.BIC  (arm64.rd-rn-rm #x8A200000))
(define arm64.ORR  (arm64.rd-rn-rm #xAA000000))
(define arm64.EOR  (arm64.rd-rn-rm #xCA000000))
(define arm64.LSLV (arm64.rd-rn-rm #x9AC02000))
(define arm64.LSRV (arm64.rd-rn-rm #x9AC02400))
(define arm64.ASRV (arm64.rd-rn-rm #x9AC02800))
(define arm64.MUL  (arm64.rd-rn-rm #x9B007C00))  ; MADD with XZR addend
(define arm64.SMULH (arm64.rd-rn-rm #x9B407C00))

(define (arm64.CMP as rn rm)
  (arm64.SUBS as arm64.ZR rn rm))

; CMP rn, rm, ASR #amount

(define (arm64.CMP/ASR as rn rm amount)
  (assert (<= 0 amount 63))
  (arm64.emit as (+ #xEB800000 (* 65536 rm) (* 1024 amount) (* 32 rn) arm64.ZR)))

(define (arm64.MOV as rd rs)
  (if (not (= rd rs))
      (arm64.ORR as rd arm64.ZR rs)))

(define (arm64.MVN as rd rs)                    ; ORN rd, xzr, rs
  (arm64.emit as (+ #xAA200000 (* 65536 rs) (* 32 arm64.ZR) rd)))

(define (arm64.imm12 base)
  (lambda (as rd rn imm)
    (assert (<= 0 imm 4095))
    (arm64.emit as (+ base (* 1024 imm) (* 32 rn) rd))))

(define arm64.ADDI   (arm64.imm12 #x91000000))
(define arm64.ADDSI  (arm64.imm12 #xB1000000))
(define arm64.SUBI   (arm64.imm12 #xD1000000))
(define arm64.SUBSI  (arm64.imm12 #xF1000000))
(define arm64.ADDI12 (arm64.imm12 #x91400000))  ; imm is shifted left 12

(define (arm64.CMPI as rn imm)
  (arm64.SUBSI as arm64.ZR rn imm))

(define (arm64.CMNI as rn imm)
  (arm64.ADDSI as arm64.ZR rn imm))

; Logical immediates: enc is the result of arm64.logical-immediate.

(define (arm64.logical-imm base)
  (lambda (as rd rn enc)
    (assert enc)
    (arm64.emit as (+ base (* 1024 enc) (* 32 rn) rd))))

(define arm64.ANDI  (arm64.logical-imm #x92000000))
(define arm64.ORRI  (arm64.logical-imm #xB2000000))
(define arm64.EORI  (arm64.logical-imm #xD2000000))
(define arm64.ANDSI (arm64.logical-imm #xF2000000))

(define (arm64.TSTI as rn enc)
  (arm64.ANDSI as arm64.ZR rn enc))

; Wide moves: hw selects the 16-bit chunk (0..3).

(define (arm64.wide-move base)
  (lambda (as rd imm hw)
    (assert (and (<= 0 imm 65535) (<= 0 hw 3)))
    (arm64.emit as (+ base (* 2097152 hw) (* 32 imm) rd))))

(define arm64.MOVN (arm64.wide-move #x92800000))
(define arm64.MOVZ (arm64.wide-move #xD2800000))
(define arm64.MOVK (arm64.wide-move #xF2800000))

; Shifts by immediate are aliases of UBFM and SBFM.

(define (arm64.bitfield base)
  (lambda (as rd rn immr imms)
    (arm64.emit as (+ base (* 65536 immr) (* 1024 imms) (* 32 rn) rd))))

(define arm64.UBFM (arm64.bitfield #xD3400000))
(define arm64.SBFM (arm64.bitfield #x93400000))

(define (arm64.LSLI as rd rn shift)
  (assert (<= 0 shift 63))
  (if (zero? shift)
      (arm64.MOV as rd rn)
      (arm64.UBFM as rd rn (- 64 shift) (- 63 shift))))

(define (arm64.LSRI as rd rn shift)
  (assert (<= 0 shift 63))
  (arm64.UBFM as rd rn shift 63))

(define (arm64.ASRI as rd rn shift)
  (assert (<= 0 shift 63))
  (arm64.SBFM as rd rn shift 63))

; Conditional select.

(define (arm64.CSEL as rd rn rm cond)
  (arm64.emit as (+ #x9A800000 (* 65536 rm) (* 4096 cond) (* 32 rn) rd)))

(define (arm64.CSET as rd cond)                 ; CSINC rd, xzr, xzr, !cond
  (arm64.emit as (+ #x9A800400
                    (* 65536 arm64.ZR)
                    (* 4096 (arm64.invert-condition cond))
                    (* 32 arm64.ZR)
                    rd)))

(define (arm64.invert-condition cond)
  (if (even? cond) (+ cond 1) (- cond 1)))

; PC-relative address of label L plus adjust.

(define (arm64.ADR as rd L adjust)
  (let ((loc (here as)))
    (if (label-value as L)
        (arm64.ADR* as rd (- (+ (label-value as L) adjust) loc))
        (begin
          (emit-fixup-proc!
           as (lambda (cv loc)
                (arm64.patch! cv loc (arm64.adr-bits (- (+ (label-value as L) adjust) loc)))))
          (arm64.ADR* as rd 0)))))

; ADR with a known byte offset from this instruction.

(define (arm64.ADR* as rd offset)
  (arm64.emit as (+ #x10000000 (arm64.adr-bits offset) rd)))

(define (arm64.adr-bits offset)
  (let ((v (arm64.field offset 21)))
    (+ (* 536870912 (remainder v 4))              ; immlo, bits 29..30
       (* 32 (quotient v 4)))))                   ; immhi, bits 5..23

; Branches.  The offset of a branch is relative to the branch
; instruction itself, and counted in instructions.

(define (arm64.branch as L make-instr patch-bits)
  (let ((loc (here as)))
    (if (label-value as L)
        (arm64.emit as (make-instr (patch-bits (quotient (- (label-value as L) loc) 4))))
        (begin
          (emit-fixup-proc!
           as (lambda (cv loc)
                (let ((offset (- (label-value as L) loc)))
                  (assert (zero? (remainder offset 4)))
                  (arm64.patch! cv loc (patch-bits (quotient offset 4))))))
          (arm64.emit as (make-instr 0))))))

(define (arm64.imm26-bits n)
  (arm64.field n 26))

(define (arm64.imm19-bits n)
  (* 32 (arm64.field n 19)))

(define (arm64.B as L)
  (arm64.branch as L (lambda (bits) (+ #x14000000 bits)) arm64.imm26-bits))

(define (arm64.BL as L)
  (arm64.branch as L (lambda (bits) (+ #x94000000 bits)) arm64.imm26-bits))

(define (arm64.Bcond as cond L)
  (arm64.branch as L (lambda (bits) (+ #x54000000 bits cond)) arm64.imm19-bits))

(define (arm64.CBZ as rt L)
  (arm64.branch as L (lambda (bits) (+ #xB4000000 bits rt)) arm64.imm19-bits))

(define (arm64.CBNZ as rt L)
  (arm64.branch as L (lambda (bits) (+ #xB5000000 bits rt)) arm64.imm19-bits))

(define (arm64.BR as rn)
  (arm64.emit as (+ #xD61F0000 (* 32 rn))))

(define (arm64.BLR as rn)
  (arm64.emit as (+ #xD63F0000 (* 32 rn))))

(define (arm64.RET as rn)
  (arm64.emit as (+ #xD65F0000 (* 32 rn))))

(define (arm64.NOP as)
  (arm64.emit as #xD503201F))

; Double-precision floating point.

(define arm64.FADD (arm64.rd-rn-rm #x1E602800))
(define arm64.FSUB (arm64.rd-rn-rm #x1E603800))
(define arm64.FMUL (arm64.rd-rn-rm #x1E600800))
(define arm64.FDIV (arm64.rd-rn-rm #x1E601800))

(define (arm64.FCMP as rn rm)
  (arm64.emit as (+ #x1E602000 (* 65536 rm) (* 32 rn))))

; eof

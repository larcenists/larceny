;; Copyright 2012 Lars T Hansen.
;;
;; PowerPC 750 overrides for the Cant primitives.
;;
;; 2012-06-28 / lth@acm.org
;;
;; This mapping has /no/ software registers.

;; TBD
;; PowerPC hardware register assignments for MacScheme registers.

(define ppc.RESULT 1)
(define ppc.SECOND 2)
(define ppc.GLOBALS 3)
(define ppc.CONT 4)
(define ppc.TMP0 5)
(define ppc.TMP1 6)
(define ppc.REG0 7)
(define ppc.REG1 8)
(define ppc.REG2 9)
(define ppc.REG3 10)
(define ppc.REG4 11)
(define ppc.REG5 12)
(define ppc.REG6 13)
(define ppc.REG7 14)
(define ppc.REG8 15)
(define ppc.REG9 16)
(define ppc.REG10 17)
(define ppc.REG11 18)
(define ppc.REG12 19)
(define ppc.REG13 20)
(define ppc.REG14 21)
(define ppc.REG15 22)
(define ppc.REG16 23)
(define ppc.REG17 24)

;; Bit indices when looking at CR0 following normal arithmetic

(define ppc.cr0/lt 0)
(define ppc.cr0/gt 1)
(define ppc.cr0/eq 2)

;; Bit indices when looking at CR2 following mcrxr 2

(define ppc.cr2/so 8)
(define ppc.cr2/ov 9)
(define ppc.cr2/ca 10)

;; Control codes used with bc

(define ppc.iffalse/not-taken 4)
(define ppc.iffalse/taken 5)
(define ppc.iftrue/not-taken 12)
(define ppc.iftrue/taken 13)
(define ppc.always 20)

; TODO: ppc.bla should come in handy here...

(define (cant.trap as code name)
  #t)

(define (cant.nop as)
  (ppc.ori as 0 0 0))

(define (cant.ldi as base offset rd)
  (let ((base (ppc.reg base))
        (rd   (ppc.reg rd)))
    (if (<= -32768 offset 32767)
        (ppc.lzw as rd base offset)
        (begin
          (ppc.setval as ppc.TMP1 offset)
          (ppc.lwzx   as rd base ppc.TMP1)))))

(define (cant.ldbiz as base offset rd)
  #t)

(define (cant.sti as rs base offset)
  #t)

(define (cant.stbi as rs base offset)
  #t)

(define (cant.movi as imm rd)
  (ppc.setvalue as (ppc.reg rd) imm))

(define (cant.mov as rs rd)
  (ppc.mr as (ppc.reg rd) (ppc.reg rs)))

(define (cant.return as r)
  (ppc.mtlr as (ppc.reg r))
  (ppc.bclr as ppc.always 0))

;; addi is limited to a 16-bit signed field.

(define (cant.setrtn as L r)
  (ppc.bl as 4)                         ; LR=(here)+4
  ...)

(define (cant.jump as L)
  #t)

;; Displacement is 27 bits, signed.

(define (cant.ba as L)
  (ppc.branch ppc as L))

;; For conditional branches we only get a 16-bit signed displacement, ie, +/- 32KB.
;; We can use the short-effective-addresses hack in the assembler to get around that.
;; Call asm-value-too-large to signal that.

(define (cant.bcc as condition r1 r2 L)
  (arm.bccx as condition (arm.reg r1) (arm.reg r2) #f arm.CMPI L))

(define (cant.bcci as condition r imm L)
  (arm.bccx as condition (arm.reg r) imm #t arm.CMP L))

(define (cant.setcc as condition r1 r2 rd)
  (arm.setccx as condition (arm.reg rd) (arm.reg r1) (arm.reg r2) #f arm.CMP))

(define (cant.setci as condition r imm rd)
  (arm.setccx as condition (arm.reg rd) (arm.reg r) imm #t arm.CMPI))

(define (cant.sma as rs shift mask add rd)
  #t)

(define (cant.add as rs1 rs2 rd L)
  (ppc.add/sub as ppc.addo ppc.add ppc.sub (ppc.reg rd) (ppc.reg rs1) (ppc.reg rs2) L))

(define (cant.sub as rs1 rs2 rd L)
  (ppc.add/sub as ppc.subo ppc.sub ppc.add (ppc.reg rd) (ppc.reg rs1) (ppc.reg rs2) L))

(define (cant.mul as rs1 rs2 rd L)
  (let ((rs1 (ppc.reg rs1))
        (rs2 (ppc.reg rs2))
        (rd  (ppc.reg rd)))
    (cond ((not L)
           (ppc.mullw as rd rs1 rs2))
          (else
           (let ((Lx (fence.label as)))
             (ppc.srawi   as ppc.TMP1 rs1 2)
             (ppc.mullwo  as ppc.TMP1 ppc.TMP1 rs2)
             (ppc.mcrxr   as 2)
             (ppc.bc      as ppc.iftrue/not-taken ppc.cr2/ov Lx)
             (ppc.mr      as rd ppc.TMP1)
             (ppc.b       as L)
             (fence.label as Lx))))))

(define (cant.and as rs1 rs2 rd)
  (pcc.and as (ppc.reg rd) (ppc.reg rs1) (ppc.reg rs2)))

(define (cant.or as rs1 rs2 rd)
  (ppc.or as (ppc.reg rd) (ppc.reg rs1) (ppc.reg rs2)))

(define (cant.xor as rs1 rs2 rd)
  (ppc.xor as (ppc.reg rd) (ppc.reg rs1) (ppc.reg rs2)))

(define (cant.shl as rs1 rs2 rd)
  (ppc.slw as (ppc.reg rd) (ppc.reg rd1) (ppc.reg rd2)))

(define (cant.shr as rs1 rs2 rd)
  (ppc.srw as (ppc.reg rd) (ppc.reg rd1) (ppc.reg rd2)))

(define (cant.shra as rs1 rs2 rd)
  (ppc.sraw as (ppc.reg rd) (ppc.reg rd1) (ppc.reg rd2)))

(define (cant.not as rs rd)
  (ppc.nor as (ppc.reg rd) (ppc.reg rs) (ppc.reg rs)))

;; PPC helpers use same argument ordering as PPC instructions.
;; Registers have all been translated.

(define (ppc.setvalue as rd imm)
  (let ((imm (if (negative? imm) (+ #x100000000 imm) imm)))
    (cond ((<= imm 32767)
           (ppc.addi as rd 0 imm))
          ((and (= #xFFFF (quotient imm 65536)) (>= 32768 (remainder imm 65536)))
           (ppc.addi as rd 0 (remainder imm 65536)))
          ((zero? (remainder imm 65536))
           (ppc.addis as rd 0 (quotient imm 65536)))
          (else
           (ppc.addis as rd 0 (quotient imm 65536))
           (ppc.ori   as rd rd (remainder imm 65536))))))

;; No conditional move on PPC; use a do/undo pattern where possible.

(define (ppc.add/sub as opo op rev rd rs1 rs2 L)
  (cond ((not L)
         (op as rd rs1 rs2))
        ((and (= rd rs1) (not (= rd rs2)))
         (opo       as rd rd rs2)
         (ppc.mcrxr as 2)
         (ppc.bc    as ppc.iffalse/taken ppc.cr2/ov L)
         (rev       as rd rd rs2))
        ((and (= rd rs2) (not (= rd rs1)))
         (opo       as rd rs1 rd)
         (ppc.mcrxr as 2)
         (ppc.bc    as ppc.iffalse/taken ppc.cr2/ov L)
         (rev       as rd rd rs1))
        (else
         (let ((Lx (fence.label as)))
           (opo         as ppc.TMP1 rs1 rs2)
           (ppc.mcrxr   as 2)
           (ppc.bc      as ppc.iftrue/not-taken ppc.cr2/ov Lx)
           (ppc.mr      as rd ppc.TMP1)
           (ppc.b       as L)
           (fence.label as Lx)))))

(define (ppc.branch as condition L)
  ...)

(define (ppc.bccx as condition r1 src2 imm? cmp L)
  (let ((field-compare? (case condition
                          ((low2-equal low2-not-equal low3-equal low3-not-equal low8-equal low8-not-equal) #t)
                          (else #f))))
    (case condition
      ((low2-equal low2-not-equal) (ppc.andi. as ppc.TMP1 r1 3))
      ((low3-equal low3-not-equal) (ppc.andi. as ppc.TMP1 r1 7))
      ((low8-equal low8-not-equal) (ppc.andi. as ppc.TMP1 r1 255)))
    (cond ((and field-compare? imm? (= 0 src2)))
          (field-compare?
           (cmp as ppc.TMP1 src2))
          (else
           (cmp as r1 src2)))
    (ppc.branch as (case condition
                     ((equal low2-equal low3-equal low8-equal) ppc-cond.EQ)
                     ((not-equal low2-not-equal low3-not-equal low8-not-equal) ppc-cond.NE)
                     ((less) ppc-cond.LT)
                     ((greater) ppc-cond.GT)
                     ((less-or-equal) ppc-cond.LE)
                     ((greater-or-equal) ppc-cond.GE)
                     (else ???))
                L)))

(define (arm.setccx as condition rd r1 src2 imm? cmp)
  (let ((field-compare? (case condition
                          ((low2-equal low2-not-equal low3-equal low3-not-equal low8-equal low8-not-equal) #t)
                          (else #f))))
    (cond ((and field-compare? imm? (= 0 src2))
           (case condition
             ((low2-equal low2-not-equal) (arm.TSTI as r1 3))
             ((low3-equal low3-not-equal) (arm.TSTI as r1 7))
             ((low8-equal low8-not-equal) (arm.TSTI as r1 255))
             (else ???)))
          (field-compare?
           (case condition
             ((low2-equal low2-not-equal) (arm.ANDSI as arm.TMP1 r1 3))
             ((low3-equal low3-not-equal) (arm.ANDSI as arm.TMP1 r1 7))
             ((low8-equal low8-not-equal) (arm.ANDSI as arm.TMP1 r1 255)))
           (cmp as arm.TMP1 src2))
          (else
           (cmp as r1 src2)))
    (arm.MOVWI as rd $imm.false)
    ((case condition
       ((equal low2-equal low3-equal low8-equal) arm.ADDI.EQ)
       ((not-equal low2-not-equal low3-not-equal low8-not-equal) arm.ADDI.NE)
       ((less) arm.ADDI.LT)
       ((greater) arm.ADDI.GT)
       ((less-or-equal) arm.ADDI.LE)
       ((greater-or-equal) arm.ADDI.GE)
       (else ???))
     as rd (- $imm.true $imm.false))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PowerPC machine assembler.
;;
;; In all cases the immediates /must/ fit in the instruction, there is
;; no checking here.  Some (to be described) admit negative immediates
;; but otherwise immediates are always nonnegative.
;;
;; Note operand order (rd and rs1) varies in strange ways, according to
;; the spec.

(define (ppc.add as rd rs1 rs2)    (ppc.xo-form as 31 rd rs1 rs2 0 266 0))
(define (ppc.addo as rd rs1 rs2)   (ppc.xo-form as 31 rd rs1 rs2 1 266 0))
(define (ppc.and as rd rs1 rs2)    (ppc.x-form  as 31 rs1 rd rs2 28 0))
(define (ppc.mullw as rd rs1 rs2)  (ppc.xo-form as 31 rd rs1 rs2 0 235 0))
(define (ppc.mullwo as rd rs1 rs2) (ppc.xo-form as 31 rd rs1 rs2 1 235 0))
(define (ppc.nor as rd rs1 rs2)    (ppc.x-form  as 31 rs1 rd rs2 124 0))
(define (ppc.or as rd rs1 rs2)     (ppc.x-form  as 31 rs1 rd rs2 444 0))
(define (ppc.slw as rd rs1 rs2)    (ppc.x-form  as 31 rs1 rd rs2 24 0))
(define (ppc.sraw as rd rs1 rd2)   (ppc.x-form  as 31 rs1 rd rs2 792 0))
(define (ppc.srawi as rd rs imm)   (ppc.x-form  as 31 rs rd (fxlogand imm 31) 824 0))
(define (ppc.srw as rd rs1 rs2)    (ppc.x-form  as 31 rs1 rd rs2 536 0))
(define (ppc.subf as rd rs1 rs2)   (ppc.xo-form as 31 rd rs1 rs2 0 40 0))
(define (ppc.subfo as rd rs1 rs2)  (ppc.xo-form as 31 rd rs1 rs2 1 40 0))
(define (ppc.xor as rd rs1 rs2)    (ppc.x-form  as 31 rs1 rd rs2 316 0))

(define (ppc.addi as rd rs imm)  (ppc.d-form as 14 rd rs (ppc.simm16 imm)))
(define (ppc.addis as rd rs imm) (ppc.d-form as 15 rd rs (ppc.uimm16 imm)))
(define (ppc.andi. as rd rs imm) (ppc.d-form as 28 rs rd (ppc.simm16 imm)))
(define (ppc.ori as rd rs imm)   (ppc.d-form as 24 rs rd (ppc.simm16 imm)))

(define (mfspr as rd n) ...)
(define (mtspr as n rs) (ppc.xfx-form as 31 rs ... complicated ... 467))

(define (ppc.bc as cond bit offset) ...)
(define (ppc.bclr as cond bit) ...)
(define (ppc.bl as offset) ...)
(define (ppc.b as offset) ...)
(define (ppc.mcrxr as cr) ...)

some compare opcodes, not yet determined

(define (ppc.lwz as rd base offset) ...)
(define (ppc.lwzx as rd base index) ...)

more load/store codes, TBD

(define (ppc.simm16 imm)
  (if (<= -32768 imm 32767)
      ...
      (error "...")))

(define (ppc.d-form as op r1 r2 imm)
  (ppc.emit as (+ (* op 4) (quotient r1 8)) (+ (* (remainder r1 8) 32) r2) (quotient imm 256) (remainder imm 256)))

(define (ppc.x-form as ...)
  ...)

(define (ppc.xo-form as ...)
  ...)

;; Useful shorthands

(define (ppc.sub as rd rs1 rs2)  (ppc.subf as rd rs2 rs1))
(define (ppc.subo as rd rs1 rs2) (ppc.subfo as rd rs2 rs1))
(define (ppc.mr as rd rs)        (ppc.or as rd rs rs))
(define (ppc.mflr as rd)         (ppc.mfspr as rd 8))
(define (ppc.mtlr as rs)         (ppc.mtspr as 8 rs))

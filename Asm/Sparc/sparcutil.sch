; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC assembler machine parameters & utility procedures.
;
; 29 April 1999 / wdc

; Round up to nearest 8.

(define (roundup8 n)
  (* (quotient (+ n 7) 8) 8))

; Given an integer code for a register, return its register label.
; This register label is the register number for a h.w. register and the
; offsets from GLOBALS[ r0 ] for a s.w. register.

(define regname
  (let ((v (vector $r.reg0  $r.reg1  $r.reg2  $r.reg3  $r.reg4  $r.reg5
                   $r.reg6  $r.reg7  $r.reg8  $r.reg9  $r.reg10 $r.reg11
                   $r.reg12 $r.reg13 $r.reg14 $r.reg15 $r.reg16 $r.reg17
                   $r.reg18 $r.reg19 $r.reg20 $r.reg21 $r.reg22 $r.reg23
                   $r.reg24 $r.reg25 $r.reg26 $r.reg27 $r.reg28 $r.reg29
                   $r.reg30 $r.reg31)))
    (lambda (r)
      (vector-ref v r))))

; Is a general-purpose register mapped to a hardware register?
; This is fragile! FIXME.

(define (hardware-mapped? r)
  (or (and (>= r $r.reg0) (<= r $r.reg7))
      (= r $r.argreg2)
      (= r $r.argreg3)
      (= r $r.result)
      (= r $r.g0)
      (= r $r.tmp0)
      (= r $r.tmp1)
      (= r $r.tmp2)))

; Used by peephole optimizer

(define (hwreg? x)
  (<= 0 x 7))

(define (immediate-int? x)
  (and (exact? x)
       (integer? x)
       (<= -1024 x 1023)))

; Given an exact integer, can it be represented as a fixnum?

(define fixnum-range?
  (let ((-two^29  (- (expt 2 29)))
        (two^29-1 (- (expt 2 29) 1)))
    (lambda (x)
      (<= -two^29 x two^29-1))))

; Does the integer x fit in the immediate field of an instruction?

(define (immediate-literal? x)
  (<= -4096 x 4095))

; Return the offset in the %GLOBALS table of the given memory-mapped 
; register. A memory-mapped register is represented by an integer which 
; is its offet, so just return the value.

(define (swreg-global-offset r) r)

; Return a bit representation of a character constant.

(define (char->immediate c)
  (+ (* (char->integer c) 65536) $imm.character))

; Convert an integer to a fixnum.

(define (thefixnum x) (* x 4))

; The offset of data slot 'n' within a procedure structure, not adjusting 
; for tag. The proc is a header followed by code, const, and then data.

(define (procedure-slot-offset n)
  (+ 12 (* n 4)))

; Src is a register, hwreg is a hardware register. If src is a
; hardware register, return src. Otherwise, emit an instruction to load
; src into hwreg and return hwreg.

(define (force-hwreg! as src hwreg)
  (if (hardware-mapped? src)
      src
      (emit-load-reg! as src hwreg)))

; Given an arbitrary constant opd, generate code to load it into a
; register r.

(define (emit-constant->register as opd r)
  (cond ((and (integer? opd) (exact? opd))
         (if (fixnum-range? opd)	
             (emit-immediate->register! as (thefixnum opd) r)
             (emit-const->register! as (emit-datum as opd) r)))
        ((boolean? opd)
         (emit-immediate->register! as
                                    (if (eq? opd #t)
                                        $imm.true
                                        $imm.false)
                                    r))
        ((equal? opd (eof-object))
         (emit-immediate->register! as $imm.eof r))
        ((equal? opd (unspecified))
         (emit-immediate->register! as $imm.unspecified r))
        ((equal? opd (undefined))
         (emit-immediate->register! as $imm.undefined r))
        ((null? opd)
         (emit-immediate->register! as $imm.null r))
        ((char? opd)
         (emit-immediate->register! as (char->immediate opd) r))
        (else
         (emit-const->register! as (emit-datum as opd) r))))


; Stuff a bitpattern or symbolic expression into a register.
; (CONST, for immediate constants.)
;
; FIXME(?): if this had access to eval-expr (currently hidden inside the
; sparc assembler) it could attempt to evaluate symbolic expressions,
; thereby selecting better code sequences when possible.

(define (emit-immediate->register! as i r)
  (let ((dest (if (not (hardware-mapped? r)) $r.tmp0 r)))
    (cond ((and (number? i) (immediate-literal? i))
           (sparc.set as i dest))
          ((and (number? i) (zero? (remainder (abs i) 1024)))
           (sparc.sethi as `(hi ,i) dest))
          (else
           (sparc.sethi as `(hi ,i) dest)
           (sparc.ori as dest `(lo ,i) dest)))
    (if (not (hardware-mapped? r))
        (emit-store-reg! as r dest))))


; Reference the constants vector and put the constant reference in a register.
; `offset' is an integer offset into the constants vector (a constant) for
; the current procedure.
; Destroys $r.tmp0 and $r.tmp1, but either can be the destination register.
; (CONST, for structured constants, GLOBAL, SETGLBL, LAMBDA).

(define (emit-const->register! as offset r)
  (let ((cvlabel (+ 4 (- (* offset 4) $tag.vector-tag))))
    (cond ((hardware-mapped? r)
           (sparc.ldi as $r.reg0 $p.constvector $r.tmp0)
           (if (asm:fits? cvlabel 13)
               (sparc.ldi as $r.tmp0 cvlabel r)
               (begin (sparc.sethi as `(hi ,cvlabel) $r.tmp1)
                      (sparc.addr  as $r.tmp0 $r.tmp1 $r.tmp0)
                      (sparc.ldi   as $r.tmp0 `(lo ,cvlabel) r))))
          (else
           (emit-const->register! as offset $r.tmp0)
           (emit-store-reg! as $r.tmp0 r)))))



; Emit single instruction to load sw-mapped reg into another reg, and return
; the destination reg.

(define (emit-load-reg! as from to)
  (if (or (hardware-mapped? from) (not (hardware-mapped? to)))
      (asm-error "emit-load-reg: " from to)
      (begin (sparc.ldi as $r.globals (swreg-global-offset from) to)
             to)))

(define (emit-store-reg! as from to)
  (if (or (not (hardware-mapped? from)) (hardware-mapped? to))
      (asm-error "emit-store-reg: " from to)
      (begin (sparc.sti as from (swreg-global-offset to) $r.globals)
             to)))

; Generic move-reg-to-HW-reg

(define (emit-move2hwreg! as from to)
  (if (hardware-mapped? from)
      (sparc.move as from to)
      (emit-load-reg! as from to))
  to)

; Evaluation of condition code for value or control.
;
; branchf.a is an annulled conditional branch that tests the condition codes
;     and branches if some condition is false.
; rd is #f or a hardware register.
; target is #f or a label.
; Exactly one of rd and target must be #f.
;
; (Why isn't this split into two separate procedures?  Because dozens of
; this procedure's callers have the value/control duality, and it saves
; space to put the test here instead of putting it in each caller.)

(define (emit-evaluate-cc! as branchf.a rd target)
  (if target
      (begin (branchf.a   as target)
             (sparc.slot  as))
      (let ((target (new-label)))
        (branchf.a   as target)
        (sparc.set   as $imm.false rd)
        (sparc.set   as $imm.true rd)
        (sparc.label as target))))

; Code for runtime safety checking.

(define (emit-check! as rs0 L1)
  (sparc.cmpi as rs0 $imm.false)
  (emit-checkcc! as sparc.be L1))

; FIXME:  This should call the exception handler for non-continuable exceptions.

(define (emit-trap! as rs1 rs2 rs3 exn)
  (if (not (= rs3 $r.reg0))
      (emit-move2hwreg! as rs3 $r.argreg3))
  (if (not (= rs2 $r.reg0))
      (emit-move2hwreg! as rs2 $r.argreg2))
  (if (not (= rs1 $r.reg0))
      (emit-move2hwreg! as rs1 $r.result))
  (millicode-call/numarg-in-reg as $m.exception exn $r.tmp0))

; Given:
;     an annulled conditional branch that branches
;         if the check is ok
;     a non-annulled conditional branch that branches
;         if the check is not ok
;     #f, or a procedure that takes an assembly segment as
;         argument and emits an instruction that goes into
;         the delay slot of either branch
;     three registers whose contents should be passed to the
;         exception handler if the check is not ok
;     the exception code
; Emits code to call the millicode exception routine with
; the given exception code if the condition is false.
;
; FIXME:  The nop can often be replaced by the instruction that
; follows it.

(begin
 '
(define (emit-checkcc-and-fill-slot!
         as branch-ok.a branch-bad slot-filler L1)
  (let* ((situation (list exn rs1 rs2 rs3))
         (L1 (exception-label as situation)))
    (if L1
        (begin (branch-bad as L1)
               (if slot-filler
                   (slot-filler as)
                   (sparc.nop as)))
        (let* ((L1 (new-label))
               (L2 (new-label)))
          (exception-label-set! as situation L1)
          (branch-ok.a as L2)
          (if slot-filler
              (slot-filler as)
              (sparc.slot  as))
          (sparc.label as L1)
          (cond ((= rs3 $r.reg0)
                 #f)
                ((hardware-mapped? $r.argreg3)
                 (emit-move2hwreg! as rs3 $r.argreg3))
                ((hardware-mapped? rs3)
                 (emit-store-reg! as rs3 $r.argreg3))
                (else
                 (emit-move2hwreg! as rs3 $r.tmp0)
                 (emit-store-reg! as $r.tmp0 $r.argreg3)))
          (if (not (= rs2 $r.reg0))
              (emit-move2hwreg! as rs2 $r.argreg2))
          (if (not (= rs1 $r.reg0))
              (emit-move2hwreg! as rs1 $r.result))
          ; FIXME:  This should be a non-continuable exception.
          (sparc.jmpli as $r.millicode $m.exception $r.o7)
          (emit-immediate->register! as (thefixnum exn) $r.tmp0)
          (sparc.label as L2)))))
#f
)

(define (emit-checkcc! as branch-bad L1)
  (branch-bad as L1)
  (sparc.nop  as))

; Generation of millicode calls for non-continuable exceptions.

(begin
 '
; To create only one millicode call per code segment per non-continuable
; exception situation, we use the "as-user" feature of assembly segments.
; Could use a hash table here.

(define (exception-label as situation)
  (let ((user-data (as-user as)))
    (if user-data
        (let ((exception-labels (assq 'exception-labels user-data)))
          (if exception-labels
              (let ((probe (assoc situation (cdr exception-labels))))
                (if probe
                    (cdr probe)
                    #f))
              #f))
        #f)))
'
(define (exception-label-set! as situation label)
  (let ((user-data (as-user as)))
    (if user-data
        (let ((exception-labels (assq 'exception-labels user-data)))
          (if exception-labels
              (let ((probe (assoc situation (cdr exception-labels))))
                (if probe
                    (error "COMPILER BUG: Exception situation defined twice")
                    (set-cdr! exception-labels
                              (cons (cons situation label)
                                    (cdr exception-labels)))))
              (begin (as-user! as
                               (cons (list 'exception-labels)
                                     user-data))
                     (exception-label-set! as situation label))))
        (begin (as-user! as '())
               (exception-label-set! as situation label)))))
#f
)

; Millicode calling

(define (millicode-call/0arg as mproc)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (sparc.nop   as))

(define (millicode-call/1arg as mproc r)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (emit-move2hwreg! as r $r.argreg2))

(define (millicode-call/1arg-in-result as mproc r)
  (millicode-call/1arg-in-reg as mproc r $r.result))

(define (millicode-call/1arg-in-reg as mproc rs rd)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (emit-move2hwreg! as rs rd))

(define (millicode-call/numarg-in-result as mproc num)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (sparc.set   as num $r.result))

(define (millicode-call/numarg-in-reg as mproc num reg)
  (if (not (hardware-mapped? reg))
      (asm-error "millicode-call/numarg-in-reg requires HW register: " reg))
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (sparc.set   as num reg))

(define (millicode-call/2arg as mproc r1 r2)
  (emit-move2hwreg! as r1 $r.argreg2)
  (sparc.jmpli      as $r.millicode mproc $r.o7)
  (emit-move2hwreg! as r2 $r.argreg3))

; NOTE: Don't use TMP0 since TMP0 is sometimes a millicode argument
; register (for example to m_exception).
;
; NOTE: Don't use sparc.set rather than sethi/ori; we need to know that
; two instructions get generated.
;
; FIXME: Should calculate the value if possible to get better precision
; and to avoid generating a fixup.  See emit-return-address! in gen-msi.sch.

(define (millicode-call/ret as mproc label)
  (cond ((short-effective-addresses)
         (sparc.jmpli as $r.millicode mproc $r.o7)
         (sparc.addi  as $r.o7 `(- ,label (- ,(here as) 4) 8) $r.o7))
        (else
         (let ((val `(- ,label (+ ,(here as) 8) 8)))
           (sparc.sethi as `(hi ,val) $r.tmp1)
           (sparc.ori   as $r.tmp1 `(lo ,val) $r.tmp1)
           (sparc.jmpli as $r.millicode mproc $r.o7)
           (sparc.addr  as $r.o7 $r.tmp1 $r.o7)))))

(define (check-timer as DESTINATION RETRY)
  (sparc.subicc as $r.timer 1 $r.timer)
  (sparc.bne.a  as DESTINATION)
  (sparc.slot   as)
  (millicode-call/ret as $m.timer-exception RETRY))

; eof

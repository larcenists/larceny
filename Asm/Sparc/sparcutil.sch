; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC assembler machine parameters & utility procedures.

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
; `cvlabel' is an integer offset into the constants vector (a constant) for
; the current procedure.
; (CONST, for structured constants.)

(define (emit-const->register! as cvlabel r)
  (let ((cvlabel (+ 4 (- (* cvlabel 4) $tag.vector-tag))))
    (sparc.ldi as $r.reg0 $p.constvector $r.tmp0)
    (if (hardware-mapped? r)
	(sparc.ldi as $r.tmp0 cvlabel r)
	(begin (sparc.ldi as $r.tmp0 cvlabel $r.tmp0)
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

; -*- Scheme -*-
;
; Scheme 313 compiler
; Machine-dependent code generation procedures.
;
; $Id: gen-msi.sch,v 1.1 92/01/19 17:40:50 lth Exp Locker: lth $
;
; (used to be part of asm.sparc.scm).


; Implementation-specific data conversion.

(define (char->immediate c)
  (+ (* (char->integer c) 65536) $imm.character))


;----------------------------------------------------------------------------
; Procedures which emit specific sequences of machine instructions.
; These are to be called from the machine-independent code generation
; routines.

; Tag a fixnum and move it into a register.

(define (emit-fixnum->register! as f r)
  (emit-immediate->register! as (* f 4) r))


; Stuff a bitpattern into a register.

(define (emit-immediate->register! as i r)
  (let ((dest (if (not (hardware-mapped? r)) $r.tmp0 r)))
    (cond ((and (<= i 4095) (>= i -4096))
	   (emit! as `(,$i.ori ,$r.g0 ,i ,dest)))
	  ((zero? (remainder (abs i) 512))
	   (emit! as `(,$i.sethi (hi ,i) ,dest)))
	  (else
	   (emit! as `(,$i.sethi (hi ,i) ,dest))
	   (emit! as `(,$i.ori  ,dest (lo ,i) ,dest))))
    (if (not (hardware-mapped? r))
	(emit! as `(,$i.sti ,dest ,(offsetof r) ,$r.globals)))))

; Reference the constants vector and put the constant reference in a register.
; `cvlabel' is an integer offset into the constants vector (a constant) for
; the current procedure.

(define (emit-const->register! as cvlabel r)
  (let ((cvlabel (- (* cvlabel 4) $tag.vector-tag)))
    (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp0))
    (if (hardware-mapped? r)
	(emit! as `(,$i.ldi ,$r.tmp0 ,cvlabel ,r))
	(begin (emit! as `(,$i.ldi ,$r.tmp0 ,cvlabel ,$r.tmp0))
	       (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof r) ,$r.globals))))))

; Store a register in a global. Assumes a value cell is a pair.
; ("setglbl" instruction).
;
; The delay slot could be filled if we didn't call 'emit-const->register'
; here; as it is, it stays a nop.

(define (emit-result-register->global! as offset)
  (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg2))
  (emit-const->register! as offset $r.result)         ; fetches ptr to cell
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcar ,$r.o7))
  (emit! as `(,$i.nop)))

; "global" instruction.
;
; If the global variable emit-undef-check? is true, then code will be
; emitted to check whether the global is not #!unspecified when loaded.
; If it is, an exception will be taken, with the global in question in
; $r.argreg2.

(define (emit-global->register! as offset r)
  (let ((l0 (new-label)))

    (define (emit-undef-check! as r l0)
      (if emit-undef-check?
	  (let ((l1 (new-label)))
	    (emit! as `(,$i.subicc ,r ,$imm.unspecified ,$r.g0))
	    (emit! as `(,$i.bne.a ,l1))
	    (emit! as `(,$i.slot))
	    (emit! as `(,$i.orr ,$r.tmp1 ,$r.g0 ,$r.argreg2))
	    (emit! as `(,$i.jmpli ,$r.millicode ,$m.undef-exception ,$r.o7))
	    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
	    (emit! as `(,$i.label ,l1)))))

    (emit-const->register! as offset $r.tmp1)
    (emit! as `(,$i.label ,l0))
    (if (hardware-mapped? r)
	(begin (emit! as `(,$i.ldi ,$r.tmp1 ,(- $tag.pair-tag) ,r))
	       (emit-undef-check! as r l0))
	(begin (emit! as `(,$i.ldi ,$r.tmp1 ,(- $tag.pair-tag) ,$r.tmp0))
	       (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof r) ,$r.globals))
	       (emit-undef-check! $r.tmp0 l0)))))

; Move one register to another.
; "movereg" instruction.

(define (emit-register->register! as from to)
  (cond ((and (hardware-mapped? from) (hardware-mapped? to))
	 (emit! as `(,$i.orr ,from ,$r.g0 ,to)))
	((hardware-mapped? from)
	 (emit! as `(,$i.sti ,from ,(offsetof to) ,$r.globals)))
	((hardware-mapped? to)
	 (emit! as `(,$i.ldi ,$r.globals ,(offsetof from) ,to)))
	(else
	 (emit! as `(,$i.ldi ,$r.globals ,(offsetof from) ,$r.tmp0))
	 (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof to) ,$r.globals)))))


; Argument check. If the args don't match, drop to the exception handler and
; skip back to the check when the exception handler returns.
	 
(define (emit-args=! as n)
  (let ((l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.subicc ,$r.result ,(* n 4) ,$r.g0))
    (emit! as `(,$i.be.a ,l2))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.arg-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l1 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l2))))


; Variable-length argument list check drops into millicode automatically.

(define (emit-args>=! as n)
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.varargs ,$r.o7))
  (emit! as `(,$i.ori ,$r.g0 ,(* n 4) ,$r.argreg2)))


; Invoke a procedure, with all gyrations. This is way too complex.

(define (emit-invoke! as n)
  (let ((l1 (new-label))
	(l2 (new-label))
	(m  (new-label)))
    (emit! as `(,$i.subicc ,$r.timer 1 ,$r.timer))
    (emit! as `(,$i.bne.a ,l2))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.timer-exception ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
    (emit! as `(,$i.subicc ,$r.tmp0 ,$tag.procedure-tag ,$r.g0))
    (emit! as `(,$i.be.a ,m))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.proc-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,m))
;    (emit! as `(,$i.orr ,$r.reg0 ,$r.g0 ,$r.argreg2))
    (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.reg0))
    (emit! as `(,$i.ldi ,$r.reg0 ,$p.codevector ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 ,$p.codeoffset ,$r.g0))
    (emit! as `(,$i.ori ,$r.g0 ,(* n 4) ,$r.result))))

; Create stack frame, then save
;
; Really want to use std/ldd here. This is easy for hardware-mapped registers
; (although the boundaries are messy), less so for memory-mapped regs.
; Also, we require some "nice" alignment of the registers in memory.
;
; The adjustment of 'n' here is due to a compiler anomaly; sometimes
; the compiler gets excited and saves more registers than there really are.
; In this case, we pad the frame out to the desired size with 0's, because the
; extra slots are used for registers spills. In reality, there should be an
; instruction to allocate a stack frame for this purpose, since SAVE is not
; what is desired. This may get fixed later.
;
; Notice the heavy calculation of the effective return address; this is so
; because in some pathological cases, the target is too far away to keep
; in an immediate.

(define (emit-save! as label n)
  (let* ((l         (new-label))
	 (framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (* 8 (quotient (+ framesize 7) 8)))
	 (limit     (min n 31)))
    (emit! as `(,$i.ldi ,$r.globals ,$g.stk-limit ,$r.tmp0))
    (emit! as `(,$i.subrcc ,$r.tmp0 ,$r.stkp ,$r.g0))
    (emit! as `(,$i.ble.a ,l))
    (emit! as `(,$i.subi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.stkoflow ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.subi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.label ,l))
; old (wrong) code
;    (emit! as `(,$i.call (+ $ 8)))
;    (emit! as `(,$i.addi ,$r.o7 (- ,(make-asm-label label) (- $ 4) 8) ,$r.o7))
; new (correct) code
    (emit-effective-address! as `(- ,(make-asm-label label) 8))
;
    (emit! as `(,$i.sti ,$r.o7 0 ,$r.stkp))
    (emit! as `(,$i.ori ,$r.g0 ,framesize ,$r.tmp0))
    (emit! as `(,$i.sti ,$r.tmp0 4 ,$r.stkp))
    (let loop ((i 0) (offset 8))
      (cond ((<= i limit)
	     (let ((r (regname i)))
	       (if (hardware-mapped? r)
		   (emit! as `(,$i.sti ,r ,offset ,$r.stkp))
		   (begin
		     (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp0))
		     (emit! as `(,$i.sti ,$r.tmp0 ,offset ,$r.stkp))))
	       (loop (+ i 1) (+ offset 4))))
	    ((<= i n)
	     (emit! as `(,$i.sti ,$r.reg0 ,offset ,$r.stkp))
	     (loop (+ i 1) (+ offset 4)))
	    (else
	     #t)))))

(define (emit-effective-address! as expr)
  (emit! as `(,$i.sethi (hi (- ,expr (+ $ 8))) ,$r.tmp0))
  (emit! as `(,$i.ori ,$r.tmp0 (lo (- ,expr (+ $ 4))) ,$r.tmp0))
  (emit! as `(,$i.call (+ $ 8)))
  (emit! as `(,$i.addr ,$r.o7 ,$r.tmp0 ,$r.o7)))

; Restore registers from stack frame
;
; Use ldd/std here; see comments for emit-save!, above.
; We pop only actual registers.

(define (emit-restore! as n)
  (let ((n (min n 31)))
    (let loop ((i 0) (offset 8))
      (if (<= i n)
	  (let ((r (regname i)))
	    (if (hardware-mapped? r)
		(emit! as `(,$i.ldi ,$r.stkp ,offset ,r))
		(begin (emit! as `(,$i.ldi ,$r.stkp ,offset ,$r.tmp0))	       
		       (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof r) ,$r.globals))))
	    (loop (+ i 1) (+ offset 4)))))))

; Pop frame. Checking for underflow is pretty cheap.

(define (emit-pop! as n)
  (let* ((framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (* 8 (quotient (+ framesize 7) 8)))
	 (l1        (new-label)))
    (emit! as `(,$i.ldi ,$r.globals ,$g.stk-start ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.subrcc ,$r.stkp, $r.tmp0, $r.g0))
    (emit! as `(,$i.ble.a ,l1))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.restore-frame ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l1))))

; Change the return address in the stack frame.

(define (emit-setrtn! as label)
  (emit-effective-address! as `(- ,(make-asm-label label) 8))
  (emit! as `(,$i.sti ,$r.o7 0 ,$r.stkp)))

; `apply' falls into millicode

(define (emit-apply! as)
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.apply ,$r.o7))
  (emit! as `(,$i.nop)))

; a nop is a nop is a nop.

(define (emit-nop! as)
  (emit! as `(,$i.nop)))


(define (emit-load! as n k)
  (if (hardware-mapped? k)
      (emit! as `(,$i.ldi ,$r.stkp ,(+ 8 (* n 4)) ,k))
      (begin (emit! as `(,$i.ldi ,$r.stkp ,(+ 8 (* n 4)) ,$r.tmp0))
	     (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof k) ,$r.globals)))))


(define (emit-store! as k n)
  (if (hardware-mapped? k)
      (emit! as `(,$i.sti ,k ,(+ 8 (* n 4)) ,$r.stkp))
      (begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof k) ,$r.tmp0))
	     (emit! as `(,$i.sti ,$r.tmp0 ,(+ 8 (* n 4)) ,$r.stkp)))))


(define (emit-lexical! as m n)
  (let ((base (emit-follow-chain! as m)))
    (emit! as `(,$i.ldi ,base 
			,(- (slotoffset n) $tag.procedure-tag) 
			,$r.result))))

; This use of vector-set is well-defined!

(define (emit-setlex! as m n)
  (let ((base (emit-follow-chain! as m)))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.vector-set ,$r.o7))
    (emit! as `(,$i.orr ,$r.g0 ,(slotoffset n) ,$r.argreg2))))


; Follow static links.

(define (emit-follow-chain! as m)
  (let loop ((q m))
    (if (not (zero? q))
	(begin (if (= q m)
		   (emit! as `(,$i.ldi ,$r.reg0 ,$p.linkoffset ,$r.tmp0))
		   (emit! as `(,$i.ldi ,$r.tmp0 ,$p.linkoffset ,$r.tmp0)))
	       (loop (- q 1)))
	(if (zero? m)
	    $r.reg0
	    $r.tmp0))))

; And many hippo returns...
;
; Original code for emit-return.
;
; (define (emit-return! as)
;   (emit! as `(,$i.ldi ,$r.stkp 0 ,$r.tmp0))
;   (emit! as `(,$i.jmpli ,$r.tmp0 8 ,$r.g0))
;   (emit! as `(,$i.nop)))
;
; This code peeps the previous instruction into the slot, if possible.
; The condition is that the previous instruction is not a branch and that
; it is not in the delay slot of some branch. It also has to be an 
; instruction -- labels will not do.
;
; There is an additional problem, in that the return instruction needs
; a temporary register. In order to not mess with the temps, we use %o7.
; If we were to use the temps, some rather more sophisticated analysis would
; be necessary.

(define (emit-return! as)
  (let ((p0 (previous-emitted-instruction 0 as))
	(p1 (previous-emitted-instruction 1 as)))
    (let ((slot (if (and (not (or (branch-instr? p0)
				  (weird-instr? p0)))
			 (not (branch-instr? p1)))
		    (begin (discard-previous-instruction! as)
			   p0)
		    `(,$i.nop))))
      (emit! as `(,$i.ldi ,$r.stkp 0 ,$r.o7))
      (emit! as `(,$i.jmpli ,$r.o7 8 ,$r.g0))
      (emit! as slot))))

; Multiple values are neat. But not neat enough to sweat them at this point.

(define (emit-mvrtn! as)
  (error 'emit-mvrtn! "multiple-value return has not been implemented (yet)."))

; Currently ignores `doc'; unclear what to do about it.

(define (emit-lexes! as n doc)
  (emit-alloc-proc! as n)
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.codevector ,$r.tmp0))
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp0 ,$p.codevector ,$r.result))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.constvector ,$r.result))
  (emit-init-proc-slots! as n))

; Ignores `doc', too.
; Assumes that `x' is the unadjusted offset into the current constant vector 
; of the code vector and that `y' is the offset of the constant vector for
; the new procedure.

(define (emit-lambda! as x y n doc)
  (let ((x (- (* 4 x) $tag.vector-tag))
	(y (- (* 4 y) $tag.vector-tag)))
  (emit-alloc-proc! as n)
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp0))
  (emit! as `(,$i.ldi ,$r.tmp0 ,x ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.codevector ,$r.result))
  (emit! as `(,$i.ldi ,$r.tmp0 ,y ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.constvector ,$r.result))
  (emit-init-proc-slots! as n)))
 
; allocate procedure with room for n register slots; return tagged pointer.

(define two^12 (expt 2 12))

(define (emit-alloc-proc! as n)
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
  (emit! as `(,$i.ori ,$r.g0 ,(* (+ n 4) 4) ,$r.result))
  (let ((header (+ (* (* (+ n 3) 4) 256) $imm.procedure-header)))
    (if (>= header two^12)
	(begin (emit! as `(,$i.sethi (hi ,header) ,$r.tmp0))
	       (emit! as `(,$i.ori ,$r.tmp0 (lo ,header) ,$r.tmp0)))
	(emit! as `(,$i.ori ,$r.g0 ,header ,$r.tmp0)))
    (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.result))
    (emit! as `(,$i.addi ,$r.result ,$tag.procedure-tag ,$r.result))))

; Initialize data slots in procedure from current registers as specified for
; `lamba' and `lexes'. If there are more data slots than registers, then
; we must generate code to cdr down the list in the last register to obtain
; the rest of the data. The list is expected to have at least the minimal
; length.
;
; The untagged pointer to the procedure is in $r.result.

(define (emit-init-proc-slots! as n)
  (let ((limit (min (- maxregs 1) n)))
    (let loop ((i 0) (offset $p.reg0))
      (if (<= i limit)
	  (let ((r (regname i)))
	    (if (hardware-mapped? r)
		(emit! as `(,$i.sti ,r ,offset ,$r.result))
		(begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp0))
		       (emit! as `(,$i.sti ,$r.tmp0 ,offset ,$r.result))))
	    (loop (+ i 1) (+ offset 4)))
	  (if (<= i n)
	      (begin
		(emit! as `(,$i.ldi ,$r.globals ,(offsetof $r.reg31) ,$r.tmp0))
		(let loop ((i i) (offset offset))
		  (emit! as `(,$i.ldi ,$r.tmp0 ,(- $tag.pair-tag) ,$r.tmp1))
		  (emit! as `(,$i.sti ,$r.tmp1 ,offset ,$r.result))
		  (if (< i n)
		      (begin 
			(emit! as `(,$i.ldi ,$r.tmp0 
					    ,(+ (- $tag.pair-tag) 4)
					    ,$r.tmp0))
			(loop (+ i 1) (+ offset 4)))))))))))

(define (emit-branch! as check-timer? label)
  (let ((label (make-asm-label label)))
    (if check-timer?
	(begin (emit! as `(,$i.subicc ,$r.timer 1 ,$r.timer))
	       (emit! as `(,$i.bne.a ,label))
	       (emit! as `(,$i.slot))
	       (emit! as `(,$i.jmpli ,$r.millicode ,$m.timer-exception ,$r.o7))
	       (emit! as `(,$i.addi ,$r.o7 (- ,label (- $ 4) 8) ,$r.o7)))
	(begin (emit! as `(,$i.b ,label))
	       (emit! as `(,$i.nop))))))

(define (emit-branchf! as label)
  (let ((label (make-asm-label label)))
    (emit! as `(,$i.subicc ,$r.result ,$imm.false ,$r.g0))
    (emit! as `(,$i.be.a ,label))
    (emit! as `(,$i.slot))))

; Some jumps are really long, so the simple offset in the jmp instruction is
; not enough, and an address must be calculated by hand. It would be sort of
; nice to be able to use the shorter sequence when possible, but right now
; only the hard way is used.

(define (emit-jump! as m label)
  (let* ((r     (emit-follow-chain! as m))
	 (label (make-asm-label label))
	 (tmp   (if (= r $r.tmp0) $r.tmp1 $r.tmp0)))
    (emit! as `(,$i.ldi ,r ,$p.codevector ,tmp))
;new
    (emit! as `(,$i.sethi (hi (+ ,$p.codeoffset ,label)) ,$r.tmp2))
    (emit! as `(,$i.addi ,$r.tmp2 (lo (+ ,$p.codeoffset ,label)) ,$r.tmp2))
    (emit! as `(,$i.jmplr ,tmp ,$r.tmp2 ,$r.g0))
;old
;    (emit! as `(,$i.jmpli ,tmp (+ ,label ,$p.codeoffset) ,$r.g0))
    (emit! as `(,$i.orr ,r ,$r.g0 ,$r.reg0))))

(define (emit-label! as l)
  (emit! as `(,$i.label ,l)))

; no-op on sparc

(define (emit-.proc! as)
  '())

; Dropps into millicode; way too heavyweight to have in line.

(define (emit-apply! as)
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.apply ,$r.g0))
  (emit! as `(,$i.nop)))

; EOF

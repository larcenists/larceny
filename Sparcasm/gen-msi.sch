;; -*- Scheme -*-
;;
;; Larceny assembler (Sparc) -- code emitters for MacScheme instructions.
;;
;; History
;;   July 17, 1995 / lth (v0.24)
;;     INVOKE and ARGS= are sensitive to unsafe-mode.
;;
;;   July 5, 1994 / lth (v0.20)
;;     Slightly changed to deal with new millicode procedures and new
;;     stack frame layouts.

; Emit single instruction to load sw-mapped reg into another reg, and return
; the destination reg.

(define (emit-load-reg! as from to)
  (if (or (hardware-mapped? from)
	  (not (hardware-mapped? to)))
      (error '() "Error in emit-load-reg:" from to)
      (begin
	(emit! as `(,$i.ldi ,$r.globals ,(offsetof from) ,to))
	to)))

(define (emit-store-reg! as from to)
  (if (or (not (hardware-mapped? from))
	  (hardware-mapped? to))
      (error '() "Error in emit-store-reg:" from to)
      (begin
	(emit! as `(,$i.sti ,from ,(offsetof to) ,$r.globals))
	to)))

; Generic move-reg-to-HW-reg

(define (emit-move2hwreg! as from to)
  (if (hardware-mapped? from)
      (emit! as `(,$i.orr ,$r.g0 ,from ,to))
      (emit-load-reg! as from to))
  to)

; Return a bit representation of a character constant.

(define (char->immediate c)
  (+ (* (char->integer c) 65536) $imm.character))

; Convert an integer to a fixnum.

(define (thefixnum x) (* x 4))

; The offset of data slot 'n' within a procedure structure, not adjusting 
; for tag. The proc is a header followed by code, const, and then data.

(define (slotoffset n)
  (+ 12 (* n 4)))

; Procedures which emit specific sequences of machine instructions.
; These are to be called from the machine-independent code generation
; routines.

; Tag a fixnum and move it into a register.

(define (emit-fixnum->register! as f r)
  (emit-immediate->register! as (* f 4) r))


; These appear to no longer be in use.
;
; (define (immediate-int? x) (<= -1024 x 1023))
; (define (hwreg? x) (<= 0 x 7))


; CONST, for immediate constants
;
; Stuff a bitpattern into a register.

(define (emit-immediate->register! as i r)
  (let ((dest (if (not (hardware-mapped? r)) $r.tmp0 r)))
    (cond ((<= -4096 i 4095)
	   (emit! as `(,$i.ori ,$r.g0 ,i ,dest)))
	  ((zero? (remainder (abs i) 512))
	   (emit! as `(,$i.sethi (hi ,i) ,dest)))
	  (else
	   (emit! as `(,$i.sethi (hi ,i) ,dest))
	   (emit! as `(,$i.ori  ,dest (lo ,i) ,dest))))
    (if (not (hardware-mapped? r))
	(emit-store-reg! as r dest))))


; CONST, for structured constants.
;
; Reference the constants vector and put the constant reference in a register.
; `cvlabel' is an integer offset into the constants vector (a constant) for
; the current procedure.

(define (emit-const->register! as cvlabel r)
  (let ((cvlabel (- (* cvlabel 4) $tag.vector-tag)))
    (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp0))
    (if (hardware-mapped? r)
	(emit! as `(,$i.ldi ,$r.tmp0 ,cvlabel ,r))
	(begin (emit! as `(,$i.ldi ,$r.tmp0 ,cvlabel ,$r.tmp0))
	       (emit-store-reg! as $r.tmp0 r)))))


; .SINGLESTEP
;
; Single step: jump to millicode; pass index of documentation string in
; %TMP0. Some instructions execute when reg0 is not a valid pointer to
; the current procedure (because this is just after returning); in this
; case we restore reg0 from the stack location given by 'funkyloc'.

(define (emit-singlestep-instr! as funky? funkyloc cvlabel)
  (if funky?
      (emit! as `(,$i.ldi ,$r.stkp ,(+ (* 4 funkyloc) 12) ,$r.reg0)))
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.singlestep ,$r.o7))
  (emit! as `(,$i.ori ,$r.g0 ,(* cvlabel 4) ,$r.argreg2)))


; SETGLBL
;
; Store the RESULT register in a global. Assumes a value cell is a pair.
; The delay slot could be filled if we didn't call 'emit-const->register'
; here; as it is, it stays a nop.

(define (emit-result-register->global! as offset)
  (emit-const->register! as offset $r.tmp0)
  (emit! as `(,$i.sti ,$r.result ,(- $tag.pair-tag) ,$r.tmp0))
  (if register-transactions-for-side-effects
      (begin
	(emit! as `(,$i.orr ,$r.g0 ,$r.result ,$r.argreg2))
	(emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
	(emit! as `(,$i.orr ,$r.g0 ,$r.tmp0 ,$r.result)))))


; GLOBAL
;
; If the global variable emit-undef-check? is true, then code will be
; emitted to check whether the global is not #!unspecified when loaded.
; If it is, an exception will be taken, with the global in question in
; $r.result.

(define (emit-global->register! as offset r)

  (define (emit-undef-check! as r)
    (if emit-undef-check?
	(let ((l1 (new-label)))
	  (emit! as `(,$i.subicc ,r ,$imm.unspecified ,$r.g0))
	  (emit! as `(,$i.bne.a ,l1))
	  (emit! as `(,$i.slot))
	  (emit! as `(,$i.orr ,$r.tmp1 ,$r.g0 ,$r.result))
	  (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
	  (emit! as `(,$i.ori ,$r.g0 ,(thefixnum $ex.undef-global) ,$r.tmp0))
	  (emit! as `(,$i.label ,l1)))))

  (emit-const->register! as offset $r.tmp1)
  (if (hardware-mapped? r)
      (begin (emit! as `(,$i.ldi ,$r.tmp1 ,(- $tag.pair-tag) ,r))
	     (emit-undef-check! as r))
      (begin (emit! as `(,$i.ldi ,$r.tmp1 ,(- $tag.pair-tag) ,$r.tmp0))
	     (emit-store-reg! as $r.tmp0 r)
	     (emit-undef-check! as $r.tmp0))))


; MOVEREG
;
; Move one register to another.

(define (emit-register->register! as from to)
  (if (not (= from to))
      (cond ((and (hardware-mapped? from) (hardware-mapped? to))
	     (emit! as `(,$i.orr ,from ,$r.g0 ,to)))
	    ((hardware-mapped? from)
	     (emit-store-reg! as from to))
	    ((hardware-mapped? to)
	     (emit-load-reg! as from to))
	    (else
	     (emit-load-reg! as from $r.tmp0)
	     (emit-store-reg! as $r.tmp0 to)))))


; ARGS=
;
; Argument check. If the args don't match, drop to the exception handler.
; FIXME: better use of the delay slot possible.

(define (emit-args=! as n)
  (if (not unsafe-mode)
      (let ((l2 (new-label)))
	(emit! as `(,$i.subicc ,$r.result ,(* n 4) ,$r.g0))
	(emit! as `(,$i.be ,l2))
	(emit! as `(,$i.ori ,$r.g0 ,(* n 4) ,$r.argreg2))
	(emit! as `(,$i.orr ,$r.reg0 ,$r.g0 ,$r.argreg3))
	(emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
	(emit! as `(,$i.ori ,$r.g0 ,(thefixnum $ex.argc) ,$r.tmp0))
	(emit! as `(,$i.label ,l2)))))

; ARGS>=
;
; Variable-length argument list check does the check and then drops into
; millicode to gather arguments into a list.
;
; We can remove the checking bit here; the millicode does it too.

(define (emit-args>=! as n)
  (let ((l2 (new-label)))
    (emit! as `(,$i.subicc ,$r.result ,(* n 4) ,$r.g0))
    (emit! as `(,$i.bge ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,(* n 4) ,$r.argreg2))
    (emit! as `(,$i.orr ,$r.reg0 ,$r.g0 ,$r.argreg3))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum $ex.vargc) ,$r.tmp0))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.varargs ,$r.o7))
    (emit! as `(,$i.nop))))


; INVOKE
;
; Invoke a procedure, with all gyrations. The exception handling here has
; a bit of magic: the return address generated in %o7 is the return address
; which is in the topmost frame on the stack. (Can't be right. FIXME.)
;
; Note: must setup argument count even in unsafe mode, because we may be
; calling code which was not compiled safe.
;
; This code takes 10 cycles on a call if the load hits the cache (SS1).

(define (emit-invoke! as n)
  (let ((l0 (new-label))
	(l2 (new-label))
	(l3 (new-label)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.subicc ,$r.timer 1 ,$r.timer))
    (emit! as `(,$i.bne.a ,l2))
    (if (not unsafe-mode)
	(emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
	(emit! as `(,$i.ldi ,$r.result ,$p.codevector ,$r.tmp0)))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.timer-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l2))
    (if (not unsafe-mode)
	(begin ; tmp0 has tag of RESULT
	       (emit! as `(,$i.subicc ,$r.tmp0 ,$tag.procedure-tag ,$r.g0))
	       (emit! as `(,$i.be.a ,l3))
	       (emit! as `(,$i.ldi ,$r.result ,$p.codevector ,$r.tmp0))
	       (emit! as `(,$i.ori ,$r.g0 ,(thefixnum $ex.nonproc) ,$r.tmp0))
	       (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.g0))
	       (emit! as `(,$i.ldi ,$r.stkp 4 ,$r.o7))))
    ; tmp0 has code vector
    ; result has procedure
    (if (not unsafe-mode)
	(emit! as `(,$i.label ,l3)))
    (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.reg0))
    (emit! as `(,$i.jmpli ,$r.tmp0 ,$p.codeoffset ,$r.g0))
    (emit! as `(,$i.ori ,$r.g0 ,(* n 4) ,$r.result))))


; SAVE -- for old compiler
;
; Create stack frame, then save registers.
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

(define (emit-save! as . args)
  (if (= (length args) 2)
      (emit-save2! as 'saveregs (cadr args) (car args))
      (emit-save2! as 'savezero (car args) #f)))

(define (emit-save2! as how n label)
  (let* ((l         (new-label))
	 (l0        (new-label))
	 (framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4)))
	 (limit     (min n 31)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.subi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.subrcc ,$r.stklim ,$r.stkp ,$r.g0))
    (emit! as `(,$i.ble.a ,l))
    (emit! as `(,$i.ori ,$r.g0 ,framesize ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.stkoflow ,$r.o7))
    (emit! as `(,$i.subi ,$r.o7 (+ (- $ ,l0) 4) ,$r.o7))
    (emit! as `(,$i.label ,l))
    ; initialize stack frame
    (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.stkp))
    (if (eq? how 'saveregs)
	(begin (emit-effective-address! as label)
	       (emit! as `(,$i.sti ,$r.o7 4 ,$r.stkp)))
	(emit! as `(,$i.sti ,$r.g0 4 ,$r.stkp)))
    (let loop ((i 0) (offset 12))
      (cond ((and (eq? how 'saveregs) (<= i limit))
	     (let ((r (regname i)))
	       (if (hardware-mapped? r)
		   (emit! as `(,$i.sti ,r ,offset ,$r.stkp))
		   (begin
		     (emit-load-reg! as r $r.tmp0)
		     (emit! as `(,$i.sti ,$r.tmp0 ,offset ,$r.stkp))))
	       (loop (+ i 1) (+ offset 4))))
	    ((<= i n)
	     (emit! as `(,$i.sti ,$r.g0 ,offset ,$r.stkp))
	     (loop (+ i 1) (+ offset 4)))
	    (else
	     #t)))))

; SAVE -- for new compiler
;
; Create stack frame.  To avoid confusing the garbage collector, the
; slots must be initialized to something definite unless they will
; immediately be initialized by a MacScheme machine store instruction.
; The creation is done by emit-save0!, and the initialization is done
; by emit-save1!.

(define (emit-save0! as n)
  (let* ((l         (new-label))
	 (l0        (new-label))
	 (framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.subi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.subrcc ,$r.stklim ,$r.stkp ,$r.g0))
    (emit! as `(,$i.ble.a ,l))
    (emit! as `(,$i.ori ,$r.g0 ,framesize ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.stkoflow ,$r.o7))
    (emit! as `(,$i.subi ,$r.o7 (+ (- $ ,l0) 4) ,$r.o7))
    (emit! as `(,$i.label ,l))
    ; initialize size and return fields of stack frame
    (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.stkp))
    (emit! as `(,$i.sti ,$r.g0 4 ,$r.stkp))))

; Given a vector v of booleans, initializes slot i of the stack frame
; if and only if (vector-ref v i).

(define (emit-save1! as v)
  (let ((n (vector-length v)))
    (let loop ((i 0) (offset 12))
      (cond ((= i n)
             #t)
            ((vector-ref v i)
	     (emit! as `(,$i.sti ,$r.g0 ,offset ,$r.stkp))
	     (loop (+ i 1) (+ offset 4)))
	    (else
	     (loop (+ i 1) (+ offset 4)))))))

(define (emit-effective-address! as label)
  (if assume-short-distance-to-call
      (let ((expr `(- ,(make-asm-label label) (- $ 4) 8)))
	(emit! as `(,$i.call (+ $ 8)))
	(emit! as `(,$i.addi ,$r.o7 ,expr ,$r.o7)))
      (let ((expr `(- ,(make-asm-label label) 8)))
	(emit! as `(,$i.sethi (hi (- ,expr (+ $ 8))) ,$r.tmp0))
	(emit! as `(,$i.ori ,$r.tmp0 (lo (- ,expr (+ $ 4))) ,$r.tmp0))
	(emit! as `(,$i.call (+ $ 8)))
	(emit! as `(,$i.addr ,$r.o7 ,$r.tmp0 ,$r.o7)))))


; RESTORE
;
; Restore registers from stack frame
; Use ldd/std here; see comments for emit-save!, above.
; We pop only actual registers.

(define (emit-restore! as n)
  (let ((n (min n 31)))
    (let loop ((i 0) (offset 12))
      (if (<= i n)
	  (let ((r (regname i)))
	    (if (hardware-mapped? r)
		(emit! as `(,$i.ldi ,$r.stkp ,offset ,r))
		(begin
		  (emit! as `(,$i.ldi ,$r.stkp ,offset ,$r.tmp0))	       
		  (emit-store-reg! as $r.tmp0 r)))
	    (loop (+ i 1) (+ offset 4)))))))

; POP -- for new compiler
;
; Pop frame.
; If returning?, then emit the return as well and put the pop
; in its delay slot.

(define (emit-pop! as n returning?)
  (let* ((framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (if returning?
        (begin (emit! as `(,$i.ldi ,$r.stkp ,(+ realsize 4) ,$r.o7))
               (emit! as `(,$i.jmpli ,$r.o7 8 ,$r.g0))
               (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp)))
        (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp)))))


; POP -- for old compiler
;
; Pop frame. Checking for underflow is pretty cheap, yet too expensive.
; If the switch fast-pop is turned on, a much faster pop is used.
; Using this switch is a guarantee from the programmer that the compiler
; will not generate code which creates spill frames...

(define (old-emit-pop! as n)
  (let* ((framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4)))
	 (l1        (new-label)))
    (if fast-pop
	(begin (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp)))
	(begin (emit! as `(,$i.ldi ,$r.globals ,$g.stkbot ,$r.tmp0))
	       (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp))
	       (emit! as `(,$i.subrcc ,$r.stkp ,$r.tmp0 ,$r.g0))
	       (emit! as `(,$i.bne.a ,l1))
	       (emit! as `(,$i.slot))
	       (emit! as `(,$i.jmpli ,$r.millicode ,$m.stkuflow ,$r.o7))
	       (emit! as `(,$i.nop))
	       (emit! as `(,$i.label ,l1))))))

; SETRTN
;
; Change the return address in the stack frame.

(define (emit-setrtn! as label)
  (emit-effective-address! as label)
  (emit! as `(,$i.sti ,$r.o7 4 ,$r.stkp)))


; APPLY
;
; `apply' falls into millicode.
;
; The timer check is performed here because it is not very easy for the
; millicode to do this.

(define (emit-apply! as r1 r2)
  (let ((l (new-label)))
    (emit! as `(,$i.subicc ,$r.timer 1 ,$r.timer))
    (emit! as `(,$i.bne ,l))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.timer-exception ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l))
    (emit-move2hwreg! as r1 $r.argreg2)
    (emit-move2hwreg! as r2 $r.argreg3)
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.apply ,$r.o7))
    (emit! as `(,$i.nop))))


; NOP
;
; a nop is a nop is a nop.

(define (emit-nop! as)
  (emit! as `(,$i.nop)))


; LOAD

(define (emit-load! as n k)
  (if (hardware-mapped? k)
      (emit! as `(,$i.ldi ,$r.stkp ,(+ 12 (* n 4)) ,k))
      (begin (emit! as `(,$i.ldi ,$r.stkp ,(+ 12 (* n 4)) ,$r.tmp0))
	     (emit-store-reg! as $r.tmp0 k))))


; STORE

(define (emit-store! as k n)
  (if (hardware-mapped? k)
      (emit! as `(,$i.sti ,k ,(+ 12 (* n 4)) ,$r.stkp))
      (begin (emit-load-reg! as k $r.tmp0)
	     (emit! as `(,$i.sti ,$r.tmp0 ,(+ 12 (* n 4)) ,$r.stkp)))))


; LEXICAL

(define (emit-lexical! as m n)
  (let ((base (emit-follow-chain! as m)))
    (emit! as `(,$i.ldi ,base 
			,(- (slotoffset n) $tag.procedure-tag) 
			,$r.result))))


; SETLEX

(define (emit-setlex! as m n)
  (let ((base (emit-follow-chain! as m)))
    (emit! as `(,$i.sti ,$r.result
			,(- (slotoffset n) $tag.procedure-tag)
			,base))
    (if register-transactions-for-side-effects
	(begin
	  (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg2))
	  (emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
	  (emit! as `(,$i.orr ,base ,$r.g0 ,$r.result))))))


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

; RETURN
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
      (emit! as `(,$i.ldi ,$r.stkp 4 ,$r.o7))
      (emit! as `(,$i.jmpli ,$r.o7 8 ,$r.g0))
      (emit! as slot))))


; MVRTN
;
; Multiple values are neat. But not neat enough to sweat them at this point.

(define (emit-mvrtn! as)
  (error 'emit-mvrtn! "multiple-value return has not been implemented (yet)."))


; LEXES

(define (emit-lexes! as n doc)
  (emit-alloc-proc! as n)
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.codevector ,$r.tmp0))
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp0 ,$p.codevector ,$r.result))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.constvector ,$r.result))
  (emit-init-proc-slots! as n))


; LAMBDA
;
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

(define emit-alloc-proc!
  (let ((two^12 (expt 2 12)))
    (lambda (as n)
      (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
      (emit! as `(,$i.ori ,$r.g0 ,(* (+ n 4) 4) ,$r.result))
      (let ((header (+ (* (* (+ n 3) 4) 256) $imm.procedure-header)))
	(if (>= header two^12)
	    (begin (emit! as `(,$i.sethi (hi ,header) ,$r.tmp0))
		   (emit! as `(,$i.ori ,$r.tmp0 (lo ,header) ,$r.tmp0)))
	    (emit! as `(,$i.ori ,$r.g0 ,header ,$r.tmp0)))
	(emit! as `(,$i.sti ,$r.tmp0 0 ,$r.result))
	(emit! as `(,$i.addi ,$r.result ,$tag.procedure-tag ,$r.result))))))

; Initialize data slots in procedure from current registers as specified for
; `lamba' and `lexes'. If there are more data slots than registers, then
; we must generate code to cdr down the list in the last register to obtain
; the rest of the data. The list is expected to have at least the minimal
; length.
;
; The tagged pointer to the procedure is in $r.result.

(define (emit-init-proc-slots! as n)
  (let ((limit (min (- maxregs 1) n)))
    (let loop ((i 0) (offset $p.reg0))
      (if (<= i limit)
	  (let ((r (regname i)))
	    (if (hardware-mapped? r)
		(emit! as `(,$i.sti ,r ,offset ,$r.result))
		(begin (emit-load-reg! as r $r.tmp0)
		       (emit! as `(,$i.sti ,$r.tmp0 ,offset ,$r.result))))
	    (loop (+ i 1) (+ offset 4)))
	  (if (<= i n)
	      (begin
		(emit-load-reg! as $r.reg31 $r.tmp0)
		(let loop ((i i) (offset offset))
		  (emit! as `(,$i.ldi ,$r.tmp0 ,(- $tag.pair-tag) ,$r.tmp1))
		  (emit! as `(,$i.sti ,$r.tmp1 ,offset ,$r.result))
		  (if (< i n)
		      (begin 
			(emit! as `(,$i.ldi ,$r.tmp0 
					    ,(+ (- $tag.pair-tag) 4)
					    ,$r.tmp0))
			(loop (+ i 1) (+ offset 4)))))))))))

; BRANCH

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

; BRANCHF

(define (emit-branchf! as label)
  (emit-branchfreg! as $r.result label))

; BRANCHFREG -- introduced by peephole optimization.
; 
(define (emit-branchfreg! as hwreg label)
  (let ((label (make-asm-label label)))
    (emit! as `(,$i.subicc ,hwreg ,$imm.false ,$r.g0))
    (emit! as `(,$i.be.a ,label))
    (emit! as `(,$i.slot))))

; JUMP
;
; Some jumps are really long, so the simple offset in the jmp instruction is
; not enough, and an address must be calculated by hand. It would be sort of
; nice to be able to use the shorter sequence when possible, but right now
; only the hard way is used.

(define (emit-jump! as m label)
  (let* ((r     (emit-follow-chain! as m))
	 (label (make-asm-label label))
	 (tmp   (if (= r $r.tmp0) $r.tmp1 $r.tmp0)))
    (emit! as `(,$i.ldi ,r ,$p.codevector ,tmp))
; should probably check 'assume-short-distance-to-call', like SAVE.
;new
    (emit! as `(,$i.sethi (hi (+ ,$p.codeoffset ,label)) ,$r.tmp2))
    (emit! as `(,$i.addi ,$r.tmp2 (lo (+ ,$p.codeoffset ,label)) ,$r.tmp2))
    (emit! as `(,$i.jmplr ,tmp ,$r.tmp2 ,$r.g0))
;old
;    (emit! as `(,$i.jmpli ,tmp (+ ,label ,$p.codeoffset) ,$r.g0))
    (emit! as `(,$i.orr ,r ,$r.g0 ,$r.reg0))))

; .LABEL

(define (emit-label! as l)
  (emit! as `(,$i.label ,l)))

; .PROC
;
; no-op on sparc

(define (emit-.proc! as)
  '())

; EOF

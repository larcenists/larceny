; Asm/Sparc/gen-msi.sch
; Larceny -- SPARC assembler code emitters for core MacScheme instructions
;
; $Id$


; SETGLBL
;
; RS must be a hardware register.
;
; A global cell is a pair, where the car holds the value.

(define (emit-register->global! as rs offset)
  (cond ((= rs $r.result)
	 (sparc.move as $r.result $r.argreg2)
	 (emit-const->register! as offset $r.result)
	 (if (write-barrier)
	     (sparc.jmpli as $r.millicode $m.addtrans $r.o7))
	 (sparc.sti as $r.argreg2 (- $tag.pair-tag) $r.result))
	(else
	 (emit-const->register! as offset $r.result)
	 (sparc.sti as rs (- $tag.pair-tag) $r.result)
	 (if (write-barrier)
	     (millicode-call/1arg as $m.addtrans rs)))))


; GLOBAL
;
; A global cell is a pair, where the car holds the value.
; If (catch-undefined-globals) is true, then code will be emitted to
; check whether the global is #!undefined when loaded. If it is, 
; an exception will be taken, with the global in question in $r.result.

(define (emit-global->register! as offset r)
  (emit-load-global as offset r (catch-undefined-globals)))

; This leaves the cell in ARGREG2.  That fact is utilized by global/invoke
; to signal an appropriate error message.

(define (emit-load-global as offset r check?)
  
  (define (emit-undef-check! as r)
    (if check?
	(let ((GLOBAL-OK (new-label)))
	  (sparc.cmpi   as r $imm.undefined)
	  (sparc.bne.a  as GLOBAL-OK)
	  (sparc.slot   as)
	  (millicode-call/0arg as $m.global-ex)            ; Cell in ARGREG2.
	  (sparc.label  as GLOBAL-OK))))

  (emit-const->register! as offset $r.argreg2)             ; Load cell.
  (if (hardware-mapped? r)
      (begin (sparc.ldi as $r.argreg2 (- $tag.pair-tag) r)
	     (emit-undef-check! as r))
      (begin (sparc.ldi as $r.argreg2 (- $tag.pair-tag) $r.tmp0)
	     (emit-store-reg! as $r.tmp0 r)
	     (emit-undef-check! as $r.tmp0))))


; MOVEREG

(define (emit-register->register! as from to)
  (if (not (= from to))
      (cond ((and (hardware-mapped? from) (hardware-mapped? to))
	     (sparc.move as from to))
	    ((hardware-mapped? from)
	     (emit-store-reg! as from to))
	    ((hardware-mapped? to)
	     (emit-load-reg! as from to))
	    (else
	     (emit-load-reg! as from $r.tmp0)
	     (emit-store-reg! as $r.tmp0 to)))))


; ARGS=

(define (emit-args=! as n)
  (if (not (unsafe-code))
      (let ((L2 (new-label)))
	(sparc.cmpi   as $r.result (thefixnum n))  ; FIXME: limit 1023 args
	(sparc.be.a   as L2)
	(sparc.slot   as)
	(millicode-call/numarg-in-reg as $m.argc-ex (thefixnum n) $r.argreg2)
	(sparc.label  as L2))))


; ARGS>=
;
; The cases for 0 and 1 rest arguments are handled in-line; all other
; cases, including too few, are handled in millicode (really: a C call-out).
;
; The fast path only applies when we don't have to mess with the last
; register, hence the test.

(define (emit-args>=! as n)
  (let ((L0  (new-label))
	(L99 (new-label))
	(L98 (new-label)))
    (if (< n (- *lastreg* 1))
	(let ((dest (regname (+ n 1))))
	  (sparc.cmpi   as $r.result (thefixnum n)) ; n args
	  (if (hardware-mapped? dest)
	      (begin
		(sparc.be.a as L99)
		(sparc.set  as $imm.null dest))
	      (begin
		(sparc.set  as $imm.null $r.tmp0)
		(sparc.be.a as L99)
		(sparc.sti  as $r.tmp0 (swreg-global-offset dest) $r.globals)))
	  (sparc.cmpi   as $r.result (thefixnum (+ n 1))) ; n+1 args
	  (sparc.bne.a  as L98)
	  (sparc.nop    as)
	  (millicode-call/numarg-in-result as $m.alloc 8)
	  (let ((src1 (force-hwreg! as dest $r.tmp1)))
	    (sparc.set as $imm.null $r.tmp0)
	    (sparc.sti as src1 0 $r.result)
	    (sparc.sti as $r.tmp0 4 $r.result)
	    (sparc.addi as $r.result $tag.pair-tag $r.result)
	    (sparc.b as L99)
	    (if (hardware-mapped? dest)
		(sparc.move as $r.result dest)
		(sparc.sti  as $r.result (swreg-global-offset dest)
			    $r.globals)))))
    ; General case
    (sparc.label  as L98)
    (sparc.move   as $r.reg0 $r.argreg3)  ; FIXME in Sparc/mcode.s
    (millicode-call/numarg-in-reg as $m.varargs (thefixnum n) $r.argreg2)
    (sparc.label  as L99)))


; INVOKE
; SETRTN/INVOKE
;
; Bummed.  Can still do better when the procedure to call is in a general
; register (avoids the redundant move to RESULT preceding INVOKE).
;
; Note we must set up the argument count even in unsafe mode, because we 
; may be calling code that was not compiled unsafe.

(define (emit-invoke as n setrtn? mc-exception)
  (let ((START    (new-label))
	(TIMER-OK (new-label))
	(PROC-OK  (new-label)))
    (cond ((not (unsafe-code))
	   (sparc.label        as START)
	   (sparc.subicc       as $r.timer 1 $r.timer)
	   (sparc.bne          as TIMER-OK)
	   (sparc.andi         as $r.result $tag.tagmask $r.tmp0)
	   (millicode-call/ret as $m.timer-exception START)
	   (sparc.label        as TIMER-OK)
	   (sparc.cmpi         as $r.tmp0 $tag.procedure-tag)
	   (sparc.be.a         as PROC-OK)
	   (sparc.ldi          as $r.result $p.codevector $r.tmp0)
	   (millicode-call/ret as mc-exception START)
	   (sparc.label        as PROC-OK))
	  (else
	   (sparc.label        as START)
	   (sparc.subicc       as $r.timer 1 $r.timer)
	   (sparc.bne.a        as TIMER-OK)
	   (sparc.ldi          as $r.result $p.codevector $r.tmp0)
	   (millicode-call/ret as $m.timer-exception START)
	   (sparc.label        as TIMER-OK)))
    (sparc.move                as $r.result $r.reg0)
    ;; FIXME: limit 1023 args
    (cond (setrtn?
	   (sparc.set          as (thefixnum n) $r.result)
	   (sparc.jmpli        as $r.tmp0 $p.codeoffset $r.o7)
	   (sparc.sti          as $r.o7 4 $r.stkp))
	  (else
	   (sparc.jmpli        as $r.tmp0 $p.codeoffset $r.g0)
	   (sparc.set          as (thefixnum n) $r.result)))))

; SAVE -- for new compiler
;
; Create stack frame.  To avoid confusing the garbage collector, the
; slots must be initialized to something definite unless they will
; immediately be initialized by a MacScheme machine store instruction.
; The creation is done by emit-save0!, and the initialization is done
; by emit-save1!.

(define (emit-save0! as n)
  (let* ((L1        (new-label))
	 (L0        (new-label))
	 (framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (sparc.label  as L0)
    (sparc.subi   as $r.stkp realsize $r.stkp)
    (sparc.cmpr   as $r.stklim $r.stkp)
    (sparc.ble.a  as L1)
    (sparc.set    as framesize $r.tmp0)
    (sparc.addi   as $r.stkp realsize $r.stkp)
    (millicode-call/ret as $m.stkoflow L0)
    (sparc.label  as L1)
    ; initialize size and return fields of stack frame
    (sparc.sti    as $r.tmp0 0 $r.stkp)
    (sparc.sti    as $r.g0 4 $r.stkp)))

; Given a vector v of booleans, initializes slot i of the stack frame
; if and only if (vector-ref v i).

(define (emit-save1! as v)
  (let ((n (vector-length v)))
    (let loop ((i 0) (offset 12))
      (cond ((= i n)
             #t)
            ((vector-ref v i)
	     (sparc.sti as $r.g0 offset $r.stkp)
	     (loop (+ i 1) (+ offset 4)))
	    (else
	     (loop (+ i 1) (+ offset 4)))))))


; RESTORE
;
; Restore registers from stack frame
; FIXME: Use ldd/std here; see comments for emit-save!, above.
; We pop only actual registers.

(define (emit-restore! as n)
  (let ((n (min n 31)))
    (do ((i      0  (+ i 1))
	 (offset 12 (+ offset 4)))
	((> i n))
      (let ((r (regname i)))
	(if (hardware-mapped? r)
	    (sparc.ldi as $r.stkp offset r)
	    (begin (sparc.ldi as $r.stkp offset $r.tmp0)
		   (emit-store-reg! as $r.tmp0 r)))))))

; POP -- for new compiler
;
; Pop frame.
; If returning?, then emit the return as well and put the pop
; in its delay slot.

(define (emit-pop! as n returning?)
  (let* ((framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (if returning?
        (begin (sparc.ldi   as $r.stkp (+ realsize 4) $r.o7)
	       (sparc.jmpli as $r.o7 8 $r.g0)
	       (sparc.addi  as $r.stkp realsize $r.stkp))
        (sparc.addi as $r.stkp realsize $r.stkp))))


; SETRTN
;
; Change the return address in the stack frame.

(define (emit-setrtn! as label)
  (emit-return-address! as label)
  (sparc.sti as $r.o7 4 $r.stkp))


; APPLY
;
; `apply' falls into millicode.
;
; The timer check is performed here because it is not very easy for the
; millicode to do this.

(define (emit-apply! as r1 r2)
  (let ((L0 (new-label)))
    (check-timer as L0 L0)
    (sparc.label as L0)
    (emit-move2hwreg! as r1 $r.argreg2)
    (emit-move2hwreg! as r2 $r.argreg3)
    (millicode-call/0arg as $m.apply)))


; LOAD

(define (emit-load! as slot dest-reg)
  (if (hardware-mapped? dest-reg)
      (sparc.ldi as $r.stkp (+ 12 (* slot 4)) dest-reg)
      (begin (sparc.ldi as $r.stkp (+ 12 (* slot 4)) $r.tmp0)
	     (emit-store-reg! as $r.tmp0 dest-reg))))


; STORE

(define (emit-store! as k n)
  (if (hardware-mapped? k)
      (sparc.sti as k (+ 12 (* n 4)) $r.stkp)
      (begin (emit-load-reg! as k $r.tmp0)
	     (sparc.sti as $r.tmp0 (+ 12 (* n 4)) $r.stkp))))


; LEXICAL

(define (emit-lexical! as m n)
  (let ((base (emit-follow-chain! as m)))
    (sparc.ldi as base (- (procedure-slot-offset n) $tag.procedure-tag)
	       $r.result)))


; SETLEX
; FIXME: should allow an in-line barrier

(define (emit-setlex! as m n)
  (let ((base (emit-follow-chain! as m)))
    (sparc.sti as $r.result (- (procedure-slot-offset n) $tag.procedure-tag)
	       base)
    (if (write-barrier)
	(begin
	  (sparc.move as $r.result $r.argreg2)
	  (millicode-call/1arg-in-result as $m.addtrans base)))))


; Follow static links.
;
; By using and leaving the result in ARGREG3 rather than in RESULT, 
; we save a temporary register.

(define (emit-follow-chain! as m)
  (let loop ((q m))
    (cond ((not (zero? q))
	   (sparc.ldi as
		      (if (= q m) $r.reg0 $r.argreg3)
		      $p.linkoffset
		      $r.argreg3)
	   (loop (- q 1)))
	  ((zero? m) 
	   $r.reg0)
	  (else 
	   $r.argreg3))))

; RETURN

(define (emit-return! as)
  (sparc.ldi   as $r.stkp 4 $r.o7)
  (sparc.jmpli as $r.o7 8 $r.g0)
  (sparc.nop   as))


; RETURN-REG k

(define (emit-return-reg! as r)
  (sparc.ldi   as $r.stkp 4 $r.o7)
  (sparc.jmpli as $r.o7 8 $r.g0)
  (sparc.move  as r $r.result))


; RETURN-CONST k
;
; The constant c must be synthesizable in a single instruction.

(define (emit-return-const! as c)
  (sparc.ldi   as $r.stkp 4 $r.o7)
  (sparc.jmpli as $r.o7 8 $r.g0)
  (emit-constant->register as c $r.result))


; MVRTN

(define (emit-mvrtn! as)
  (asm-error "multiple-value return has not been implemented (yet)."))


; LEXES

(define (emit-lexes! as n-slots)
  (emit-alloc-proc! as n-slots)
  (sparc.ldi as $r.reg0 $p.codevector $r.tmp0)
  (sparc.ldi as $r.reg0 $p.constvector $r.tmp1)
  (sparc.sti as $r.tmp0 $p.codevector $r.result)
  (sparc.sti as $r.tmp1 $p.constvector $r.result)
  (emit-init-proc-slots! as n-slots))


; LAMBDA

(define (emit-lambda! as code-offs const-offs n-slots)
  (let ((code-offs  (+ 4 (- (* 4 code-offs) $tag.vector-tag)))
	(const-offs (+ 4 (- (* 4 const-offs) $tag.vector-tag))))
  (emit-alloc-proc! as n-slots)
  (sparc.ldi as $r.reg0 $p.constvector $r.tmp0)
  (sparc.ldi as $r.tmp0 code-offs $r.tmp1)
  (sparc.sti as $r.tmp1 $p.codevector $r.result)
  (sparc.ldi as $r.tmp0 const-offs $r.tmp1)
  (sparc.sti as $r.tmp1 $p.constvector $r.result)
  (emit-init-proc-slots! as n-slots)))
 
; Allocate procedure with room for n register slots; return tagged pointer.

(define emit-alloc-proc!
  (let ((two^12 (expt 2 12)))
    (lambda (as n)
      (millicode-call/numarg-in-result as $m.alloc (* (+ n 4) 4))
      (let ((header (+ (* (* (+ n 3) 4) 256) $imm.procedure-header)))
	(emit-immediate->register! as header $r.tmp0)
	(sparc.sti  as $r.tmp0 0 $r.result)
	(sparc.addi as $r.result $tag.procedure-tag $r.result)))))

; Initialize data slots in procedure from current registers as specified for
; `lamba' and `lexes'. If there are more data slots than registers, then
; we must generate code to cdr down the list in the last register to obtain
; the rest of the data. The list is expected to have at least the minimal
; length.
;
; The tagged pointer to the procedure is in $r.result.

(define (emit-init-proc-slots! as n)

  (define (save-registers lo hi offset)
    (do ((lo     lo     (+ lo 1))
	 (offset offset (+ offset 4)))
	((> lo hi))
      (let ((r (force-hwreg! as (regname lo) $r.tmp0)))
	(sparc.sti as r offset $r.result))))

  (define (save-list lo hi offset)
    (emit-load-reg! as $r.reg31 $r.tmp0)
    (do ((lo     lo      (+ lo 1))
	 (offset offset (+ offset 4)))
	((> lo hi))
      (sparc.ldi as $r.tmp0 (- $tag.pair-tag) $r.tmp1)
      (sparc.sti as $r.tmp1 offset $r.result)
      (if (< lo hi)
	  (begin 
	    (sparc.ldi as $r.tmp0 (+ (- $tag.pair-tag) 4) $r.tmp0)))))
      
  (cond ((< n *lastreg*)
	 (save-registers 0 n $p.reg0))
	(else
	 (save-registers 0 (- *lastreg* 1) $p.reg0)
	 (save-list      *lastreg* n (+ $p.reg0 (* *lastreg* 4))))))

; BRANCH

(define (emit-branch! as check-timer? label)
  (if check-timer?
      (check-timer as label label)
      (begin (sparc.b   as label)
	     (sparc.nop as))))


; BRANCHF

(define (emit-branchf! as label)
  (emit-branchfreg! as $r.result label))


; BRANCHFREG -- introduced by peephole optimization.

(define (emit-branchfreg! as hwreg label)
  (sparc.cmpi as hwreg $imm.false)
  (sparc.be.a as label)
  (sparc.slot as))


; BRANCH-WITH-SETRTN -- introduced by peephole optimization

(define (emit-branch-with-setrtn! as label)
  (let ((L0 (new-label)))
    (check-timer as L0 L0)
    (sparc.label as L0)
    (sparc.call as label)
    (sparc.sti as $r.o7 4 $r.stkp)))

; JUMP
;
; Given the finalization order (outer is finalized before inner is assembled)
; the label value will always be available when a jump is assembled.  The
; only exception is when m = 0, but does this ever happen?  This code handles
; the case anyway.

(define (emit-jump! as m label)
  (let* ((r      (emit-follow-chain! as m))
	 (labelv (label-value as label))
	 (v      (if (number? labelv)
		     (+ labelv $p.codeoffset)
		     (list '+ label $p.codeoffset))))
    (sparc.ldi as r $p.codevector $r.tmp0)
    (if (and (number? v) (immediate-literal? v))
	(sparc.jmpli as $r.tmp0 v $r.g0)
	(begin (emit-immediate->register! as v $r.tmp1)
	       (sparc.jmplr as $r.tmp0 $r.tmp1 $r.g0)))
    (sparc.move  as r $r.reg0)))


; .SINGLESTEP
;
; Single step: jump to millicode; pass index of documentation string in
; %TMP0. Some instructions execute when reg0 is not a valid pointer to
; the current procedure (because this is just after returning); in this
; case we restore reg0 from the stack location given by 'funkyloc'.

(define (emit-singlestep-instr! as funky? funkyloc cvlabel)
  (if funky?
      (sparc.ldi as $r.stkp (+ (thefixnum funkyloc) 12) $r.reg0))
  (millicode-call/numarg-in-reg as $m.singlestep
				   (thefixnum cvlabel)
				   $r.argreg2))


; Emit the effective address of a label-8 into %o7.
;
; There are multiple ways to do this.  If the call causes an expensive
; bubble in the pipeline it is probably much less expensive to grub
; the code vector address out of the procedure in REG0 and calculate it
; that way.  FIXME: We need to benchmark these options.
;
; In general the point is moot as the common-case sequence
;       setrtn L1
;       invoke n
;   L1:
; should be peephole-optimized into the obvious fast code.

(define (emit-return-address! as label)
  (let* ((loc  (here as))
	 (lloc (label-value as label)))

    (define (emit-short val)
      (sparc.call as (+ loc 8))
      (sparc.addi as $r.o7 val $r.o7))

    (define (emit-long val)
      ; Don't use sparc.set: we need to know that two instructions get
      ; generated.
      (sparc.sethi as `(hi ,val) $r.tmp0)
      (sparc.ori   as $r.tmp0 `(lo ,val) $r.tmp0)
      (sparc.call  as (+ loc 16))
      (sparc.addr  as $r.o7 $r.tmp0 $r.o7))

    (cond (lloc
	   (let ((target-rel-addr (- lloc loc 8)))
	     (if (immediate-literal? target-rel-addr)
		 (emit-short target-rel-addr)
		 (emit-long (- target-rel-addr 8)))))
	  ((short-effective-addresses)
	   (emit-short `(- ,label ,loc 8)))
	  (else
	   (emit-long `(- ,label ,loc 16))))))

; eof

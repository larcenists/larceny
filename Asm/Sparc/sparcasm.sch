; Asm/Sparc/sparcasm.sch
; Larceny -- SPARC machine assembler
;
; $Id$
;
; The procedure `sparc-instruction' takes an instruction class keyword and
; some operands and returns an assembler procedure for the instruction
; denoted by the class and the operands.
;
; All assembler procedures for SPARC mnemonics are defined at the end 
; of this file.
;
; The SPARC has 32-bit, big-endian words.  All instructions are 1 word.
; This assembler currently accepts a subset of the SPARC v8 instruction set.
;
; Each assembler procedure takes an `as' assembly structure (see 
; Asm/Common/pass5p1.sch) and operands relevant to the instruction, and
; side-effects the assembly structure by emitting bits for the instruction
; and any necessary fixups.  There are separate instruction mnemonics and
; assembler procedures for instructions which in the SPARC instruction set 
; are normally considered the "same".  For example, the `add' instruction is
; split into two operations here: `sparc.addr' takes a register as operand2,
; and `sparc.addi' takes an immediate.  We could remove this restriction
; by using objects with identity rather than numbers for registers, but it
; does not seem to be an important problem.
;
; Operands that denote values (addresses, immediates, offsets) may be
; expressed using symbolic expressions. These expressions must conform
; to the following grammar:
;
;   <expr> --> ( <number> . <obj> )        ; label
;            | <number>                    ; literal value (exact integer)
;            | (+ <expr> ... )             ; sum
;            | (- <expr> ... )             ; difference
;            | (hi <expr>)                 ; high 22 bits
;            | (lo <expr>)                 ; low 10 bits
;
; Each assembler procedure will check that its value operand(s) fit in 
; their instruction fields.  It is a fatal error for an operand not 
; to fit, and the assembler calls `asm-error' to signal this error.  
; However, in some cases the assembler will instead call the error 
; procedure `asm-value-too-large', which allows the higher-level assembler 
; to retry the assembly with different settings (typically, by splitting 
; a jump instruction into an offset calculation and a jump).
;
; Note: I removed `$' from the expression grammar because it wreaks havoc with
; fixups and because `(here as)' can always be used instead.
;
; Note: the idiom that is seen in this file,
;   (emit-fixup-proc! as (lambda (b l) (fixup b l)))
; when `fixup' is a local procedure, avoids allocation of the closure
; except in the cases where the fixup is in fact needed, for gains in
; speed and reduction in allocation.  (Ask me if you want numbers.)
 
(define sparc-instruction)

(let ()

  (define ibit (asm:bv 0 0 #x20 0))	; immediate bit: 2^13
  (define abit (asm:bv #x20 0 0 0))	; annul bit: 2^29
  (define zero (asm:bv 0 0 0 0))        ; all zero bits

  (define two^32 (expt 2 32))

  ; Constant expression evaluation. If the expression cannot be 
  ; evaluated, eval-expr returns #f, otherwise a number.
  ; The symbol table lookup must fail by returning #f.

  (define (eval-expr as e)

    (define (complement x)
      (modulo (+ two^32 x) two^32))

    (define (hibits e)
      (cond ((not e) e)
	    ((< e 0)
	     (complement (quotient (complement e) 1024)))
	    (else
	     (quotient e 1024))))

    (define (lobits e)
      (cond ((not e) e)
	    ((< e 0)
	     (remainder (complement e) 1024))
	    (else
	     (remainder e 1024))))

    (define (evaluate e)
      (cond ((integer? e)      e)
	    ((label? e)        (label-value as e))
	    ((eq? 'hi (car e)) (hibits (evaluate (cadr e))))
	    ((eq? 'lo (car e)) (lobits (evaluate (cadr e))))
	    ((eq? '+ (car e))
	     (let loop ((e (cdr e)) (s 0))
	       (if (null? e) s
		   (let ((op (evaluate (car e))))
		     (if (not op) op
			 (loop (cdr e) (+ s op)))))))
	    ((eq? '- (car e))  
	     (let loop ((e (cdr e)) (d #f))
	       (if (null? e) d
		   (let ((op (evaluate (car e))))
		     (if (not op) op
			 (loop (cdr e) (if d (- d op) op)))))))
	    (else
	     (signal-error 'badexpr e))))

    (evaluate e))

  ; Common error handling.

  (define (signal-error code . rest)
    (define msg "SPARC assembler: ")
    (case code
      ((badexpr)
       (asm-error msg "invalid expression " (car rest)))
      ((toolarge)
       (asm-error msg "value too large in " (car rest) ": "
		  (cadr rest) " = " (caddr rest)))
      ((fixup)
       (asm-error msg "fixup failed in " (car rest) " for " (cadr rest)))
      ((unaligned)
       (asm-error msg "unaligned target in " (car rest) ": " (cadr rest)))
      (else 
       (error "Invalid error code in assembler: " code))))

  ; The following procedures construct instructions by depositing field
  ; values directly into bytevectors; the location parameter in the dep-*!
  ; procedures is the address in the bytevector of the most significant byte.

  (define (copy bits)
    (let ((bv (make-bytevector 4)))
      (bytevector-set! bv 0 (bytevector-ref bits 0))
      (bytevector-set! bv 1 (bytevector-ref bits 1))
      (bytevector-set! bv 2 (bytevector-ref bits 2))
      (bytevector-set! bv 3 (bytevector-ref bits 3))
      bv))

  (define (copy-instr bv from to)
    (bytevector-set! bv to (bytevector-ref bv from))
    (bytevector-set! bv (+ to 1) (bytevector-ref bv (+ from 1)))
    (bytevector-set! bv (+ to 2) (bytevector-ref bv (+ from 2)))
    (bytevector-set! bv (+ to 3) (bytevector-ref bv (+ from 3))))

  (define (dep-rs1! bits k rs1)
    (bytevector-set! bits (+ k 1)
		     (logior (bytevector-ref bits (+ k 1))
			     (rshl rs1 2)))
    (bytevector-set! bits (+ k 2)
		     (logior (bytevector-ref bits (+ k 2))
			     (lsh (logand rs1 3) 6))))

  (define (dep-rs2! bits k rs2)
    (bytevector-set! bits (+ k 3)
		     (logior (bytevector-ref bits (+ k 3)) rs2)))

  (define (dep-rd! bits k rd)
    (bytevector-set! bits k
		     (logior (bytevector-ref bits k) (lsh rd 1))))

  (define (dep-imm! bits k imm)
    (cond ((fixnum? imm)
	   (bytevector-set! bits (+ k 3) (logand imm 255))
	   (bytevector-set! bits (+ k 2)
			    (logior (bytevector-ref bits (+ k 2))
				    (logand (rsha imm 8) 31))))
	  ((bytevector? imm)
	   (bytevector-set! bits (+ k 3) (bytevector-ref imm 0))
	   (bytevector-set! bits (+ k 2)
			    (logior (bytevector-ref bits (+ k 2))
				    (logand (bytevector-ref imm 1)
					    31))))
	  (else
	   (dep-imm! bits k (asm:int->bv imm)))))

  (define (dep-branch-offset! bits k offs)
    (cond ((fixnum? offs)
	   (if (not (= (logand offs 3) 0))
	       (signal-error 'unaligned "branch" offs))
	   (dep-imm22! bits k (rsha offs 2)))
	  ((bytevector? offs)
	   (if (not (= (logand (bytevector-ref offs 3) 3) 0))
	       (signal-error 'unaligned "branch" (asm:bv->int offs)))
	   (dep-imm22! bits k (asm:rsha offs 2)))
	  (else
	   (dep-branch-offset! bits k (asm:int->bv offs)))))

  (define (dep-imm22! bits k imm)
    (cond ((fixnum? imm)
	   (bytevector-set! bits (+ k 3) (logand imm 255))
	   (bytevector-set! bits (+ k 2)
			    (logand (rsha imm 8) 255))
	   (bytevector-set! bits (+ k 1)
			    (logior (bytevector-ref bits (+ k 1))
				    (logand (rsha imm 16) 63))))
	  ((bytevector? imm)
	   (bytevector-set! bits (+ k 3) (bytevector-ref imm 3))
	   (bytevector-set! bits (+ k 2) (bytevector-ref imm 2))
	   (bytevector-set! bits (+ k 1)
			    (logior (bytevector-ref bits (+ k 1))
				    (logand (bytevector-ref imm 1)
					    63))))
	  (else
	   (dep-imm22! bits k (asm:int->bv imm)))))

  (define (dep-call-offset! bits k offs)
    (cond ((fixnum? offs)
	   (if (not (= (logand offs 3) 0))
	       (signal-error 'unaligned "call" offs))
	   (bytevector-set! bits (+ k 3) (logand (rsha offs 2) 255))
	   (bytevector-set! bits (+ k 2) (logand (rsha offs 10) 255))
	   (bytevector-set! bits (+ k 1) (logand (rsha offs 18) 255))
	   (bytevector-set! bits k (logior (bytevector-ref bits k)
					   (logand (rsha offs 26) 63))))
	  ((bytevector? offs)
	   (if (not (= (logand (bytevector-ref offs 3) 3) 0))
	       (signal-error 'unaligned "call" (asm:bv->int offs)))
	   (let ((offs (asm:rsha offs 2)))
	     (bytevector-set! bits (+ k 3) (bytevector-ref offs 3))
	     (bytevector-set! bits (+ k 2) (bytevector-ref offs 2))
	     (bytevector-set! bits (+ k 1) (bytevector-ref offs 1))
	     (bytevector-set! bits k (logior (bytevector-ref bits k)
					     (logand (bytevector-ref offs 0)
						     63)))))
	  (else
	   (dep-call-offset! bits k (asm:int->bv offs)))))

  ; Add 1 to an instruction (to bump a branch offset by 4).
  ; FIXME: should check for field overflow.

  (define (add1 bv loc)
    (let* ((r0 (+ (bytevector-ref bv (+ loc 3)) 1))
	   (d0 (logand r0 255))
	   (c0 (rshl r0 8)))
      (bytevector-set! bv (+ loc 3) d0)
      (let* ((r1 (+ (bytevector-ref bv (+ loc 2)) c0))
	     (d1 (logand r1 255))
	     (c1 (rshl r1 8)))
	(bytevector-set! bv (+ loc 2) d1)
	(let* ((r2 (+ (bytevector-ref bv (+ loc 1)) c1))
	       (d2 (logand r2 255)))
	  (bytevector-set! bv (+ loc 1) d2)))))

  ; For delay slot filling -- uses the assembler value scratchpad in
  ; the as structure.  Delay slot filling is discussed in the comments
  ; for `branch' and `class-slot', below.
  
  (define (remember-branch-target as obj)
    (assembler-value! as 'branch-target obj))

  (define (recover-branch-target as)
    (assembler-value as 'branch-target))

  ; Mark the instruction at the current address as not being eligible 
  ; for being lifted into a branch delay slot.
  ;
  ; FIXME: should perhaps be a hash table; see BOOT-STATUS file for details.

  (define (not-a-delay-slot-instruction as)
    (assembler-value! as 'not-dsi
		      (cons (here as)
			    (or (assembler-value as 'not-dsi) '()))))

  (define (is-a-delay-slot-instruction? as addr)
    (not (memv addr (or (assembler-value as 'not-dsi) '()))))

  ; SETHI, etc.

  (define (class-sethi bits)
    (let ((bits (asm:lsh bits 22)))
      (lambda (as val rd)

	(define (fixup bv loc)
	  (dep-imm22! bv loc
		      (or (eval-expr as val)
			  (signal-error 'fixup "sethi" val))))

	(let ((bits (copy bits))
	      (e    (eval-expr as val)))
	  (if e
	      (dep-imm22! bits 0 e)
	      (emit-fixup-proc! as (lambda (b l) (fixup b l))))
	  (dep-rd! bits 0 rd)
	  (emit! as bits)))))

  ; NOP is a peculiar sethi

  (define (class-nop i)
    (let ((instr (class-sethi i)))
      (lambda (as)
	(instr as 0 $r.g0))))

  ; Un-annulled branches.

  (define (class00b i) (branch i zero))

  ; Annulled branches.

  (define (class00a i) (branch i abit))

  ; Branches in general.  
  ; The `annul' parameter is either `zero' or `abit' (see top of file).
  ;
  ; Annuled branches require special treatement for delay slot
  ; filling based on the `slot' pseudo-instruction.
  ;
  ; Strategy: when a branch with the annul bit set is assembled, remember 
  ; its target in a one-element cache in the AS structure. When a slot
  ; instruction is found (it has its own class) then the cached
  ; value (possibly a delayed expression) is gotten, and a fixup for the
  ; slot is registered.  When the fixup is later evaluated, the branch
  ; target instruction can be found, examined, and evaluated. 
  ; 
  ; The cached value is always valid when the slot instruction is assembled,
  ; because a slot instruction is always directly preceded by an annulled
  ; branch (which will always set the cache).

  (define (branch bits annul)
    (let ((bits (asm:logior (asm:lsh bits 25) (asm:lsh #b010 22) annul)))
      (lambda (as target0)
	(let ((target `(- ,target0 ,(here as))))

	  (define (expr)
	    (let ((e (eval-expr as target)))
	      (cond ((not e)
		     e)
		    ((not (zero? (logand e 3)))
		     (signal-error 'unaligned "branch" target0))
		    ((asm:fits? e 24)
		     e)
		    (else
		     (asm-value-too-large as "branch" target e)))))

	  (define (fixup bv loc)
	    (let ((e (expr)))
	      (if e
		  (dep-branch-offset! bv loc e)
		  (signal-error 'fixup "branch" target0))))

	  (if (not (eq? annul zero))
	      (remember-branch-target as target0)
	      (remember-branch-target as #f)) ; Clears the cache.
	  (not-a-delay-slot-instruction as)
	  (let ((bits (copy bits))
		(e    (expr)))
	    (if e
		(dep-branch-offset! bits 0 e)
		(emit-fixup-proc! as (lambda (b l) (fixup b l))))
	    (emit! as bits))))))

  ; Branch delay slot pseudo-instruction.
  ;
  ; Get the branch target expression from the cache in the AS structure,
  ; and if it is not #f, register a fixup procedure for the delay slot that 
  ; will copy the target instruction to the slot and add 4 to the branch
  ; offset (unless that will overflow the offset or the instruction at the
  ; target is not suitable for lifting).
  ;
  ; It's important that this fixup run _after_ any fixups for the branch
  ; instruction itself!

  (define (class-slot)
    (let ((nop-instr (class-nop #b100)))
      (lambda (as)

	; The branch target is the expression denoting the target location.

	(define branch-target (recover-branch-target as))

	(define (fixup bv loc)
	  (let ((bt (or (eval-expr as branch-target)
			(asm-error "Branch fixup: can't happen: " 
				   branch-target))))
	    (if (is-a-delay-slot-instruction? as bt)
		(begin
		  (copy-instr bv bt loc)
		  (add1 bv (- loc 4))))))

	(if (and branch-target (fill-delay-slots))
	    (emit-fixup-proc! as (lambda (b l) (fixup b l))))
	(nop-instr as))))

  ; ALU stuff, register operand, rdy, wryr. Also: jump.

  (define (class10r bits . extra)
    (cond ((and (not (null? extra)) (eq? (car extra) 'rdy))
	   (let ((op (class10r bits)))
	     (lambda (as rd)
	       (op as 0 0 rd))))
	  ((and (not (null? extra)) (eq? (car extra) 'wry))
	   (let ((op (class10r bits)))
	     (lambda (as rs)
	       (op as rs 0 0))))
	  (else
	   (let ((bits  (asm:logior (asm:lsh #b10 30) (asm:lsh bits 19)))
		 (jump? (and (not (null? extra)) (eq? (car extra) 'jump))))
	     (lambda (as rs1 rs2 rd)
	       (if jump?
		   (not-a-delay-slot-instruction as))
	       (let ((bits (copy bits)))
		 (dep-rs1! bits 0 rs1)
		 (dep-rs2! bits 0 rs2)
		 (dep-rd! bits 0 rd)
		 (emit! as bits)))))))


  ; ALU stuff, immediate operand, wryi. Also: jump.

  (define (class10i bits  . extra)
    (if (and (not (null? extra)) (eq? (car extra) 'wry))
	(let ((op (class10i bits)))
	  (lambda (as src)
	    (op as 0 src 0)))
	(let ((bits  (asm:logior (asm:lsh #b10 30) (asm:lsh bits 19) ibit))
	      (jump? (and (not (null? extra)) (eq? (car extra) 'jump))))
	  (lambda (as rs1 e rd)

	    (define (expr)
	      (let ((imm (eval-expr as e)))
		(cond ((not imm)
		       imm)
		      ((asm:fits? imm 13)
		       imm)
		      (jump?
		       (asm-value-too-large as "`jmpli'" e imm))
		      (else
		       (asm-value-too-large as "ALU instruction" e imm)))))

	    (define (fixup bv loc)
	      (let ((e (expr)))
		(if e
		    (dep-imm! bv loc e)
		    (signal-error 'fixup "ALU instruction" e))))

	    (if jump?
		(not-a-delay-slot-instruction as))
	    (let ((bits (copy bits))
		  (e    (expr)))
	      (if e
		  (dep-imm! bits 0 e)
		  (emit-fixup-proc! as (lambda (b l) (fixup b l))))
	      (dep-rs1! bits 0 rs1)
	      (dep-rd! bits 0 rd)
	      (emit! as bits))))))

  ; Memory stuff, register operand.

  (define (class11r bits)
    (let ((bits (asm:logior (asm:lsh #b11 30) (asm:lsh bits 19))))
      (lambda (as rs1 rs2 rd)
	(let ((bits (copy bits)))
	  (dep-rs1! bits 0 rs1)
	  (dep-rs2! bits 0 rs2)
	  (dep-rd! bits 0 rd)
	  (emit! as bits)))))

  ; Memory stuff, immediate operand.

  (define (class11i bits)
    (let ((bits (asm:logior (asm:lsh #b11 30) (asm:lsh bits 19) ibit)))
      (lambda (as rs1 e rd)

	(define (expr)
	  (let ((imm (eval-expr as e)))
	    (cond ((not imm) imm)
		  ((asm:fits? imm 13) imm)
		  (else 
		   (signal-error 'toolarge "Memory instruction" e imm)))))

	(define (fixup bv loc)
	  (let ((e (expr)))
	    (if e
		(dep-imm! bv loc e)
		(signal-error 'fixup "Memory instruction" e))))

	(let ((bits (copy bits))
	      (e    (expr)))
	  (dep-rs1! bits 0 rs1)
	  (dep-rd! bits 0 rd)
	  (if e
	      (dep-imm! bits 0 e)
	      (emit-fixup-proc! as (lambda (b l) (fixup b l))))
	  (emit! as bits)))))

  ; For store instructions.  The syntax is (st a b c) meaning m[ b+c ] <- a.
  ; However, on the Sparc, the destination (rd) field is  the source of
  ; a store, so we transform the instruction into (st c b a) and pass it
  ; to the real store procedure.

  (define (class11sr bits)
    (let ((store-instr (class11r bits)))
      (lambda (as a b c)
	(store-instr as c b a))))

  (define (class11si bits)
    (let ((store-instr (class11i bits)))
      (lambda (as a b c)
	(store-instr as c b a))))

  ; Call is a class all by itself.
    
  (define (class-call)
    (let ((code (asm:lsh #b01 30)))
      (lambda (as target0)
	(let ((target `(- ,target0 ,(here as))))

	  (define (fixup bv loc)
	    (let ((e (eval-expr as target)))
	      (if e
		  (dep-call-offset! bv loc e)
		  (signal-error 'fixup "call" target0))))

	  (let ((bits (copy code))
		(e    (eval-expr as target)))
	    (not-a-delay-slot-instruction as)
	    (if e
		(dep-call-offset! bits 0 e)
		(emit-fixup-proc! as (lambda (b l) (fixup b l))))
	    (emit! as bits))))))

  (define (class-label)
    (lambda (as label)
      (emit-label! as label)))

  (set! sparc-instruction
	(lambda (kwd . ops)
	  (case kwd
	    ((i11)   (apply class11i ops))
	    ((r11)   (apply class11r ops))
	    ((si11)  (apply class11si ops))
	    ((sr11)  (apply class11sr ops))
	    ((sethi) (apply class-sethi ops))
	    ((r10)   (apply class10r ops))
	    ((i10)   (apply class10i ops))
	    ((b00)   (apply class00b ops))
	    ((a00)   (apply class00a ops))
	    ((call)  (apply class-call ops))
	    ((label) (apply class-label ops))
	    ((nop)   (apply class-nop ops))
	    ((slot)  (apply class-slot ops))
	    (else
	     (asm-error "sparc-instruction: unrecognized class: " kwd)))))
  'sparc-instruction)

; Instruction mnemonics

(define sparc.lddi    (sparc-instruction 'i11 #b000011))
(define sparc.lddr    (sparc-instruction 'r11 #b000011))
(define sparc.ldi     (sparc-instruction 'i11 #b000000))
(define sparc.ldr     (sparc-instruction 'r11 #b000000))
(define sparc.ldhi    (sparc-instruction 'i11 #b000010))
(define sparc.ldhr    (sparc-instruction 'r11 #b000010))
(define sparc.ldbi    (sparc-instruction 'i11 #b000001))
(define sparc.ldbr    (sparc-instruction 'r11 #b000001))
(define sparc.lddfi   (sparc-instruction 'i11 #b100001))
(define sparc.lddfr   (sparc-instruction 'r11 #b100001))
(define sparc.stdi    (sparc-instruction 'si11 #b000111))
(define sparc.stdr    (sparc-instruction 'sr11 #b000111))
(define sparc.sti     (sparc-instruction 'si11 #b000100))
(define sparc.str     (sparc-instruction 'sr11 #b000100))
(define sparc.sthi    (sparc-instruction 'si11 #b000110))
(define sparc.sthr    (sparc-instruction 'sr11 #b000110))
(define sparc.stbi    (sparc-instruction 'si11 #b000101))
(define sparc.stbr    (sparc-instruction 'sr11 #b000101))
(define sparc.stdfi   (sparc-instruction 'si11 #b100111))
(define sparc.stdfr   (sparc-instruction 'sr11 #b100111))
(define sparc.sethi   (sparc-instruction 'sethi #b100))
(define sparc.andr    (sparc-instruction 'r10 #b000001))
(define sparc.andrcc  (sparc-instruction 'r10 #b010001))
(define sparc.andi    (sparc-instruction 'i10 #b000001))
(define sparc.andicc  (sparc-instruction 'i10 #b010001))
(define sparc.orr     (sparc-instruction 'r10 #b000010))
(define sparc.orrcc   (sparc-instruction 'r10 #b010010))
(define sparc.ori     (sparc-instruction 'i10 #b000010))
(define sparc.oricc   (sparc-instruction 'i10 #b010010))
(define sparc.xorr    (sparc-instruction 'r10 #b000011))
(define sparc.xorrcc  (sparc-instruction 'r10 #b010011))
(define sparc.xori    (sparc-instruction 'i10 #b000011))
(define sparc.xoricc  (sparc-instruction 'i10 #b010011))
(define sparc.sllr    (sparc-instruction 'r10 #b100101))
(define sparc.slli    (sparc-instruction 'i10 #b100101))
(define sparc.srlr    (sparc-instruction 'r10 #b100110))
(define sparc.srli    (sparc-instruction 'i10 #b100110))
(define sparc.srar    (sparc-instruction 'r10 #b100111))
(define sparc.srai    (sparc-instruction 'i10 #b100111))
(define sparc.addr    (sparc-instruction 'r10 #b000000))
(define sparc.addrcc  (sparc-instruction 'r10 #b010000))
(define sparc.addi    (sparc-instruction 'i10 #b000000))
(define sparc.addicc  (sparc-instruction 'i10 #b010000))
(define sparc.taddrcc (sparc-instruction 'r10 #b100000))
(define sparc.taddicc (sparc-instruction 'i10 #b100000))
(define sparc.subr    (sparc-instruction 'r10 #b000100))
(define sparc.subrcc  (sparc-instruction 'r10 #b010100))
(define sparc.subi    (sparc-instruction 'i10 #b000100))
(define sparc.subicc  (sparc-instruction 'i10 #b010100))
(define sparc.tsubrcc (sparc-instruction 'r10 #b100001))
(define sparc.tsubicc (sparc-instruction 'i10 #b100001))
(define sparc.smulr   (sparc-instruction 'r10 #b001011))
(define sparc.smulrcc (sparc-instruction 'r10 #b011011))
(define sparc.smuli   (sparc-instruction 'i10 #b001011))
(define sparc.smulicc (sparc-instruction 'i10 #b011011))
(define sparc.sdivr   (sparc-instruction 'r10 #b001111))
(define sparc.sdivrcc (sparc-instruction 'r10 #b011111))
(define sparc.sdivi   (sparc-instruction 'i10 #b001111))
(define sparc.sdivicc (sparc-instruction 'i10 #b011111))
(define sparc.b       (sparc-instruction 'b00 #b1000))
(define sparc.b.a     (sparc-instruction 'a00 #b1000))
(define sparc.bne     (sparc-instruction 'b00 #b1001))
(define sparc.bne.a   (sparc-instruction 'a00 #b1001))
(define sparc.be      (sparc-instruction 'b00 #b0001))
(define sparc.be.a    (sparc-instruction 'a00 #b0001))
(define sparc.bg      (sparc-instruction 'b00 #b1010))
(define sparc.bg.a    (sparc-instruction 'a00 #b1010))
(define sparc.ble     (sparc-instruction 'b00 #b0010))
(define sparc.ble.a   (sparc-instruction 'a00 #b0010))
(define sparc.bge     (sparc-instruction 'b00 #b1011))
(define sparc.bge.a   (sparc-instruction 'a00 #b1011))
(define sparc.bl      (sparc-instruction 'b00 #b0011))
(define sparc.bl.a    (sparc-instruction 'a00 #b0011))
(define sparc.bgu     (sparc-instruction 'b00 #b1100))
(define sparc.bgu.a   (sparc-instruction 'a00 #b1100))
(define sparc.bleu    (sparc-instruction 'b00 #b0100))
(define sparc.bleu.a  (sparc-instruction 'a00 #b0100))
(define sparc.bcc     (sparc-instruction 'b00 #b1101))
(define sparc.bcc.a   (sparc-instruction 'a00 #b1101))
(define sparc.bcs     (sparc-instruction 'b00 #b0101))
(define sparc.bcs.a   (sparc-instruction 'a00 #b0101))
(define sparc.bpos    (sparc-instruction 'b00 #b1110))
(define sparc.bpos.a  (sparc-instruction 'a00 #b1110))
(define sparc.bneg    (sparc-instruction 'b00 #b0110))
(define sparc.bneg.a  (sparc-instruction 'a00 #b0110))
(define sparc.bvc     (sparc-instruction 'b00 #b1111))
(define sparc.bvc.a   (sparc-instruction 'a00 #b1111))
(define sparc.bvs     (sparc-instruction 'b00 #b0111))
(define sparc.bvs.a   (sparc-instruction 'a00 #b0111))
(define sparc.call    (sparc-instruction 'call))
(define sparc.jmplr   (sparc-instruction 'r10 #b111000 'jump))
(define sparc.jmpli   (sparc-instruction 'i10 #b111000 'jump))
(define sparc.nop     (sparc-instruction 'nop #b100))
(define sparc.ornr    (sparc-instruction 'r10 #b000110))
(define sparc.orni    (sparc-instruction 'i10 #b000110))
(define sparc.ornrcc  (sparc-instruction 'r10 #b010110))
(define sparc.ornicc  (sparc-instruction 'i10 #b010110))
(define sparc.andni   (sparc-instruction 'i10 #b000101))
(define sparc.andnr   (sparc-instruction 'r10 #b000101))
(define sparc.andnicc (sparc-instruction 'i10 #b010101))
(define sparc.andnrcc (sparc-instruction 'r10 #b010101))
(define sparc.rdy     (sparc-instruction 'r10 #b101000 'rdy))
(define sparc.wryr    (sparc-instruction 'r10 #b110000 'wry))
(define sparc.wryi    (sparc-instruction 'i10 #b110000 'wry))

; Strange instructions.

(define sparc.slot    (sparc-instruction 'slot))
(define sparc.label   (sparc-instruction 'label))

; Aliases.

(define sparc.bnz     sparc.bne)
(define sparc.bnz.a   sparc.bne.a)
(define sparc.bz      sparc.be)
(define sparc.bz.a    sparc.be.a)
(define sparc.bgeu    sparc.bcc)
(define sparc.bgeu.a  sparc.bcc.a)
(define sparc.blu     sparc.bcs)
(define sparc.blu.a   sparc.bcs.a)

; Abstractions.

(define (sparc.cmpr as r1 r2) (sparc.subrcc as r1 r2 $r.g0))
(define (sparc.cmpi as r imm) (sparc.subicc as r imm $r.g0))
(define (sparc.move as rs rd) (sparc.orr as $r.g0 rs rd))
(define (sparc.set as imm rd) (sparc.ori as $r.g0 imm rd))
(define (sparc.btsti as rs imm) (sparc.andicc as rs imm $r.g0))
(define (sparc.clr as rd) (sparc.move as $r.g0 rd))

(define (sparc.deccc as rs . rest)
  (let ((k (cond ((null? rest) 1)
		 ((null? (cdr rest)) (car rest))
		 (else (asm-error "sparc.deccc: too many operands: " rest)))))
    (sparc.subicc as rs k rs)))

; eof

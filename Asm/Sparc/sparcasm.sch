; Asm/Sparc/sparcasm.sch
; Larceny -- SPARC machine assembler
;
; $Id: sparcasm.sch,v 1.1 1997/07/07 20:36:24 lth Exp lth $
;
; The procedure `sparc-instruction' takes an instruction class keyword and
; some operands and returns an assembler procedure for that instruction.
;
; Assembler procedures for SPARC mnemonics are defined at the end 
; of this file.
;
; Each assembler procedure takes an `as' assembly structure (see 
; Asm/Common/pass5p1.sch) and operands relevant to the instruction, and
; side-effects the assembly structure by emitting bits for the instruction
; and any necessary fixups.  There are separate instruction mnemonics and
; assembler procedures for instructions which in the SPARC instruction set 
; are normally considered the "same".  For example, the `add' instruction is
; split into two operations here: `sparc.addr' takes a register as operand2,
; and `sparc.addi' takes an immediate.  We could remove this restriction
; by using magic constants (procedures, vectors) rather than numbers for
; registers, but it does not seem to be an important problem.
;
; Operands that denote values (addresses, immediates, offsets) may be
; expressed using symbolic expressions. These expressions must conform
; to the following grammar:
;
;   <expr> --> <symbol>                    ; label
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

(define sparc-instruction)

(let ()

  (define ibit (asm:bv 0 0 #x20 0))	; immediate bit: 2^13
  (define abit (asm:bv #x20 0 0 0))	; annul bit: 2^29

  ; Constant expression evaluation. If the expression cannot be 
  ; evaluated, eval-expr returns #f, otherwise a number.
  ; The symbol table lookup must fail by returning #f.

  (define (eval-expr as e)

    (define (hibits e) (if (not e) e (quotient e 1024)))
    (define (lobits e) (if (not e) e (remainder e 1024)))

    (define (evaluate e)
      (cond ((integer? e)      e)
	    ((symbol? e)       (label-value as e))
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

  ; Procedures that compute operand bits.

  (define (rs1-op r) (asm:lsh r 14))
  (define (rs2-op r) r)
  (define (rd-op r)  (asm:lsh r 25))
  (define (imm-op v) (asm:lobits v 13))

  ; Instruction manipulation

  (define (get-instr bv loc)
    (asm:bv (bytevector-ref bv loc)
	    (bytevector-ref bv (+ loc 1))
	    (bytevector-ref bv (+ loc 2))
	    (bytevector-ref bv (+ loc 3))))

  (define (set-instr! bv loc instr)
    (bytevector-set! bv (+ loc 3) (bytevector-ref instr 0))
    (bytevector-set! bv (+ loc 2) (bytevector-ref instr 1))
    (bytevector-set! bv (+ loc 1) (bytevector-ref instr 2))
    (bytevector-set! bv loc (bytevector-ref instr 3)))

  (define (display-instr instr)
    (do ((i 3 (- i 1)))
	((< i 0))
      (format #t "~a " (number->string (bytevector-ref instr i) 16))))

  ; Register a fixup procedure and return 0.

  (define (fixup-logior as proc)
    (emit-fixup-proc!
     as 
     (lambda (bv loc)
       (set-instr! bv loc (asm:logior (get-instr bv loc) (proc)))))
    0)

  ; For delay slot filling -- uses the assembler value scratchpad in
  ; the as structure.  Delay slot filling is discussed in the comments
  ; for `branch' and `class-slot', below.
  
  (define (remember-branch-target as obj)
    (assembler-value! as 'branch-target obj))

  (define (recover-branch-target as)
    (assembler-value as 'branch-target))

  ; Mark the instruction at the current address as not being eligible 
  ; for being lifted into a branch delay slot.

  (define (not-a-delay-slot-instruction as)
    (assembler-value! as 'not-dsi
		      (cons (here as)
			    (or (assembler-value as 'not-dsi) '()))))

  (define (is-a-delay-slot-instruction? as addr)
    (not (memv addr (assembler-value as 'not-dsi))))

  ; SETHI, etc.

  (define (class-sethi bits)
    (let ((bits (asm:lsh bits 22)))
      (lambda (as val rd)

	(define (expr)
	  (let ((v (eval-expr as val)))
	    (cond ((not v) v)
		  (else (asm:lobits v 22)))))

	(define (fixup)
	  (or (expr) (signal-error 'fixup "sethi" val)))

	(let ((e  (or (expr) (fixup-logior as fixup)))
	      (rd (rd-op rd)))
	  (emit-instr! as (asm:logior rd bits e))))))

  ; NOP is a peculiar sethi

  (define (class-nop i)
    (let ((instr (class-sethi i)))
      (lambda (as)
	(instr as 0 $r.g0))))

  ; Un-annulled branches.

  (define (class00b i) (branch i 0))

  ; Annulled branches.

  (define (class00a i) (branch i abit))

  ; Branches in general.  
  ; The `annul' parameter is either 0 or the value of the `abit' variable.
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
    (let ((bits (asm:lsh bits 25))
	  (code (asm:lsh #b010 22)))
      (lambda (as target0)
	(let ((target `(- ,target0 ,(here as))))

	  (define (expr)
	    (let ((e (eval-expr as target)))
	      (cond ((not e) e)
		    ((not (zero? (logand e 3)))
		     (signal-error 'unaligned "branch" target0))
		    ((asm:fits? e 24)
		     e)
		    (else
		     (asm-value-too-large as "branch" target e)))))

	  (define (fixup)
	    (let ((e (expr)))
	      (if e
		  (asm:lobits (asm:rsha e 2) 22)
		  (signal-error 'fixup "branch" target0))))

	  (let ((e (expr)))
	    (let ((offset (asm:lobits
			   (asm:rsha (or e (fixup-logior as fixup)) 2)
			   22))
		  (bt     (or e expr)))
	      (if (not (eqv? annul 0))
		  (remember-branch-target as bt)
		  (remember-branch-target as #f)) ; Clears the cache.
	      (not-a-delay-slot-instruction as)
	      (emit-instr! as (asm:logior annul bits code offset))))))))

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
  ;
  ; FIXME: check overflow of the branch offset field.

  (define (class-slot)
    (let ((nop-instr (class-nop #b100)))
      (lambda (as)
	; The branch target as remembered is the relative displacement from
	; the branch instruction itself.
	(let* ((branch-target
		(recover-branch-target as))
	       (fixup
		(lambda (bv loc)
		  (if (procedure? branch-target)
		      (set! branch-target (branch-target)))
		  (set! branch-target (+ branch-target (- loc 4)))
		  (if (is-a-delay-slot-instruction? as branch-target)
		      (begin
			(set-instr! bv loc (get-instr bv branch-target))
			(set-instr! bv
				    (- loc 4)
				    (asm:add (get-instr bv (- loc 4)) 1)))))))
	  (if (fill-delay-slots)
	      (emit-fixup-proc! as fixup))
	  (nop-instr as)))))

  ; ALU stuff, register operand. Also: jump.
  ; If 'extra' is non-null, it's a jump.

  (define (class10r bits . extra)
    (let ((bits  (asm:lsh bits 19))
	  (code  (asm:lsh #b10 30))
	  (jump? (not (null? extra))))
      (lambda (as rs1 rs2 rd)
	(let ((rs1 (rs1-op rs1))
	      (rs2 (rs2-op rs2))
	      (rd  (rd-op rd)))
	  (if jump?
	      (not-a-delay-slot-instruction as))
	  (emit-instr! as (asm:logior code rd bits rs1 rs2))))))

  ; ALU stuff, immediate operand. Also: jump.
  ; If 'extra' is non-null, it's a jump.

  (define (class10i bits  . extra)
    (let ((bits  (asm:lsh bits 19))
	  (code  (asm:lsh #b10 30))
	  (jump? (not (null? extra))))
      (lambda (as rs1 e rd)

	(define (expr)
	  (let ((imm (eval-expr as e)))
	    (cond ((not imm) imm)
		  ((asm:fits? imm 13) (imm-op imm))
		  (jump?
		   (asm-value-too-large as "`jmpli'" e imm))
		  (else
		   (asm-value-too-large as "ALU instruction" e imm)))))

	(define (fixup)
	  (or (expr) (signal-error 'fixup "ALU instruction" e)))

	(let ((imm (or (expr) (fixup-logior as fixup)))
	      (rs1 (rs1-op rs1))
	      (rd  (rd-op rd)))
	  (if jump?
	      (not-a-delay-slot-instruction as))
	  (emit-instr! as (asm:logior code rd bits rs1 ibit imm))))))

  ; Memory stuff, register operand.

  (define (class11r bits)
    (let ((bits (asm:lsh bits 19))
	  (code (asm:lsh #b11 30)))
      (lambda (as rs1 rs2 rd)
	(let ((rs1 (rs1-op rs1))
	      (rs2 (rs2-op rs2))
	      (rd  (rd-op rd)))
	  (emit-instr! as (asm:logior code rd bits rs1 rs2))))))

  ; Memory stuff, immediate operand.

  (define (class11i bits)
    (let ((bits (asm:lsh bits 19))
	  (code (asm:lsh #b11 30)))
      (lambda (as rs1 e rd)

	(define (expr)
	  (let ((imm (eval-expr as e)))
	    (cond ((not imm) imm)
		  ((asm:fits? imm 13) (imm-op imm))
		  (else 
		   (signal-error 'toolarge "Memory instruction" e imm)))))

	(define (fixup)
	  (or (expr) (signal-error 'fixup "Memory instruction" e)))

	(let ((imm (or (expr) (fixup-logior as fixup)))
	      (rs1 (rs1-op rs1))
	      (rd  (rd-op rd)))
	  (emit-instr! as (asm:logior code rd bits rs1 ibit imm))))))

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

	  (define (expr)
	    (let ((e (eval-expr as target)))
	      (if (not e)
		  e
		  (let ((e (asm:int->bv e)))
		    (if (zero? (logand (bytevector-ref e 0) 3))	; Alignment.
			(asm:rshl e 2)
			(signal-error 'unaligned "call" target0))))))

	  (define (fixup)
	    (or (expr) (signal-error 'fixup "call" target0)))

	  (let ((offset (or (expr)
			    (fixup-logior as fixup))))
	    (not-a-delay-slot-instruction as)
	    (emit-instr! as (asm:logior code offset)))))))

  (define (class-label)
    (lambda (as label)
      (emit-label! as label)))

  (define (class-smul adj ccs)
    (lambda (x cl)
      (asm-error "class-smul unimplemented -- call millicode instead.")))

  (define (class-sdiv adj ccs)
    (lambda (x cl)
      (asm-error "class-sdiv unimplemented -- call millicode instead.")))

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
	    ((smul)  (apply class-smul ops))
	    ((sdiv)  (apply class-sdiv ops))
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
(define sparc.smulr   (sparc-instruction 'smul 'r 'nocc))
(define sparc.smulrcc (sparc-instruction 'smul 'r 'cc))
(define sparc.smuli   (sparc-instruction 'smul 'i 'nocc))
(define sparc.smulicc (sparc-instruction 'smul 'i 'cc))
(define sparc.sdivr   (sparc-instruction 'sdiv 'r 'nocc))
(define sparc.sdivrcc (sparc-instruction 'sdiv 'r 'cc))
(define sparc.sdivi   (sparc-instruction 'sdiv 'i 'nocc))
(define sparc.sdivicc (sparc-instruction 'sdiv 'i 'cc))
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

(define (sparc.deccc as rs . rest)
  (let ((k (cond ((null? rest) 1)
		 ((null? (cdr rest)) (car rest))
		 (else (asm-error "sparc.deccc: too many operands: " rest)))))
    (sparc.subicc as rs k rs)))

; EOF

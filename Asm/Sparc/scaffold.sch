; Test code for new sparc assembler.
; This code tests the assembler's code generation by exhaustion.
; The idea is to assemble the same instruction sequences with the old
; assembler and compare them.
;
; It does not (yet) test such features as expression evaluation,
; label lookup, fixup insertion, and overflow handling.

(define xport #f)
(define (emit-instr! as instr)
  (display "#x" xport)
  (do ((i (- (bytevector-length instr) 1) (- i 1)))
      ((< i 0) #t)
    (display-byte (bytevector-ref instr i)))
  (newline xport))

(define (display-byte x)
  (display (string-ref "0123456789abcdef" (quotient x 16)) xport)
  (display (string-ref "0123456789abcdef" (remainder x 16)) xport))

(define (assembler-value! as tag val) #t)

(define (assembler-value as tag) #t)

(define (as-lc as) 0)

(define (as-lookup as e) 37)

(define (remember-branch-target as expr) #t)

(define (emit-fixup-proc! as proc)
  (display "FIXUP!" xport)
  (newline xport))

(define (emit-label! as label) 
  (display "LABEL! " xport)
  (display label xport)
  (newline xport))

(define (make-as) #t)

(define (header msg)
  (display msg xport)
  (newline xport)
  (flush-output-port xport))

(define (sparctest . rest)
  (call-with-output-file "sparctest.new"
    (lambda (p)
      (set! xport p)
      (let ((as (make-as)))
	(if (or (null? rest)
		(memq 'i rest))
	    (for-each (lambda (instr)
			(header (car instr))
			(test-i (cadr instr) as))
		      i-instructions))
	(if (or (null? rest)
		(memq 'r rest))
	    (for-each (lambda (instr)
			(header (car instr))
			(test-r (cadr instr) as))
		      r-instructions))
	(if (or (null? rest)
		(memq 'b rest))
	    (for-each (lambda (instr)
			(header (car instr))
			(test-branch (cadr instr) as))
		      branch-instructions))
	(if (or (null? rest)
		(memq 'm rest))
	    (begin (header "SETHI")
		   (test-sethi sparc.sethi as)
		   (header "CALL")
		   (test-call sparc.call as)
		   (header "NOP")
		   (test-nop sparc.nop as)
		   ; (test-slot sparc.slot as)
		   #t))))))

; Three-address arithmetic/jump with rs2

(define r-instructions
  `(("LDDR" ,sparc.lddr)
    ("LDR" ,sparc.ldr)
    ("LDHR" ,sparc.ldhr)
    ("LDBR" ,sparc.ldbr)
    ("LDDFR" ,sparc.lddfr)
    ("STDR" ,sparc.stdr)
    ("STR" ,sparc.str)
    ("STHR" ,sparc.sthr)
    ("STBR" ,sparc.stbr)
    ("STDFR" ,sparc.stdfr)
    ("ANDR" ,sparc.andr)
    ("ANDRCC" ,sparc.andrcc)
    ("ORR"  ,sparc.orr)
    ("ORRCC" ,sparc.orrcc)
    ("XORR" ,sparc.xorr)
    ("XORRCC" ,sparc.xorrcc)
    ("SLLR" ,sparc.sllr)
    ("SRLR" ,sparc.srlr)
    ("SRAR" ,sparc.srar)
    ("ADDR" ,sparc.addr)
    ("ADDRCC" ,sparc.addrcc)
    ("TADDRCC" ,sparc.taddrcc)
    ("SUBR" ,sparc.subr)
    ("SUBRCC" ,sparc.subrcc)
    ("TSUBRCC" ,sparc.tsubrcc)
;    ("SMULR" ,sparc.smulr)
;    ("SMULRCC" ,sparc.smulrcc)
;    ("SDIVR" ,sparc.sdivr)
;    ("SDIVRCC" ,sparc.sdivrcc)
    ("ORNR" ,sparc.ornr)
    ("ORNRCC" ,sparc.ornrcc)
    ("ANDNR" ,sparc.andnr)
    ("ANDNRCC" ,sparc.andnrcc)
    ("JMPLR" ,sparc.jmplr)))

; Three-address arithmetic/jump with immediate
(define i-instructions
  `(("LDDI" ,sparc.lddi)
    ("LDI" ,sparc.ldi)
    ("LDHI" ,sparc.ldhi)
    ("LDBI" ,sparc.ldbi)
    ("LDDFI" ,sparc.lddfi)
    ("STDI" ,sparc.stdi)
    ("STI" ,sparc.sti)
    ("STHI" ,sparc.sthi)
    ("STBI" ,sparc.stbi)
    ("STDFI" ,sparc.stdfi)
    ("ANDI" ,sparc.andi)
    ("ANDICC" ,sparc.andicc)
    ("ORI" ,sparc.ori)
    ("ORICC" ,sparc.oricc)
    ("XORI" ,sparc.xori)
    ("XORICC" ,sparc.xoricc)
    ("SLLI" ,sparc.slli)
    ("SRLI" ,sparc.srli)
    ("SRAI" ,sparc.srai)
    ("ADDI" ,sparc.addi)
    ("ADDICC" ,sparc.addicc)
    ("TADDICC" ,sparc.taddicc)
    ("SUBI" ,sparc.subi)
    ("SUBICC" ,sparc.subicc)
    ("TSUBICC" ,sparc.tsubicc)
;    ("SMULI" ,sparc.smuli)
;    ("SMULICC" ,sparc.smulicc)
;    ("SDIVI" ,sparc.sdivi)
;    ("SDIVICC" ,sparc.sdivicc)
    ("ORNI" ,sparc.orni)
    ("ORNICC" ,sparc.ornicc)
    ("ANDNI" ,sparc.andni)
    ("ANDNICC" ,sparc.andnicc)
    ("JMPLI" ,sparc.jmpli)))

; Pc-relative branches
(define branch-instructions
  `(("B" ,sparc.b)
    ("B.a" ,sparc.b.a)
    ("BNE" ,sparc.bne)
    ("BNE.a" ,sparc.bne.a)
    ("BE" ,sparc.be)
    ("BE.a" ,sparc.be.a)
    ("BG" ,sparc.bg)
    ("BG.a" ,sparc.bg.a)
    ("BLE" ,sparc.ble)
    ("BLE.a" ,sparc.ble.a)
    ("BGE" ,sparc.bge)
    ("BGE.a" ,sparc.bge.a)
    ("BL" ,sparc.bl)
    ("BL.a" ,sparc.bl.a)
    ("BGU" ,sparc.bgu)
    ("BGU.a" ,sparc.bgu.a)
    ("BLEU" ,sparc.bleu)
    ("BLEU.a" ,sparc.bleu.a)
    ("BCC" ,sparc.bcc)
    ("BCC.a" ,sparc.bcc.a)
    ("BCS" ,sparc.bcs)
    ("BCS.a" ,sparc.bcs.a)
    ("BPOS" ,sparc.bpos)
    ("BPOS.a" ,sparc.bpos.a)
    ("BNEG" ,sparc.bneg)
    ("BNEG.a" ,sparc.bneg.a)
    ("BVC" ,sparc.bvc)
    ("BVC.a" ,sparc.bvc.a)
    ("BVS" ,sparc.bvs)
    ("BVS.a" ,sparc.bvs.a)))

; We can't use all registers -- it'll take forever.  These are somewhat
; representative.

(define registers 
  (list $r.reg0 $r.reg1 $r.reg5 
	$r.result $r.argreg3 $r.timer $r.globals $r.tmp0
	$r.tmp1))

(define immediates
  (list 0 4 -10 1055 -2977 -4096 4095))

(define offsets
  (list 0 4 -8 1200 -4324 -8388608 8388604))

; the two large ones are actually the same value.

(define call-offsets
  (list 0 4 -8 1200 -4324 -8388608 8388604 
	(- (expt 2 32) 4) 
	(+ (- (expt 2 32)) 4)))

(define (test-i instr as)
  (for-each (lambda (rs1)
	      (for-each (lambda (imm)
			  (for-each (lambda (rd)
				      (instr as rs1 imm rd))
				    registers))
			immediates))
	    registers))

(define (test-r instr as)
  (for-each (lambda (rs1)
	      (for-each (lambda (rs2)
			  (for-each (lambda (rd)
				      (instr as rs1 rs2 rd))
				    registers))
			registers))
	    registers))

(define (test-branch instr as)
  (for-each (lambda (offset)
	      (instr as offset))
	    offsets))

(define (test-sethi instr as)
  (for-each (lambda (value)
	      (for-each (lambda (rd)
			  (instr as value rd))
			registers))
	    (map (lambda (x) (quotient x 4)) offsets)))

(define (test-call instr as)
  (for-each (lambda (offset)
	      (instr as offset))
	    call-offsets))

(define (test-nop instr as)
  (instr as))

; eof

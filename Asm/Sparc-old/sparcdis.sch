; -*- Scheme -*-
;
; Scheme 313 compiler
; Disassembler for SPARC

; Takes a bytevector (the code vector) and returns a list of symbolic
; Sparc assembly language instructions using symbolic register names,
; millicode table offsets, and global table offsets.

(define disassemble-codevector

  (let ()

    ; Useful constants

    (define two^3 (expt 2 3))
    (define two^5 (expt 2 5))
    (define two^6 (expt 2 6))
    (define two^8 (expt 2 8))
    (define two^12 (expt 2 12))
    (define two^13 (expt 2 13))
    (define two^14 (expt 2 14))
    (define two^16 (expt 2 16))
    (define two^19 (expt 2 19))
    (define two^21 (expt 2 21))
    (define two^22 (expt 2 22))
    (define two^24 (expt 2 24))
    (define two^25 (expt 2 25))
    (define two^29 (expt 2 29))
    (define two^30 (expt 2 30))
    (define two^32 (expt 2 32))

    ; Class 0 has branches and weirdness, like sethi and nop.
    ; We dispatch first on the op2 field and then on the op3 field.

    (define class00
      (let ((b-table
	     (vector 0                ; $i.bn
		     $i.be
		     $i.ble
		     $i.bl
		     $i.bleu
		     $i.bcs
		     $i.bneg
		     $i.bvs
		     $i.b
		     $i.bne
		     $i.bg
		     $i.bge
		     $i.bgu
		     $i.bcc
		     $i.bpos
		     $i.bvc
	             0                ; $i.bn.a
		     $i.be.a
		     $i.ble.a
		     $i.bl.a
		     $i.bleu.a
		     $i.bcs.a
		     $i.bneg.a
		     $i.bvs.a
		     $i.b.a
		     $i.bne.a
		     $i.bg.a
		     $i.bge.a
		     $i.bgu.a
		     $i.bcc.a
		     $i.bpos.a
		     $i.bvc.a)))

	(lambda (ip instr)
	  (let ((op2 (op2field instr)))
	    (cond ((= op2 #b100)
		   (if (zero? (rdfield instr))
		       `(,$i.nop)
		       `(,$i.sethi ,(imm22field instr) ,(rdfield instr))))
		  ((= op2 #b010)
		   `(,(vector-ref b-table (rdfield instr))
		     ,(* 4 (imm22field instr))))
		  (else
		   (error 'class00
			  "Can't disassemble ~a at ip ~a with op2 ~a"
			  instr ip op2)))))))

    ; Class 1 is the call instruction; there's no choice.

    (define (class01 ip instr)
      `(,$i.call ,(* 4 (imm30field instr))))

    ; Class 2 is for the ALU. Dispatch on op3 field.

    (define class10
      (let ((op3-table
	     `#((,$i.addr   ,$i.addi)
		(,$i.andr   ,$i.andi)
		(,$i.orr    ,$i.ori)
		(,$i.xorr   ,$i.xori)
		(,$i.subr   ,$i.subi)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(,$i.smulr  ,$i.smuli)
		(0          0)
		(0          0)
		(0          0)
		(,$i.sdivr  ,$i.sdivi)
		(,$i.addrcc ,$i.addicc)
		(,$i.andrcc ,$i.andicc)
		(,$i.orrcc  ,$i.oricc)
		(,$i.xorrcc ,$i.xoricc)
		(,$i.subrcc ,$i.subicc)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(,$i.smulrcc ,$i.smulicc)
		(0          0)
		(0          0)
		(0          0)
		(,$i.sdivrcc ,$i.sdivicc)
		(,$i.taddrcc ,$i.taddicc)
		(,$i.tsubrcc ,$i.tsubicc)
		(0          0)
		(0          0)
		(0          0)
		(,$i.sllr   ,$i.slli)
		(,$i.srlr   ,$i.srli)
		(,$i.srar   ,$i.srai)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(,$i.jmplr  ,$i.jmpli)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0))))

	(lambda (ip instr)
	  (nice-instruction op3-table ip instr))))


    ; Class 3 is memory stuff.

    (define class11
      (let ((op3-table
	     `#((,$i.ldr    ,$i.ldi)
		(,$i.ldbr   ,$i.ldbi)
		(,$i.ldhr   ,$i.ldhi)
		(,$i.lddr   ,$i.lddi)
		(,$i.str    ,$i.sti)
		(,$i.stbr   ,$i.stbi)
		(,$i.sthr   ,$i.sthi)
		(,$i.stdr   ,$i.stdi)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(,$i.lddfr  ,$i.lddfi)
		(0          0)
		(0          0)
		(0          0)
		(,$i.stdfr  ,$i.stdfi)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0)
		(0          0))))

	(lambda (ip instr)
	  (nice-instruction op3-table ip instr))))

    ; For classes 2 and 3

    ; Ugly hack, but stores require twiddling the operands.

    (define store?
      (let ((slist `(,$i.str ,$i.sti ,$i.stbr ,$i.stbi ,$i.sthr ,$i.sthi
			    ,$i.stdr ,$i.stdi ,$i.stdfr ,$i.stdfi)))
	(lambda (i)
	  (not (null? (memv i slist))))))

    (define (nice-instruction op3-table ip instr)
      (let* ((op3  (op3field instr))
	     (imm  (ifield instr))
	     (rd   (rdfield instr))
	     (rs1  (rs1field instr))
	     (src2 (if (zero? imm)
		       (rs2field instr)
		       (imm13field instr))))
	(let ((op ((if (zero? imm) car cadr) (vector-ref op3-table op3))))
	  (if (store? op)
	      `(,op ,rd  ,src2 ,rs1)
	      `(,op ,rs1 ,src2 ,rd)))))

    ; The following procedures pick apart an instruction

    (define (op2field instr)
      (remainder (quotient instr two^22) two^3))

    (define (op3field instr)
      (remainder (quotient instr two^19) two^6))

    (define (ifield instr)
      (remainder (quotient instr two^13) 2))

    (define (rs2field instr)
      (remainder instr two^5))

    (define (rs1field instr)
      (remainder (quotient instr two^14) two^5))

    (define (rdfield instr)
      (remainder (quotient instr two^25) two^5))

    (define (imm13field instr)
      (let ((x (remainder instr two^13)))
	(if (not (zero? (quotient x two^12)))
	    (- x two^13)
	    x)))
	
    (define (imm22field instr)
      (let ((x (remainder instr two^22)))
	(if (not (zero? (quotient x two^21)))
	    (- x two^22)
	    x)))

    (define (imm30field instr)
      (let ((x (remainder instr two^30)))
	(if (not (zero? (quotient x two^29)))
	    (- x two^30)
	    x)))

    ; Step through the codevector and disassemble each instruction in turn.

    (define (disassemble code)

      ; Sparc instructions come in four classes based on the two high
      ; bits of the instruction.

      (define class-table (vector class00 class01 class10 class11))

      (let ((l (bytevector-length code)))
	(let loop ((i 0) (ilist '()))
	  (if (< i l)
	      (let ((instr (+ (* (bytevector-ref code i) two^24)
			      (* (bytevector-ref code (+ i 1)) two^16)
			      (* (bytevector-ref code (+ i 2)) two^8)
			      (bytevector-ref code (+ i 3))))
		    (idx   (quotient (bytevector-ref code i) 64)))
		(if (not (<= 0 idx 3))
		    (error "shit happens." instr idx i 
			   (bytevector-ref code i)
			   (bytevector-ref code (+ i 1))
			   (bytevector-ref code (+ i 2))
			   (bytevector-ref code (+ i 3))
			   ))
		(loop (+ i 4)
		      (cons ((vector-ref class-table idx) i instr) ilist)))
	      (reverse ilist)))))

    disassemble))


;-----------------------------------------------------------
; Print a list of { disassembled, unassembled } instructions
;
; It blatantly assumes that the first instruction is at address 0, and prints
; addresses (and relative addresses) given that.

(define print-instructions

  (let ()

    (define rtable
      '#($r.g0 $r.tmp0 $r.g2 $r.g3 $r.g4 $r.g5 $r.g6 $r.g7
         $r.result $r.argreg2 $r.argreg3 $r.stkp $r.tmp1 $r.tmp2 $r.o6 $r.o7
	 $r.reg0 $r.reg1 $r.reg2 $r.reg3 $r.reg4 $r.reg5 $r.reg6 $r.reg7
	 $r.e-top $r.e-limit $r.i2 $r.i3 $r.timer $r.millicode $r.i6 $r.globals))

    (define itable
      (let* ((freg (lambda (r a)
		     (display "$f.")
		     (display r)))
	     (reg (lambda (r a)
		    (display (vector-ref rtable r))))
	     (imm (lambda (i a)
		    (display i)))
	     (bimm (lambda (i a)
		     (if (symbol? i)
			 (display i)
			 (display (+ i a)))))
	     (rir `(,reg ,imm ,reg))
	     (rrr `(,reg ,reg ,reg))
	     (v   (make-vector 120 '()))
	     (defi (lambda (i x l)
		     (vector-set! v i (cons x l)))))
	  
	  (defi $i.lddi 'lddi    rir)
	  (defi $i.ldi 'ldi     rir)
	  (defi $i.addi 'addi    rir)
	  (defi $i.ldhi 'ldhi    rir)
	  (defi $i.ldbi 'ldbi    rir)
	  (defi $i.lddfi 'lddfi   `(,reg ,imm ,freg))
	  (defi $i.stdi 'stdi   rir)
	  (defi $i.sti 'sti     rir)
	  (defi $i.sthi 'sthi    rir)
	  (defi $i.stbi 'stbi    rir)
	  (defi $i.stdfi 'stdfi   `(,freg ,imm ,reg))
	  (defi $i.sethi 'sethi   `(,(lambda (i a) (display (* i 1024))) ,reg))
	  (defi $i.andr 'andr    rrr)
	  (defi $i.andrcc 'andrcc  rrr)
	  (defi $i.andi 'andi    rir)
	  (defi $i.andicc 'andicc  rir)
	  (defi $i.orr 'orr     rrr)
	  (defi $i.orrcc 'orrcc   rrr)
	  (defi $i.ori 'ori     rir)
	  (defi $i.oricc 'oricc   rir)
	  (defi $i.xorr 'xorr    rrr)
	  (defi $i.xorrcc 'xorrcc  rrr)
	  (defi $i.xori 'xori    rir)
	  (defi $i.xoricc 'xoricc  rir)
	  (defi $i.sllr 'sllr    rrr)
	  (defi $i.slli 'slli    rir)
	  (defi $i.srlr 'srlr    rrr)
	  (defi $i.srli 'srli    rir)
	  (defi $i.srar 'srar    rrr)
	  (defi $i.srai 'srai    rir)
	  (defi $i.addr 'addr    rrr)
	  (defi $i.addrcc 'addrcc  rrr)
	  (defi $i.addicc 'addicc  rir)
	  (defi $i.taddrcc 'taddrcc rrr)
	  (defi $i.taddicc 'taddicc rir)
	  (defi $i.subr 'subr    rrr)
	  (defi $i.subrcc 'subrcc  rrr)
	  (defi $i.subi 'subi    rir)
	  (defi $i.subicc 'subicc  rir)
	  (defi $i.tsubrcc 'tsubrcc rrr)
	  (defi $i.tsubicc 'tsubicc rir)
	  (defi $i.smulr 'smulr   rrr)
	  (defi $i.smulrcc 'smulrcc rrr)
	  (defi $i.smuli 'smuli   rir)
	  (defi $i.smulicc 'smulicc rir)
	  (defi $i.sdivr 'sdivr   rrr)
	  (defi $i.sdivrcc 'sdivrcc rrr)
	  (defi $i.sdivi 'sdivi   rir)
	  (defi $i.sdivicc 'sdivicc rir)
	  (defi $i.b 'b	 `(,bimm))
	  (defi $i.b.a 'b.a	 `(,bimm))
	  (defi $i.bne 'bne	 `(,bimm))
	  (defi $i.bne.a 'bne.a   `(,bimm))
	  (defi $i.be 'be      `(,bimm))
	  (defi $i.be.a 'be.a    `(,bimm))
	  (defi $i.bg 'bg      `(,bimm))
	  (defi $i.bg.a 'bg.a    `(,bimm))
	  (defi $i.ble 'ble     `(,bimm))
	  (defi $i.ble.a 'ble.a   `(,bimm))
	  (defi $i.bge 'bge     `(,bimm))
	  (defi $i.bge.a 'bge.a   `(,bimm))
	  (defi $i.bl 'bl      `(,bimm))
	  (defi $i.bl.a 'bl.a    `(,bimm))
	  (defi $i.bgu 'bgu     `(,bimm))
	  (defi $i.bgu.a 'bgu.a   `(,bimm))
	  (defi $i.bleu 'bleu    `(,bimm))
	  (defi $i.bleu.a 'bleu.a  `(,bimm))
	  (defi $i.bcc 'bcc     `(,bimm))
	  (defi $i.bcc.a 'bcc.a   `(,bimm))
	  (defi $i.bcs 'bcs     `(,bimm))
	  (defi $i.bcs.a 'bcs.a   `(,bimm))
	  (defi $i.bpos 'bpos    `(,bimm))
	  (defi $i.bpos.a 'bpos.a  `(,bimm))
	  (defi $i.bneg 'bneg    `(,bimm))
	  (defi $i.bneg.a 'bneg.a  `(,bimm))
	  (defi $i.bvc 'bvc     `(,bimm))
	  (defi $i.bvc.a 'bvc.a   `(,bimm))
	  (defi $i.bvs 'bvs     `(,bimm))
	  (defi $i.bvs.a 'bvs.a   `(,bimm))
	  (defi $i.call 'call    `(,bimm))
	  (defi $i.jmplr 'jmplr   rrr)
	  (defi $i.jmpli 'jmpli   rir)
	  (defi $i.label 'label    '())
	  (defi $i.nop 'nop      '())
	  (defi $i.lddr 'lddr     rrr)
	  (defi $i.ldr 'ldr      rrr)
	  (defi $i.ldhr 'ldhr     rrr)
	  (defi $i.ldbr 'ldbr     rrr)
	  (defi $i.lddfr 'lddfr     rrr)
	  (defi $i.stdr 'stdr     rrr)
	  (defi $i.str 'str      rrr)
	  (defi $i.sthr 'sthr     rrr)
	  (defi $i.stbr 'stbr     rrr)
	  (defi $i.stdfr 'stfr     rrr)
	  (defi $i.slot 'slot      '())
	  v))
    
    (define (print-i i a)
      (if (= (car i) $i.label)
	  (begin (display (cadr i))
		 (display ": ")
		 (display "(")
		 (display a)
		 (display ")"))
	  (begin (display a)
		 (display #\tab)
		 (display (car (vector-ref itable (car i))))
		 (display #\tab)
		 (map (lambda (x y)
			(display " ")
			(x y a))
		      (cdr (vector-ref itable (car i)))
		      (cdr i))))
      (newline))

    (define (print-ilist ilist a)
      (if (null? ilist)
	  '()
	  (begin (print-i (car ilist) a)
		 (print-ilist (cdr ilist) (+ a 4)))))
    
    (lambda (ilist)
      (print-ilist ilist 0))))

; eof

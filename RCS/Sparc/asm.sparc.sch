; -*- Scheme -*-
;
; Scheme 313 compiler (SPARC)
; Machine-dependent part of the assembler.
; Machine-dependent code generation procedures.
;
; $Id: asm.sparc.scm,v 1.1 91/08/09 18:05:30 lth Exp Locker: lth $
;
; BUGS: The assembler must be able to handle simple expressions in label
;       fields for the call instruction (at least); it does not do that yet.

; Table of core Sparc instruction set. Instructions not used by the 
; compiler have been left out.
;
; Instructions are listed in the order found in the architecture manual,
; appendix B.

; largest symbolic opcode value + 1.
    
(define opcode-table-size 110)

; symbolic opcodes for sparc.

(define $i.lddi      0)
(define $i.lddr      100)
(define $i.ldwi      1)
(define $i.ldwr      101)
(define $i.ldhi      3)
(define $i.ldhr      102)
(define $i.ldbi      4)
(define $i.ldbr      103)
(define $i.lddfi     5)
(define $i.lddfr     104)
(define $i.stddi     6)
(define $i.stddr     105)
(define $i.stdwi     7)
(define $i.stdwr     106)
(define $i.stdhi     8)
(define $i.stdhr     107)
(define $i.stdbi     9)
(define $i.stdbr     108)
(define $i.stdfi     10)
(define $i.stdfr     109)
(define $i.sethi     11)
(define $i.andr	     12)
(define $i.andrcc    13)
(define $i.andi	     14)
(define $i.andicc    15)
(define $i.orr	     16)
(define $i.orrcc     17)
(define $i.ori	     18)
(define $i.oricc     19)
(define $i.xorr	     20)
(define $i.xorrcc    21)
(define $i.xori	     22)
(define $i.xoricc    23)
(define $i.sllr	     24)
(define $i.slli	     25)
(define $i.srlr	     26)
(define $i.srli	     27)
(define $i.srar	     28)
(define $i.srai	     29)
(define $i.addr	     30)
(define $i.addrcc    31)
(define $i.addi	     31)
(define $i.addicc    32)
(define $i.taddrcc   33)
(define $i.taddicc   34)
(define $i.subr	     35)
(define $i.subrcc    36)
(define $i.subi	     37)
(define $i.subicc    38)
(define $i.tsubrcc   39)
(define $i.tsubicc   40)
(define $i.smulr     41)
(define $i.smulrcc   42)
(define $i.smuli     43)
(define $i.smulicc   44)
(define $i.sdivr     45)
(define $i.sdivrcc   46)
(define $i.sdivi     47)
(define $i.sdivicc   48)
(define $i.b	     49)
(define $i.b.a	     50)
(define $i.bne	     51)
(define $i.bne.a     52)
(define $i.be	     53)
(define $i.be.a	     54)
(define $i.bg	     55)
(define $i.bg.a	     56)
(define $i.ble	     57)
(define $i.ble.a     58)
(define $i.bge	     59)
(define $i.bge.a     60)
(define $i.bl	     61)
(define $i.bl.a	     62)
(define $i.bgu	     63)
(define $i.bgu.a     64)
(define $i.bleu	     65)
(define $i.bleu.a    66)
(define $i.bcc	     67)
(define $i.bcc.a     68)
(define $i.bcs	     69)
(define $i.bcs.a     70)
(define $i.bpos	     71)
(define $i.bpos.a    72)
(define $i.bneg	     73)
(define $i.bneg.a    74)
(define $i.bvc	     75)
(define $i.bvc.a     76)
(define $i.bvs	     77)
(define $i.bvs.a     78)
(define $i.call	     79)
(define $i.jmplr     80)
(define $i.jmpli     81)
(define $i.label     82)          ; a label
(define $i.nop       83)

; Register definitions. These are fixed!

(define $r.g0 0)
(define $r.g1 1)
(define $r.i0 24)
(define $r.i1 25)
(define $r.i2 26)
(define $r.i3 27)
(define $r.i4 28)
(define $r.i5 29)
(define $r.i6 30)
(define $r.i7 31)
(define $r.l0 16)
(define $r.l1 17)
(define $r.l2 18)
(define $r.l3 19)
(define $r.l4 20)
(define $r.l5 21)
(define $r.l6 22)
(define $r.l7 23)
(define $r.o0 8)
(define $r.o1 9)
(define $r.o2 10)
(define $r.o3 11)
(define $r.o4 12)
(define $r.o5 13)
(define $r.o6 14)
(define $r.o7 15)

; Register mapping for hardware-mapped registers; offsets into `globals'
; for memory-mapped registers. These had better correspond to the ones
; in "registers.s.h"!

(define $r.tmp0      $r.g1)
(define $r.tmp1      $r.i0)
(define $r.tmp2      $r.i1)
(define $r.reg0      $r.l0)
(define $r.reg1      $r.l1)
(define $r.reg2      $r.l2)
(define $r.reg3      $r.l3)
(define $r.reg4      $r.l4)
(define $r.reg5      $r.l5)
(define $r.reg6      $r.l6)
(define $r.reg7      $r.l7)
(define $r.result    $r.o0)
(define $r.argreg2   $r.o1)
(define $r.argreg3   $r.o2)
(define $r.stkp      $r.o3)
(define $r.globals   $r.i7)
(define $r.millicode $r.i5)
(define $r.e-top     $r.o4)
(define $r.e-limit   $r.o5)

; Register mappings for memory-mapped registers: these are byte offsets
; from the location of globals[ REG0_OFFSET ].

(define $r.reg8   32)
(define $r.reg9   36)
(define $r.reg10  40)
(define $r.reg11  44)
(define $r.reg12  48)
(define $r.reg13  52)
(define $r.reg14  56)
(define $r.reg15  60)
(define $r.reg16  64)
(define $r.reg17  68)
(define $r.reg18  72)
(define $r.reg19  76)
(define $r.reg20  80)
(define $r.reg21  84)
(define $r.reg22  88)
(define $r.reg23  92)
(define $r.reg24  96)
(define $r.reg25  100)
(define $r.reg26  104)
(define $r.reg27  108)
(define $r.reg28  112)
(define $r.reg29  116)
(define $r.reg30  120)
(define $r.reg31  124)

; Various immediate quantities (low byte).
; Must correspond to the ones documented in "gcinterface.txt" and defined
; in "layouts.h" and "layouts.s.h".

(define $imm.true              0)
(define $imm.false             0)
(define $imm.null              0)
(define $imm.unspecified       0)
(define $imm.eof               0)
(define $imm.character         0)
(define $imm.vector-header     0)
(define $imm.bytevector-header 0)
(define $imm.procedure-header  0)


; Bytevector 'xxx' fields, pre-shifted. 
; Must correspond to the ones in "layouts.s.h".

(define $tag.bytevec-hdrtag    0)
(define $tag.string-hdrtag     0)
(define $tag.flonum-hdrtag     0)
(define $tag.compnum-hdrtag    0)
(define $tag.bignum-hdrtag     0)


; Vector 'xxx' fields, pre-shifted.
; Ditto.

(define $tag.vector-hdrtag     0)
(define $tag.rectnum-hdrtag    0)
(define $tag.ratnum-hdrtag     0)


; Pointer tags.

(define $tag.pair              0)
(define $tag.procedure         0)
(define $tag.vector            0)
(define $tag.bytevector        0)

; Millicode indices

(define $m.timer-exception     0)     ; timer expired
(define $m.proc-exception      0)     ; non-procedure in call
(define $m.arg-exception       0)     ; wrong # of arguments
(define $m.varargs             0)     ; handle varargs
(define $m.stkoflow            0)     ; stack cache overflow
(define $m.apply               0)     ; handle the 'apply' instruction
(define $m.alloc               0)     ; allocate memory
(define $m.alloci              0)     ; ditto with initialization
(define $m.zero?               0)     ; generic zero?
(define $m.numeq               0)     ; generic =
(define $m.numlt               0)     ; generic <
(define $m.numgt               0)     ; generic >
(define $m.numle               0)     ; generic <=
(define $m.numge               0)     ; generic >=
(define $m.add                 0)     ; generic +
(define $m.subtract            0)     ; generic -
(define $m.type-exception      0)     ; generic type exception (bletch)

; Various offsets into various structures.

(define $o.reg0        0)     ; byte offset of REG0 in globals[] table

; these are adjusted for a procedure tag.

(define $o.codevector  (- 4 $tag.procedure)) ; offs of code vec in proc struct
(define $o.constvector (- 8 $tag.procedure)) ; offs of const vec in proc struct


; Given an integer code for a register, return its register label.

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

(define (hardware-mapped? r)
  (or (and (>= r $r.reg0) (<= r $r.reg7))
      (eq? r $r.result)
      (eq? r $r.tmp0)
      (eq? r $r.tmp1)
      (eq? r $r.tmp2)))

; Return the offset in the %GLOBALS table of the given memory-mapped register.

(define (offsetof r)
  (+ $o.reg0 r))


; Takes a code list (list of Sparc assembly instructions in our 
; Special Format) and returns a bytevector with the assembled code.
;
; The format of an instruction in the code list is
;
;    (op opd ...)
;
; where 'op' is the numeric code for an opcode (see the opcode table above)
; and the operands are either register numbers (see the register tables above)
; or literals; which it is, is given away by the opcode.

(define assemble-codevector

  (let ()

    ; Current pc during assembly (relative to start of code vector). It needs 
    ; to be global since it is shared between 

    (define fptr 0)

    ;-------------------------------------------------------------------------
    ; Symbol Table Stuff
    ;
    ; No error checking here; this is not intended to process human-created
    ; stuff. The compiler ought to know what it's doing.

    (define symtab '())
    
    ; new symbol table

    (define (symtab.clear!)
      (set! symtab '()))

    ; define a label corresponding to the current pc.

    (define (symtab.define-label! label)
      (let ((x (assq label symtab)))
	(if (not x)
	    (set! symtab (cons (cons label fptr) symtab)))))

    (define (symtab.lookup l)
      (let ((x (assq label symtab)))
	(if x
	    (cdr x)
	    0)))

    (define (resolve-label l)
      (if (symbol? l)
	  (- (symtab.lookup l) fptr)
	  l))


    ;------------------------------------------------------------------------
    ; Bit Operations are not necessarily very pleasant in Scheme...
    
    ; exponent table

    (define etable 
      (let ((v (make-vector 32 0)))
	(let loop ((i 0) (j 1))
	  (if (< i 32)
	      (begin (vector-set! v i j)
		     (loop (+ i 1) (* j 2)))))))

    (define (shl m places)
      (* m (vector-ref etable places)))

    (define (lobits m bits)
      (remainder m (vector-ref etable bits))) ; maybe iffy if m < 0?

    (define (hibits m bits)
      (quotient m (vector-ref etable (- 32 bits))))


      
    ; The instruction table.

    (define itable

      (let ()

	; sethi, etc.

	(define (class-sethi i)
	  (let ((i (shl i 21)))
	    (lambda (x)
	      (let ((n (instruction.arg1 x))
		    (rd (instruction.arg2 x)))
		(+ (shl rd 24) i n )))))

	; nop is an alias for sethi

	(define (class-nop i)
	  (let ((q (class-sethi i)))
	    (lambda (x)
	      (q '(dummy 0 ,$r.g0)))))

	; un-annulled branches

	(define (class00b i)
	  (let ((i (shl i 24)))
	    (lambda (x)
	      (let ((offset (resolve-label (instruction.arg1 x))))
		(+ i (shl #b010 21) (lobits offset 22))))))
      
	; annuled branches

	(define (class00a i)
	  (let ((i (shl i 24)))
	    (lambda (x)
	      (let ((offset (resolve-label (instruction.arg1 x))))
		(+ (shl #b1 28) i (shl #b010 21) (lobits offset 22))))))
  
	; alu stuff and some others

	(define (class10r i)
	  (let ((i (shl i 18)))
	    (lambda (x)
	      (let ((rs1 (instruction.arg1 x))
		    (rs2 (instruction.arg2 x))
		    (rd  (instruction.arg3 x)))
		(+ #x80000000 (shl rd 24) i (shl rs1 13) rs2)))))

	; ditto

	(define (class10i i)
	  (let ((i (shl i 18)))
	    (lambda (x)
	      (let ((rs1 (instruction.arg1 x))
		    (imm (instruction.arg2 x))
		    (rd  (instruction.arg3 x)))
		(+ #x80000000 (shl rd 24) i (shl rs1 13) #x2000 imm)))))

	; memory stuff

	(define (class11r i)
	  (let ((i (shl i 18)))
	    (lambda (x)
	      (let ((rs1 (instruction.arg1 x))
		    (rs2 (instruction.arg2 x))
		    (rd  (instruction.arg3 x)))
		(+ #xC0000000 (shl rd 24) i (shl rs1 13) rs2)))))

	; ditto

	(define (class11i i)
	  (let ((i (shl i 18)))
	    (lambda (x)
	      (let ((rs1 (instruction.arg1 x))
		    (imm (instruction.arg2 x))
		    (rd  (instruction.arg3 x)))
		(+ #xC0000000 (shl rd 24) i (shl rs1 13) #x2000 imm)))))

	; call is a class all by itself

	(define (class-call)
	  (lambda (x)
	    (let ((offset (instruction.arg1 x)))
	      (+ #x40000000 offset))))

	(define (class-label)
	  (lambda (x)
	    (symtab.define-label! (instruction.arg1 x))))

	; Multiplication and division are weird, since we want to call 
	; library  routines on all current implementations of the 
	; architecture.

	(define (class-smul adj ccs)
	  (lambda (x) (error 'class-smul
			     "Unimplemented -- call millicode instead.")))

	(define (class-sdiv adj ccs)
	  (lambda (x) (error 'class-sdiv
			     "Unimplemented -- call millicode instead.")))

	; make the opcode vector

	(let ((v (make-vector opcode-count)))
	  (vector-set! $i.lddi    (class11i #b000011))
	  (vector-set! $i.lddr    (class11r #b000011))
	  (vector-set! $i.ldwi    (class11i #b000000))
	  (vector-set! $i.ldwr    (class11r #b000000))
	  (vector-set! $i.ldhi    (class11i #b000010))
	  (vector-set! $i.ldhr    (class11r #b000010))
	  (vector-set! $i.ldbi    (class11i #b000001))
	  (vector-set! $i.ldbr    (class11r #b000001))
	  (vector-set! $i.lddfi   (class11i #b100001))
	  (vector-set! $i.lddfr   (class11r #b100001))
	  (vector-set! $i.stddi   (class11i #b000111))
	  (vector-set! $i.stddr   (class11r #b000111))
	  (vector-set! $i.stdwi   (class11i #b000100))
	  (vector-set! $i.stdwr   (class11r #b000100))
	  (vector-set! $i.stdhi   (class11i #b000110))
	  (vector-set! $i.stdhr   (class11r #b000110))
	  (vector-set! $i.stdbi   (class11i #b000101))
	  (vector-set! $i.stdbr   (class11r #b000101))
	  (vector-set! $i.stdfi   (class11i #b100111))
	  (vector-set! $i.stdfr   (class11r #b100111))
	  (vector-set! $i.sethi   (class-sethi #b100))
	  (vector-set! $i.andr    (class10r #b000001))
	  (vector-set! $i.andrcc  (class10r #b010001))
	  (vector-set! $i.andi    (class10i #b000001))
	  (vector-set! $i.andicc  (class10i #b010001))
	  (vector-set! $i.orr     (class10r #b000010))
	  (vector-set! $i.orrcc   (class10r #b010010))
	  (vector-set! $i.ori     (class10i #b000010))
	  (vector-set! $i.oricc   (class10i #b010010))
	  (vector-set! $i.xorr    (class10r #b000011))
	  (vector-set! $i.xorrcc  (class10r #b010011))
	  (vector-set! $i.xori    (class10i #b000011))
	  (vector-set! $i.xoricc  (class10i #b010011))
	  (vector-set! $i.sllr    (class10r #b100101))
	  (vector-set! $i.slli    (class10i #b100101))
	  (vector-set! $i.srlr    (class10r #b100110))
	  (vector-set! $i.srli    (class10i #b100110))
	  (vector-set! $i.srar    (class10r #b100111))
	  (vector-set! $i.srai    (class10i #b100111))
	  (vector-set! $i.addr    (class10r #b000000))
	  (vector-set! $i.addrcc  (class10r #b010000))
	  (vector-set! $i.addi    (class10i #b000000))
	  (vector-set! $i.addicc  (class10i #b010000))
	  (vector-set! $i.taddrcc (class10r #b100000))
	  (vector-set! $i.taddicc (class10i #b100000))
	  (vector-set! $i.subr    (class10r #b000100))
	  (vector-set! $i.subrcc  (class10r #b010100))
	  (vector-set! $i.subi    (class10i #b000100))
	  (vector-set! $i.subicc  (class10i #b010100))
	  (vector-set! $i.tsubrcc (class10r #b100001))
	  (vector-set! $i.tsubicc (class10i #b100001))
	  (vector-set! $i.smulr   (class-smul 'r 'nocc))
	  (vector-set! $i.smulrcc (class-smul 'r 'cc))
	  (vector-set! $i.smuli   (class-smul 'i 'nocc))
	  (vector-set! $i.smulicc (class-smul 'i 'cc))
	  (vector-set! $i.sdivr   (class-sdiv 'r 'nocc))
	  (vector-set! $i.sdivrcc (class-sdiv 'r 'cc))
	  (vector-set! $i.sdivi   (class-sdiv 'i 'nocc))
	  (vector-set! $i.sdivicc (class-sdiv 'i 'cc))
	  (vector-set! $i.b       (class00b #b1000))
	  (vector-set! $i.b.a     (class00a #b1000))
	  (vector-set! $i.bne     (class00b #b1001))
	  (vector-set! $i.bne.a   (class00a #b1001))
	  (vector-set! $i.be      (class00b #b0001))
	  (vector-set! $i.be.a    (class00a #b0001))
	  (vector-set! $i.bg      (class00b #b1010))
	  (vector-set! $i.bg.a    (class00a #b1010))
	  (vector-set! $i.ble     (class00b #b0010))
	  (vector-set! $i.ble.a   (class00a #b0010))
	  (vector-set! $i.bge     (class00b #b1011))
	  (vector-set! $i.bge.a   (class00a #b1011))
	  (vector-set! $i.bl      (class00b #b0011))
	  (vector-set! $i.bl.a    (class00a #b0011))
	  (vector-set! $i.bgu     (class00b #b1100))
	  (vector-set! $i.bgu.a   (class00a #b1100))
	  (vector-set! $i.bleu    (class00b #b0100))
	  (vector-set! $i.bleu.a  (class00a #b0100))
	  (vector-set! $i.bcc     (class00b #b1101))
	  (vector-set! $i.bcc.a   (class00a #b1101))
	  (vector-set! $i.bcs     (class00b #b0101))
	  (vector-set! $i.bcs.a   (class00a #b0101))
	  (vector-set! $i.bpos    (class00b #b1110))
	  (vector-set! $i.bpos.a  (class00a #b1110))
	  (vector-set! $i.bneg    (class00b #b0110))
	  (vector-set! $i.bneg.a  (class00a #b0110))
	  (vector-set! $i.bvc     (class00b #b1111))
	  (vector-set! $i.bvc.a   (class00a #b1111))
	  (vector-set! $i.bvs     (class00b #b0111))
	  (vector-set! $i.bvs.a   (class00a #b0111))
	  (vector-set! $i.call    (class-call))
	  (vector-set! $i.jmplr   (class10r #b111000))
	  (vector-set! $i.jmpli   (class10i #b111000))
	  (vector-set! $i.label   (class-label))
	  (vector-set! $i.nop     (class-nop #b100))
	  v)))

      ; Assembler, main loop.
      ;
      ; `l' is a list of symbolic assembly instructions, each of which is a 
      ; list. Returns a bytevector of the assembled instructions.
      ;
      ; We make two passes over the list. The first pass calculates label 
      ; values. The second pass emits the code.
    
      (define (assemble-codevector l)

	(define f (make-bytevector (* (length l) 4))) ; Too simple?

	(define (emit! store? i)
	  (let ((i1 (quotient i #x1000000))
		(i2 (remainder (quotient i #x10000) #x10000))
		(i3 (remainder (quotient i #x100) #x100))
		(i4 (remainder i #x100)))
	    (if store?
		(begin (bytevector-set! f fptr i1)
		       (bytevector-set! f (+ fptr 1) i2)
		       (bytevector-set! f (+ fptr 2) i3)
		       (bytevector-set! f (+ fptr 3) i4)))
	    (set! fptr (+ fptr 4))))

	(define (assemble-instruction pass i)
	  (emit! (= pass 2) ((vector-ref itable (car i)) i)))

	(symtab.clear!)
	(set! fptr 0)
	(let loop ((il l))
	  (if (not (null? ill))
	      (begin (assemble-instruction 1 (car il))
		     (loop (cdr il)))
	      (begin (set! f (make-bytevector fptr))
		     (set! fptr 0)
		     (let loop ((il l))
		       (if (not (null? ill))
			   (begin (assemble-instruction  (car il))
				  (loop (cdr il)))
			   f))))))

	assemble-codevector))


;-----------------------------------------------------------------------------
; Pseudo-operators

(define (m-lo f)
  (lobits f 10))

(define (m-hi f)
  (hibits f 22))


;-----------------------------------------------------------------------------
; Print an unassembled list of instructions (for debugging purposes)

(define (print-ilist ilist)

  (define itable
    '#(($i.lddi      0)
       ($i.ldwi      1)
       ($i.ldhi      3)
       ($i.ldbi      4)
       ($i.lddfi     5)
       ($i.stddi     6)
       ($i.stdwi     7)
       ($i.stdhi     8)
       ($i.stdbi     9)
       ($i.stdfi     10)
       ($i.sethi     11)
       ($i.andr	     12)
       ($i.andrcc    13)
       ($i.andi	     14)
       ($i.andicc    15)
       ($i.orr	     16)
       ($i.orrcc     17)
       ($i.ori	     18)
       ($i.oricc     19)
       ($i.xorr	     20)
       ($i.xorrcc    21)
       ($i.xori	     22)
       ($i.xoricc    23)
       ($i.sllr	     24)
       ($i.slli	     25)
       ($i.srlr	     26)
       ($i.srli	     27)
       ($i.srar	     28)
       ($i.srai	     29)
       ($i.addr	     30)
       ($i.addrcc    31)
       ($i.addi	     31)
       ($i.addicc    32)
       ($i.taddrcc   33)
       ($i.taddicc   34)
       ($i.subr	     35)
       ($i.subrcc    36)
       ($i.subi	     37)
       ($i.subicc    38)
       ($i.tsubrcc   39)
       ($i.tsubicc   40)
       ($i.smulr     41)
       ($i.smulrcc   42)
       ($i.smuli     43)
       ($i.smulicc   44)
       ($i.sdivr     45)
       ($i.sdivrcc   46)
       ($i.sdivi     47)
       ($i.sdivicc   48)
       ($i.b	     49)
       ($i.b.a	     50)
       ($i.bne	     51)
       ($i.bne.a     52)
       ($i.be	     53)
       ($i.be.a	     54)
       ($i.bg	     55)
       ($i.bg.a	     56)
       ($i.ble	     57)
       ($i.ble.a     58)
       ($i.bge	     59)
       ($i.bge.a     60)
       ($i.bl	     61)
       ($i.bl.a	     62)
       ($i.bgu	     63)
       ($i.bgu.a     64)
       ($i.bleu	     65)
       ($i.bleu.a    66)
       ($i.bcc	     67)
       ($i.bcc.a     68)
       ($i.bcs	     69)
       ($i.bcs.a     70)
       ($i.bpos	     71)
       ($i.bpos.a    72)
       ($i.bneg	     73)
       ($i.bneg.a    74)
       ($i.bvc	     75)
       ($i.bvc.a     76)
       ($i.bvs	     77)
       ($i.bvs.a     78)
       ($i.call	     79)
       ($i.jmplr     80)
       ($i.jmpli     81)
       ($i.label     82)
       ($i.nop       83)
       (0 84)
       (0 85)
       (0 86)
       (0 87)
       (0 88)
       (0 89)
       (0 90)
       (0 91)
       (0 92)
       (0 93)
       (0 94)
       (0 95)
       (0 96)
       (0 97)
       (0 98)
       (0 99)
       ($i.lddr      100)
       ($i.ldwr      101)
       ($i.ldhr      102)
       ($i.ldbr      103)
       ($i.lddfr     104)
       ($i.stddr     105)
       ($i.stdwr     106)
       ($i.stdhr     107)
       ($i.stdbr     108)
       ($i.stdfr     109)))

  (define (print-i i)
    (display (car (vector-ref itable (car i))))
    (map (lambda (x) (display " ") (display x)) (cdr i))
    (newline))

  (if (null? ilist)
      '()
      (begin (print-i (car ilist))
	     (print-ilist (cdr ilist)))))


;-----------------------------------------------------------------------------
; Implementation-specific data conversion.

(define (char->immediate c)
  (+ (* c 65536) $imm.character))


;----------------------------------------------------------------------------
; Procedures which emit specific sequences of machine instructions.
; These are to be called from the machine-independent code generation
; routines.
;
; These are complicated by the presence of memory-mapped registers.


; Tag a fixnum and move it into a register.

(define (emit-fixnum->register! as f r)
  (emit-immediate->register! as (* f 4) r))


; Stuff a bitpattern into a register.

(define (emit-immediate->register! as i r)
  (let ((dest (if (not (hardware-mapped? r)) $r.tmp0 r)))
    (cond ((and (<= i 4095) (>= i -4096))
	   (emit! as `(,$i.addi ,$g0 ,(m-lo i) ,dest)))
	  ((zero? (remainder (abs i) 512))
	   (emit! as `(,$i.sethi ,(m-hi i) ,dest)))
	  (else
	   (emit! as `(,$i.sethi ,(m-hi i) ,dest))
	   (emit! as `(,$i.addi  ,dest ,(m-lo i) ,dest))))
    (if (not (hardware-mapped? r))
	(emit! as `(,$i.sti ,dest ,(offsetof r) ,$r.globals)))))


; Reference the constants vector and put the constant reference in a register.
; `cvlabel' is an integer offset into the constants vector (a constant) for
; the current procedure.

(define (emit-const->register! as cvlabel r)
  (emit! as `(,$i.ldi ,$r.reg0 ,$o.constvector ,$r.tmp0))
  (if (hardware-mapped? r)
      (emit! as `(,$i.ldi ,$r.tmp0 ,cvlabel ,r))
      (begin (emit! as `(,$i.ldi ,$r.tmp0 ,cvlabel, ,$r.tmp0))
	     (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof r) ,$r.globals)))))


; Move one register to another.

(define (emit-register->register! as from to)
  (cond ((and (hardware-mapped? from) (hardware-mapped? to))
	 (emit! as `(,$i.addr ,from ,$g0 ,to)))
	((hardware-mapped? from)
	 (emit! as `(,$i.sti ,from ,(offsetof to) ,$r.globals)))
	((hardware-mapped? to)
	 (emit! as `(,$i.ldi $r.globals ,(offsetof from) ,to)))
	(else
	 (emit! as '(,$i.ldi ,$r.globals ,(offsetof from) ,$r.tmp0))
	 (emit! as '(,$i.sti ,$r.tmp0 ,(offsetof to) ,$r.globals)))))


; Argument check. If the args don't match, drop to the exception handler and
; skip back to the check when the exception handler returns.
	 
(define (emit-args=! as n)
  (let ((l (new-label)))
    (emit! as `(,$i.subicc ,$r.result ,(* n 4) ,$r.g0))
    (emit! as `(,$i.be ,l))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.ldi ,$r.millicode ,$m.arg-exception ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
    (emit! as `(,$i.subi ,$r.o7 20 ,$r.o7))
    (emit! as `(,$i.label ,l))))


; Variable-length argument list check drops into millicode automatically.

(define (emit-args>=! as n)
  (emit! as `(,$i.ldi ,$r.millicode ,$m.varargs ,$r.tmp0))
  (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
  (emit! as `(,$i.nop)))


; Invoke a procedure, with all gyrations.

(define (emit-invoke! as n)
  (let ((l (new-label))
	(m (new-label)))
    (emit! as `(,$i.subicc ,$r.timer ,$r.timer ,$r.g0))
    (emit! as `(,$i.bne.a ,l))
    (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
    (emit! as `(,$i.ldi ,$r.millicode ,$m.timer-exception ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
    (emit! as `(,$i.label l))
    (emit! as `(,$i.subicc ,$r.tmp0 ,$tag.procedure ,$r.g0))
    (emit! as `(,$i.be ,m))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.ldi ,$r.millicode ,$m.proc-exception ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
    (emit! as `(,$i.subi ,$r.o7 24 ,$r.o7))
    (emit! as `(,$i.label m))
    (emit! as `(,$i.subi ,$r.timer 1 ,$r.timer))
    (emit! as `(,$i.addr ,$r.reg0 ,$r.g0 ,$r.argreg2))
    (emit! as `(,$i.addr ,$r.result ,$r.g0 ,$r.reg0))
    (emit! as `(,$i.addi ,$r.g0 ,(* n 4) ,$r.result))
    (emit! as `(,$i.ldi ,$r.reg0 ,$o.codevector ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 ,$o.codeoffset ,$r.g0))
    (emit! as `(,$i.nop))))

; Create stack frame, then save

(define (emit-save! as label n)
  (let ((l (new-label)))
    (emit! as `(,$i.ldi ,$r.globals ,$o.stk-limit ,$r.tmp0))
    (emit! as `(,$i.subrcc ,$r.tmp0 ,$r.stkp ,$r.g0))
    (emit! as `(,$i.ble ,l))
    (emit! as `(,$i.sub ,$r.stkp ,(+ 8 (* (+ n 1) 4)) ,$r.stkp))
    (emit! as `(,$i.ldi $r.millicode ,$m.stkoflow ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.sub ,$r.stkp ,(+ 8 (* (+ n 1) 4)) ,$r.stkp))
    (emit! as `(,$i.label l))
    (emit! as `(,$i.call (+ $ 8)))
    (emit! as `(,$i.sub ,$r.o7 (- label (- $ 4)) ,$r.o7))
    (emit! as `(,$i.sti ,$r.o7 0 ,$r.stkp))
    (emit! as `(,$i.addi ,$r.g0 ,(* (+ 8 (* (+ n 1) 4)) 4) ,$r.tmp0))
    (emit! as `(,$i.sti ,$r.tmp0 4 ,$r.stkp))
    (let loop ((i 0) (offset 8))
      (if (<= i n)
	  (let ((r (regname i)))
	    (if (hardware-mapped? r)
		(emit! as `(,$i.sti ,r ,offset ,$r.stkp))
		(begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp0))
		       (emit! as `(,$i.sti ,$r.tmp0 ,offset ,$r.stkp))))
	    (loop (+ i 1) (+ offset 4)))))))

; Restore registers from stack frame

(define (emit-restore! as n)
  (let loop ((i 0) (offset 8))
    (if (<= i n)
	(let ((r (regname i)))
	  (if (hardware-mapped? r)
	      (emit! as `(,$i.ldi ,$r.stkp ,offset ,r))
	      (begin (emit! as `(,$i.ldi ,$r.stkp ,offset ,$r.tmp0))
		     (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof r) ,$r.globals))))
	  (loop (+ i 1) (+ offset 4))))))

; Pop frame

(define (emit-pop! as n)
  (emit! as `(,$i.addi ,$r.stkp ,(+ 8 (* (+ n 1) 4)) ,$r.stkp)))

; Chenge the return address in the stack frame.

(define (emit-setrtn as label)
  (emit! as `(,$i.call (+ $ 8)))
  (emit! as `(,$i.sub ,$r.o7 (- label (- $ 4)) ,$r.o7))
  (emit! as `(,$i.sti ,$r.o7 0 ,$r.stkp)))

; `apply' falls into millicode

(define (emit-apply! as)
  (emit! as `(,$i.ldi ,$r.millicode ,$m.apply ,$r.tmp0))
  (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
  (emit! as `(,$i.nop)))

; a nop is a nop is a nop.

(define (emit-nop! as)
  (emit! as `(,$i.nop)))


(define (emit-load! as n k)
  (if (hardware-mapped? k)
      (emit! as `(,$i.ldi ,$r.stkp ,(+ 8 (* n 4)) ,k))
      (begin (emit! as `(,$i.ldi ,$r.stkp ,(+ 8 (* n 4)) ,$r.tmp0))
	     (emit! as `(,$i.sti ,$r.tmp0 ,(offsetof k) ,$r.globals)))))


(define (emit-store! as n k)
  (if (hardware-mapped? k)
      (emit! as `(,$i.sti ,k ,(+ 8 (* n 4)) ,$r.stkp))
      (begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof k) ,$r.tmp0))
	     (emit! as `(,$i.sti ,$r.tmp0 ,(+ 8 (* n 4)) ,$r.stkp)))))


(define (emit-lexical! as m n)
  (let ((base (emit-follow-chain! as m)))
    (emit! as `(,$i.ldi ,base ,(slotoffset n) ,$r.result))))

(define (emit-setlex! as m n)
  (let ((base (emit-follow-chain! as m)))
    (emit! as '(,$i.sti ,$r.result ,(slotoffset n) ,base))))

(define (emit-follow-chain! as m)
  (let loop ((q m))
    (if (not (zero? q))
	(begin (if (= q m)
		   (emit! as `(,$i.ldi ,$r.reg0 ,$o.linkoffset ,$r.tmp0))
		   (emit! as `(,$i.ldi ,$r.tmp0 ,$o.linkoffset ,$r.tmp0)))
	       (loop (- q 1)))
	(if (zero? m)
	    $r.reg0
	    $r.tmp0))))

; And many hippo returns...

(define (emit-return! as)
  (emit! as `(,$i.ldi ,$r.stkp 0 $r.tmp0))
  (emit! as `(,$i.jmpli $r.tmp0 8 ,$r.g0))
  (emit! as `(,$i.nop)))

; Multiple values are neat. But not neat enough to sweat them at this point.

(define (emit-mvrtn! as)
  (error 'emit-mvrtn! "multiple-value return has not been implemented (yet)."))

; Currently ignores `doc'; unclear what to do about it. Should probably go
; in the constant vector at some standard offset.

(define (emit-lexes! as n doc)
  (emit-alloc-proc! as n)
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.codevector ,$r.tmp0))
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp0 ,$p.codevector ,$r.result))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.constvector ,$r.result))
  (emit-init-proc-slots! as n))

; Ignores `doc', too.
; Assumes that `x' is the unadjusted offset into the current constant vector 
; of the code vector and that the constant vector for the new proc is in 
; the slot after the code vector. (Reasonably, `doc' could follow the 
; new constant vector.)

(define (emit-lambda! as x n doc)
  (emit-alloc-proc! as n)
  (emit! as `(,$i.ldi ,$r.reg0 ,$p.constvector ,$r.tmp0))
  (emit! as `(,$i.ldi ,$r.tmp0 ,(- x $tag.vector) ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.codevector ,$r.result))
  (emit! as `(,$i.ldi ,$r.tmp0 ,(- (+ x 4) $tag.vector) ,$r.tmp1))
  (emit! as `(,$i.sti ,$r.tmp1 ,$p.constvector ,$r.result))
  (emit-init-proc-slots! as n))
 
; allocate procedure with room for n register slots; return tagged pointer.

(define (emit-alloc-proc! as n)
  (emit! as `(,$i.ldi ,$r.millicode ,$m.alloc ,$r.tmp0))
  (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
  (emit! as `(,$i.addi ,$r.g0 ,(* (+ n 4) 4) ,$r.result))
  (emit! as `(,$i.addi ,$r.g0
		       ,(+ (* (* (+ n 3) 4) 256) $tag.procedure-header)
		       ,$r.tmp0))
  (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.result))
  (emit! as `(,$i.addi ,$r.result ,$tag.procedure ,$r.result)))

; Initialize data slots in procedure from current registers as specified for
; `lamba' and `lexes'.

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
	  (error 'emit-init-proc-slots!
		 "Can't deal with the linked list (yet).")))))

; Primops.


(define (emit-primop0! as op)
  '())

(define (emit-primop1! as op r)
  '())

(define (emit-primop2! as op r1 r2)
  '())

; assoc list of primops with generating procedures.

(define primop-list
  (list (cons 'zero?
	      (lambda (as)
		(emit-cmp-primop! as $i.tsubrcc $i.beq.a $m.zero? $r.g0)))
	(cons '=
	      (lambda (as r)
		(emit-cmp-primop! as $i.tsubrcc $i.beq.a $m.numeq r)))
	(cons '<
	      (lambda (as r)
		(emit-cmp-primop! as $i.tsubrcc $i.blt.a $m.numlt r)))
	(cons '<=
	      (lambda (as r)
		(emit-cmp-primop! as $i.tsubrcc $i.ble.a $m.numle r)))
	(cons '>
	      (lambda (as r)
		(emit-cmp-primop! as $i.tsubrcc $i.bgt.a $m.numgt r)))
	(cons '>=
	      (lambda (as r)
		(emit-cmp-primop! as $i.tsubrcc $i.bge.a $m.numge r)))
	(cons '+
	      (lambda (as r)
		(emit-arith-primop! as $i.taddrcc $m.add r)))
	(cons '-
	      (lambda (as r)
		(emit-arith-primop! as $i.tsubrcc $m.subtract r)))
	(cons '*
	      (lambda (as r)
		(error '() "No multiplication (yet).")))
	(cons 'null?
	      (lambda (as r)
		(emit-cmp-primop! as $i.subicc $i.beq.a $imm.null)))
	(cons 'pair?
	      (lambda (as)
		(let ((l (new-label)))
		  (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
		  (emit! as `(,$i.cmpi ,$r.tmp0 ,$tag.pair))
		  (emit! as `(,$i.addi ,$r.g0 ,$imm.true ,$r.result))
		  (emit! as `(,$i.bne.a ,l))
		  (emit! as `(,$i.addi ,$r.g0 ,$imm.false ,$r.result))
		  (emit! as `(,$i.label ,l)))))
	(cons 'cons
	      (lambda (as r)
		(emit! as `(,$i.ldi ,$r.millicode ,$m.alloc ,$r.tmp0))
		(emit! as `(,$i.addr ,$r.result ,$r.g0 ,$r.argreg2))
		(emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
		(emit! as `(,$i.addr ,$r.g0 8 ,$r.result))
		(emit! as `(,$i.sti ,$r.argreg2 0 ,$r.result))
		(emit! as `(,$i.sti ,r 4 ,$r.result))
		(emit! as `(,$i.arri ,$r.result ,$tag.pair ,$r.result))))
	(cons 'car
	      (lambda (as)
		(emit-pair-op! as 0)))
	(cons 'cdr
	      (lambda (as)
		(emit-pair-op! as 4)))
	(cons 'make-vector
	      (lambda (as r)
		'()))
	(cons 'vector-length
	      (lambda (as)
		'()))
	(cons 'vector-ref
	      (lambda (as r)
		'()))
	(cons 'vector-set!
	      (lambda (as r1 r2)
		'()))
	(cons 'make-cell
	      (lambda (as)
		(emit! as `(,$i.ldi ,$r.millicode ,$m.alloc ,$r.tmp0))
		(emit! as `(,$i.addr ,$r.result ,$r.g0 ,$r.argreg2))
		(emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
		(emit! as `(,$i.addr ,$r.g0 8 ,$r.result))
		(emit! as `(,$i.sti ,$r.argreg2 0 ,$r.result))
		(emit! as `(,$i.sti ,$r.g0 4 ,$r.result))
		(emit! as `(,$i.arri ,$r.result ,$tag.pair ,$r.result))))
	(cons 'cell-ref
	      (lambda (as)
		(emit! as `(,$i.ldi ,$r.result ,(- $tag.pair) ,$r.result))))
	(cons 'cell-set!
	      (lambda (as r)
		(emit! as `(,$i.sti ,r ,(- $tag.pair) ,$r.result))))))

(define (emit-cmp-primop! as cmp test generic r)
  (let ((l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,i ,$r.result ,r ,$r.g0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,$i.addi ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,$i.ldi ,$r.millicode ,generic ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.b ,l2))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,test ,l2))
    (emit! as `(,$i.addi ,$r.g0 ,$imm.true ,$r.result))
    (emit! as `(,$i.label ,l2))))

(define (emit-arith-primop! as i generic r)
  (let ((l1 (new-label)))
    (emit! as `(,i ,$r.result ,r $r.tmp0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,$i.addr ,$r.tmp0 ,$r.g0 $r.result))
    (emit! as `(,$i.ldi ,$r.millicode ,generic ,$r.tmp0))
    (emit! as `(,$i.addr ,r ,$r.g0 ,$r.argreg2))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$o7))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l1))))

(define (emit-pair-op! as offset)
  (let ((l (new-label)))
    (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
    (emit! as `(,$i.cmpi ,$r.tmp0 ,$tag.pair))
    (emit! as `(,$i.be ,l))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.ldi ,$r.millicode ,$m.type-exception ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.tmp0 0 ,$r.o7))
    (emit! as `(,$i.subi ,$r.o7 20 ,$r.o7))
    (emit! as `(,$i.label ,l))
    (emit! as `(,$i.ldi ,$r.result ,(- offset $tag.pair) ,$r.result))))

; -*- Scheme -*-
;
; Scheme 313 compiler
; Machine-dependent part of the assembler, for Sparc.
;
; $Id: asm.sparc.scm,v 1.8 91/08/23 22:11:57 lth Exp Locker: lth $
;
; There are a lot of tables here that have values which must correspond to
; those in header files used by C and assembly. We should find a better way
; to keep all these values in synch; some creative use of M4 comes to mind.

; largest symbolic opcode value + 1.
    
(define opcode-table-size 110)

; Table of core Sparc instruction set. Instructions not used by the 
; compiler have been left out.

(define $i.lddi      0)
(define $i.ldi       1)
(define $i.addi	     2)
(define $i.ldhi      3)
(define $i.ldbi      4)
(define $i.lddfi     5)
(define $i.stdi      6)
(define $i.sti       7)
(define $i.sthi      8)
(define $i.stbi      9)
(define $i.stdfi     10)
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
(define $i.label     82)
(define $i.nop       83)
(define $i.slot      84)
(define $i.bgeu      $i.bcc)
(define $i.bgeu.a    $i.bcc.a)
(define $i.blu       $i.bcs)
(define $i.blu.a     $i.bcs.a)
(define $i.ornr      85)
(define $i.orni      86)
(define $i.ornrcc    87)
(define $i.ornicc    88)
(define $i.lddr      100)
(define $i.ldr       101)
(define $i.ldhr      102)
(define $i.ldbr      103)
(define $i.lddfr     104)
(define $i.stdr      105)
(define $i.str       106)
(define $i.sthr      107)
(define $i.stbr      108)
(define $i.stdfr      109)

; Register definitions. These are fixed!

(define $r.g0 0)
(define $r.g1 1)
(define $r.g2 2)
(define $r.g3 3)
(define $r.g4 4)
(define $r.g5 5)
(define $r.g6 6)
(define $r.g7 7) 
(define $r.o0 8)
(define $r.o1 9)
(define $r.o2 10)
(define $r.o3 11)
(define $r.o4 12)
(define $r.o5 13)
(define $r.o6 14)
(define $r.o7 15)
(define $r.l0 16)
(define $r.l1 17)
(define $r.l2 18)
(define $r.l3 19)
(define $r.l4 20)
(define $r.l5 21)
(define $r.l6 22)
(define $r.l7 23)
(define $r.i0 24)
(define $r.i1 25)
(define $r.i2 26)
(define $r.i3 27)
(define $r.i4 28)
(define $r.i5 29)
(define $r.i6 30)
(define $r.i7 31)

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
(define $r.timer     $r.i4)

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

(define maxregs   32)
(define lastreg   (- maxregs 1))

; Various immediate quantities (low byte only, in most cases).
; Must correspond to the ones documented in "gcinterface.txt" and defined
; in "layouts.h" and "layouts.s.h".

(define $imm.true              #b00000110)
(define $imm.false             #b00000010)
(define $imm.null              #b00001010)
(define $imm.unspecified       #b0000000100010110)  ; misc #1
(define $imm.character         #b00100110)
(define $imm.vector-header     #b10100010)
(define $imm.bytevector-header #b11000010)
(define $imm.procedure-header  #b11111110)


; Bytevector 'xxx' fields, pre-shifted. 
; Must correspond to the ones in "layouts.s.h".

(define $tag.bytevector-typetag    #b00000000)
(define $tag.string-typetag        #b00000100)
(define $tag.flonum-typetag        #b00001000)
(define $tag.compnum-typetag       #b00001100)
(define $tag.bignum-typetag        #b00010000)


; Vector 'xxx' fields, pre-shifted.
; Ditto.

(define $tag.vector-typetag     #b00000000)
(define $tag.rectnum-typetag    #b00000100)
(define $tag.ratnum-typetag     #b00001000)
(define $tag.symbol-typetag     #b00001100)
(define $tag.port-typetag       #b00010000)

; Pointer tags.

(define $tag.pair-tag          #b001)
(define $tag.procedure-tag     #b111)
(define $tag.vector-tag        #b011)
(define $tag.bytevector-tag    #b101)
(define $tag.tagmask           #b111)

; Millicode indices, as defined in "millicode.h". These are byte offsets from
; the start of the table.

(define $m.timer-exception     168)   ; timer expired
(define $m.proc-exception      176)   ; non-procedure in call
(define $m.arg-exception       184)   ; wrong # of arguments
(define $m.varargs             192)   ; handle varargs
(define $m.stkoflow            0)     ; stack cache overflow
(define $m.apply               200)   ; handle the 'apply' instruction
(define $m.alloc               16)    ; allocate memory
(define $m.alloci              24)    ; ditto with initialization
(define $m.zerop               88)    ; generic zero?
(define $m.numeq               208)   ; generic =
(define $m.numlt               216)   ; generic <
(define $m.numgt               232)   ; generic >
(define $m.numle               224)   ; generic <=
(define $m.numge               240)   ; generic >=
(define $m.add                 96)    ; generic +
(define $m.subtract            104)   ; generic -
(define $m.multiply            112)   ; generic *
(define $m.type-exception      80)    ; generic type exception (bletch)
(define $m.complexp            312)
(define $m.realp               320)
(define $m.rationalp           328)
(define $m.integerp            336)
(define $m.exactp              344)
(define $m.inexactp            352)
(define $m.capture-continuation 152)
(define $m.restore-continuation 160)
(define $m.divide              136)
(define $m.quotient            120)
(define $m.remainder           128)
(define $m.modulo              304)
(define $m.negate              144)
(define $m.setcar              32)
(define $m.setcdr              40)
(define $m.vector-set          48)
(define $m.exact->inexact      360)
(define $m.inexact->exact      368)
(define $m.debug               432)
(define $m.reset               440)
(define $m.exit                448)
(define $m.break               456)
(define $m.typetag             464)
(define $m.typetag-set         472)
(define $m.eqv                 480)
(define $m.open-file           256)
(define $m.close-file          264)
(define $m.unlink-file         280)
(define $m.read-file           288)
(define $m.write-file          296)

; Various byte offsets into globals[]

(define $g.reg0        80)
(define $g.stk-limit   48)

; these are adjusted for a procedure tag.

(define $p.codevector  (- 4 $tag.procedure-tag))
(define $p.constvector (- 8 $tag.procedure-tag))
(define $p.linkoffset  (- 12 $tag.procedure-tag))
(define $p.reg0        $p.linkoffset)
(define $p.codeoffset  (- 4 $tag.bytevector-tag))

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

; Is an instruction a branch of some sort (including jumps and calls)
; This should really be implemented with an attribute table rather than
; depending on the ordinal values of the instructions!

(define (branch-instr? i)
  (and (>= (car i) 49) (<= (car i) 81)))

(define (weird-instr? i)
  (or (= (car i) $i.label)
      (< (car i) 0)))


; Return the offset in the %GLOBALS table of the given memory-mapped register.

(define (offsetof r)
  (+ $g.reg0 r))

; The offset of slot 'n' within a procedure structure.

(define (slotoffset n)
  (- (+ 12 (* n 4)) $tag.procedure-tag))

; Misc

(define two^32 (expt 2 32))

; Assembler proper.
;
; The assembler takes two arguments: a code list (list of Sparc assembly
; instructions in our Special Format) and a symbol table of enclosing 
; procedures, and returns a pair consisting of a bytevector (the assembled
; code) and the symbol table of this procedure concatenated with the one
; for the enclosing procedures.
;
; The format of an instruction in the code list is
;
;    (op opd ...)
;
; where `op' is the numeric code for an opcode (see the opcode table above)
; and the operands are either register numbers (see the register tables above)
; or literal expressions (immediates); which it is, is given away by the 
; opcode.
;
; The expressions in an immediate field must follow the grammar:
;
;   expr --> symbol
;          | `$'
;          | number
;          | ( `+' expr ... ) 
;          | ( `-' expr ... )
;          | ( `hi' expr )
;          | ( `lo' expr )
;
; The value of a label (a symbol) is its offset from the start of the code
; vector it is in. The special symbol `$' denotes the address of the start
; of the current instruction relative to the start of the code vector. 

(define assemble-codevector

  (let ()

    ; Constant expression evaluation.

    (define (eval-expr e)
      (let ((q (cond ((number? e)
		      e)
		     ((eq? e '$)	; current pc
		      fptr)
		     ((symbol? e)
		      ; Dangerous! If a symbol is undefined on pass2, then
		      ; we won't know, because lookup won't tell. This
		      ; needs to be fixed (although it is a compiler bug,
		      ; such bugs *are* hard to find!)
		      (symtab.lookup e))    
		     ((eq? '+ (car e))
		      (apply + (map eval-expr (cdr e))))
		     ((eq? '- (car e))  
		      ; nonconforming, as IEEE '-' takes 1 or 2 args only
		      (apply - (map eval-expr (cdr e))))
		     ((eq? 'hi (car e))
		      (hibits (eval-expr (cadr e)) 22))
		     ((eq? 'lo (car e))
		      (lobits (eval-expr (cadr e)) 10))
		     (else
		      (error 'eval-expr "Illegal expression")))))
	q))

    ; Current pc during assembly (relative to start of code vector). It needs 
    ; to be global since it is shared between 

    (define fptr 0)

    ; Hack for symbol table sanity.

    (define asm-pass 0)

    ; Symbol Table Stuff
    ;
    ; No error checking here; this is not intended to process human-created
    ; stuff. The compiler ought to know what it's doing.

    (define symtab '())
    
    ; use old symbol table

    (define (symtab.set! x)
      (set! symtab x))

    ; return symbol table

    (define (symtab.get)
      symtab)

    ; define a label corresponding to the current location.

    (define (symtab.define-label! label insn)
      (let ((x (assq label symtab)))
	(if (not x)
	    (set! symtab (cons (list label fptr insn) symtab)))))

    (define (symtab.lookup l)
      (let ((x (assq l symtab)))
	(if x
	    (cadr x)
	    (begin (if (= asm-pass 2)
		       (display (format "Undefined label ~a~%" l)))
		   0))))

    (define (symtab.target-insn l)
      (let ((x (assq l symtab)))
	(if x
	    (caddr x)
	    '(-1))))

    ; Bit Operations are not necessarily very pleasant in Scheme...
    
    ; exponent table holds 2^0 .. 2^32 in slots 0 .. 32.

    (define etable 
      (let ((v (make-vector 33 0)))
	(let loop ((i 0) (j 1))
	  (if (< i 33)
	      (begin (vector-set! v i j)
		     (loop (+ i 1) (* j 2)))
	      v))))

    ; Shift an integer m left n places.

    (define (shl m n)
      (* m (vector-ref etable n)))

    ; Extract the n lowest bits of the 32-bit integer m as a positive integer.

    (define (lobits m n)
      (if (negative? m)
	  (remainder (- (vector-ref etable 32) (abs m)) (vector-ref etable n))
	  (remainder m (vector-ref etable n))))

    ; Extract the n highest bits of the 32-bit integer m as a positive
    ; integer; the bits are shifted all the way to the right in the 
    ; result.

    (define (hibits m n)
      (if (negative? m)
	  (quotient (- (vector-ref etable 32) (abs m))
		    (vector-ref etable (- 32 n)))
	  (quotient m (vector-ref etable (- 32 n)))))

    ; Operations on code lists.
    ; A code list is a mutable data structure with operations `next',
    ; `push!', and `drop!'. The first returns the head of the code list.
    ; The second puts a new one on the list. The last removes the first.

    (define (make-codelist ilist)
      (cons ilist '()))

    (define (next cl)
      (if (null? (car cl))
	  '(-1)
	  (caar cl)))

    (define (push! cl i)
      (set-car! cl (cons i (car cl))))

    (define (drop! cl)
      (set-car! cl (if (null? (car cl)) '() (cdar cl))))

    (define (empty? cl)
      (null? (car cl)))


    ; let's have some scoping here!

    (let ()

      ; The instruction table.

      (define itable

	(let ()

	  (define ibit (expt 2 13))
	  (define abit (expt 2 29))

	  ; sethi, etc.

	  (define (class-sethi i)
	    (let ((i (shl i 22)))
	      (lambda (x cl)
		(let ((n  (lobits (eval-expr (operand1 x)) 22))
		      (rd (shl (operand2 x) 25)))
		  (+ rd i n)))))

	  ; nop is a peculiar sethi

	  (define (class-nop i)
	    (let ((q (class-sethi i)))
	      (lambda (x cl)
		(q `(dummy 0 ,$r.g0) cl))))

	  ; un-annulled branches

	  (define (class00b i)
	    (let ((i    (shl i 25))
		  (code (shl #b010 22)))
	      (lambda (x cl)
		(let ((offset (quotient
			       (lobits (eval-expr `(- ,(operand1 x) $)) 24)
			       4)))
		  (+ i code offset)))))
      
	  ; Annuled branches. Here we have to check for `slot' instructions
	  ; afterwards, because they can be filled from the branch target.
	  ;
	  ; The instruction at the branch target is kept in the symbol table.
	  ; The `slot' instruction will be replaced by the target and the
	  ; target address will be adjusted if the target instruction is 
	  ; suitable, i.e. if it is not a control transfer instruction.
	  ;
	  ; The target may not be available on the first pass through; this is
	  ; no cause of concern, as it will be resolved on the second pass,
	  ; and no decisions made here on the first pass are of interest in
	  ; final code generation.

	  (define (class00a i)
	    (let ((i    (shl i 25))
		  (code (shl #b010 22)))
	      (lambda (x cl)
		(let ((n (next cl))
		      (t (symtab.target-insn (operand1 x))))
		  (if (and (= (car n) $i.slot)
			   (not (branch-instr? t))
			   (not (weird-instr? t)))
		      (let ((offset
			     (quotient
			      (lobits (eval-expr `(+ (- ,(operand1 x) $) 4)) 24)
			      4)))
			(drop! cl)
			(push! cl t)
			(+ abit i code offset))
		      (let ((offset
			     (quotient
			      (lobits (eval-expr `(- ,(operand1 x) $)) 24)
			      4)))
			(+ abit i code offset)))))))
  
	  ; alu stuff and some others

	  (define (class10r i)
	    (let ((i    (shl i 19))
		  (code (shl #b10 30)))
	      (lambda (x cl)
		(let ((rs1 (shl (operand1 x) 14))
		      (rs2 (operand2 x))
		      (rd  (shl (operand3 x) 25)))
		  (+ code rd i rs1 rs2)))))

	  ; ditto

	  (define (class10i i)
	    (let ((i    (shl i 19))
		  (code (shl #b10 30)))
	      (lambda (x cl)
		(let ((rs1 (shl (operand1 x) 14))
		      (imm (lobits (eval-expr (operand2 x)) 13))
		      (rd  (shl (operand3 x) 25)))
		  (+ code rd i rs1 ibit imm)))))

	  ; memory stuff

	  (define (class11r i)
	    (let ((i    (shl i 19))
		  (code (shl #b11 30)))
	      (lambda (x cl)
		(let ((rs1 (shl (operand1 x) 14))
		      (rs2 (operand2 x))
		      (rd  (shl (operand3 x) 25)))
		  (+ code rd i rs1 rs2)))))

	  ; ditto

	  (define (class11i i)
	    (let ((i    (shl i 19))
		  (code (shl #b11 30)))
	      (lambda (x cl)
		(let ((rs1 (shl (operand1 x) 14))
		      (imm (lobits (eval-expr (operand2 x)) 13))
		      (rd  (shl (operand3 x) 25)))
		  (+ code rd i rs1 ibit imm)))))

	  ; For Stores. syntax is (st a b c) meaning m[ b + c ] <- a.
	  ; However, on the sparc, the destination field is the source
	  ; of a store, so we transform it into (st c b a) and pass it to
	  ; the real store procedure.

	  (define (class11sr i)
	    (let ((q (class11r i)))
	      (lambda (x cl)
		(q (list (car x)
			 (cadddr x)
			 (caddr x)
			 (cadr x))
		   cl))))

	  (define (class11si i)
	    (let ((q (class11i i)))
	      (lambda (x cl)
		(q (list (car x)
			 (cadddr x)
			 (caddr x)
			 (cadr x))
		   cl))))

	  ; call is a class all by itself

	  (define (class-call)
	    (let ((code (shl #b01 30)))
	      (lambda (x cl)
		(let ((offset (quotient
			       (lobits (eval-expr `(- ,(operand1 x) $)) 32)
			       4)))
		  (+ code offset)))))

	  (define (class-label)
	    (lambda (x cl)
	      (let ((n (next cl)))
		(symtab.define-label! (operand1 x) (if (weird-instr? n) '(-1) n))
		'())))

	  ; Multiplication and division are weird, since we want to call 
	  ; library  routines on all current implementations of the 
	  ; architecture.

	  (define (class-smul adj ccs)
	    (lambda (x cl)
	      (error 'class-smul "Unimplemented -- call millicode instead.")))

	  (define (class-sdiv adj ccs)
	    (lambda (x cl)
	      (error 'class-sdiv "Unimplemented -- call millicode instead.")))

	  ; make the opcode vector

	  (let ((v (make-vector opcode-table-size)))
	    (vector-set! v $i.lddi    (class11i #b000011))
	    (vector-set! v $i.lddr    (class11r #b000011))
	    (vector-set! v $i.ldi     (class11i #b000000))
	    (vector-set! v $i.ldr     (class11r #b000000))
	    (vector-set! v $i.ldhi    (class11i #b000010))
	    (vector-set! v $i.ldhr    (class11r #b000010))
	    (vector-set! v $i.ldbi    (class11i #b000001))
	    (vector-set! v $i.ldbr    (class11r #b000001))
	    (vector-set! v $i.lddfi   (class11i #b100001))
	    (vector-set! v $i.lddfr   (class11r #b100001))
	    (vector-set! v $i.stdi    (class11si #b000111))
	    (vector-set! v $i.stdr    (class11sr #b000111))
	    (vector-set! v $i.sti     (class11si #b000100))
	    (vector-set! v $i.str     (class11sr #b000100))
	    (vector-set! v $i.sthi    (class11si #b000110))
	    (vector-set! v $i.sthr    (class11sr #b000110))
	    (vector-set! v $i.stbi    (class11si #b000101))
	    (vector-set! v $i.stbr    (class11sr #b000101))
	    (vector-set! v $i.stdfi   (class11si #b100111))
	    (vector-set! v $i.stdfr   (class11sr #b100111))
	    (vector-set! v $i.sethi   (class-sethi #b100))
	    (vector-set! v $i.andr    (class10r #b000001))
	    (vector-set! v $i.andrcc  (class10r #b010001))
	    (vector-set! v $i.andi    (class10i #b000001))
	    (vector-set! v $i.andicc  (class10i #b010001))
	    (vector-set! v $i.orr     (class10r #b000010))
	    (vector-set! v $i.orrcc   (class10r #b010010))
	    (vector-set! v $i.ori     (class10i #b000010))
	    (vector-set! v $i.oricc   (class10i #b010010))
	    (vector-set! v $i.xorr    (class10r #b000011))
	    (vector-set! v $i.xorrcc  (class10r #b010011))
	    (vector-set! v $i.xori    (class10i #b000011))
	    (vector-set! v $i.xoricc  (class10i #b010011))
	    (vector-set! v $i.sllr    (class10r #b100101))
	    (vector-set! v $i.slli    (class10i #b100101))
	    (vector-set! v $i.srlr    (class10r #b100110))
	    (vector-set! v $i.srli    (class10i #b100110))
	    (vector-set! v $i.srar    (class10r #b100111))
	    (vector-set! v $i.srai    (class10i #b100111))
	    (vector-set! v $i.addr    (class10r #b000000))
	    (vector-set! v $i.addrcc  (class10r #b010000))
	    (vector-set! v $i.addi    (class10i #b000000))
	    (vector-set! v $i.addicc  (class10i #b010000))
	    (vector-set! v $i.taddrcc (class10r #b100000))
	    (vector-set! v $i.taddicc (class10i #b100000))
	    (vector-set! v $i.subr    (class10r #b000100))
	    (vector-set! v $i.subrcc  (class10r #b010100))
	    (vector-set! v $i.subi    (class10i #b000100))
	    (vector-set! v $i.subicc  (class10i #b010100))
	    (vector-set! v $i.tsubrcc (class10r #b100001))
	    (vector-set! v $i.tsubicc (class10i #b100001))
	    (vector-set! v $i.smulr   (class-smul 'r 'nocc))
	    (vector-set! v $i.smulrcc (class-smul 'r 'cc))
	    (vector-set! v $i.smuli   (class-smul 'i 'nocc))
	    (vector-set! v $i.smulicc (class-smul 'i 'cc))
	    (vector-set! v $i.sdivr   (class-sdiv 'r 'nocc))
	    (vector-set! v $i.sdivrcc (class-sdiv 'r 'cc))
	    (vector-set! v $i.sdivi   (class-sdiv 'i 'nocc))
	    (vector-set! v $i.sdivicc (class-sdiv 'i 'cc))
	    (vector-set! v $i.b       (class00b #b1000))
	    (vector-set! v $i.b.a     (class00a #b1000))
	    (vector-set! v $i.bne     (class00b #b1001))
	    (vector-set! v $i.bne.a   (class00a #b1001))
	    (vector-set! v $i.be      (class00b #b0001))
	    (vector-set! v $i.be.a    (class00a #b0001))
	    (vector-set! v $i.bg      (class00b #b1010))
	    (vector-set! v $i.bg.a    (class00a #b1010))
	    (vector-set! v $i.ble     (class00b #b0010))
	    (vector-set! v $i.ble.a   (class00a #b0010))
	    (vector-set! v $i.bge     (class00b #b1011))
	    (vector-set! v $i.bge.a   (class00a #b1011))
	    (vector-set! v $i.bl      (class00b #b0011))
	    (vector-set! v $i.bl.a    (class00a #b0011))
	    (vector-set! v $i.bgu     (class00b #b1100))
	    (vector-set! v $i.bgu.a   (class00a #b1100))
	    (vector-set! v $i.bleu    (class00b #b0100))
	    (vector-set! v $i.bleu.a  (class00a #b0100))
	    (vector-set! v $i.bcc     (class00b #b1101))
	    (vector-set! v $i.bcc.a   (class00a #b1101))
	    (vector-set! v $i.bcs     (class00b #b0101))
	    (vector-set! v $i.bcs.a   (class00a #b0101))
	    (vector-set! v $i.bpos    (class00b #b1110))
	    (vector-set! v $i.bpos.a  (class00a #b1110))
	    (vector-set! v $i.bneg    (class00b #b0110))
	    (vector-set! v $i.bneg.a  (class00a #b0110))
	    (vector-set! v $i.bvc     (class00b #b1111))
	    (vector-set! v $i.bvc.a   (class00a #b1111))
	    (vector-set! v $i.bvs     (class00b #b0111))
	    (vector-set! v $i.bvs.a   (class00a #b0111))
	    (vector-set! v $i.call    (class-call))
	    (vector-set! v $i.jmplr   (class10r #b111000))
	    (vector-set! v $i.jmpli   (class10i #b111000))
	    (vector-set! v $i.label   (class-label))
	    (vector-set! v $i.nop     (class-nop #b100))
	    (vector-set! v $i.slot    (class-nop #b100))
	    (vector-set! v $i.ornr    (class10r #b000110))
	    (vector-set! v $i.orni    (class10i #b000110))
	    (vector-set! v $i.ornrcc  (class10r #b010110))
	    (vector-set! v $i.ornicc  (class10i #b010110))
	    v)))

      ; Assembler, main loop.
      ;
      ; `codelist' is a list of symbolic assembly instructions, each of 
      ; which is a 
      ; list. Returns a bytevector of the assembled instructions.
      ;
      ; We make two passes over the list. The first pass calculates label 
      ; values. The second pass emits the code.
    
      (define (assemble-codevector instructions symtab)

	(define f '())

	(define (emit! store? i)
	  (if (not (null? i))
	      (let ((i1 (quotient i (expt 256 3)))
		    (i2 (remainder (quotient i (expt 256 2)) 256))
		    (i3 (remainder (quotient i 256) 256))
		    (i4 (remainder i 256)))
		(if store?
		    (begin (if (>= i two^32)
			       (error "ouch." i fptr 
				      (bytevector-ref f (- fptr 4))
				      (bytevector-ref f (- fptr 3))
				      (bytevector-ref f (- fptr 2))
				      (bytevector-ref f (- fptr 1))
				      ))
			   (bytevector-set! f fptr i1)
			   (bytevector-set! f (+ fptr 1) i2)
			   (bytevector-set! f (+ fptr 2) i3)
			   (bytevector-set! f (+ fptr 3) i4)))
		(set! fptr (+ fptr 4)))))

	(define (assemble-instruction pass i cl)
	  (emit! (= pass 2) ((vector-ref itable (car i)) i cl)))

	(symtab.set! symtab)

	(set! fptr 0)
	(set! asm-pass 1)
	(let loop ((cl (make-codelist instructions)))
	  (if (not (empty? cl))
	      (let ((i (next cl)))
		(drop! cl)
		(assemble-instruction 1 i cl)
		(loop cl))
	      (begin (set! f (make-bytevector fptr))
		     (set! fptr 0)
		     (set! asm-pass 2)
		     (let loop ((cl (make-codelist instructions)))
		       (if (not (empty? cl))
			   (let ((i (next cl)))
			     (drop! cl)
			     (assemble-instruction 2 i cl)
			     (loop cl))
			   (begin (if listify? 
				      (print-ilist (disassemble f)))
				  (cons f (symtab.get)))))))))

      ; yo!
      
      assemble-codevector)))

; EOF

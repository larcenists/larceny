; -*- Scheme -*-
;
; Scheme 313 compiler (SPARC)
; Machine-dependent part of the assembler.
; Machine-dependent code generation procedures.
;
; $Id$

;-----------------------------------------------------------------------------
; Table of core Sparc instruction set. Instructions not used by the assembler
; have been left out.
;
; Instructions are listed in the order found in the architecture manual ,
; appendix B.

; largest symbolic opcode value + 1.

(define opcode-table-size 110)

; symbolic opcodes for sparc.

(define $i.lddi		0)  
(define $i.lddr		100)
(define $i.ldwi		1)
(define $i.ldwr		101)
(define $i.ldhi		3)
(define $i.ldhr		102)
(define $i.ldbi		4)
(define $i.ldbr		103)
(define $i.lddfi	5)
(define $i.lddfr	104)
(define $i.stddi	6)
(define $i.stddr	105)
(define $i.stdwi	7)
(define $i.stdwr	106)
(define $i.stdhi	8)
(define $i.stdhr	107)
(define $i.stdbi	9)
(define $i.stdbr	108)
(define $i.stdfi	10)
(define $i.stdfr	109)
(define $i.sethi	11)
(define $i.andr		12)
(define $i.andrcc	13)
(define $i.andi		14)
(define $i.andicc	15)
(define $i.orr		16)
(define $i.orrcc	17)
(define $i.ori		18)
(define $i.oricc	19)
(define $i.xorr		20)
(define $i.xorrcc	21)
(define $i.xori		22)
(define $i.xoricc	23)
(define $i.sllr		24)
(define $i.slli		25)
(define $i.srlr		26)
(define $i.srli		27)
(define $i.srar		28)
(define $i.srai		29)
(define $i.addr		30)
(define $i.addrcc	31)
(define $i.addi		31)
(define $i.addicc	32)
(define $i.taddrcc	33)
(define $i.taddicc	34)
(define $i.subr		35)
(define $i.subrcc	36)
(define $i.subi		37)
(define $i.subicc	38)
(define $i.tsubrcc	39)
(define $i.tsubicc	40)
(define $i.smulr	41)
(define $i.smulrcc	42)
(define $i.smuli	43)
(define $i.smulicc	44)
(define $i.sdivr	45)
(define $i.sdivrcc	46)
(define $i.sdivi	47)
(define $i.sdivicc	48)
(define $i.b		49)
(define $i.b,a		50)
(define $i.bne		51)
(define $i.bne,a	52)
(define $i.be		53)
(define $i.be,a		54)
(define $i.bg		55)
(define $i.bg,a		56)
(define $i.ble		57)
(define $i.ble,a	58)
(define $i.bge		59)
(define $i.bge,a	60)
(define $i.bl		61)
(define $i.bl,a		62)
(define $i.bgu		63)
(define $i.bgu,a	64)
(define $i.bleu		65)
(define $i.bleu,a	66)
(define $i.bcc		67)
(define $i.bcc,a	68)
(define $i.bcs		69)
(define $i.bcs,a	70)
(define $i.bpos		71)
(define $i.bpos,a	72)
(define $i.bneg		73)
(define $i.bneg,a	74)
(define $i.bvc		75)
(define $i.bvc,a	76)
(define $i.bvs		77)
(define $i.bvs,a	78)
(define $i.call		79)
(define $i.jmplr	80)
(define $i.jmpli        81)
(define $i.label        82)          ; a label

; Register definitions. These are fixed.

(define $g0 0)
(define $g1 1)
(define $i0 24)
(define $i1 25)
(define $i2 26)
(define $i3 27)
(define $i4 28)
(define $i5 29)
(define $i6 30)
(define $i7 31)
(define $l0 16)
(define $l1 17)
(define $l2 18)
(define $l3 19)
(define $l4 20)
(define $l5 21)
(define $l6 22)
(define $l7 23)
(define $o0 8)
(define $o1 9)
(define $o2 10)
(define $o3 11)
(define $o4 12)
(define $o5 13)
(define $o6 14)
(define $o7 15)

; Register mapping for hardware-mapped registers; offsets into `globals'
; for memory-mapped registers.

(define $tmp0      $g1)
(define $tmp1      $i0)
(define $tmp2      $i1)
(define $reg0      $l0)
(define $reg1      $l1)
(define $reg2      $l2)
(define $reg3      $l3)
(define $reg4      $l4)
(define $reg5      $l5)
(define $reg6      $l6)
(define $reg7      $l7)
(define $result    $o0)
(define $argreg2   $o1)
(define $argreg3   $o2)
(define $stkp      '())
(define $globals   '())
(define $millicode '())
(define $e-top     '())
(define $e-limit   '())

; Is a general-purpose register mapped to a hardware register?

(define (hardware-mapped? r)
  (and (>= r $reg0) (<= 4 $reg7)))

; Assembler, main loop.
;
; `l' is a list of symbolic assembly instructions, each of which is a list.
; Returns a bytevector of the assembled instructions.
;
; We make two passes over the list. The first pass calculates label values.
; The second pass emits the code.

; Current pc during assembly (relative to start of code vector). It needs to
; be global since it is shared between 

(define fptr 0)

(define (assemble-codevector l)

  (define f (make-bytevector (* (length l) 4)))  ; Too simple?

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

;-----------------------------------------------------------------------------
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


;-----------------------------------------------------------------------------
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
  (remainder m (vector-ref etable bits)))   ; maybe iffy if m < 0?

(define (hibits m bits)
  (quotient m (vector-ref etable (- 32 bits))))


;-----------------------------------------------------------------------------
; Instruction Class Handlers.

; sethi, etc.

(define (class00 i)
  (let ((i (shl i 21)))
    (lambda (x)
      (let ((n (instruction.arg1 x))
	    (rd (instruction.arg2 x)))
	(+ (shl rd 24) i n ))))

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

; call is a class all by itself :-)

(define (class-call)
  (lambda (x)
    (let ((offset (instruction.arg1 x)))
      (+ #x40000000 offset))))

(define (class-label)
  (lambda (x)
    (symtab.define-label! (instruction.arg1 x))))

; Multiplication and division are weird, since we want to call library
; routines on all current implementations of the architecture.

(define (class-smul adj ccs)
  (lambda (x) (error 'class-smul "Unimplemented -- call millicode instead.")))

(define (class-sdiv adj ccs)
  (lambda (x) (error 'class-sdiv "Unimplemented -- call millicode instead.")))

; The instruction table.

(define itable
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
    (vector-set! $i.sethi   (class00 #b100))
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
    (vector-set! $i.b,a     (class00a #b1000))
    (vector-set! $i.bne     (class00b #b1001))
    (vector-set! $i.bne,a   (class00a #b1001))
    (vector-set! $i.be      (class00b #b0001))
    (vector-set! $i.be,a    (class00a #b0001))
    (vector-set! $i.bg      (class00b #b1010))
    (vector-set! $i.bg,a    (class00a #b1010))
    (vector-set! $i.ble     (class00b #b0010))
    (vector-set! $i.ble,a   (class00a #b0010))
    (vector-set! $i.bge     (class00b #b1011))
    (vector-set! $i.bge,a   (class00a #b1011))
    (vector-set! $i.bl      (class00b #b0011))
    (vector-set! $i.bl,a    (class00a #b0011))
    (vector-set! $i.bgu     (class00b #b1100))
    (vector-set! $i.bgu,a   (class00a #b1100))
    (vector-set! $i.bleu    (class00b #b0100))
    (vector-set! $i.bleu,a  (class00a #b0100))
    (vector-set! $i.bcc     (class00b #b1101))
    (vector-set! $i.bcc,a   (class00a #b1101))
    (vector-set! $i.bcs     (class00b #b0101))
    (vector-set! $i.bcs,a   (class00a #b0101))
    (vector-set! $i.bpos    (class00b #b1110))
    (vector-set! $i.bpos,a  (class00a #b1110))
    (vector-set! $i.bneg    (class00b #b0110))
    (vector-set! $i.bneg,a  (class00a #b0110))
    (vector-set! $i.bvc     (class00b #b1111))
    (vector-set! $i.bvc,a   (class00a #b1111))
    (vector-set! $i.bvs     (class00b #b0111))
    (vector-set! $i.bvs,a   (class00a #b0111))
    (vector-set! $i.call    (class-call))
    (vector-set! $i.jmplr   (class10r #b111000))
    (vector-set! $i.jmpli   (class10i #b111000))
    (vector-set! $i.label   (class-label))
    v))


;-----------------------------------------------------------------------------
; Pseudo-operators

(define (m-lo f)
  (lobits f 10))

(define (m-hi f)
  (hibits f 22))


;-----------------------------------------------------------------------------
; Procedures which emit specific sequences of machine instructions.
; These are to be called from the machine-independent code generation
; routines.

; tag a fixnum and move it into a register.

(define (m-fixnum->reg f r)
  (let ((f (* f 4)))                             ; add the tag
    (cond ((and (<= f 4095) (>= f -4096))
	   `((,$i.addi ,$g0 ,(m-lo f) ,r)))
	  ((zero? (remainder (abs f) 512))
	   `((,$i.sethi ,(m-hi f) ,r)))
	  (else
	   `((,$i.sethi ,(m-hi f) ,r)
	     (,$i.addi  ,r ,(m-lo f) ,r))))))


; move one register to another.

(define (m-movereg from to)
  `((,$i.addr ,from ,$g0 ,to)))

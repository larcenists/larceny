; Asm/Sparc/sparcdis.sch
; Larceny back-end -- SPARC disassembler
;
; $Id: sparcdis.sch,v 1.3 1997/09/23 20:02:38 lth Exp lth $
;
; Interface.
;  (disassemble-codevector codevector)             => decoded-instruction-list
;  (disassemble-instruction instruction address)    => decoded-instruction
;  (print-instructions decoded-instruction-list)    => unspecified
;      Also takes an optional port and optionally the symbol "native-names".
;  (format-instruction decoded-instruction address larceny-names?) => string
; 
; A `decoded-instruction' is a list where the car is a mnemonic (see below)
; and the operands are appropriate for that mnemonic.
;
; A `mnemonic' is an exact nonnegative integer.  It encodes the name of
; the instruction as well as its attributes (operand pattern and instruction
; type).  See below for specific operations on mnemonics.

(define (disassemble-codevector cv)
  (define (loop addr ilist)
    (if (< addr 0)
	ilist
	(loop (- addr 4)
	      (cons (disassemble-instruction (bytevector-word-ref cv addr)
					     addr)
		    ilist))))
  (loop (- (bytevector-length cv) 4) '()))

(define disassemble-instruction)	    ; Defined below.

; Mnemonics

(define *asm-annul* 1)
(define *asm-immed* 2)
(define *asm-store* 4)
(define *asm-load* 8)
(define *asm-branch* 16)
(define *asm-freg* 32)

(define *asm-bits*
  `((a . ,*asm-annul*) (i . ,*asm-immed*) (s . ,*asm-store*)
    (l . ,*asm-load*) (b . ,*asm-branch*) (f . ,*asm-freg*)))

(define *asm-mnemonic-table* '())

(define mnemonic 
  (let ((n 0))
    (lambda (name . rest)
      (let* ((probe (assq name *asm-mnemonic-table*))
	     (code  (* 1024 
		       (if probe
			   (cdr probe)
			   (let ((code n))
			     (set! n (+ n 1))
			     (set! *asm-mnemonic-table*
				   (cons (cons name code)
					 *asm-mnemonic-table*))
			     code)))))
	(for-each (lambda (x)
		    (set! code (+ code (cdr (assq x *asm-bits*)))))
		  rest)
	code))))

(define (mnemonic:name mnemonic)
  (let ((mnemonic (quotient mnemonic 1024)))
    (let loop ((t *asm-mnemonic-table*))
      (cond ((null? t) #f)
	    ((= (cdar t) mnemonic) (caar t))
	    (else (loop (cdr t)))))))

(define (mnemonic=? m name)
  (= (quotient m 1024) (quotient (mnemonic name) 1024)))

(define (mnemonic:test bit)
  (lambda (mnemonic)
    (not (zero? (logand mnemonic bit)))))

(define mnemonic:annul? (mnemonic:test *asm-annul*))
(define mnemonic:immediate? (mnemonic:test *asm-immed*))
(define mnemonic:store? (mnemonic:test *asm-store*))
(define mnemonic:load? (mnemonic:test *asm-load*))
(define mnemonic:branch? (mnemonic:test *asm-branch*))
(define mnemonic:freg? (mnemonic:test *asm-freg*))

; Instruction disassembler.

(let ()

  ;; Useful constants

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

  ;; Class 0 has branches and weirdness, like sethi and nop.
  ;; We dispatch first on the op2 field and then on the op3 field.

  (define class00
    (let ((b-table
	   (vector (mnemonic 'bn 'b)
		   (mnemonic 'be 'b)
		   (mnemonic 'ble 'b)
		   (mnemonic 'bl 'b)
		   (mnemonic 'bleu 'b)
		   (mnemonic 'bcs 'b)
		   (mnemonic 'bneg 'b)
		   (mnemonic 'bvs 'b)
		   (mnemonic 'ba 'b)
		   (mnemonic 'bne 'b)
		   (mnemonic 'bg 'b)
		   (mnemonic 'bge 'b)
		   (mnemonic 'bgu 'b)
		   (mnemonic 'bcc 'b)
		   (mnemonic 'bpos 'b)
		   (mnemonic 'bvc 'b)
		   (mnemonic 'bn 'a 'b)
		   (mnemonic 'be 'a 'b)
		   (mnemonic 'ble 'a 'b)
		   (mnemonic 'bl 'a 'b)
		   (mnemonic 'bleu 'a 'b)
		   (mnemonic 'bcs 'a 'b)
		   (mnemonic 'bneg 'a 'b)
		   (mnemonic 'bvs 'a 'b)
		   (mnemonic 'ba 'a 'b)
		   (mnemonic 'bne 'a 'b)
		   (mnemonic 'bg 'a 'b)
		   (mnemonic 'bge 'a 'b)
		   (mnemonic 'bgu 'a 'b)
		   (mnemonic 'bcc 'a 'b)
		   (mnemonic 'bpos 'a 'b)
		   (mnemonic 'bvc 'a 'b)))
	  (nop (mnemonic 'nop))
	  (sethi (mnemonic 'sethi)))

      (lambda (ip instr)
	(let ((op2 (op2field instr)))
	  (cond ((= op2 #b100)
		 (if (zero? (rdfield instr))
		     `(,nop)
		     `(,sethi ,(imm22field instr) ,(rdfield instr))))
		((= op2 #b010)
		 `(,(vector-ref b-table (rdfield instr))
		   ,(* 4 (imm22field instr))))
		(else
		 (disasm-error "Can't disassemble " (number->string instr 16)
			       " at ip=" ip
			       " with op2=" op2)))))))

  ;; Class 1 is the call instruction; there's no choice.

  (define (class01 ip instr)
    `(,(mnemonic 'call) ,(* 4 (imm30field instr))))

  ;; Class 2 is for the ALU. Dispatch on op3 field.

  (define class10
    (let ((op3-table
	   `#((,(mnemonic 'add)   ,(mnemonic 'add 'i))
	      (,(mnemonic 'and)   ,(mnemonic 'and 'i))
	      (,(mnemonic 'or)    ,(mnemonic 'or 'i))
	      (,(mnemonic 'xor)   ,(mnemonic 'xor 'i))
	      (,(mnemonic 'sub)   ,(mnemonic 'sub 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)                              ; 10
	      (,(mnemonic 'smul)  ,(mnemonic 'smul 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'sdiv)  ,(mnemonic 'sdiv 'i))
	      (,(mnemonic 'addcc) ,(mnemonic 'addcc 'i))
	      (,(mnemonic 'andcc) ,(mnemonic 'andcc 'i))
	      (,(mnemonic 'orcc)  ,(mnemonic 'orcc 'i))
	      (,(mnemonic 'xorcc) ,(mnemonic 'xorcc 'i))
	      (,(mnemonic 'subcc) ,(mnemonic 'subcc 'i))  ; 20
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'smulcc) ,(mnemonic 'smulcc 'i))
	      (0          0)
	      (0          0)
	      (0          0)                               ; 30
	      (,(mnemonic 'sdivcc) ,(mnemonic 'sdivcc 'i))
	      (,(mnemonic 'taddcc) ,(mnemonic 'taddcc 'i))
	      (,(mnemonic 'tsubcc) ,(mnemonic 'tsubcc 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'sll)   ,(mnemonic 'sll 'i))
	      (,(mnemonic 'srl)   ,(mnemonic 'srl 'i))
	      (,(mnemonic 'sra)   ,(mnemonic 'sra 'i))
	      (0          0)                               ; 40
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)                               ; 50
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'jmpl)  ,(mnemonic 'jmpl 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'save)  ,(mnemonic 'save 'i))   ; 60
	      (,(mnemonic 'restore) ,(mnemonic 'restore 'i))
	      (0          0)
	      (0          0))))

      (lambda (ip instr)
	(nice-instruction op3-table ip instr))))


  ;; Class 3 is memory stuff.

  (define class11
    (let ((op3-table
	   `#((,(mnemonic 'ld 'l)    ,(mnemonic 'ld 'i 'l))
	      (,(mnemonic 'ldb 'l)   ,(mnemonic 'ldb 'i 'l))
	      (,(mnemonic 'ldh 'l)   ,(mnemonic 'ldh 'i 'l))
	      (,(mnemonic 'ldd 'l)   ,(mnemonic 'ldd 'i 'l))
	      (,(mnemonic 'st 's)    ,(mnemonic 'st 'i 's))
	      (,(mnemonic 'stb 's)   ,(mnemonic 'stb 'i 's))
	      (,(mnemonic 'sth 's)   ,(mnemonic 'sth 'i 's))
	      (,(mnemonic 'std 's)   ,(mnemonic 'std 'i 's))
	      (0          0)
	      (0          0)
	      (0          0)		; 10
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 20
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 30
	      (0          0)
	      (,(mnemonic 'ldf 'f 'l) ,(mnemonic 'ldf 'i 'f 'l))
	      (0          0)
	      (0          0)
	      (,(mnemonic 'lddf 'f 'l) ,(mnemonic 'lddf 'i 'f 'l))
	      (,(mnemonic 'stf 'f 's)  ,(mnemonic 'stf 'i 'f 's))
	      (0          0)
	      (0          0)
	      (,(mnemonic 'stdf 'f 's) ,(mnemonic 'stdf 'i 'f 's))
	      (0          0)		; 40
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 50
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 60
	      (0          0)
	      (0          0)
	      (0          0))))

      (lambda (ip instr)
	(nice-instruction op3-table ip instr))))

  ;; For classes 2 and 3

  (define (nice-instruction op3-table ip instr)
    (let* ((op3  (op3field instr))
	   (imm  (ifield instr))
	   (rd   (rdfield instr))
	   (rs1  (rs1field instr))
	   (src2 (if (zero? imm)
		     (rs2field instr)
		     (imm13field instr))))
      (let ((op ((if (zero? imm) car cadr) (vector-ref op3-table op3))))
	`(,op ,rs1 ,src2 ,rd))))

  ;; The following procedures pick apart an instruction

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

  (set! disassemble-instruction
	(let ((class-table (vector class00 class01 class10 class11)))
	  (lambda (instr addr)
	    ((vector-ref class-table (quotient instr two^30)) addr instr))))

  'disassemble-instruction)


; Instruction printer
;
; It assumes that the first instruction comes from address 0, and prints
; addresses (and relative addresses) based on that assumption.
;
; If the optional symbol native-names is supplied, then SPARC register
; names is used, and millicode calls are not annotated with millicode names.

(define (print-instructions ilist . rest)

  (define port (current-output-port))
  (define larceny-names? #t)

  (define (print-ilist ilist a)
    (if (null? ilist)
	'()
	(begin (display (format-instruction (car ilist) a larceny-names?)
			port)
	       (newline port)
	       (print-ilist (cdr ilist) (+ a 4)))))
  
  (do ((rest rest (cdr rest)))
      ((null? rest))
    (cond ((port? (car rest))
	   (set! port (car rest)))
	  ((eq? (car rest) 'native-names)
	   (set! larceny-names? #f))))
  
  (print-ilist ilist 0))

(define format-instruction)		    ; Defined below

(define *format-instructions-pretty* #t)

; Instruction formatter.

(let ()

  (define use-larceny-registers #t)

  (define sparc-register-table 
    (vector "%g0" "%g1" "%g2" "%g3" "%g4" "%g5" "%g6" "%g7"
	    "%o0" "%o1" "%o2" "%o3" "%o4" "%o5" "%o6" "%o7"
	    "%l0" "%l1" "%l2" "%l3" "%l4" "%l5" "%l6" "%l7"
	    "%i0" "%i1" "%i2" "%i3" "%i4" "%i5" "%i6"  "%i7"))

  (define larceny-register-table
    (make-vector 32 #f))

  (define (larceny-register-name reg . rest)
    (if (null? rest)
	(or (and use-larceny-registers
		 (vector-ref larceny-register-table reg))
	    (vector-ref sparc-register-table reg))
	(vector-set! larceny-register-table reg (car rest))))

  (define millicode-procs '())

  (define (float-register-name reg)
    (string-append "%f" (number->string reg)))
    
  (define op car)
  (define op1 cadr)
  (define op2 caddr)
  (define op3 cadddr)
  (define tabstring (string #\tab))

  (define (heximm n)
    (if (>= n 16)
	(string-append tabstring "! 0x" (number->string n 16))
	""))

  (define (pcrel n)
    (string-append "." (if (>= n 0) "+" "") (number->string n)))

  (define (millicode-name offset . rest)
    (if (null? rest)
	(let ((probe (assv offset millicode-procs)))
	  (if probe
	      (cdr probe)
	      "[unknown]"))
	(set! millicode-procs
	      (cons (cons offset (car rest)) millicode-procs))))

  (define (millicode-call offset)
    (string-append tabstring "! " (millicode-name offset)))

  (define (plus/minus n)
    (cond ((< n 0)
	   (string-append " - " (number->string (abs n))))
	  ((and (= n 0) *format-instructions-pretty*) "")
	  (else
	   (string-append " + " (number->string n)))))

  (define (srcreg instr extractor)
    (if (mnemonic:freg? (op instr))
	(float-register-name (extractor instr))
	(larceny-register-name (extractor instr))))
	
  (define (sethi instr)
    (string-append (number->string (* (op1 instr) 1024)) ", "
		   (larceny-register-name (op2 instr))
		   (heximm (* (op1 instr) 1024))))

  (define (rrr instr)
    (string-append (larceny-register-name (op1 instr)) ", "
		   (larceny-register-name (op2 instr)) ", "
		   (larceny-register-name (op3 instr))))

  (define (rir instr)
    (string-append (larceny-register-name (op1 instr)) ", "
		   (number->string (op2 instr)) ", "
		   (larceny-register-name (op3 instr))
		   (heximm (op2 instr))))

  (define (sir instr)
    (string-append (srcreg instr op3) ", [ "
		   (larceny-register-name (op1 instr))
		   (plus/minus (op2 instr)) " ]"))

  (define (srr instr)
    (string-append (srcreg instr op3) ", [ "
		   (larceny-register-name (op1 instr)) "+"
		   (larceny-register-name (op2 instr)) " ]"))
      
  (define (lir instr)
    (string-append "[ " (larceny-register-name (op1 instr))
		   (plus/minus (op2 instr)) " ], "
		   (srcreg instr op3)))

  (define (lrr instr)
    (string-append "[ " (larceny-register-name (op1 instr)) "+"
		   (larceny-register-name (op2 instr)) " ], "
		   (srcreg instr op3)))

  (define (bimm instr addr)
    (string-append "#" (number->string (+ (op1 instr) addr))))

  (define (jmpli instr)
    (string-append (larceny-register-name (op1 instr)) 
		   (plus/minus (op2 instr)) ", "
		   (larceny-register-name (op3 instr))
		   (if (and (= (op1 instr) $r.globals)
			    use-larceny-registers)
		       (millicode-call (op2 instr))
		       (heximm (op2 instr)))))

  (define (jmplr instr)
    (string-append (larceny-register-name (op1 instr)) "+"
		   (larceny-register-name (op2 instr)) ", "
		   (larceny-register-name (op3 instr))))

;  (define (call instr a)
;    (pcrel (op1 instr)))

  (define (call instr addr)
    (string-append "#" (number->string (+ (op1 instr) addr))))

  ;; If we want to handle instruction aliases (clr, mov, etc) then
  ;; the structure of this procedure must change, because as it is,
  ;; the printing of the name is independent of the operand values.

  (define (format-instr i a larceny-names?)
    (set! use-larceny-registers larceny-names?)
    (let ((m (car i)))
      (string-append (number->string a)
		     tabstring
		     (symbol->string (mnemonic:name m))
		     (if (mnemonic:annul? m) ",a" "")
		     tabstring
		     (cond ((mnemonic:store? m) 
			    (if (mnemonic:immediate? m) (sir i) (srr i)))
			   ((mnemonic:load? m)
			    (if (mnemonic:immediate? m) (lir i) (lrr i)))
			   ((mnemonic:branch? m) (bimm i a))
			   ((mnemonic=? m 'sethi) (sethi i))
			   ((mnemonic=? m 'nop) "")
			   ((mnemonic=? m 'jmpl)
			    (if (mnemonic:immediate? m) (jmpli i) (jmplr i)))
			   ((mnemonic=? m 'call) (call i a))
			   ((mnemonic:immediate? m) (rir i))
			   (else (rrr i))))))

  (larceny-register-name $r.tmp0 "%tmp0")
  (larceny-register-name $r.result "%result")
  (larceny-register-name $r.argreg2 "%argreg2")
  (larceny-register-name $r.argreg3 "%argreg3")
  (larceny-register-name $r.tmp1 "%tmp1")
  (larceny-register-name $r.tmp2 "%tmp2")
  (larceny-register-name $r.reg0 "%r0")
  (larceny-register-name $r.reg1 "%r1")
  (larceny-register-name $r.reg2 "%r2")
  (larceny-register-name $r.reg3 "%r3")
  (larceny-register-name $r.reg4 "%r4")
  (larceny-register-name $r.reg5 "%r5")
  (larceny-register-name $r.reg6 "%r6")
  (larceny-register-name $r.reg7 "%r7")
  (larceny-register-name $r.e-top "%etop")
  (larceny-register-name $r.e-limit "%elim")
  (larceny-register-name $r.timer "%timer")
  (larceny-register-name $r.millicode "%millicode")
  (larceny-register-name $r.globals "%globals")
  (larceny-register-name $r.stkp "%stkp")       ; note: after elim

  (millicode-name $m.alloc "alloc")
  (millicode-name $m.alloci "alloci")
  (millicode-name $m.gc "gc")
  (millicode-name $m.addtrans "addtrans")
  (millicode-name $m.stkoflow "stkoflow")
  (millicode-name $m.stkuflow "stkuflow")
  (millicode-name $m.creg "creg")
  (millicode-name $m.creg-set! "creg-set!")
  (millicode-name $m.add "+")
  (millicode-name $m.subtract "- (binary)")
  (millicode-name $m.multiply "*")
  (millicode-name $m.quotient "quotient")
  (millicode-name $m.remainder "remainder")
  (millicode-name $m.divide "/")
  (millicode-name $m.modulo "modulo")
  (millicode-name $m.negate "- (unary)")
  (millicode-name $m.numeq "=")
  (millicode-name $m.numlt "<")
  (millicode-name $m.numle "<=")
  (millicode-name $m.numgt ">")
  (millicode-name $m.numge ">=")
  (millicode-name $m.zerop "zero?")
  (millicode-name $m.complexp "complex?")
  (millicode-name $m.realp "real?")
  (millicode-name $m.rationalp "rational?")
  (millicode-name $m.integerp "integer?")
  (millicode-name $m.exactp "exact?")
  (millicode-name $m.inexactp "inexact?")
  (millicode-name $m.exact->inexact "exact->inexact")
  (millicode-name $m.inexact->exact "inexact->exact")
  (millicode-name $m.make-rectangular "make-rectangular")
  (millicode-name $m.real-part "real-part")
  (millicode-name $m.imag-part "imag-part")
  (millicode-name $m.sqrt "sqrt")
  (millicode-name $m.round "round")
  (millicode-name $m.truncate "truncate")
  (millicode-name $m.apply "apply")
  (millicode-name $m.varargs "varargs")
  (millicode-name $m.typetag "typetag")
  (millicode-name $m.typetag-set "typetag-set")
  (millicode-name $m.break "break")
  (millicode-name $m.eqv "eqv?")
  (millicode-name $m.partial-list->vector "partial-list->vector")
  (millicode-name $m.timer-exception "timer-exception")
  (millicode-name $m.exception "exception")
  (millicode-name $m.singlestep "singlestep")
  (millicode-name $m.syscall "syscall")
  (millicode-name $m.bvlcmp "bvlcmp")
  (millicode-name $m.enable-interrupts "enable-interrupts")
  (millicode-name $m.disable-interrupts "disable-interrupts")
  (millicode-name $m.alloc-bv "alloc-bv")

  (set! format-instruction format-instr)
  'format-instruction)


; eof

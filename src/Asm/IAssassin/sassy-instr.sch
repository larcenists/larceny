;;; NASM/i386 macros for the MacScheme instruction set.
;;;
;;; $Id: i386-instr.asm 2715 2005-12-14 22:27:51Z tov $
;;; 
;;; Sections in this file
;;;   - Section 1: handy macros
;;;   - Section 2: utility macros for MAL instruction definitions
;;;   - Section 3: MAL instruction definitions
;;;   - Section 4: MAL primitive operations (OP1, OP2, OP2IMM, OP3)
;;; 
;;; Conventions in the generated code:
;;;   - Labels defined within the a codevector are local to the
;;;     codevector they are defined in (using the NASM convention
;;;     for local labels).
;;;   - The above implies that the JUMP instruction must know the
;;;     name of the codevector it is jumping to.
;;;   - Constants are emitted with symbolic names when possible.
;;;   - Any address that may escape to Scheme code (codevector ptr,
;;;     return address) must be aligned on a 4-byte boundary.  For
;;;     millicode calls this means inserting NOPs following the call
;;;     instruction; millicode stubs fix up the return address.
;;; 
;;; Conventions in this file:
;;;   - No concrete register names may be used!  Use symbolic names
;;;     only, to ease reassignments later (my Pentium 3 seems to have
;;;     preference for working in some registers over others; some
;;;     investigation must be done)
;;; 
;;; Performance, code size, etc:
;;;   - Using the low byte of a register when possible (eg for
;;;     tag testing) reduces code size and does not appear to
;;;     impact performance.
;;;   - 'add r, -1' generates a 32-bit datum; 'sub' might be better
;;;     if you know your constant is negative.
;;;   - the exception handling code is large.  Can we shrink further,
;;;     eg by encoding registers with arguments as well as restart
;;;     address in a literal following the call point?  Must pack 
;;; 	very densely to fit all in 4 bytes, but that would be a major
;;;     win; 3 would be better still.  Use variable-length encoding? 
;;;   - Generally search for OPTIMIZEME below
;;;
;;; Defines affecting the generated code:
;;;   UNSAFE_CODE        omit all type checks
;;;   UNSAFE_GLOBALS     omit undefined-checks on globals
;;;   INLINE_ALLOCATION  inline all allocation
;;;   INLINE_ASSIGNMENT  inline the write barrier (partially)

;; [Maybe (make-assembly-structure ...)]
(define current-sassy-assembly-structure 
  (make-parameter "current-sassy-assembly-structure" #f))

(define (fake-sassy-assembly-structure!)
  (current-sassy-assembly-structure 
   (make-assembly-structure '() (assembly-table) (make-user-data)))
  (unspecified))

(define (unsafe-globals)
  (unsafe-code))

(define sassy-instr-directives '())
  ;;'((macro comment (lambda x '(begin)))))

(define-syntax define-sassy-instr
  (syntax-rules ()
    ((_ (NAME ARGS ...) BODY ...)
     (define (NAME ARGS ...) (seqlist BODY ...))))) ; do this for now...

;; Apply f to args and pull out the resulting list.
;; (Seems trivial now; but idea is to move all the dependencies on the
;;  list structure into this function.)
(define (do-sassy-instr f . args)
  (apply f args))

;; Has to be a macro since order of eval is undef'd in Scheme
(define-syntax seqlist 
  (syntax-rules (cond let quasiquote repeat-times)
    ((seqlist) 
     (list))
    ((seqlist (repeat-times n EXP) EXPS ...)
     (seqlist (let loop ((i n)) (cond ((not (zero? i)) EXP (loop (- i 1))))) EXPS ...))
    ((_ (cond (Q A ...) ...) EXPS ...)
     (append (mcond '() (Q (seqlist A ...)) ...) 
             (seqlist EXPS ...)))
    ((_ (let ((I E) ...) BODY ...) EXPS ...)
     (append (let ((I E) ...) (seqlist BODY ...)) 
             (seqlist EXPS ...)))
    ((_ (let LOOP-ID ((I E) ...) BODY ...) EXPS ...)
     (append (let LOOP-ID ((I E) ...) (seqlist BODY ...))
             (seqlist EXPS ...)))
    ;; Note in below two clauses, first uses CONS, second uses APPEND
    ((_ (quasiquote EXP1) EXPS ...)
     (cons (quasiquote EXP1) (seqlist EXPS ...)))
    ((_ EXP1 EXPS ...)
     (let ((v EXP1))
       (append v (seqlist EXPS ...))))))

(define-syntax mcond
  (syntax-rules (else)
    ((_ end) end)
    ((_ end (else A)) A)
    ((_ end (Q A) C ...) (if Q A (mcond end C ...)))))
     

;; ;; NAME : Operand ... -> [Listof SassyInstr]
;; (define-sassy-instr (NAME ARGS ...) BODY)

(define (G_REG n) 
  (case n 
    ((0) $g.reg0) ((1) $g.reg1) ((2) $g.reg2) ((3) $g.reg3)
    ((4) $g.reg4) ((5) $g.reg5) ((6) $g.reg6) ((7) $g.reg7)
    ;; Bleah, Lars' naming convention got weird with below regs
    ;; TODO change the $r to $g before I merge this in.
    ((8) $r.reg8) ((9) $r.reg9) ((10) $r.reg10) ((11) $r.reg11)
    ((12) $r.reg12) ((13) $r.reg13) ((14) $r.reg14) ((15) $r.reg15)
    ((16) $r.reg16) ((17) $r.reg17) ((18) $r.reg18) ((19) $r.reg19)
    ((20) $r.reg20) ((21) $r.reg21) ((22) $r.reg22) ((23) $r.reg23)
    ((24) $r.reg24) ((25) $r.reg25) ((26) $r.reg26) ((27) $r.reg27)
    ((28) $r.reg28) ((29) $r.reg29) ((30) $r.reg30) ((31) $r.reg31)
    (else (error 'G_REG (string-append " unknown sw register "
                                       (number->string n))))))
(define (REG n) 
  (case n   
    ((eax ebx ecx edx edi esi esp ebp) n)
    ((1) $r.reg1) ((2) $r.reg2) ((3) $r.reg3) ((0) $r.reg0)
    (else (error 'REG (string-append " unknown register "
                                     (number->string n))))))

(define (REG_LOW n)
  (case n
    ((eax) 'al)
    ((ebx) 'bl)
    ((ecx) 'cl)
    ((edx) 'dl)
    ((1) $r.reg1.low) ((2) $r.reg2.low)
    (else (error 'REG (string-append " unknown low register "
                                     (number->string n))))))

;; *tries* to produce the low variant of hwreg
(define (try-low hwreg)
  (case hwreg
    ((eax)    'al)
    ((ebx)    'bl)
    ((ecx)    'cl)
    ((edx)    'dl)
    ((edi)    'edi)
    ((esi)    'esi)
    ((esp)    'esp)
    ((ebp)    'ebp)
    (else (error 'try-low hwreg " is not a symbolic hwreg..."))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Handy macros for this and that.  More fundamental stuff is
;;; defined in i386-machine.ah

(define (invert-cc cc)
  (let-syntax ((cases (syntax-rules ()
                        ((cases x (CC NCC) ...)
                         (case x
                           ((CC) 'NCC) ...
                           ((NCC) 'CC) ...)))))
    (cases cc (z nz) (e ne) (g ng) (l nl) (o no) (le g) (be a))))
    
(define (result-reg? n)      (or (eq? n $r.result)))
(define (hwreg_has_low r)    
  (case r
    ((eax ebx ecx edx) #t)
    ((edi esi esp ebp) #f)
    (else 
     (or (result-reg? r) (= r 1) (= r 2)))))
(define (is_hwreg n)         (or (result-reg? n) (<= 0 n 3)))
(define (fixnum n)           (arithmetic-shift n 2))
(define (roundup8 x)         (logand (+ x 7) (lognot 7)))
(define (words2bytes n)      (* n 4))
(define (stkslot n)          `(& ,$r.cont ,(+ STK_REG0 (words2bytes n))))
(define (framesize n)        (roundup8 (+ wordsize STK_OVERHEAD (words2bytes n))))
(define (recordedsize n)     (+ STK_OVERHEAD (words2bytes n)))
(define (t_label s)          s)
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Utility macros for MAL instruction definitions

(define-sassy-instr (ia86.mcall fcn name)
  ;; `(comment -- ,name)
  `(call	(& ,$r.globals ,fcn))
  `(align ,code_align))

;;; loadr targetreg, regno
;;; 	load HW register targetreg from VM register regno
	
(define-sassy-instr (ia86.loadr targetreg regno)
  (cond ((is_hwreg regno)
         `(mov ,targetreg ,(REG regno)))
        (else 
         `(mov ,targetreg (& ,$r.globals ,(G_REG regno))))))

;;; storer regno, sourcereg
;;;     store VM register regno from HW register sourcereg
;;;     Does not destroy sourcereg

(define-sassy-instr (ia86.storer regno sourcereg)
  (cond ((is_hwreg regno)
         `(mov	,(REG regno) ,sourcereg))
        (else
         `(mov  (& ,$r.globals ,(G_REG regno)) ,sourcereg))))

;;; loadc hwreg, slot
;;;	Load constant vector element 'slot' into hwreg

(define (intel-reg? r)
  (memq r '(eax ebx ecx edx edi esi esp ebp al  bl  cl  dl)))

(define-sassy-instr (assert-intel-reg hwreg)
  (cond ((not (intel-reg? hwreg))
         (error 'assert-intel-reg hwreg))))
	
(define-sassy-instr (ia86.loadc hwreg slot)
  (assert-intel-reg hwreg)
  `(mov	,hwreg (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_CONSTVECTOR)))
  `(mov	,hwreg (& ,hwreg ,(+ (- $tag.vector-tag) (words2bytes (+ slot 1))))))

;;; write_barrier r1 r2
;;;	Move values from hardware registers r1 and r2 to RESULT 
;;; 	and SECOND and perform a write barrier.  r1 and r2 may 
;;; 	be -1, in which case the value must already be in RESULT 
;;; 	and SECOND.
;;;
;;;     For INLINE_ASSIGNMENT, test the r2 value and skip the barrier
;;;     if the low bit is not 1.

(define-sassy-instr (ia86.write_barrier r1 r2)
  (cond
   ((inline-assignment)
    (let ((L0 (fresh-label)))
      (cond ((and (not (= r2 -1))
                  (is_hwreg r2))
             `(test	,(REG r2) 1)
             `(jz short ,L0)
             `(mov	,$r.second ,(REG r2)))
            (else
             (cond ((not (= r2 -1))
                    `(mov	,$r.second ,(REG r2))))
             `(test	,$r.second 1)
             `(jz short ,L0)))
      (cond ((not (= r1 -1))
             `(mov	,$r.result ,(REG r1))))
      (ia86.mcall $m.partial-barrier 'partial-barrier)
      `(label ,L0)))
   (else 
    (cond ((not (= r1 -1))
           `(mov ,$r.result ,(REG r1))))
    (cond ((not (= r2 -1))
           `(mov ,$r.second ,(REG r2))))
    (ia86.mcall $m.full-barrier 'full-barrier))))
	
;;; timer_check
;;;	decrement timer and take interrupt if zero

(define-sassy-instr (ia86.timer_check)
  (let ((L1 (fresh-label)))
    `(dec (dword (& ,$r.globals ,$g.timer)))
    `(jnz short ,L1)
    (ia86.mcall	$m.timer-exception 'timer-exception)
    `(label ,L1)))

;;; exception_noncontinuable excode
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is noncontinuable
	
(define-sassy-instr (ia86.exception_noncontinuable excode)
  ;; `(comment -- exception ,excode)
  `(call	(& ,$r.globals ,$m.exception))
  `(dwords	,excode))
		
;;; exception_continuable excode restart
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is continuable at the address
;;;	of the second argument.  Smallest code probably results
;;;	from just inserting a jump.
;;;
;;;	Moving the exception code to globals takes 8 bytes using
;;;		mov	dword [GLOBALS+offset] ,ex
;;;     even if offset is short.  Placing the exception code in two 
;;; 	bytes at the return address saves six bytes, though the 
;;; 	handler code must adjust the return address.
;;;
;;;	Important that M_EXCEPTION is at short offset from
;;;	globals, to save 3 bytes!  (It can be a negative offset.)

(define-sassy-instr (ia86.exception_continuable excode restart)
  ;; `(comment -- exception ,excode)
  `(call	(& ,$r.globals ,$m.exception))
  `(dwords	,excode)
  `(align	,code_align)
  `(jmp	,restart))

;;; alloc
;;;	Given fixnum number of words in RESULT, allocate
;;;	a structure of that many bytes and leave a raw pointer
;;;	in RESULT.

(define-sassy-instr (ia86.alloc)
  (cond ((inline-allocation)
         (let ((L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L1)
	   `(mov ,$r.temp (& ,$r.globals ,$g.etop))
           `(lea ,$r.temp (& ,$r.temp ,$r.result 4)) ; allocate and round
           `(and ,$r.temp -8)                  ;  up to 8-byte boundary
	   `(cmp ,$r.temp ,$r.cont)
	   `(jle short ,L2)
	   (ia86.mcall $m.morecore 'morecore)
	   `(jmp short ,L1)
           `(label ,L2)
           `(mov ,$r.result (& ,$r.globals ,$g.etop))
	   `(mov (& ,$r.globals ,$g.etop) ,$r.temp)))
        (else
         (ia86.mcall $m.alloc 'alloc))))

;;; const2reg hwreg const
;;; Move a constant to a register *without changing the flags*

(define-sassy-instr (ia86.const2reg hwreg const)
  (assert-intel-reg hwreg)
  `(mov ,hwreg ,const))         ; 5 bytes

;;; const2regf hwreg const
;;; Move a constant to a register, possibly killing the flags
;;; Makes for smaller code size.

(define-sassy-instr (ia86.const2regf hwreg const)
  (assert-intel-reg hwreg)
  (cond ((= const 0)
	 `(xor ,hwreg ,hwreg))  ; 2 bytes
	((= const 1)
         `(xor ,hwreg ,hwreg) ; 3 bytes
         `(inc ,hwreg))
	((= const -1)
         `(xor ,hwreg ,hwreg) ; 3 bytes
         `(dec ,hwreg))
	(else
	 `(mov ,hwreg ,const)))); 5 bytes
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; MacScheme machine instruction set

(define-sassy-instr (ia86.T_ALIGN x)
  `(align	,x))

(define-sassy-instr (ia86.T_LABEL x)
  `(label ,(t_label x)))

;;; CONST is broken up into two instructions depending on the
;;; type of the constant.  The argument to T_CONST_IMM is a bitpattern
;;; or symbolic constant name, to T_CONST_CONSTVECTOR it is the
;;; constant vector index.
	
(define-sassy-instr (ia86.T_CONST_IMM x)
  (ia86.const2regf $r.result x))

(define-sassy-instr (ia86.T_CONST_CONSTVECTOR x)
  (ia86.loadc	$r.result x))

(define-sassy-instr (ia86.T_CONST_SETREG_IMM x regno)
  (cond ((is_hwreg regno)
         (ia86.const2regf (REG regno) x))
        (else
         (ia86.const2regf $r.temp x) ; this could go through RESULT if we liked.
         ;; (should we instead mov directly to the slot for the swreg?)
	 (ia86.storer regno $r.temp))))

(define-sassy-instr (ia86.T_CONST_SETREG_CONSTVECTOR x regno)
  (cond ((is_hwreg regno)
         (ia86.loadc (REG regno) x))
        (else
	 (ia86.loadc $r.temp x)
	 (ia86.storer regno $r.temp))))

;;; OPTIMIZEME:	if #!undefined fit in a byte, then we could do a byte
;;; compare here, at least.  (Why does it not fit in a byte?)

(define-sassy-instr (ia86.T_GLOBAL x)
  (ia86.T_GLOBAL_SETREG x $r.result))

(define-sassy-instr (ia86.T_GLOBAL_SETREG x hwreg)
  (assert-intel-reg hwreg)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    `(label ,L0)
    (ia86.loadc $r.temp x)
    `(mov ,hwreg (& ,$r.temp ,(- $tag.pair-tag)))
    (cond ((not (unsafe-globals))
           `(cmp ,hwreg ,$imm.undefined)
           `(jne short ,L1)
           `(mov ,$r.result ,$r.temp)
           (ia86.mcall $m.global-ex 'global-ex)
           `(jmp short ,L0)
           `(label ,L1)))))

(define-sassy-instr (ia86.T_SETGLBL x)
  `(mov	,$r.second ,$r.result)
  (ia86.loadc	$r.result x)
  `(mov	(& ,$r.result ,(- $tag.pair-tag)) ,$r.second)
  (ia86.write_barrier -1 -1))

(define-sassy-instr (ia86.T_CONST_SETGLBL_IMM x glbl)
  (ia86.const2regf $r.second x)
  (ia86.loadc	$r.result glbl)
  `(mov	(& ,$r.result ,(- $tag.pair-tag)) ,$r.second)
  (ia86.write_barrier -1 -1))

(define-sassy-instr (ia86.T_CONST_SETGLBL_CONSTVECTOR x glbl)
  (ia86.T_CONST_CONSTVECTOR x)
  (ia86.T_SETGLBL glbl))

(define-sassy-instr (ia86.T_REG_SETGLBL regno x)
  (ia86.loadc	$r.result x)
  `(mov	(& ,$r.result ,(- $tag.pair-tag)) ,(REG regno))
  (ia86.write_barrier -1 regno))

(define-sassy-instr (ia86.T_LEXICAL rib off)
  (cond ((> rib 0)
         `(mov ,$r.temp (& ,$r.reg0 ,(+ PROC_REG0 (- $tag.procedure-tag))))
         (repeat-times (- rib 1) 
                       `(mov ,$r.temp (& ,$r.temp ,(+ PROC_REG0 (- $tag.procedure-tag)))))
         `(mov ,$r.result (& ,$r.temp ,(+ PROC_REG0 (- $tag.procedure-tag) (words2bytes off)))))
        (else
         `(mov ,$r.result (& ,$r.reg0 ,(+ PROC_REG0 (- $tag.procedure-tag) (words2bytes off)))))))

(define-sassy-instr (ia86.T_SETLEX rib off)
  (cond ((> rib 0)
         `(mov ,$r.temp (& ,$r.reg0 ,(+ PROC_REG0 (- $tag.procedure-tag))))
         (repeat-times (- rib 1) `(mov ,$r.temp (& ,$r.temp ,(+ PROC_REG0 (- $tag.procedure-tag)))))
         `(mov (& ,$r.temp ,(+ PROC_REG0 (- $tag.procedure-tag) (words2bytes off))) ,$r.result))
        (else
         `(mov (& ,$r.reg0 ,(+ PROC_REG0 (- $tag.procedure-tag) (words2bytes off))) ,$r.result))))
	
(define-sassy-instr (ia86.T_STACK slot)
  `(mov	,$r.result ,(stkslot slot)))

(define-sassy-instr (ia86.T_SETSTK slot)
  `(mov	,(stkslot slot) ,$r.result))

(define-sassy-instr (ia86.T_LOAD regno slot)
  (cond ((is_hwreg regno)
         `(mov ,(REG regno) ,(stkslot slot)))
        (else
         `(mov ,$r.temp ,(stkslot slot))
         (ia86.storer regno $r.temp))))

(define-sassy-instr (ia86.T_STORE regno slot)
  (cond ((is_hwreg regno)
         `(mov	,(stkslot slot) ,(REG regno)))
        (else
	 (ia86.loadr	$r.temp regno)
	 `(mov	,(stkslot slot) ,$r.temp))))

(define-sassy-instr (ia86.T_REG regno)
  (ia86.loadr $r.result regno))
	
;;; Does not destroy RESULT.  The peephole optimizer uses that fact.
(define-sassy-instr (ia86.T_SETREG regno)
  (ia86.storer	regno $r.result))

(define-sassy-instr (ia86.T_MOVEREG regno1 regno2)
  (cond ((is_hwreg regno1)
	 (ia86.storer regno2 (REG regno1)))
	((is_hwreg regno2)
	 (ia86.loadr (REG regno2) regno1))
	(else
         (ia86.loadr $r.temp regno1)
         (ia86.storer regno2 $r.temp))))

(define-sassy-instr (ia86.init_closure r)
  (let ((regno (cond ((> r *lastreg*)
                      (- *lastreg* 1))
                     (else 
                      r)))
        (L1 (fresh-label)))
    (cond 
     ((> r *lastreg*)
      `(mov (& ,$r.globals ,$g.stkp) ,$r.cont)     ; Need a working register!
      `(mov (& ,$r.globals ,$g.result) ,$r.result) ; Save for later
      `(add ,$r.result ,(+ PROC_REG0 (words2bytes LASTREG)))
      (ia86.loadr $r.cont LASTREG)
      `(label ,L1)
      `(mov ,$r.temp (& ,$r.cont ,(- $tag.pair-tag)))
      `(mov (& ,$r.result) ,$r.temp)
      `(add ,$r.result ,wordsize)
      `(mov ,$r.cont (& ,$r.cont ,(+ (- $tag.pair-tag) wordsize)))
      `(cmp ,$r.cont ,$imm.null)
      `(jne short ,L1)
      `(mov ,$r.cont (& ,$r.globals ,$g.stkp))
      `(mov ,$r.result (& ,$r.globals ,$g.result))))
    
    (let rep ((regno regno))
      (cond 
       ((>= regno 0)
        (cond ((is_hwreg regno)
               `(mov (& ,$r.result ,(+ PROC_REG0 (words2bytes regno)))
                     ,(REG regno)))
              (else
               (ia86.loadr $r.temp regno)
               `(mov (& ,$r.result ,(+ PROC_REG0 (words2bytes regno))) ,$r.temp)))
        (rep (- regno 1)))))

    `(add ,$r.result.low ,$tag.procedure-tag)))

(define-sassy-instr (ia86.T_LAMBDA codevec constvec n)
  ;; arguments are codevector offset, constant vector offset, and n
  (ia86.const2regf $r.result (fixnum (+ PROC_HEADER_WORDS PROC_OVERHEAD_WORDS n 1)))
  (ia86.alloc)
  `(mov (dword (& ,$r.result)) ,(logior
                             (arithmetic-shift (words2bytes (+ PROC_OVERHEAD_WORDS n 1))
                                               8)
                             $hdr.procedure))
  `(push ,$r.reg0) ; REG0 temp ptr to constants (saves size + reload of TEMP)
  `(mov	,$r.reg0 (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_CONSTVECTOR)))
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.vector-tag) (words2bytes (+ codevec 1)))))
  `(mov	(dword (& ,$r.result ,PROC_CODEVECTOR_NATIVE)) ,$r.temp)
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.vector-tag) (words2bytes (+ constvec 1)))))
  `(pop ,$r.reg0)  ; restore REG0
  `(mov	(dword (& ,$r.result ,PROC_CONSTVECTOR      )) ,$r.temp)
  (ia86.init_closure n))

(define-sassy-instr (ia86.T_LEXES n)
  ;; argument is n
  (ia86.const2regf $r.result (fixnum (+ PROC_HEADER_WORDS PROC_OVERHEAD_WORDS n 1)))
  (ia86.alloc)
  `(mov	(dword (& ,$r.result)) ,(logior
                             (arithmetic-shift (words2bytes (+ PROC_OVERHEAD_WORDS n 1))
                                               8)
                             $hdr.procedure))
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_CODEVECTOR_NATIVE)))
  `(mov	(dword (& ,$r.result ,PROC_CODEVECTOR_NATIVE)) ,$r.temp)
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_CONSTVECTOR)))
  `(mov	(dword (& ,$r.result ,PROC_CONSTVECTOR)) ,$r.temp)
  (ia86.init_closure n))

(define-sassy-instr (ia86.T_ARGSEQ n)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(cmp	,$r.result ,(fixnum n))
           `(je short ,L1)
           `(mov (& ,$r.globals ,$g.second) ,(fixnum n))
           (ia86.mcall	$m.argc-ex 'argc-ex)
           `(jmp short	,L0)
           `(label ,L1)))))


(define-sassy-instr (ia86.T_ARGSGE n)
  (ia86.const2regf $r.second (fixnum n))
  (cond ((and (not (unsafe-code))
              (> n 0))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(cmp ,$r.result ,$r.second)
           `(jge short ,L1)
           (ia86.mcall $m.argc-ex 'argc-ex)
           `(jmp short ,L0)
           `(label ,L1))))
  (ia86.mcall $m.varargs 'varargs))

;;; (See sassy-invoke.sch for the T_INVOKE and T_GLOBAL_INVOKE
;;;  definitions that used to be here.)
	
;;; Allocate the frame but initialize only the basic slots
;;; and any pad words.  Leave RESULT clear at the end; this
;;; fact is used by T_SAVE1 below.

(define-sassy-instr (ia86.T_SAVE0 n)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    `(label ,L0)
    `(sub ,$r.cont ,(framesize n))
    `(cmp ,$r.cont (& ,$r.globals ,$g.etop))
    `(jge short ,L1)
    `(add ,$r.cont ,(framesize n))
    (ia86.mcall $m.stkoflow 'stkoflow)
    `(jmp short ,L0)
    `(label ,L1)
    `(mov (dword (& ,$r.cont)) ,(recordedsize n))
    ;; Not necessary to store reg0 here, this is handled
    ;; explicitly by the generated code.
    `(xor	,$r.result ,$r.result)
    `(mov	(dword (& ,$r.cont ,STK_RETADDR)) ,$r.result)
    (cond ((= (- (framesize n) (recordedsize n)) 8)
           ;; We have a pad word at the end -- clear it
           `(mov (dword ,(stkslot (+ n 1))) ,$r.result)))))

;;; Initialize the numbered slot to the value of RESULT.
;;; Using RESULT is probably OK because it is almost certainly 0
;;; after executing T_SAVE0 and only T_STORE instructions
;;; after that.

(define-sassy-instr (ia86.T_SAVE1 n)
  `(mov	(dword ,(stkslot n)) ,$r.result))

;;; T_SAVE may still be emitted by the assembler when peephole 
;;; optimization is disabled.

(define-sassy-instr (ia86.T_SAVE n)
  `(T_SAVE0 ,n)
  (let rep ((slotno 1))
    (cond ((<= slotno n)
           `(T_SAVE1 ,slotno)
           (rep (+ slotno 1))))))

'(define-sassy-instr (ia86.T_SETRTN lbl)
  ;;; This has not been optimized.  (19 bytes)
  ;;; Ryan points out that we could grab the base address 
  ;;; of the codevector via R0 instead of doing the call below.
  (let ((L1 (fresh-label)))
    ;; `(mov	(dword (& CONT ,STK_RETADDR)) (t_label ,lbl))
    `(call $eip) ;; puts $eip into first element of GLOBALS.
    `(label ,L1) ;; absolute name for $eip
    `(pop ,$r.temp)  ;; stash return address in ,TEMP
    `(sub ,$r.temp (reloc rel ,L1)) ;; adjust to point to base of segment
    `(add ,$r.temp (reloc rel ,(t_label lbl)))  ;; adjust to point to lbl
    `(mov (& ,$r.cont ,STK_RETADDR) ,$r.temp)   ;; save in ret addr slot
    ))

;; Alternate version (15 bytes).  
;; (But SETRTN/INVOKE may be inspired by prior approach...)
(define-sassy-instr (ia86.T_SETRTN lbl)
  `(mov ,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_CODEVECTOR_NATIVE)))
  `(add ,$r.temp (reloc abs 
                    ,(t_label lbl)
                    ,(+ (- $tag.bytevector-tag))))
  `(mov (& ,$r.cont ,STK_RETADDR) ,$r.temp))

(define-sassy-instr (ia86.T_RESTORE n)
  (let rep ((slotno 0))
    (cond ((<= slotno n)
           (cond ((is_hwreg slotno)
                  `(mov ,(REG slotno) (dword ,(stkslot slotno))))
                 (else
                  `(mov ,$r.temp (dword ,(stkslot slotno)))
                  `(mov (& ,$r.globals ,(G_REG slotno)) ,$r.temp)))
           (rep (+ slotno 1))))))

(define-sassy-instr (ia86.T_POP n)
  `(add	,$r.cont ,(framesize n)))
	
(define-sassy-instr (ia86.T_POPSTK)
  (error 'T_POPSTK "not implemented -- students only"))

'(define-sassy-instr (ia86.T_RETURN)
  `(jmp (& ,$r.cont ,STK_RETADDR)))

;; one extra byte, but... if matched with the call's in setrtn/invoke,
;; *much* faster than the above... 
;; [[ if not matched, then we end up slower... ugh]]
(define-sassy-instr (ia86.T_RETURN)
  `(push (& ,$r.cont ,STK_RETADDR))
  `(ret))

;;; (See sassy-invoke.sch for the T_APPLY definition that used to be here.)
	
(define-sassy-instr (ia86.T_NOP)
  `(nop)) ;; The interface doesn't actually support empty lists, I think...

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; of the Petit implementation; we can almost certainly remove it
;;; after we have a complete Sassy backend).

(define-sassy-instr (ia86.T_JUMP levels label name)
  (ia86.T_JUMP* levels label #f))

(define-sassy-instr (ia86.T_SETRTN_JUMP levels label)
  (let ((ign (set! *did-emit-setrtn-jump* #t)))
    (ia86.T_JUMP* levels label #t)))

(define (emit-setrtn-jump-patch-code as)
  (define (emit x) (apply emit-sassy as x))
  (emit `(label setrtn-jump-patch-code-label))
  (emit `(pop (& ,$r.cont ,STK_RETADDR)))  ;; pre-aligned return address
  (emit `(jmp ,$r.temp)))
  
(define-sassy-instr (ia86.T_JUMP* levels label setrtn?)
  (ia86.timer_check)
  (let ((offset
	 (let loop ((obj (current-sassy-assembly-structure)))
	   (cond ((not obj) ;; Didn't find our label by following
		  ;; chain of structures; must be in current one
		  #f)
		 ((find-label obj label) => (lambda (entry) (cdr entry)))
		 (else (loop (as-parent obj)))))))
    (repeat-times levels `(mov ,$r.reg0 (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_REG0))))
    ;; Now REG0 holds the closure we're jumping into; calculate the
    ;; address as start of codevector plus offset from above.
    (cond (offset
           `(lea ,$r.temp (& ,$r.reg0 ,(- $tag.procedure-tag)))
           `(mov ,$r.temp (& ,$r.temp ,PROC_CODEVECTOR_NATIVE))
           `(lea ,$r.temp (& ,$r.temp ,(+ (- $tag.bytevector-tag) offset)))
           (cond (setrtn?
                  `(align ,code_align -1)
                  `(call setrtn-jump-patch-code-label))
                 (else
                  `(jmp ,$r.temp))))
          (else
           ;; If offset is false, then this must be a label in the
           ;; code vector we are currently attempting to construct.
           ;; At least, that's Felix's impression has after a
           ;; discussion with Will on the semantics of MacScheme's
           ;; JUMP.  Therefore emit a jmp and let Sassy handle it.
           (cond (setrtn?
                  (let ((ign '(begin (display "UGH!!!  MUST BE A BETTER WAY!")
                                    (newline))))
                    ;; We just clobbered reg0 above, but the reasoning above
                    ;; implies that this is the same codevector, so its okay.
                    `(mov ,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) PROC_CODEVECTOR_NATIVE)))
                    `(add ,$r.temp (reloc abs 
                                      ,(t_label (compiled-procedure 
                                                 (current-sassy-assembly-structure) 
                                                 label))
                                      ,(- $tag.bytevector-tag))))
                  `(align ,code_align -1)
                  `(call setrtn-jump-patch-code-label))
                 (else
                  `(jmp ,(t_label (compiled-procedure 
                                   (current-sassy-assembly-structure) 
                                   label)))))))))

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

(define-sassy-instr (ia86.T_SKIP lbl)
  `(jmp	,(t_label lbl)))

(define-sassy-instr (ia86.T_SKIPF lbl)
  `(cmp	,$r.result.low ,$imm.false)
  `(je	,(t_label lbl)))

(define-sassy-instr (ia86.T_BRANCH lbl)
  `(dec	(dword (& ,$r.globals ,$g.timer)))
  `(jnz	,(t_label lbl))
  (ia86.mcall	$m.timer-exception 'timer-exception)
  `(jmp	,(t_label lbl)))

(define-sassy-instr (ia86.T_BRANCHF lbl)
  (ia86.timer_check)
  `(cmp	,$r.result.low ,$imm.false)
  `(je	,(t_label lbl)))

(define-sassy-instr (ia86.T_REG_BRANCHF regno lbl a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  `(cmp	,(try-low (REG regno)) ,$imm.false)
  `(je	,(t_label lbl)))

(define-sassy-instr (ia86.T_CHECK w x y lbl)
  `(cmp	,$r.result.low ,$imm.false)
  `(je	,(t_label lbl)))

;; Felix thinks we shouldn't be seeing these.  And yet we do.
(define-sassy-instr (ia86.T_REG_CHECK regno lbl)
  `(cmp	,(try-low (REG regno)) ,$imm.false)
  `(je	,(t_label lbl)))

;; Call handler for noncontinuable exception with RESULT,
;; SECOND, and THIRD defined.

(define-sassy-instr (ia86.T_TRAP w x y z)
	;; Order matters here, because SECOND is TEMP and
	;; may be destroyed by loading of THIRD
  (cond ((not (= w 0))
         (ia86.loadr $r.result w)))
  (cond ((not (= y 0))
	   ;; OPTIMIZEME: optimize for case when %3 is HW reg
	   ;; (this will however have almost no impact)
         (ia86.loadr	$r.temp y)
         `(mov (& ,$r.globals ,$g.third) ,$r.temp)))
  (cond ((not (= x 0))
         (ia86.loadr $r.second x))
        (else 
         `(xor ,$r.second ,$r.second)))
  (ia86.exception_noncontinuable z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Helper macros for primitive operations

;;; setcc cc
;;;	Set RESULT to true if jcc jumps.
;;; 
;;;	It is probably the case that SECOND is available here,
;;;	but I'm not using that fact yet.
	
;(define-sassy-macro (ia86.setcc cc)				; 11 bytes, no jump
;	set,cc	RESULT_LOW		; 2 bytes
;	and	RESULT, 1		; 3 bytes
;	shl	RESULT, 2		; 3 bytes
;	or	RESULT_LOW, TRUE_CONST	; 3 bytes
;)

;;; Jesse thinks that the above is buggy (should be or'ing with FALSE_CONST)

;;; TRUE=6
;;; FALSE=2

(define-sassy-instr (ia86.setcc/cmov hwreg cc) ; 11 bytes, kills TEMP
  (assert-intel-reg hwreg) 
  (let ((cmovcc  (string->symbol (string-append "cmov" (symbol->string cc)))))
    `(mov ,hwreg ,$imm.false)           ; 5 bytes
    `(lea ,$r.temp  (& ,hwreg 4))          ; 3 bytes
    `(,cmovcc ,hwreg ,$r.temp)))           ; 3 bytes

(define-sassy-instr (ia86.setcc/set hwreg cc)	; ~12 bytes
  (assert-intel-reg hwreg) 
  (let ((setcc  (string->symbol (string-append "set" (symbol->string cc)))))
    `(,setcc ,(REG_LOW hwreg))          ; 3 bytes
    `(and ,hwreg 1)                     ; 3 bytes
    `(shl ,hwreg 2)                     ; 3 bytes
    `(or ,(REG_LOW hwreg) ,$imm.false))); 3 bytes

(define-sassy-instr (ia86.setcc/set-2 hwreg cc)	; 13 bytes
  (assert-intel-reg hwreg) 
  (let ((setcc  (string->symbol (string-append "set" (symbol->string cc)))))
    `(,setcc ,(REG_LOW hwreg))          ; 3 bytes
    `(and ,hwreg 1)                     ; 3 bytes
    `(lea ,hwreg (& (* 4 ,hwreg) 2))))  ; 7 bytes

(define-sassy-instr (ia86.setcc/set-3 hwreg cc)	; 12 bytes, kills TEMP
  (assert-intel-reg hwreg) 
  (let ((setcc  (string->symbol (string-append "set" (symbol->string cc)))))
    `(,setcc ,(REG_LOW hwreg))          ; 3 bytes
    `(xor ,$r.temp ,$r.temp)                  ; 2 bytes
    `(and ,hwreg 1)                     ; 3 bytes
    `(lea ,hwreg                        ; 4 bytes
          (& (* 4 ,hwreg) ,$r.temp 2))))

(define-sassy-instr (ia86.setcc/jmp hwreg cc)	; ~10 bytes, jumps
  (assert-intel-reg hwreg)
  (let ((L1 (fresh-label))
        (jcc (string->symbol (string-append "j" (symbol->string cc)))))
    (ia86.const2reg hwreg $imm.true)	; 5 bytes
    `(,jcc short ,L1)			; 2 bytes
    `(sub	,(try-low hwreg) 4)	; 3 bytes (would be 2 if RESULT=eax)
    `(label ,L1)))

(define-sassy-instr (ia86.setcc hwreg cc)
  (cond 
   (#f
    (ia86.setcc/cmov hwreg cc))
   (#f ; (hwreg_has_low hwreg)
    (ia86.setcc/set  hwreg cc))
   (else
    (ia86.setcc/jmp  hwreg cc))
   ))


;;; double_tag_predicate ptrtag, hdr
;;;	Set RESULT to #t if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise set RESULT
;;;	to #f.

(define-sassy-instr (ia86.double_tag_predicate ptrtag hdr)
  (ia86.double_tag_test $r.result ptrtag hdr)
  (ia86.setcc $r.result 'z))

;;; fixnum_test_temp_is_free reg
;;;	Test reg for fixnum-ness and clear zero flag if fixnum.  OK to
;;;	destroy TEMP.
	
(define-sassy-instr (ia86.fixnum_test_temp_is_free regno)
  (cond ((is_hwreg regno) 
         (cond 
          ((hwreg_has_low regno)
           `(test	,(REG_LOW regno) ,fixtag_mask))
          (else
           ;; test	REG,reg, fixtag_mask
           ;; Above is 6 bytes, below is 4 bytes.  Performance?
           `(mov	,$r.temp ,(REG regno))
           `(test	,$r.temp.low ,fixtag_mask))))
        (else
         `(test	(byte (& ,$r.globals ,(G_REG regno))) ,fixtag_mask))))

;;; single_tag_test ptrtag
;;;	Leave zero flag set if hwreg contains a value with the given
;;;     3-bit tag.

(define-sassy-instr (ia86.single_tag_test hwreg x)
  (assert-intel-reg hwreg)
  `(lea	,$r.temp (& ,hwreg ,(- 8 x)))
  `(test	,$r.temp.low 7))
	
;;; single_tag_test_ex ptrtag, exception_code
;;;	Unless in unsafe mode, test the pointer in RESULT for the
;;;	tag and signal an exception if it does not match.

(define-sassy-instr (ia86.single_tag_test_ex hwreg x y)
  (assert-intel-reg hwreg)
  (cond 
   ((not (unsafe-code))
    (let ((L0 (fresh-label))
          (L1 (fresh-label)))
      `(label ,L0)
      (ia86.single_tag_test hwreg x)
      `(jz short ,L1)
      (ia86.exception_continuable y L0)
      `(label ,L1)))))

;;; double_tag_test ptrtag, hdr
;;;	Set the zero flag if hwreg has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise reset the flag.
;;;     If zero flag is set, leaves the header field in TEMP.

(define-sassy-instr (ia86.double_tag_test hwreg x y)
  (let ((L1 (fresh-label)))
    (ia86.single_tag_test hwreg x)
    `(jnz short ,L1)
    `(mov	,$r.temp (& ,hwreg ,(- x)))
    `(cmp	,$r.temp.low ,y)
    `(label ,L1)))
	
;;; fixnum_arithmetic regno, operation, undo-operation, ex
	
(define-sassy-instr (ia86.fixnum_arithmetic regno y z ex)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           `(jnz short ,L1)
           (ia86.loadr	$r.temp regno)
           `(,y	,$r.result ,$r.temp)
           `(jno short ,L2)
           `(,z	,$r.result ,$r.temp)
           `(label ,L1)
           (ia86.exception_continuable ex L0)	; second is tmp so 2nd arg is in place
           `(label ,L2)))
        ((is_hwreg regno)
         `(,y	,$r.result ,(REG regno)))
        (else
         `(,y	,$r.result (& ,$r.globals ,(G_REG regno))))))
	
(define-sassy-instr (ia86.trusted_fixnum_compare sregno dregno regno y)
  (cond ((is_hwreg regno)
         `(cmp	,(REG sregno) ,(REG regno)))
        (else
         `(cmp	,(REG sregno) (& ,$r.globals ,(G_REG regno)))))
  (ia86.setcc	(REG dregno) y))

;;; fixnum_compare reg, cc, ex
;;; OPTIMIZEME for whenever ,x is a hwreg

(define-sassy-instr (ia86.fixnum_compare regno y z)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           `(jz short ,L1)
           (ia86.loadr	$r.second regno)
           (ia86.exception_continuable z L0)         ; second is tmp so 2nd arg is in place
           `(label ,L1 ))))
  (cond
   ((is_hwreg regno)
    `(cmp	,$r.result ,(REG regno)))
   (else
    `(cmp	,$r.result (& ,$r.globals ,(G_REG regno)))))
  (ia86.setcc $r.result y))

;;; fixnum_shift r2, operation, ex
;;; 
;;; Shift count must be in CL if it is not constant
;;; OPTIMIZEME: we can do better than what I do here.
	
(define-sassy-instr (ia86.fixnum_shift x y z)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp x)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           (ia86.loadr	$r.second x)
           `(jz short ,L2)
           `(label ,L1)
           (ia86.exception_continuable z L0)
           `(label ,L2)
           `(cmp	,$r.temp ,(fixnum 32))	; SECOND is ,TEMP
           `(jge short ,L1)))
        (else
         (ia86.loadr	$r.temp x)))
  `(shr	,$r.temp 2)
  `(mov	(& ,$r.globals ,G_REGALIAS_ECX) ecx)
  `(mov	cl ,$r.temp.low)
  `(,y	,$r.result cl)
  `(mov	ecx (& ,$r.globals ,G_REGALIAS_ECX))
  (cond ((not (eq? y 'shl))
         ;; Right shifts: mask out low bits
         ;; OPTIMIZEME: if RESULT were eax, masking RESULT_LOW with a byte
         ;; would save one byte here.
         `(and	,$r.result ,(lognot fixtag_mask)))))
	
;;; generic_arithmetic sregno, dregno, tmpreg, regno, operation, undo-operation, millicode
;;; Note that sregno and dregno are always hw registers
	
(define-sassy-instr (ia86.generic_arithmetic sregno dregno regno y z millicode)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(or	,$r.temp ,(REG sregno))
    `(test	,$r.temp.low ,fixtag_mask)
    (ia86.loadr	$r.temp regno)
    `(jnz short ,L1)
    (cond ((not (equal? dregno sregno))
           `(mov ,(REG dregno) ,(REG sregno))))
    `(,y	,(REG dregno) ,$r.temp)
    `(jno short ,L2)
    `(,z	,(REG dregno) ,$r.temp)
    `(label ,L1)
    (cond ((not (result-reg? sregno))
           `(mov ,$r.result ,(REG sregno))))
    (ia86.mcall	millicode y)
    (cond ((not (result-reg? dregno))
           `(mov ,(REG dregno) ,$r.result)))
    `(label ,L2 )))

(define-sassy-instr (ia86.generic_compare_branchf hwreg regno jnc mcode L a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(or	,$r.temp ,hwreg)
    `(test	,$r.temp.low ,fixtag_mask)
    (ia86.loadr	$r.second regno)
    `(jz short ,L1)
    (cond ((not (result-reg? hwreg))
           `(mov ,$r.result ,hwreg)))
    (ia86.mcall	mcode jnc)
    `(cmp	,$r.result.low ,$imm.false)
    `(je	,(t_label L))
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp	,hwreg ,$r.second)
    `(,jnc ,(t_label L))
    `(label ,L2)
    ))

;;; generic_compare reg, condition, millicode

(define-sassy-instr (ia86.generic_compare regno condition z)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(or	,$r.temp ,$r.result)
    `(test	,$r.temp.low ,fixtag_mask)
    (ia86.loadr	$r.second regno)
    `(jz short ,L1)
    (ia86.mcall	z condition)
    `(jmp short ,L2	)
    `(label ,L1)
    `(cmp	,$r.result ,$r.second)
    (ia86.setcc	$r.result condition)
    `(label ,L2 )))

;;; generic_char_compare reg, cc, ex

(define-sassy-instr (ia86.generic_char_compare regno y z)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.second regno)
           `(cmp	,$r.second.low ,$imm.character)
           `(jz	short ,L2)
           `(label ,L1)
           (ia86.exception_continuable z L0)
           `(label ,L2)
           `(cmp	,$r.result.low ,$imm.character)
           `(jne short ,L1)
           `(cmp	,$r.result ,$r.second)))
        ((is_hwreg regno)
         `(cmp	,$r.result ,(REG regno)))
        (else
         `(cmp	,$r.result (& ,$r.globals ,(G_REG regno)))))
  (ia86.setcc	$r.result y))

(define-sassy-instr (ia86.generic_imm_compare_branchf hwregno imm jnc mcode L)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	,(try-low (REG hwregno)) ,fixtag_mask)
    `(jz short ,L1)
    (cond ((not (equal? $r.result hwregno))
           `(mov ,$r.result ,(REG hwregno))))
    (ia86.const2regf $r.second imm)
    (ia86.mcall	mcode jnc)
    `(cmp	,$r.result.low ,$imm.false)
    `(je	,(t_label L))
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp	,(REG hwregno) ,imm)
    `(,jnc	,(t_label L))
    `(label ,L2)))

;;; generic_imm_compare imm, cc, millicode

(define-sassy-instr (ia86.generic_imm_compare imm cc mcode)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	,$r.result.low ,fixtag_mask)
    `(jz short ,L1)
    (ia86.const2regf $r.second imm)
    (ia86.mcall	mcode cc)
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp	,$r.result ,imm)
    (ia86.setcc	$r.result cc)
    `(label ,L2)))
	
;;; generic_char_imm_compare imm, cc, ex
	
(define-sassy-instr (ia86.generic_char_imm_compare x y z)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           `(cmp	,$r.result.low ,$imm.character)
           `(jz	short ,L1)
           (ia86.const2regf $r.second x)
           (ia86.exception_continuable z L0)))
    `(label ,L1)
    `(cmp	,$r.result ,x)
    (ia86.setcc	$r.result y)))

;;; indexed_structure_length ptrtag, hdrtag, ex, byte?

(define-sassy-instr (ia86.indexed_structure_length/hdr ptrtag hdrtag ex byte?)		; string-length or bytevector-length
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.double_tag_test $r.result ptrtag hdrtag)
           `(jz short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable ex L0)
           `(label ,L1)
           `(mov	,$r.result ,$r.temp)))
        (else
         `(mov	,$r.result (& ,$r.result ,(- ptrtag)))))
  `(shr	,$r.result 8)
  (cond (byte?
         `(shl	,$r.result 2))))


;;; indexed_structure_length ptrtag, ex, byte?
	
(define-sassy-instr (ia86.indexed_structure_length ptrtag ex byte?)
  (ia86.single_tag_test_ex $r.result ptrtag ex)
  `(mov	,$r.result (& ,$r.result ,(- ptrtag)))
  `(shr	,$r.result 8)
  (cond (byte?
         `(shl	,$r.result 2))))


;;; indexed_structure_test reg_index, reg_value, ptrtag, hdrtag, ex, byte?, test_reg_value
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that reg_index is a fixnum in the range of the structure.
;;;	If hdrtag is zero then do not check it.

(define-sassy-instr (ia86.indexed_structure_test regno reg-value ptrtag hdrtag ex byte? test_reg_value)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label))
               (L3 (fresh-label)))
           `(label ,L0)
           (ia86.fixnum_test_temp_is_free regno)
           `(jnz short ,L1)
           (cond ((not (= hdrtag 0))
                  (ia86.double_tag_test $r.result ptrtag hdrtag)
                  `(jz short ,L2))
                 (else
                  (ia86.single_tag_test $r.result ptrtag)
                  `(jz short ,L3)))
           `(label ,L1)
	   (ia86.loadr  $r.temp  reg-value)
	   `(mov (& ,$r.globals ,$g.third) ,$r.temp)
           (ia86.loadr	$r.second regno)
           (ia86.exception_continuable ex L0)
           (cond ((= hdrtag 0)
                  `(label ,L3)
                  `(mov	,$r.temp (& ,$r.result ,(- ptrtag)))))
           `(label ,L2)
           `(shr	,$r.temp 8)
           (cond (byte?
                  `(shl	,$r.temp 2)))	; Length is now a fixnum
           (cond ((is_hwreg regno)
                  `(cmp	,$r.temp ,(REG regno)))
                 (else
                  `(cmp	,$r.temp (& ,$r.globals ,(G_REG regno)))))
           `(jbe short ,L1)
           (test_reg_value	reg-value L1)))))

;;; indexed_structure_test_imm index, ptrtag, hdrtag, ex, byte?
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that index (a fixnum) is in the range of the structure.
;;;	If hdrtag is zero then do not check it.
	
(define-sassy-instr (ia86.indexed_structure_test_imm x y hdrtag ex byte?)
  (cond 
   ((not (unsafe-code))
    (let ((L0 (fresh-label))
          (L1 (fresh-label))
          (L2 (fresh-label))
          (L3 (fresh-label)))
      `(label ,L0)
      (cond ((not (= hdrtag 0))
             (ia86.double_tag_test $r.result y hdrtag)
             `(jz short ,L2))
            (else
             (ia86.single_tag_test $r.result y)
             `(jz short ,L3)))
      `(label ,L1)
      `(mov	,$r.second ,x)
      (ia86.exception_continuable ex L0)
      (cond ((= hdrtag 0)
             `(label ,L3)
             `(mov	,$r.temp (& ,$r.result ,(- y)))))
      `(label ,L2)
      `(shr	,$r.temp 8)
      (cond (byte?
             `(shl	,$r.temp 2)))		; Length is now a fixnum
      `(cmp	,$r.temp ,x)
      `(jbe short ,L1)))))

;;; load_from_indexed_structure index_reg, ptrtag, byte?
	
(define-sassy-instr (ia86.load_from_indexed_structure regno y byte?)
  (cond (byte?
         (ia86.loadr	$r.temp regno)
         `(shr	,$r.temp 2)
         `(mov	,$r.result.low (& ,$r.result ,$r.temp ,(+ (- y) wordsize)))
         `(and	,$r.result #xFF))
        ((is_hwreg regno)
         `(mov	,$r.result (& ,$r.result ,(REG regno) ,(+ (- y) wordsize))))
        (else
         (ia86.loadr	$r.temp regno)
         `(mov	,$r.result (& ,$r.result ,$r.temp ,(+ (- y) wordsize))))))

(define-sassy-instr (ia86.load_from_indexed_structure_imm x y byte?)
  (cond (byte?
         `(mov	,$r.result.low (& ,$r.result ,(+ (- y) wordsize (quotient x 4))))
         `(and	,$r.result #xFF))
        (else
         `(mov	,$r.result (& ,$r.result ,(+ (- y) wordsize x))))))
				
;;; indexed_structure_ref reg, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in ,RESULT.

(define-sassy-instr (ia86.indexed_structure_ref/hdr reg ptrtag hdrtag ex byte?)
  (ia86.indexed_structure_test  reg 0  ptrtag  hdrtag  ex  byte? ia86.check_nothing)
  (ia86.load_from_indexed_structure  reg  ptrtag  byte?))

;;; indexed_structure_ref_imm idx, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in $r.result.

(define-sassy-instr (ia86.indexed_structure_ref_imm/hdr idx ptrtag hdrtag ex byte?)
  (ia86.indexed_structure_test_imm  idx  ptrtag  hdrtag  ex  byte?)
  (ia86.load_from_indexed_structure_imm  idx  ptrtag  byte?))
				
;;; indexed_structure_ref reg, ptrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-instr (ia86.indexed_structure_ref reg ptrtag ex byte?)
  (ia86.indexed_structure_test  reg 0  ptrtag 0  ex  byte? ia86.check_nothing)
  (ia86.load_from_indexed_structure  reg  ptrtag  byte?))

(define-sassy-instr (ia86.indexed_structure_ref_imm reg ptrtag ex byte?)
  (ia86.indexed_structure_test_imm  reg  ptrtag 0  ex  byte?)
  (ia86.load_from_indexed_structure_imm  reg  ptrtag  byte?))

;;; check_nothing regno, label
;;;	Just a placeholder.

(define-sassy-instr (ia86.check_nothing regno y)
  )
	
;;; check_fixnum regno, label
;;;	Branch to label if regno does not hold a fixnum

(define-sassy-instr (ia86.check_fixnum regno y)
  (ia86.fixnum_test_temp_is_free regno)
  `(jnz short ,y))
	
;;; check_char regno, label
;;;	Branch to label if regno does not hold a char.
;;;	Leaves char in TEMP (even if it is in hwreg, the value must
;;;	be shifted anyway).

(define-sassy-instr (ia86.check_char regno y)
  (ia86.loadr	$r.temp regno)
  `(cmp	,$r.temp.low ,$imm.character)
  `(jnz short ,y))

;;; indexed_structure_set_* reg_idx, reg_value, ptrtag, hdrtag, ex
;;;	If hdrtag is 0 then do not check it.

(define-sassy-instr (ia86.indexed_structure_set_char regno reg-value ptrtag hdrtag ex)
  (cond 
   ((unsafe-code) 
    ;; ia86.check_char has this effect, but is not called by
    ;; ia86.indexed_structure_test in unsafe mode
    (ia86.loadr $r.temp reg-value))
   (else
    (ia86.indexed_structure_test regno reg-value ptrtag hdrtag ex #t ia86.check_char)))
  `(mov	(& ,$r.globals ,$g.stkp) ,$r.cont)
  (ia86.loadr	$r.cont regno)
  `(shr	,$r.temp ,char_shift)
  `(shr	,$r.cont 2)
  `(mov	(& ,$r.result ,$r.cont ,(+ (- ptrtag) wordsize)) ,$r.temp.low)
  `(mov	,$r.cont (& ,$r.globals ,$g.stkp)))

(define-sassy-instr (ia86.indexed_structure_set_byte regno1 regno2 z hdrtag ex)
  (ia86.indexed_structure_test regno1 regno2 z hdrtag ex #t ia86.check_fixnum)
  `(mov	(& ,$r.globals ,$g.stkp) ,$r.cont)
  (ia86.loadr	$r.cont regno1)
  `(shr	,$r.cont 2)
  (ia86.loadr	$r.temp regno2)
  `(shr	,$r.temp 2)
  `(mov	(& ,$r.result ,$r.cont ,(+ (- z) wordsize)) ,$r.temp.low)
  `(mov	,$r.cont (& ,$r.globals ,$g.stkp)))

(define-sassy-instr (ia86.indexed_structure_set_word regno1 regno2 z hdrtag ex)
  (ia86.indexed_structure_test regno1 regno2 z hdrtag ex #f ia86.check_nothing)
  (ia86.do_indexed_structure_set_word $r.result regno1 regno2 z))

;; hwregno is either 'RESULT or a hardware regno.
;; FIXME: we should consistently use symbols or numbers for regs
(define-sassy-instr (ia86.do_indexed_structure_set_word hwregno regno1 regno2 z)
  (cond ((and (is_hwreg regno2) (is_hwreg regno1))
         `(mov	(& ,(REG hwregno) ,(REG regno1) ,(+ (- z) wordsize)) ,(REG regno2))
         (ia86.write_barrier (reg/result->num hwregno) regno2))
        ((is_hwreg regno2)
         (ia86.loadr	$r.temp regno1)
         `(mov	(& ,(REG hwregno) ,$r.temp ,(+ (- z) wordsize)) ,(REG regno2))
         (ia86.write_barrier (reg/result->num hwregno) regno2))
        ((is_hwreg regno1)
         (ia86.loadr	$r.second regno2)
         `(mov	(& ,(REG hwregno) ,(REG regno1) ,(+ (- z) wordsize)) ,$r.second)
         (ia86.write_barrier (reg/result->num hwregno) -1))
        (else
         `(mov	(& ,$r.globals ,$g.stkp) ,$r.cont)
         (ia86.loadr	$r.cont regno1)
         (ia86.loadr	$r.second regno2)
         `(mov	(& ,(REG hwregno) ,$r.cont ,(+ (- z) wordsize)) ,$r.second)
         `(mov	,$r.cont (& ,$r.globals ,$g.stkp))
         (ia86.write_barrier (reg/result->num hwregno) -1))))

;;; make_indexed_structure_word regno ptrtag hdrtag ex
;;;	Allocate a word structure with the length specified in RESULT
;;;	(fixnum number of entries).  If ,x is not -1, then initialize 
;;;	it with the contents of (REG ,x), otherwise with #!unspecified.
	
(define-sassy-instr (ia86.make_indexed_structure_word regno y z ex)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	,$r.result ,(logior fixtag_mask #x80000000))
           `(jz short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable ex L0)
           `(label ,L1))))
  `(mov	(& ,$r.globals ,$g.alloctmp) ,$r.result)
  `(add	,$r.result ,wordsize)
  (cond ((= regno -1)
         `(mov	,$r.second ,$imm.unspecified))
        (else
         (ia86.loadr	$r.second regno)))
  (ia86.mcall	$m.alloci 'alloci)
  `(mov	,$r.temp (& ,$r.globals ,$g.alloctmp))
  `(shl	,$r.temp 8)
  `(or	,$r.temp ,z)
  `(mov	(& ,$r.result) ,$r.temp)
  `(add	,$r.result ,y))

;;; make_indexed_structure_byte regno hdrtag ex
;;;	Allocate a byte structure with the length specified in RESULT
;;;     (fixnum number of bytes).  If ,x is not -1, then (REG ,x) must
;;;     hold a char value to be used for initialization (a check is
;;;     performed that is a char).  If ,x is -1, no initialization 
;;; 	is performed.

(define-sassy-instr (ia86.make_indexed_structure_byte regno hdrtag ex)
  (let ((L0 (fresh-label))
        (L1 (fresh-label))
        (L2 (fresh-label)))
    (cond ((not (unsafe-code))
           ;; OPTIMIZEME (size): Unless allocation is inline,
           ;; the fixnum test can be moved into the millicode.
           ;; (As can the char test, I guess -- in fact, this whole
           ;; instruction is probably best moved into millicode)
           ;; OPTIMIZEME (speed): Both branches are mispredicted here.
           `(label ,L0)
           (cond ((not (= regno -1))
                  (ia86.loadr	$r.second regno))
                 (else 
                  `(xor ,$r.second ,$r.second)))
           `(test	,$r.result ,(logior fixtag_mask #x80000000))
           `(jz short ,L2)
           `(label ,L1)
           (ia86.exception_continuable ex L0)
           `(label ,L2 )
           (cond ((not (= regno -1))
                  `(cmp	,$r.second.low ,$imm.character)
                  `(jne	short ,L1)))))
    `(mov	(& ,$r.globals ,$g.alloctmp) ,$r.result)
    `(add	,$r.result ,(fixnum wordsize))
    (ia86.mcall	$m.alloc-bv 'alloc-bv)
    (cond ((not (= regno -1))
           (ia86.loadr	$r.temp regno)
           `(mov	(& ,$r.globals ,G_REGALIAS_ECX) ecx)
           `(mov	(& ,$r.globals ,G_REGALIAS_EDI) edi)
           `(shr	,$r.temp ,char_shift)	; char arg to byte value
           `(mov	ecx (& ,$r.globals ,$g.alloctmp))
           `(shr	ecx 2)		; byte count
           `(lea	edi (& ,$r.result 4))	; destination ptr
           `(cld)
           `(rep (stosb))		; byte fill
           `(mov	ecx (& ,$r.globals ,G_REGALIAS_ECX))
           `(mov	edi (& ,$r.globals ,G_REGALIAS_EDI))))
    `(mov	,$r.temp (& ,$r.globals ,$g.alloctmp))
    `(shl	,$r.temp 6)
    `(or	,$r.temp ,hdrtag)
    `(mov	(& ,$r.result)  ,$r.temp)
    `(add	,$r.result ,$tag.bytevector-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Primitive operations
;;; 
;;; The names 'n' in T_OP1_n are the primcodes from the table in
;;; Compiler/standard-C.imp.sch (yes, that's right).

(define-sassy-instr (ia86.T_OP1 x)   ;; YUCK eval!
  (let ((f (case x
             ((1 break) ia86.T_OP1_1) 
             ((3 unspecified) ia86.T_OP1_3) 
             ((4 undefined) ia86.T_OP1_4) 
             ((5 eof-object) ia86.T_OP1_5)
             ((6 enable-interrupts) ia86.T_OP1_6) 
             ((7 disable-interrupts) ia86.T_OP1_7) 
             ((8 typetag) ia86.T_OP1_8) 
             ((9 not) ia86.T_OP1_9)
             ((10 null?) ia86.T_OP1_10) 
             ((11 pair?) ia86.T_OP1_11) 
             ((12 eof-object?) ia86.T_OP1_12) 
             ((13 port?) ia86.T_OP1_13) 
             ((14 structure?) ia86.T_OP1_14)
             ((15 car) ia86.T_OP1_15) 
             ((16 cdr) ia86.T_OP1_16) 
             ((17 symbol?) ia86.T_OP1_17) 
             ((18 complex?) ia86.T_OP1_18) 
             ((20 rational?) ia86.T_OP1_20) 
             ((21 compnum?) ia86.T_OP1_21) 
             ((22 integer?) ia86.T_OP1_22) 
             ((23 fixnum?) ia86.T_OP1_23) 
             ((24 flonum?) ia86.T_OP1_24)
             ((25 exact?) ia86.T_OP1_25) 
             ((26 inexact?) ia86.T_OP1_26) 
             ((27 exact->inexact) ia86.T_OP1_27) 
             ((28 inexact->exact) ia86.T_OP1_28) 
             ((29 round) ia86.T_OP1_29)
             ((30 truncate) ia86.T_OP1_30) 
             ((31 zero?) ia86.T_OP1_31) 
             ((32 --) ia86.T_OP1_32) 
             ((33 fxlognot) ia86.T_OP1_33) 
             ((34 real-part) ia86.T_OP1_34)
             ((35 imag-part) ia86.T_OP1_35) 
             ((36 char?) ia86.T_OP1_36) 
             ((37 char->integer) ia86.T_OP1_37) 
             ((char->integer:chr) ia86.T_OP1_char->integer:chr)
             ((38 integer->char) ia86.T_OP1_38) 
             ((integer->char:fix) ia86.T_OP1_38) 
             ((integer->char:trusted) ia86.T_OP1_integer->char:trusted)
             ((39 string?) ia86.T_OP1_39)
             ((40 string-length string-length:str) ia86.T_OP1_40) 
             ((41 vector?) ia86.T_OP1_41) 
             ((42 vector-length) ia86.T_OP1_42) 
             ((43 bytevector?) ia86.T_OP1_43) 
             ((44 bytevector-length) ia86.T_OP1_44)
             ((46 make-bytevector) ia86.T_OP1_46) 
             ((47 procedure?) ia86.T_OP1_47) 
             ((48 procedure-length) ia86.T_OP1_48) 
             ((49 make-procedure) ia86.T_OP1_49)
             ((52 make-cell) ia86.T_OP1_52) 
             ((54 cell-ref) ia86.T_OP1_54)
             ((94 bytevector-like?) ia86.T_OP1_94)
             ((95 vector-like?) ia86.T_OP1_95)
             ((101 vector-like-length) ia86.T_OP1_101) 
             ((102 bytevector-like-length) ia86.T_OP1_102) 
             ;((104 petit-patch-boot-code) ia86.T_OP1_104)
             ((105 syscall) ia86.T_OP1_105)
             ((106 creg) ia86.T_OP1_106) 
             ((107 creg-set!) ia86.T_OP1_107) 
             ((108 gc-counter) ia86.T_OP1_108)
             ((200 most-positive-fixnum) ia86.T_OP1_200) 
             ((201 most-negative-fixnum) ia86.T_OP1_201) 
             ((204 fx--) ia86.T_OP1_204)
             ((211 fxzero?) ia86.T_OP1_211) 
             ((212 fxpositive?) ia86.T_OP1_212) 
             ((213 fxnegative?) ia86.T_OP1_213)
             ((401 vector-length:vec) ia86.T_OP1_401) 
             ((404 car:pair) ia86.T_OP1_404)
             ((405 cdr:pair) ia86.T_OP1_405)
             ((612 internal:branchf-zero?) ia86.T_OP1_612)
             ((1000 timestamp!) ia86.T_OP1_1000)
             ((1001 p-monitor!) ia86.T_OP1_1001)
             (else (error 'ia86.T_OP1 x))
             )))
    (f)))

;; T_OP1* handles $reg/op1/setreg
(define-sassy-instr (ia86.T_OP1* op rs rd)
  (let ((f (case op
             ((3 unspecified) ia86.T_OP1_3*)
             ((4 undefined) ia86.T_OP1_4*)
             ((5 eof-object) ia86.T_OP1_5*)
             ((9 not) ia86.T_OP1_9*)
             ((10 null?) ia86.T_OP1_10*)
             ((11 pair?) ia86.T_OP1_11*)
             ((12 eof-object?) ia86.T_OP1_12*)
             ((15 car) ia86.T_OP1_15*)
             ((16 cdr) ia86.T_OP1_16*)
             ((23 fixnum?)    ia86.T_OP1_23*)
             ((47 procedure?) ia86.T_OP1_47*)
             ((52 make-cell)  ia86.T_OP1_52*)
             ((54 cell-ref)   ia86.T_OP1_54*)
             ((94 bytevector-like?) ia86.T_OP1_94*)
             ((95 vector-like?) ia86.T_OP1_95*)
             ((401 vector-length:vec) ia86.T_OP1_401*)
             ((404 car:pair) ia86.T_OP1_404*)
             ((405 cdr:pair) ia86.T_OP1_405*)
             (else (error 'ia86.T_OP1* op))
             )))
    (f rs rd)))

(define-sassy-instr (ia86.T_OP2 x y) ;; YUCK eval!
  (let ((f (case x
             ((45 bytevector-fill!) ia86.T_OP2_45)
             ((55 typetag-set!) ia86.T_OP2_55) 
             ((56 eq?) ia86.T_OP2_56) 
             ((57 eqv?) ia86.T_OP2_57) 
             ((58 cons) ia86.T_OP2_58) 
             ((59 set-car!) ia86.T_OP2_59)
             ((60 set-cdr!) ia86.T_OP2_60) 
             ((61 +) ia86.T_OP2_61) 
             ((62 -) ia86.T_OP2_62) 
             ((63 *) ia86.T_OP2_63) 
             ((64 /) ia86.T_OP2_64)
             ((65 quotient) ia86.T_OP2_65) 
             ((66 <) ia86.T_OP2_66) 
             ((67 <=) ia86.T_OP2_67) 
             ((68 =) ia86.T_OP2_68) 
             ((69 >) ia86.T_OP2_69)
             ((70 >=) ia86.T_OP2_70) 
             ((71 fxlogand) ia86.T_OP2_71) 
             ((72 fxlogior) ia86.T_OP2_72) 
             ((73 fxlogxor) ia86.T_OP2_73) 
             ((74 fxlsh) ia86.T_OP2_74)
             ((75 fxrsha) ia86.T_OP2_75) 
             ((76 fxrshl) ia86.T_OP2_76) 
             ((77 rot) ia86.T_OP2_77) 
             ((78 string-ref string-ref:trusted) ia86.T_OP2_78) 
             ((79 string-set!) ia86.T_OP2_79)
             ((80 make-vector) ia86.T_OP2_80) 
             ((81 vector-ref) ia86.T_OP2_81) 
             ((82 bytevector-ref) ia86.T_OP2_82) 
             ((83 procedure-ref) ia86.T_OP2_83) 
             ((84 cell-set! cell-set!:nwb) ia86.T_OP2_84)
             ((85 char<?) ia86.T_OP2_85) 
             ((86 char<=?) ia86.T_OP2_86) 
             ((87 char=?) ia86.T_OP2_87) 
             ((88 char>?) ia86.T_OP2_88) 
             ((89 char>=?) ia86.T_OP2_89)
             ((90 sys$partial-list->vector) ia86.T_OP2_90)
             ((96 bytevector-like-ref) ia86.T_OP2_96) 
             ((98 sys$bvlcmp) ia86.T_OP2_98) 
             ((99 vector-like-ref) ia86.T_OP2_99)
             ((103 remainder) ia86.T_OP2_103) 
             ((109 make-string) ia86.T_OP2_109)
             ((202 fx+) ia86.T_OP2_202) 
             ((203 fx-) ia86.T_OP2_203)
             ((205 fx*) ia86.T_OP2_205) 
             ((206 fx=) ia86.T_OP2_206) 
             ((207 fx<) ia86.T_OP2_207) 
             ((208 fx<=) ia86.T_OP2_208) 
             ((209 fx>) ia86.T_OP2_209)
             ((210 fx>=) ia86.T_OP2_210)
             ((402 vector-ref:trusted) ia86.T_OP2_402) 
             ((406 =:fix:fix) ia86.T_OP2_406) 
             ((407 <:fix:fix) ia86.T_OP2_407) 
             ((408 <=:fix:fix) ia86.T_OP2_408) 
             ((409 >=:fix:fix) ia86.T_OP2_409)
             ((410 >:fix:fix) ia86.T_OP2_410)
             ((500 +:idx:idx) ia86.T_OP2_500) 
             ((501 +:fix:fix) ia86.T_OP2_501) 
             ((502 -:idx:idx) ia86.T_OP2_502) 
             ((503 -:fix:fix) ia86.T_OP2_503)
             ((700 bytevector-ref:trusted) ia86.T_OP2_700) 
             (else (error 'ia86.T_OP2 x))
             )))
    (f y)))

;; T_OP2* handles $reg/op2/setreg

(define-sassy-instr (ia86.T_OP2* op rs1 rs2 rd)
  (let ((f (case op
             ((56 eq?) ia86.T_OP2_56*)
             ((58 cons) ia86.T_OP2_58*)
             ((59 set-car!) ia86.T_OP2_59*)
             ((60 set-cdr!) ia86.T_OP2_60*)
             ((61 +) ia86.T_OP2_61*)
             ((62 -) ia86.T_OP2_62*)
             ((402 vector-ref:trusted) ia86.T_OP2_402*)
             ((406 =:fix:fix) ia86.T_OP2_406*)
             ((407 <:fix:fix) ia86.T_OP2_407*)
             ((408 <=:fix:fix) ia86.T_OP2_408*)
             ((409 >=:fix:fix) ia86.T_OP2_409*)
             ((410 >:fix:fix) ia86.T_OP2_410*)
             (else (error 'ia86.T_OP2* op)))))
    ;; odd order of args is because of the odd define-sassy-instr/peep
    ;; interface (rs1 and rd are often 'RESULT)
    (f rs1 rd rs2)))

(define-sassy-instr (ia86.T_OP2IMM x y) ;; YUCK eval!
  (let ((f (case x
             ((128 typetag-set!) ia86.T_OP2IMM_128) 
             ((129 eq?) ia86.T_OP2IMM_129)
             ((130 +) ia86.T_OP2IMM_130) 
             ((131 -) ia86.T_OP2IMM_131) 
             ((132 <) ia86.T_OP2IMM_132) 
             ((133 <=) ia86.T_OP2IMM_133) 
             ((134 =) ia86.T_OP2IMM_134)
             ((135 >) ia86.T_OP2IMM_135) 
             ((136 >=) ia86.T_OP2IMM_136) 
             ((137 char<?) ia86.T_OP2IMM_137) 
             ((138 char<=?) ia86.T_OP2IMM_138) 
             ((139 char=?) ia86.T_OP2IMM_139)
             ((140 char>?) ia86.T_OP2IMM_140) 
             ((141 char>=?) ia86.T_OP2IMM_141) 
             ((142 string-ref) ia86.T_OP2IMM_142) 
             ((143 vector-ref) ia86.T_OP2IMM_143) 
             ((144 bytevector-ref) ia86.T_OP2IMM_144)
             ((145 bytevector-like-ref) ia86.T_OP2IMM_145)
             ((146 vector-like-ref) ia86.T_OP2IMM_146)
             ((250 fx+) ia86.T_OP2IMM_250) 
             ((251 fx-) ia86.T_OP2IMM_251) 
             ((253 fx=) ia86.T_OP2IMM_253) 
             ((254 fx<) ia86.T_OP2IMM_254)
             ((255 fx<=) ia86.T_OP2IMM_255) 
             ((256 fx>) ia86.T_OP2IMM_256) 
             ((257 fx>=) ia86.T_OP2IMM_257)
             ((450 vector-ref:trusted) ia86.T_OP2IMM_450) 
             ((451 =:fix:fix) ia86.T_OP2IMM_451) 
             ((452 <:fix:fix) ia86.T_OP2IMM_452) 
             ((453 <=:fix:fix) ia86.T_OP2IMM_453) 
             ((454 >:fix:fix) ia86.T_OP2IMM_454)
             ((455 >=:fix:fix) ia86.T_OP2IMM_455)
             ((520 +:idx:idx) ia86.T_OP2IMM_520) 
             ((521 +:fix:fix) ia86.T_OP2IMM_521) 
             ((522 -:idx:idx) ia86.T_OP2IMM_522) 
             ((523 -:fix:fix) ia86.T_OP2IMM_523)
             ((700 bytevector-ref:trusted) ia86.T_OP2IMM_700) 
             (else (error 'ia86.T_OP2IMM x))
             )))
    (f y)))

(define-sassy-instr (ia86.T_OP2IMM* op rs1 imm rd)
  (let ((f (case op
             ((129 eq?) ia86.T_OP2IMM_129*)
             ((130 +)   ia86.T_OP2IMM_130*)
             ((131 -)   ia86.T_OP2IMM_131*)
             ((450 vector-ref:trusted) ia86.T_OP2IMM_450*)
             ((451 =:fix:fix) ia86.T_OP2IMM_451*)
             ((452 <:fix:fix) ia86.T_OP2IMM_452*)
             ((453 <=:fix:fix) ia86.T_OP2IMM_453*)
             ((454 >:fix:fix) ia86.T_OP2IMM_454*)
             ((455 >=:fix:fix) ia86.T_OP2IMM_455*)
             ((520 +:idx:idx) ia86.T_OP2IMM_520*)
             ((521 +:fix:fix) ia86.T_OP2IMM_521*)
             ((522 -:idx:idx) ia86.T_OP2IMM_522*)
             ((523 -:fix:fix) ia86.T_OP2IMM_523*)
             (else (error 'ia86.T_OP2IMM* op)))))
    (f rs1 rd imm)))

(define-sassy-instr (ia86.T_OP3 x y z) ;; YUCK eval!
  (let ((f (case x
             ((79 string-set! string-set!:trusted) ia86.T_OP3_79)
             ((91 vector-set!) ia86.T_OP3_91) 
             ((92 bytevector-set!) ia86.T_OP3_92) 
             ((93 procedure-set!) ia86.T_OP3_93) 
             ((97 bytevector-like-set!) ia86.T_OP3_97)
             ((100 vector-like-set!) ia86.T_OP3_100)
             ;; TODO: add support for :nwb
             ((403 vector-set!:trusted vector-set!:trusted:nwb) ia86.T_OP3_403)
             (else (error 'ia86.T_OP3 x))
             )))
    (f y z)))

(define-sassy-instr (ia86.T_REG_OP1_BRANCHF op rs L a-skip?)
  (let ((f (case op
             ((internal:branchf-null?) ia86.T_REG_OP1_BRANCHF_NULL?)
             ((internal:branchf-eof-object?) ia86.T_REG_OP1_BRANCHF_EOF_OBJECT?)
             ((internal:branchf-fixnum?) ia86.T_REG_OP1_BRANCHF_FIXNUM?)
             ((internal:branchf-pair?) ia86.T_REG_OP1_BRANCHF_PAIR?)
             ((internal:branchf-zero?) ia86.T_REG_OP1_BRANCHF_ZERO?)
             (else (error 'ia86.T_REG_OP1_BRANCHF op)))))
    (f rs L a-skip?)))

(define-sassy-instr (ia86.T_REG_OP1_CHECK op rs L)
  (let ((f (case op
             ((internal:check-fixnum?) ia86.T_REG_OP1_CHECK_FIXNUM?)
             ((internal:check-pair?)   ia86.T_REG_OP1_CHECK_PAIR?)
             ((internal:check-vector?) ia86.T_REG_OP1_CHECK_VECTOR?)
             ((internal:check-string?) ia86.T_REG_OP1_CHECK_STRING?)
             (else (error 'ia86.T_REG_OP1_CHECK op)))))
    (f rs L)))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF op rs1 rs2 L a-skip?)
  (let ((f (case op
             ((internal:branchf-eq?) ia86.T_REG_OP2_BRANCHF_EQ?)
             ((internal:branchf-<)   ia86.T_REG_OP2_BRANCHF_66)
             ((internal:branchf-<=)  ia86.T_REG_OP2_BRANCHF_67)
             ((internal:branchf-=)   ia86.T_REG_OP2_BRANCHF_68)
             ((internal:branchf->)   ia86.T_REG_OP2_BRANCHF_69)
             ((internal:branchf->=)  ia86.T_REG_OP2_BRANCHF_70)
             ((internal:branchf-=:fix:fix)   ia86.T_REG_OP2_BRANCHF_406)
             ((internal:branchf-<:fix:fix)   ia86.T_REG_OP2_BRANCHF_407)
             ((internal:branchf-<=:fix:fix)  ia86.T_REG_OP2_BRANCHF_408)
             ((internal:branchf->=:fix:fix)  ia86.T_REG_OP2_BRANCHF_409)
             ((internal:branchf->:fix:fix)   ia86.T_REG_OP2_BRANCHF_410)
             (else (error 'ia86.T_REG_OP2_BRANCHF op)))))
    (f rs1 rs2 L a-skip?)))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF op rs1 imm L a-skip?)
  (let ((f (case op
             ((internal:branchf-eq?/imm) ia86.T_REG_OP2IMM_BRANCHF_EQ?)
             ((internal:branchf-</imm)   ia86.T_REG_OP2IMM_BRANCHF_132)
             ((internal:branchf-<=/imm)  ia86.T_REG_OP2IMM_BRANCHF_133)
             ((internal:branchf-=/imm)   ia86.T_REG_OP2IMM_BRANCHF_134)
             ((internal:branchf->/imm)   ia86.T_REG_OP2IMM_BRANCHF_135)
             ((internal:branchf->=/imm)  ia86.T_REG_OP2IMM_BRANCHF_136)
             ((internal:branchf-=:fix:fix/imm)   ia86.T_REG_OP2IMM_BRANCHF_451)
             ((internal:branchf-<:fix:fix/imm)   ia86.T_REG_OP2IMM_BRANCHF_452)
             ((internal:branchf-<=:fix:fix/imm)  ia86.T_REG_OP2IMM_BRANCHF_453)
             ((internal:branchf->:fix:fix/imm)   ia86.T_REG_OP2IMM_BRANCHF_454)
             ((internal:branchf->=:fix:fix/imm)  ia86.T_REG_OP2IMM_BRANCHF_455)
             (else (error 'ia86.T_REG_OP2IMM_BRANCHF op)))))
    (f rs1 imm L a-skip?)))

;; N.B. only rs1 is guaranteed to be a hwreg...
(define-sassy-instr (ia86.T_REG_OP2_CHECK op rs1 rs2 L)
  (let ((f (case op
             ((internal:check-=:fix:fix)  ia86.T_REG_OP2_CHECK_406)
             ((internal:check-<:fix:fix)  ia86.T_REG_OP2_CHECK_407)
             ((internal:check-<=:fix:fix) ia86.T_REG_OP2_CHECK_408)
             ((internal:check->=:fix:fix) ia86.T_REG_OP2_CHECK_409)
             ((internal:check->:fix:fix)  ia86.T_REG_OP2_CHECK_410)
             (else (error 'ia86.T_REG_OP2_CHECK op)))))
    (f rs1 rs2 L)))

(define-sassy-instr (ia86.T_REG_OP2IMM_CHECK op rs1 imm L)
  (let ((f (case op
             ((internal:check-=:fix:fix)  ia86.T_REG_OP2IMM_CHECK_451)
             ((internal:check-<:fix:fix)  ia86.T_REG_OP2IMM_CHECK_452)
             ((internal:check-<=:fix:fix) ia86.T_REG_OP2IMM_CHECK_453)
             ((internal:check->=:fix:fix) ia86.T_REG_OP2IMM_CHECK_455)
             ((internal:check->:fix:fix)  ia86.T_REG_OP2IMM_CHECK_454)
             (else (error 'ia86.T_REG_OP2_CHECK op)))))
    (f rs1 imm L)))

;; N.B. only rs1 is guaranteed to be a hwreg...
(define-sassy-instr (ia86.T_REG_OP3 op rs1 rs2 rs3)
  (let ((f (case op
             ((internal:vector-set!:trusted) ia86.T_REG_OP3_403)
             (else (error 'ia86.T_REG_OP3 op))
             )))
    (f rs1 rs2 rs3)))

(define-syntax define-sassy-instr/peep 
  (syntax-rules (or)
    ((_ (or (IA86.T_OPk_n* RS RD args-fst ...)
            (IA86.T_OPk_n  args ...))
        BODY ...)
     (begin
       (if (not (equal? '(args-fst ...) '(args ...)))
           ;; (safe guard against my own wee brain)
           (error 'define-sassy-instr/peep " ill-formed-args"))
       (define-sassy-instr (IA86.T_OPk_n args ...)
         (IA86.T_OPk_n* $r.result $r.result args ...))
       (define-sassy-instr (IA86.T_OPk_n* RS RD args ...)
         BODY ...)))))

(define-sassy-instr (ia86.T_OP1_1)		; break
  (ia86.mcall	$m.break 'break))

(define-sassy-instr/peep (or (ia86.T_OP1_3* rs rd)	; unspecified
                             (ia86.T_OP1_3))
  (ia86.const2regf (REG rd) $imm.unspecified))

(define-sassy-instr/peep (or (ia86.T_OP1_4* rs rd)	; undefined
                             (ia86.T_OP1_4))
  (ia86.const2regf (REG rd) $imm.undefined))

(define-sassy-instr/peep (or (ia86.T_OP1_5* rs rd)	; eof-object
                             (ia86.T_OP1_5))
  (ia86.const2regf (REG rd) $imm.eof))

(define-sassy-instr (ia86.T_OP1_6)		; enable-interrupts
  (ia86.mcall	$m.enable-interrupts 'enable-interrupts))

(define-sassy-instr (ia86.T_OP1_7)		; disable-interrupts
  (ia86.mcall	$m.disable-interrupts 'disable-interrupts))

(define-sassy-instr (ia86.T_OP1_8)		; typetag
  (ia86.mcall	$m.typetag 'typetag))

(define-sassy-instr/peep (or (ia86.T_OP1_9* rs rd)		; not
                             (ia86.T_OP1_9))
  `(cmp	,(try-low (REG rs)) ,$imm.false)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr/peep (or (ia86.T_OP1_10* rs rd)		; null?
                             (ia86.T_OP1_10))
  `(cmp	,(try-low (REG rs)) ,$imm.null)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr/peep (or (ia86.T_OP1_11* rs rd)
                             (ia86.T_OP1_11))		; pair?
  (ia86.single_tag_test (REG rs) $tag.pair-tag)
  (ia86.setcc	(REG rd) 'z))
	
(define-sassy-instr/peep (or (ia86.T_OP1_12* rs rd)	; eof-object?
                             (ia86.T_OP1_12))
  `(cmp	,(REG rs) ,$imm.eof)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr (ia86.T_OP1_13)		; port?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.port))

(define-sassy-instr (ia86.T_OP1_14)		; structure?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.struct))

(define-sassy-instr/peep (or (ia86.T_OP1_15* rs rd)		; car
                             (ia86.T_OP1_15))
  (ia86.single_tag_test_ex (REG rs) $tag.pair-tag $ex.car)
  `(mov	,(REG rd) (& ,(REG rs) ,(- $tag.pair-tag))))

(define-sassy-instr/peep (or (ia86.T_OP1_16* rs rd)		; cdr
                             (ia86.T_OP1_16))
  (ia86.single_tag_test_ex (REG rs) $tag.pair-tag $ex.cdr)
  `(mov	,(REG rd) (& ,(REG rs) ,(+ (- $tag.pair-tag) wordsize))))

(define-sassy-instr (ia86.T_OP1_17)		; symbol?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.symbol))

(define-sassy-instr (ia86.T_OP1_18)		; number? and complex?
  (ia86.mcall	$m.complexp 'complexp))

(define-sassy-instr (ia86.T_OP1_20)		; real? and rational?
  (ia86.mcall	$m.rationalp 'rationalp))

(define-sassy-instr (ia86.T_OP1_21)		; compnum?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.compnum))

(define-sassy-instr (ia86.T_OP1_22)		; integer?
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	,$r.result.low ,fixtag_mask)
    `(je short	,L1)
    (ia86.mcall	$m.integerp 'integerp)
    `(jmp short ,L2)
    `(label ,L1)
    (ia86.const2regf $r.result $imm.true)
    `(label ,L2 )))

(define-sassy-instr/peep (or (ia86.T_OP1_23* rs rd)		; fixnum?
                             (ia86.T_OP1_23))
  (ia86.fixnum_test_temp_is_free rs)
  (ia86.setcc	(REG rd) 'z))
	
(define-sassy-instr (ia86.T_OP1_24)		; flonum?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.flonum))

(define-sassy-instr (ia86.T_OP1_25)		; exact?
  (ia86.mcall	$m.exactp 'exactp))

(define-sassy-instr (ia86.T_OP1_26)		; inexact?
  (ia86.mcall	$m.inexactp 'inexactp))

(define-sassy-instr (ia86.T_OP1_27)		; exact->inexact
  (ia86.mcall	$m.exact->inexact 'exact->inexact))

(define-sassy-instr (ia86.T_OP1_28)		; inexact->exact
  (ia86.mcall	$m.inexact->exact 'inexact->exact))

(define-sassy-instr (ia86.T_OP1_29)		; round
  (ia86.mcall	$m.round 'round))

(define-sassy-instr (ia86.T_OP1_30)		; truncate
  (ia86.mcall	$m.truncate 'truncate))

(define-sassy-instr (ia86.T_OP1_31)		; zero?
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	,$r.result.low ,fixtag_mask)
    `(jz short ,L1)
    (ia86.mcall	$m.zerop 'zerop)
    `(jmp short ,L2)
    `(label ,L1)
    `(and	,$r.result ,$r.result)
    (ia86.setcc	$r.result 'z)
    `(label ,L2)))

(define-sassy-instr (ia86.T_OP1_32)		; --
  (ia86.mcall	$m.negate 'negate))

(define-sassy-instr (ia86.T_OP1_33)		; fxlognot
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           `(test	,$r.result.low ,fixtag_mask)
           `(jz short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.lognot L0)
           `(label ,L1)))
    `(lea	,$r.result (& ,$r.result ,fixtag_mask))
    `(not	,$r.result)))

(define-sassy-instr (ia86.T_OP1_34)		; real-part
  (ia86.mcall	$m.real-part 'real-part))
	
(define-sassy-instr (ia86.T_OP1_35)		; imag-part
  (ia86.mcall	$m.imag-part 'imag-part))

(define-sassy-instr (ia86.T_OP1_36)		; char?
  `(cmp	,$r.result.low ,$imm.character)
  (ia86.setcc	$r.result 'z))

(define-sassy-instr (ia86.T_OP1_37)		; char->integer
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           `(cmp	,$r.result.low ,$imm.character)
           `(jz	short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.char2int L0)
           `(label ,L1)))
    (ia86.T_OP1_char->integer:chr)))

(define-sassy-instr (ia86.T_OP1_char->integer:chr) ; char->integer:chr
  `(shr	,$r.result 6))

(define-sassy-instr (ia86.T_OP1_38)		; integer->char
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (FAULT (fresh-label)))
           `(label ,L0)
           ;; Argument must be fixnum
           `(test      ,$r.result.low ,fixtag_mask)
           `(jnz short  ,FAULT)
           ;; Argument cannot be a surrogate (#x0000d800 - #x0000dfff).
           `(mov       ,$r.temp ,$r.result)
           `(sar       ,$r.temp 13)
           `(cmp       ,$r.temp #b11011)
           `(jz short  ,FAULT)
           ;; Argument must be non-negative and less than #x00110000.
           `(cmp       ,$r.temp 544)
           `(jge short ,FAULT)
           `(cmp       ,$r.temp 0)
           `(jnl short ,L1)
           `(label ,FAULT)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.int2char L0)
	   `(label ,L1))))
  (ia86.T_OP1_integer->char:trusted))

(define-sassy-instr (ia86.T_OP1_integer->char:trusted)	; integer->char:trusted
  `(shl	,$r.result 6)
  `(or	,$r.result.low ,$imm.character))

(define-sassy-instr (ia86.T_OP1_39)		; string?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.string))

(define-sassy-instr (ia86.T_OP1_40)		; string-length
  (ia86.indexed_structure_length/hdr $tag.bytevector-tag $hdr.string $ex.slen #t))
		
(define-sassy-instr (ia86.T_OP1_41)		; vector?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.vector))


(define-sassy-instr (ia86.T_OP1_42)		; vector-length
  (ia86.indexed_structure_length/hdr $tag.vector-tag $hdr.vector $ex.vlen #f))
		
(define-sassy-instr (ia86.T_OP1_43)		; bytevector?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.bytevector))

(define-sassy-instr (ia86.T_OP1_44)		; bytevector-length
  (ia86.indexed_structure_length/hdr $tag.bytevector-tag $hdr.bytevector $ex.bvlen #t))

;; OPTIMIZEME
(define-sassy-instr (ia86.T_OP2_45 x)		; bytevector-fill!
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.single_tag_test $r.result $tag.bytevector-tag)
           `(jz short ,L2)
           `(label ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.bvfill L0)
           `(label ,L2)
           (ia86.loadr	$r.second x)
           `(test	,$r.second.low ,fixtag_mask)
           `(jnz short ,L1)))
        (else
         (ia86.loadr	$r.second x)))
  (ia86.mcall	$m.bytevector-like-fill 'bytevector-like-fill))

(define-sassy-instr (ia86.T_OP1_46)		; make-bytevector
  (ia86.make_indexed_structure_byte -1 $hdr.bytevector  $ex.mkbvl))

(define-sassy-instr/peep (or (ia86.T_OP1_47* rs rd)		; procedure?
                             (ia86.T_OP1_47))
  (ia86.single_tag_test (REG rs) $tag.procedure-tag)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr (ia86.T_OP1_48)		; procedure-length
  (ia86.indexed_structure_length $tag.procedure-tag $ex.plen  #f))

(define-sassy-instr (ia86.T_OP1_49)		; make-procedure
  ;; exception code wrong, matches Sparc
  (ia86.make_indexed_structure_word -1 $tag.procedure-tag  $hdr.procedure  $ex.mkvl))
		
(define-sassy-instr/peep (or (ia86.T_OP1_52* rs rd)	; make-cell just maps to cons, for now
                             (ia86.T_OP1_52))
  (ia86.T_OP2_58* rs rd 1)	; OPTIMIZEME: remove next instr by specializing
  `(mov	(& ,(REG rd) ,(- 4 $tag.pair-tag)) (dword ,$imm.unspecified)))

(define-sassy-instr/peep (or (ia86.T_OP1_54* rs rd)	; cell-ref
                             (ia86.T_OP1_54))
  `(mov	,(REG rd) (& ,(REG rs) ,(- $tag.pair-tag))))

(define-sassy-instr (ia86.T_OP2_55 x)		; typetag-set!
  (ia86.loadr	$r.second x)
  (ia86.mcall	$m.typetag-set 'typetag-set))

(define-sassy-instr/peep (or (ia86.T_OP2_56* rs1 rd rs2)       	; eq?
                             (ia86.T_OP2_56 rs2))
  (cond ((is_hwreg rs2)
         `(cmp	,(REG rs1) ,(REG rs2)))
        (else
         `(cmp	,(REG rs1) (& ,$r.globals ,(G_REG rs2)))))
  (ia86.setcc	(REG rd) 'z))

;; XXX timer? XXX
(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_EQ? rs1 rs2 L a-skip?)
  (cond ((is_hwreg rs2)
         `(cmp	,(REG rs1) ,(REG rs2)))
        (else
         `(cmp	,(REG rs1) (& ,$r.globals ,(G_REG rs2)))))
  `(jne ,(t_label L)))

;; XXX timer? XXX
(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_EQ? rs1 imm L a-skip?)
  `(cmp	,(REG rs1) ,imm)
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.T_OP2_57 regno)		; eqv?
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.eqv 'eqv))

;; OPTIMIZEME: 42-46 bytes; can we do better?
(define-sassy-instr/peep (or (ia86.T_OP2_58* rs1 rd rs2)       	; cons
                             (ia86.T_OP2_58 rs2))
  (cond ((inline-allocation)
         (let ((L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L1)
           `(mov	,$r.temp (& ,$r.globals ,$g.etop))
           `(add	,$r.temp 8)
           `(cmp	,$r.temp ,$r.cont)
           `(jle short ,L2)
           (ia86.mcall	$m.morecore 'morecore)
           `(jmp short ,L1)
           `(label ,L2)
           `(mov	(& ,$r.globals ,$g.etop) ,$r.temp)
           `(mov	(& ,$r.temp -8) ,(REG rs1))
           (cond ((is_hwreg rs2)
                  `(mov (& ,$r.temp -4) ,(REG rs2))
                  `(lea	,(REG rd) (& ,$r.temp ,(+ -8 $tag.pair-tag))))
                 (else
                  `(lea	,$r.result (& ,$r.temp ,(+ -8 $tag.pair-tag)))
                  (ia86.loadr	$r.temp rs2)
                  `(mov	(& ,$r.result ,(+ (- $tag.pair-tag) 4)) ,$r.temp)
                  (cond ((not (result-reg? rd))
                         `(mov ,(REG rd) ,$r.result)))))
           ))
        (else
         `(mov	(& ,$r.globals ,$g.alloctmp) ,(REG rs1))
         `(mov	,$r.result 8)
         (ia86.mcall	$m.alloc 'alloc)
         `(mov	,$r.temp (& ,$r.globals ,$g.alloctmp))
         `(mov	(& ,$r.result)  ,$r.temp)
         (cond ((is_hwreg rs2)
                `(mov	(& ,$r.result 4) ,(REG rs2)))
               (else
                (ia86.loadr	$r.temp rs2)
                `(mov	(& ,$r.result 4) ,$r.temp)))
         `(add	,$r.result ,$tag.pair-tag)
         (cond ((not (result-reg? rd))
                `(mov ,(REG rd) ,$r.result)))
         )))

(define (reg/result->num r)
  (cond ((result-reg? r) -1)
        ((number? r) r)
        (else (error 'reg/result->num r))))
	
(define-sassy-instr/peep (or (ia86.T_OP2_59* rs1 rd rs2)	; set-car!
                             (ia86.T_OP2_59 rs2))
  (ia86.single_tag_test_ex (REG rs1) $tag.pair-tag $ex.setcar)
  (cond ((is_hwreg rs2)
         `(mov	(& ,(REG rs1) ,(- $tag.pair-tag)) ,(REG rs2))
         (ia86.write_barrier (reg/result->num rs1) rs2))
        (else
         (ia86.loadr	$r.second rs2)
         `(mov	(& ,(REG rs1) ,(- $tag.pair-tag)) ,$r.second)
         (ia86.write_barrier (reg/result->num rs1) -1))))

(define-sassy-instr/peep (or (ia86.T_OP2_60* rs1 rd rs2)	; set-cdr!
                             (ia86.T_OP2_60 rs2))
  (ia86.single_tag_test_ex (REG rs1) $tag.pair-tag $ex.setcdr)
  (cond ((is_hwreg rs2)
         `(mov	(& ,(REG rs1) ,(+ (- $tag.pair-tag) wordsize)) ,(REG rs2))
         (ia86.write_barrier (reg/result->num rs1) rs2))
        (else
         (ia86.loadr	$r.second rs2)
         `(mov	(& ,(REG rs1) ,(+ (- $tag.pair-tag) wordsize)) ,$r.second)
         (ia86.write_barrier (reg/result->num rs1) -1))))

(define-sassy-instr/peep (or (ia86.T_OP2_61* rs1 rd rs2) ; +
                             (ia86.T_OP2_61 rs2))
  (ia86.generic_arithmetic rs1 rd rs2  'add  'sub  $m.add))

(define-sassy-instr/peep (or (ia86.T_OP2_62* rs1 rd rs2) ; -
                             (ia86.T_OP2_62 rs2))
  (ia86.generic_arithmetic rs1 rd rs2 'sub  'add  $m.subtract))

(define-sassy-instr (ia86.T_OP2_63 regno)	; *
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.multiply 'multiply))
	
(define-sassy-instr (ia86.T_OP2_64 regno)	; /
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.divide 'divide))

(define-sassy-instr (ia86.T_OP2_65 regno)	; quotient
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.quotient 'quotient))

(define-sassy-instr (ia86.T_OP2_66 regno)	; <
  (ia86.generic_compare regno 'l  $m.numlt))
	
(define-sassy-instr (ia86.T_OP2_67 regno)	; <=
  (ia86.generic_compare regno 'le  $m.numle))

(define-sassy-instr (ia86.T_OP2_68 regno)	; =
  (ia86.generic_compare regno 'e  $m.numeq))

(define-sassy-instr (ia86.T_OP2_69 regno)	; >
  (ia86.generic_compare regno 'g  $m.numgt))

(define-sassy-instr (ia86.T_OP2_70 regno)	; >=
  (ia86.generic_compare regno 'ge $m.numge))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_66 rs1 rs2 L a-skip?)	; <
  (ia86.generic_compare_branchf (REG rs1) rs2 'jnl  $m.numlt L a-skip?))
	
(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_67 rs1 rs2 L a-skip?)	; <=
  (ia86.generic_compare_branchf (REG rs1) rs2 'jg  $m.numle L a-skip?))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_68 rs1 rs2 L a-skip?)	; =
  (ia86.generic_compare_branchf (REG rs1) rs2 'jne  $m.numeq L a-skip?))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_69 rs1 rs2 L a-skip?)	; >
  (ia86.generic_compare_branchf (REG rs1) rs2 'jng  $m.numgt L a-skip?))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_70 rs1 rs2 L a-skip?)	; >=
  (ia86.generic_compare_branchf (REG rs1) rs2 'jl $m.numge L a-skip?))


(define-sassy-instr (ia86.T_OP2_71 regno)	; fxlogand
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           (ia86.loadr	$r.second regno)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.logand L0)
           `(label ,L1)
           `(and	,$r.result ,$r.second)))
        ((is_hwreg regno)
         `(and	,$r.result ,(REG regno)))
        (else
         `(and	,$r.result (& ,$r.globals ,(G_REG regno))))))

(define-sassy-instr (ia86.T_OP2_72 regno)	; fxlogior
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           `(jz short ,L1)
           (ia86.loadr	$r.second regno)
           (ia86.exception_continuable $ex.logior L0)
           `(label ,L1)
           `(mov	,$r.result ,$r.temp)))
        ((is_hwreg regno)
         `(or	,$r.result ,(REG regno)))
        (else
         `(or	,$r.result (& ,$r.globals ,(G_REG regno))))))

(define-sassy-instr (ia86.T_OP2_73 regno)	; fxlogxor
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           (ia86.loadr	$r.second regno)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.logxor L0)
           `(label ,L1)	
           `(xor	,$r.result ,$r.second)))
        ((is_hwreg regno)
         `(xor	,$r.result ,(REG regno)))
        (else
         `(xor	,$r.result (& ,$r.globals ,(G_REG regno))))))

(define-sassy-instr (ia86.T_OP2_74 regno)		; lsh
  (ia86.fixnum_shift regno 'shl  $ex.lsh))
	
(define-sassy-instr (ia86.T_OP2_75 regno)		; rsha
  (ia86.fixnum_shift regno 'sar  $ex.rsha))

(define-sassy-instr (ia86.T_OP2_76 regno)		; rshl
  (ia86.fixnum_shift regno 'shr  $ex.rshl))
	
(define-sassy-instr (ia86.T_OP2_77 regno)		; rot
  (error 'T_OP2_rot "not implemented"))

(define-sassy-instr (ia86.T_OP2_78 regno)		; string-ref
  (ia86.indexed_structure_ref/hdr regno $tag.bytevector-tag  $hdr.string  $ex.sref #t)
  `(shl	,$r.result ,char_shift)
  `(or	,$r.result.low ,$imm.character))

(define-sassy-instr (ia86.T_OP3_79 regno y)		; string-set!
  (ia86.indexed_structure_set_char regno y  $tag.bytevector-tag  $hdr.string  $ex.sset))

(define-sassy-instr (ia86.T_OP2_80 regno)		; make-vector
  (ia86.make_indexed_structure_word regno $tag.vector-tag  $hdr.vector  $ex.mkvl))

(define-sassy-instr (ia86.T_OP2_81 regno)		; vector-ref
  (ia86.indexed_structure_ref/hdr regno $tag.vector-tag  $hdr.vector  $ex.vref #f))

(define-sassy-instr (ia86.T_OP2_82 regno)		; bytevector-ref
  (ia86.indexed_structure_ref/hdr regno $tag.bytevector-tag  $hdr.bytevector  $ex.bvref #t)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.T_OP2_700  rs2)	; bytevector-ref:trusted
  (ia86.load_from_indexed_structure rs2 $tag.bytevector-tag #t)
  `(shl ,$r.result 2))

(define-sassy-instr (ia86.T_OP2_83 regno)		; procedure-ref
  (ia86.indexed_structure_ref regno $tag.procedure-tag  $ex.pref #f))

(define-sassy-instr (ia86.T_OP2_84 regno)		; cell-set!
  (cond ((is_hwreg regno)
         `(mov	(& ,$r.result ,(- $tag.pair-tag)) ,(REG regno))
         (ia86.write_barrier -1 regno))
        (else
         (ia86.loadr	$r.second regno)
         `(mov	(& ,$r.result ,(- $tag.pair-tag)) ,$r.second)
         (ia86.write_barrier -1 -1))))

(define-sassy-instr (ia86.T_OP2_85 regno)		; char<?
  (ia86.generic_char_compare regno 'l  $ex.char<?))

(define-sassy-instr (ia86.T_OP2_86 regno)		; char<=?
  (ia86.generic_char_compare regno 'le  $ex.char<=?))

(define-sassy-instr (ia86.T_OP2_87 regno)		; char=?
  (ia86.generic_char_compare regno 'e  $ex.char=?))

(define-sassy-instr (ia86.T_OP2_88 regno)		; char>?
  (ia86.generic_char_compare regno 'g  $ex.char>?))

(define-sassy-instr (ia86.T_OP2_89 regno)		; char>=?
  (ia86.generic_char_compare regno 'ge  $ex.char>=?))

(define-sassy-instr (ia86.T_OP2_90 regno)		; sys$partial-list->vector
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.partial-list->vector 'partial-list->vector))

(define-sassy-instr (ia86.T_OP3_91 regno y)		; vector-set!
  (ia86.indexed_structure_set_word regno y  $tag.vector-tag  $hdr.vector  $ex.vset))

(define-sassy-instr (ia86.T_OP3_92 regno y)		; bytevector-set!
  (ia86.indexed_structure_set_byte regno y  $tag.bytevector-tag  $hdr.bytevector  $ex.bvset))

(define-sassy-instr (ia86.T_OP3_93 regno y)		; procedure-set!
  (ia86.indexed_structure_set_word regno y  $tag.procedure-tag  0  $ex.pset))

(define-sassy-instr/peep (or (ia86.T_OP1_94* rs rd)		; bytevector-like?
                             (ia86.T_OP1_94))
  (ia86.single_tag_test (REG rs) $tag.bytevector-tag)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr/peep (or (ia86.T_OP1_95* rs rd)		; vector-like?
                             (ia86.T_OP1_95))
  (ia86.single_tag_test (REG rs) $tag.vector-tag)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr (ia86.T_OP2_96 regno)		; bytevector-like-ref
  (ia86.indexed_structure_ref regno $tag.bytevector-tag  $ex.bvlref #t)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.T_OP3_97 regno y)		; bytevector-like-set!
  (ia86.indexed_structure_set_byte regno y  $tag.bytevector-tag  0  $ex.bvlset))

(define-sassy-instr (ia86.T_OP2_98 regno)		; sys$bvlcmp
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.bvlcmp 'bvlcmp))

(define-sassy-instr (ia86.T_OP2_99 regno)		; vector-like-ref
  (ia86.indexed_structure_ref regno $tag.vector-tag  $ex.vlref  #f))

(define-sassy-instr (ia86.T_OP3_100 regno y)		; vector-like-set!
  (ia86.indexed_structure_set_word regno y  $tag.vector-tag  0  $ex.vlset))

(define-sassy-instr (ia86.T_OP1_101)		; vector-like-length
  (ia86.indexed_structure_length $tag.vector-tag $ex.vllen #f))

(define-sassy-instr (ia86.T_OP1_102)		; bytevector-like-length
  (ia86.indexed_structure_length $tag.bytevector-tag $ex.bvllen #t))

(define-sassy-instr (ia86.T_OP2_103 regno)		; remainder
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.remainder 'remainder))

(define-sassy-instr (ia86.T_OP1_104)		; petit-patch-boot-code
  (ia86.mcall	$m.petit-patch-boot-code 'petit-patch-boot-code))

(define-sassy-instr (ia86.T_OP1_105)		; syscall
  (ia86.mcall	$m.syscall 'syscall))

(define-sassy-instr (ia86.T_OP1_106)		; creg
  (ia86.mcall	$m.creg 'creg))

(define-sassy-instr (ia86.T_OP1_107)		; creg-set!
  (ia86.mcall	$m.creg-set! 'creg-set!))

(define-sassy-instr (ia86.T_OP1_108)		; gc-counter
  `(mov	,$r.result (& ,$r.globals ,$g.gccnt)))

(define-sassy-instr (ia86.T_OP2_109 regno)		; make-string
  ;; exception code wrong, matches Sparc
  (ia86.make_indexed_structure_byte regno $hdr.string  $ex.mkbvl))

(define-sassy-instr (ia86.T_OP2IMM_128 imm)		; typetag-set!
  (ia86.const2regf $r.second imm)
  (ia86.mcall	$m.typetag-set 'typetag-set))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_129* rs1 rd imm)	; eq?
                             (ia86.T_OP2IMM_129 imm))
  `(cmp	,(REG rs1) ,imm)
  (ia86.setcc	(REG rd) 'z))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_130* rs1 rd imm)	; +
                             (ia86.T_OP2IMM_130 imm))
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	,(try-low (REG rs1)) ,fixtag_mask)
    `(jnz short ,L1)
    (cond ((not (equal? rs1 rd))
           `(mov       ,(REG rd) ,(REG rs1))))
    `(add	,(REG rd) ,imm)
    `(jno short ,L2)
    `(sub	,(REG rd) ,imm)
    `(label ,L1)
    (cond ((and (not (result-reg? rd))
                (not (result-reg? rs1)))
           `(mov       ,$r.result ,(REG rs1))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.add 'add)
    (cond ((not (result-reg? rd))
           `(mov       ,(REG rd) ,$r.result)))
    `(label ,L2)))


(define-sassy-instr/peep (or (ia86.T_OP2IMM_131* rs1 rd imm)	; -
                             (ia86.T_OP2IMM_131 imm))
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	,(try-low (REG rs1)) ,fixtag_mask)
    `(jnz short ,L1)
    (cond ((not (equal? rs1 rd))
           `(mov ,(REG rd) ,(REG rs1))))
    `(sub	,(REG rd) ,imm)
    `(jno short ,L2)
    `(add	,(REG rd) ,imm)
    `(label ,L1)
    (cond ((and (not (result-reg? rd))
                (not (result-reg? rs1)))
           `(mov       ,$r.result ,(REG rs1))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.subtract 'subtract)
    (cond ((not (result-reg? rd))
           `(mov       ,(REG rd) ,$r.result)))
    `(label ,L2)))

(define-sassy-instr (ia86.T_OP2IMM_132 imm)		; <
  (ia86.generic_imm_compare imm 'l  $m.numlt))

(define-sassy-instr (ia86.T_OP2IMM_133 imm)		; <=
  (ia86.generic_imm_compare imm 'le  $m.numle))

(define-sassy-instr (ia86.T_OP2IMM_134 imm)		; =
  (ia86.generic_imm_compare imm 'e  $m.numeq))

(define-sassy-instr (ia86.T_OP2IMM_135 imm)		; >
  (ia86.generic_imm_compare imm 'g  $m.numgt))

(define-sassy-instr (ia86.T_OP2IMM_136 imm)		; >=
  (ia86.generic_imm_compare imm 'ge  $m.numge))

;; XXX timer? XXX
(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_132 rs imm L a-skip?)	; <
  (ia86.generic_imm_compare_branchf rs imm 'jnl $m.numlt L))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_133 rs imm L a-skip?)	; <=
  (ia86.generic_imm_compare_branchf rs imm 'jg  $m.numle L))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_134 rs imm L a-skip?)	; =
  (ia86.generic_imm_compare_branchf rs imm 'jne $m.numeq L))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_135 rs imm L a-skip?)	; >
  (ia86.generic_imm_compare_branchf rs imm 'jng $m.numgt L))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_136 rs imm L a-skip?)	; >=
  (ia86.generic_imm_compare_branchf rs imm 'jl  $m.numge L))


(define-sassy-instr (ia86.T_OP2IMM_137 imm)		; char<?
  (ia86.generic_char_imm_compare imm 'l  $ex.char<?))

(define-sassy-instr (ia86.T_OP2IMM_138 imm)		; char<=?
  (ia86.generic_char_imm_compare imm 'le  $ex.char<=?))

(define-sassy-instr (ia86.T_OP2IMM_139 imm)		; char=?
  (ia86.generic_char_imm_compare imm 'e  $ex.char=?))

(define-sassy-instr (ia86.T_OP2IMM_140 imm)		; char>?
  (ia86.generic_char_imm_compare imm 'g  $ex.char>?))

(define-sassy-instr (ia86.T_OP2IMM_141 imm)		; char>=?
  (ia86.generic_char_imm_compare imm 'ge  $ex.char>=?))

;;; The following five are probably a waste of effort.

(define-sassy-instr (ia86.T_OP2IMM_142 imm)		; string-ref
  (ia86.indexed_structure_ref_imm/hdr imm $tag.bytevector-tag  $hdr.string  $ex.sref  #t)
  `(shl	,$r.result ,char_shift)
  `(or	,$r.result.low ,$imm.character))

(define-sassy-instr (ia86.T_OP2IMM_143 imm)		; vector-ref
  (ia86.indexed_structure_ref_imm/hdr imm $tag.vector-tag  $hdr.vector  $ex.vref  #f))

(define-sassy-instr (ia86.T_OP2IMM_144 regno)		; bytevector-ref
  (ia86.indexed_structure_ref_imm/hdr regno $tag.bytevector-tag  $hdr.bytevector  $ex.bvref  #t)
  `(shl	,$r.result 2))
	
(define-sassy-instr (ia86.T_OP2IMM_145 imm)		; bytevector-like-ref
  (ia86.indexed_structure_ref_imm imm $tag.bytevector-tag  $ex.bvlref  #t)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.T_OP2IMM_146 imm)		; vector-like-ref
  (ia86.indexed_structure_ref_imm imm $tag.vector-tag  $ex.vlref  #f))

(define-sassy-instr (ia86.T_OP1_200)		; most-positive-fixnum
  (ia86.const2regf $r.result #x7FFFFFFC))

(define-sassy-instr (ia86.T_OP1_201)		; most-negative-fixnum
  (ia86.const2regf $r.result #x80000000))

(define-sassy-instr (ia86.T_OP2_202 regno)		; fx+
  (ia86.fixnum_arithmetic regno 'add  'sub  $ex.fx+))

(define-sassy-instr (ia86.T_OP2_203 regno)		; fx-
  (ia86.fixnum_arithmetic regno 'sub  'add  $ex.fx-))

(define-sassy-instr (ia86.T_OP1_204)		; fx--
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           `(test	,$r.result.low ,fixtag_mask)
           `(jnz short ,L1)
           `(neg	,$r.result)
           `(jno short ,L2)
           ;; No need to undo: RESULT is unchanged
           `(label ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fx-- L0)
           `(label ,L2)))
        (else
         `(neg	,$r.result))))

;; OPTIMIZEME: Introduce OP2IMM form of fx* that uses LEA tricks
(define-sassy-instr (ia86.T_OP2_205 regno)              ; fx*
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           `(jnz short ,L1)
           (ia86.loadr	$r.temp regno)
           `(sar	,$r.temp 2)
           `(imul	,$r.temp ,$r.result)
           `(jno short ,L2)
           `(label ,L1)
           (ia86.loadr	$r.temp regno)
           (ia86.exception_continuable $ex.fx* L0)
           `(label ,L2)
           `(mov	,$r.result ,$r.temp)))
        ((is_hwreg regno)
         `(sar	,$r.result 2)
         `(imul	,$r.result ,(REG regno)))
        (else
         `(sar	,$r.result 2)
         `(imul	,$r.result (& ,$r.globals ,(G_REG regno))))))

(define-sassy-instr (ia86.T_OP2_206 regno)		; fx=
  (ia86.fixnum_compare regno 'e  $ex.fx=))

(define-sassy-instr (ia86.T_OP2_207 regno)		; fx<
  (ia86.fixnum_compare regno 'l  $ex.fx<))

(define-sassy-instr (ia86.T_OP2_208 regno)		; fx<=
  (ia86.fixnum_compare regno 'le  $ex.fx<=))

(define-sassy-instr (ia86.T_OP2_209 regno)		; fx>
  (ia86.fixnum_compare regno 'g  $ex.fx>))

(define-sassy-instr (ia86.T_OP2_210 regno)		; fx>=
  (ia86.fixnum_compare regno 'ge  $ex.fx>=))

; Changed T_OP2_2{11,12,13} to OP1.
; Do we refer to these as OP2 anywhere?
(define-sassy-instr (ia86.T_OP1_211)		; fxzero?
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	,$r.result.low ,fixtag_mask)
           `(jz short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fxzero? L0)
           `(label ,L1))))
  `(test	,$r.result ,$r.result)
  (ia86.setcc	$r.result 'z))

(define-sassy-instr (ia86.T_OP1_212)		; fxpositive?
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	,$r.result.low ,fixtag_mask)
           `(jz short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fxpositive? L0)
           `(label ,L1))))
  `(cmp	,$r.result 0)
  (ia86.setcc	$r.result 'g))

(define-sassy-instr (ia86.T_OP1_213)		; fxnegative?
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	,$r.result.low ,fixtag_mask)
           `(jz short ,L1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fxnegative? L0)
           `(label ,L1))))
  `(cmp	,$r.result 0)
  (ia86.setcc $r.result 'l))

(define-sassy-instr (ia86.fixnum_imm_arithmetic imm y z ex)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.const2regf $r.temp imm)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           `(jnz short ,L1)
           (ia86.const2regf $r.temp imm)
           `(,y	,$r.result ,$r.temp)
           `(jno short ,L2)
           `(,z	,$r.result ,$r.temp)
           `(label ,L1)
           (ia86.exception_continuable ex L0)	; second is tmp so 2nd arg is in place
           `(label ,L2)))
        (else
         (ia86.const2regf $r.temp imm)
         `(,y	,$r.result ,$r.temp))))
	
(define-sassy-instr (ia86.T_OP2IMM_250 imm)           ; fx+
  (ia86.fixnum_imm_arithmetic imm 'add  'sub  $ex.fx+))

(define-sassy-instr (ia86.T_OP2IMM_251 imm)           ; fx-
  (ia86.fixnum_imm_arithmetic imm 'sub  'add  $ex.fx-))

;;; fixnum_imm_compare const, cc, ex
(define-sassy-instr (ia86.fixnum_imm_compare imm y z)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           (ia86.const2regf $r.temp imm)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,fixtag_mask)
           `(jz short ,L1)
           (ia86.const2regf $r.temp imm)
           (ia86.exception_continuable z L0)	; second is tmp so 2nd arg is in place
           `(label ,L1)))
    (ia86.const2regf $r.temp imm)
    `(cmp	,$r.result 	,$r.temp)
    (ia86.setcc $r.result y)))

(define-sassy-instr (ia86.T_OP2IMM_253 imm)		; fx=
  (ia86.fixnum_imm_compare imm 'e  $ex.fx=))

(define-sassy-instr (ia86.T_OP2IMM_254 imm)		; fx<
  (ia86.fixnum_imm_compare imm 'l  $ex.fx<))

(define-sassy-instr (ia86.T_OP2IMM_255 imm)		; fx<=
  (ia86.fixnum_imm_compare  imm 'le  $ex.fx<=))

(define-sassy-instr (ia86.T_OP2IMM_256 imm)		; fx>
  (ia86.fixnum_imm_compare  imm 'g  $ex.fx>))

(define-sassy-instr (ia86.T_OP2IMM_257 imm)		; fx>=
  (ia86.fixnum_imm_compare  imm 'ge  $ex.fx>=))

;;; Unsafe/trusted primitives

(define-sassy-instr/peep (or (ia86.T_OP1_401* rs rd)		; vector-length:vec
                             (ia86.T_OP1_401))
  `(mov	,(REG rd) (& ,(REG rs) ,(- $tag.vector-tag)))
  `(shr	,(REG rd) 8))

(define-sassy-instr/peep (or (ia86.T_OP2_402* rs1 rd rs2)	; vector-ref:trusted
                             (ia86.T_OP2_402 rs2))
  (cond ((is_hwreg rs2)
         `(mov	,(REG rd) (& ,(REG rs1) 
                             ,(REG rs2)
                             ,(- wordsize $tag.vector-tag))))
        (else
         (ia86.loadr	$r.temp rs2)
         `(mov	,(REG rd) (& ,(REG rs1) 
                             ,$r.temp 
                             ,(- wordsize $tag.vector-tag))))))

(define-sassy-instr (ia86.T_OP3_403 regno y)		; vector-set!:trusted
  (ia86.do_indexed_structure_set_word $r.result regno y $tag.vector-tag))

(define-sassy-instr (ia86.T_REG_OP3_403 hwregno regno2 regno3)
  (ia86.do_indexed_structure_set_word hwregno regno2 regno3 $tag.vector-tag))

(define-sassy-instr/peep (or (ia86.T_OP1_404* rs rd)		; car:pair
                             (ia86.T_OP1_404))
  `(mov	,(REG rd) (& ,(REG rs) ,(- $tag.pair-tag))))

(define-sassy-instr/peep (or (ia86.T_OP1_405* rs rd)		; cdr:pair
                             (ia86.T_OP1_405))
  `(mov	,(REG rd) (& ,(REG rs) ,(+ (- $tag.pair-tag) wordsize))))

(define-sassy-instr/peep (or (ia86.T_OP2_406* rs1 rd rs2)		; =:fix:fix
                             (ia86.T_OP2_406 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'e))

(define-sassy-instr/peep (or (ia86.T_OP2_407* rs1 rd rs2)		; <:fix:fix
                             (ia86.T_OP2_407 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'l))

(define-sassy-instr/peep (or (ia86.T_OP2_408* rs1 rd rs2)		; <=:fix:fix
                             (ia86.T_OP2_408 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'le))

(define-sassy-instr/peep (or (ia86.T_OP2_409* rs1 rd rs2)		; >=:fix:fix
                             (ia86.T_OP2_409 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'ge))

(define-sassy-instr/peep (or (ia86.T_OP2_410* rs1 rd rs2)		; >:fix:fix
                             (ia86.T_OP2_410 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'g))

(define-sassy-instr (ia86.trusted_fixnum_compare_branch hwregno regno L jnc)
  (cond ((is_hwreg regno)
         `(cmp	,(REG hwregno) ,(REG regno)))
        (else
         `(cmp	,(REG hwregno) (& ,$r.globals ,(G_REG regno)))))
  `(,jnc ,(t_label L)))

;; For these, we jump on the *opposite* of the condition we are
;; testing for, b/c the check jumps if condition *fails*
(define-sassy-instr (ia86.T_REG_OP2_CHECK_406 hwregno regno2 L)	; =:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jne))

(define-sassy-instr (ia86.T_REG_OP2_CHECK_407 hwregno regno2 L)	; <:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jge))

(define-sassy-instr (ia86.T_REG_OP2_CHECK_408 hwregno regno2 L)	; <=:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jg))

(define-sassy-instr (ia86.T_REG_OP2_CHECK_409 hwregno regno2 L)	; >=:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jl))

(define-sassy-instr (ia86.T_REG_OP2_CHECK_410 hwregno regno2 L)	; >:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jle))


(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_406 hwregno regno2 L a-skip?)	; =:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jne))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_407 hwregno regno2 L a-skip?)	; <:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jge))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_408 hwregno regno2 L a-skip?)	; <=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jg))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_409 hwregno regno2 L a-skip?)	; >=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jl))

(define-sassy-instr (ia86.T_REG_OP2_BRANCHF_410 hwregno regno2 L a-skip?)	; >:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 L 'jle))


(define-sassy-instr/peep (or (ia86.T_OP2IMM_450* rs1 rd imm)	; vector-ref:trusted
                             (ia86.T_OP2IMM_450 imm))
  `(mov	,(REG rd) (& ,(REG rs1) ,imm ,(- wordsize $tag.vector-tag))))

(define-sassy-instr (ia86.T_OP2IMM_700  idx)	; bytevector-ref:trusted
  (ia86.load_from_indexed_structure_imm idx $tag.bytevector-tag #t)
  `(shl ,$r.result 2))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_451* rs1 rd imm)		; =:fix:fix
                             (ia86.T_OP2IMM_451 imm))
  `(cmp	,(REG rs1) ,imm)
  (ia86.setcc	(REG rd) 'e))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_452* rs1 rd imm)		; <:fix:fix
                             (ia86.T_OP2IMM_452 imm))
  `(cmp	,(REG rs1) ,imm)
  (ia86.setcc	(REG rd) 'l))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_453* rs1 rd imm)		; <=:fix:fix
                             (ia86.T_OP2IMM_453 imm))
  `(cmp	,(REG rs1) ,imm)
  (ia86.setcc	(REG rd) 'le))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_454* rs1 rd imm)		; >:fix:fix
                             (ia86.T_OP2IMM_454 imm))
  `(cmp	,(REG rs1) ,imm)
  (ia86.setcc	(REG rd) 'g))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_455* rs1 rd imm)		; >=:fix:fix
                             (ia86.T_OP2IMM_455 imm))
  `(cmp	,(REG rs1) ,imm)
  (ia86.setcc	(REG rd) 'ge))

(define-sassy-instr (ia86.trusted_fixnum_compare_imm_branch hwregno imm L jnc)
  `(cmp	,(REG hwregno) ,imm)
  `(,jnc ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP2IMM_CHECK_451 hwregno imm L)	; =:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jne))

(define-sassy-instr (ia86.T_REG_OP2IMM_CHECK_452 hwregno imm L)	; <:fix:fix
 (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jnl))

(define-sassy-instr (ia86.T_REG_OP2IMM_CHECK_453 hwregno imm L) ; <=:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jg))

(define-sassy-instr (ia86.T_REG_OP2IMM_CHECK_454 hwregno imm L)	; >:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jng))

(define-sassy-instr (ia86.T_REG_OP2IMM_CHECK_455 hwregno imm L)	; >=:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jl))


(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_451 hwregno imm L a-skip?) ; =:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jne))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_452 hwregno imm L a-skip?) ; <:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jnl))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_453 hwregno imm L a-skip?) ; <=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jg))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_454 hwregno imm L a-skip?) ; >:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jng))

(define-sassy-instr (ia86.T_REG_OP2IMM_BRANCHF_455 hwregno imm L a-skip?) ; >=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm L 'jl))


;;; Introduced by representation inference.  Trusted.

(define-sassy-instr (ia86.T_OP2_500 regno)		; +:idx:idx
  (cond ((is_hwreg regno)
         `(add	,$r.result ,(REG regno)))
        (else
         `(add	,$r.result (& ,$r.globals ,(G_REG regno))))))

(define-sassy-instr (ia86.T_OP2_501 regno)		; +:fix:fix
  (let ((L1 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(add	,$r.result ,$r.temp)
    `(jno short ,L1)
    `(sub	,$r.result ,$r.temp)
    (ia86.mcall	$m.add 'add)                          ; second is temp so 2nd arg is in place
    `(label ,L1)))

(define-sassy-instr (ia86.T_OP2_502 regno)		; -:idx:idx
  (cond ((is_hwreg regno)
         `(sub	,$r.result ,(REG regno)))
        (else
         `(sub	,$r.result (& ,$r.globals ,(G_REG regno))))))

(define-sassy-instr (ia86.T_OP2_503 regno)		; -:fix:fix
  (let ((L1 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(sub	,$r.result ,$r.temp)
    `(jno short ,L1)
    `(add	,$r.result ,$r.temp)
    (ia86.mcall	$m.subtract 'subtract)	; second is temp so 2nd arg is in place
    `(label ,L1)))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_520* rs1 rd imm)	; +:idx:idx
                             (ia86.T_OP2IMM_520 imm))
  `(lea ,(REG rd) (& ,(REG rs1) ,imm)))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_521* rs1 rd imm)	; +:fix:fix
                             (ia86.T_OP2IMM_521 imm))
  (cond ((not (equal? rs1 rd))
         `(mov ,(REG rd) ,(REG rs1))))
  (let ((L1 (fresh-label)))
    `(add	,(REG rd) ,imm)
    `(jno short ,L1)
    `(sub	,(REG rd) ,imm)
    (cond ((not (result-reg? rd))
           `(mov       ,$r.result ,(REG rd))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.add 'add)
    (cond ((not (result-reg? rd))
           `(mov    ,(REG rd) ,$r.result)))
    `(label ,L1)))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_522* rs1 rd imm)	; -:idx:idx
                             (ia86.T_OP2IMM_522 imm))
  `(lea ,(REG rd) (& ,(REG rs1) ,(- imm))))

(define-sassy-instr/peep (or (ia86.T_OP2IMM_523* rs1 rd imm)	; -:fix:fix
                             (ia86.T_OP2IMM_523 imm))
  (cond ((not (equal? rs1 rd))
         `(mov  ,(REG rd) ,(REG rs1))))
  (let ((L1 (fresh-label)))
    `(sub	,(REG rd) ,imm)
    `(jno short ,L1)
    `(add	,(REG rd) ,imm)
    (cond ((not (result-reg? rd))
           `(mov      ,$r.result ,(REG rd))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.subtract 'subtract)
    (cond ((not (result-reg? rd))
           `(mov     ,(REG rd) ,$r.result)))
    `(label ,L1)))

(define-sassy-instr (ia86.reg_generic_compare_lowimm_branchf imm rs L a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  `(cmp	,(try-low (REG rs)) ,imm)
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.reg_generic_compare_imm_branchf imm rs L a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  `(cmp	,(REG rs) ,imm)
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP1_BRANCHF_NULL? rs L a-skip?)
  (ia86.reg_generic_compare_lowimm_branchf $imm.null rs L a-skip?))

(define-sassy-instr (ia86.T_REG_OP1_BRANCHF_EOF_OBJECT? rs L a-skip?)
  (ia86.reg_generic_compare_imm_branchf $imm.eof rs L a-skip?))

(define-sassy-instr (ia86.T_REG_OP1_BRANCHF_PAIR? rs L a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.single_tag_test (REG rs) $tag.pair-tag)
  `(jnz ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP1_BRANCHF_ZERO? rs L a-skip?)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (cond ((not a-skip?)
           (ia86.timer_check)))
    (ia86.fixnum_test_temp_is_free rs)
    `(jz short ,L1)
    `(mov ,$r.result ,(REG rs))
    (ia86.mcall	$m.zerop 'zerop)
    `(cmp ,$r.result.low ,$imm.false)
    `(je  ,(t_label L))
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp       ,(REG rs) 0)
    `(jnz ,(t_label L))
    `(label ,L2)))

(define-sassy-instr (ia86.T_REG_OP1_BRANCHF_FIXNUM? rs L a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (cond ((hwreg_has_low rs)
         `(test ,(REG_LOW rs) ,fixtag_mask))
        (else
         `(mov ,$r.result ,(REG rs))
         `(test ,$r.result.low ,fixtag_mask)))
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP1_CHECK_FIXNUM? rs L)
  (cond ((hwreg_has_low rs)
         `(test ,(REG_LOW rs) ,fixtag_mask))
        (else
         `(mov ,$r.result ,(REG rs))
         `(test ,$r.result.low ,fixtag_mask)))
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP1_CHECK_PAIR? rs L)
  (ia86.single_tag_test (REG rs) $tag.pair-tag)
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP1_CHECK_VECTOR? rs L)
  (ia86.double_tag_test (REG rs) $tag.vector-tag $hdr.vector)
  `(jne ,(t_label L)))

(define-sassy-instr (ia86.T_REG_OP1_CHECK_STRING? rs L)
  (ia86.double_tag_test (REG rs) $tag.bytevector-tag $hdr.string)
  `(jne ,(t_label L)))

;; "Reflective ops"; processor level performance measurement.
;; Very sketch!

(define-sassy-instr (ia86.T_OP1_1000)           ; larceny-timestamp!
  (let ((L3 (fresh-label)))
    (ia86.double_tag_test $r.result $tag.bytevector-tag $hdr.bytevector)
    ;; FIXME: should also check length >= 8
    `(jnz short ,L3)                        ;; leave RESULT alone if arg bad
    `(mov (& ,$r.globals ,G_REGALIAS_EBX) ebx) ;; save RESULT
    `(mov (& ,$r.globals ,G_REGALIAS_ECX) ecx) ;;  and
    `(mov (& ,$r.globals ,G_REGALIAS_EDX) edx) ;;   others
    ;; `(cpuid)                                ;; serialize (writes eax, ebx, ecx, edx)
    `(rdtsc)                                ;; writes 64 bit value into edx:eax
    `(mov ebx (& ,$r.globals ,G_REGALIAS_EBX)) ;; restore RESULT
    `(mov (& ,$r.result ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES)) 
          eax)                              ;; save timestamp low bits
    `(mov (& ,$r.result ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES wordsize))
          edx)                              ;; save timestamp high bits
    ;; `(cpuid)                                ;; serialize (writes eax, ebx, ecx, edx)
    `(mov ebx (& ,$r.globals ,G_REGALIAS_EBX)) ;; restore RESULT
    `(mov ecx (& ,$r.globals ,G_REGALIAS_ECX)) ;;  and
    `(mov edx (& ,$r.globals ,G_REGALIAS_EDX)) ;;   others
    `(label ,L3)))

(define-sassy-instr (ia86.T_OP1_1001)
  `(rdpmc))

;;; eof

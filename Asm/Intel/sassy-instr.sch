;;; NASM/i386 macros for the MacScheme instruction set.
;;;
;;; $Id: i386-instr.asm 2715 2005-12-14 22:27:51Z tov $
;;; 
;;; On the i386 architecture, the Twobit native assembler emits
;;; textual assembly language rather than machine code.  The output
;;; is then assembled with nasm, "The Netwide Assembler", a macro
;;; assembler that runs on many platforms and accepts the same syntax
;;; everywhere (namely, standard Intel syntax).  The assembled files
;;; can then be loaded into Larceny via its already-existing mechanism
;;; for loading code dynamically.
;;; 
;;; Because nasm is a macro assembler, Twobit's assembler just outputs
;;; something that is only superficially different from MacScheme assembly
;;; language.  The present file defines the macros that allow that output
;;; to be assembled.
;;; 
;;; Sections in this file
;;;   - Section 1: handy macros
;;;   - Section 2: utility macros for MAL instruction definitions
;;;   - Section 3: MAL instruction definitions
;;;   - Section 4: MAL primitive operations (OP1, OP2, OP2IMM, OP3)
;;; 
;;; Conventions in the generated code:
;;;   - All generated codevectors are bracketed by uses of
;;;     'begin_codevector' and 'end_codevector'.
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

(define (unsafe-globals)
  (unsafe-code))

(define sassy-instr-directives '())

(define-syntax define-sassy-constant
  (syntax-rules ()
    ((_ NAME VAL)
     (begin 
       (define NAME VAL)
       (set! sassy-instr-directives
             (cons '(macro NAME (! VAL))
                   sassy-instr-directives))))))
  
(define-syntax define-sassy-macro
  (syntax-rules ()
    ((_ (NAME ARGS ...) BODY)
     (begin 
       (define (NAME ARGS ...) BODY)
       (set! sassy-instr-directives
             (cons '(macro NAME (lambda (ARGS ...) BODY))
                   sassy-instr-directives))))))

(define-syntax define-sassy-instr
  (syntax-rules ()
    ((_ (NAME ARGS ...) BODY ...)
     (define (NAME ARGS ...) (seqlist BODY ...))))) ; do this for now...

;; Has to be a macro since order of eval is undef'd in Scheme
(define-syntax seqlist 
  (syntax-rules (begin cond let quasiquote)
    ((seqlist) 
     (list))
    ((_ (begin EXP ...) EXPS ...)
     (append (begin EXP ...) 
             (seqlist EXPS ...)))
    ((_ (cond (Q A ...) ...) EXPS ...)
     (append (mcond (Q (seqlist A ...)) ...) 
             (seqlist EXPS ...)))
    ((_ (let ((I E) ...) BODY ...) EXPS ...)
     (append (let ((I E) ...) (seqlist BODY ...)) 
             (seqlist EXPS ...)))
    ;; Note in below two clauses, first uses CONS, second uses APPEND
    ((_ (quasiquote EXP1) EXPS ...)
     (cons (quasiquote EXP1) (seqlist EXPS ...)))
    ((_ EXP1 EXPS ...)
     (let ((v EXP1))
       (append v (seqlist EXPS ...))))))

(define-syntax mcond
  (syntax-rules (else)
    ((_) '())
    ((_ (else A)) A)
    ((_ (Q A) C ...) (if Q A (mcond C ...)))))
     

;; ;; NAME : Operand ... -> [Listof SassyInstr]
;; (define-sassy-instr (NAME ARGS ...) BODY)

(define-sassy-constant PAIR_TAG $tag.pair-tag)
(define-sassy-constant VEC_TAG $tag.vector-tag)  
(define-sassy-constant PROC_TAG $tag.procedure-tag)  
(define-sassy-constant M_GLOBAL_EX $m.global-ex)
(define-sassy-constant M_INVOKE_EX $m.invoke-ex)
(define-sassy-constant M_GLOBAL_INVOKE_EX $m.global-invoke-ex)

(define (G_REG n) (+ 12 (* n 4))) ;; FSK: YUCK!  HACK!
(define (REG n) (case n           ;; FSK: YUCK!  ANOTHER HACK!
                  ((1) 'ecx)
                  ((2) 'edx)
                  ((3) 'edi)
                  ((4) 'esi)
                  (else (error 'REG "unknown register"))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Handy macros for this and that.  More fundamental stuff is
;;; defined in i386-machine.ah

(define-sassy-constant fixtag_mask	    3)
(define-sassy-constant tag_mask             7)
(define-sassy-constant hdr_shift            8)
(define-sassy-constant char_shift	    16)

(define-sassy-macro (is_hwreg n)         (<= FIRST_HWREG n LAST_HWREG))
(define-sassy-macro (fixnum n)           (lsh n 2))
(define-sassy-macro (roundup4 x)	 (logand (+ x 3) (complement 3)))
(define-sassy-macro (roundup8 x)	 (logand (+ x 7) (complement 7)))
(define-sassy-macro (words2bytes n)      (* n 4))
(define-sassy-macro (stkslot n)          (+ CONT STK_REG0 (words2bytes n)))
(define-sassy-macro (framesize n)        (roundup8 (+ wordsize STK_OVERHEAD (words2bytes n))))
(define-sassy-macro (recordedsize n)     (+ STK_OVERHEAD (words2bytes n)))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Utility macros for MAL instruction definitions

(define-sassy-instr (ia86.mcall fcn)
  `(call	(& GLOBALS ,fcn))
  `(align code_align))

;;; loadr targetreg, regno
;;; 	load HW register targetreg from VM register regno
	
(define-sassy-instr (ia86.loadr targetreg regno)
  (cond ((is_hwreg regno)
         `(mov ,targetreg (REG ,regno)))
        (else 
         `(mov ,targetreg (& GLOBALS ,(+ (G_REG regno)))))))

;;; storer regno, sourcereg
;;;     store VM register regno from HW register sourcereg
;;;     Does not destroy sourcereg

(define-sassy-instr (ia86.storer regno sourcereg)
  (cond ((is_hwreg regno)
         `(mov	,(REG regno) ,sourcereg))
        (else
         `(mov  (& GLOBALS ,(+ (G_REG regno))) ,sourcereg))))

;;; loadc hwreg, slot
;;;	Load constant vector element 'slot' into hwreg
	
(define-sassy-instr (ia86.loadc hwreg slot)
  (ia86.loadr	hwreg 0)
  `(mov	,hwreg (& ,hwreg ,(+ (- PROC_TAG) PROC_CONSTVECTOR)))
  `(mov	,hwreg (& ,hwreg ,(+ (- VEC_TAG) (words2bytes (+ slot 1))))))

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
             `(test	(reg ,r2) 1)
             `(jz short ,L0)
             `(mov	SECOND (reg ,r2)))
            (else
             (cond ((not (= r2 -1))
                    `(mov	SECOND ,(REG r2))))
             `(test	SECOND 1)
             `(jz short ,L0)))
      (cond ((not (= r1 -1))
             `(mov	RESULT ,(reg r1))))
      (ia86.mcall $m.partial-barrier)
      `(label ,L0)))
   (else 
    (cond ((not (= r1 -1))
           `(mov RESULT ,(reg r1))))
    (cond ((not (= r2 -1))
           `(mov SECOND ,(reg r2))))
    (ia86.mcall $m.full-barrier))))
	
;;; timer_check
;;;	decrement timer and take interrupt if zero

(define-sassy-instr (ia86.timer_check)
  (let ((L1 (fresh-label)))
    `(dec (dword (& (+ GLOBALS G_TIMER))))
    `(jnz short ,L1)
    (ia86.mcall	$m.timer-exception)
    `(label ,L1)))

;;; exception_noncontinuable excode
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is noncontinuable
	
(define-sassy-instr (ia86.exception_noncontinuable excode)
  `(call	(& (+ GLOBALS ,$m.exception)))
  `(dw	,excode))
		
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
  `(call	(& (+ GLOBALS ,$m.exception)))
  `(dw	,excode)
  `(align	code_align)
  `(jmp	,restart))

;;; FSK: umm.  these probably can't work this way in Sassy

;;; begin_codevector name
;;; 	Define a code vector, just raw code
	
;;(define-sassy-macro (ia86.begin_codevector name)
;;  (section	.text
;;	align	code_align
;;,name:


;;; end_codevector name
;;;	Terminate a codevector started by begin_codevector

;;(define-sassy-macro (ia86.end_codevector name)
;;end_codevector_,name:


;;; alloc
;;;	Given fixnum number of words in RESULT, allocate
;;;	a structure of that many bytes and leave a raw pointer
;;;	in RESULT.

(define-sassy-instr (ia86.alloc)
  (cond ((inline-allocation)
         (let ((L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L1)
	   `(mov TEMP (& (+ GLOBALS G_ETOP)))
	   `(add TEMP RESULT) ; allocate
	   `(add TEMP 4)      ;  and
	   `(add TEMP -8)     ;   round up to 8-byte boundary
	   `(cmp TEMP CONT)
	   `(jle short ,L2)
	   (ia86.mcall $m.morecore)
	   `(jmp short ,L1)
           `(label ,L2)
           `(mov RESULT (& (+ GLOBALS G_ETOP)))
	   `(mov (& (+ GLOBALS G_ETOP)) TEMP)))
        (else
         (ia86.mcall $m.alloc))))

;;; const2reg hwreg const
;;; Move a constant to a register *without changing the flags*

(define-sassy-instr (ia86.const2reg hwreg const)
  `(mov ,hwreg ,const))         ; 5 bytes

;;; const2regf hwreg const
;;; Move a constant to a register, possibly killing the flags
;;; Makes for smaller code size.

(define-sassy-instr (ia86.const2regf hwreg const)
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

(define-sassy-instr (ia86.T_CONT)
  )
  
	
(define-sassy-instr (ia86.T_LABEL n)
  `(label ,n))

;;; CONST is broken up into two instructions depending on the
;;; type of the constant.  The argument to T_CONST_IMM is a bitpattern
;;; or symbolic constant name, to T_CONST_CONSTVECTOR it is the
;;; constant vector index.
	
(define-sassy-instr (ia86.T_CONST_IMM x)
  (ia86.const2regf RESULT x))

(define-sassy-instr (ia86.T_CONST_CONSTVECTOR x)
  (ia86.loadc	RESULT x))

(define-sassy-instr (ia86.T_CONST_SETREG_IMM x r)
  (cond ((is_hwreg r)
         `(mov ,(reg r) ,x))
        (else
         `(mov TEMP ,x)
	 (ia86.storer r TEMP))))

(define-sassy-instr (ia86.T_CONST_SETREG_CONSTVECTOR x r)
  (cond ((is_hwreg r)
         (ia86.loadc (reg r) x))
        (else
	 (ia86.loadc TEMP x)
	 (ia86.storer r TEMP))))

;;; OPTIMIZEME:	if #!undefined fit in a byte, then we could do a byte
;;; compare here, at least.  (Why does it not fit in a byte?)

(define-sassy-instr (ia86.T_GLOBAL x)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    `(label ,L0)
    (ia86.loadc TEMP x)
    `(mov RESULT (& TEMP ,(- PAIR_TAG)))
    (cond ((not (unsafe-globals))
           `(cmp RESULT UNDEFINED_CONST)
           `(jne short ,L1)
           `(mov RESULT TEMP)
           (ia86.mcall $m.global-ex)
           `(jmp short ,L0)
           `(label ,L1)))))

(define-sassy-instr (ia86.T_SETGLBL x)
  `(mov	SECOND RESULT)
  (ia86.loadc	RESULT x)
  `(mov	(& RESULT ,(- PAIR_TAG) SECOND))
  (ia86.write_barrier -1 -1))

(define-sassy-instr (ia86.T_LEXICAL rib off)
  (ia86.loadr	TEMP 0)		; We know R0 is not a HWREG
  `(times ,rib (mov TEMP (& TEMP ,(+ (- PROC_TAG) PROC_REG0))))
  `(mov RESULT (& TEMP ,(+ (- PROC_TAG) PROC_REG0 (words2bytes off)))))

(define-sassy-instr (ia86.T_SETLEX rib off)
  (ia86.loadr	TEMP 0)		; We know R0 is not a HWREG
  `(times ,rib (mov TEMP (& TEMP ,(+ (- PROC_TAG) PROC_REG0))))
  `(mov (& TEMP ,(+ (- PROC_TAG) PROC_REG0 (words2bytes off))) RESULT))
	
(define-sassy-instr (ia86.T_STACK slot)
  `(mov	RESULT (stkslot ,slot)))

(define-sassy-instr (ia86.T_SETSTK slot)
  `(mov	(stkslot ,slot) RESULT))

(define-sassy-instr (ia86.T_LOAD r slot)
  (cond ((is_hwreg r)
         `(mov ,(reg r) (stkslot ,slot)))
        (else
         `(mov TEMP (stkslot ,slot))
         (ia86.storer r TEMP))))

(define-sassy-instr (ia86.T_STORE r slot)
  (cond ((is_hwreg r)
         `(mov	(stkslot ,slot) ,(reg r)))
        (else
	 (ia8.loadr	TEMP r)
	 `(mov	(stkslot ,slot) TEMP))))

(define-sassy-instr (ia86.T_REG r)
  (ia86.loadr RESULT r))
	
;;; Does not destroy RESULT.  The peephole optimizer uses that fact.
(define-sassy-instr (ia86.T_SETREG r)
  (ia86.storer	r RESULT))

(define-sassy-instr (ia86.T_MOVEREG r1 r2)
  (cond ((is_hwreg r1)
	 (ia86.storer r2 (reg r1)))
	((is_hwreg r2)
	 (ia86.loadr (reg r2) r1))
	(else
         (ia86.loadr TEMP r1)
         (ia86.storer r2 TEMP))))

(define-sassy-instr (ia86.init_closure r)
  (let ((regno 'set-banged-below)
        (L1 (fresh-label)))
    (cond 
     ((> r *lastreg*)
      (begin 
        (set! regno (- *lastreg* 1))
        `((mov (& GLOBALS ,(+ G_STKP)) CONT)     ; Need a working register!
          (mov (& GLOBALS ,(+ G_RESULT)) RESULT) ; Save for later
          (add RESULT (+ PROC_REG0 (words2bytes LASTREG)))
          (loadr CONT 31)
          (label ,L1)
          (mov TEMP (& (- CONT PAIR_TAG)))
          (mov (& RESULT) TEMP)
          (add RESULT wordsize)
          (mov CONT (& CONT ,(+ (- PAIR_TAG) wordsize)))
          (cmp CONT NIL_CONST)
          (jne short ,L1)
          (mov CONT (& GLOBALS ,(+ G_STKP)))
          (mov RESULT (& GLOBALS ,(+ G_RESULT))))))
     (else
      (begin (set! regno r)
             '())))
    (begin
      (let rep ((regno regno))
        (cond 
         ((>= regno 0)
          (append
           (if (is_hwreg regno)
               `((mov (& RESULT ,(+ PROC_REG0 (words2bytes regno))) 
                      ,(reg regno)))
               `(,@(ia86.loadr TEMP regno)
                 (mov (& RESULT ,(+ PROC_REG0 (words2bytes regno))) TEMP)))
           (rep (- regno 1))))
         (else 
          '()))))
    `(add RESULT_LOW PROC_TAG)))

(define-sassy-instr (ia86.T_LAMBDA codevec constvec n)
  ;; arguments are codevector name, constant vector offset, and n
  (ia86.const2regf RESULT (fixnum (+ PROC_HEADER_WORDS PROC_OVERHEAD_WORDS n 1)))
  (ia86.alloc)
  `(mov (dword (& RESULT)) ,(logior
                             (lsh (words2bytes (+ PROC_OVERHEAD_WORDS n 1))
                                  8)
                             $hdr.procedure))
  ;; Adjust only if code is in bytevectors!
  ;;mov	dword [RESULT+PROC_CODEVECTOR_NATIVE], ,codevec + BVEC_TAG
  `(mov	(dword (& RESULT ,(+ PROC_CODEVECTOR_NATIVE))) ,codevec)
  (ia86.loadc	TEMP constvec)
  `(mov	(& RESULT ,(+ PROC_CONSTVECTOR)) TEMP)
  (ia86.init_closure n))

(define-sassy-instr (ia86.T_LEXES n)
  ;; argument is n
  (ia86.const2regf RESULT (fixnum (+ PROC_HEADER_WORDS PROC_OVERHEAD_WORDS n 1)))
  (ia86.alloc)
  `(mov	(dword (& RESULT)) ,(logior
                             (lsh (words2bytes (+ PROC_OVERHEAD_WORDS n 1))
                                  8)
                             $hdr.procedure))
  (ia86.loadr	TEMP 0)
  `(mov	TEMP (& TEMP ,(+ (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))
  `(mov	(dword (& RESULT ,(+ PROC_CODEVECTOR_NATIVE))) TEMP)
  (ia86.loadr	TEMP 0)
  `(mov	TEMP (& TEMP ,(+ (- PROC_TAG) PROC_CONSTVECTOR)))
  `(mov	(dword (& RESULT ,(+ PROC_CONSTVECTOR))) TEMP)
  (ia86.init_closure n))

(define-sassy-instr (ia86.T_ARGSEQ n)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(cmp	RESULT (fixnum ,n))
           `(je short ,L1)
           (ia86.mcall	$m.argc-ex)
           `(jmp	L0)
           `(label ,L1)))))


(define-sassy-instr (ia86.T_ARGSGE n)
  (ia86.const2regf SECOND (fixnum n))
  (cond ((and (not (unsafe-code))
              (> n 0))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(cmp RESULT SECOND)
           `(jge short ,L1)
           (ia86.mcall $m.argc-ex)
           `(jmp ,L0)
           `(label ,L1))))
  (ia86.mcall $m.varargs))

;;; Note the millicode for M_INVOKE_EX is used to check for timer
;;; exception as well, and must check the timer first.

(define-sassy-instr (ia86.T_INVOKE n)
  (cond ((unsafe-code)
         (ia86.timer_check)
         (ia86.storer 0 RESULT)
         `(mov TEMP RESULT)
         (ia86.const2regf RESULT (fixnum n))
         `(jmp (& TEMP ,(+ (- PROC_TAG) PROC_CODEVECTOR_NATIVE))))
        (else
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(dec (dword (& GLOBALS ,(+ G_TIMER))))
           `(jnz short ,L1)
           `(label ,L0)
	   (ia86.mcall $m.invoke-ex)
           `(label ,L1)
	   `(lea TEMP (& RESULT ,(+ 8 (- PROC_TAG))))
	   `(test TEMP_LOW tag_mask)
	   `(jnz short L0)
           ;; Observe TEMP points to proc+8 here and that if we were
           ;; really perverse we would lay the procedure out so that
           ;; the codevector is stored at offset 4, reducing the size
           ;; of the JMP instruction below.
	   (ia86.storer	0 RESULT)
	   (ia86.const2regf RESULT (fixnum n))
	   `(jmp	(& TEMP ,(+ -8 PROC_CODEVECTOR_NATIVE)))))))

;;; Introduced by peephole optimization.
;;; 
;;; The trick here is that the tag check for procedure-ness will
;;; catch undefined variables too.  So there is no need to see
;;; if the global has an undefined value, specifically.  The
;;; exception handler figures out the rest.

(define-sassy-instr (ia86.T_GLOBAL_INVOKE g n)
  (cond ((unsafe-globals)
         `(T_GLOBAL ,g)
         `(T_INVOKE ,n))
        (else
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           (ia86.loadc	RESULT g)		; global cell
           `(label ,L2)
	   `(mov TEMP                    ;   dereference
                 (& (- RESULT PAIR_TAG)))
	   `(inc TEMP)			; really TEMP += PROC_TAG-8
	   `(test TEMP_LOW tag_mask)	; tag test
	   `(jz short ,L0)
           `(label ,L1)
	   (ia86.mcall $m.global-invoke-ex) ; RESULT has global cell (always)
	   `(jmp short ,L2)		; Since TEMP is dead following timer interrupt
           `(label ,L0)
	   `(dec (dword                  ; timer
                  (& GLOBALS ,(+ G_TIMER))))
	   `(jz short ,L1)                ;   test
	   `(dec TEMP)                   ; undo ptr adjustment
	   (ia86.storer 0 TEMP)              ; save proc ptr
	   (ia86.const2regf RESULT           ; argument count
                            (fixnum n))
	   `(jmp	(& TEMP ,(+ (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))))))
	
;;; Allocate the frame but initialize only the basic slots
;;; and any pad words.  Leave RESULT clear at the end; this
;;; fact is used by T_SAVE1 below.

(define-sassy-instr (ia86.T_SAVE0 n)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    `(label ,L0)
    `(sub CONT (framesize ,n))
    `(cmp CONT (& GLOBALS ,(+ G_ETOP)))
    `(jge short ,L1)
    `(add CONT (framesize ,n))
    (ia86.mcall $m.stkoflow)
    `(jmp ,L0)
    `(label ,L1)
    `(mov (dword (& CONT)) (recordedsize ,n))
    ;; Not necessary to store reg0 here, this is handled
    ;; explicitly by the generated code.
    `(xor	RESULT RESULT)
    `(mov	(dword (& CONT ,(+ STK_RETADDR))) RESULT)
    (cond ((= (- (framesize n) (recordedsize n)) 8)
           ;; We have a pad word at the end -- clear it
           `(mov (dword (& (stkslot (+ ,n 1)))) RESULT)))))

;;; Initialize the numbered slot to the value of RESULT.
;;; Using RESULT is probably OK because it is almost certainly 0
;;; after executing T_SAVE0 and only T_STORE instructions
;;; after that.

(define-sassy-instr (ia86.T_SAVE1 n)
  `(mov	(dword (& (stkslot ,n))) RESULT))

;;; T_SAVE may still be emitted by the assembler when peephole 
;;; optimization is disabled.

(define-sassy-instr (ia86.T_SAVE n)
  `(T_SAVE0 ,n)
  (begin
    (let rep ((slotno 1))
      (cond ((<= slotno n)
             (cons `(T_SAVE1 ,slotno)
                   (rep (+ slotno 1))))
            (else 
             '())))))

(define-sassy-instr (ia86.T_SETRTN lbl)
  `(mov	(dword (& CONT ,(+ STK_RETADDR))) (t_label ,lbl)))

(define-sassy-instr (ia86.T_RESTORE n)
  (begin
    (let rep ((slotno 0))
      (cond ((<= slotno n)
             (append (if (is_hwreg slotno)
                         `((mov ,(reg slotno) (dword (& (stkslot ,slotno)))))
                         `((mov TEMP (dword (& (stkslot ,slotno))))
                           (mov (& GLOBALS ,(+ (g_reg slotno))) TEMP)))
                     (rep (+ slotno 1))))
            (else
             '())))))
  
(define-sassy-instr (ia86.T_POP n)
  `(add	CONT (framesize ,n)))
	
(define-sassy-instr (ia86.T_POPSTK)
  (error 'T_POPSTK "not implemented -- students only"))

(define-sassy-instr (ia86.T_RETURN)
  `(jmp (& CONT ,(+ STK_RETADDR))))
	
(define-sassy-instr (ia86.T_APPLY x y)
  (ia86.timer_check)
  (ia86.loadr	TEMP y)
  `(mov	(& GLOBALS ,(+ $g.third)) TEMP)
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.apply)
  (ia86.loadr	TEMP 0)
  `(mov	TEMP (& TEMP ,(+ (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))
  `(jmp	TEMP))

(define-sassy-instr (ia86.T_NOP)
  '()) ;; nothing

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; in this implementation)
	
(define-sassy-instr (ia86.T_JUMP levels label name)
  (ia86.timer_check)
  (cond ((> levels 0) 
         (ia86.loadr TEMP 0)		; We know R0 is not a HWREG
         `(times levels 
                 (mov TEMP (& TEMP ,(+ (- PROC_TAG) PROC_REG0))))
         (ia86.storer 0 TEMP)))
  `(jmp ,name))

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

(define-sassy-instr (ia86.T_SKIP lbl)
  `(jmp	(t_label ,lbl)))

(define-sassy-instr (ia86.T_SKIPF lbl)
  `(cmp	RESULT_LOW FALSE_CONST)
  `(je	(t_label ,lbl)))

(define-sassy-instr (ia86.T_BRANCH lbl)
  `(dec	(dword (& GLOBALS ,(+ G_TIMER))))
  `(jnz	(t_label ,lbl))
  (ia86.mcall	$m.timer-exception)
  `(jmp	(t_label ,lbl)))

(define-sassy-instr (ia86.T_BRANCHF lbl)
  (ia86.timer_check)
  `(cmp	RESULT_LOW FALSE_CONST)
  `(je	(t_label ,lbl)))

(define-sassy-instr (ia86.T_CHECK w x y z)
  `(cmp	RESULT_LOW FALSE_CONST)
  `(je	(t_label ,z)))

;; Call handler for noncontinuable exception with RESULT,
;; SECOND, and THIRD defined.

(define-sassy-instr (ia86.T_TRAP w x y z)
	;; Order matters here, because SECOND is TEMP and
	;; may be destroyed by loading of THIRD
  (cond ((not (= w 0))
         (ia86.loadr RESULT w)))
  (cond ((not (= y 0))
	   ;; OPTIMIZEME: optimize for case when %3 is HW reg
	   ;; (this will however have almost no impact)
         (ia86.loadr	TEMP y)
         `(mov (& GLOBALS ,(+ $g.third)) TEMP)))
  (cond ((not (= x 0))
         (ia86.loadr SECOND x)))
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
(define-sassy-instr (ia86.setcc cc)				; 10 bytes, jump
  (let ((L1 (fresh-label))
        (jcc (string->symbol (string-append "j" (symbol->string cc)))))
    (ia86.const2reg RESULT TRUE_CONST)	; 5 bytes
    `(,jcc short ,L1)			; 2 bytes
    `(sub	RESULT_LOW 4)		; 3 bytes (would be 2 if RESULT=eax)
    `(label ,L1)))

;;; double_tag_predicate ptrtag, hdr
;;;	Set RESULT to #t if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise set RESULT
;;;	to #f.

(define-sassy-instr (ia86.double_tag_predicate ptrtag hdr)
  (ia86.double_tag_test ptrtag hdr)
  (ia86.setcc 'z))

;;; fixnum_test_temp_is_free reg
;;;	Test reg for fixnum-ness and clear zero flag if fixnum.  OK to
;;;	destroy TEMP.
	
(define-sassy-instr (ia86.fixnum_test_temp_is_free reg)
  (cond ((is_hwreg reg) 
         (cond 
          ((hwreg_has_low reg)
           `(test	REG %+ ,reg %+ _LOW fixtag_mask))
          (else
           ;; test	REG,reg, fixtag_mask
           ;; Above is 6 bytes, below is 4 bytes.  Performance?
           `(mov	TEMP (REG,reg))
           `(test	TEMP_LOW fixtag_mask))))
        (else
         `(test	byte (& GLOBALS ,(+ (G_REG reg))) fixtag_mask))))

;;; single_tag_test ptrtag
;;;	Leave zero flag set if RESULT contains a value with the given
;;;     3-bit tag.

(define-sassy-instr (ia86.single_tag_test x)
  `(lea	TEMP (& RESULT ,(+ (- 8 x))))
  `(test	TEMP_LOW 7))
	
;;; single_tag_test_ex ptrtag, exception_code
;;;	Unless in unsafe mode, test the pointer in RESULT for the
;;;	tag and signal an exception if it does not match.

(define-sassy-instr (ia86.single_tag_test_ex x y)
  (cond 
   ((not (unsafe-code))
    (let ((L0 (fresh-label))
          (L1 (fresh-label)))
      `(label ,L0)
      (ia86.single_tag_test x)
      `(jz short ,L1)
      (ia86.exception_continuable y L0)
      `(label ,L1)))))

;;; double_tag_test ptrtag, hdr
;;;	Set the zero flag if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise reset the flag.
;;;     If zero flag is set, leaves the header field in TEMP.

(define-sassy-instr (ia86.double_tag_test x y)
  (let ((L1 (fresh-label)))
    (ia86.single_tag_test x)
    `(jnz short ,L1)
    `(mov	TEMP (& (- RESULT ,x)))
    `(cmp	TEMP_LOW ,y)
    `(label ,L1)))
	
;;; fixnum_arithmetic regno, operation, undo-operation, ex
	
(define-sassy-instr (ia86.fixnum_arithmetic x y z ex)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           `(jnz short ,L1)
           (ia86.loadr	TEMP x)
           `(,y	RESULT TEMP)
           `(jno short ,L2)
           `(,z	RESULT TEMP)
           `(label ,L1)
           (ia86.exception_continuable ex L0)	; second is tmp so 2nd arg is in place
           `(label ,L2)))
        ((is_hwreg x)
         ;; This looks totally bogus -- should use ,y? XXX TODO BUG HERE
         `(add	RESULT (REG,x)))
        (else
         `(add	RESULT (& GLOBALS ,(+ (G_REG x)))))))
	
;;; trusted_fixnum_compare r, cc
(define-sassy-instr (ia86.trusted_fixnum_compare x y)
  (cond ((is_hwreg x)
         `(cmp	RESULT (REG,x))
         `(cmp	RESULT (& GLOBALS ,(+ (G_REG x)))))
        (else
         (ia86.setcc	y))))

;;; fixnum_compare reg, cc, ex
;;; OPTIMIZEME for whenever ,x is a hwreg

(define-sassy-instr (ia86.fixnum_compare x y z)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.loadr	SECOND x)
           (ia86.exception_continuable z L0)         ; second is tmp so 2nd arg is in place
           `(label ,L1 )))
        ((is_hwreg x)
         `(cmp	RESULT (REG,x)))
        (else
         `(cmp	RESULT (& GLOBALS ,(+ (G_REG x))))))
  (ia86.setcc y))

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
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           (ia86.loadr	SECOND x)
           `(jz short ,L2)
           `(label ,L1)
           (ia86.exception_continuable z L0)
           `(label ,L2)
           `(cmp	TEMP fixnum(32))	; SECOND is TEMP
           `(jge short ,L1)))
        (else
         (ia86.loadr	TEMP x)))
  `(shr	TEMP 2)
  `(mov	(& GLOBALS ,(+ G_REGALIAS_ECX)) ecx)
  `(mov	cl al)			; Code knows TEMP is EAX
  `(,y	RESULT cl)
  `(mov	ecx (& GLOBALS ,(+ G_REGALIAS_ECX)))
  (cond ((not (eq? y 'shl))
         ;; Right shifts: mask out low bits
         ;; OPTIMIZEME: if RESULT were eax, masking RESULT_LOW with a byte
         ;; would save one byte here.
         `(and	RESULT ~fixtag_mask))))
	
;;; generic_arithmetic regno, operation, undo-operation, millicode
	
(define-sassy-instr (ia86.generic_arithmetic x y z millicode)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (ia86.loadr	TEMP x)
    `(or	TEMP RESULT)
    `(test	TEMP_LOW fixtag_mask)
    (ia86.loadr	TEMP x)
    `(jnz short ,L1)
    `(,y	RESULT TEMP)
    `(jno short ,L2)
    `(,z	RESULT TEMP)
    `(label ,L1)
    (ia86.mcall	millicode)		; second is temp so 2nd arg is in place
    `(label ,L2 )))

;;; generic_compare reg, condition, millicode

(define-sassy-instr (ia86.generic_compare x y z)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (ia86.loadr	TEMP x)
    `(or	TEMP RESULT)
    `(test	TEMP_LOW fixtag_mask)
    (ia86.loadr	SECOND x)
    `(jz short ,L1)
    (ia86.mcall	z)
    `(jmp short L2	)
    `(label ,L1)
    `(cmp	RESULT SECOND)
    (ia86.setcc	y)
    `(label ,L2 )))

;;; generic_char_compare reg, cc, ex

(define-sassy-instr (ia86.generic_char_compare x y z)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	SECOND x)
           `(cmp	SECOND_LOW ,$imm.character)
           `(jz	,L2)
           `(label ,L1)
           (ia86.exception_continuable z L0)
           `(label ,L2)
           `(cmp	RESULT_LOW ,$imm.character)
           `(jne short ,L1)
           `(cmp	RESULT SECOND)))
        ((is_hwreg x)
         `(cmp	RESULT (REG,x)))
        (else
         `(cmp	RESULT (& GLOBALS ,(+ (G_REG x))))))
  (ia86.setcc	y))
	
;;; generic_imm_compare imm, cc, millicode

(define-sassy-instr (ia86.generic_imm_compare x y z)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	RESULT_LOW fixtag_mask)
    `(jz short ,L1)
    (ia86.const2regf SECOND x)
    (ia86.mcall	z)
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp	RESULT ,x)
    (ia86.setcc	y)
    `(label ,L2)))
	
;;; generic_char_imm_compare imm, cc, ex
	
(define-sassy-instr (ia86.generic_char_imm_compare x y z)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           `(cmp	RESULT_LOW ,$imm.character)
           `(jz	,L1)
           `(const2regf SECOND ,x)
           `(exception_continuable ,z L0)))
    `(label ,L1)
    `(cmp	RESULT ,x)
    (ia86.setcc	y)))

;;; indexed_structure_length ptrtag, hdrtag, ex, byte?

(define-sassy-instr (ia86.indexed_structure_length/hdr ptrtag hdrtag ex byte?)		; string-length or bytevector-length
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.double_tag_test ptrtag hdrtag)
           `(jz short ,L1)
           (ia86.exception_continuable ex L0)
           `(label ,L1)
           `(mov	RESULT TEMP)))
        (else
         `(mov	RESULT (& (- RESULT ,ptrtag)))))
  `(shr	RESULT 8)
  (cond (byte?
         `(shl	RESULT 2))))


;;; indexed_structure_length ptrtag, ex, byte?
	
(define-sassy-instr (ia86.indexed_structure_length ptrtag ex byte?)
  (ia86.single_tag_test_ex ptrtag ex)
  `(mov	RESULT (& (- RESULT ,ptrtag)))
  `(shr	RESULT 8)
  (cond (byte?
         `(shl	RESULT 2))))


;;; indexed_structure_test reg_index, reg_value, ptrtag, hdrtag, ex, byte?, test_reg_value
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that reg_index is a fixnum in the range of the structure.
;;;	If hdrtag is zero then do not check it.

(define-sassy-instr (ia86.indexed_structure_test x y z hdrtag ex byte? test_reg_value)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label))
               (L3 (fresh-label)))
           `(label ,L0)
           (ia86.fixnum_test_temp_is_free x)
           `(jnz short ,L1)
           (cond (hdrtag
                  `(double_tag_test ,z ,hdrtag)
                  `(jz short L2))
                 (else
                  `(single_tag_test ,z)
                  `(jz short L3)))
           `(label ,L1)
           (ia86.loadr	SECOND x)
           (ia86.exception_continuable ex L0)
           (cond ((= hdrtag 0)
                  `(label ,L3)
                  `(mov	TEMP (& (- RESULT ,z)))))
           `(label ,L2)
           `(shr	TEMP 8)
           (cond (byte?
                  `(shl	TEMP 2)))	; Length is now a fixnum
           (cond ((is_hwreg x)
                  `(cmp	TEMP (REG,x)))
                 (else
                  `(cmp	TEMP (& GLOBALS ,(+ (G_REG x))))))
           `(jbe short ,L1)
           (ia86.test_reg_value	y L1)))))

;;; indexed_structure_test_imm index, ptrtag, hdrtag, ex, byte?
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that index (a fixnum) is in the range of the structure.
;;;	If hdrtag is zero then do not check it.
	
(define-sassy-instr (ia86.indexed_structure_test_imm x y z ex byte?)
  (cond 
   ((not (unsafe-code))
    (let ((L0 (fresh-label))
          (L1 (fresh-label))
          (L2 (fresh-label))
          (L3 (fresh-label)))
      `(label ,L0)
      (cond ((not (= z 0))
             (ia86.double_tag_test y z)
             `(jz short ,L2))
            (else
             (ia86.single_tag_test y)
             `(jz short ,L3)))
      `(label ,L1)
      `(mov	SECOND ,x)
      (ia86.exception_continuable ex L0)
      (cond ((= z 0)
             `(label ,L3)
             `(mov	TEMP (& (- RESULT ,y)))))
      `(label ,L2)
      `(shr	TEMP 8)
      (cond (byte?
             `(shl	TEMP 2)))		; Length is now a fixnum
      `(cmp	TEMP ,x)
      `(jbe short ,L1)))))

;;; load_from_indexed_structure index_reg, ptrtag, byte?
	
(define-sassy-instr (ia86.load_from_indexed_structure x y z)
  (cond (z
         (ia86.loadr	TEMP x)
         `(shr	TEMP 2)
         `(mov	RESULT_LOW (& RESULT ,(+ (- y) wordsize TEMP)))
         `(and	RESULT #xFF))
        ((is_hwreg x)
         `(mov	RESULT (& RESULT ,(+ (- y) wordsize (REG x)))))
        (else
         (ia86.loadr	TEMP x)
         `(mov	RESULT (& RESULT ,(+ (- y) wordsize TEMP))))))

(define-sassy-instr (ia86.load_from_indexed_structure_imm x y z)
  (cond (z
         `(mov	RESULT_LOW (& RESULT ,(+ (- y) wordsize (quotient x 4))))
         `(and	RESULT #xFF))
        (else
         `(mov	RESULT (& RESULT ,(+ (- y) wordsize x))))))
				
;;; indexed_structure_ref reg, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-instr (ia86.indexed_structure_ref/hdr reg ptrtag hdrtag ex byte?)
  (ia86.indexed_structure_test  reg 0  ptrtag  hdrtag  ex  byte? check_nothing)
  (ia86.load_from_indexed_structure  reg  ptrtag  byte?))

;;; indexed_structure_ref_imm idx, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-instr (ia86.indexed_structure_ref_imm/hdr idx ptrtag hdrtag ex byte?)
  (ia86.indexed_structure_test_imm  idx  ptrtag  hdrtag  ex  byte?)
  (ia86.load_from_indexed_structure_imm  idx  ptrtag  byte?))
				
;;; indexed_structure_ref reg, ptrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-instr (ia86.indexed_structure_ref reg ptrtag ex byte?)
  (ia86.indexed_structure_test  reg 0  ptrtag 0  ex  byte? check_nothing)
  (ia86.load_from_indexed_structure  reg  ptrtag  byte?))

(define-sassy-instr (ia86.indexed_structure_ref_imm reg ptrtag ex chk)
  (ia86.indexed_structure_test_imm  reg  ptrtag 0  ex  chk)
  (ia86.load_from_indexed_structure_imm  reg  ptrtag  chk))

;;; check_nothing regno, label
;;;	Just a placeholder.

(define-sassy-instr (ia86.check_nothing x y)
  )
	
;;; check_fixnum regno, label
;;;	Branch to label if regno does not hold a fixnum

(define-sassy-instr (ia86.check_fixnum x y)
  (ia86.fixnum_test_temp_is_free x)
  `(jnz short ,y))
	
;;; check_char regno, label
;;;	Branch to label if regno does not hold a char.
;;;	Leaves char in TEMP (even if it is in hwreg, the value must
;;;	be shifted anyway).

(define-sassy-instr (ia86.check_char x y)
  (ia86.loadr	TEMP x)
  `(cmp	TEMP_LOW ,$imm.character)
  `(jnz short ,y))

;;; indexed_structure_set_* reg_idx, reg_value, ptrtag, hdrtag, ex
;;;	If hdrtag is 0 then do not check it.

(define-sassy-instr (ia86.indexed_structure_set_char x y z hdrtag ex)
  (ia86.indexed_structure_test x y z hdrtag ex 1 check_char)
  `(mov	(& GLOBALS ,(+ G_STKP)) CONT)
  (ia86.loadr	CONT x)
  `(shr	TEMP 16)
  `(shr	CONT 2)
  `(mov	(& RESULT ,(+ (- z) wordsize CONT)) TEMP_LOW)
  `(mov	CONT (& GLOBALS ,(+ G_STKP))))

(define-sassy-instr (ia86.indexed_structure_set_byte x y z hdrtag ex)
  (ia86.indexed_structure_test x y z hdrbyte ex 1 check_fixnum)
  `(mov	(& GLOBALS ,(+ G_STKP)) CONT)
  (ia86.loadr	CONT x)
  `(shr	CONT 2)
  (ia86.loadr	TEMP y)
  `(shr	TEMP 2)
  `(mov	(& RESULT ,(+ (- z) wordsize CONT)) TEMP_LOW)
  `(mov	CONT (& GLOBALS ,(+ G_STKP))))

(define-sassy-instr (ia86.indexed_structure_set_word x y z hdrtag ex)
  (ia86.indexed_structure_test x y z hdrtag ex 0 check_nothing)
  (ia86.do_indexed_structure_set_word x y z))

(define-sassy-instr (ia86.do_indexed_structure_set_word x y z)
  (cond ((and (is_hwreg y) (is_hwreg x))
         `(mov	(& RESULT ,(+ (- z) wordsize (REG x))) (REG y))
         (ia86.write_barrier -1 y))
        ((is_hwreg y)
         (ia86.loadr	TEMP x)
         `(mov	(& RESULT ,(+ (- z) wordsize TEMP)) (REG y))
         (ia86.write_barrier -1 y))
        ((is_hwreg x)
         (ia86.loadr	SECOND y)
         `(mov	(& RESULT ,(+ (- z) wordsize (REG x))) SECOND)
         (ia86.write_barrier -1 -1))
        (else
         `(mov	(& GLOBALS ,(+ G_STKP)) CONT)
         (ia86.loadr	CONT x)
         (ia86.loadr	SECOND y)
         `(mov	(& RESULT ,(+ (- z) wordsize CONT)) SECOND)
         `(mov	CONT (& GLOBALS ,(+ G_STKP)))
         (ia86.write_barrier -1 -1))))

;;; make_indexed_structure_word regno ptrtag hdrtag ex
;;;	Allocate a word structure with the length specified in RESULT
;;;	(fixnum number of entries).  If ,x is not -1, then initialize 
;;;	it with the contents of (REG,x), otherwise with #!unspecified.
	
(define-sassy-instr (ia86.make_indexed_structure_word x y z ex)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	RESULT (logior fixtag_mask #x80000000))
           `(jz short ,L1)
           (ia86.exception_continuable ex L0)
           `(label ,L1))))
  `(mov	(& GLOBALS ,(+ G_ALLOCTMP)) RESULT)
  `(add	RESULT wordsize)
  (cond ((= x -1)
         `(mov	SECOND UNSPECIFIED_CONST))
        (else
         (ia86.loadr	SECOND x)))
  (ia86.mcall	$m.alloci)
  `(mov	TEMP (& GLOBALS ,(+ G_ALLOCTMP)))
  `(shl	TEMP 8)
  `(or	TEMP ,z)
  `(mov	(& RESULT) ,TEMP)
  `(add	RESULT ,y))

;;; make_indexed_structure_byte regno hdrtag ex
;;;	Allocate a byte structure with the length specified in RESULT
;;;     (fixnum number of bytes).  If ,x is not -1, then (REG,x) must
;;;     hold a char value to be used for initialization (a check is
;;;     performed that is a char).  If ,x is -1, no initialization 
;;; 	is performed.

(define-sassy-instr (ia86.make_indexed_structure_byte x y z)
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
           (cond ((not (= (x -1)))
                  (ia86.loadr	SECOND x)))
           `(test	RESULT (logior fixtag_mask #x80000000))
           `(jz short L2)
           `(label ,L1)
           (ia86.exception_continuable z L0)
           `(label ,L2 )
           (cond ((not (= x -1))
                  `(cmp	SECOND_LOW ,$imm.character)
                  `(jne	L1)))))
    `(mov	(& GLOBALS ,(+ G_ALLOCTMP)) RESULT)
    `(add	RESULT fixnum(wordsize))
    (ia86.mcall	$m.alloc-bv)
    (cond ((not (= x -1))
           (ia86.loadr	eax x)		; Code knows that eax is TEMP/SECOND
           `(mov	(& GLOBALS ,(+ G_REGALIAS_ECX)) ecx)
           `(mov	(& GLOBALS ,(+ G_REGALIAS_EDI)) edi)
           `(shr	eax char_shift)	; byte value
           `(mov	ecx (& GLOBALS ,(+ G_ALLOCTMP)))
           `(shr	ecx 2)		; byte count
           `(lea	edi (& RESULT ,(+ 4)))	; destination ptr
           `(cld)
           `(rep stosb)		; byte fill
           `(mov	ecx (& GLOBALS ,(+ G_REGALIAS_ECX)))
           `(mov	edi (& GLOBALS ,(+ G_REGALIAS_EDI)))))
    `(mov	TEMP (& GLOBALS ,(+ G_ALLOCTMP)))
    `(shl	TEMP 6)
    `(or	TEMP ,y)
    `(mov	(& RESULT)  ,TEMP)
    `(add	RESULT BVEC_TAG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Primitive operations
;;; 
;;; The names 'n' in T_OP1_n are the primcodes from the table in
;;; Compiler/standard-C.imp.sch (yes, that's right).

(define-sassy-instr (ia86.T_OP1 x)   ;; YUCK eval!
  ((eval (string->symbol (string-append "ia86.T_OP1_" (number->string x))))))
(define-sassy-instr (ia86.T_OP2 x y) ;; YUCK eval!
  ((eval (string->symbol (string-append "ia86.T_OP2_" (number->string x))))
   y))
(define-sassy-instr (ia86.T_OP2IMM x y) ;; YUCK eval!
  ((eval (string->symbol (string-append "ia86.T_OP2IMM_" (number->string x))))
   y))

(define-sassy-instr (ia86.T_OP1_1)		; break
  (ia86.mcall	$m.break))

(define-sassy-instr (ia86.T_OP1_3)		; unspecified
  (ia86.const2regf RESULT UNSPECIFIED_CONST))

(define-sassy-instr (ia86.T_OP1_4)		; undefined
  (ia86.const2regf RESULT UNDEFINED_CONST))

(define-sassy-instr (ia86.T_OP1_5)		; eof-object
  (ia86.const2regf RESULT EOF_CONST))

(define-sassy-instr (ia86.T_OP1_6)		; enable-interrupts
  (ia86.mcall	$m.enable-interrupts))

(define-sassy-instr (ia86.T_OP1_7)		; disable-interrupts
  (ia86.mcall	$m.disable-interrupts))

(define-sassy-instr (ia86.T_OP1_8)		; typetag
  (ia86.mcall	$m.typetag))

(define-sassy-instr (ia86.T_OP1_9)		; not
  `(cmp	RESULT_LOW FALSE_CONST)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_10)		; null?
  `(cmp	RESULT_LOW NIL_CONST)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_11)		; pair?
  (ia86.single_tag_test PAIR_TAG)
  (ia86.setcc	'z))
	
(define-sassy-instr (ia86.T_OP1_12)		; eof-object?
  `(cmp	RESULT EOF_CONST)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_13)		; port?
  (ia86.double_tag_predicate VEC_TAG $hdr.port))

(define-sassy-instr (ia86.T_OP1_14)		; structure?
  (ia86.double_tag_predicate VEC_TAG $hdr.struct))

(define-sassy-instr (ia86.T_OP1_15)		; car
  (ia86.single_tag_test_ex PAIR_TAG $ex.car)
  `(mov	RESULT (& (- RESULT PAIR_TAG))))

(define-sassy-instr (ia86.T_OP1_16)		; cdr
  (ia86.single_tag_test_ex PAIR_TAG $ex.cdr)
  `(mov	RESULT (& RESULT ,(+ (- PAIR_TAG) wordsize))))

(define-sassy-instr (ia86.T_OP1_17)		; symbol?
  (ia86.double_tag_predicate VEC_TAG $hdr.symbol))

(define-sassy-instr (ia86.T_OP1_18)		; number? and complex?
  (ia86.mcall	$m.complexp))

(define-sassy-instr (ia86.T_OP1_20)		; real? and rational?
  (ia86.mcall	$m.rationalp))

(define-sassy-instr (ia86.T_OP1_21)		; compnum?
  (ia86.double_tag_predicate BVEC_TAG $hdr.compnum))

(define-sassy-instr (ia86.T_OP1_22)		; integer?
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	RESULT_LOW fixtag_mask)
    `(je	,L1)
    (ia86.mcall	$m.integerp)
    `(jmp short ,L2)
    `(label ,L1)
    (ia86.const2regf RESULT TRUE_CONST)
    `(label ,L2 )))

(define-sassy-instr (ia86.T_OP1_23)		; fixnum?
  `(test	RESULT_LOW fixtag_mask)
  (ia86.setcc	'z))
	
(define-sassy-instr (ia86.T_OP1_24)		; flonum?
  (ia86.double_tag_predicate BVEC_TAG $hdr.flonum))

(define-sassy-instr (ia86.T_OP1_25)		; exact?
  (ia86.mcall	$m.exactp))

(define-sassy-instr (ia86.T_OP1_26)		; inexact?
  (ia86.mcall	$m.inexactp))

(define-sassy-instr (ia86.T_OP1_27)		; exact->inexact
  (ia86.mcall	$m.exact->inexact))

(define-sassy-instr (ia86.T_OP1_28)		; inexact->exact
  (ia86.mcall	$m.inexact->exact))

(define-sassy-instr (ia86.T_OP1_29)		; round
  (ia86.mcall	$m.round))

(define-sassy-instr (ia86.T_OP1_30)		; truncate
  (ia86.mcall	$m.truncate))

(define-sassy-instr (ia86.T_OP1_31)		; zero?
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	RESULT_LOW fixtag_mask)
    `(jz short ,L1)
    (ia86.mcall	$m.zerop)
    `(jmp short ,L2)
    `(label ,L1)
    `(and	RESULT RESULT)
    (ia86.setcc	'z)
    `(label ,L2)))

(define-sassy-instr (ia86.T_OP1_32)		; --
  (ia86.mcall	$m.negate))

(define-sassy-instr (ia86.T_OP1_33)		; lognot
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           `(test	RESULT_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.lognot L0)
           `(label ,L1)))
    `(lea	RESULT (& RESULT ,(+ fixtag_mask)))
    `(not	RESULT)))

(define-sassy-instr (ia86.T_OP1_34)		; real-part
  (ia86.mcall	$m.real-part))
	
(define-sassy-instr (ia86.T_OP1_35)		; imag-part
  (ia86.mcall	$m.imag-part))

(define-sassy-instr (ia86.T_OP1_36)		; char?
  `(cmp	RESULT_LOW ,$imm.character)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_37)		; char->integer
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           `(cmp	RESULT_LOW ,$imm.character)
           `(jz	,L1)
           (ia86.exception_continuable $ex.char2int L0)
           `(label ,L1)))
    `(shr	RESULT 14)))

(define-sassy-instr (ia86.T_OP1_38)		; integer->char
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	RESULT_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.int2char L0))
         `(label ,L1)))
  `(and	RESULT 1023)
  `(shl	RESULT 14)
  `(or	RESULT_LOW ,$imm.character))

(define-sassy-instr (ia86.T_OP1_39)		; string?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.string))

(define-sassy-instr (ia86.T_OP1_40)		; string-length
  (ia86.indexed_structure_length/hdr $tag.bytevector-tag $hdr.string $ex.slen 1))
		
(define-sassy-instr (ia86.T_OP1_41)		; vector?
  (ia86.double_tag_predicate VEC_TAG $hdr.vector))


(define-sassy-instr (ia86.T_OP1_42)		; vector-length
  (ia86.indexed_structure_length/hdr VEC_TAG $hdr.vector $ex.vlen 0))
		
(define-sassy-instr (ia86.T_OP1_43)		; bytevector?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.bytevector))

(define-sassy-instr (ia86.T_OP1_44)		; bytevector-length
  (ia86.indexed_structure_length/hdr $tag.bytevector-tag $hdr.bytevector $ex.bvlen 1))

(define-sassy-instr (ia86.T_OP2_45 x)		; bytevector-fill!
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.single_tag_test $tag.bytevector-tag)
           `(jz short ,L2)
           `(label ,L1)
           (ia86.exception_continuable $ex.bvfill L0)
           `(label ,L2)
           (ia86.loadr	SECOND x)
           `(test	SECOND_LOW fixtag_mask)
           `(jnz short ,L1)))
        (else
         (ia86.loadr	SECOND x)))
  (ia86.mcall	$m.bytevector-like-fill))

(define-sassy-instr (ia86.T_OP1_46)		; make-bytevector
  (ia86.make_indexed_structure_byte -1 $hdr.bytevector  $ex.mkbvl))

(define-sassy-instr (ia86.T_OP1_47)		; procedure?
  (ia86.single_tag_test PROC_TAG)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_48)		; procedure-length
  (ia86.indexed_structure_length PROC_TAG $ex.plen  0))

(define-sassy-instr (ia86.T_OP1_49)		; make-procedure
  ;; exception code wrong, matches Sparc
  (ia86.make_indexed_structure_word -1 PROC_TAG  $hdr.procedure  $ex.mkvl))
		
(define-sassy-instr (ia86.T_OP1_52)		; make-cell just maps to cons, for now
  (T_OP2_58 1)		; OPTIMIZEME: remove next instr by specializing
  `(mov	(& RESULT ,(+ 4-PAIR_TAG)) dword UNSPECIFIED_CONST))

(define-sassy-instr (ia86.T_OP1_54)		; cell-ref
  `(mov	RESULT (& (- RESULT PAIR_TAG))))

(define-sassy-instr (ia86.T_OP2_55 x)		; typetag-set!
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.typetag-set))

(define-sassy-instr (ia86.T_OP2_56 x)		; eq?
  (cond ((is_hwreg x)
         `(cmp	RESULT ,(REG x)))
        (else
         `(cmp	RESULT (& GLOBALS ,(+ (G_REG x))))))
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP2_57 x)		; eqv?
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.eqv))
				
(define-sassy-instr (ia86.T_OP2_58 x)		; cons
  (cond ((inline-allocation)
         (let ((L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L1)
           `(mov	TEMP (& GLOBALS ,(+ G_ETOP)))
           `(add	TEMP 8)
           `(cmp	TEMP CONT)
           `(jle short ,L2)
           (ia86.mcall	$m.morecore)
           `(jmp short ,L1)
           `(label ,L2)
           `(mov	(& GLOBALS ,(+ G_ETOP)) TEMP)
           `(mov	(& TEMP-8)  ,RESULT)
           `(lea	RESULT (& TEMP-8 ,(+ PAIR_TAG)))
           (cond ((is_hwreg x)
                  `(mov	(& RESULT ,(+ (- PAIR_TAG) 4)) ,(REG x)))
                 (else
                  (ia86.loadr	TEMP x)
                  `(mov	(& RESULT ,(+ (- PAIR_TAG) 4)) TEMP)))))
        (else
         `(mov	(& GLOBALS ,(+ G_ALLOCTMP)) RESULT)
         `(mov	RESULT 8)
         (ia86.mcall	$m.alloc)
         `(mov	TEMP (& GLOBALS ,(+ G_ALLOCTMP)))
         `(mov	(& RESULT)  ,TEMP)
         (cond ((is_hwreg x)
                `(mov	(& RESULT ,(+ 4)) ,(REG x)))
               (else
                (ia86.loadr	TEMP x)
                `(mov	(& RESULT ,(+ 4)) TEMP)))
        `(add	RESULT PAIR_TAG))))
	
(define-sassy-instr (ia86.T_OP2_59 x)		; set-car!
  (ia86.single_tag_test_ex PAIR_TAG $ex.setcar)
  (cond ((is_hwreg x)
         `(mov	(& (- RESULT PAIR_TAG)) ,(REG x))
         (ia86.write_barrier -1 x))
        (else
         (ia86.loadr	SECOND x)
         `(mov	(& (- RESULT PAIR_TAG)) SECOND)
         (ia86.write_barrier -1 -1))))

(define-sassy-instr (ia86.T_OP2_60 x)		; set-cdr!
  (ia86.single_tag_test_ex PAIR_TAG $ex.setcdr)
  (cond ((is_hwreg x)
         `(mov	(& RESULT ,(+ (- PAIR_TAG) wordsize)) ,(REG x))
         (ia86.write_barrier -1 x))
        (else
         (ia86.loadr	SECOND x)
         `(mov	(& RESULT ,(+ (- PAIR_TAG) wordsize)) SECOND)
         (ia86.write_barrier -1 -1))))

(define-sassy-instr (ia86.T_OP2_61 x)		; +
  (ia86.generic_arithmetic x add  sub  $m.add))

(define-sassy-instr (ia86.T_OP2_62 x)		; -
  (ia86.generic_arithmetic x sub  add  $m.subtract))

(define-sassy-instr (ia86.T_OP2_63 x)		; *
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.multiply))
	
(define-sassy-instr (ia86.T_OP2_64 x)		; /
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.divide))

(define-sassy-instr (ia86.T_OP2_65 x)		; quotient
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.quotient))

(define-sassy-instr (ia86.T_OP2_66 x)		; <
  (ia86.generic_compare x 'l  $m.numlt))
	
(define-sassy-instr (ia86.T_OP2_67 x)		; <=
  (ia86.generic_compare x 'le  $m.numle))

(define-sassy-instr (ia86.T_OP2_68 x)		; =
  (ia86.generic_compare x 'e  $m.numeq))

(define-sassy-instr (ia86.T_OP2_69 x)		; >
  (ia86.generic_compare x 'g  $m.numgt))

(define-sassy-instr (ia86.T_OP2_70 x)		; >=
  (ia86.generic_compare x 'ge $m.numge))

(define-sassy-instr (ia86.T_OP2_71 x)		; logand
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           (ia86.loadr	SECOND x)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.logand L0)
           `(label ,L1)
           `(and	RESULT SECOND)))
        ((is_hwreg x)
         `(and	RESULT ,(REG x)))
        (else
         `(and	RESULT (& GLOBALS ,(+ (G_REG x)))))))

(define-sassy-instr (ia86.T_OP2_72 x)		; logior
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.loadr	SECOND x)
           (ia86.exception_continuable $ex.logior L0)
           `(label ,L1)
           `(mov	RESULT TEMP)))
        ((is_hwreg x)
         `(or	RESULT ,(REG x)))
        (else
         `(or	RESULT (& GLOBALS ,(+ (G_REG x)))))))

(define-sassy-instr (ia86.T_OP2_73 x)		; logxor
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           (ia86.loadr	SECOND x)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.logxor L0)
           `(label ,L1)	
           `(xor	RESULT SECOND)))
        ((is_hwreg x)
         `(xor	RESULT ,(REG x)))
        (else
         `(xor	RESULT (& GLOBALS ,(+ (G_REG x)))))))

(define-sassy-instr (ia86.T_OP2_74 x)		; lsh
  (ia86.fixnum_shift x shl  $ex.lsh))
	
(define-sassy-instr (ia86.T_OP2_75 x)		; rsha
  (ia86.fixnum_shift x sar  $ex.rsha))

(define-sassy-instr (ia86.T_OP2_76 x)		; rshl
  (ia86.fixnum_shift x shr  $ex.rshl))
	
(define-sassy-instr (ia86.T_OP2_77 x)		; rot
  (error 'T_OP2_rot "not implemented"))

(define-sassy-instr (ia86.T_OP2_78 x)		; string-ref
  (ia86.indexed_structure_ref/hdr x $tag.bytevector-tag  $hdr.string  $ex.sref 1)
  `(shl	RESULT char_shift)
  `(or	RESULT_LOW ,$imm.character))

(define-sassy-instr (ia86.T_OP3_79 x y)		; string-set!
  (ia86.indexed_structure_set_char x y  $tag.bytevector-tag  $hdr.string  $ex.sset))

(define-sassy-instr (ia86.T_OP2_80 x)		; make-vector
  (ia86.make_indexed_structure_word x VEC_TAG  $hdr.vector  $ex.mkvl))

(define-sassy-instr (ia86.T_OP2_81 x)		; vector-ref
  (ia86.indexed_structure_ref/hdr x VEC_TAG  $hdr.vector  $ex.vref 0))

(define-sassy-instr (ia86.T_OP2_82 x)		; bytevector-ref
  (ia86.indexed_structure_ref/hdr x $tag.bytevector-tag  $hdr.bytevector  $ex.bvref 1)
  `(shl	RESULT 2))

(define-sassy-instr (ia86.T_OP2_83 x)		; procedure-ref
  (ia86.indexed_structure_ref x PROC_TAG  $ex.pref 0))

(define-sassy-instr (ia86.T_OP2_84 x)		; cell-set!
  (cond ((is_hwreg x)
         `(mov	(& RESULT-PAIR_TAG) ,(REG x))
         (ia86.write_barrier -1 x))
        (else
         (ia86.loadr	SECOND x)
         `(mov	(& RESULT-PAIR_TAG) ,SECOND)
         (ia86.write_barrier -1 -1))))

(define-sassy-instr (ia86.T_OP2_85 x)		; char<?
  (ia86.generic_char_compare x l  $ex.char<?))

(define-sassy-instr (ia86.T_OP2_86 x)		; char<=?
  (ia86.generic_char_compare x le  $ex.char<=?))

(define-sassy-instr (ia86.T_OP2_87 x)		; char=?
  (ia86.generic_char_compare x e  $ex.char=?))

(define-sassy-instr (ia86.T_OP2_88 x)		; char>?
  (ia86.generic_char_compare x g  $ex.char>?))

(define-sassy-instr (ia86.T_OP2_89 x)		; char>=?
  (ia86.generic_char_compare x ge  $ex.char>=?))

(define-sassy-instr (ia86.T_OP2_90 x)		; sys$partial-list->vector
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.partial-list->vector))

(define-sassy-instr (ia86.T_OP3_91 x y)		; vector-set!
  (ia86.indexed_structure_set_word x y  VEC_TAG  $hdr.vector  $ex.vset))

(define-sassy-instr (ia86.T_OP3_92 x y)		; bytevector-set!
  (ia86.indexed_structure_set_byte x y  $tag.bytevector-tag  $hdr.bytevector  $ex.bvset))

(define-sassy-instr (ia86.T_OP3_93 x y)		; procedure-set!
  (ia86.indexed_structure_set_word x y  PROC_TAG  0  $ex.pset))

(define-sassy-instr (ia86.T_OP1_94)		; bytevector-like?
  (ia86.single_tag_test $tag.bytevector-tag)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_95)		; vector-like?
  (ia86.single_tag_test VEC_TAG)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP2_96 x)		; bytevector-like-ref
  (ia86.indexed_structure_ref x $tag.bytevector-tag  $ex.bvlref 1)
  `(shl	RESULT 2))

(define-sassy-instr (ia86.T_OP3_97 x y)		; bytevector-like-set!
  (ia86.indexed_structure_set_byte x y  $tag.bytevector-tag  0  $ex.bvlset))

(define-sassy-instr (ia86.T_OP2_98 x)		; sys$bvlcmp
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.bvlcmp))

(define-sassy-instr (ia86.T_OP2_99 x)		; vector-like-ref
  (ia86.indexed_structure_ref x VEC_TAG  $ex.vlref  0))

(define-sassy-instr (ia86.T_OP3_100 x y)		; vector-like-set!
  (ia86.indexed_structure_set_word x y  VEC_TAG  0  $ex.vlset))

(define-sassy-instr (ia86.T_OP1_101)		; vector-like-length
  (ia86.indexed_structure_length VEC_TAG $ex.vllen 0))

(define-sassy-instr (ia86.T_OP1_102)		; bytevector-like-length
  (ia86.indexed_structure_length $tag.bytevector-tag $ex.bvllen 1))

(define-sassy-instr (ia86.T_OP2_103 x)		; remainder
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.remainder))

(define-sassy-instr (ia86.T_OP1_104)		; petit-patch-boot-code
  (ia86.mcall	$m.petit-patch-boot-code))

(define-sassy-instr (ia86.T_OP1_105)		; syscall
  (ia86.mcall	$m.syscall))

(define-sassy-instr (ia86.T_OP1_106)		; creg
  (ia86.mcall	$m.creg))

(define-sassy-instr (ia86.T_OP1_107)		; creg-set!
  (ia86.mcall	$m.creg-set!))

(define-sassy-instr (ia86.T_OP1_108)		; gc-counter
  `(mov	RESULT (& GLOBALS ,(+ G_GC_CNT))))

(define-sassy-instr (ia86.T_OP2_109 x)		; make-string
  ;; exception code wrong, matches Sparc
  (ia86.make_indexed_structure_byte x $hdr.string  $ex.mkbvl))

(define-sassy-instr (ia86.T_OP2IMM_128 x)		; typetag-set!
  (ia86.const2regf SECOND x)
  (ia86.mcall	$m.typetag-set))

(define-sassy-instr (ia86.T_OP2IMM_129 x)		; eq?
  `(cmp	RESULT ,x)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP2IMM_130 x)		; +
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	RESULT_LOW fixtag_mask)
    `(jnz short ,L1)
    `(add	RESULT ,x)
    `(jno short ,L2)
    `(sub	RESULT ,x)
    `(label ,L1)
    `(mov	SECOND ,x)
    (ia86.mcall	$m.add)
    `(label ,L2)))


(define-sassy-instr (ia86.T_OP2IMM_131 x)		; -
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	RESULT_LOW fixtag_mask)
    `(jnz short ,L1)
    `(sub	RESULT ,x)
    `(jno short ,L2)
    `(add	RESULT ,x)
    `(label ,L1)
    `(mov	SECOND ,x)
    (ia86.mcall	$m.subtract)
    `(label ,L2)))

(define-sassy-instr (ia86.T_OP2IMM_132 x)		; <
  (ia86.generic_imm_compare x 'l  $m.numlt))

(define-sassy-instr (ia86.T_OP2IMM_133 x)		; <=
  (ia86.generic_imm_compare x 'le  $m.numle))

(define-sassy-instr (ia86.T_OP2IMM_134 x)		; =
  (ia86.generic_imm_compare x 'e  $m.numeq))

(define-sassy-instr (ia86.T_OP2IMM_135 x)		; >
  (ia86.generic_imm_compare x 'g  $m.numgt))

(define-sassy-instr (ia86.T_OP2IMM_136 x)		; >=
  (ia86.generic_imm_compare x 'ge  $m.numge))

(define-sassy-instr (ia86.T_OP2IMM_137 x)		; char<?
  (ia86.generic_char_imm_compare x 'l  $ex.char<?))

(define-sassy-instr (ia86.T_OP2IMM_138 x)		; char<=?
  (ia86.generic_char_imm_compare x 'le  $ex.char<=?))

(define-sassy-instr (ia86.T_OP2IMM_139 x)		; char=?
  (ia86.generic_char_imm_compare x 'e  $ex.char=?))

(define-sassy-instr (ia86.T_OP2IMM_140 x)		; char>?
  (ia86.generic_char_imm_compare x 'g  $ex.char>?))

(define-sassy-instr (ia86.T_OP2IMM_141 x)		; char>=?
  (ia86.generic_char_imm_compare x 'ge  $ex.char>=?))

;;; The following five are probably a waste of effort.

(define-sassy-instr (ia86.T_OP2IMM_142 x)		; string-ref
  (ia86.indexed_structure_ref_imm/hdr x $tag.bytevector-tag  $hdr.string  $ex.sref  1)
  `(shl	RESULT char_shift)
  `(or	RESULT_LOW ,$imm.character))

(define-sassy-instr (ia86.T_OP2IMM_143 x)		; vector-ref
  (ia86.indexed_structure_ref_imm/hdr x VEC_TAG  $hdr.vector  $ex.vref  0))

(define-sassy-instr (ia86.T_OP2IMM_144 x)		; bytevector-ref
  (ia86.indexed_structure_ref_imm/hdr x $tag.bytevector-tag  $hdr.bytevector  $ex.bvref  1)
  `(shl	RESULT 2))
	
(define-sassy-instr (ia86.T_OP2IMM_145 x)		; bytevector-like-ref
  (ia86.indexed_structure_ref_imm x $tag.bytevector-tag  $ex.bvlref  1)
  `(shl	RESULT 2))

(define-sassy-instr (ia86.T_OP2IMM_146 x)		; vector-like-ref
  (ia86.indexed_structure_ref_imm x VEC_TAG  $ex.vlref  0))

(define-sassy-instr (ia86.T_OP1_200)		; most-positive-fixnum
  (ia86.const2regf RESULT #x7FFFFFFC))

(define-sassy-instr (ia86.T_OP1_201)		; most-negative-fixnum
  (ia86.const2regf RESULT #x80000000))

(define-sassy-instr (ia86.T_OP2_202 x)		; fx+
  (ia86.fixnum_arithmetic x add  sub  $ex.fx+))

(define-sassy-instr (ia86.T_OP2_203 x)		; fx-
  (ia86.fixnum_arithmetic x sub  add  $ex.fx-))

(define-sassy-instr (ia86.T_OP1_204)		; fx--
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           `(test	RESULT_LOW fixtag_mask)
           `(jnz short ,L1)
           `(neg	RESULT)
           `(jno short ,L2)
           ;; No need to undo: RESULT is unchanged
           `(label ,L1)
           (ia86.exception_continuable $ex.fx-- L0)
           `(label ,L2)))
        (else
         `(neg	RESULT))))

(define-sassy-instr (ia86.T_OP2_205 x)              ; fx*
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.loadr	TEMP x)
           `(or	TEMP RESULT)
           (ia86.test	TEMP_LOW fixtag_mask)
           `(jnz short ,L1)
           (ia86.loadr	TEMP x)
           `(sar	TEMP 2)
           `(imul	TEMP RESULT)
           `(jno short ,L2)
           `(label ,L1)
           (ia86.loadr	TEMP x)
           (ia86.exception_continuable $ex.fx* L0)
           `(label ,L2)
           `(mov	RESULT TEMP)))
        ((is_hwreg x)
         `(shr	RESULT)
         `(imul	RESULT ,(REG x)))
        (else
         `(shr	RESULT)
         `(imul	RESULT (& GLOBALS ,(+ (G_REG x)))))))

(define-sassy-instr (ia86.T_OP2_206 x)		; fx=
  (ia86.fixnum_compare x e  $ex.fx=))

(define-sassy-instr (ia86.T_OP2_207 x)		; fx<
  (ia86.fixnum_compare x l  $ex.fx<))

(define-sassy-instr (ia86.T_OP2_208 x)		; fx<=
  (ia86.fixnum_compare x le  $ex.fx<=))

(define-sassy-instr (ia86.T_OP2_209 x)		; fx>
  (ia86.fixnum_compare x g  $ex.fx>))

(define-sassy-instr (ia86.T_OP2_210 x)		; fx>=
  (ia86.fixnum_compare x ge  $ex.fx>=))

; Changed T_OP2_2{11,12,13} to OP1.
; Do we refer to these as OP2 anywhere?
(define-sassy-instr (ia86.T_OP1_211)		; fxzero?
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	RESULT_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.fxzero? L0)
           `(label ,L1))))
  (ia86.test	RESULT RESULT)
  (ia86.setcc	'z))

(define-sassy-instr (ia86.T_OP1_212)		; fxpositive?
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           `(test	RESULT_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.fxpositive? L0)
           `(label ,L1))))
  `(cmp	RESULT 0)
  (ia86.setcc	'g))

(define-sassy-instr (ia86.T_OP1_213)		; fxnegative?
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label)))
           `(label ,L0)
           (ia86.test	RESULT_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.exception_continuable $ex.fxnegative? L0)
           `(label ,L1))))
  (cmp	RESULT 0)
  (setcc        'l))

(define-sassy-instr (ia86.fixnum_imm_arithmetic x y z ex)
  (cond ((not (unsafe-code))
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(label ,L0)
           (ia86.const2regf TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           `(jnz short ,L1)
           (ia86.const2regf TEMP x)
           `(,y	RESULT TEMP)
           `(jno short ,L2)
           `(,z	RESULT TEMP)
           `(label ,L1)
           (ia86.exception_continuable ex L0)	; second is tmp so 2nd arg is in place
           `(label ,L2)))
        (else
         (ia86.const2regf TEMP x)
         `(,y	RESULT TEMP))))
	
(define-sassy-instr (ia86.T_OP2IMM_250 x)           ; fx+
  (ia86.fixnum_imm_arithmetic x add  sub  $ex.fx+))

(define-sassy-instr (ia86.T_OP2IMM_251 x)           ; fx-
  (ia86.fixnum_imm_arithmetic x sub  add  $ex.fx-))

;;; fixnum_imm_compare const, cc, ex
(define-sassy-instr (ia86.fixnum_imm_compare x y z)
  (let ((L0 (fresh-label))
        (L1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,L0)
           (ia86.const2regf TEMP x)
           `(or	TEMP RESULT)
           `(test	TEMP_LOW fixtag_mask)
           `(jz short ,L1)
           (ia86.const2regf TEMP x)
           (ia86.exception_continuable z L0)	; second is tmp so 2nd arg is in place
           `(label ,L1)))
    (ia86.const2regf TEMP x)
    `(cmp	RESULT 	TEMP)
    (ia86.setcc y)))

(define-sassy-instr (ia86.T_OP2IMM_253 x)		; fx=
  (ia86.fixnum_imm_compare x 'e  $ex.fx=))

(define-sassy-instr (ia86.T_OP2IMM_254 x)		; fx<
  (ia86.fixnum_imm_compare x 'l  $ex.fx<))

(define-sassy-instr (ia86.T_OP2IMM_255 x)		; fx<=
  (ia86.fixnum_imm_compare  x 'le  $ex.fx<=))

(define-sassy-instr (ia86.T_OP2IMM_256 x)		; fx>
  (ia86.fixnum_imm_compare  x 'g  $ex.fx>))

(define-sassy-instr (ia86.T_OP2IMM_257 x)		; fx>=
  (ia86.fixnum_imm_compare  x 'ge  $ex.fx>=))

;;; Unsafe/trusted primitives

(define-sassy-instr (ia86.T_OP1_401)		; vector-length:vec
  `(mov	RESULT (& (- ,(+ RESULT VEC_TAG))))
  `(shr	RESULT 8))

(define-sassy-instr (ia86.T_OP2_402 x)		; vector-ref:trusted
  `(add	RESULT wordsize-VEC_TAG)
  (cond ((is_hwreg x)
         `(mov	RESULT (& RESULT ,(+ (REG x)))))
        (else
         (ia86.loadr	TEMP x)
         `(mov	RESULT (& RESULT ,(+ TEMP))))))

(define-sassy-instr (ia86.T_OP3_403 x y)		; vector-set!:trusted
  (ia86.do_indexed_structure_set_word x y VEC_TAG))

(define-sassy-instr (ia86.T_OP1_404)		; car:pair
  `(mov	RESULT (& RESULT ,(- PAIR_TAG))))

(define-sassy-instr (ia86.T_OP1_405)		; cdr:pair
  `(mov	RESULT (& RESULT ,(+ (- PAIR_TAG) wordsize))))

(define-sassy-instr (ia86.T_OP2_406 x)		; =:fix:fix
  (ia86.trusted_fixnum_compare x 'e))

(define-sassy-instr (ia86.T_OP2_407 x)		; <:fix:fix
  (ia86.trusted_fixnum_compare x 'l))


(define-sassy-instr (ia86.T_OP2_408 x)		; <=:fix:fix
  (ia86.trusted_fixnum_compare x 'le))

(define-sassy-instr (ia86.T_OP2_409 x)		; >=:fix:fix
  (ia86.trusted_fixnum_compare x 'ge))

(define-sassy-instr (ia86.T_OP2_410 x)		; >:fix:fix
  (ia86.trusted_fixnum_compare x 'g))

(define-sassy-instr (ia86.T_OP2IMM_450 x)		; vector-ref:trusted
  `(mov	RESULT (& (+ RESULT (wordsize-VEC_TAG) ,x))))

(define-sassy-instr (ia86.T_OP2IMM_451 x)		; =:fix:fix
  `(cmp	RESULT ,x)
  (ia86.setcc	'e))

(define-sassy-instr (ia86.T_OP2IMM_452 x)		; <:fix:fix
  `(cmp	RESULT ,x)
  (ia86.setcc	'l))

(define-sassy-instr (ia86.T_OP2IMM_453 x)		; <=:fix:fix
  `(cmp	RESULT ,x)
  (ia86.setcc	'le))

(define-sassy-instr (ia86.T_OP2IMM_454 x)		; >:fix:fix
  `(cmp	RESULT ,x)
  (ia86.setcc	'g))

(define-sassy-instr (ia86.T_OP2IMM_455 x)		; >=:fix:fix
  `(cmp	RESULT ,x)
  (ia86.setcc	'ge))

;;; Introduced by representation inference.  Trusted.

(define-sassy-instr (ia86.T_OP2_500 x)		; +:idx:idx
  (cond ((is_hwreg x)
         `(add	RESULT ,(REG x)))
        (else
         `(add	RESULT (& GLOBALS ,(+ (G_REG x)))))))

(define-sassy-instr (ia86.T_OP2_501 x)		; +:fix:fix
  (let ((L1 (fresh-label)))
    (ia86.loadr	TEMP x)
    `(add	RESULT TEMP)
    `(jno short ,L1)
    `(sub	RESULT TEMP)
    (ia86.mcall	M_ADD)                          ; second is temp so 2nd arg is in place
    `(label ,L1)))

(define-sassy-instr (ia86.T_OP2_502 x)		; -:idx:idx
  (cond ((is_hwreg x)
         `(sub	RESULT ,(REG x)))
        (else
         `(sub	RESULT (& GLOBALS ,(+ (G_REG x)))))))

(define-sassy-instr (ia86.T_OP2_503 x)		; -:fix:fix
  (let ((L1 (fresh-label)))
    (ia86.loadr	TEMP x)
    `(sub	RESULT TEMP)
    `(jno short ,L1)
    `(add	RESULT TEMP)
    (ia86.mcall	M_SUBTRACT)	; second is temp so 2nd arg is in place
    `(label ,L1)))

(define-sassy-instr (ia86.T_OP2IMM_520 x)		; +:idx:idx
  `(add	RESULT ,x))

(define-sassy-instr (ia86.T_OP2IMM_521 x)		; +:fix:fix
  (let ((L1 (fresh-label)))
    `(add	RESULT ,x)
    `(jno short ,L1)
    `(sub	RESULT ,x)
    `(mov	SECOND ,x)
    (ia86.mcall	M_ADD)
    `(label ,L1)))

(define-sassy-instr (ia86.T_OP2IMM_522 x)		; -:idx:idx
  `(sub	RESULT ,x))

(define-sassy-instr (ia86.T_OP2IMM_523 x)		; -:fix:fix
  (let ((L1 (fresh-label)))
    `(sub	RESULT ,x)
    `(jno short ,L1)
    `(add	RESULT ,x)
    `(mov	SECOND ,x)
    (ia86.mcall	M_SUBTRACT)
    `(label ,L1)))


;;; Experimental stuff below this line, we need more than this to support
;;; peephole optimization well.

(define-sassy-instr (ia86.T_OP1_612 x)		; internal:branchf-zero?
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    (ia86.timer_check)
    `(test	RESULT_LOW fixtag_mask)
    `(jz short ,L1)
    (ia86.mcall	M_ZEROP)
    `(cmp	RESULT_LOW FALSE_CONST)
    `(je	(t_label ,x))
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp	RESULT 0)
    `(jne	(t_label ,x))
    `(label ,L2)))

(define-sassy-instr (ia86.OP2IMM_BRANCHF_lessthan x y)
  (let ((L1 (fresh-label))
        (L2 (fresh-label)))
    `(test	RESULT_LOW fixtag_mask)
    `(jz short ,L1)
    (ia86.const2regf SECOND x)
    (ia86.mcall	M_NUMLT)
    `(cmp	RESULT_LOW FALSE_CONST)
    `(je	(t_label ,y))
    `(jmp short ,L2)
    `(label ,L1)
    `(cmp	RESULT ,x)
    `(jge	(t_label ,y))
    `(label ,L2)))
	
;;; eof

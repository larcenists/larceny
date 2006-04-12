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

(define-sassy-constant VEC_TAG $tag.vector-tag)  
       
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
(define-sassy-macro (char n)	         (logior (lsh n char_shift) IMM_CHAR))
(define-sassy-macro (roundup4 x)	 (logand (+ x 3) (complement 3)))
(define-sassy-macro (roundup8 x)	 (logand (+ x 7) (complement 7)))
(define-sassy-macro (words2bytes n)      (* n 4))
(define-sassy-macro (stkslot n)          (+ CONT STK_REG0 (words2bytes n)))
(define-sassy-macro (framesize n)        (roundup8 (+ wordsize STK_OVERHEAD (words2bytes n))))
(define-sassy-macro (recordedsize n)     (+ STK_OVERHEAD (words2bytes n)))
(define-sassy-macro (t_label l)          l)
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Utility macros for MAL instruction definitions

(define-sassy-macro (mcall fcn)
  `(begin (call	(& (+ GLOBALS ,fcn)))
	  (align code_align)))

;;; loadr targetreg, regno
;;; 	load HW register targetreg from VM register regno
	
(define-sassy-macro (loadr targetreg regno)
  (if (is_hwreg regno)
      `(mov ,targetreg (REG ,regno))
      `(mov ,targetreg (& (+ GLOBALS (G_REG ,regno))))))

;;; storer regno, sourcereg
;;;     store VM register regno from HW register sourcereg
;;;     Does not destroy sourcereg

(define-sassy-macro (storer regno sourcereg)
  (if (is_hwreg regno)
      `(mov	(REG ,regno) ,sourcereg)
      `(mov     (& (+ GLOBALS (G_REG ,regno))) ,sourcereg)))

;;; loadc hwreg, slot
;;;	Load constant vector element 'slot' into hwreg
	
(define-sassy-macro (loadc hwreg slot)
  `(begin 
     (loadr	,hwreg 0)
     (mov	,hwreg (& (+ ,hwreg (- PROC_TAG) PROC_CONSTVECTOR)))
     (mov	,hwreg (& (+ ,hwreg (- VEC_TAG) (words2bytes ,(+ slot 1)))))))

;;; write_barrier r1 r2
;;;	Move values from hardware registers r1 and r2 to RESULT 
;;; 	and SECOND and perform a write barrier.  r1 and r2 may 
;;; 	be -1, in which case the value must already be in RESULT 
;;; 	and SECOND.
;;;
;;;     For INLINE_ASSIGNMENT, test the r2 value and skip the barrier
;;;     if the low bit is not 1.

(define-sassy-macro (write_barrier r1 r2)
  (cond
   ((inline-assignmnet)
    `(locals (L0)
       (begin 
	 ,(if (and (not (= r2 -1))
		   (is_hwreg r2))
	      `(begin 
		(test	(reg ,r2) 1)
		(jz short L0)
		(mov	SECOND (reg ,r2)))
	      `(begin
		,@(if (not (= r2 -1))
		      (list `(mov	SECOND (REG ,r2)))
		      (list))
		(test	SECOND 1)
		(jz short L0)))
	 ,(if (not (= r1 -1))
	      (list `(mov	RESULT (reg ,r1)))
	      (list))
	 (mcall M_PARTIAL_BARRIER)
	 (label L0))))
   (else 
    `(begin 
       ,(if (not (= r1 -1))
	    (list `(mov RESULT (reg ,r1)))
	    (list))
       ,(if (not (= r2 -1))
	    (list `(mov SECOND (reg ,r2)))
	    (list))
       (mcall M_FULL_BARRIER)))))
	
;;; timer_check
;;;	decrement timer and take interrupt if zero

(define-sassy-macro (timer_check)
  `(locals (L1)
     (begin
       (dec (dword (& (+ GLOBALS G_TIMER))))
       (jnz short L1)
       (mcall	M_TIMER_EXCEPTION)
       (label L1))))

;;; exception_noncontinuable excode
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is noncontinuable
	
(define-sassy-macro (exception_noncontinuable excode)
  `(begin
     (call	(& (+ GLOBALS M_EXCEPTION)))
     (dw	,excode)))
		
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

(define-sassy-macro (exception_continuable excode restart)
  `(begin
     (call	(& (+ GLOBALS M_EXCEPTION)))
     (dw	,excode)
     (align	code_align)
     (jmp	,restart)))

;;; FSK: umm.  these probably can't work this way in Sassy

;;; begin_codevector name
;;; 	Define a code vector, just raw code
	
;;(define-sassy-macro (begin_codevector name)
;;  (section	.text
;;	align	code_align
;;,name:


;;; end_codevector name
;;;	Terminate a codevector started by begin_codevector

;;(define-sassy-macro (end_codevector name)
;;end_codevector_,name:


;;; alloc
;;;	Given fixnum number of words in RESULT, allocate
;;;	a structure of that many bytes and leave a raw pointer
;;;	in RESULT.

(define-sassy-macro (alloc)
  (if (inline-allocation)
      `(locals (L1 L2)
	 (label L1
	   (mov TEMP (& (+ GLOBALS G_ETOP)))
	   (add TEMP RESULT) ; allocate
	   (add TEMP 4)      ;  and
	   (add TEMP -8)     ;   round up to 8-byte boundary
	   (cmp TEMP CONT)
	   (jle short L2)
	   (mcall M_MORECORE)
	   (jmp short L1))
	 (label L2
	   (mov RESULT (& (+ GLOBALS G_ETOP)))
	   (mov (& (+ GLOBALS G_ETOP)) TEMP)))
      '(mcall M_ALLOC)))

;;; const2reg hwreg const
;;; Move a constant to a register *without changing the flags*

(define-sassy-macro (const2reg hwreg const)
  `(mov ,hwreg ,const))         ; 5 bytes

;;; const2regf hwreg const
;;; Move a constant to a register, possibly killing the flags
;;; Makes for smaller code size.

(define-sassy-macro (const2regf hwreg const)
  (cond ((= const 0)
	 `(xor ,hwreg ,hwreg))  ; 2 bytes
	((= const 1)
	 `(begin 
	    (xor ,hwreg ,hwreg) ; 3 bytes
	    (inc ,hwreg)))
	((= const -1)
	 `(begin
	    (xor ,hwreg ,hwreg) ; 3 bytes
	    (dec ,hwreg)))
	(else
	 `(mov ,hwreg ,const)))); 5 bytes
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; MacScheme machine instruction set

(define-sassy-macro (T_ALIGN x)
  `(align	,x))

(define-sassy-macro (T_CONT)
  `())
	
(define-sassy-macro (T_LABEL n)
  `(label ,(t_label n)))

;;; CONST is broken up into two instructions depending on the
;;; type of the constant.  The argument to T_CONST_IMM is a bitpattern
;;; or symbolic constant name, to T_CONST_CONSTVECTOR it is the
;;; constant vector index.
	
(define-sassy-macro (T_CONST_IMM x)
  `(const2regf RESULT ,x))

(define-sassy-macro (T_CONST_CONSTVECTOR x)
  `(loadc	RESULT ,x))

(define-sassy-macro (T_CONST_SETREG_IMM x r)
  (if (is_hwreg r)
      `(mov (reg ,r) x)
      `(begin 
	 (mov TEMP ,x)
	 (storer ,r TEMP))))

(define-sassy-macro (T_CONST_SETREG_CONSTVECTOR x r)
  (if (is_hwreg r)
      `(loadc (reg r) x)
      `(begin 
	 (loadc TEMP x)
	 (storer r TEMP))))

;;; OPTIMIZEME:	if #!undefined fit in a byte, then we could do a byte
;;; compare here, at least.  (Why does it not fit in a byte?)

(define-sassy-macro (T_GLOBAL x)
  `(locals (L0 L1)
     (label L0 
       (loadc TEMP ,x)
       (mov RESULT (& (- TEMP PAIR_TAG)))
       ,@(if (not (unsafe-globals))
	     `((cmp RESULT UNDEFINED_CONST)
	       (jne short L1)
	       (mov RESULT TEMP)
	       (mcall M_GLOBAL_EX)
	       (jmp short L0)
	       (label L1))
	     (list)))))

(define-sassy-macro (T_SETGLBL x)
  `(begin
     (mov	SECOND RESULT)
     (loadc	RESULT ,x)
     (mov	(& (- RESULT PAIR_TAG) SECOND))
     (write_barrier -1 -1)))

(define-sassy-macro (T_LEXICAL rib off)
  `(begin
     (loadr	TEMP 0)		; We know R0 is not a HWREG
     (times rib (mov TEMP (& (+ TEMP (- PROC_TAG) PROC_REG0))))
     (mov RESULT (& (+ TEMP (- PROC_TAG) PROC_REG0 (words2bytes off))))))

(define-sassy-macro (T_SETLEX rib off)
  `(begin 
     (loadr	TEMP 0)		; We know R0 is not a HWREG
     (times rib (mov TEMP (& (+ TEMP (- PROC_TAG) PROC_REG0))))
     (mov (& (+ TEMP (- PROC_TAG) PROC_REG0 (words2bytes off))) RESULT)))
	
(define-sassy-macro (T_STACK slot)
  `(mov	RESULT (stkslot ,slot)))

(define-sassy-macro (T_SETSTK slot)
  `(mov	(stkslot ,slot) RESULT))

(define-sassy-macro (T_LOAD r slot)
  (if (is_hwreg r)
      `(mov (reg ,r) (stkslot ,slot))
      `(begin (mov TEMP (stkslot ,slot))
	      (storer ,r TEMP))))

(define-sassy-macro (T_STORE r slot)
  (if (is_hwreg r)
      `(mov	(stkslot ,slot) (reg ,r))
      `(begin 
	 (loadr	TEMP ,r)
	 (mov	(stkslot ,slot) TEMP))))

(define-sassy-macro (T_REG r)
  `(loadr RESULT ,r))
	
;;; Does not destroy RESULT.  The peephole optimizer uses that fact.
(define-sassy-macro (T_SETREG r)
  `(storer	,r RESULT))

(define-sassy-macro (T_MOVEREG r1 r2)
  (cond ((is_hwreg r1)
	 `(storer ,r2 (reg ,r1)))
	((is_hwreg r2)
	 `(loadr (reg ,r2) ,r1))
	(else
	 `(begin 
	    (loadr TEMP ,r1)
	    (storer ,r2 TEMP)))))

(define-sassy-macro (init_closure r)
  (let ((regno 'set-banged-below))
    `(begin
       ,(cond 
	 ((> r *lastreg*)
	  (set! regno (- *lastreg* 1))
	  `(locals (L1)
	     (mov (& (+ GLOBALS G_STKP)) CONT)     ; Need a working register!
	     (mov (& (+ GLOBALS G_RESULT)) RESULT) ; Save for later
	     (add RESULT (+ PROC_REG0 (words2bytes LASTREG)))
	     (loadr CONT 31)
	     (label L1
	       (mov TEMP (& (- CONT PAIR_TAG)))
	       (mov (& RESULT) TEMP)
	       (add RESULT wordsize)
	       (mov CONT (& (+ CONT (- PAIR_TAG) wordsize)))
	       (cmp CONT NIL_CONST)
	       (jne short L1)
	       (mov CONT (& (+ GLOBALS G_STKP)))
	       (mov RESULT (& (+ GLOBALS G_RESULT))))))
	 (else
	  (set! regno r)
	  '()))
       ,@(let rep ((regno regno))
	   (cond 
	    ((>= regno 0)
	     (cons 
	      (if (is_hwreg regno)
		  `(mov (& (+ RESULT PROC_REG0 (words2bytes ,regno))) 
			(reg ,regno))
		  `(begin 
		     (loadr TEMP ,regno)
		     (mov (& (+ RESULT PROC_REG0 (words2bytes ,regno))) TEMP)))
	      (rep (- regno 1))))
	    (else 
	     '())))
       (add RESULT_LOW PROC_TAG))))

(define-sassy-macro (T_LAMBDA codevec constvec n)
  ;; arguments are codevector name, constant vector offset, and n
  `(begin
     (const2regf RESULT (fixnum (+ PROC_HEADER_WORDS PROC_OVERHEAD_WORDS ,n 1)))
     (alloc)
     (mov (dword (& RESULT)) (orr 
			      (lsh (words2bytes ,(+ PROC_OVERHEAD_WORDS n 1))
				   8)
			      PROC_HDR))
     ;; Adjust only if code is in bytevectors!
     ;mov	dword [RESULT+PROC_CODEVECTOR_NATIVE], ,codevec + BVEC_TAG
     (mov	(dword (& (+ RESULT PROC_CODEVECTOR_NATIVE))) ,codevec)
     (loadc	TEMP ,constvec)
     (mov	(& (+ RESULT PROC_CONSTVECTOR)) TEMP)
     (init_closure ,n)))

(define-sassy-macro (T_LEXES n)
  ;; argument is n
  `(begin
     (const2regf RESULT (fixnum (+ PROC_HEADER_WORDS PROC_OVERHEAD_WORDS ,n 1)))
     (alloc)
     (mov	(dword (& RESULT)) (orr 
				    (lsh (words2bytes ,(+ PROC_OVERHEAD_WORDS n 1))
					 8)
				    PROC_HDR))
     (loadr	TEMP 0)
     (mov	TEMP (& (+ TEMP (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))
     (mov	(dword (& (+ RESULT PROC_CODEVECTOR_NATIVE))) TEMP)
     (loadr	TEMP 0)
     (mov	TEMP (& (+ TEMP (- PROC_TAG) PROC_CONSTVECTOR)))
     (mov	(dword (& (+ RESULT PROC_CONSTVECTOR))) TEMP)
     (init_closure ,n)))

(define-sassy-macro (T_ARGSEQ n)
  (if (not (unsafe-code))
      `(locals (L0 L1)
	 (label L0
	   (cmp	RESULT (fixnum ,n))
	   (je short L1)
	   (mcall	M_ARGC_EX)
	   (jmp	L0))
	 (label L1))
      '()))

(define-sassy-macro (T_ARGSGE n)
  `(begin
     (const2regf SECOND (fixnum ,n))
     ,@(if (and (not (unsafe-code))
		(> n 0))
	   '(locals (L0 L1)
	      (label L0 
		(cmp RESULT SECOND)
		(jge short L1)
		(mcall M_ARGC_EX)
		(jmp L0))
	      (label L1))
	   '())
     (mcall M_VARARGS)))

;;; Note the millicode for M_INVOKE_EX is used to check for timer
;;; exception as well, and must check the timer first.

(define-sassy-macro (T_INVOKE n)
  (if (unsafe-code)
      `(begin (timer-check)
	      (storer 0 RESULT)
	      (mov TEMP RESULT)
	      (const2regf RESULT (fixnum ,n))
	      (jmp (& (+ TEMP (- PROC_TAG) PROC_CODEVECTOR_NATIVE))))
      `(locals (L0 L1)
	 (dec (dword (& (+ GLOBALS G_TIMER))))
	 (jnz short L1)
	 (label L0 
	   (mcall M_INVOKE_EX))
	 (label L1 
	   (lea TEMP (& (+ RESULT 8 (- PROC_TAG))))
	   (test TEMP_LOW tag_mask)
	   (jnz short L0)
	;; Observe TEMP points to proc+8 here and that if we were
	;; really perverse we would lay the procedure out so that
	;; the codevector is stored at offset 4, reducing the size
	;; of the JMP instruction below.
	   (storer	0 RESULT)
	   (const2regf RESULT (fixnum ,n))
	   (jmp	(& (+ TEMP -8 PROC_CODEVECTOR_NATIVE)))))))

;;; Introduced by peephole optimization.
;;; 
;;; The trick here is that the tag check for procedure-ness will
;;; catch undefined variables too.  So there is no need to see
;;; if the global has an undefined value, specifically.  The
;;; exception handler figures out the rest.

(define-sassy-macro (T_GLOBAL_INVOKE g n)
  (if (unsafe-globals)
      `(begin (T_GLOBAL ,g)
	      (T_INVOKE ,n))
      `(locals (L0 L1 L2)
	 (loadc	RESULT g)		; global cell
	 (label L2
	   (mov TEMP                    ;   dereference
		(& (- RESULT PAIR_TAG)))
	   (inc TEMP)			; really TEMP += PROC_TAG-8
	   (test TEMP_LOW tag_mask)	; tag test
	   (jz short L0))
	 (label L1
	   (mcall M_GLOBAL_INVOKE_EX)   ; RESULT has global cell (always)
	   (jmp short L2))		; Since TEMP is dead following timer interrupt
	 (label L0
	   (dec (dword                  ; timer
		 (& (+ GLOBALS G_TIMER))))
	   (jz short L1)                ;   test
	   (dec TEMP)                   ; undo ptr adjustment
	   (storer 0 TEMP)              ; save proc ptr
	   (const2regf RESULT           ; argument count
		       (fixnum ,n))
	   (jmp	(& (+ TEMP (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))))))
	
;;; Allocate the frame but initialize only the basic slots
;;; and any pad words.  Leave RESULT clear at the end; this
;;; fact is used by T_SAVE1 below.

(define-sassy-macro (T_SAVE0 n)
  `(locals (L0 L1)
     (label L0 
       (sub CONT (framesize ,n))
       (cmp CONT (& (+ GLOBALS G_ETOP)))
       (jge short L1)
       (add CONT (framesize ,n))
       (mcall M_STKOFLOW)
       (jmp L0))
     (label L1
       (mov (dword (& CONT)) (recordedsize ,n))
       ;; Not necessary to store reg0 here, this is handled
       ;; explicitly by the generated code.
       (xor	RESULT RESULT)
       (mov	(dword (& (+ CONT STK_RETADDR))) RESULT)
       ,@(if (= (- (framesize n) (recordedsize n)) 8)
	     ;; We have a pad word at the end -- clear it
	     (list `(mov (dword (& (stkslot (+ ,n 1)))) RESULT))
	     (list)))))

;;; Initialize the numbered slot to the value of RESULT.
;;; Using RESULT is probably OK because it is almost certainly 0
;;; after executing T_SAVE0 and only T_STORE instructions
;;; after that.

(define-sassy-macro (T_SAVE1 n)
  `(mov	(dword (& (stkslot ,n))) RESULT))

;;; T_SAVE may still be emitted by the assembler when peephole 
;;; optimization is disabled.

(define-sassy-macro (T_SAVE n)
  `(begin
     (T_SAVE0 ,n)
     ,@(let rep ((slotno 1))
	 (cond ((<= slotno n)
		(cons `(T_SAVE1 ,slotno)
		      (rep (+ slotno 1))))
	       (else 
		'())))))

(define-sassy-macro (T_SETRTN lbl)
  `(mov	(dword (& (+ CONT STK_RETADDR))) (t_label ,lbl)))

(define-sassy-macro (T_RESTORE n)
  (let rep ((slotno 0))
    (cond ((<= slotno n)
	   (cons (if (is_hwreg slotno)
		     `(mov (reg ,slotno) (dword (& (stkslot ,slotno))))
		     `(begin (mov TEMP (dword (& (stkslot ,slotno))))
			     (mov (& (+ GLOBALS (g_reg ,slotno))) TEMP)))
		 (rep (+ slotno 1))))
	  (else
	   '()))))

(define-sassy-macro (T_POP n)
  `(add	CONT (framesize ,n)))
	
(define-sassy-macro (T_POPSTK)
  (error 'T_POPSTK "not implemented -- students only"))

(define-sassy-macro (T_RETURN)
  '(jmp (& (+ CONT STK_RETADDR))))
	
(define-sassy-macro (T_APPLY x y)
  `(begin
     (timer_check)
     (loadr	TEMP ,y)
     (mov	(& (+ GLOBALS G_THIRD)) TEMP)
     (loadr	SECOND ,x)
     (mcall	M_APPLY)
     (loadr	TEMP 0)
     (mov	TEMP (& (+ TEMP (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))
     (jmp	TEMP)))

(define-sassy-macro (T_NOP)
  '()) ;; nothing

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; in this implementation)
	
(define-sassy-macro (T_JUMP levels label name)
  `(begin 
     (timer_check)
     ,@(if (> levels 0) 
	   (list '(begin 
		    (loadr TEMP 0)		; We know R0 is not a HWREG
		    (times levels 
		      (mov TEMP (& (+ TEMP (- PROC_TAG) PROC_REG0))))
		    (storer 0 TEMP)))
	   (list))
     (jmp ,name)))

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

(define-sassy-macro (T_SKIP lbl)
  `(jmp	(t_label ,lbl)))

(define-sassy-macro (T_SKIPF lbl)
  `(begin
     (cmp	RESULT_LOW FALSE_CONST)
     (je	(t_label ,lbl))))

(define-sassy-macro (T_BRANCH lbl)
  `(begin
     (dec	(dword (& (+ GLOBALS G_TIMER))))
     (jnz	(t_label ,lbl))
     (mcall	M_TIMER_EXCEPTION)
     (jmp	(t_label ,lbl))))

(define-sassy-macro (T_BRANCHF lbl)
  `(begin
     ((timer_check)
      (cmp	RESULT_LOW FALSE_CONST)
      (je	(t_label ,lbl)))))

(define-sassy-macro (T_CHECK w x y z)
  `(begin
     (cmp	RESULT_LOW FALSE_CONST)
     (je	(t_label ,z))))

;; Call handler for noncontinuable exception with RESULT,
;; SECOND, and THIRD defined.

(define-sassy-macro (T_TRAP w x y z)
	;; Order matters here, because SECOND is TEMP and
	;; may be destroyed by loading of THIRD
  `(begin
     ,@(if (not (= w 0))
	   (list '(loadr RESULT ,w))
	   (list))
     ,@(if (not (= y 0))
	   ;; OPTIMIZEME: optimize for case when %3 is HW reg
	   ;; (this will however have almost no impact)
	   (list `(begin (loadr	TEMP ,y)
			 (mov (& (+ GLOBALS G_THIRD)) TEMP)))
	   (list))
     ,@(if (not (= x 0))
	   (list `(loadr SECOND ,x))
	   (list))
     (exception_noncontinuable ,z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Helper macros for primitive operations

;;; setcc cc
;;;	Set RESULT to true if jcc jumps.
;;; 
;;;	It is probably the case that SECOND is available here,
;;;	but I'm not using that fact yet.
	
;(define-sassy-macro (setcc cc)				; 11 bytes, no jump
;	set,cc	RESULT_LOW		; 2 bytes
;	and	RESULT, 1		; 3 bytes
;	shl	RESULT, 2		; 3 bytes
;	or	RESULT_LOW, TRUE_CONST	; 3 bytes
;)

;;; TRUE=6
;;; FALSE=2
(define-sassy-macro (setcc cc)				; 10 bytes, jump
  `(locals (L1)
     (const2reg RESULT TRUE_CONST)	; 5 bytes
     (j,cc short L1)			; 2 bytes
     (sub	RESULT_LOW 4)		; 3 bytes (would be 2 if RESULT=eax)
     (label L1)))

;;; double_tag_predicate ptrtag, hdr
;;;	Set RESULT to #t if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise set RESULT
;;;	to #f.

(define-sassy-macro (double_tag_predicate ptrtag hdr)
  `(begin
     (double_tag_test ,ptrtag ,hdr)
     (setcc z)))

;;; fixnum_test_temp_is_free reg
;;;	Test reg for fixnum-ness and clear zero flag if fixnum.  OK to
;;;	destroy TEMP.
	
(define-sassy-macro (fixnum_test_temp_is_free reg)
  `(begin
     ,(cond ((is_hwreg reg) 
             (cond 
              ((hwreg_has_low reg)
               `(test	REG %+ ,reg %+ _LOW fixtag_mask))
              (else
               ;; test	REG,reg, fixtag_mask
               ;; Above is 6 bytes, below is 4 bytes.  Performance?
               `(begin
                  (mov	TEMP (REG,reg))
                  (test	TEMP_LOW fixtag_mask)))))
            (else
             '(test	byte (& (+ GLOBALS (G_REG,reg))) fixtag_mask)))))

;;; single_tag_test ptrtag
;;;	Leave zero flag set if RESULT contains a value with the given
;;;     3-bit tag.

(define-sassy-macro (single_tag_test x)
  `(begin
     (lea	TEMP (& (+ RESULT (- 8 ,x))))
     (test	TEMP_LOW 7)))
	
;;; single_tag_test_ex ptrtag, exception_code
;;;	Unless in unsafe mode, test the pointer in RESULT for the
;;;	tag and signal an exception if it does not match.

(define-sassy-macro (single_tag_test_ex x y)
  (cond 
   ((not (unsafe-code))
    `(locals (L0 L1)
       (label L0
         (single_tag_test ,x)
         (jz short L1)
         (exception_continuable ,y L0))
       (label L1)))
   (else
    '(begin))))

;;; double_tag_test ptrtag, hdr
;;;	Set the zero flag if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise reset the flag.
;;;     If zero flag is set, leaves the header field in TEMP.

(define-sassy-macro (double_tag_test x y)
  `(locals (L1)
     (single_tag_test ,x)
     (jnz short L1)
     (mov	TEMP (& (- RESULT ,x)))
     (cmp	TEMP_LOW ,y)
     (label L1)))
	
;;; fixnum_arithmetic regno, operation, undo-operation, ex
	
(define-sassy-macro (fixnum_arithmetic x y z ex)
  (cond ((not (unsafe-code))
         `(locals (L0 L1 L2)
            (label L0 
              (loadr	TEMP ,x)
              (or	TEMP RESULT)
              (test	TEMP_LOW fixtag_mask)
              (jnz short L1)
              (loadr	TEMP ,x)
              (,y	RESULT TEMP)
              (jno short L2)
              (,z	RESULT TEMP))
            (label L1 
              (exception_continuable ,ex L0))	; second is tmp so 2nd arg is in place
            (label L2)))
        ((is_hwreg x)
         ;; This looks totally bogus -- should use ,y? XXX TODO BUG HERE
         `(add	RESULT (REG,x)))
        (else
         `(add	RESULT (& (+ GLOBALS (G_REG,x)))))))
	
;;; trusted_fixnum_compare r, cc
(define-sassy-macro (trusted_fixnum_compare x y)
  `(begin
     ,(if (is_hwreg x)
          `(cmp	RESULT (REG,x))
          `(cmp	RESULT (& (+ GLOBALS (G_REG,x)))))
     (setcc	,y)))

;;; fixnum_compare reg, cc, ex
;;; OPTIMIZEME for whenever ,x is a hwreg

(define-sassy-macro (fixnum_compare x y z)
  `(begin
     ,(cond ((not (unsafe-code))
             `(locals (L0 L1)
                (label L0
                  (loadr	TEMP ,x)
                  (or	TEMP RESULT)
                  (test	TEMP_LOW fixtag_mask)
                  (jz short L1)
                  (loadr	SECOND ,x)
                  (exception_continuable ,z L0))         ; second is tmp so 2nd arg is in place
                (label L1 )))
            ((is_hwreg x)
             `(cmp	RESULT (REG,x)))
            (else
             `(cmp	RESULT (& (+ GLOBALS (G_REG,x))))))
     (setcc ,y)))

;;; fixnum_shift r2, operation, ex
;;; 
;;; Shift count must be in CL if it is not constant
;;; OPTIMIZEME: we can do better than what I do here.
	
(define-sassy-macro (fixnum_shift x y z)
  `(begin
     ,(cond ((not (unsafe-code))
             `(locals (L0 L1 L2)
                (label L0
                  (loadr	TEMP ,x)
                  (or	TEMP RESULT)
                  (test	TEMP_LOW fixtag_mask)
                  (loadr	SECOND ,x)
                  (jz short L2))
                (label L1
                  (exception_continuable ,z L0))
                (label L2
                  (cmp	TEMP fixnum(32))	; SECOND is TEMP
                  (jge short L1))))
            (else
             `(loadr	TEMP ,x)))
     (shr	TEMP 2)
     (mov	(& (+ GLOBALS G_REGALIAS_ECX)) ecx)
     (mov	cl al)			; Code knows TEMP is EAX
     (,y	RESULT cl)
     (mov	ecx (& (+ GLOBALS G_REGALIAS_ECX)))
     ,@(cond ((eq? y 'shl)
              (list)) ;; Nothing
             (else
              ;; Right shifts: mask out low bits
              ;; OPTIMIZEME: if RESULT were eax, masking RESULT_LOW with a byte
              ;; would save one byte here.
              (list `(and	RESULT ~fixtag_mask))))))
	
;;; generic_arithmetic regno, operation, undo-operation, millicode
	
(define-sassy-macro (generic_arithmetic x y z millicode)
  `(locals (L1 L2)
     (loadr	TEMP ,x)
     (or	TEMP RESULT)
     (test	TEMP_LOW fixtag_mask)
     (loadr	TEMP ,x)
     (jnz short L1)
     (,y	RESULT TEMP)
     (jno short L2)
     (,z	RESULT TEMP)
     (label L1
       (mcall	,millicode))		; second is temp so 2nd arg is in place
     (label L2 )))
	
;;; generic_compare reg, condition, millicode

(define-sassy-macro (generic_compare x y z)
  `(locals (L1 L2)
     (loadr	TEMP ,x)
     (or	TEMP RESULT)
     (test	TEMP_LOW fixtag_mask)
     (loadr	SECOND ,x)
     (jz short L1)
     (mcall	,z)
     (jmp short L2	)
     (label L1
       (cmp	RESULT SECOND)
       (setcc	,y))
     (label L2 )))

;;; generic_char_compare reg, cc, ex

(define-sassy-macro (generic_char_compare x y z)
  `(begin
     ,(cond ((not (unsafe-code))
             `(locals (L0 L1 L2)
                (label L0
                  (loadr	SECOND ,x)
                  (cmp	SECOND_LOW IMM_CHAR)
                  (jz	L2))
                (label L1
                  (exception_continuable ,z L0))
                (label L2
                  (cmp	RESULT_LOW IMM_CHAR)
                  (jne short L1)
                  (cmp	RESULT SECOND))))
            ((is_hwreg x)
             `(cmp	RESULT (REG,x)))
            (else
             `(cmp	RESULT (& (+ GLOBALS (G_REG,x))))))
     (setcc	,y)))
	
;;; generic_imm_compare imm, cc, millicode

(define-sassy-macro (generic_imm_compare x y z)
  `(locals (L1 L2)
     (test	RESULT_LOW fixtag_mask)
     (jz short L1)
     (const2regf SECOND ,x)
     (mcall	,z)
     (jmp short L2)
     (label L1
       (cmp	RESULT ,x)
       (setcc	,y))
     (label L2 	)))
	
;;; generic_char_imm_compare imm, cc, ex
	
(define-sassy-macro (generic_char_imm_compare x y z)
  `(locals (L0 L1)
     ,@(cond ((not (unsafe-code))
              `(label L0
                 (cmp	RESULT_LOW IMM_CHAR)
                 (jz	L1)
                 (const2regf SECOND ,x)
                 (exception_continuable ,z L0)))
             (else
              '()))
     (label L1
       (cmp	RESULT ,x)
       (setcc	,y))))

;;; indexed_structure_length ptrtag, hdrtag, ex, byte?

(define-sassy-macro (indexed_structure_length x y z byte?)		; string-length or bytevector-length
  `(begin
     ,(cond ((not (unsafe-code))
             `(locals (L0 L1)
                (label L0
                  (double_tag_test ,x ,y)
                  (jz short L1)
                  (exception_continuable ,z L0))
                (label L1 	
                  (mov	RESULT TEMP))))
            (else
             `(mov	RESULT (& (- RESULT ,x)))))
     (shr	RESULT 8)
     ,@(if byte?
           (list `(shl	RESULT 2))
           (list))))

;;; indexed_structure_length ptrtag, ex, byte?
	
(define-sassy-macro (indexed_structure_length x y z)
  `(begin
     (single_tag_test_ex ,x ,y)
     (mov	RESULT (& (- RESULT ,x)))
     (shr	RESULT 8)
     ,@(if z
           (list `(shl	RESULT 2))
           (list))))

;;; indexed_structure_test reg_index, reg_value, ptrtag, hdrtag, ex, byte?, test_reg_value
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that reg_index is a fixnum in the range of the structure.
;;;	If hdrtag is zero then do not check it.

(define-sassy-macro (indexed_structure_test x y z hdrtag ex byte? test_reg_value)
  (cond ((not (unsafe-code))
         `(locals (L0 L1 L2 L3)
            (label L0 
              (fixnum_test_temp_is_free ,x)
              (jnz short L1)
              ,@(cond (hdrtag
                       `((double_tag_test ,z ,hdrtag)
                         (jz short L2)))
                      (else
                       `((single_tag_test ,z)
                         (jz short L3)))))
            (label L1
              (loadr	SECOND ,x)
              (exception_continuable ,ex L0))
            ,@(if (= hdrtag 0)
                  (list `(label L3
                           (mov	TEMP (& (- RESULT ,z)))))
                  (list))
            (label L2
              (shr	TEMP 8)
              ,@(if byte?
                    (list `(shl	TEMP 2))	; Length is now a fixnum
                    (list))
              ,(if (is_hwreg x)
                   `(cmp	TEMP (REG,x))
                   `(cmp	TEMP (& (+ GLOBALS (G_REG,x)))))
              (jbe short L1)
              (,test_reg_value	,y L1))))
        (else
         '(begin))))

;;; indexed_structure_test_imm index, ptrtag, hdrtag, ex, byte?
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that index (a fixnum) is in the range of the structure.
;;;	If hdrtag is zero then do not check it.
	
(define-sassy-macro (indexed_structure_test_imm x y z ex byte?)
  (cond 
   ((not (unsafe-code))
    `(locals (L0 L1 L2 L3)
       (label L0 
         ,@(cond ((not (= z 0))
                  `((double_tag_test ,y ,z)
                    (jz short L2)))
                 (else
                  `((single_tag_test ,y)
                    (jz short L3)))))
       (label L1
         (mov	SECOND ,x)
         (exception_continuable ,ex L0)
         ,@(if (= z 0)
               `((label L3
                   (mov	TEMP (& (- RESULT ,y)))))
               '()))
       (label L2
         (shr	TEMP 8)
         ,@(if byte?
               (list
                `(shl	TEMP 2))		; Length is now a fixnum
               (list))
         (cmp	TEMP ,x)
         (jbe short L1))))
   (else
    `(begin))))

;;; load_from_indexed_structure index_reg, ptrtag, byte?
	
(define-sassy-macro (load_from_indexed_structure x y z)
  (cond (z
         `(begin
            (loadr	TEMP ,x)
            (shr	TEMP 2)
            (mov	RESULT_LOW (& (+ RESULT (- ,y) wordsize TEMP)))
            (and	RESULT #xFF)))
        ((is_hwreg x)
         `(mov	RESULT (& (+ RESULT (- ,y) wordsize (REG,x)))))
        (else
         `(begin
            (loadr	TEMP ,x)
            (mov	RESULT (& (+ RESULT (- ,y) wordsize TEMP)))))))

(define-sassy-macro (load_from_indexed_structure_imm x y z)
  (cond (z
         `(begin
            (mov	RESULT_LOW (& (+ RESULT (- ,y) wordsize (,x/4))))
            (and	RESULT #xFF)))
        (else
         `(mov	RESULT (& (+ RESULT (- ,y) wordsize ,x))))))
				
;;; indexed_structure_ref reg, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-macro (indexed_structure_ref x y z ex byte?)
  `(begin
     (indexed_structure_test ,x 0 ,y ,z ,ex ,byte? check_nothing)
     (load_from_indexed_structure ,x ,y ,byte?)))

;;; indexed_structure_ref_imm idx, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-macro (indexed_structure_ref_imm x y z ex byte?)
  `(begin
     (indexed_structure_test_imm ,x ,y ,z ,ex ,byte?)
     (load_from_indexed_structure_imm ,x ,y ,byte?)))
				
;;; indexed_structure_ref reg, ptrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-macro (indexed_structure_ref x y z byte?)
  `(begin
     (indexed_structure_test ,x 0 ,y 0 ,z ,byte? check_nothing)
     (load_from_indexed_structure ,x ,y ,byte?)))

(define-sassy-macro (indexed_structure_ref_imm x y z chk)
  `(begin
     (indexed_structure_test_imm ,x ,y 0 ,z ,chk)
     (load_from_indexed_structure_imm ,x ,y ,chk)))

;;; check_nothing regno, label
;;;	Just a placeholder.

(define-sassy-macro (check_nothing x y)
  `(begin))
	
;;; check_fixnum regno, label
;;;	Branch to label if regno does not hold a fixnum

(define-sassy-macro (check_fixnum x y)
  `(begin
     (fixnum_test_temp_is_free ,x)
     (jnz short ,y)))
	
;;; check_char regno, label
;;;	Branch to label if regno does not hold a char.
;;;	Leaves char in TEMP (even if it is in hwreg, the value must
;;;	be shifted anyway).

(define-sassy-macro (check_char x y)
  `(begin
     (loadr	TEMP ,x)
     (cmp	TEMP_LOW IMM_CHAR)
     (jnz short ,y)))

;;; indexed_structure_set_* reg_idx, reg_value, ptrtag, hdrtag, ex
;;;	If hdrtag is 0 then do not check it.

(define-sassy-macro (indexed_structure_set_char x y z hdrtag ex)
  `(begin
     (indexed_structure_test ,x ,y ,z ,hdrtag ,ex 1 check_char)
     (mov	(& (+ GLOBALS G_STKP)) CONT)
     (loadr	CONT ,x)
     (shr	TEMP 16)
     (shr	CONT 2)
     (mov	(& (+ RESULT (- ,z) wordsize CONT)) TEMP_LOW)
     (mov	CONT (& (+ GLOBALS G_STKP)))))

(define-sassy-macro (indexed_structure_set_byte x y z hdrtag ex)
  `(begin
     (indexed_structure_test ,x ,y ,z ,hdrbyte ,ex 1 check_fixnum)
     (mov	(& (+ GLOBALS G_STKP)) CONT)
     (loadr	CONT ,x)
     (shr	CONT 2)
     (loadr	TEMP ,y)
     (shr	TEMP 2)
     (mov	(& (+ RESULT (- ,z) wordsize CONT)) TEMP_LOW)
     (mov	CONT (& (+ GLOBALS G_STKP)))))

(define-sassy-macro (indexed_structure_set_word x y z hdrtag ex)
  `(begin
     (indexed_structure_test ,x ,y ,z ,hdrtag ,ex 0 check_nothing)
     (do_indexed_structure_set_word ,x ,y ,z)))

(define-sassy-macro (do_indexed_structure_set_word x y z)
  (cond ((and (is_hwreg y) (is_hwreg x))
         `(begin
            (mov	(& (+ RESULT (- ,z) wordsize (REG,x))) (REG,y))
            (write_barrier -1 ,y)))
        ((is_hwreg y)
         `(begin
            (loadr	TEMP ,x)
            (mov	(& (+ RESULT (- ,z) wordsize TEMP)) (REG,y))
            (write_barrier -1 ,y)))
        ((is_hwreg x)
         `(begin
            (loadr	SECOND ,y)
            (mov	(& (+ RESULT (- ,z) wordsize (REG,x))) SECOND)
            (write_barrier -1 -1)))
        (else
         `(begin
            (mov	(& (+ GLOBALS G_STKP)) CONT)
            (loadr	CONT ,x)
            (loadr	SECOND ,y)
            (mov	(& (+ RESULT (- ,z) wordsize CONT)) SECOND)
            (mov	CONT (& (+ GLOBALS G_STKP)))
            (write_barrier -1 -1)))))

;;; make_indexed_structure_word regno ptrtag hdrtag ex
;;;	Allocate a word structure with the length specified in RESULT
;;;	(fixnum number of entries).  If ,x is not -1, then initialize 
;;;	it with the contents of (REG,x), otherwise with #!unspecified.
	
(define-sassy-macro (make_indexed_structure_word x y z ex)
  `(begin
     ,@(cond ((not (unsafe-code))
              (list `(locals (L0 L1)
                       (label L0
                         (test	RESULT (logior fixtag_mask #x80000000))
                         (jz short L1)
                         (exception_continuable ,ex L0))
                       (label L1))))
             (else
              (list)))
     (mov	(& (+ GLOBALS G_ALLOCTMP)) RESULT)
     (add	RESULT wordsize)
     ,(cond ((= x -1)
             `(mov	SECOND UNSPECIFIED_CONST))
            (else
             `(loadr	SECOND ,x)))
     (mcall	M_ALLOCI)
     (mov	TEMP (& (+ GLOBALS G_ALLOCTMP)))
     (shl	TEMP 8)
     (or	TEMP ,z)
     (mov	(& (+ RESULT)) TEMP)
     (add	RESULT ,y)))

;;; make_indexed_structure_byte regno hdrtag ex
;;;	Allocate a byte structure with the length specified in RESULT
;;;     (fixnum number of bytes).  If ,x is not -1, then (REG,x) must
;;;     hold a char value to be used for initialization (a check is
;;;     performed that is a char).  If ,x is -1, no initialization 
;;; 	is performed.

(define-sassy-macro (make_indexed_structure_byte x y z)
  `(locals (L0 L1 L2)
     ,@(cond ((not (unsafe-code))
              ;; OPTIMIZEME (size): Unless allocation is inline,
              ;; the fixnum test can be moved into the millicode.
              ;; (As can the char test, I guess -- in fact, this whole
              ;; instruction is probably best moved into millicode)
              ;; OPTIMIZEME (speed): Both branches are mispredicted here.
              (list 
               `(label L0 
                  ,@(if (not (= (x -1)))
                        (list `(loadr	SECOND ,x))
                        (list))
                  (test	RESULT (logior fixtag_mask #x80000000))
                  (jz short L2)
                  (label L1
                    (exception_continuable ,z L0))
                  (label L2 
                    ,@(cond ((not (= x -1))
                             `((cmp	SECOND_LOW IMM_CHAR)
                               (jne	L1))
                             (else 
                              '()))))))
              (else
               (list))))
     (mov	(& (+ GLOBALS G_ALLOCTMP)) RESULT)
     (add	RESULT fixnum(wordsize))
     (mcall	M_ALLOC_BV)
     ,@(cond ((not (= x -1))
              (list
               `(begin
                  (loadr	eax ,x)		; Code knows that eax is TEMP/SECOND
                  (mov	(& (+ GLOBALS G_REGALIAS_ECX)) ecx)
                  (mov	(& (+ GLOBALS G_REGALIAS_EDI)) edi)
                  (shr	eax char_shift)	; byte value
                  (mov	ecx (& (+ GLOBALS G_ALLOCTMP)))
                  (shr	ecx 2)		; byte count
                  (lea	edi (& (+ RESULT 4)))	; destination ptr
                  (cld)
                  (rep stosb)		; byte fill
                  (mov	ecx (& (+ GLOBALS G_REGALIAS_ECX)))
                  (mov	edi (& (+ GLOBALS G_REGALIAS_EDI))))))
             (else 
              (list)))
     (mov	TEMP (& (+ GLOBALS G_ALLOCTMP)))
     (shl	TEMP 6)
     (or	TEMP ,y)
     (mov	(& (+ RESULT)) TEMP)
     (add	RESULT BVEC_TAG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Primitive operations
;;; 
;;; The names 'n' in T_OP1_n are the primcodes from the table in
;;; Compiler/standard-C.imp.sch (yes, that's right).

(define-sassy-macro (T_OP1 x)
  (list (string->symbol (string-append "T_OP1_" (number->string x)))))
(define-sassy-macro (T_OP2 x y)
  (list (string->symbol (string-append "T_OP2_" (number->string x))) 
        y))
(define-sassy-macro (T_OP2IMM x y)
  (list (string->symbol (string-append "T_OP2IMM_" (number->string x))) 
        y))

(define-sassy-macro (T_OP1_1)		; break
  `(mcall	M_BREAK))

(define-sassy-macro (T_OP1_3)		; unspecified
  `(const2regf RESULT UNSPECIFIED_CONST))

(define-sassy-macro (T_OP1_4)		; undefined
  `(const2regf RESULT UNDEFINED_CONST))

(define-sassy-macro (T_OP1_5)		; eof-object
  `(const2regf RESULT EOF_CONST))

(define-sassy-macro (T_OP1_6)		; enable-interrupts
  `(mcall	M_ENABLE_INTERRUPTS))

(define-sassy-macro (T_OP1_7)		; disable-interrupts
  `(mcall	M_DISABLE_INTERRUPTS))

(define-sassy-macro (T_OP1_8)		; typetag
  `(mcall	M_TYPETAG))

(define-sassy-macro (T_OP1_9)		; not
  `(begin
     (cmp	RESULT_LOW FALSE_CONST)
     (setcc	z)))

(define-sassy-macro (T_OP1_10)		; null?
  `(begin
     (cmp	RESULT_LOW NIL_CONST)
     (setcc	z)))

(define-sassy-macro (T_OP1_11)		; pair?
  `(begin
     (single_tag_test PAIR_TAG)
     (setcc	z)))
	
(define-sassy-macro (T_OP1_12)		; eof-object?
  `(begin
     (cmp	RESULT EOF_CONST)
     (setcc	z)))

(define-sassy-macro (T_OP1_13)		; port?
  `(double_tag_predicate VEC_TAG PORT_HDR))

(define-sassy-macro (T_OP1_14)		; structure?
  `(double_tag_predicate VEC_TAG STRUCT_HDR))

(define-sassy-macro (T_OP1_15)		; car
  `(begin
     (single_tag_test_ex PAIR_TAG EX_CAR)
     (mov	RESULT (& (- RESULT PAIR_TAG)))))
	
(define-sassy-macro (T_OP1_16)		; cdr
  `(begin
     (single_tag_test_ex PAIR_TAG EX_CDR)
     (mov	RESULT (& (+ RESULT (- PAIR_TAG) wordsize)))))

(define-sassy-macro (T_OP1_17)		; symbol?
  `(double_tag_predicate VEC_TAG SYMBOL_HDR))

(define-sassy-macro (T_OP1_18)		; number? and complex?
  `(mcall	M_COMPLEXP))

(define-sassy-macro (T_OP1_20)		; real? and rational?
  `(mcall	M_RATIONALP))

(define-sassy-macro (T_OP1_21)		; compnum?
  `(double_tag_predicate BVEC_TAG COMPNUM_HDR))

(define-sassy-macro (T_OP1_22)		; integer?
  `(locals (L1 L2)
     (test	RESULT_LOW fixtag_mask)
     (je	L1)
     (mcall	M_INTEGERP)
     (jmp short L2)
     (label L1
       (const2regf RESULT TRUE_CONST))
     (label L2 )))

(define-sassy-macro (T_OP1_23)		; fixnum?
  `(begin
     (test	RESULT_LOW fixtag_mask)
     (setcc	z)))
	
(define-sassy-macro (T_OP1_24)		; flonum?
  `(double_tag_predicate BVEC_TAG FLONUM_HDR))

(define-sassy-macro (T_OP1_25)		; exact?
  `(mcall	M_EXACTP))

(define-sassy-macro (T_OP1_26)		; inexact?
  `(mcall	M_INEXACTP))

(define-sassy-macro (T_OP1_27)		; exact->inexact
  `(mcall	M_EXACT2INEXACT))

(define-sassy-macro (T_OP1_28)		; inexact->exact
  `(mcall	M_INEXACT2EXACT))

(define-sassy-macro (T_OP1_29)		; round
  `(mcall	M_ROUND))

(define-sassy-macro (T_OP1_30)		; truncate
  `(mcall	M_TRUNCATE))

(define-sassy-macro (T_OP1_31)		; zero?
  `(locals (L1 L2)
     (test	RESULT_LOW fixtag_mask)
     (jz short L1)
     (mcall	M_ZEROP)
     (jmp short L2)
     (label L1
       (and	RESULT RESULT)
       (setcc	z))
     (label L2)))

(define-sassy-macro (T_OP1_32)		; --
  `(mcall	M_NEGATE))

(define-sassy-macro (T_OP1_33)		; lognot
  `(locals (L0 L1)
     ,@(if (not (unsafe-code))
           `((label L0
               (test	RESULT_LOW fixtag_mask)
               (jz short L1)
               (exception_continuable EX_LOGNOT L0))
             (label L1))
           '())
     (lea	RESULT (& (+ RESULT fixtag_mask)))
     (not	RESULT)))

(define-sassy-macro (T_OP1_34)		; real-part
  `(mcall	M_REAL_PART))
	
(define-sassy-macro (T_OP1_35)		; imag-part
  `(mcall	M_IMAG_PART))

(define-sassy-macro (T_OP1_36)		; char?
  `(begin
     (cmp	RESULT_LOW IMM_CHAR)
     (setcc	z)))

(define-sassy-macro (T_OP1_37)		; char->integer
  `(locals (L0 L1)
     ,@(if (not (unsafe-code))
           `((label L0
               (cmp	RESULT_LOW IMM_CHAR)
               (jz	L1)
               (exception_continuable EX_CHAR2INT L0))
             (label L1))
           '())
     (shr	RESULT 14)))
	
(define-sassy-macro (T_OP1_38)		; integer->char
  `(begin
     ,@(if (not (unsafe-code))
           `((label L0
               (test	RESULT_LOW fixtag_mask)
               (jz short L1)
               (exception_continuable EX_INT2CHAR L0))
             (label L1))
           '())
     (and	RESULT 1023)
     (shl	RESULT 14)
     (or	RESULT_LOW IMM_CHAR)))

(define-sassy-macro (T_OP1_39)		; string?
  `(double_tag_predicate BVEC_TAG STR_HDR))

(define-sassy-macro (T_OP1_40)		; string-length
  `(indexed_structure_length BVEC_TAG STR_HDR EX_STRING_LENGTH 1))
		
(define-sassy-macro (T_OP1_41)		; vector?
  `(double_tag_predicate VEC_TAG VECTOR_HDR))


(define-sassy-macro (T_OP1_42)		; vector-length
  `(indexed_structure_length VEC_TAG VECTOR_HDR EX_VECTOR_LENGTH 0))

		
(define-sassy-macro (T_OP1_43)		; bytevector?
  `(double_tag_predicate BVEC_TAG BYTEVECTOR_HDR))

(define-sassy-macro (T_OP1_44)		; bytevector-length
  `(indexed_structure_length BVEC_TAG BYTEVECTOR_HDR EX_BYTEVECTOR_LENGTH 1))

(define-sassy-macro (T_OP2_45 x)		; bytevector-fill!
  `(begin
     ,(cond ((not (unsafe-code))
              `(locals (L0 L1 L2)
                 (label L0
                   (single_tag_test BVEC_TAG)
                   (jz short L2))
                 (label L1
                   (exception_continuable EX_BVFILL L0))
                 (label L2 
                   (loadr	SECOND ,x)
                   (test	SECOND_LOW fixtag_mask)
                   (jnz short L1))))
            (else
             `(loadr	SECOND ,x)))
     (mcall	M_BYTEVECTOR_LIKE_FILL)))

(define-sassy-macro (T_OP1_46)		; make-bytevector
  `(make_indexed_structure_byte -1 BYTEVECTOR_HDR  EX_MAKE_BYTEVECTOR))

(define-sassy-macro (T_OP1_47)		; procedure?
  `(begin
     (single_tag_test PROC_TAG)
     (setcc	z)))

(define-sassy-macro (T_OP1_48)		; procedure-length
  `(indexed_structure_length PROC_TAG EX_PROCEDURE_LENGTH  0))

(define-sassy-macro (T_OP1_49)		; make-procedure
  `(make_indexed_structure_word -1 PROC_TAG  PROC_HDR  EX_MAKE_PROCEDURE))
		
(define-sassy-macro (T_OP1_52)		; make-cell just maps to cons, for now
  `(begin
     (T_OP2_58 1)		; OPTIMIZEME: remove next instr by specializing
     (mov	(& (+ RESULT 4-PAIR_TAG)) dword UNSPECIFIED_CONST)))

(define-sassy-macro (T_OP1_54)		; cell-ref
  `(mov	RESULT (& (- RESULT PAIR_TAG))))

(define-sassy-macro (T_OP2_55 x)		; typetag-set!
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_TYPETAG_SET)))

(define-sassy-macro (T_OP2_56 x)		; eq?
  `(begin 
     ,(cond ((is_hwreg x)
             `(cmp	RESULT (REG,x)))
            (else
             `(cmp	RESULT (& (+ GLOBALS (G_REG,x))))))
     (setcc	z)))

(define-sassy-macro (T_OP2_57 x)		; eqv?
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_EQV)))
				
(define-sassy-macro (T_OP2_58 x)		; cons
  (cond ((inline-allocation)
         `(locals (L1 L2)
            (label L1
              (mov	TEMP (& (+ GLOBALS G_ETOP)))
              (add	TEMP 8)
              (cmp	TEMP CONT)
              (jle short L2)
              (mcall	M_MORECORE)
              (jmp short L1))
            (label L2
              (mov	(& (+ GLOBALS G_ETOP)) TEMP)
              (mov	(& (+ TEMP-8)) RESULT)
              (lea	RESULT (& (+ TEMP-8 PAIR_TAG)))
              ,(if (is_hwreg x)
                   `(mov	(& (+ RESULT (- PAIR_TAG) 4)) (REG,x))
                   `(begin
                      (loadr	TEMP ,x)
                      (mov	(& (+ RESULT (- PAIR_TAG) 4)) TEMP))))))
        (else
         `(begin
            (mov	(& (+ GLOBALS G_ALLOCTMP)) RESULT)
            (mov	RESULT 8)
            (mcall	M_ALLOC)
            (mov	TEMP (& (+ GLOBALS G_ALLOCTMP)))
            (mov	(& (+ RESULT)) TEMP)
            ,(if (is_hwreg x)
                 `(mov	(& (+ RESULT 4)) (REG,x))
                 `(begin
                    (loadr	TEMP ,x)
                    (mov	(& (+ RESULT 4)) TEMP)))
            (add	RESULT PAIR_TAG)))))
	
(define-sassy-macro (T_OP2_59 x)		; set-car!
  `(begin
     (single_tag_test_ex PAIR_TAG EX_SETCAR)
     ,@(cond 
        ((is_hwreg x)
         `((mov	(& (- RESULT PAIR_TAG)) (REG,x))
           (write_barrier -1 ,x)))
        (else
         `((loadr	SECOND ,x)
           (mov	(& (- RESULT PAIR_TAG)) SECOND)
           (write_barrier -1 -1))))))

(define-sassy-macro (T_OP2_60 x)		; set-cdr!
  `(begin
     (single_tag_test_ex PAIR_TAG EX_SETCDR)
     ,@(cond ((is_hwreg x)
              `((mov	(& (+ RESULT (- PAIR_TAG) wordsize)) (REG,x))
                (write_barrier -1 ,x)))
             (else
              `((loadr	SECOND ,x)
                (mov	(& (+ RESULT (- PAIR_TAG) wordsize)) SECOND)
                (write_barrier -1 -1))))))

(define-sassy-macro (T_OP2_61 x)		; +
  `(generic_arithmetic ,x add  sub  M_ADD))

(define-sassy-macro (T_OP2_62 x)		; -
  `(generic_arithmetic ,x sub  add  M_SUBTRACT))

(define-sassy-macro (T_OP2_63 x)		; *
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_MULTIPLY)))
	
(define-sassy-macro (T_OP2_64 x)		; /
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_DIVIDE)))
	
(define-sassy-macro (T_OP2_65 x)		; quotient
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_QUOTIENT)))

(define-sassy-macro (T_OP2_66 x)		; <
  `(generic_compare ,x l  M_NUMLT))
	
(define-sassy-macro (T_OP2_67 x)		; <=
  `(generic_compare ,x le  M_NUMLE))

(define-sassy-macro (T_OP2_68 x)		; =
  `(generic_compare ,x e  M_NUMEQ))

(define-sassy-macro (T_OP2_69 x)		; >
  `(generic_compare ,x g  M_NUMGT))

(define-sassy-macro (T_OP2_70 x)		; >=
  `(generic_compare ,x ge  M_NUMGE))

(define-sassy-macro (T_OP2_71 x)		; logand
  (cond ((not (unsafe-code))
         `(locals (L0 L1)
            (label L0
              (loadr	TEMP ,x)
              (or	TEMP RESULT)
              (test	TEMP_LOW fixtag_mask)
              (loadr	SECOND ,x)
              (jz short L1)
              (exception_continuable EX_LOGAND L0))
            (label L1 	
              (and	RESULT SECOND))))
        ((is_hwreg x)
         `(and	RESULT (REG,x)))
        (else
         `(and	RESULT (& (+ GLOBALS (G_REG,x)))))))

(define-sassy-macro (T_OP2_72 x)		; logior
  (cond ((not (unsafe-code))
         `(locals (L0 L1)
            (label L0
              (loadr	TEMP ,x)
              (or	TEMP RESULT)
              (test	TEMP_LOW fixtag_mask)
              (jz short L1)
              (loadr	SECOND ,x)
              (exception_continuable EX_LOGIOR L0))
            (label L1 	
              (mov	RESULT TEMP))))
        ((is_hwreg x)
         `(or	RESULT (REG,x)))
        (else
         `(or	RESULT (& (+ GLOBALS (G_REG,x)))))))

(define-sassy-macro (T_OP2_73 x)		; logxor
  (cond ((not (unsafe-code))
         `(locals (L0 L1)
            (label L0
              (loadr	TEMP ,x)
              (or	TEMP RESULT)
              (test	TEMP_LOW fixtag_mask)
              (loadr	SECOND ,x)
              (jz short L1)
              (exception_continuable EX_LOGXOR L0))
            (label L1 	
              (xor	RESULT SECOND))))
        ((is_hwreg x)
         `(xor	RESULT (REG,x)))
        (else
         `(xor	RESULT (& (+ GLOBALS (G_REG,x)))))))

(define-sassy-macro (T_OP2_74 x)		; lsh
  `(fixnum_shift ,x shl  EX_LSH))
	
(define-sassy-macro (T_OP2_75 x)		; rsha
  `(fixnum_shift ,x sar  EX_RSHA))

(define-sassy-macro (T_OP2_76 x)		; rshl
  `(fixnum_shift ,x shr  EX_RSHL))
	
(define-sassy-macro (T_OP2_77 x)		; rot
  (error 'T_OP2_rot "not implemented"))

(define-sassy-macro (T_OP2_78 x)		; string-ref
  `(begin
     (indexed_structure_ref ,x BVEC_TAG  STR_HDR  EX_STRING_REF  1)
     (shl	RESULT char_shift)
     (or	RESULT_LOW IMM_CHAR)))

(define-sassy-macro (T_OP3_79 x y)		; string-set!
  `(indexed_structure_set_char ,x ,y  BVEC_TAG  STR_HDR  EX_STRING_SET))

(define-sassy-macro (T_OP2_80 x)		; make-vector
  `(make_indexed_structure_word ,x VEC_TAG  VEC_HDR  EX_MAKE_VECTOR))

(define-sassy-macro (T_OP2_81 x)		; vector-ref
  `(indexed_structure_ref ,x VEC_TAG  VEC_HDR  EX_VREF  0))

(define-sassy-macro (T_OP2_82 x)		; bytevector-ref
  `(begin
     (indexed_structure_ref ,x BVEC_TAG  BYTEVECTOR_HDR  EX_BYTEVECTOR_REF  1)
     (shl	RESULT 2)))

(define-sassy-macro (T_OP2_83 x)		; procedure-ref
  `(indexed_structure_ref ,x PROC_TAG  EX_PROCEDURE_REF  0))

(define-sassy-macro (T_OP2_84 x)		; cell-set!
  (cond ((is_hwreg x)
         `(begin
            (mov	(& (+ RESULT-PAIR_TAG)) (REG,x))
            (write_barrier -1 ,x)))
        (else
         `(begin
            (loadr	SECOND ,x)
            (mov	(& (+ RESULT-PAIR_TAG)) SECOND)
            (write_barrier -1 -1)))))

(define-sassy-macro (T_OP2_85 x)		; char<?
  `(generic_char_compare ,x l  EX_CHARLT))

(define-sassy-macro (T_OP2_86 x)		; char<=?
  `(generic_char_compare ,x le  EX_CHARLE))

(define-sassy-macro (T_OP2_87 x)		; char=?
  `(generic_char_compare ,x e  EX_CHAREQ))

(define-sassy-macro (T_OP2_88 x)		; char>?
  `(generic_char_compare ,x g  EX_CHARGT))

(define-sassy-macro (T_OP2_89 x)		; char>=?
  `(generic_char_compare ,x ge  EX_CHARGE))

(define-sassy-macro (T_OP2_90 x)		; sys$partial-list->vector
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_PARTIAL_LIST2VECTOR)))

(define-sassy-macro (T_OP3_91 x y)		; vector-set!
  `(indexed_structure_set_word ,x ,y  VEC_TAG  VEC_HDR  EX_VECTOR_SET))

(define-sassy-macro (T_OP3_92 x y)		; bytevector-set!
  `(indexed_structure_set_byte ,x ,y  BVEC_TAG  BYTEVECTOR_HDR  EX_BYTEVECTOR_SET))

(define-sassy-macro (T_OP3_93 x y)		; procedure-set!
  `(indexed_structure_set_word ,x ,y  PROC_TAG  0  EX_PROCEDURE_SET))

(define-sassy-macro (T_OP1_94)		; bytevector-like?
  `(begin
     (single_tag_test BVEC_TAG)
     (setcc	z)))

(define-sassy-macro (T_OP1_95)		; vector-like?
  `(begin
     (single_tag_test VEC_TAG)
     (setcc	z)))

(define-sassy-macro (T_OP2_96 x)		; bytevector-like-ref
  `(begin
     (indexed_structure_ref ,x BVEC_TAG  EX_BYTEVECTOR_LIKE_REF  1)
     (shl	RESULT 2)))

(define-sassy-macro (T_OP3_97 x y)		; bytevector-like-set!
  `(indexed_structure_set_byte ,x ,y  BVEC_TAG  0  EX_BYTEVECTOR_LIKE_SET))

(define-sassy-macro (T_OP2_98 x)		; sys$bvlcmp
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_BYTEVECTOR_LIKE_COMPARE)))

(define-sassy-macro (T_OP2_99 x)		; vector-like-ref
  `(indexed_structure_ref ,x VEC_TAG  EX_VLREF  0))

(define-sassy-macro (T_OP3_100 x y)		; vector-like-set!
  `(indexed_structure_set_word ,x ,y  VEC_TAG  0  EX_VECTOR_LIKE_SET))

(define-sassy-macro (T_OP1_101)		; vector-like-length
  `(indexed_structure_length VEC_TAG EX_VECTOR_LIKE_LENGTH  0))

(define-sassy-macro (T_OP1_102)		; bytevector-like-length
  `(indexed_structure_length BVEC_TAG EX_BYTEVECTOR_LIKE_LENGTH  1))

(define-sassy-macro (T_OP2_103 x)		; remainder
  `(begin
     (loadr	SECOND ,x)
     (mcall	M_REMAINDER)))

(define-sassy-macro (T_OP1_104)		; petit-patch-boot-code
  `(mcall	M_PETIT_PATCH_BOOT_CODE))

(define-sassy-macro (T_OP1_105)		; syscall
  `(mcall	M_SYSCALL))

(define-sassy-macro (T_OP1_106)		; creg
  `(mcall	M_CREG))

(define-sassy-macro (T_OP1_107)		; creg-set!
  `(mcall	M_CREG_SET))

(define-sassy-macro (T_OP1_108)		; gc-counter
  `(mov	RESULT (& (+ GLOBALS G_GC_CNT))))

(define-sassy-macro (T_OP2_109 x)		; make-string
  `(make_indexed_structure_byte ,x STR_HDR  EX_MAKE_STRING))

(define-sassy-macro (T_OP2IMM_128 x)		; typetag-set!
  `(begin
     (const2regf SECOND ,x)
     (mcall	M_TYPETAG_SET)))

(define-sassy-macro (T_OP2IMM_129 x)		; eq?
  `(begin
     (cmp	RESULT ,x)
     (setcc	z)))

(define-sassy-macro (T_OP2IMM_130 x)		; +
  `(locals (L1 L2)
     (test	RESULT_LOW fixtag_mask)
     (jnz short L1)
     (add	RESULT ,x)
     (jno short L2)
     (sub	RESULT ,x)
     (label L1
       (mov	SECOND ,x)
       (mcall	M_ADD))
     (label L2)))


(define-sassy-macro (T_OP2IMM_131 x)		; -
  `(locals (L1 L2)
     (test	RESULT_LOW fixtag_mask)
     (jnz short L1)
     (sub	RESULT ,x)
     (jno short L2)
     (add	RESULT ,x)
     (label L1
       (mov	SECOND ,x)
       (mcall	M_SUBTRACT))
     (label L2)))

(define-sassy-macro (T_OP2IMM_132 x)		; <
  `(generic_imm_compare ,x l  M_NUMLT))

(define-sassy-macro (T_OP2IMM_133 x)		; <=
  `(generic_imm_compare ,x le  M_NUMLE))

(define-sassy-macro (T_OP2IMM_134 x)		; =
  `(generic_imm_compare ,x e  M_NUMEQ))

(define-sassy-macro (T_OP2IMM_135 x)		; >
  `(generic_imm_compare ,x g  M_NUMGT))

(define-sassy-macro (T_OP2IMM_136 x)		; >=
  `(generic_imm_compare ,x ge  M_NUMGE))

(define-sassy-macro (T_OP2IMM_137 x)		; char<?
  `(generic_char_imm_compare ,x l  EX_CHARLT))

(define-sassy-macro (T_OP2IMM_138 x)		; char<=?
  `(generic_char_imm_compare ,x le  EX_CHARLE))

(define-sassy-macro (T_OP2IMM_139 x)		; char=?
  `(generic_char_imm_compare ,x e  EX_CHAREQ))

(define-sassy-macro (T_OP2IMM_140 x)		; char>?
  `(generic_char_imm_compare ,x g  EX_CHARGT))

(define-sassy-macro (T_OP2IMM_141 x)		; char>=?
  `(generic_char_imm_compare ,x ge  EX_CHARGE))

;;; The following five are probably a waste of effort.

(define-sassy-macro (T_OP2IMM_142 x)		; string-ref
  `(begin
     (indexed_structure_ref_imm ,x BVEC_TAG  STR_HDR  EX_STRING_REF  1)
     (shl	RESULT char_shift)
     (or	RESULT_LOW IMM_CHAR)))

(define-sassy-macro (T_OP2IMM_143 x)		; vector-ref
  `(indexed_structure_ref_imm ,x VEC_TAG  VECTOR_HDR  EX_VECTOR_REF  0))

(define-sassy-macro (T_OP2IMM_144 x)		; bytevector-ref
  `(begin
     (indexed_structure_ref_imm ,x BVEC_TAG  BYTEVECTOR_HDR  EX_BYTEVECTOR_REF  1)
     (shl	RESULT 2)))
	
(define-sassy-macro (T_OP2IMM_145 x)		; bytevector-like-ref
  `(begin
     (indexed_structure_ref_imm ,x BVEC_TAG  EX_BYTEVECTOR_LIKE_REF  1)
     (shl	RESULT 2)))

(define-sassy-macro (T_OP2IMM_146 x)		; vector-like-ref
  `(indexed_structure_ref_imm ,x VEC_TAG  EX_VECTOR_LIKE_REF  0))

(define-sassy-macro (T_OP1_200)		; most-positive-fixnum
  `(const2regf RESULT #x7FFFFFFC))

(define-sassy-macro (T_OP1_201)		; most-negative-fixnum
  `(const2regf RESULT #x80000000))

(define-sassy-macro (T_OP2_202 x)		; fx+
  `(fixnum_arithmetic ,x add  sub  EX_FXADD))

(define-sassy-macro (T_OP2_203 x)		; fx-
  `(fixnum_arithmetic ,x sub  add  EX_FXSUB))

(define-sassy-macro (T_OP1_204)		; fx--
  (cond ((not (unsafe-code))
         (label L0
           (test	RESULT_LOW fixtag_mask)
           (jnz short L1)
           (neg	RESULT)
           (jno short L2))
	;; No need to undo: RESULT is unchanged
         (label L1
           (exception_continuable EX_FXNEG L0))
         (label L2))
        (else
         `(neg	RESULT))))

(define-sassy-macro (T_OP2_205 x)              ; fx*
  (cond ((not (unsafe-code))
         `(locals (L0 L1 L2)
            (label L0 	
              (loadr	TEMP ,x)
              (or	TEMP RESULT)
              (test	TEMP_LOW fixtag_mask)
              (jnz short L1)
              (loadr	TEMP ,x)
              (sar	TEMP 2)
              (imul	TEMP RESULT)
              (jno short L2))
            (label L1
              (loadr	TEMP ,x)
              (exception_continuable EX_FXMUL L0))
            (label L2 
              (mov	RESULT TEMP))))
        (else
         `(begin
            (shr	RESULT)
            ,(if (is_hwreg x)
                 `(imul	RESULT (REG,x))
                 `(imul	RESULT (& (+ GLOBALS (G_REG,x)))))))))
	
(define-sassy-macro (T_OP2_206 x)		; fx=
  `(fixnum_compare ,x e  EX_FXEQ))

(define-sassy-macro (T_OP2_207 x)		; fx<
  `(fixnum_compare  ,x l  EX_FXLT))

(define-sassy-macro (T_OP2_208 x)		; fx<=
  `(fixnum_compare  ,x le  EX_FXLE))

(define-sassy-macro (T_OP2_209 x)		; fx>
  `(fixnum_compare  ,x g  EX_FXGT))

(define-sassy-macro (T_OP2_210 x)		; fx>=
  `(fixnum_compare  ,x ge  EX_FXGE))

; Changed T_OP2_2{11,12,13} to OP1.
; Do we refer to these as OP2 anywhere?
(define-sassy-macro (T_OP1_211)		; fxzero?
  `(begin
     ,@(if (not (unsafe-code))
           (list
            (locals (L0 L1)
              (label L0
                (test	RESULT_LOW fixtag_mask)
                (jz short L1)
                (exception_continuable EX_FXZERO L0))
              (label L1 )))
           (list))
     (test	RESULT RESULT)
     (setcc	z)))

(define-sassy-macro (T_OP1_212)		; fxpositive?
  `(begin
     ,@(if (not (unsafe-code))
           (list 
            `(locals (L0 L1)
               (label L0
                 (test	RESULT_LOW fixtag_mask)
                 (jz short L1)
                 (exception_continuable EX_FXPOSITIVE L0))
               (label L1)))
           (list))
     (cmp	RESULT 0)
     (setcc	g)))

(define-sassy-macro (T_OP1_213)		; fxnegative?
  `(begin
     ,@(if (not (unsafe-code))
           (list 
            `(locals (L0 L1)
               (label L0
                 (test	RESULT_LOW fixtag_mask)
                 (jz short L1)
                 (exception_continuable EX_FXNEGATIVE L0))
               (label L1)))
           (list))
     (cmp	RESULT 0)
     (setcc	l)))

(define-sassy-macro (fixnum_imm_arithmetic x y z ex)
  (if (not (unsafe-code))
      `(locals (L0 L1 L2)
         (label L0
           (const2regf TEMP ,x)
           (or	TEMP RESULT)
           (test	TEMP_LOW fixtag_mask)
           (jnz short L1)
           (const2regf TEMP ,x)
           (,y	RESULT TEMP)
           (jno short L2)
           (,z	RESULT TEMP))
         (label L1
           (exception_continuable ,ex L0))	; second is tmp so 2nd arg is in place
         (label L2))
      `(begin
         (const2regf TEMP ,x)
         (,y	RESULT TEMP))))
	
(define-sassy-macro (T_OP2IMM_250 x)           ; fx+
  `(fixnum_imm_arithmetic ,x add  sub  EX_FXADD))

(define-sassy-macro (T_OP2IMM_251 x)           ; fx-
  `(fixnum_imm_arithmetic ,x sub  add  EX_FXSUB))

;;; fixnum_imm_compare const, cc, ex
(define-sassy-macro (fixnum_imm_compare x y z)
  `(locals (L0 L1)
     ,@(if (not (unsafe-code))
           (list `(label L0 
                    (const2regf TEMP ,x)
                    (or	TEMP RESULT)
                    (test	TEMP_LOW fixtag_mask)
                    (jz short L1)
                    (const2regf TEMP ,x)
                    (exception_continuable ,z L0))	; second is tmp so 2nd arg is in place
                 (label L1))
           (list))
     (const2regf TEMP ,x)
     (cmp	RESULT 	TEMP)
     (setcc ,y)))

(define-sassy-macro (T_OP2IMM_253 x)		; fx=
  `(fixnum_imm_compare ,x e  EX_FXEQ))

(define-sassy-macro (T_OP2IMM_254 x)		; fx<
  `(fixnum_imm_compare  ,x l  EX_FXLT))

(define-sassy-macro (T_OP2IMM_255 x)		; fx<=
  `(fixnum_imm_compare  ,x le  EX_FXLE))

(define-sassy-macro (T_OP2IMM_256 x)		; fx>
  `(fixnum_imm_compare  ,x g  EX_FXGT))

(define-sassy-macro (T_OP2IMM_257 x)		; fx>=
  `(fixnum_imm_compare  ,x ge  EX_FXGE))

;;; Unsafe/trusted primitives

(define-sassy-macro (T_OP1_401)		; vector-length:vec
  `(begin
     (mov	RESULT (& (+ (- RESULT VEC_TAG))))
     (shr	RESULT 8)))

(define-sassy-macro (T_OP2_402 x)		; vector-ref:trusted
  `(begin
     (add	RESULT wordsize-VEC_TAG)
     ,@(if (is_hwreg x)
           `((mov	RESULT (& (+ RESULT (REG,x)))))
           `((loadr	TEMP ,x)
             (mov	RESULT (& (+ RESULT TEMP)))))))

(define-sassy-macro (T_OP3_403 x y)		; vector-set!:trusted
  `(do_indexed_structure_set_word ,x ,y VEC_TAG))

(define-sassy-macro (T_OP1_404)		; car:pair
  `(mov	RESULT (& (+ RESULT-PAIR_TAG))))

(define-sassy-macro (T_OP1_405)		; cdr:pair
  `(mov	RESULT (& (+ RESULT-PAIR_TAG wordsize))))

(define-sassy-macro (T_OP2_406 x)		; =:fix:fix
  `(trusted_fixnum_compare ,x e))

(define-sassy-macro (T_OP2_407 x)		; <:fix:fix
  `(trusted_fixnum_compare ,x l))


(define-sassy-macro (T_OP2_408 x)		; <=:fix:fix
  `(trusted_fixnum_compare ,x le))

(define-sassy-macro (T_OP2_409 x)		; >=:fix:fix
  `(trusted_fixnum_compare ,x ge))

(define-sassy-macro (T_OP2_410 x)		; >:fix:fix
  `(trusted_fixnum_compare ,x g))

(define-sassy-macro (T_OP2IMM_450 x)		; vector-ref:trusted
  `(mov	RESULT (& (+ RESULT (wordsize-VEC_TAG) ,x))))

(define-sassy-macro (T_OP2IMM_451 x)		; =:fix:fix
  `(begin
     (cmp	RESULT ,x)
     (setcc	e)))

(define-sassy-macro (T_OP2IMM_452 x)		; <:fix:fix
  `(begin
     (cmp	RESULT ,x)
     (setcc	l)))

(define-sassy-macro (T_OP2IMM_453 x)		; <=:fix:fix
  `(begin
     (cmp	RESULT ,x)
     (setcc	le)))

(define-sassy-macro (T_OP2IMM_454 x)		; >:fix:fix
  `(begin
     (cmp	RESULT ,x)
     (setcc	g)))

(define-sassy-macro (T_OP2IMM_455 x)		; >=:fix:fix
  `(begin
     (cmp	RESULT ,x)
     (setcc	ge)))

;;; Introduced by representation inference.  Trusted.

(define-sassy-macro (T_OP2_500 x)		; +:idx:idx
  (if (is_hwreg x)
      `(add	RESULT (REG,x))
      `(add	RESULT (& (+ GLOBALS (G_REG,x))))))

(define-sassy-macro (T_OP2_501 x)		; +:fix:fix
  `(locals (L1)
     (loadr	TEMP ,x)
     (add	RESULT TEMP)
     (jno short L1)
     (sub	RESULT TEMP)
     (mcall	M_ADD)                          ; second is temp so 2nd arg is in place
     (label L1)))
	
(define-sassy-macro (T_OP2_502 x)		; -:idx:idx
  (if (is_hwreg x)
      `(sub	RESULT (REG ,x))
      `(sub	RESULT (& (+ GLOBALS (G_REG,x))))))

(define-sassy-macro (T_OP2_503 x)		; -:fix:fix
  `(locals (L1)
     (loadr	TEMP ,x)
     (sub	RESULT TEMP)
     (jno short L1)
     (add	RESULT TEMP)
     (mcall	M_SUBTRACT)	; second is temp so 2nd arg is in place
     (label L1)))

(define-sassy-macro (T_OP2IMM_520 x)		; +:idx:idx
  `(add	RESULT ,x))

(define-sassy-macro (T_OP2IMM_521 x)		; +:fix:fix
  `(locals (L1)
     (add	RESULT ,x)
     (jno short L1)
     (sub	RESULT ,x)
     (mov	SECOND ,x)
     (mcall	M_ADD)
     (label L1)))

(define-sassy-macro (T_OP2IMM_522 x)		; -:idx:idx
  `(sub	RESULT ,x))

(define-sassy-macro (T_OP2IMM_523 x)		; -:fix:fix
  `(locals (L1)
     (sub	RESULT ,x)
     (jno short L1)
     (add	RESULT ,x)
     (mov	SECOND ,x)
     (mcall	M_SUBTRACT)
     (label L1)))


;;; Experimental stuff below this line, we need more than this to support
;;; peephole optimization well.

(define-sassy-macro (T_OP1_612 x)		; internal:branchf-zero?
  `(locals (L1 L2)
     (timer_check)
     (test	RESULT_LOW fixtag_mask)
     (jz short L1)
     (mcall	M_ZEROP)
     (cmp	RESULT_LOW FALSE_CONST)
     (je	(t_label ,x))
     (jmp short L2)
     (label L1 	
       (cmp	RESULT 0)
       (jne	(t_label ,x)))
     (label L2)))

(define-sassy-macro (OP2IMM_BRANCHF_lessthan x y)
  `(locals (L1 L2)
     (test	RESULT_LOW fixtag_mask)
     (jz short L1)
     (const2regf SECOND ,x)
     (mcall	M_NUMLT)
     (cmp	RESULT_LOW FALSE_CONST)
     (je	(t_label ,y))
     (jmp short L2)
     (label L1 	
       (cmp	RESULT ,x)
       (jge	(t_label ,y)))
     (label L2)))
	
;;; eof

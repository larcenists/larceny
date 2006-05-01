;;; NASM/i386 macros for the MacScheme instruction set.
;;;
;;; $Id$
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

%ifdef UNSAFE_CODE
 %ifndef USAFE_GLOBALS
  %define UNSAFE_GLOBALS
 %endif
%endif
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Handy macros for this and that.  More fundamental stuff is
;;; defined in i386-machine.ah

%define fixtag_mask	    3
%define tag_mask            7
%define hdr_shift           8
%define char_shift	    16
%define is_hwreg(n)         ((n) >= FIRST_HWREG && (n) <= LAST_HWREG)
%define fixnum(n)           ((n)<<2)
%define char(n)	            (((n)<<char_shift)|IMM_CHAR)
%define roundup4(x)	    (((x)+3)&~3)
%define roundup8(x)	    (((x)+7)&~7)
%define words2bytes(n)      ((n)*4)
%define stkslot(n)          (CONT+STK_REG0+words2bytes(n))
%define framesize(n)        roundup8(wordsize+STK_OVERHEAD+words2bytes(n))
%define recordedsize(n)     (STK_OVERHEAD+words2bytes(n))
%define t_label(l)          l
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Utility macros for MAL instruction definitions

%macro mcall 1
	call	[GLOBALS+%1]
	align	code_align
%endmacro

;;; loadr targetreg, regno
;;; 	load HW register targetreg from VM register regno
	
%macro loadr 2
%if is_hwreg(%2)
	mov	%1, REG%2
%else
	mov	%1, [GLOBALS+G_REG%2]
%endif
%endmacro

;;; storer regno, sourcereg
;;;     store VM register regno from HW register sourcereg
;;;     Does not destroy sourcereg

%macro storer 2
%if is_hwreg(%1)
	mov	REG%1, %2
%else
	mov	[GLOBALS+G_REG%1], %2
%endif
%endmacro

;;; loadc hwreg, slot
;;;	Load constant vector element 'slot' into hwreg
	
%macro loadc 2
	loadr	%1, 0
	mov	%1, [%1-PROC_TAG+PROC_CONSTVECTOR]
	mov	%1, [%1-VEC_TAG+words2bytes((%2)+1)]
%endmacro

;;; write_barrier r1 r2
;;;	Move values from hardware registers r1 and r2 to RESULT 
;;; 	and SECOND and perform a write barrier.  r1 and r2 may 
;;; 	be -1, in which case the value must already be in RESULT 
;;; 	and SECOND.
;;;
;;;     For INLINE_ASSIGNMENT, test the r2 value and skip the barrier
;;;     if the low bit is not 1.

%macro write_barrier 2
%ifdef INLINE_ASSIGNMENT
 %if %2 != -1 && is_hwreg(%2)
	test	REG%2, 1
	jz short %%L0
	mov	SECOND, REG%2
 %else
  %if %2 != -1
	mov	SECOND, REG%2
  %endif
	test	SECOND, 1
	jz short %%L0
 %endif
 %if %1 != -1
	mov	RESULT, REG%1
 %endif
	mcall	M_PARTIAL_BARRIER
%%L0:
%else
 %if %1 != -1
	mov	RESULT, REG%1
 %endif
 %if %2 != -1
	mov	SECOND, REG%2
 %endif
	mcall	M_FULL_BARRIER
%endif
%endmacro
	
;;; timer_check
;;;	decrement timer and take interrupt if zero

%macro timer_check 0
	dec	dword [ GLOBALS+G_TIMER ]
	jnz short %%L1
	mcall	M_TIMER_EXCEPTION
%%L1:
%endmacro

;;; exception_noncontinuable excode
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is noncontinuable
	
%macro exception_noncontinuable	1
	call	[GLOBALS+M_EXCEPTION]
	dw	%1
%endmacro
		
;;; exception_continuable excode restart
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is continuable at the address
;;;	of the second argument.  Smallest code probably results
;;;	from just inserting a jump.
;;;
;;;	Moving the exception code to globals takes 8 bytes using
;;;		mov	dword [GLOBALS+offset], %1
;;;     even if offset is short.  Placing the exception code in two 
;;; 	bytes at the return address saves six bytes, though the 
;;; 	handler code must adjust the return address.
;;;
;;;	Important that M_EXCEPTION is at short offset from
;;;	globals, to save 3 bytes!  (It can be a negative offset.)

%macro exception_continuable 2
	call	[GLOBALS+M_EXCEPTION]
	dw	%1
	align	code_align
	jmp	%2
%endmacro

;;; begin_codevector name
;;; 	Define a code vector, just raw code
	
%macro begin_codevector 1
	section	.text
	align	code_align
%1:
%endmacro

;;; end_codevector name
;;;	Terminate a codevector started by begin_codevector

%macro end_codevector 1
end_codevector_%1:
%endmacro

;;; alloc
;;;	Given fixnum number of words in RESULT, allocate
;;;	a structure of that many bytes and leave a raw pointer
;;;	in RESULT.

%macro alloc 0
%ifdef INLINE_ALLOCATION
%%L1:	mov	TEMP, [GLOBALS+G_ETOP]
	add	TEMP, RESULT	; allocate
	add	TEMP, 4		;   and
	and	TEMP, -8	;     round up to 8-byte boundary
	cmp	TEMP, CONT
	jle short %%L2
	mcall	M_MORECORE
	jmp short %%L1
%%L2:	mov	RESULT, [GLOBALS+G_ETOP]
	mov	[GLOBALS+G_ETOP], TEMP
%else
	mcall	M_ALLOC
%endif
%endmacro

;;; const2reg hwreg const
;;; Move a constant to a register *without changing the flags*

%macro const2reg 2
	mov	%1, %2		; 5 bytes
%endmacro

;;; const2regf hwreg const
;;; Move a constant to a register, possibly killing the flags
;;; Makes for smaller code size.

%macro const2regf 2
%if %2==0
	xor	%1, %1		; 2 bytes
%elif %2==1
	xor	%1, %1		; 3 bytes
	inc	%1
%elif %2==-1
	xor	%1, %1		; 3 bytes
	dec	%1
%else
	mov	%1, %2		; 5 bytes
%endif
%endmacro
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; MacScheme machine instruction set

%macro T_ALIGN 1
	align	%1
%endmacro

%macro T_CONT 0
	;; No-op
%endmacro
	
%macro T_LABEL 1
t_label(%1):
%endmacro

;;; CONST is broken up into two instructions depending on the
;;; type of the constant.  The argument to T_CONST_IMM is a bitpattern
;;; or symbolic constant name, to T_CONST_CONSTVECTOR it is the
;;; constant vector index.
	
%macro T_CONST_IMM 1
	const2regf RESULT, %1
%endmacro

%macro T_CONST_CONSTVECTOR 1
	loadc	RESULT, %1
%endmacro

%macro T_CONST_SETREG_IMM 2
%if is_hwreg(%2)
	mov	REG%2, %1
%else	
	mov	TEMP, %1
	storer	%2, TEMP
%endif
%endmacro

%macro T_CONST_SETREG_CONSTVECTOR 2
%if is_hwreg(%2)
	loadc	REG%2, %1
%else
	loadc	TEMP, %1
	storer	%2, TEMP
%endif
%endmacro

;;; OPTIMIZEME:	if #!undefined fit in a byte, then we could do a byte
;;; compare here, at least.  (Why does it not fit in a byte?)

%macro T_GLOBAL 1
%%L0:	loadc	TEMP, %1
	mov	RESULT, [TEMP-PAIR_TAG]
%ifndef UNSAFE_GLOBALS
	cmp	RESULT, UNDEFINED_CONST
	jne short %%L1
	mov	RESULT, TEMP
	mcall	M_GLOBAL_EX
	jmp short %%L0
%%L1:
%endif
%endmacro

%macro T_SETGLBL 1
	mov	SECOND, RESULT
	loadc	RESULT, %1
	mov	[RESULT-PAIR_TAG], SECOND
	write_barrier -1, -1
%endmacro

%macro T_LEXICAL 2
	loadr	TEMP, 0		; We know R0 is not a HWREG
%assign ribno 0
%rep 65536
  %if ribno == %1
    %exitrep
  %endif
	mov	TEMP, [TEMP-PROC_TAG+PROC_REG0]
  %assign ribno ribno+1
%endrep
	mov	RESULT, [TEMP-PROC_TAG+PROC_REG0+words2bytes(%2)]
%endmacro

%macro T_SETLEX 2
	loadr	TEMP, 0		; We know R0 is not a HWREG
%assign ribno 0
%rep 65536
  %if ribno == %1
    %exitrep
  %endif
	mov	TEMP, [TEMP-PROC_TAG+PROC_REG0]
  %assign ribno ribno+1
%endrep
	mov	[TEMP-PROC_TAG+PROC_REG0+words2bytes(%2)], RESULT
%endmacro
	
%macro T_STACK 1
	mov	RESULT, [stkslot(%1)]
%endmacro

%macro T_SETSTK 1
	mov	[stkslot(%1)], RESULT
%endmacro

%macro T_LOAD 2
%if is_hwreg(%1)
	mov	REG%1, [stkslot(%2)]
%else
	mov	TEMP, [stkslot(%2)]
	storer	%1, TEMP
%endif
%endmacro

%macro T_STORE 2
%if is_hwreg(%1)
	mov	[stkslot(%2)], REG%1
%else
	loadr	TEMP, %1
	mov	[stkslot(%2)], TEMP
%endif
%endmacro

%macro T_REG 1
	loadr RESULT, %1
%endmacro
	
;;; Does not destroy RESULT.  The peephole optimizer uses that fact.
%macro T_SETREG 1
	storer	%1, RESULT
%endmacro

%macro T_MOVEREG 2
%if is_hwreg(%1)
	storer	%2, REG%1
%elif is_hwreg(%2)
	loadr	REG%2, %1
%else
	loadr	TEMP, %1
	storer	%2, TEMP
%endif
%endmacro

%macro init_closure 1
%if %1 >= LASTREG
	mov	[GLOBALS+G_STKP], CONT       ; Need a working register!
	mov	[GLOBALS+G_RESULT], RESULT   ; Save for later
	add	RESULT, PROC_REG0+words2bytes(LASTREG)
	loadr	CONT, 31
%%L1:	mov	TEMP, [CONT-PAIR_TAG]
	mov	[RESULT], TEMP
	add	RESULT, wordsize
	mov	CONT, [CONT-PAIR_TAG+wordsize]
	cmp	CONT, NIL_CONST
	jne short %%L1
	mov	CONT, [GLOBALS+G_STKP]
	mov	RESULT, [GLOBALS+G_RESULT]
  %assign regno LASTREG-1
%else
  %assign regno %1
%endif
%rep LASTREG
  %if regno < 0
    %exitrep
  %endif
  %if is_hwreg(regno)
	mov	[RESULT+PROC_REG0+words2bytes(regno)], REG %+ regno
  %else
	loadr	TEMP, regno
	mov	[RESULT+PROC_REG0+words2bytes(regno)], TEMP
  %endif
  %assign regno regno-1
%endrep
	add	RESULT_LOW, PROC_TAG
%endmacro

%macro T_LAMBDA 3
	;; arguments are codevector name, constant vector offset, and n
	const2regf RESULT, fixnum(PROC_HEADER_WORDS+PROC_OVERHEAD_WORDS+%3+1)
	alloc
	mov	dword [RESULT], (words2bytes(PROC_OVERHEAD_WORDS+%3+1) << 8) | PROC_HDR
	;; Adjust only if code is in bytevectors!
	;mov	dword [RESULT+PROC_CODEVECTOR_NATIVE], %1 + BVEC_TAG
	mov	dword [RESULT+PROC_CODEVECTOR_NATIVE], %1
	loadc	TEMP, %2
	mov	[RESULT+PROC_CONSTVECTOR], TEMP
	init_closure %3
%endmacro
	
%macro T_LEXES 1
	;; argument is n
	const2regf RESULT, fixnum(PROC_HEADER_WORDS+PROC_OVERHEAD_WORDS+%1+1)
	alloc
	mov	dword [RESULT], (words2bytes(PROC_OVERHEAD_WORDS+%1+1) << 8) | PROC_HDR
	loadr	TEMP, 0
	mov	TEMP, [TEMP-PROC_TAG+PROC_CODEVECTOR_NATIVE]
	mov	dword [RESULT+PROC_CODEVECTOR_NATIVE], TEMP
	loadr	TEMP, 0
	mov	TEMP, [TEMP-PROC_TAG+PROC_CONSTVECTOR]
	mov	dword [RESULT+PROC_CONSTVECTOR], TEMP
	init_closure %1
%endmacro

%macro T_ARGSEQ 1
%ifndef UNSAFE_CODE
%%L0:	
	cmp	RESULT, fixnum(%1)
	je short %%L1
	mov	dword [GLOBALS+G_SECOND], fixnum(%1)
	mcall	M_ARGC_EX
	jmp	%%L0
%%L1:
%endif
%endmacro

%macro T_ARGSGE 1
	const2regf SECOND, fixnum(%1)
%ifndef UNSAFE_CODE
 %if %1 > 0
%%L0:	cmp	RESULT, SECOND
	jge short %%L1
	mov	dword [GLOBALS+G_SECOND], fixnum(%1)
	mcall	M_ARGC_EX
	jmp	%%L0
%%L1:
 %endif
%endif
	mcall	M_VARARGS
%endmacro

;;; Note the millicode for M_INVOKE_EX is used to check for timer
;;; exception as well, and must check the timer first.

%macro T_INVOKE 1
%ifdef UNSAFE_CODE
	timer_check
	storer	0, RESULT
	mov	TEMP, RESULT
	const2regf RESULT, fixnum(%1)
	jmp	[TEMP-PROC_TAG+PROC_CODEVECTOR_NATIVE]
%else
	dec	dword [GLOBALS+G_TIMER]
	jnz short %%L1
%%L0:	mcall	M_INVOKE_EX
%%L1:	lea	TEMP, [RESULT+(8-PROC_TAG)]
	test	TEMP_LOW, tag_mask
	jnz short %%L0
	;; Observe TEMP points to proc+8 here and that if we were
	;; really perverse we would lay the procedure out so that
	;; the codevector is stored at offset 4, reducing the size
	;; of the JMP instruction below.
	storer	0, RESULT
	const2regf RESULT, fixnum(%1)
	jmp	[TEMP-8+PROC_CODEVECTOR_NATIVE]
%endif
%endmacro

;;; Introduced by peephole optimization.
;;; 
;;; The trick here is that the tag check for procedure-ness will
;;; catch undefined variables too.  So there is no need to see
;;; if the global has an undefined value, specifically.  The
;;; exception handler figures out the rest.

%macro T_GLOBAL_INVOKE 2
%ifdef UNSAFE_GLOBALS
	T_GLOBAL %1
	T_INVOKE %2
%else
	loadc	RESULT, %1		; global cell
%%L2:	mov	TEMP, [RESULT-PAIR_TAG]	;   dereference
	inc	TEMP			; really TEMP += PROC_TAG-8
	test	TEMP_LOW, tag_mask	; tag test
	jz short %%L0
%%L1:	mcall	M_GLOBAL_INVOKE_EX	; RESULT has global cell (always)
	jmp	short %%L2		; Since TEMP is dead following timer interrupt
%%L0:	dec	dword [GLOBALS+G_TIMER]	; timer
	jz short %%L1			;   test
	dec	TEMP			; undo ptr adjustment
	storer	0, TEMP			; save proc ptr
	const2regf RESULT, fixnum(%2)	; argument count
	jmp	[TEMP-PROC_TAG+PROC_CODEVECTOR_NATIVE]
%endif
%endmacro
	
;;; Allocate the frame but initialize only the basic slots
;;; and any pad words.  Leave RESULT clear at the end; this
;;; fact is used by T_SAVE1 below.

%macro T_SAVE0 1
%%L0:	sub	CONT, framesize(%1)
	cmp	CONT, [GLOBALS+G_ETOP]
	jge short %%L1
	add	CONT, framesize(%1)
	mcall	M_STKOFLOW
	jmp	%%L0
%%L1:	mov	dword [CONT], recordedsize(%1)
	;; Not necessary to store reg0 here, this is handled
	;; explicitly by the generated code.
	xor	RESULT, RESULT
	mov	dword [CONT+STK_RETADDR], RESULT
%if (framesize(%1)-recordedsize(%1))==8
	;; We have a pad word at the end -- clear it
	mov	dword [stkslot(%1+1)], RESULT
%endif
%endmacro

;;; Initialize the numbered slot to the value of RESULT.
;;; Using RESULT is probably OK because it is almost certainly 0
;;; after executing T_SAVE0 and only T_STORE instructions
;;; after that.

%macro T_SAVE1 1
	mov	dword [stkslot(%1)], RESULT
%endmacro

;;; T_SAVE may still be emitted by the assembler when peephole 
;;; optimization is disabled.

%macro T_SAVE 1
	T_SAVE0 %1
%assign slotno 1
%rep	65536
  %if slotno > %1
    %exitrep
  %endif
	T_SAVE1 slotno
  %assign slotno slotno+1
%endrep
%endmacro

%macro T_SETRTN 1
	mov	dword [CONT+STK_RETADDR], t_label(%1)
%endmacro

%macro T_RESTORE 1
%assign slotno 0
%rep	65536
  %if slotno > %1
    %exitrep
  %endif
  %if is_hwreg(slotno)
	mov	REG %+ slotno, dword [stkslot(slotno)]
  %else
	mov	TEMP, dword [stkslot(slotno)]
	mov	[GLOBALS+G_REG %+ slotno], TEMP
  %endif
  %assign slotno slotno+1
%endrep
%endmacro

%macro T_POP 1
	add	CONT, framesize(%1)
%endmacro
	
%macro T_POPSTK 0
%error T_POPSTK not implemented -- students only
%endmacro

%macro T_RETURN 0
	jmp	[CONT+STK_RETADDR]
%endmacro
	
%macro T_APPLY 2
	timer_check
	loadr	TEMP, %2
	mov	[GLOBALS+G_THIRD], TEMP
	loadr	SECOND, %1
	mcall	M_APPLY
	loadr	TEMP, 0
	mov	TEMP, [TEMP-PROC_TAG+PROC_CODEVECTOR_NATIVE]
	jmp	TEMP
%endmacro

%macro T_NOP 0
	;; Nothing
%endmacro

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; in this implementation)
	
%macro T_JUMP 3
	timer_check
%if %1 > 0
	loadr	TEMP, 0		; We know R0 is not a HWREG
%assign ribno 0
%rep 65536
  %if ribno == %1
    %exitrep
  %endif
	mov	TEMP, [TEMP-PROC_TAG+PROC_REG0]
  %assign ribno ribno+1
%endrep
	storer	0, TEMP
%endif
	jmp	%3
%endmacro

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

%macro T_SKIP 1	
	jmp	t_label(%1)
%endmacro

%macro T_SKIPF 1	
	cmp	RESULT_LOW, FALSE_CONST
	je	t_label(%1)
%endmacro

%macro T_BRANCH 1	
	dec	dword [ GLOBALS+G_TIMER ]
	jnz	t_label(%1)
	mcall	M_TIMER_EXCEPTION
	jmp	t_label(%1)
%endmacro

%macro T_BRANCHF 1
	timer_check
	cmp	RESULT_LOW, FALSE_CONST
	je	t_label(%1)
%endmacro

%macro T_CHECK 4
	cmp	RESULT_LOW, FALSE_CONST
	je	t_label(%4)
%endmacro

;; Call handler for noncontinuable exception with RESULT,
;; SECOND, and THIRD defined.

%macro T_TRAP 4
	;; Order matters here, because SECOND is TEMP and
	;; may be destroyed by loading of THIRD
%if %1 != 0
	loadr	RESULT, %1
%endif
%if %3 != 0
	;; OPTIMIZEME: optimize for case when %3 is HW reg
	;; (this will however have almost no impact)
	loadr	TEMP, %3
	mov	[GLOBALS+G_THIRD], TEMP
%endif
%if %2 != 0
	loadr	SECOND, %2
%endif
	exception_noncontinuable %4
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Helper macros for primitive operations

;;; setcc cc
;;;	Set RESULT to true if jcc jumps.
;;; 
;;;	It is probably the case that SECOND is available here,
;;;	but I'm not using that fact yet.
	
;%macro setcc 1				; 11 bytes, no jump
;	set%1	RESULT_LOW		; 2 bytes
;	and	RESULT, 1		; 3 bytes
;	shl	RESULT, 2		; 3 bytes
;	or	RESULT_LOW, TRUE_CONST	; 3 bytes
;%endmacro

;;; Jesse thinks that the above is buggy (should be or'ing with FALSE_CONST)

;;; TRUE=6
;;; FALSE=2
%macro setcc 1				; 10 bytes, jump
	const2reg RESULT, TRUE_CONST	; 5 bytes
	j%1 short %%L1			; 2 bytes
	sub	RESULT_LOW, 4		; 3 bytes (would be 2 if RESULT=eax)
%%L1:	
%endmacro

;;; double_tag_predicate ptrtag, hdr
;;;	Set RESULT to #t if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise set RESULT
;;;	to #f.

%macro double_tag_predicate 2
	double_tag_test %1, %2
	setcc z
%endmacro

;;; fixnum_test_temp_is_free reg
;;;	Test reg for fixnum-ness and clear zero flag if fixnum.  OK to
;;;	destroy TEMP.
	
%macro fixnum_test_temp_is_free 1
%if is_hwreg(%1)
%if hwreg_has_low(%1)
	test	REG %+ %1 %+ _LOW, fixtag_mask
%else
	;; test	REG%1, fixtag_mask
	;; Above is 6 bytes, below is 4 bytes.  Performance?
	mov	TEMP, REG%1
	test	TEMP_LOW, fixtag_mask
%endif
%else
	test	byte [GLOBALS+G_REG%1], fixtag_mask
%endif
%endmacro

;;; single_tag_test ptrtag
;;;	Leave zero flag set if RESULT contains a value with the given
;;;     3-bit tag.

%macro single_tag_test 1
	lea	TEMP, [RESULT+(8-%1)]
	test	TEMP_LOW, 7
%endmacro
	
;;; single_tag_test_ex ptrtag, exception_code
;;;	Unless in unsafe mode, test the pointer in RESULT for the
;;;	tag and signal an exception if it does not match.

%macro single_tag_test_ex 2
%ifndef UNSAFE_CODE
%%L0:	single_tag_test %1
	jz short %%L1
	exception_continuable %2, %%L0
%%L1:
%endif
%endmacro

;;; double_tag_test ptrtag, hdr
;;;	Set the zero flag if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise reset the flag.
;;;     If zero flag is set, leaves the header field in TEMP.

%macro double_tag_test 2
	single_tag_test %1
	jnz short %%L1
	mov	TEMP, [RESULT-%1]
	cmp	TEMP_LOW, %2
%%L1:	
%endmacro
	
;;; fixnum_arithmetic regno, operation, undo-operation, ex
	
%macro fixnum_arithmetic 4
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	jnz short %%L1
	loadr	TEMP, %1
	%2	RESULT, TEMP
	jno short %%L2
	%3	RESULT, TEMP
%%L1:	exception_continuable %4, %%L0	; second is tmp so 2nd arg is in place
%%L2:
%else
 ; This looks totally bogus -- should use %2? XXX TODO BUG HERE
 %if is_hwreg(%1)
	add	RESULT, REG%1
 %else
	add	RESULT, [GLOBALS+G_REG%1]
 %endif
%endif
%endmacro
	
;;; trusted_fixnum_compare r, cc
%macro trusted_fixnum_compare 2
%if is_hwreg(%1)
	cmp	RESULT, REG%1
%else
	cmp	RESULT, [GLOBALS+G_REG%1]
%endif
	setcc	%2
%endmacro

;;; fixnum_compare reg, cc, ex
;;; OPTIMIZEME for whenever %1 is a hwreg

%macro fixnum_compare 3
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	jz short %%L1
	loadr	SECOND, %1
	exception_continuable %3, %%L0	; second is tmp so 2nd arg is in place
%%L1:
%endif
%if is_hwreg(%1)
	cmp	RESULT, REG%1
%else
	cmp	RESULT, [GLOBALS+G_REG%1]
%endif
	setcc %2
%endmacro

;;; fixnum_shift r2, operation, ex
;;; 
;;; Shift count must be in CL if it is not constant
;;; OPTIMIZEME: we can do better than what I do here.
	
%macro fixnum_shift 3
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	loadr	SECOND, %1
	jz short %%L2
%%L1:	exception_continuable %3, %%L0
%%L2:	cmp	TEMP, fixnum(32)	; SECOND is TEMP
	jge short %%L1
%else
	loadr	TEMP, %1
%endif
	shr	TEMP, 2
	mov	[GLOBALS+G_REGALIAS_ECX], ecx
	mov	cl, al			; Code knows TEMP is EAX
 	%2	RESULT, cl
	mov	ecx, [GLOBALS+G_REGALIAS_ECX]
%ifidn %2, shl
	;; Nothing
%else
	;; Right shifts: mask out low bits
	;; OPTIMIZEME: if RESULT were eax, masking RESULT_LOW with a byte
	;; would save one byte here.
	and	RESULT, ~fixtag_mask
%endif
%endmacro
	
;;; generic_arithmetic regno, operation, undo-operation, millicode
	
%macro generic_arithmetic 4
	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	loadr	TEMP, %1
	jnz short %%L1
	%2	RESULT, TEMP
	jno short %%L2
	%3	RESULT, TEMP
%%L1:	mcall	%4		; second is temp so 2nd arg is in place
%%L2:
%endmacro
	
;;; generic_compare reg, condition, millicode

%macro generic_compare 3
	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	loadr	SECOND, %1
	jz short %%L1
	mcall	%3
	jmp short %%L2	
%%L1:	cmp	RESULT, SECOND
	setcc	%2
%%L2:
%endmacro

;;; generic_char_compare reg, cc, ex

%macro generic_char_compare 3
%ifndef UNSAFE_CODE
%%L0:	loadr	SECOND, %1
	cmp	SECOND_LOW, IMM_CHAR
	jz	%%L2
%%L1:	exception_continuable %3, %%L0
%%L2:	cmp	RESULT_LOW, IMM_CHAR
	jne short %%L1
	cmp	RESULT, SECOND
%else
 %if is_hwreg(%1)
	cmp	RESULT, REG%1
 %else
	cmp	RESULT, [GLOBALS+G_REG%1]
 %endif
%endif
	setcc	%2
%endmacro
	
;;; generic_imm_compare imm, cc, millicode

%macro generic_imm_compare 3
	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	const2regf SECOND, %1
	mcall	%3
	jmp short %%L2
%%L1:	cmp	RESULT, %1
	setcc	%2
%%L2:	
%endmacro
	
;;; generic_char_imm_compare imm, cc, ex
	
%macro generic_char_imm_compare 3
%ifndef UNSAFE_CODE
%%L0:	cmp	RESULT_LOW, IMM_CHAR
	jz	%%L1
	const2regf SECOND, %1
	exception_continuable %3, %%L0
%endif
%%L1:	cmp	RESULT, %1
	setcc	%2
%endmacro

;;; indexed_structure_length ptrtag, hdrtag, ex, byte?

%macro indexed_structure_length 4		; string-length or bytevector-length
%ifndef UNSAFE_CODE
%%L0:	double_tag_test %1, %2
	jz short %%L1
	exception_continuable %3, %%L0
%%L1:	
	mov	RESULT, TEMP
%else
	mov	RESULT, [RESULT-%1]
%endif
	shr	RESULT, 8
%if %4
	shl	RESULT, 2
%endif
%endmacro

;;; indexed_structure_length ptrtag, ex, byte?
	
%macro indexed_structure_length 3	
	single_tag_test_ex %1, %2
	mov	RESULT, [RESULT-%1]
	shr	RESULT, 8
%if %3	
	shl	RESULT, 2
%endif
%endmacro

;;; indexed_structure_test reg_index, reg_value, ptrtag, hdrtag, ex, byte?, test_reg_value
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that reg_index is a fixnum in the range of the structure.
;;;	If hdrtag is zero then do not check it.

%macro indexed_structure_test 7
%ifndef UNSAFE_CODE
%%L0:
	fixnum_test_temp_is_free %1
	jnz short %%L1
 %if %4
	double_tag_test %3, %4
	jz short %%L2
 %else
	single_tag_test %3
	jz short %%L3
 %endif
%%L1:	loadr	SECOND, %1
	exception_continuable %5, %%L0
 %if %4==0
%%L3:	mov	TEMP, [RESULT-%3]
 %endif
%%L2:	shr	TEMP, 8
 %if %6
	shl	TEMP, 2		; Length is now a fixnum
 %endif
 %if is_hwreg(%1)
	cmp	TEMP, REG%1
 %else
	cmp	TEMP, [GLOBALS+G_REG%1]
 %endif
	jbe short %%L1
	%7	%2, %%L1
%endif
%endmacro

;;; indexed_structure_test_imm index, ptrtag, hdrtag, ex, byte?
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that index (a fixnum) is in the range of the structure.
;;;	If hdrtag is zero then do not check it.
	
%macro indexed_structure_test_imm 5
%ifndef UNSAFE_CODE
%%L0:
 %if %3
	double_tag_test %2, %3
	jz short %%L2
 %else
	single_tag_test %2
	jz short %%L3
 %endif
%%L1:	mov	SECOND, %1
	exception_continuable %4, %%L0
 %if %3==0
%%L3:	mov	TEMP, [RESULT-%2]
 %endif
%%L2:	shr	TEMP, 8
 %if %5
	shl	TEMP, 2		; Length is now a fixnum
 %endif
	cmp	TEMP, %1
	jbe short %%L1
%endif
%endmacro

;;; load_from_indexed_structure index_reg, ptrtag, byte?
	
%macro load_from_indexed_structure 3
%if %3
	loadr	TEMP, %1
	shr	TEMP, 2
	mov	RESULT_LOW, [RESULT-%2+wordsize+TEMP]
	and	RESULT, 0xFF
%else
 %if is_hwreg(%1)
	mov	RESULT, [RESULT-%2+wordsize+REG%1]
 %else
	loadr	TEMP, %1
	mov	RESULT, [RESULT-%2+wordsize+TEMP]
 %endif
%endif
%endmacro

%macro load_from_indexed_structure_imm 3
%if %3
	mov	RESULT_LOW, [RESULT-%2+wordsize+(%1/4)]
	and	RESULT, 0xFF
%else
	mov	RESULT, [RESULT-%2+wordsize+%1]
%endif
%endmacro
				
;;; indexed_structure_ref reg, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in RESULT.

%macro indexed_structure_ref 5
	indexed_structure_test %1, 0, %2, %3, %4, %5, check_nothing
	load_from_indexed_structure %1, %2, %5
%endmacro

;;; indexed_structure_ref_imm idx, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in RESULT.

%macro indexed_structure_ref_imm 5
	indexed_structure_test_imm %1, %2, %3, %4, %5
	load_from_indexed_structure_imm %1, %2, %5
%endmacro
				
;;; indexed_structure_ref reg, ptrtag, ex, byte?
;;;	Leave the raw value in RESULT.

%macro indexed_structure_ref 4
	indexed_structure_test %1, 0, %2, 0, %3, %4, check_nothing
	load_from_indexed_structure %1, %2, %4
%endmacro

%macro indexed_structure_ref_imm 4
	indexed_structure_test_imm %1, %2, 0, %3, %4
	load_from_indexed_structure_imm %1, %2, %4
%endmacro

;;; check_nothing regno, label
;;;	Just a placeholder.

%macro check_nothing 2
%endmacro
	
;;; check_fixnum regno, label
;;;	Branch to label if regno does not hold a fixnum

%macro check_fixnum 2
	fixnum_test_temp_is_free %1
	jnz short %2
%endmacro
	
;;; check_char regno, label
;;;	Branch to label if regno does not hold a char.
;;;	Leaves char in TEMP (even if it is in hwreg, the value must
;;;	be shifted anyway).

%macro check_char 2
	loadr	TEMP, %1
	cmp	TEMP_LOW, IMM_CHAR
	jnz short %2
%endmacro

;;; indexed_structure_set_* reg_idx, reg_value, ptrtag, hdrtag, ex
;;;	If hdrtag is 0 then do not check it.

%macro indexed_structure_set_char 5
	indexed_structure_test %1, %2, %3, %4, %5, 1, check_char
	mov	[GLOBALS+G_STKP], CONT
	loadr	CONT, %1
	shr	TEMP, 16
	shr	CONT, 2
	mov	[RESULT-%3+wordsize+CONT], TEMP_LOW
	mov	CONT, [GLOBALS+G_STKP]
%endmacro

%macro indexed_structure_set_byte 5
	indexed_structure_test %1, %2, %3, %4, %5, 1, check_fixnum
	mov	[GLOBALS+G_STKP], CONT
	loadr	CONT, %1
	shr	CONT, 2
	loadr	TEMP, %2
	shr	TEMP, 2
	mov	[RESULT-%3+wordsize+CONT], TEMP_LOW
	mov	CONT, [GLOBALS+G_STKP]
%endmacro

%macro indexed_structure_set_word 5
	indexed_structure_test %1, %2, %3, %4, %5, 0, check_nothing
	do_indexed_structure_set_word %1, %2, %3
%endmacro

%macro do_indexed_structure_set_word 3
%if is_hwreg(%2)
 %if is_hwreg(%1)
	mov	[RESULT-%3+wordsize+REG%1], REG%2
	write_barrier -1, %2
 %else
	loadr	TEMP, %1
	mov	[RESULT-%3+wordsize+TEMP], REG%2
	write_barrier -1, %2
 %endif
%elif is_hwreg(%1)
	loadr	SECOND, %2
	mov	[RESULT-%3+wordsize+REG%1], SECOND
	write_barrier -1, -1
%else
	mov	[GLOBALS+G_STKP], CONT
	loadr	CONT, %1
	loadr	SECOND, %2
	mov	[RESULT-%3+wordsize+CONT], SECOND
	mov	CONT, [GLOBALS+G_STKP]
	write_barrier -1, -1
%endif
%endmacro

;;; make_indexed_structure_word regno ptrtag hdrtag ex
;;;	Allocate a word structure with the length specified in RESULT
;;;	(fixnum number of entries).  If %1 is not -1, then initialize 
;;;	it with the contents of REG%1, otherwise with #!unspecified.
	
%macro make_indexed_structure_word 4
%ifndef UNSAFE_CODE
%%L0:	test	RESULT, fixtag_mask|0x80000000
	jz short %%L1
	exception_continuable %4, %%L0
%%L1:
%endif
	mov	[GLOBALS+G_ALLOCTMP], RESULT
	add	RESULT, wordsize
%if %1 == -1
	mov	SECOND, UNSPECIFIED_CONST
%else
	loadr	SECOND, %1
%endif
	mcall	M_ALLOCI
	mov	TEMP, [GLOBALS+G_ALLOCTMP]
	shl	TEMP, 8
	or	TEMP, %3
	mov	[RESULT], TEMP
	add	RESULT, %2
%endmacro

;;; make_indexed_structure_byte regno hdrtag ex
;;;	Allocate a byte structure with the length specified in RESULT
;;;     (fixnum number of bytes).  If %1 is not -1, then REG%1 must
;;;     hold a char value to be used for initialization (a check is
;;;     performed that is a char).  If %1 is -1, no initialization 
;;; 	is performed.

%macro make_indexed_structure_byte 3
%ifndef UNSAFE_CODE
	;; OPTIMIZEME (size): Unless allocation is inline,
	;; the fixnum test can be moved into the millicode.
	;; (As can the char test, I guess -- in fact, this whole
	;; instruction is probably best moved into millicode)
	;; OPTIMIZEME (speed): Both branches are mispredicted here.
%%L0:
%if %1 != -1
	loadr	SECOND, %1
%endif
	test	RESULT, fixtag_mask|0x80000000
	jz short %%L2
%%L1:	exception_continuable %3, %%L0
%%L2:
%if %1 != -1
	cmp	SECOND_LOW, IMM_CHAR
	jne	%%L1
%endif
%endif
	mov	[GLOBALS+G_ALLOCTMP], RESULT
	add	RESULT, fixnum(wordsize)
	mcall	M_ALLOC_BV
%if %1 != -1
	loadr	eax, %1		; Code knows that eax is TEMP/SECOND
	mov	[GLOBALS+G_REGALIAS_ECX], ecx
	mov	[GLOBALS+G_REGALIAS_EDI], edi
	shr	eax, char_shift	; byte value
	mov	ecx, [GLOBALS+G_ALLOCTMP]
	shr	ecx, 2		; byte count
	lea	edi, [RESULT+4]	; destination ptr
	cld
	rep stosb		; byte fill
	mov	ecx, [GLOBALS+G_REGALIAS_ECX]
	mov	edi, [GLOBALS+G_REGALIAS_EDI]
%endif
	mov	TEMP, [GLOBALS+G_ALLOCTMP]
	shl	TEMP, 6
	or	TEMP, %2
	mov	[RESULT], TEMP
	add	RESULT, BVEC_TAG
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Primitive operations
;;; 
;;; The names 'n' in T_OP1_n are the primcodes from the table in
;;; Compiler/standard-C.imp.sch (yes, that's right).

%macro T_OP1_1 0		; break
	mcall	M_BREAK
%endmacro

%macro T_OP1_3 0		; unspecified
	const2regf RESULT, UNSPECIFIED_CONST
%endmacro

%macro T_OP1_4 0		; undefined
	const2regf RESULT, UNDEFINED_CONST
%endmacro

%macro T_OP1_5 0		; eof-object
	const2regf RESULT, EOF_CONST
%endmacro

%macro T_OP1_6 0		; enable-interrupts
	mcall	M_ENABLE_INTERRUPTS
%endmacro

%macro T_OP1_7 0		; disable-interrupts
	mcall	M_DISABLE_INTERRUPTS
%endmacro

%macro T_OP1_8 0		; typetag
	mcall	M_TYPETAG
%endmacro

%macro T_OP1_9 0		; not
	cmp	RESULT_LOW, FALSE_CONST
	setcc	z
%endmacro

%macro T_OP1_10 0		; null?
	cmp	RESULT_LOW, NIL_CONST
	setcc	z
%endmacro

%macro T_OP1_11 0		; pair?
	single_tag_test PAIR_TAG
	setcc	z
%endmacro
	
%macro T_OP1_12 0		; eof-object?
	cmp	RESULT, EOF_CONST
	setcc	z
%endmacro

%macro T_OP1_13 0		; port?
	double_tag_predicate VEC_TAG, PORT_HDR
%endmacro

%macro T_OP1_14 0		; structure?
	double_tag_predicate VEC_TAG, STRUCT_HDR
%endmacro

%macro T_OP1_15 0		; car
	single_tag_test_ex PAIR_TAG, EX_CAR
	mov	RESULT, [RESULT-PAIR_TAG]
%endmacro
	
%macro T_OP1_16 0		; cdr
	single_tag_test_ex PAIR_TAG, EX_CDR
	mov	RESULT, [RESULT-PAIR_TAG+wordsize]
%endmacro

%macro T_OP1_17 0		; symbol?
	double_tag_predicate VEC_TAG, SYMBOL_HDR
%endmacro

%macro T_OP1_18 0		; number? and complex?
	mcall	M_COMPLEXP
%endmacro

%macro T_OP1_20 0		; real? and rational?
	mcall	M_RATIONALP
%endmacro

%macro T_OP1_21 0		; compnum?
	double_tag_predicate BVEC_TAG, COMPNUM_HDR
%endmacro

%macro T_OP1_22 0		; integer?
	test	RESULT_LOW, fixtag_mask
	je	%%L1
	mcall	M_INTEGERP
	jmp short %%L2
%%L1:	const2regf RESULT, TRUE_CONST
%%L2:
%endmacro

%macro T_OP1_23 0		; fixnum?
	test	RESULT_LOW, fixtag_mask
	setcc	z
%endmacro
	
%macro T_OP1_24 0		; flonum?
	double_tag_predicate BVEC_TAG, FLONUM_HDR
%endmacro

%macro T_OP1_25 0		; exact?
	mcall	M_EXACTP
%endmacro

%macro T_OP1_26 0		; inexact?
	mcall	M_INEXACTP
%endmacro

%macro T_OP1_27 0		; exact->inexact
	mcall	M_EXACT2INEXACT
%endmacro

%macro T_OP1_28 0		; inexact->exact
	mcall	M_INEXACT2EXACT
%endmacro

%macro T_OP1_29 0		; round
	mcall	M_ROUND
%endmacro

%macro T_OP1_30 0		; truncate
	mcall	M_TRUNCATE
%endmacro

%macro T_OP1_31 0		; zero?
	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	mcall	M_ZEROP
	jmp short %%L2
%%L1:	and	RESULT, RESULT
	setcc	z
%%L2:
%endmacro

%macro T_OP1_32 0		; --
	mcall	M_NEGATE
%endmacro

%macro T_OP1_33 0		; fxlognot
%ifndef UNSAFE_CODE
%%L0:	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	exception_continuable EX_LOGNOT, %%L0
%%L1:	
%endif
	lea	RESULT, [RESULT+fixtag_mask]
	not	RESULT
%endmacro

%macro T_OP1_34 0		; real-part
	mcall	M_REAL_PART
%endmacro
	
%macro T_OP1_35 0		; imag-part
	mcall	M_IMAG_PART
%endmacro

%macro T_OP1_36 0		; char?
	cmp	RESULT_LOW, IMM_CHAR
	setcc	z
%endmacro

%macro T_OP1_37 0		; char->integer
%ifndef UNSAFE_CODE
%%L0:	cmp	RESULT_LOW, IMM_CHAR
	jz	%%L1
	exception_continuable EX_CHAR2INT, %%L0
%%L1:	
%endif
	shr	RESULT, 14
%endmacro
	
%macro T_OP1_38 0		; integer->char
%ifndef UNSAFE_CODE
%%L0:	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	exception_continuable EX_INT2CHAR, %%L0
%%L1:
%endif
	and	RESULT, 1023
	shl	RESULT, 14
	or	RESULT_LOW, IMM_CHAR
%endmacro

%macro T_OP1_39 0		; string?
	double_tag_predicate BVEC_TAG, STR_HDR
%endmacro

%macro T_OP1_40 0		; string-length
	indexed_structure_length BVEC_TAG, STR_HDR, EX_STRING_LENGTH, 1
%endmacro
		
%macro T_OP1_41 0		; vector?
	double_tag_predicate VEC_TAG, VECTOR_HDR
%endmacro

%macro T_OP1_42 0		; vector-length
	indexed_structure_length VEC_TAG, VECTOR_HDR, EX_VECTOR_LENGTH, 0
%endmacro
		
%macro T_OP1_43 0		; bytevector?
	double_tag_predicate BVEC_TAG, BYTEVECTOR_HDR
%endmacro

%macro T_OP1_44 0		; bytevector-length
	indexed_structure_length BVEC_TAG, BYTEVECTOR_HDR, EX_BYTEVECTOR_LENGTH, 1
%endmacro

%macro T_OP2_45 1		; bytevector-fill!
%ifndef UNSAFE_CODE
%%L0:	single_tag_test BVEC_TAG
	jz short %%L2
%%L1:	exception_continuable EX_BVFILL, %%L0
%%L2:
	loadr	SECOND, %1
	test	SECOND_LOW, fixtag_mask
	jnz short %%L1
%else
	loadr	SECOND, %1
%endif
	mcall	M_BYTEVECTOR_LIKE_FILL
%endmacro

%macro T_OP1_46 0		; make-bytevector
	make_indexed_structure_byte -1, BYTEVECTOR_HDR, EX_MKBVL
%endmacro

%macro T_OP1_47 0		; procedure?
	single_tag_test PROC_TAG
	setcc	z
%endmacro

%macro T_OP1_48 0		; procedure-length
	indexed_structure_length PROC_TAG, EX_PROCEDURE_LENGTH, 0
%endmacro

%macro T_OP1_49 0		; make-procedure
	; exception code wrong, matches Sparc
	make_indexed_structure_word -1, PROC_TAG, PROC_HDR, EX_MKVL
%endmacro
		
%macro T_OP1_52 0		; make-cell just maps to cons, for now
	T_OP2_58 1		; OPTIMIZEME: remove next instr by specializing
	mov	[RESULT+4-PAIR_TAG], dword UNSPECIFIED_CONST
%endmacro

%macro T_OP1_54 0		; cell-ref
	mov	RESULT, [RESULT-PAIR_TAG]
%endmacro

%macro T_OP2_55 1		; typetag-set!
	loadr	SECOND, %1
	mcall	M_TYPETAG_SET
%endmacro

%macro T_OP2_56 1		; eq?
%if is_hwreg(%1)
	cmp	RESULT, REG%1
%else
	cmp	RESULT, [GLOBALS+G_REG%1]
%endif
	setcc	z
%endmacro

%macro T_OP2_57 1		; eqv?
	loadr	SECOND, %1
	mcall	M_EQV
%endmacro
				
%macro T_OP2_58 1		; cons
%ifdef INLINE_ALLOCATION
%%L1:	mov	TEMP, [GLOBALS+G_ETOP]
	add	TEMP, 8
	cmp	TEMP, CONT
	jle short %%L2
	mcall	M_MORECORE
	jmp short %%L1
%%L2:	mov	[GLOBALS+G_ETOP], TEMP
	mov	[TEMP-8], RESULT
	lea	RESULT, [TEMP-8+PAIR_TAG]
 %if is_hwreg(%1)
	mov	[RESULT-PAIR_TAG+4], REG%1
 %else
	loadr	TEMP, %1
	mov	[RESULT-PAIR_TAG+4], TEMP
 %endif
%else
	mov	[GLOBALS+G_ALLOCTMP], RESULT
	mov	RESULT, 8
	mcall	M_ALLOC
	mov	TEMP, [GLOBALS+G_ALLOCTMP]
	mov	[RESULT], TEMP
 %if is_hwreg(%1)
	mov	[RESULT+4], REG%1
 %else
	loadr	TEMP, %1
	mov	[RESULT+4], TEMP
 %endif
	add	RESULT, PAIR_TAG
%endif
%endmacro
	
%macro T_OP2_59 1		; set-car!
	single_tag_test_ex PAIR_TAG, EX_SETCAR
%if is_hwreg(%1)
	mov	[RESULT-PAIR_TAG], REG%1
	write_barrier -1, %1
%else
	loadr	SECOND, %1
	mov	[RESULT-PAIR_TAG], SECOND
	write_barrier -1, -1
%endif
%endmacro

%macro T_OP2_60 1		; set-cdr!
	single_tag_test_ex PAIR_TAG, EX_SETCDR
%if is_hwreg(%1)
	mov	[RESULT-PAIR_TAG+wordsize], REG%1
	write_barrier -1, %1
%else
	loadr	SECOND, %1
	mov	[RESULT-PAIR_TAG+wordsize], SECOND
	write_barrier -1, -1
%endif
%endmacro

%macro T_OP2_61 1		; +
	generic_arithmetic %1, add, sub, M_ADD
%endmacro

%macro T_OP2_62 1		; -
	generic_arithmetic %1, sub, add, M_SUBTRACT
%endmacro

%macro T_OP2_63 1		; *
	loadr	SECOND, %1
	mcall	M_MULTIPLY
%endmacro
	
%macro T_OP2_64 1		; /
	loadr	SECOND, %1
	mcall	M_DIVIDE
%endmacro
	
%macro T_OP2_65 1		; quotient
	loadr	SECOND, %1
	mcall	M_QUOTIENT
%endmacro

%macro T_OP2_66 1		; <
	generic_compare %1, l, M_NUMLT
%endmacro
	
%macro T_OP2_67 1		; <=
	generic_compare %1, le, M_NUMLE
%endmacro

%macro T_OP2_68 1		; =
	generic_compare %1, e, M_NUMEQ
%endmacro

%macro T_OP2_69 1		; >
	generic_compare %1, g, M_NUMGT
%endmacro

%macro T_OP2_70 1		; >=
	generic_compare %1, ge, M_NUMGE
%endmacro

%macro T_OP2_71 1		; fxlogand
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	loadr	SECOND, %1
	jz short %%L1
	exception_continuable EX_LOGAND, %%L0
%%L1:	
	and	RESULT, SECOND
%else
 %if is_hwreg(%1)
	and	RESULT, REG%1
 %else
	and	RESULT, [GLOBALS+G_REG%1]
 %endif
%endif
%endmacro

%macro T_OP2_72 1		; fxlogior
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	jz short %%L1
	loadr	SECOND, %1
	exception_continuable EX_LOGIOR, %%L0
%%L1:	
	mov	RESULT, TEMP
%else
 %if is_hwreg(%1)
	or	RESULT, REG%1
 %else
	or	RESULT, [GLOBALS+G_REG%1]
 %endif
%endif
%endmacro

%macro T_OP2_73 1		; fxlogxor
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	loadr	SECOND, %1
	jz short %%L1
	exception_continuable EX_LOGXOR, %%L0
%%L1:	
	xor	RESULT, SECOND
%else
 %if is_hwreg(%1)
	xor	RESULT, REG%1
 %else
	xor	RESULT, [GLOBALS+G_REG%1]
 %endif
%endif
%endmacro

%macro T_OP2_74 1		; fxlsh
	fixnum_shift %1, shl, EX_LSH
%endmacro
	
%macro T_OP2_75 1		; fxrsha
	fixnum_shift %1, sar, EX_RSHA
%endmacro

%macro T_OP2_76 1		; fxrshl
	fixnum_shift %1, shr, EX_RSHL
%endmacro
	
%macro T_OP2_77 1		; rot
%error T_OP2_rot not implemented
%endmacro

%macro T_OP2_78 1		; string-ref
	indexed_structure_ref %1, BVEC_TAG, STR_HDR, EX_STRING_REF, 1
	shl	RESULT, char_shift
	or	RESULT_LOW, IMM_CHAR
%endmacro

%macro T_OP3_79 2		; string-set!
	indexed_structure_set_char %1, %2, BVEC_TAG, STR_HDR, EX_STRING_SET
%endmacro

%macro T_OP2_80 1		; make-vector
	make_indexed_structure_word %1, VEC_TAG, VEC_HDR, EX_MKVL
%endmacro

%macro T_OP2_81 1		; vector-ref
	indexed_structure_ref %1, VEC_TAG, VEC_HDR, EX_VREF, 0
%endmacro

%macro T_OP2_82 1		; bytevector-ref
	indexed_structure_ref %1, BVEC_TAG, BYTEVECTOR_HDR, EX_BYTEVECTOR_REF, 1
	shl	RESULT, 2
%endmacro

%macro T_OP2_83 1		; procedure-ref
	indexed_structure_ref %1, PROC_TAG, EX_PROCEDURE_REF, 0
%endmacro

%macro T_OP2_84 1		; cell-set!
%if is_hwreg(%1)
	mov	[RESULT-PAIR_TAG], REG%1
	write_barrier -1, %1
%else
	loadr	SECOND, %1
	mov	[RESULT-PAIR_TAG], SECOND
	write_barrier -1, -1
%endif
%endmacro

%macro T_OP2_85 1		; char<?
	generic_char_compare %1, l, EX_CHARLT
%endmacro

%macro T_OP2_86 1		; char<=?
	generic_char_compare %1, le, EX_CHARLE
%endmacro

%macro T_OP2_87 1		; char=?
	generic_char_compare %1, e, EX_CHAREQ
%endmacro

%macro T_OP2_88 1		; char>?
	generic_char_compare %1, g, EX_CHARGT
%endmacro

%macro T_OP2_89 1		; char>=?
	generic_char_compare %1, ge, EX_CHARGE
%endmacro

%macro T_OP2_90 1		; sys$partial-list->vector
	loadr	SECOND, %1
	mcall	M_PARTIAL_LIST2VECTOR
%endmacro

%macro T_OP3_91 2		; vector-set!
	indexed_structure_set_word %1, %2, VEC_TAG, VEC_HDR, EX_VECTOR_SET
%endmacro

%macro T_OP3_92 2		; bytevector-set!
	indexed_structure_set_byte %1, %2, BVEC_TAG, BYTEVECTOR_HDR, EX_BYTEVECTOR_SET
%endmacro

%macro T_OP3_93 2		; procedure-set!
	indexed_structure_set_word %1, %2, PROC_TAG, 0, EX_PROCEDURE_SET
%endmacro

%macro T_OP1_94 0		; bytevector-like?
	single_tag_test BVEC_TAG
	setcc	z
%endmacro

%macro T_OP1_95 0		; vector-like?
	single_tag_test VEC_TAG
	setcc	z
%endmacro

%macro T_OP2_96 1		; bytevector-like-ref
	indexed_structure_ref %1, BVEC_TAG, EX_BVLREF, 1
	shl	RESULT, 2
%endmacro

%macro T_OP3_97 2		; bytevector-like-set!
	indexed_structure_set_byte %1, %2, BVEC_TAG, 0, EX_BVLSET
%endmacro

%macro T_OP2_98 1		; sys$bvlcmp
	loadr	SECOND, %1
	mcall	M_BYTEVECTOR_LIKE_COMPARE
%endmacro

%macro T_OP2_99 1		; vector-like-ref
	indexed_structure_ref %1, VEC_TAG, EX_VLREF, 0
%endmacro

%macro T_OP3_100 2		; vector-like-set!
	indexed_structure_set_word %1, %2, VEC_TAG, 0, EX_VECTOR_LIKE_SET
%endmacro

%macro T_OP1_101 0		; vector-like-length
	indexed_structure_length VEC_TAG, EX_VLLEN, 0
%endmacro

%macro T_OP1_102 0		; bytevector-like-length
	indexed_structure_length BVEC_TAG, EX_BVLLEN, 1
%endmacro

%macro T_OP2_103 1		; remainder
	loadr	SECOND, %1
	mcall	M_REMAINDER
%endmacro

%macro T_OP1_104 0		; petit-patch-boot-code
	mcall	M_PETIT_PATCH_BOOT_CODE
%endmacro

%macro T_OP1_105 0		; syscall
	mcall	M_SYSCALL
%endmacro

%macro T_OP1_106 0		; creg
	mcall	M_CREG
%endmacro

%macro T_OP1_107 0		; creg-set!
	mcall	M_CREG_SET
%endmacro

%macro T_OP1_108 0		; gc-counter
	mov	RESULT, [GLOBALS+G_GC_CNT]
%endmacro

%macro T_OP2_109 1		; make-string
	; exception code wrong, matches Sparc
	make_indexed_structure_byte %1, STR_HDR, EX_MKBVL 
%endmacro

%macro T_OP2IMM_128 1		; typetag-set!
	const2regf SECOND, %1
	mcall	M_TYPETAG_SET
%endmacro

%macro T_OP2IMM_129 1		; eq?
	cmp	RESULT, %1
	setcc	z
%endmacro

%macro T_OP2IMM_130 1		; +
	test	RESULT_LOW, fixtag_mask
	jnz short %%L1
	add	RESULT, %1
	jno short %%L2
	sub	RESULT, %1
%%L1:	mov	SECOND, %1
	mcall	M_ADD
%%L2:
%endmacro

%macro T_OP2IMM_131 1		; -
	test	RESULT_LOW, fixtag_mask
	jnz short %%L1
	sub	RESULT, %1
	jno short %%L2
	add	RESULT, %1
%%L1:	mov	SECOND, %1
	mcall	M_SUBTRACT
%%L2:	
%endmacro

%macro T_OP2IMM_132 1		; <
	generic_imm_compare %1, l, M_NUMLT
%endmacro

%macro T_OP2IMM_133 1		; <=
	generic_imm_compare %1, le, M_NUMLE
%endmacro

%macro T_OP2IMM_134 1		; =
	generic_imm_compare %1, e, M_NUMEQ
%endmacro

%macro T_OP2IMM_135 1		; >
	generic_imm_compare %1, g, M_NUMGT
%endmacro

%macro T_OP2IMM_136 1		; >=
	generic_imm_compare %1, ge, M_NUMGE
%endmacro

%macro T_OP2IMM_137 1		; char<?
	generic_char_imm_compare %1, l, EX_CHARLT
%endmacro

%macro T_OP2IMM_138 1		; char<=?
	generic_char_imm_compare %1, le, EX_CHARLE
%endmacro

%macro T_OP2IMM_139 1		; char=?
	generic_char_imm_compare %1, e, EX_CHAREQ
%endmacro

%macro T_OP2IMM_140 1		; char>?
	generic_char_imm_compare %1, g, EX_CHARGT
%endmacro

%macro T_OP2IMM_141 1		; char>=?
	generic_char_imm_compare %1, ge, EX_CHARGE
%endmacro

;;; The following five are probably a waste of effort.

%macro T_OP2IMM_142 1		; string-ref
	indexed_structure_ref_imm %1, BVEC_TAG, STR_HDR, EX_STRING_REF, 1
	shl	RESULT, char_shift
	or	RESULT_LOW, IMM_CHAR
%endmacro

%macro T_OP2IMM_143 1		; vector-ref
	indexed_structure_ref_imm %1, VEC_TAG, VECTOR_HDR, EX_VECTOR_REF, 0
%endmacro

%macro T_OP2IMM_144 1		; bytevector-ref
	indexed_structure_ref_imm %1, BVEC_TAG, BYTEVECTOR_HDR, EX_BYTEVECTOR_REF, 1
	shl	RESULT, 2
%endmacro
	
%macro T_OP2IMM_145 1		; bytevector-like-ref
	indexed_structure_ref_imm %1, BVEC_TAG, EX_BVLREF, 1
	shl	RESULT, 2
%endmacro

%macro T_OP2IMM_146 1		; vector-like-ref
	indexed_structure_ref_imm %1, VEC_TAG, EX_VLREF, 0
%endmacro

%macro T_OP1_200 0		; most-positive-fixnum
	const2regf RESULT, 0x7FFFFFFC
%endmacro

%macro T_OP1_201 0		; most-negative-fixnum
	const2regf RESULT, 0x80000000
%endmacro

%macro T_OP2_202 1		; fx+
	fixnum_arithmetic %1, add, sub, EX_FXADD
%endmacro

%macro T_OP2_203 1		; fx-
	fixnum_arithmetic %1, sub, add, EX_FXSUB
%endmacro

%macro T_OP1_204 0		; fx--
%ifndef UNSAFE_CODE
%%L0:	test	RESULT_LOW, fixtag_mask
	jnz short %%L1
	neg	RESULT
	jno short %%L2
	;; No need to undo: RESULT is unchanged
%%L1:	exception_continuable EX_FXNEG, %%L0
%%L2:	
%else
	neg	RESULT
%endif
%endmacro

%macro T_OP2_205 1              ; fx*
%ifndef UNSAFE_CODE
%%L0:	loadr	TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	jnz short %%L1
	loadr	TEMP, %1
	sar	TEMP, 2
	imul	TEMP, RESULT
	jno short %%L2
%%L1:	loadr	TEMP, %1
	exception_continuable EX_FXMUL, %%L0
%%L2:
	mov	RESULT, TEMP
%else
	shr	RESULT
 %if is_hwreg(%1)
	imul	RESULT, REG%1
 %else
	imul	RESULT, [GLOBALS+G_REG%1]
 %endif
%endif
%endmacro
	
%macro T_OP2_206 1		; fx=
	fixnum_compare %1, e, EX_FXEQ
%endmacro

%macro T_OP2_207 1		; fx<
	fixnum_compare  %1, l, EX_FXLT
%endmacro

%macro T_OP2_208 1		; fx<=
	fixnum_compare  %1, le, EX_FXLE
%endmacro

%macro T_OP2_209 1		; fx>
	fixnum_compare  %1, g, EX_FXGT
%endmacro

%macro T_OP2_210 1		; fx>=
	fixnum_compare  %1, ge, EX_FXGE
%endmacro

; Changed T_OP2_2{11,12,13} to OP1.
; Do we refer to these as OP2 anywhere?
%macro T_OP1_211 0		; fxzero?
%ifndef UNSAFE_CODE
%%L0:	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	exception_continuable EX_FXZERO, %%L0
%%L1:
%endif
	test	RESULT, RESULT
	setcc	z
%endmacro

%macro T_OP1_212 0		; fxpositive?
%ifndef UNSAFE_CODE
%%L0:	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	exception_continuable EX_FXPOSITIVE, %%L0
%%L1:
%endif
	cmp	RESULT, 0
	setcc	g
%endmacro

%macro T_OP1_213 0		; fxnegative?
%ifndef UNSAFE_CODE
%%L0:	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	exception_continuable EX_FXNEGATIVE, %%L0
%%L1:
%endif
	cmp	RESULT, 0
	setcc	l
%endmacro

%macro fixnum_imm_arithmetic 4
%ifndef UNSAFE_CODE
%%L0:	const2regf TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	jnz short %%L1
	const2regf TEMP, %1
	%2	RESULT, TEMP
	jno short %%L2
	%3	RESULT, TEMP
%%L1:	exception_continuable %4, %%L0	; second is tmp so 2nd arg is in place
%%L2:
%else
	const2regf TEMP, %1
	%2	RESULT, TEMP
%endif
%endmacro
	
%macro T_OP2IMM_250 1           ; fx+
	fixnum_imm_arithmetic %1, add, sub, EX_FXADD
%endmacro

%macro T_OP2IMM_251 1           ; fx-
	fixnum_imm_arithmetic %1, sub, add, EX_FXSUB
%endmacro

;;; fixnum_imm_compare const, cc, ex
%macro fixnum_imm_compare 3
%ifndef UNSAFE_CODE
%%L0:	const2regf TEMP, %1
	or	TEMP, RESULT
	test	TEMP_LOW, fixtag_mask
	jz short %%L1
	const2regf TEMP, %1
	exception_continuable %3, %%L0	; second is tmp so 2nd arg is in place
%%L1:
%endif
	const2regf TEMP, %1
	cmp	RESULT,	TEMP
	setcc %2
%endmacro


%macro T_OP2IMM_253 1		; fx=
	fixnum_imm_compare %1, e, EX_FXEQ
%endmacro

%macro T_OP2IMM_254 1		; fx<
	fixnum_imm_compare  %1, l, EX_FXLT
%endmacro

%macro T_OP2IMM_255 1		; fx<=
	fixnum_imm_compare  %1, le, EX_FXLE
%endmacro

%macro T_OP2IMM_256 1		; fx>
	fixnum_imm_compare  %1, g, EX_FXGT
%endmacro

%macro T_OP2IMM_257 1		; fx>=
	fixnum_imm_compare  %1, ge, EX_FXGE
%endmacro

;;; Unsafe/trusted primitives

%macro T_OP1_401 0		; vector-length:vec
	mov	RESULT, [RESULT-VEC_TAG]
	shr	RESULT, 8
%endmacro

%macro T_OP2_402 1		; vector-ref:trusted
	add	RESULT, wordsize-VEC_TAG
%if is_hwreg(%1)
	mov	RESULT, [RESULT+REG%1]
%else
	loadr	TEMP, %1
	mov	RESULT, [RESULT+TEMP]
%endif
%endmacro

%macro T_OP3_403 2		; vector-set!:trusted
	do_indexed_structure_set_word %1, %2, VEC_TAG
%endmacro

%macro T_OP1_404 0		; car:pair
	mov	RESULT, [RESULT-PAIR_TAG]
%endmacro

%macro T_OP1_405 0		; cdr:pair
	mov	RESULT, [RESULT-PAIR_TAG+wordsize]
%endmacro

%macro T_OP2_406 1		; =:fix:fix
	trusted_fixnum_compare %1, e
%endmacro

%macro T_OP2_407 1		; <:fix:fix
	trusted_fixnum_compare %1, l
%endmacro

%macro T_OP2_408 1		; <=:fix:fix
	trusted_fixnum_compare %1, le
%endmacro

%macro T_OP2_409 1		; >=:fix:fix
	trusted_fixnum_compare %1, ge
%endmacro

%macro T_OP2_410 1		; >:fix:fix
	trusted_fixnum_compare %1, g
%endmacro

%macro T_OP2IMM_450 1		; vector-ref:trusted
	mov	RESULT, [RESULT+(wordsize-VEC_TAG)+%1]
%endmacro

%macro T_OP2IMM_451 1		; =:fix:fix
	cmp	RESULT, %1
	setcc	e
%endmacro

%macro T_OP2IMM_452 1		; <:fix:fix
	cmp	RESULT, %1
	setcc	l
%endmacro

%macro T_OP2IMM_453 1		; <=:fix:fix
	cmp	RESULT, %1
	setcc	le
%endmacro

%macro T_OP2IMM_454 1		; >:fix:fix
	cmp	RESULT, %1
	setcc	g
%endmacro

%macro T_OP2IMM_455 1		; >=:fix:fix
	cmp	RESULT, %1
	setcc	ge
%endmacro

;;; Introduced by representation inference.  Trusted.

%macro T_OP2_500 1		; +:idx:idx
%if is_hwreg(%1)
	add	RESULT, REG%1
%else
	add	RESULT, [GLOBALS+G_REG%1]
%endif
%endmacro

%macro T_OP2_501 1		; +:fix:fix
	loadr	TEMP, %1
	add	RESULT, TEMP
	jno short %%L1
	sub	RESULT, TEMP
	mcall	M_ADD		; second is temp so 2nd arg is in place
%%L1:
%endmacro
	
%macro T_OP2_502 1		; -:idx:idx
%if is_hwreg(%1)
	sub	RESULT, REG%1
%else
	sub	RESULT, [GLOBALS+G_REG%1]
%endif
%endmacro

%macro T_OP2_503 1		; -:fix:fix
	loadr	TEMP, %1
	sub	RESULT, TEMP
	jno short %%L1
	add	RESULT, TEMP
	mcall	M_SUBTRACT	; second is temp so 2nd arg is in place
%%L1:
%endmacro

%macro T_OP2IMM_520 1		; +:idx:idx
	add	RESULT, %1
%endmacro

%macro T_OP2IMM_521 1		; +:fix:fix
	add	RESULT, %1
	jno short %%L1
	sub	RESULT, %1
	mov	SECOND, %1
	mcall	M_ADD
%%L1:
%endmacro

%macro T_OP2IMM_522 1		; -:idx:idx
	sub	RESULT, %1
%endmacro

%macro T_OP2IMM_523 1		; -:fix:fix
	sub	RESULT, %1
	jno short %%L1
	add	RESULT, %1
	mov	SECOND, %1
	mcall	M_SUBTRACT
%%L1:
%endmacro

;;; Experimental stuff below this line, we need more than this to support
;;; peephole optimization well.

%macro T_OP1_612 1		; internal:branchf-zero?
	timer_check
	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	mcall	M_ZEROP
	cmp	RESULT_LOW, FALSE_CONST
	je	t_label(%1)
	jmp short %%L2
%%L1:	cmp	RESULT, 0
	jne	t_label(%1)
%%L2:
%endmacro

%macro OP2IMM_BRANCHF_lessthan 2
	test	RESULT_LOW, fixtag_mask
	jz short %%L1
	const2regf SECOND, %1
	mcall	M_NUMLT
	cmp	RESULT_LOW, FALSE_CONST
	je	t_label(%2)
	jmp short %%L2
%%L1:	cmp	RESULT, %1
	jge	t_label(%2)
%%L2:	
%endmacro
	
;;; eof

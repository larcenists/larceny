;;; i386 millicode entry points and millicode jump vector initialization.
;;;
;;; $Id: i386-millicode.asm 3077 2006-06-02 21:45:50Z pnkfelix $
;;;
;;; Millicode implementation for the x86-nasm system.  Most millicode
;;; entry points here just jump to the C implementation, incurring
;;; the cost of context switching.  The performance-critical ones can
;;; be replaced by hand-coded assembler or similar code.
	
%include "i386-machine.ah"
%define fixnum(n)           ((n)<<2)

;;; It is generally helpful to enable OPTIMIZE_MILLICODE: it turns
;;; on some assembler versions of millicode routines normally written
;;; in C, and by avoiding taking a context switch to C we gain
;;; some performance.
;;;
;;; Note, though, that only code outside this %define is necessary
;;; for correct operation.  The amount of assembler in this file
;;; is not an indication of the required porting effort.
	
%define OPTIMIZE_MILLICODE 1
%define OPTIMIZE_BARRIER 1
%define SSB_ENQUEUE_OFFSET_AS_FIXNUM 1
	
;;; Assertion checking is turned on:

%define CHECK_ASSERTIONS

	section	.text align=4
	
	global  EXTNAME(i386_scheme_jump)
	global	EXTNAME(i386_stack_underflow)
	global	EXTNAME(i386_return_from_scheme)
	global	EXTNAME(i386_dispatch_loop_return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Machine language stubs for system operations implemented in C.
	
	extern	EXTNAME(mem_stkuflow)
	extern	EXTNAME(return_from_scheme)
	extern	EXTNAME(dispatch_loop_return)
	extern	EXTNAME(gclib_pagebase)
	extern	EXTNAME(mc_exception)

;;; This is an assertion (which requires eax to be free)
;;; that REG0 points to a procedure object.
;;; XXX turn off before release.
%macro CHECKZEROREG 2
%ifdef CHECK_ASSERTIONS
%ifdef REG0
	mov	eax, REG0
%else
	mov	eax, dword [GLOBALS + G_REG0 + %1]
%endif
	lea	eax, [eax+(8 - PROC_TAG)]
	test	al, 7
	jz	%%L1
%ifdef REG0
	mov	eax, REG0
%else
	mov	eax, dword [GLOBALS + G_REG0 + %1]
%endif
	int3
	add	eax, %2
%%L1:
%endif
%endmacro
		
;;; The return address of the bottommost frame in the stack cache points
;;; to i386_stack_underflow; all we do is call the C function that
;;; escapes to the dispatch loop to restore a frame and continue execution.
	
	align	code_align
EXTNAME(i386_stack_underflow):
	sub	CONT, 4+STK_RETADDR
	mov	eax, EXTNAME(mem_stkuflow)
	jmp	callout_to_C_leave_retaddr_absolute

	
;;; The return address of a frame created by a callout from Scheme to
;;; millicode points to i386_return_from_scheme; all we do is call 
;;; the C function that escapes to the dispatch loop to restore state
;;; and continue execution
	
	align	code_align
EXTNAME(i386_return_from_scheme):
	sub	CONT, 4+STK_RETADDR
%ifdef REG0
	mov	REG0, dword [ CONT + 5*4 ]
%else
	mov	eax, dword [ CONT + 5*4 ]
	mov	[GLOBALS + G_REG0], eax
%endif
	CHECKZEROREG 0, 3
	mov	eax, EXTNAME(return_from_scheme)
	jmp	callout_to_C

	
;;; The return address of the bottommost frame on the stack points
;;; to i386_dispatch_loop_return; all we do is call the C function
;;; that terminates the dispatch loop.
	
	align	code_align
EXTNAME(i386_dispatch_loop_return):
	sub	esp, 4 		; Adjust stkp (the .cont action)
	CHECKZEROREG 0, 4
	mov	eax, EXTNAME(dispatch_loop_return)
	jmp	callout_to_C

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Enter scheme mode.  Arguments are globals ptr and address to 
;;; jump to.  Does not return, so need to save ESP only.

EXTNAME(i386_scheme_jump):
	;; Set precision to 53 bit mantissa to comply w/ IEEE 754
	fstcw	[saved_fpu_ctrl]
	mov	eax, [saved_fpu_ctrl]
	and	eax, 0xfcff	; clear precision control (PC)
	or	eax, 0x0200	; set PC to REAL8 mode
	push	eax
	fldcw	[esp]
	fwait
	pop	eax
	
	;; Switch to Scheme mode
	mov	eax, [esp+8]	; Address to jump to
	mov	ebx, [esp+4]	; globals table

	mov	[ebx+G_SAVED_ESP], esp
	
	mov	GLOBALS, ebx
%ifdef REG0
	mov	REG0, [GLOBALS+G_REG0]
%endif
	mov	REG1, [GLOBALS+G_REG1]
	mov	REG2, [GLOBALS+G_REG2]
	mov	REG3, [GLOBALS+G_REG3]
%ifdef REG4
	mov	REG4, [GLOBALS+G_REG4]
%endif
	mov	RESULT, [GLOBALS+G_RESULT]
	mov	CONT, [GLOBALS+G_STKP]

	jmp	TEMP		; TEMP is eax always

	
;;; Millicode stubs
;;; 
;;; Millicode is generally implemented in C.  Switch to C mode, call out
;;; to the C code, and if it returns then switch back to Scheme mode and
;;; return to the caller.

%macro PUBLIC 1
	global	EXTNAME(%1)
	align	code_align
EXTNAME(%1):
%endmacro

;;; The unadjusted return address is in GLOBALS[-1]; adjust it and
;;; save it in G_RETADDR.
;;;
;;; Destroys: EAX

	;; On entry, esp is off by 4
%macro SAVE_RETURN_ADDRESS_INTRSAFE 0
	mov	eax, dword [esp]			; return address
	add	eax, 3		                	;  rounded up
	and	eax, 0xFFFFFFFC	                	;   to 4-byte boundary
	add	esp, 4					; fixup esp
	mov	dword [GLOBALS+G_RETADDR], eax		;    saved for later
%endmacro

;;; Arguments: second? c-name callout-method

%macro MILLICODE_STUB 3
	extern	EXTNAME(%2)
%if %1
	mov	[GLOBALS+G_SECOND], eax
%endif
	SAVE_RETURN_ADDRESS_INTRSAFE
	mov	eax, EXTNAME(%2)
	jmp	%3
%endmacro

%macro MCg_wb 1
	MILLICODE_STUB 0, %1, callout_to_C_wb
%endmacro
	
%macro MCg 1
	MILLICODE_STUB 0, %1, callout_to_C
%endmacro

%macro MCgk 1
	MILLICODE_STUB 0, %1, callout_to_Ck
%endmacro

%macro MC2g 1
	MILLICODE_STUB 1, %1, callout_to_C
%endmacro

%macro MC2g_wb 1
	MILLICODE_STUB 1, %1, callout_to_C_wb
%endmacro

%macro MC2gk 1
	MILLICODE_STUB 1, %1, callout_to_Ck
%endmacro

	;; On entry, esp is off by 4
PUBLIC i386_alloc_bv
	MCg	mc_alloc_bv

PUBLIC i386_alloc
%ifdef OPTIMIZE_MILLICODE
	mov	TEMP, [GLOBALS+G_ETOP]
	add	RESULT, 7
	and	RESULT, 0xfffffff8
	add	TEMP, SCE_BUFFER
	add	TEMP, RESULT		; Might wrap around on a large request
	jc	L1
	cmp	TEMP, CONT
	ja	L1
	mov	RESULT, [GLOBALS+G_ETOP]
	sub	TEMP, SCE_BUFFER
	mov	[GLOBALS+G_ETOP], TEMP
	xor	TEMP, TEMP
	ret
L1:	xor	TEMP, TEMP
%endif ; OPTIMIZE_MILLICODE
	MCg	mc_alloc

	;; On entry, esp is off by 4
PUBLIC i386_alloci
%ifdef OPTIMIZE_MILLICODE
	mov	[GLOBALS+G_SECOND], SECOND
	mov	[GLOBALS+G_REGALIAS_ECX], ecx
	mov	[GLOBALS+G_REGALIAS_EDI], edi
	add	RESULT, 7
	and	RESULT, 0xfffffff8
	mov	ecx, RESULT		; Byte count for initialization
	shr	ecx, 2			;   really want words
	mov	TEMP, [GLOBALS+G_ETOP]
	add	TEMP, SCE_BUFFER
	add	TEMP, RESULT		; Might wrap around on a large request
	jc	L2
	cmp	TEMP, CONT
	ja	L2
	mov	RESULT, [GLOBALS+G_ETOP]
	sub	TEMP, SCE_BUFFER
	mov	[GLOBALS+G_ETOP], TEMP
	mov	eax, [ GLOBALS+G_SECOND ]
	mov	edi, RESULT
	cld
	rep stosd
	mov	REG1, [GLOBALS+G_REG1]
	mov	REG3, [GLOBALS+G_REG3]
	xor	SECOND, SECOND
	ret
L2:	mov	SECOND, [GLOBALS+G_SECOND]
	mov	ecx, [GLOBALS+G_REGALIAS_ECX]
	mov	edi, [GLOBALS+G_REGALIAS_EDI]
%endif ; OPTIMIZE_MILLICODE
	MC2g	mc_alloci
	
PUBLIC i386_morecore
	MCg	mc_morecore
	
PUBLIC i386_stack_overflow
	MILLICODE_STUB 0, mc_stack_overflow, callout_to_C
	
PUBLIC i386_capture_continuation
	MCg	mc_capture_continuation
	
PUBLIC i386_restore_continuation
	MCg	mc_restore_continuation
	
PUBLIC i386_full_barrier
%if OPTIMIZE_MILLICODE && OPTIMIZE_BARRIER
	cmp dword[GLOBALS+G_CONCURRENT_MARK], 0 ; If concurrent mark off
	je	Lfullgenbarrier			;   skip to gen barrier
	mov	[GLOBALS+G_WBVALUE], SECOND	; Free up working registers
	mov	TEMP, [GLOBALS+G_THIRD]		; globals[G_THIRD] holds tgt slot
	mov	TEMP, [TEMP]			; TEMP := *slot
	test	TEMP, 1				; If *slot is not ptr
	jz	LfullgenbarrierRestoreSecond	;   skip to gen barrier
	mov	[GLOBALS+G_REG1], REG1		; Free up working registers
	mov	REG1, [GLOBALS+G_GENV]		; Map page -> generation
	sub	TEMP, [EXTNAME(gclib_pagebase)]	; Load
	shr	TEMP, 12			;   generation number
	shl	TEMP, 2				;     (using byte offset)
	mov	TEMP, [REG1+TEMP]		;       for *slot
	cmp	TEMP, 0				; If gno = 0
	je	LfullgenbarrierRestoreBothRegs	;   skip to gen barrier
	mov	TEMP, RESULT			; TEMP := lhs
	sub	TEMP, [EXTNAME(gclib_pagebase)]	; Load
	shr	TEMP, 12			;   generation number
	shl	TEMP, 2				;     (using byte offset)
	mov	TEMP, [REG1+TEMP]		;       for lhs
	cmp	TEMP, 0				; If gno == 0
	jne	Lsatbbarrier			;   skip satb to gen barrier
LfullgenbarrierRestoreBothRegs:
	mov	REG1, [GLOBALS+G_REG1]		; restore orig REG1
LfullgenbarrierRestoreSecond:
	mov 	SECOND, [GLOBALS+G_WBVALUE]
Lfullgenbarrier:				;
	test	SECOND, 1			; If rhs is ptr
	jnz	EXTNAME(i386_partial_barrier)	;   enter the barrier
	ret					; Otherwise return
Lsatbbarrier:					;
	mov	TEMP, [GLOBALS+G_THIRD]		; globals[G_THIRD] holds tgt slot
	mov	TEMP, [TEMP]			; oldval = *slot
	mov	REG1, [GLOBALS+G_SATBTOPV]	; ptr to SATB SSB
	mov	REG1, [REG1]			; SATB SSB top
	mov	[REG1], TEMP			; 
	mov	TEMP, [GLOBALS+G_SATBTOPV]	; 
	add	REG1, 4				; Move SATB SSB ptr
	mov	[TEMP], REG1			; store moved ptr
	mov	TEMP, [GLOBALS+G_SATBLIMV]	; ptr to SATB SSB limit ptr
	mov	TEMP, [TEMP]			; the limit ptr
	cmp	REG1, TEMP			; if ptr!=limit
	jne	LfullgenbarrierRestoreBothRegs	;   then no overflow, so done
	mov	REG1, [GLOBALS+G_REG1]		;
	mov	[GLOBALS+G_WBDEST], RESULT
	mov	SECOND, [GLOBALS+G_WBVALUE]	; Restore state and 
	MC2g_wb	mc_compact_satb_ssb_and_genb	;   handle overflow
%else  ; OPTIMIZE_MILLICODE
	mov	[GLOBALS+G_WBDEST], RESULT
	mov	[GLOBALS+G_WBVALUE], SECOND
	MC2g_wb	mc_full_barrier
%endif ; OPTIMIZE_MILLICODE
	
PUBLIC i386_partial_barrier
%if OPTIMIZE_MILLICODE && OPTIMIZE_BARRIER
  %ifdef GCLIB_LARGE_TABLE
    %error Optimized write barrier does not work with "GCLIB_LARGE_TABLE" yet
  %endif
	cmp	dword [GLOBALS+G_GENV], 0	; Barrier is enabled
	jne	Lpb1				;   if generation map not 0
	ret					; Otherwise return to scheme
Lpb1:	mov	[GLOBALS+G_WBDEST], RESULT	; Save state and
	mov	[GLOBALS+G_WBVALUE], SECOND	;   free up some
	mov	[GLOBALS+G_REG1], REG1		;     working registers
	mov	REG1, [GLOBALS+G_GENV]		; Map page -> generation
	sub	RESULT, [EXTNAME(gclib_pagebase)]	; Load
	shr	RESULT, 12			;   generation number
	shl	RESULT, 2			;     (using byte offset)
	mov	RESULT, [REG1+RESULT]		;       for lhs
	sub	SECOND, [EXTNAME(gclib_pagebase)]	; Load
	shr	SECOND, 12			;   generation number
	shl	SECOND, 2			;     (using byte offset)
	mov	SECOND, [REG1+SECOND]		;       for rhs
	cmp	RESULT, SECOND			; Only store lhs in SSB
	jg	Lpb3				;   if gen(lhs) > gen(rhs)
	jl	Lpb4				;   (or more complex non-gen logic)
Lpb2:	mov	RESULT, [GLOBALS+G_WBDEST]	; Restore
	mov	SECOND, [GLOBALS+G_WBVALUE]	;   state
	mov	REG1, [GLOBALS+G_REG1]		;     and
	ret					;       return to Scheme
Lpb4:	cmp	dword [GLOBALS+G_FILTER_REMSET_GEN_ORDER], 0
	jne	Lpb2 		                ; filter when generational
	cmp	dword [GLOBALS+G_FILTER_REMSET_LHS_NUM], RESULT
	je	Lpb2		                ; filter lhs nursery
	cmp	dword [GLOBALS+G_FILTER_REMSET_RHS_NUM], SECOND
	je	Lpb2		                ; filter rhs static area
Lpb3:	mov	RESULT, SECOND			; Preserve gen(rhs)
	shl	RESULT, 2			; Gen(rhs) as byte offset
	mov	REG1, [GLOBALS+G_SSBTOPV]	; Array of ptrs into SSBs
	mov	SECOND, [GLOBALS+G_WBDEST]	; The value to store (lhs)
	mov	REG1, [REG1+RESULT]		; The correct SSB ptr
	mov	[REG1], SECOND			; Store lhs
%if SSB_ENQUEUE_OFFSET_AS_FIXNUM
	add	REG1, 4				; Move SSB ptr
	and	SECOND, ~TAGMASK		; ptrof(lhs)
	sub	SECOND, [GLOBALS+G_THIRD]	;  THIRD holds dest's offset
	neg	SECOND				;   = offset - ptrof(lhs)
	mov	[REG1], SECOND
	mov	SECOND, [GLOBALS+G_SSBTOPV]	; Array of ptrs into SSBs
	add	REG1, 4				; Move SSB ptr
	mov	[SECOND+RESULT], REG1		; Store moved ptr
	mov	SECOND, [GLOBALS+G_SSBLIMV]	; Array of SSB limit ptrs
	add	REG1, 4				;  SSB needs +1 slot (offset)
	mov	SECOND, [SECOND+RESULT]		; The correct limit ptr
	cmp	REG1, SECOND			; If ptr < limit
	jl	Lpb2				;   then no overflow, so done
	xor	RESULT, RESULT			; Clear
	xor	SECOND, SECOND			;   state
	mov	REG1, [GLOBALS+G_REG1]		;     and
	MCg_wb	mc_compact_ssbs			;       handle overflow
%else  ; SSB_ENQUEUE_OFFSET_AS_FIXNUM
	mov	SECOND, [GLOBALS+G_SSBTOPV]	; Array of ptrs into SSBs
	add	REG1, 4				; Move SSB ptr
	mov	[SECOND+RESULT], REG1		; Store moved ptr
	mov	SECOND, [GLOBALS+G_SSBLIMV]	; Array of SSB limit ptrs
	mov	SECOND, [SECOND+RESULT]		; The correct limit ptr
	cmp	REG1, SECOND			; If ptr!=limit
	jne	Lpb2				;   then no overflow, so done
	xor	RESULT, RESULT			; Clear
	xor	SECOND, SECOND			;   state
	mov	REG1, [GLOBALS+G_REG1]		;     and
	MCg_wb	mc_compact_ssbs			;       handle overflow
%endif
%else  ; OPTIMIZE_MILLICODE
	mov	[GLOBALS+G_WBDEST], RESULT	; Save 
	mov	[GLOBALS+G_WBVALUE], SECOND	;   state
	MC2g_wb	mc_partial_barrier
%endif ; OPTIMIZE_MILLICODE
	
PUBLIC i386_break
	MCg	mc_break
	
PUBLIC i386_timer_exception
	MCgk	mc_timer_exception
	
PUBLIC i386_enable_interrupts
	MCgk	mc_enable_interrupts
	
PUBLIC i386_disable_interrupts
	MCgk	mc_disable_interrupts
	
;;; With Sassy:
;;; Can't convert retaddr to a relative offset from
;;; the codevector for r0 here, because mc_apply
;;; overwrites r0 and so we would not be able to
;;; recover the original address.		
PUBLIC i386_apply
	MILLICODE_STUB 1, mc_apply, callout_to_C_leave_retaddr_absolute
	
PUBLIC i386_restargs
	MC2g	mc_restargs
	
PUBLIC i386_syscall
	MC2gk	mc_syscall
	
PUBLIC i386_typetag
	MCg	mc_typetag
	
PUBLIC i386_typetag_set
	MC2g	mc_typetag_set
	
PUBLIC i386_eqv
	MC2gk	mc_eqv
	
PUBLIC i386_partial_list2vector
	MC2g	mc_partial_list2vector
	
PUBLIC i386_bytevector_like_fill
	MC2g	mc_bytevector_like_fill
	
PUBLIC i386_bytevector_like_compare
	MC2g	mc_bytevector_like_compare

;;; general_tag2_predicate tag1 tag2

%macro general_tag2_predicate 2
	lea	RESULT, [RESULT - %1]
	test	RESULT_LOW, 7
	jnz	%%RETFALSE
	mov	RESULT, [RESULT]
	cmp	RESULT_LOW, %2
	jnz	%%RETFALSE
	mov	RESULT, TRUE_CONST
	ret
%%RETFALSE:	
	mov	RESULT, FALSE_CONST
	ret
%endmacro

;;; generic_fp_unary opcode
	
%macro generic_fp_unary 1
	lea	RESULT, [RESULT + (8 - BVEC_TAG)]
	test	RESULT_LOW, 7
	jnz 	%%NOFLO
	mov	TEMP, [RESULT-8]
	cmp	TEMP_LOW, FLONUM_HDR
	jnz	%%NOFLO
	;; Got here, got a float!
	mov	TEMP, [GLOBALS+G_ETOP]
	add	TEMP, 16+SCE_BUFFER	; try alloc flo object
	cmp	TEMP, CONT
	ja	%%NOROOM
	sub	TEMP, SCE_BUFFER
	fld qword [RESULT] 	; load fp arg
	mov	RESULT, TEMP
	mov	[GLOBALS+G_ETOP], TEMP ; commit allocation
	mov dword [RESULT-16], (12 << 8 | FLONUM_HDR) ; stash hdr bits
	%1					      ; perform fp comp
	fstp qword [RESULT-8]	; store the computation result in flo object
	lea	RESULT, [RESULT-16+BVEC_TAG] ; set the tag
	ret
%%NOROOM:
	;; No room to allocate flo object; give up
%%NOFLO:
	;; Not floating point inputs
	;; Restore RESULT before invoking C support routine
	lea	RESULT, [RESULT - (8 - BVEC_TAG)]
%endmacro

;;; generic_fl_cmp eql-lit less-lit greater-lit indeterm-lit
;;; performs no checking: the operands are known to be flonums
	
%macro generic_fl_cmp 4
	;mov	TEMP, SECOND	; TEMP aliases SECOND
	lea	RESULT, [RESULT + (8 - BVEC_TAG)]
	lea	TEMP, [TEMP + (8 - BVEC_TAG)]
	fld qword [TEMP]
	fld qword [RESULT]
	fcompp			; compare and pop both ST(0) & ST(1)
	fstsw ax		; retrieve cmp result in ax
	fwait
	sahf			; transfer ax to cond codes
	jpe	%%INDETERM
	ja	%%GREATER
	jb	%%LESS
	;jz	%%EQL		; fall into %%EQL case
%%EQL:
	mov	RESULT, %1
	ret
%%LESS:
	mov	RESULT, %2
	ret
%%GREATER:
	mov	RESULT, %3
	ret
%%INDETERM:
	mov	RESULT, %4
	ret
%endmacro
	
;;; generic_fp_cmp eql-lit less-lit greater-lit indeterm-lit
%macro generic_fp_cmp 4
	mov	[GLOBALS+G_SECOND], SECOND
	;mov	TEMP, [GLOBALS+G_SECOND+4] ; TEMP alias's SECOND
	lea	TEMP, [SECOND + (8 - BVEC_TAG)]
	test	TEMP_LOW, 7
	lea	RESULT, [RESULT + (8 - BVEC_TAG)]
	jnz	%%NOFLO
	test	RESULT_LOW, 7
	jnz 	%%NOFLO
	mov	TEMP, [TEMP-8]
	cmp	TEMP_LOW, FLONUM_HDR
	jnz     %%NOFLO
	xor	TEMP_LOW, [RESULT-8] ; (opt: if bits match, then xor is zero)
	jnz     %%NOFLO
	;; Got here, got two floats!
	mov	TEMP, [GLOBALS+G_SECOND]
	lea	TEMP, [TEMP + (8 - BVEC_TAG)]
	fld qword [TEMP]
	fld qword [RESULT]
	fcompp			; compare and pop both ST(0) & ST(1)
	fstsw ax		; retreive cmp result in ax
	fwait
	sahf			; transfer ax to cond codes
	jpe	%%INDETERM
	ja	%%GREATER
	jb	%%LESS
	;jz	%%EQL		; fall into %%EQL case
%%EQL:
	mov	RESULT, %1
	ret
%%LESS:
	mov	RESULT, %2
	ret
%%GREATER:
	mov	RESULT, %3
	ret
%%INDETERM:
	mov	RESULT, %4
	ret
%%NOFLO:
	;; Not floating point inputs
	;; Restore RESULT before invoking C support routine
	lea	RESULT, [RESULT - (8 - BVEC_TAG)]
	mov	SECOND, [GLOBALS+G_SECOND]
%endmacro
	
	
;;; generic_fl_op opcode (binary operations)
;;; performs no checking: the operands are known to be flonums
	
%macro generic_fl_op 1
	lea	RESULT, [RESULT + (8 - BVEC_TAG)]
	mov	[GLOBALS+G_SECOND], SECOND ; TEMP aliases SECOND
	mov	TEMP, [GLOBALS+G_ETOP]
	add	TEMP, 16+SCE_BUFFER	; try alloc 4 words
	cmp	TEMP, CONT
	ja	%%NOROOM
	fld  qword [RESULT] 	; load the first fp arg
	sub	TEMP, SCE_BUFFER
	mov	RESULT, TEMP
	mov	[GLOBALS+G_ETOP], TEMP ; commit the allocation
	mov	TEMP, [GLOBALS+G_SECOND]
	mov  dword  [RESULT-16], (12 << 8 | FLONUM_HDR)	; stash header bits
	lea	TEMP, [TEMP + (8 - BVEC_TAG)]
	%1   qword [TEMP]	; perform the fp computation
	fstp qword [RESULT-8]	; store the computation result in flo object
	lea	RESULT, [RESULT-16+BVEC_TAG] ; set the tag 
	ret
%%NOROOM:	
	;; No room to allocate float; give up
	;; Restore RESULT and SECOND before invoking C support routine
	lea	RESULT, [RESULT - (8 - BVEC_TAG)]
	mov	SECOND, [GLOBALS+G_SECOND]
%endmacro
			
;;; generic_fp_op opcode (binary operations)
;;; performs full checking
	
%macro generic_fp_op 1
	mov	[GLOBALS+G_SECOND], SECOND
	;mov	TEMP, [GLOBALS+G_SECOND+4] ; TEMP alias's SECOND
	lea	TEMP, [SECOND + (8 - BVEC_TAG)]
	test	TEMP_LOW, 7
	lea	RESULT, [RESULT + (8 - BVEC_TAG)]
	jnz	%%NOFLO
	test	RESULT_LOW, 7
	jnz 	%%NOFLO
	mov	TEMP, [TEMP-8]
	cmp	TEMP_LOW, FLONUM_HDR
	jnz     %%NOFLO
	xor	TEMP_LOW, [RESULT-8] ; (opt: if bits match, then xor is zero)
	jnz     %%NOFLO
	;; Got here, got two floats!
	mov	TEMP, [GLOBALS+G_ETOP]
	add	TEMP, 16+SCE_BUFFER	; try alloc 4 words (to preserve alignment of etop)
	cmp	TEMP, CONT
	ja	%%NOROOM
	fld  qword [RESULT] 	; load the first fp arg
	sub	TEMP, SCE_BUFFER
	mov	RESULT, TEMP
	mov	[GLOBALS+G_ETOP], TEMP ; commit the allocation
	mov	TEMP, [GLOBALS+G_SECOND]
	mov  dword  [RESULT-16], (12 << 8 | FLONUM_HDR)	; stash header bits
	lea	TEMP, [TEMP + (8 - BVEC_TAG)]
	%1   qword [TEMP]	; perform the fp computation
	fstp qword [RESULT-8]	; store the computation result in flo object
	lea	RESULT, [RESULT-16+BVEC_TAG] ; set the tag 
	ret
%%NOROOM:	
	;; No room to allocate float; give up
%%NOFLO:	
	;; Not floating point inputs
	;; Restore RESULT before invoking C support routine
	lea	RESULT, [RESULT - (8 - BVEC_TAG)]
	mov	SECOND, [GLOBALS+G_SECOND]
%endmacro
			
	;; On entry, GLOBALS pointer is off by 4
	
PUBLIC i386_fladd
%ifdef OPTIMIZE_MILLICODE
	generic_fl_op fadd
%endif
PUBLIC i386_add
%ifdef OPTIMIZE_MILLICODE
	generic_fp_op fadd
%endif
	MC2gk	mc_add
	
PUBLIC i386_flsub
%ifdef OPTIMIZE_MILLICODE
	generic_fl_op fsub
%endif
PUBLIC i386_sub
%ifdef OPTIMIZE_MILLICODE
	generic_fp_op fsub
%endif
	MC2gk	mc_sub
	
PUBLIC i386_flmul
%ifdef OPTIMIZE_MILLICODE
	generic_fl_op fmul
%endif
PUBLIC i386_mul
%ifdef OPTIMIZE_MILLICODE
	generic_fp_op fmul
%endif
	MC2gk	mc_mul
	
PUBLIC i386_fldiv
%ifdef OPTIMIZE_MILLICODE
	generic_fl_op fdiv
%endif
PUBLIC i386_div
%ifdef OPTIMIZE_MILLICODE
	generic_fp_op fdiv
%endif
	MC2gk	mc_div
	
PUBLIC i386_quo
	MC2gk	mc_quo
	
PUBLIC i386_rem
	MC2gk	mc_rem
	
PUBLIC i386_neg
%ifdef OPTIMIZE_MILLICODE
	generic_fp_unary fchs
%endif
	MCgk	mc_neg
	
PUBLIC i386_abs
%ifdef OPTIMIZE_MILLICODE
	generic_fp_unary fabs
%endif
	MCgk	mc_abs
	
PUBLIC i386_flequalp
%ifdef OPTIMIZE_MILLICODE
	generic_fl_cmp TRUE_CONST, FALSE_CONST, FALSE_CONST, FALSE_CONST
%endif
PUBLIC i386_equalp
%ifdef OPTIMIZE_MILLICODE
	generic_fp_cmp TRUE_CONST, FALSE_CONST, FALSE_CONST, FALSE_CONST
%endif
	MC2gk	mc_equalp
	
PUBLIC i386_fllessp
%ifdef OPTIMIZE_MILLICODE
	generic_fl_cmp FALSE_CONST, TRUE_CONST, FALSE_CONST, FALSE_CONST
%endif
PUBLIC i386_lessp
%ifdef OPTIMIZE_MILLICODE
	generic_fp_cmp FALSE_CONST, TRUE_CONST, FALSE_CONST, FALSE_CONST
%endif
	MC2gk	mc_lessp
	
PUBLIC i386_flless_or_equalp
%ifdef OPTIMIZE_MILLICODE
	generic_fl_cmp TRUE_CONST, TRUE_CONST, FALSE_CONST, FALSE_CONST
%endif
PUBLIC i386_less_or_equalp
%ifdef OPTIMIZE_MILLICODE
	generic_fp_cmp TRUE_CONST, TRUE_CONST, FALSE_CONST, FALSE_CONST
%endif
	MC2gk	mc_less_or_equalp
	
PUBLIC i386_flgreaterp
%ifdef OPTIMIZE_MILLICODE
	generic_fl_cmp FALSE_CONST, FALSE_CONST, TRUE_CONST, FALSE_CONST
%endif
PUBLIC i386_greaterp
%ifdef OPTIMIZE_MILLICODE
	generic_fp_cmp FALSE_CONST, FALSE_CONST, TRUE_CONST, FALSE_CONST
%endif
	MC2gk	mc_greaterp
	
PUBLIC i386_flgreater_or_equalp
%ifdef OPTIMIZE_MILLICODE
	generic_fl_cmp TRUE_CONST, FALSE_CONST, TRUE_CONST, FALSE_CONST
%endif
PUBLIC i386_greater_or_equalp
%ifdef OPTIMIZE_MILLICODE
	generic_fp_cmp TRUE_CONST, FALSE_CONST, TRUE_CONST, FALSE_CONST
%endif
	MC2gk	mc_greater_or_equalp
	
PUBLIC i386_exact2inexact
	MCgk	mc_exact2inexact
	
PUBLIC i386_inexact2exact
	MCgk	mc_inexact2exact
	
PUBLIC i386_real_part
	MCg	mc_real_part
	
PUBLIC i386_imag_part
	MCg	mc_imag_part
	
PUBLIC i386_round
	MCgk	mc_round
	
PUBLIC i386_truncate
	MCgk	mc_truncate
	
PUBLIC i386_zerop
	MCg	mc_zerop
	
PUBLIC i386_complexp
	MCg	mc_complexp
	
PUBLIC i386_rationalp
	MCg	mc_rationalp
	
PUBLIC i386_integerp
	MCg	mc_integerp
	
PUBLIC i386_exactp
	MCg	mc_exactp
	
PUBLIC i386_inexactp
	MCg	mc_inexactp

PUBLIC i386_flonump
	general_tag2_predicate BVEC_TAG, FLONUM_HDR
	
;;; bignum-add-step!
;;;
;;; Arguments are in registers reg1 through reg5:
;;;
;;;     reg1 contains b, a bytevector-like bignum (little-endian 32-bit bigits)
;;;     reg2 contains c, a bytevector-like bignum (little-endian 32-bit bigits)
;;;     reg3 contains i, a bytevector (not bignum) index into b
;;;     reg4 contains j, a bytevector (not bignum) index into c
;;;     reg5 contains a 1-bit carry represented as a fixnum
;;;
;;; The step
;;; 
;;;     adds the 32 bits starting at b[i] to the 32 bits starting at c[j]
;;;     adds the carry to c[j]
;;;     returns the carry
;;;
;;; Uses reg1 through reg3 to hold raw bits, but clears them at end.

PUBLIC i386_bignum_add_step
	mov	TEMP, REG3
	sar	TEMP, 2
	add	REG1, TEMP
	mov	TEMP, [GLOBALS + G_REG4]
	sar	TEMP, 2
	add	REG2, TEMP
	mov	RESULT, [GLOBALS + G_REG5]
	sar	RESULT, 2
	mov	REG3, RESULT
	xor	RESULT, RESULT			; new carry = 0
	mov	TEMP, [REG2 + (4 - BVEC_TAG)]   ; c[j]
	add	TEMP, [REG1 + (4 - BVEC_TAG)]   ; c[j] + b[i]
	jnc	bignum_add_step_add_carry
	mov	RESULT, 4                       ; new carry = 1
bignum_add_step_add_carry:
	add	TEMP, REG3
	jnc	bignum_add_step_store
	mov	RESULT, 4                       ; new carry = 1
bignum_add_step_store:
	mov	[REG2 + (4 - BVEC_TAG)], TEMP   ; c[j]
	xor	TEMP, TEMP
	mov	REG1, TEMP
	mov	REG2, TEMP
	mov	REG3, TEMP
	ret
	
;;; bignum-subtract-step!
;;;
;;; Arguments are in registers reg1 through reg5:
;;;
;;;     reg1 contains b, a bytevector-like bignum (little-endian 32-bit bigits)
;;;     reg2 contains c, a bytevector-like bignum (little-endian 32-bit bigits)
;;;     reg3 contains i, a bytevector (not bignum) index into b
;;;     reg4 contains j, a bytevector (not bignum) index into c
;;;     reg5 contains a 1-bit carry (borrow) represented as a fixnum
;;;
;;; The step
;;; 
;;;     subtracts the 32 bits starting at b[i] from the 32 bits at c[j]
;;;     subtracts the carry (borrow) from c[j]
;;;     returns the new carry (borrow)
;;;
;;; Uses reg1 through reg3 to hold raw bits, but clears them at end.

PUBLIC i386_bignum_subtract_step
	mov	TEMP, REG3
	sar	TEMP, 2
	add	REG1, TEMP
	mov	TEMP, [GLOBALS + G_REG4]
	sar	TEMP, 2
	add	REG2, TEMP
	mov	RESULT, [GLOBALS + G_REG5]
	sar	RESULT, 2
	mov	REG3, RESULT
	xor	RESULT, RESULT			; new carry = 0
	mov	TEMP, [REG2 + (4 - BVEC_TAG)]   ; c[j]
	sub	TEMP, [REG1 + (4 - BVEC_TAG)]   ; c[j] + b[i]
	jnc	bignum_subtract_step_subtract_carry
	mov	RESULT, 4                       ; new carry = 1
bignum_subtract_step_subtract_carry:
	sub	TEMP, REG3
	jnc	bignum_subtract_step_store
	mov	RESULT, 4                       ; new carry = 1
bignum_subtract_step_store:
	mov	[REG2 + (4 - BVEC_TAG)], TEMP   ; c[j]
	xor	TEMP, TEMP
	mov	REG1, TEMP
	mov	REG2, TEMP
	mov	REG3, TEMP
	ret
	
;;; bignum-multiply-step!
;;;
;;; Arguments are in registers reg1 through reg6:
;;;
;;;     reg1 contains b, a bytevector-like bignum (little-endian 32-bit bigits)
;;;     reg2 contains c, a bytevector-like bignum (little-endian 32-bit bigits)
;;;     reg3 contains i, a bytevector (not bignum) index into b
;;;     reg4 contains j, a bytevector (not bignum) index into c
;;;     reg5 contains k, a 4-byte bytevector
;;;     reg6 contains carry, a 4-byte bytevector
;;;
;;; The step
;;; 
;;; 	multiplies the 32 bits starting at b[i] by the 32 bits in k
;;;     adds the 32 bits starting at c[j]
;;;     adds the 32 bits in carry
;;;     stores the high-order 32 bits of that result into carry
;;;     stores the low-order 32 bits of that result into c[j]
;;;
;;; No result is returned.
;;;
;;; The IA32 multiply instruction multiplies eax (TEMP/SECOND) by a 32-bit
;;; operand to obtain a 64-bit unsigned result.
;;; The IA32 multiply instruction places that 64-bit result in edx:eax,
;;; with the high-order bits in edx.  edx is reg2, and eax is TEMP/SECOND.
;;; c (in reg2) must be saved around the multiplication.
;;; eax and edx are roots, so they must be cleared before returning.

PUBLIC i386_bignum_multiply_step
	mov	[GLOBALS + G_REG1], REG1
	mov	[GLOBALS + G_REG2], REG2
	mov	RESULT, REG3
	sar	RESULT, 2
	add	RESULT, REG1
	mov	eax, [RESULT + (4 - BVEC_TAG)]  ; b[i]
	mov	REG1, REG2                      ; c is now in reg1
	mov	RESULT, [GLOBALS + G_REG5]
	mov	RESULT, [RESULT + (4 - BVEC_TAG)] ; k
	xor	edx, edx
	mul	RESULT				; edx:eax = b[i] * k
	mov	RESULT, [GLOBALS + G_REG4]
	sar	RESULT, 2
	add	RESULT, REG1
	add	eax, [RESULT + (4 - BVEC_TAG)]
	jnc	multiply_step_add_carry
	add	edx, 1                          ; edx:eax = b[i] * k + c[j]
multiply_step_add_carry:
	mov	RESULT, [GLOBALS + G_REG6]
	add	eax, [RESULT + (4 - BVEC_TAG)]
	jnc	multiply_step_store
	add	edx, 1                          ; edx:eax = b[i]*k+c[j]+carry
multiply_step_store:
	mov	[RESULT + (4 - BVEC_TAG)], edx  ; carry
	mov	RESULT, [GLOBALS + G_REG4]
	sar	RESULT, 2
	add	RESULT, REG1
	mov	[RESULT + (4 - BVEC_TAG)], eax  ; c[j]
	xor	edx, edx
	xor	eax, eax
	mov	REG1, [GLOBALS + G_REG1]
	mov	REG2, [GLOBALS + G_REG2]
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PUBLIC i386_exception				; Exn encoded in instr stream
	mov	[GLOBALS+G_SECOND], SECOND
	mov	SECOND, [esp]			; Exn code address
	mov	ax, [SECOND]			; Exn code
	and	eax, 0xFFFF			;   is 16 bits
	shl	eax, 2				;     encoded as fixnum
	jmp	i386_signal_exception_intrsafe
	
PUBLIC i386_global_exception			; RESULT holds the global cell
	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	mov	SECOND, fixnum(EX_UNDEF_GLOBAL)
	jmp	i386_signal_exception_intrsafe
	
PUBLIC i386_invoke_exception			; RESULT holds defined value
	cmp	dword [GLOBALS+G_TIMER], 0
	jnz	Linv1
	jmp	EXTNAME(i386_timer_exception)
Linv1:	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	mov	SECOND, fixnum(EX_NONPROC)
	jmp	i386_signal_exception_intrsafe
	
PUBLIC i386_global_invoke_exception		; RESULT holds the global cell
	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	cmp	dword [GLOBALS+G_TIMER], 0
	jnz	Lginv1
	jmp	EXTNAME(i386_timer_exception)
Lginv1:	cmp	dword [RESULT-PAIR_TAG], UNDEFINED_CONST
	jnz	Lginv2
	mov	SECOND, fixnum(EX_UNDEF_GLOBAL)
	jmp	i386_signal_exception_intrsafe
Lginv2:	mov	RESULT, [RESULT-PAIR_TAG]
	mov	SECOND, fixnum(EX_NONPROC)
	jmp	i386_signal_exception_intrsafe
	
PUBLIC i386_argc_exception			; RESULT holds actual arg cnt
%ifdef REG0
	mov	dword [GLOBALS+G_THIRD], REG0
%else
        mov     SECOND, [GLOBALS+G_REG0]
	mov	dword [GLOBALS+G_THIRD], SECOND
%endif
	mov	SECOND, fixnum(EX_ARGC)
	jmp	i386_signal_exception_intrsafe
	
PUBLIC i386_petit_patch_boot_code
%ifdef X86_SASSY
	int3
%else
	MCg	mc_petit_patch_boot_code
%endif

%macro INTERNAL_FIXNUM_TO_RETADDR 0
	mov	[saved_temp_reg], eax
	CHECKZEROREG 0, 1
%ifdef REG0
	mov	eax, dword [REG0 - PROC_TAG + PROC_CODEVECTOR_NATIVE]
%else
	mov	eax, dword [GLOBALS + G_REG0]
	mov	eax, dword [eax - PROC_TAG + PROC_CODEVECTOR_NATIVE]
%endif
	add	eax, BVEC_HEADER_BYTES - BVEC_TAG
	add	dword [GLOBALS + G_RETADDR], eax
	mov	eax, [saved_temp_reg]
%endmacro

%macro INTERNAL_RETADDR_TO_FIXNUM 0
	mov	[saved_temp_reg], eax
	CHECKZEROREG 0, 2
%ifdef REG0
	mov	eax, dword [REG0 - PROC_TAG + PROC_CODEVECTOR_NATIVE]
%else
	mov	eax, dword [GLOBALS + G_REG0]
	mov	eax, dword [eax - PROC_TAG + PROC_CODEVECTOR_NATIVE]
%endif
	add	eax, BVEC_HEADER_BYTES - BVEC_TAG
	sub	dword [GLOBALS + G_RETADDR], eax
	mov	eax, [saved_temp_reg]
%endmacro

%macro SAVE_STATE 1
 %ifdef X86_SASSY
	INTERNAL_RETADDR_TO_FIXNUM
 %endif
	SAVE_STATE_ABSOLUTE %1
%endmacro

%macro RESTORE_STATE 1
	RESTORE_STATE_ABSOLUTE %1
 %ifdef X86_SASSY
	INTERNAL_FIXNUM_TO_RETADDR
 %endif
%endmacro

%macro SAVE_STATE_ABSOLUTE 1
	mov	[%1], GLOBALS
	mov	[GLOBALS + G_STKP], CONT
	mov	[GLOBALS + G_RESULT], RESULT
%ifdef REG0
	mov	[GLOBALS + G_REG0], REG0
%endif
	mov	[GLOBALS + G_REG1], REG1
	mov	[GLOBALS + G_REG2], REG2
	mov	[GLOBALS + G_REG3], REG3
%ifdef REG4
	mov	[GLOBALS + G_REG4], REG4
%endif
%endmacro

%macro RESTORE_STATE_ABSOLUTE 1
	mov	GLOBALS, [%1]
	mov	CONT, [GLOBALS + G_STKP]
	mov	RESULT, [GLOBALS + G_RESULT]
%ifdef REG0
	mov	REG0, [GLOBALS + G_REG0]
%endif
	mov	REG1, [GLOBALS + G_REG1]
	mov	REG2, [GLOBALS + G_REG2]
	mov	REG3, [GLOBALS + G_REG3]
%ifdef REG4
	mov	REG4, [GLOBALS + G_REG4]
%endif
%endmacro

%macro CALLOUT_TO_C 1
	mov	ebx, GLOBALS
	mov	esp, [ebx+G_SAVED_ESP]
	mov	ecx, [ebx+G_RETADDR]	
	push	0		; stack on OS X must be 16-byte aligned
	push	ecx
	push	ebx		; globals
	call	eax
	add	esp, 12
%endmacro

;;; i386_signal_exception
;;;	Signal the exception by calling signal_exception in the
;;;	C millicode.
;;;
;;; On entry:
;;;	RESULT has first value
;;;	globals[G_SECOND] has second value
;;;	globals[G_THIRD] has third value
;;;	SECOND has fixnum exception code
;;;	globals[-1] has the unadjusted return address

	;; On entry, esp is off by 4	
i386_signal_exception_intrsafe:
	shr	SECOND, 2			; fixnum -> native
	mov	[tmp_exception_code], SECOND    ; SECOND=eax
	SAVE_RETURN_ADDRESS_INTRSAFE		; compute G_RETADDR, fixup globals
	SAVE_STATE saved_globals_pointer	; forces RESULT into GLOBALS
	mov	ebx, GLOBALS
	mov	esp, [ebx+G_SAVED_ESP]
	push	0		; stack on OS X must be 16-byte aligned
	push	dword [tmp_exception_code]	; exception code
	push	ebx				; globals
	call	EXTNAME(mc_exception)
	add	esp, 12
	RESTORE_STATE saved_globals_pointer
	jmp	[GLOBALS + G_RETADDR]

;;; callout_to_C
;;;	Switch from Scheme to C mode and call a C function, then
;;;	restore Scheme mode and return.  Do not change the Scheme
;;;	continuation; C code may longjump out of the call.
;;;
;;;	On entry, eax/TEMP/SECOND contains the address of the C
;;; 	procedure to call, and the scheme return address has been saved
;;;	in G_RETADDR.

callout_to_C:
	SAVE_STATE saved_globals_pointer
	CALLOUT_TO_C 0
	RESTORE_STATE saved_globals_pointer
	jmp	[GLOBALS + G_RETADDR]

callout_to_C_wb:
	SAVE_STATE saved_globals_pointer
	CALLOUT_TO_C 0
	RESTORE_STATE saved_globals_pointer
	mov	RESULT, [GLOBALS+G_WBDEST]	; Restore
	mov	SECOND, [GLOBALS+G_WBVALUE]	;   state
	jmp	[GLOBALS + G_RETADDR]

callout_to_C_leave_retaddr_absolute:
	SAVE_STATE_ABSOLUTE saved_globals_pointer
	CALLOUT_TO_C 0
	RESTORE_STATE_ABSOLUTE saved_globals_pointer
	jmp	[GLOBALS + G_RETADDR]

callout_to_Ck:
	SAVE_STATE saved_globals_pointer
	CALLOUT_TO_C 1
	RESTORE_STATE saved_globals_pointer
	jmp	[GLOBALS + G_RETADDR]
	

	section	.data

saved_fpu_ctrl:
	dd	0
saved_temp_reg:
	dd	0
saved_globals_pointer:
	dd	0
tmp_exception_code:
	dd	0

;;; eof

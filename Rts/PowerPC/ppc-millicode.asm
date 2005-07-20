;;; PowerPC millicode entry points and millicode jump vector initialization.
;;;
;;; $Id$
;;;
;;; Most millicode entry points just jump to the C implementation.
;;; Performance critical routines need to be hand-coded in assembler
;;; or inlined.
;;;
;;; For more information about the PowerPC back-end, see ppc-machine.ah.
	
;;; Jump to a compiled Scheme procedure.
;;; 
;;; First parameter:  address to jump to
;;; Second parameter: globals ptr

EXTNAME(ppc_scheme_jump):

	;; Save all callee-saves registers
	;; 
	;; FIXME: save floating registers too, but OK for now since
	;; all FP arithmetic is done in C, where the C compiler handles
	;; it.
	
	subfic	r1, r1, 4*(2+32-13)	; two arguments, callee-save regs
	stw	r3,   0(r1)
	stw	r4,   4(r1)
	stw	r13,  8(r1)
	stw	r14, 12(r1)
	stw	r15, 16(r1)
	stw	r16, 20(r1)
	stw	r17, 24(r1)
	stw	r18, 28(r1)
	stw	r19, 32(r1)
	stw	r20, 36(r1)
	stw	r21, 40(r1)
	stw	r22, 43(r1)
	stw	r23, 48(r1)
	stw	r24, 52(r1)
	stw	r25, 56(r1)
	stw	r26, 60(r1)
	stw	r27, 64(r1)
	stw	r28, 68(r1)
	stw	r29, 72(r1)
	stw	r30, 76(r1)
	stw	r31, 80(r1)

	;; Load the machine state
	
	lwz	GLOBALS, 8(r1)
	lwz	RESULT, G_RESULT(GLOBALS)
	xor	SECOND, SECOND, SECOND
	lwz	CONT, G_CONT(GLOBALS)
	lwz	TIMER, G_TIMER(GLOBALS)
	lwz	REG0, G_REG0(GLOBALS)
	lwz	REG1, G_REG1(GLOBALS)
	lwz	REG2, G_REG2(GLOBALS)
	lwz	REG3, G_REG3(GLOBALS)
	lwz	REG4, G_REG4(GLOBALS)
	lwz	REG5, G_REG5(GLOBALS)
	lwz	REG6, G_REG6(GLOBALS)
	lwz	REG7, G_REG7(GLOBALS)
	lwz	REG8, G_REG8(GLOBALS)
	lwz	REG9, G_REG9(GLOBALS)
	lwz	REG10, G_REG10(GLOBALS)
	lwz	REG11, G_REG11(GLOBALS)
	lwz	REG12, G_REG12(GLOBALS)
	lwz	REG13, G_REG13(GLOBALS)
	lwz	REG14, G_REG14(GLOBALS)
	lwz	REG15, G_REG15(GLOBALS)
	lwz	REG16, G_REG16(GLOBALS)
	lwz	REG17, G_REG17(GLOBALS)
	lwz	REG18, G_REG18(GLOBALS)
	lwz	REG19, G_REG19(GLOBALS)
	lwz	REG20, G_REG20(GLOBALS)
	lwz	REG21, G_REG21(GLOBALS)
	lwz	REG22, G_REG22(GLOBALS)
	lwz	REG23, G_REG23(GLOBALS)

	;; Jump to Scheme code

	lwz	TEMP, 4(r1)
	mtlr	TEMP
	blr

PUBLIC(ppc_alloc_bv)
	MCg(mc_alloc_bv)
	
PUBLIC(ppc_alloc)
	MCg(mc_alloc)
	
PUBLIC(ppc_alloci)
	MC2g(mc_alloci)
	
PUBLIC(ppc_morecore)
	MCg(mc_morecore)
	
PUBLIC(ppc_stack_overflow)
	MILLICODE_STUB(0, `mc_stack_overflow', `callout_to_C')
	
PUBLIC(ppc_capture_continuation)
	MCg(mc_capture_continuation)
	
PUBLIC(ppc_restore_continuation)
	MCg(mc_restore_continuation)
	
PUBLIC(ppc_full_barrier)
	MC2g(mc_full_barrier)
	
PUBLIC(ppc_partial_barrier)
	MC2g(mc_partial_barrier)
	
PUBLIC(ppc_break)
	MCg(mc_break)
	
PUBLIC(ppc_timer_exception)
	MCgk(mc_timer_exception)
	
PUBLIC(ppc_enable_interrupts)
	MCgk(mc_enable_interrupts)
	
PUBLIC(ppc_disable_interrupts)
	MCgk(mc_disable_interrupts)
	
PUBLIC(ppc_apply)
	MC2g(mc_apply)
	
PUBLIC(ppc_restargs)
	MC2g(mc_restargs)
	
PUBLIC(ppc_syscall)
	MC2gk(mc_syscall)
	
PUBLIC(ppc_typetag)
	MCg(mc_typetag)
	
PUBLIC(ppc_typetag_set)
	MC2g(mc_typetag_set)
	
PUBLIC(ppc_eqv)
	MC2gk(mc_eqv)
	
PUBLIC(ppc_partial_list2vector)
	MC2g(mc_partial_list2vector)
	
PUBLIC(ppc_bytevector_like_fill)
	MC2g(mc_bytevector_like_fill)
	
PUBLIC(ppc_bytevector_like_compare)
	MC2g(mc_bytevector_like_compare)
	
PUBLIC(ppc_add)
	MC2gk(mc_add)
	
PUBLIC(ppc_sub)
	MC2gk(mc_sub)
	
PUBLIC(ppc_mul)
	MC2gk(mc_mul)
	
PUBLIC(ppc_div)
	MC2gk(mc_div)
	
PUBLIC(ppc_quo)
	MC2gk(mc_quo)
	
PUBLIC(ppc_rem)
	MC2gk(mc_rem)
	
PUBLIC(ppc_neg)
	MCgk(mc_neg)
	
PUBLIC(ppc_abs)
	MCgk(mc_abs)
	
PUBLIC(ppc_equalp)
	MC2gk(mc_equalp)
	
PUBLIC(ppc_lessp)
	MC2gk(mc_lessp)
	
PUBLIC(ppc_less_or_equalp)
	MC2gk(mc_less_or_equalp)
	
PUBLIC(ppc_greaterp)
	MC2gk(mc_greaterp)
	
PUBLIC(ppc_greater_or_equalp)
	MC2gk(mc_greater_or_equalp)
	
PUBLIC(ppc_exact2inexact)
	MCgk(mc_exact2inexact)
	
PUBLIC(ppc_inexact2exact)
	MCgk(mc_inexact2exact)
	
PUBLIC(ppc_real_part)
	MCg(mc_real_part)
	
PUBLIC(ppc_imag_part)
	MCg(mc_imag_part)
	
PUBLIC(ppc_round)
	MCgk(mc_round)
	
PUBLIC(ppc_truncate)
	MCgk(mc_truncate)
	
PUBLIC(ppc_zerop)
	MCg(mc_zerop)
	
PUBLIC(ppc_complexp)
	MCg(mc_complexp)
	
PUBLIC(ppc_rationalp)
	MCg(mc_rationalp)
	
PUBLIC(ppc_integerp)
	MCg(mc_integerp)
	
PUBLIC(ppc_exactp)
	MCg(mc_exactp)
	
PUBLIC(ppc_inexactp)
	MCg(mc_inexactp)
	
PUBLIC(ppc_petit_patch_boot_code)
	MCg(mc_petit_patch_boot_code)

PUBLIC(ppc_exception)				; Exn encoded in instr stream
	add	esp, 4				; Fixup GLOBALS
	mov	[GLOBALS+G_SECOND], SECOND
	mov	SECOND, [GLOBALS-4]		; Exn code address
	mov	ax, [SECOND]			; Exn code
	and	eax, 0xFFFF			;   is 16 bits
	shl	eax, 2				;     encoded as fixnum
	jmp	ppc_signal_exception
	
PUBLIC(ppc_global_exception)			; RESULT holds the global cell
	add	esp, 4				; Fixup GLOBALS
	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	mov	SECOND, EX_UNDEF_GLOBAL
	jmp	ppc_signal_exception
	
PUBLIC(ppc_invoke_exception)			; RESULT holds defined value
	add	esp, 4				; Fixup GLOBALS
	cmp	dword [GLOBALS+G_TIMER], 0
	jnz	Linv1
	sub	esp, 4
	jmp	ppc_timer_exception
Linv1:	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	mov	SECOND, EX_NONPROC
	jmp	ppc_signal_exception
	
PUBLIC(ppc_global_invoke_exception)		; RESULT holds the global cell
	add	esp, 4				; Fixup GLOBALS
	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	cmp	dword [GLOBALS+G_TIMER], 0
	jnz	Lginv1
	sub	esp, 4
	jmp	ppc_timer_exception
Lginv1:	cmp	dword [RESULT-PAIR_TAG], UNDEFINED_CONST
	jnz	Lginv2
	mov	SECOND, EX_UNDEF_GLOBAL
	jmp	ppc_signal_exception	
Lginv2:	mov	RESULT, [RESULT-PAIR_TAG]
	mov	SECOND, EX_NONPROC
	jmp	ppc_signal_exception
	
PUBLIC(ppc_argc_exception)			; RESULT holds actual arg cnt
	add	esp, 4				; Fixup GLOBALS
	mov	dword [GLOBALS+G_SECOND], FALSE_CONST
	mov	SECOND, EX_ARGC
	jmp	ppc_signal_exception
	
define(`SAVE_STATE',
`	mov	[$1], GLOBALS
	mov	[GLOBALS + G_STKP], CONT
	mov	[GLOBALS + G_RESULT], RESULT
	mov	[GLOBALS + G_REG1], REG1
	mov	[GLOBALS + G_REG2], REG2
	mov	[GLOBALS + G_REG3], REG3
	mov	[GLOBALS + G_REG4], REG4
')

define(`RESTORE_STATE',
`	mov	GLOBALS, [$1]
	mov	CONT, [GLOBALS + G_STKP]
	mov	RESULT, [GLOBALS + G_RESULT]
	mov	REG1, [GLOBALS + G_REG1]
	mov	REG2, [GLOBALS + G_REG2]
	mov	REG3, [GLOBALS + G_REG3]
	mov	REG4, [GLOBALS + G_REG4]
')

define(`CALLOUT_TO_C',
`	mov	ebx, GLOBALS
	mov	esp, [ebx+G_SAVED_ESP]
	ifelse($1,1,
	`mov	ecx, [ebx+G_RETADDR]	
	push	ecx')
	push	ebx		; globals
	call	eax
	ifelse($1,1,
	`add	esp, 8',
	`add	esp, 4')
')

;;; ppc_signal_exception
;;;	Signal the exception by calling signal_exception in the
;;;	C millicode.
;;;
;;; On entry:
;;;	RESULT has first value
;;;	globals[G_SECOND] has second value
;;;	globals[G_THIRD] has third value
;;;	SECOND has fixnum exception code
;;;	globals[-1] has the unadjusted return address

ppc_signal_exception:
	shr	SECOND, 2			; fixnum -> native
	mov	[tmp_exception_code], SECOND    ; SECOND=eax
	SAVE_RETURN_ADDRESS			; compute G_RETADDR
	SAVE_STATE saved_globals_pointer	; forces RESULT into GLOBALS
	mov	ebx, GLOBALS
	mov	esp, [ebx+G_SAVED_ESP]
	push	dword [tmp_exception_code]	; exception code
	push	ebx				; globals
	call	mc_exception
	add	esp, 8
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

callout_to_Ck:
	SAVE_STATE saved_globals_pointer
	CALLOUT_TO_C 1
	RESTORE_STATE saved_globals_pointer
	jmp	[GLOBALS + G_RETADDR]
	

	section	.data

saved_globals_pointer:
	dd	0
tmp_exception_code:
	dd	0

;;; eof
	
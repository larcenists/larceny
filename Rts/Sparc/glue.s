! Rts/Sparc/glue.s
! Larceny Run-time System (SPARC) -- context switching and glue code.
!
! $Id: glue.s,v 1.2 1997/08/22 21:12:21 lth Exp $
!
! To fix:
! * The register save/restore code used in scheme-to-scheme calls needs to
!   be faster.

#include "asmdefs.h"
#include "asmmacro.h"

	.seg	"text"

	! Scheme entry point

	.global	EXTNAME(scheme_start)

	! Runtime-system internal procedures

	.global callout_to_C			! call C, return to Scheme
	.global	internal_scheme_call		! call Scheme, return to Scheme
	.global internal_callout_to_C		! call C, return to mcode
	.global internal_save_scheme_context	! enter Scheme mode
	.global internal_restore_scheme_context	! leave Scheme mode
	.global	internal_restore_globals	! restore global regs
	.global internal_push			! save a value
	.global internal_pop			! restore that value
	.global internal_save_vm_regs		! save R0..Rn
	.global internal_restore_vm_regs	! restore R0..Rn
	.global	internal_retaddr2fixnum		! make retaddr relocatable
	.global internal_fixnum2retaddr		! make retaddr absolute
	.global internal_check_signals		! check Unix signals

! _scheme_start: Scheme VM entry point.
!
! Call from: C
! Input    : globals[]
! Output   : globals[]
! Destroys : Caller's %o and %g caller-save registers.
!
! _scheme_start is called from the larceny_call().  It sets up the virtual
! machine and then calls the procedure in globals[ G_REG0 ].  If that
! procedure returns, then _scheme_start returns to its caller.
! 
! Larceny_call() must allocate a stack frame and must also place arguments
! in the register save area in globals[], and must set up the argument
! count in globals[ G_RESULT ].

EXTNAME(scheme_start):
	save	%sp, -96, %sp			! Standard stack frame
	st	%i7, [ %i6-4 ]			! Save return address

	set	EXTNAME(globals), %l0		! Pointer to globals[]
	call	internal_restore_scheme_context	! Enter Scheme mode
	clr	[ %l0 + G_RETADDR ]		! Return offset 0 for sanity

	set	L1-8, %TMP0			! Store the return
	st	%TMP0, [ %STKP+4 ]		!   address in the frame

	ld	[ %REG0 - PROC_TAG + CODEVECTOR ], %TMP0
	jmp	%TMP0 - BVEC_TAG + CODEOFFSET	! Call Scheme procedure
	nop

L1:	call	internal_save_scheme_context	! Enter C mode
	nop

	ld	[ %i6-4 ], %i7			! Restore return address
	ret
	restore


! internal_scheme_call: call Scheme from millicode, return to Scheme.
!
! Call from: millicode
! Inputs   : RESULT, ARGREG2, ARGREG3, TMP0 = objects: arguments to procedure
!            TMP1 = argument count (0..4)
!            TMP2 = vector index into CALLOUTS
!            o7 = scheme return address
!            globals[ G_SCHCALL_SAVERES ] = boolean (RESULT restore flag)
! Outputs  : Unspecified
! Destroys : Temporaries, (RESULT), ARGREG2, ARGREG3
!
! Note that the calling conventions briefly violate the VM invariant
! by keeping a root in TMP0.
!
! Call Scheme when the VM is in Scheme mode already. The problem here is
! that when Scheme code calls a millicode procedure, it is not required to
! save any of its registers.  Thus, when the millicode must call out to 
! Scheme, the caller's context must be saved before the new Scheme procedure
! is invoked.  This context must also be restored before control is returned
! to the original caller, and, again, the original caller will not do this,
! so we must arrange for it to happen.
!
! To accomodate interrupts, the contents of RESULT is saved and restored iff 
! globals[ G_SCHCALL_SAVERES ] == #t on entry to this procedure.
!
! We create a special stack frame, shown below. The frame's return address
! points to the millicode procedure internal_scheme_return (defined below); 
! the save area contains the return offset from R0 as a fixnum and the saved
! registers.
!
!     +------------------------------------------------+  <- tos
!     | (frame size)                                   |
!     | (retaddr - to internal_scheme_return)          |
!     | (dynlink - garbage)                            |
!     | 0                                              |
!     | (scheme return address as offset off saved R0) |
!     | (saved R0)                                     |
!     | (saved R1)                                     |
!     | ...                                            |
!     | (saved R31)                                    |
!     | (saved RESULT)                                 |
!     | (saved RESULT restore flag)                    |
!     +------------------------------------------------+

! FIXME: these should be parameterized by the number of registers to save,
! and the code below assumes some fixed positions also.

! REALFRAMESIZE is what we bump stkp by: full header plus pad word
#define S2S_REALFRAMESIZE	(5+34)*4+4

! FRAMESIZE is what we store in the frame: sans header or pad word
#define S2S_FRAMESIZE		(4+34)*4

! BASE is the offset of REG0 in the stack frame
#define S2S_BASE		20

internal_scheme_call:

	! This procedure is not reentrant.

	st	%o7, [ %GLOBALS + G_RETADDR ]		! in case of stk oflo

	! First, save all state in private temporaries.  We use private
	! temporaries since a stack overflow may need to use the common
	! system temporaries (like G_RETADDR).  However, we may assume that
	! RESULT, ARGREG2, and ARGREG3 will not be clobbered during
	! a stack overflow.

	cmp	%TMP1, 4				! test: store TMP0?
	be,a	1f					! if so, then
	st	%TMP0, [ %GLOBALS + G_SCHCALL_ARG4 ]	!   save it in a root
1:	st	%TMP1, [ %GLOBALS + G_SCHCALL_ARGC ]	! always save TMP1
	st	%TMP2, [ %GLOBALS + G_SCHCALL_PROCIDX ]	! always save TMP2
	mov	%o7, %TMP0				! convert
	call	internal_retaddr2fixnum			!   return address
	nop						!     to a fixnum
	st	%TMP0, [ %GLOBALS + G_SCHCALL_RETADDR ] ! save it

	! Create stack frame.

Ls2s2:	sub	%STKP, S2S_REALFRAMESIZE, %STKP		! try to allocate
	cmp	%STKP, %STKLIM				! check overflow;
	bge	Ls2s3					!   skip if not
	nop
	add	%STKP, S2S_REALFRAMESIZE, %STKP		! un-try and
	call	EXTNAME(mem_internal_stkoflow)		!   handle overflow;
	sub	%o7, (.-Ls2s2)+4, %o7			!     return to Ls2s2

	! Initialize stack frame header

Ls2s3:	set	S2S_FRAMESIZE, %TMP0			! store
	st	%TMP0, [ %STKP ]			!   frame size
	set	internal_scheme_return, %TMP0		! store
	st	%TMP0, [ %STKP+4 ]			!   retaddr
	st	%g0, [ %STKP+8 ]			! dynlink
	st	%g0, [ %STKP+12 ]			! proc (fixed)
	ld	[ %GLOBALS + G_SCHCALL_RETADDR ], %TMP0	! get return address
	st	%TMP0, [ %STKP+16 ]			!   and store it

	! Save registers in frame -- this can be made faster.

	call	internal_save_vm_regs			! flush R0...Rn
	nop
	add	%GLOBALS, G_REG0, %TMP0			! source ptr
	call	copyregs				! copy regs to frame
	add	%STKP, S2S_BASE, %TMP1			! destination ptr

	! Save RESULT and the RESULT restore flag; clear the flag.

	st	%RESULT, [ %STKP+S2S_BASE+(32*4) ]	! save RESULT always
	ld	[ %GLOBALS + G_SCHCALL_SAVERES ], %TMP2 ! get restore flag
	st	%TMP2, [ %STKP+S2S_BASE+(33*4) ]	!   and save it
	st	%g0, [ %GLOBALS + G_SCHCALL_SAVERES ]	! clear flag

	! Set up the arguments to the procedure.

	mov	%RESULT, %REG1
	mov	%ARGREG2, %REG2
	mov	%ARGREG3, %REG3
	ld	[ %GLOBALS + G_SCHCALL_ARGC ], %TMP2
	cmp	%TMP2, 4
	be,a	.+8
	ld	[ %GLOBALS + G_SCHCALL_ARG4 ], %REG4

	! Get the procedure to call.

	ld	[ %GLOBALS + G_SCHCALL_PROCIDX ], %TMP0
	ld	[ %GLOBALS + G_CALLOUTS ], %TMP1
	ld	[ %TMP1 - GLOBAL_CELL_TAG + CELL_VALUE_OFFSET ], %TMP1
	cmp	%TMP1, UNDEFINED_CONST
	be	Ls2s5
	nop
	add	%TMP1, 4 - VEC_TAG, %TMP1
	ld	[ %TMP1 + %TMP0 ], %REG0		! the procedure!

	! Call it.

	ld	[ %REG0 - PROC_TAG + CODEVECTOR ], %TMP0
	jmp	%TMP0 - BVEC_TAG + CODEOFFSET		! wheeee!
	sll	%TMP2, 2, %RESULT			! argc

! Error: no millicode vector present.
Ls2s5:
	set	Ls2serror, %TMP1
	set	EXTNAME(C_panic), %TMP0
	b	internal_callout_to_C
	nop
	! never returns

	.seg	"data"
Ls2serror:
	.asciz	"internal_scheme_call: no callout vector present."
	.seg	"text"


! copyregs: Routine used by scheme_call and scheme_return to copy registers
!
! Input:    TMP0 = source ptr
!           TMP1 = dest ptr
! Output:   Nothing
! Destroys: TMP0, TMP1, TMP2
!
! Copies 32 words (the VM registers) as quickly as possible. 
! FIXME: Should unroll fully.

copyregs:
	mov	8, %TMP2			! counter
Lcopyreg:
	ld	[ %TMP0 + 0 ], %REG0
	ld	[ %TMP0 + 4 ], %REG1
	st	%REG0, [ %TMP1 + 0 ]
	ld	[ %TMP0 + 8 ], %REG2
	st	%REG1, [ %TMP1 + 4 ]
	ld	[ %TMP0 + 12 ], %REG3
	st	%REG2, [ %TMP1 + 8 ]
	deccc	1, %TMP2
	st	%REG3, [ %TMP1 + 12 ]
	inc	16, %TMP0
	bne	Lcopyreg
	inc	16, %TMP1
	jmp	%o7+8
	nop


! internal_scheme_return: return from callout made through the
!   internal_scheme_call procedure, above.
!
! Call from: Don't. This procedure is always returned into.
! Inputs   : None.
! Outputs  : None.
! Destroys : Temporaries.
!
! See comments for internal_scheme_call, above.
!
! After popping the frame we must force another frame into the cache
! if the cache is empty, or we violate subtle invariants on the stack
! cache scheme.

internal_scheme_return:
	! two pad words: procedures return to internal_scheme_return+8
	nop
	nop

	! Restore registers from the stack frame.

	add	%STKP, S2S_BASE, %TMP0		! source ptr
	call	copyregs
	add	%GLOBALS, G_REG0, %TMP1		! dest ptr
	call	internal_restore_vm_regs
	nop

	! Conditionally restore the value of RESULT.

	ld	[ %STKP+S2S_BASE+(33*4) ], %TMP0
	cmp	%TMP0, TRUE_CONST
	be,a	.+8
	ld	[ %STKP+S2S_BASE+(32*4) ], %RESULT

	! Get and compute the return address.

	call	internal_fixnum2retaddr
	ld	[ %STKP + 16 ], %TMP0
	mov	%TMP0, %o7

	! Pop the frame.

	add	%STKP, S2S_REALFRAMESIZE, %STKP

	! Force the presence of a stack frame on the stack.

	ld	[ %GLOBALS + G_STKBOT ], %TMP0
	cmp	%STKP, %TMP0
	bne	Ls2srtn2
	nop
	st	%o7, [ %GLOBALS + G_RETADDR ]
	set	EXTNAME(C_restore_frame), %TMP0
	call	internal_callout_to_C
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7
Ls2srtn2:
	jmp	%o7+8
	nop


! callout_to_C: call a C procedure and return to Scheme.
!
! Call from: millicode
! Input:     TMP0 = address of C procedure
!            TMP1 = first argument
!            TMP2 = second argument
! Output:    Nothing.
! Destroys:  Temporaries

callout_to_C:
	st	%o7, [ %GLOBALS + G_RETADDR ]
	st	%TMP0, [ %GLOBALS + G_CALLOUT_TMP0 ]
	st	%TMP1, [ %GLOBALS + G_CALLOUT_TMP1 ]
	st	%TMP2, [ %GLOBALS + G_CALLOUT_TMP2 ]

	call	internal_save_scheme_context
	nop

	ld	[ %GLOBALS + G_CALLOUT_TMP0 ], %TMP0
	ld	[ %GLOBALS + G_CALLOUT_TMP1 ], %o0
	ld	[ %GLOBALS + G_CALLOUT_TMP2 ], %o1
	jmpl	%TMP0, %o7
	nop

	call	internal_restore_scheme_context
	nop

	! Check signals.  This costs a little, but code on the critical
	! path shouldn't be using C callouts anyway.  (Hint: fix varargs!)

	call	internal_check_signals
	nop

	ld	[ %GLOBALS + G_RETADDR ], %o7
	jmp	%o7 + 8
	nop


! internal_callout_to_C: call a C procedure and return to millicode.
!
! Call from: millicode
! Input:     TMP0 = address of C procedure
!            TMP1 = first argument
!            TMP2 = second argument
!            globals[ G_RETADDR ] = scheme return address
! Output:    TMP0 = return value from C procedure
! Destroys:  Temporaries

internal_callout_to_C:
	st	%TMP0, [ %GLOBALS + G_CALLOUT_TMP0 ]
	st	%TMP1, [ %GLOBALS + G_CALLOUT_TMP1 ]
	st	%TMP2, [ %GLOBALS + G_CALLOUT_TMP2 ]

	mov	%o7, %TMP0				! save
	call	internal_push				!   millicode return 
	nop						!     address

	call	internal_save_scheme_context
	nop

	ld	[ %GLOBALS + G_CALLOUT_TMP0 ], %TMP0
	ld	[ %GLOBALS + G_CALLOUT_TMP1 ], %o0
	ld	[ %GLOBALS + G_CALLOUT_TMP2 ], %o1
	jmpl	%TMP0, %o7
	nop

	st	%o0, [ %GLOBALS + G_CALLOUT_TMP0 ]
	call	internal_restore_scheme_context
	nop

	call	internal_pop				! restore return
	nop						!   address to TMP0
	jmp	%TMP0 + 8
	ld	[ %GLOBALS + G_CALLOUT_TMP0 ], %TMP0


! internal_restore_scheme_context: restore the VM state from the globals
!   table, switch to Scheme mode.
!
! Call from: millicode only, and only when in C mode
! Input    : Nothing
! Output   : Nothing
! Destroys : Temporaries

internal_restore_scheme_context:
	set	EXTNAME(globals), %GLOBALS
	
	! this is the body of internal_restore_globals, in-line.
	set	dzero, %TMP1
	ldd	[ %TMP1 ], %f0

	ld	[ %GLOBALS+G_ARGREG2 ], %ARGREG2
	ld	[ %GLOBALS+G_ARGREG3 ], %ARGREG3
	ld	[ %GLOBALS+G_RESULT ], %RESULT
	ld	[ %GLOBALS+G_ETOP ], %E_TOP
	ld	[ %GLOBALS+G_TIMER ], %TIMER
	ld	[ %GLOBALS+G_STKP ], %STKP

	! Duplicate the load to REG0 so that we can convert the retaddr...
	ld	[ %GLOBALS+G_REG0 ], %REG0
	mov	%o7, %TMP2
	call	internal_fixnum2retaddr
	ld	[ %GLOBALS + G_RETADDR ], %TMP0
	st	%TMP0, [ %GLOBALS + G_RETADDR ]
	mov	%TMP2, %o7

internal_restore_vm_regs:
	ld	[ %GLOBALS+G_REG0 ], %REG0
	ld	[ %GLOBALS+G_REG1 ], %REG1
	ld	[ %GLOBALS+G_REG2 ], %REG2
	ld	[ %GLOBALS+G_REG3 ], %REG3
	ld	[ %GLOBALS+G_REG4 ], %REG4
	ld	[ %GLOBALS+G_REG5 ], %REG5
	ld	[ %GLOBALS+G_REG6 ], %REG6
	ld	[ %GLOBALS+G_REG7 ], %REG7
	jmp	%o7+8
	nop


! internal_restore_globals: restore global registers which may
! have been destroyed by C code.
!
! Call from: Millicode
! Input    : nothing
! Output   : nothing
! Destroys : Temporaries
!
! We keep the following globals:
!   0.0d0 in  %f0/%f1

internal_restore_globals:
	set	dzero, %TMP1
	ldd	[ %TMP1 ], %f0
	jmp	%o7+8
	nop

	.seg	"data"
	.align	8
dzero:
	.double 0r0.0
	.seg	"text"


! internal_save_scheme_context: save the virtual machine state,
! go to C mode.
!
! Call from: Millicode.
! Input    : Nothing
! Output   : Nothing
! Destroys : Temporaries

internal_save_scheme_context:
	st	%ARGREG2, [ %GLOBALS+G_ARGREG2 ]
	st	%ARGREG3, [ %GLOBALS+G_ARGREG3 ]
	st	%RESULT, [ %GLOBALS+G_RESULT ]
	st	%E_TOP, [ %GLOBALS+G_ETOP ]
	st	%STKP, [ %GLOBALS+G_STKP ]
	st	%TIMER, [ %GLOBALS+G_TIMER ]

	mov	%o7, %TMP2
	call	internal_retaddr2fixnum
	ld	[ %GLOBALS + G_RETADDR ], %TMP0
	st	%TMP0, [ %GLOBALS + G_RETADDR ]
	mov	%TMP2, %o7

internal_save_vm_regs:
	st	%REG0, [ %GLOBALS+G_REG0 ]
	st	%REG1, [ %GLOBALS+G_REG1 ]
	st	%REG2, [ %GLOBALS+G_REG2 ]
	st	%REG3, [ %GLOBALS+G_REG3 ]
	st	%REG4, [ %GLOBALS+G_REG4 ]
	st	%REG5, [ %GLOBALS+G_REG5 ]
	st	%REG6, [ %GLOBALS+G_REG6 ]
	st	%REG7, [ %GLOBALS+G_REG7 ]
	jmp	%o7+8
	nop


! internal_retaddr2fixnum: convert a return address to a return offset
!
! Call from: millicode
! Input:     REG0 = procedure, TMP0 = return address
! Output:    TMP0 = return offset
! Destroys:  TMP1

internal_retaddr2fixnum:
	tst	%REG0
	be	Lr2f1
	nop
	ld	[ %REG0 - PROC_TAG + 4 ], %TMP1
	inc	4-BVEC_TAG, %TMP1
	sub	%TMP0, %TMP1, %TMP0
Lr2f1:
	jmp	%o7 + 8
	nop

! Call from: millicode
! Input:     REG0 = procedure, TMP0 = fixnum (offset)
! Output:    TMP0 = return address
! Destroys:  TMP1

internal_fixnum2retaddr:
	tst	%REG0
	be	Lf2r1
	nop
	ld	[ %REG0 - PROC_TAG + 4 ], %TMP1
	inc	4-BVEC_TAG, %TMP1
	add	%TMP0, %TMP1, %TMP0
Lf2r1:
	jmp	%o7 + 8
	nop

! Call from: millicode
! Input:     nothing; Scheme return in G_RETADDR
! Output:    nothing
! Destroys:  temporaries

internal_check_signals:
	ld	[ %GLOBALS + G_SIGNAL ], %TMP0
	cmp	%TMP0, 0
	bne	1f
	nop

	ld	[ %GLOBALS + G_FPE_CODE ], %TMP0
	tst	%TMP0
	bz	3f
	nop
	mov	%TMP0, %ARGREG3
	mov	EX_FPE, %TMP0
	b	EXTNAME(m_exception)
	clr	[ %GLOBALS + G_FPE_CODE ]

3:	jmp	%o7 + 8
	nop

1:	ld	[ %GLOBALS + G_SIGINT ], %TMP0
	cmp	%TMP0, 0
	be	2f
	nop
	! SIGINT
	mov	EX_KBDINTR, %TMP0
	b	raise_signal_exception
	mov	G_SIGINT, %TMP1

	! No signals were discovered -- clear master flag and return.
	! FIXME: this is really a race condition; we should block signals
	! while clearing the flag.
2:	jmp	%o7 + 8
	st	%g0, [ %GLOBALS + G_SIGNAL ]

	! Raise an exception.  TMP0 has the correct exception code,
	! TMP1 has the globals index for the signal in question.
	! FIXME: this is really a race condition; we should block signals
	! while clearing the flag.

raise_signal_exception:
	st	%g0, [ %GLOBALS + %TMP1 ]		! clear signal's flag
	ld	[ %GLOBALS + G_RETADDR ], %o7		! to return
	set	TRUE_CONST, %TMP1			! setup to
	b	EXTNAME(m_exception)
	st	%TMP1, [ %GLOBALS + G_SCHCALL_SAVERES ]	!   save RESULT



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Simple stack for saving and restoring values (mostly return addresses).
! Do *not* use this to store roots if a GC may happen!
!

! internal_push: push a value onto the internal stack, preserving
! 	all registers.
!
! Call from: anywhere (Scheme mode)
! Input:     TMP0 = value to push
! Output:    Nothing
! Destroys:  Nothing

internal_push:
	st	%TMP1, [ %GLOBALS+G_PUSHTMP ]	! we need TMP1
	set	stackp, %TMP1			! load
	ld	[ %TMP1 ], %TMP1		!   stack ptr
	inc	4, %TMP1			! bump it
	st	%TMP0, [ %TMP1 ]		! store item
	set	stackp, %TMP0			! store
	st	%TMP1, [ %TMP0 ]		!   new stack ptr
	ld	[ %TMP1 ], %TMP0		! restore value in TMP0
	ld	[ %GLOBALS + G_PUSHTMP ], %TMP1	! restore TMP1
	jmp	%o7+8
	nop

! internal_pop: pop a value off the internal stack, preserving
! 	all registers.
!
! Call from: anywhere (Scheme mode)
! Input:     Nothing
! Output:    TMP0 = value popped
! Destroys:  TMP0

internal_pop:
	st	%TMP1, [ %GLOBALS + G_PUSHTMP ]	! need %TMP1
	set	stackp, %TMP1			! load
	ld	[ %TMP1 ], %TMP0		!   stack ptr
	dec	4, %TMP0			! bump it
	st	%TMP0, [ %TMP1 ]		! save new stack ptr
	ld	[ %TMP0+4 ], %TMP0		! load element
	ld	[ %GLOBALS + G_PUSHTMP ], %TMP1	! restore %TMP1
	jmp	%o7+8
	nop

	.seg	"data"
	.align	4
stack:	.word	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
stackp:	.word	stack-4
	.seg	"text"


! end of file

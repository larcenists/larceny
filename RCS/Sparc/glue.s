! -*- Fundamental -*-
!
! Larceny Run-time System (SPARC)
!
! $Id: glue.s,v 1.11 1992/08/04 18:27:53 lth Exp lth $
!
! This file contains miscellaneous glue procedures and many millicode
! procedures. The most important glue procedures are _scheme_start which
! invokes the Scheme system (callable from C) and _scheme_call, which calls
! a Scheme procedure from millicode with complete saving of state.

#include "registers.s.h"
#include "millicode.s.h"
#include "offsets.s.h"
#include "layouts.s.h"
#include "exceptions.s.h"
#include "milliprocs.s.h"


	.seg	"text"

	! Runtime-system internal procedures

	.global	_scheme_start
	.global	_scheme_call

	! Millicode procs

	.global	_m_open_file
	.global	_m_close_file
	.global	_m_unlink_file
	.global	_m_read_file
	.global	_m_write_file
	.global _m_resource_usage
	.global _m_apply
	.global _m_varargs
	.global	_m_typetag
	.global	_m_typetag_set
	.global	_m_eqv
	.global	_m_debug
	.global	_m_reset
	.global	_m_exit
	.global	_m_break
	.global _m_dumpheap
	.global _m_singlestep
	.global _m_generic_exception
	.global _m_partial_list2vector

! These need to appear eventually, I think.
!
!	.global _m_vector_ref
!	.global _m_vector_set
!	.global _m_vector_like_ref
!	.global _m_vector_like_set
!	.global _m_procedure_ref
!	.global _m_procedure_set
!	.global _m_bytevector_ref
!	.global _m_bytevector_set
!	.global _m_bytevector_like_ref
!	.global _m_bytevector_like_set

	! Mostly obsolete exception handlers (but not obsolete until
	! the primops catch up with the new exception handling policies).

	.global	_not_supported
	.global	_type_exception
	.global	_timer_exception
	.global	_proc_exception
	.global _arg_exception
	.global	_arith_exception
	.global	_undef_exception

! The procedure _scheme_start is called from the C-language initialization
! code. _scheme_start sets up the virtual machine and then calls the
! application specific startup procedure in the globals slot 'SCHEME_ENTRY'.
! If that procedure returns, then _scheme_start returns to its caller.
! 
! Note that if the startup procedure returns, Scheme I/O buffers will not
! be flushed, nor will any other exit procedures be called. The only way
! for all this to happen is for the application to do it, and it is normally
! done by calling the scheme procedure "exit".
!
! If the C startup wishes to pass arguments to SCHEME_ENTRY, it should 
! intialize the appropriate register save areas in globals[], as we avoid
! touching the registers here. (If there are no arguments, the startup
! must set %RESULT to 0, at least).
!
! [This procedure may be a specialization of the C-to-scheme calling stuff;
! if so, _scheme_start should later be merged with the general case.]

_scheme_start:
	save	%sp, -96, %sp			! Standard stack frame
	st	%i7, [ %fp + 0x44 ]

	call	_mem_restore_scheme_context
	nop

! Call application code: setup a minimal continuation and jump. The
! continuation contains a dummy procedure field. No stack overflow check
! is performed, so the stack cache must have a minimum of 16 bytes free.
! (It would be ridiculous if it did not.)

	set	L1-8, %TMP0		! this is valid; this code won't move
	sub	%STKP, 16, %STKP	! allocate frame
	st	%TMP0, [ %STKP ]	! return address
	mov	12, %TMP1
	st	%TMP1, [ %STKP+4 ]	! size
	st	%g0, [ %STKP+8 ]	! procedure (dummy!)
L2:
	ld	[ %GLOBALS + SCHEME_ENTRY_OFFSET ], %REG0
	and	%REG0, TAGMASK, %TMP0
	cmp	%TMP0, PROC_TAG
	beq	L0
	nop

	! Startup slot does not have a procedure; just print an error and
	! exit. This should never happen unless the user has manually hacked
	! the heap image (as we can check that we get a procedure at dump 
	! time), and frankly, I couldn't care if ze shoots zemself in the 
	! foot.

	set	Lnonproc, %o0
	call	_printf
	nop
	call	_exit
	mov	1, %o0

	! Everything is fine.
L0:
	ld	[ %REG0 - PROC_TAG + CODEVECTOR ], %TMP0
	jmp	%TMP0 - BVEC_TAG + CODEOFFSET
	nop

	! Return to C code
L1:
	call	_mem_save_scheme_context
	nop

	ld	[ %fp + 0x44 ], %i7
	ret
	restore

	.seg	"data"
Lnonproc:
	.asciz	"Startup slot does not have a procedure!\n"
	.seg	"text"

! `_scheme_call'
!
! Call Scheme when the VM is in Scheme mode already. The problem here is
! that when Scheme code calls a millicode procedure, it is not required to
! save any of its registers. Thus, when the millicode must call on Scheme
! again to do some work for it, the caller's context must be saved before
! the new Scheme procedure is invoked. This context must also be restored
! before control is returned to the original caller, and, again, the original
! caller will not do this. So we must arrange for it to happen.
!
! This is what happens: We create two stack frames, like shown below.
! The top frame has no saved registers except R0, which is a pointer to the
! Scheme procedure "scheme2scheme-helper" (defined in "Lib/Sparc/glue.mal").
! The return address in this frame points to the very start of that procedure.
! The second frame has a full set of saved registers, and its R0 is a pointer
! to the original caller. The return address is the return address in the
! original caller -- the point to which millicode should have returned.
! When the Scheme procedure which was called from millicode returns, it
! will return into scheme2scheme-helper, which in turn pops *both* frames and
! restores the original context before returning to the original Scheme
! procedure.
!
!      +---------------+  <-- top of stack
!      | (return addr) |
!      | (frame size)  |
!      | (proc ptr)    |
!      | (unused)      |
!      +---------------+
!      | (return addr) |
!      | (frame size)  |
!      | (proc ptr)    |
!      | (Reg1)        |
!          :
!      | (Reg31)       |
!      +---------------+
!      |  (stuff)      |
!
! This millicode procedure takes the following arguments:
!  * %RESULT, %ARGREG1, %ARGREG2, and %TMP0 are used for arguments to be
!    passed on to the Scheme procedure.
!  * %o7 is the return address to the original caller (who is in %REG0).
!  * %TMP1 is the argument count (0, 1, 2, 3, or 4).
!  * %TMP2 is the vector index into the global vector of Scheme procedures
!    callable from millicode. A pointer to a value cell which holds the
!    vector pointer is in the root MILLICODE_SUPPORT.
!
! This procedure then sets up the two stack frames and invokes the requested
! Scheme procedure, which returns through the helper procedure in the top
! stack frame. This procedure never returns to its caller.
!
! The "BUG!" entries below are not currently serious; however, the values 
! stored are not tagged, and we store them in rootables. The reason the bugs
! are not serious is that the values interpreted as pointers are outside
! any space and hence not touched by the collector (better be right).

_scheme_call:

	! Save parameters which were passed in registers we need to use.
	!
	! We can't just store the return address, as a stack frame overflow
	! may trigger a collection, which will change the procedure's
	! location. So we store the offset into the procedure as a fixnum.

	st	%TMP0, [ %GLOBALS + GLUE_TMP1_OFFSET ]		! 4th arg
	st	%TMP1, [ %GLOBALS + GLUE_TMP2_OFFSET ]		! argc. BUG!
	st	%TMP2, [ %GLOBALS + GLUE_TMP3_OFFSET ]		! proc. BUG!
	ld	[ %REG0 - PROC_TAG + CODEVECTOR ], %TMP0
	sub	%TMP0, BVEC_TAG, %TMP0
	sub	%o7, %TMP0, %TMP0
	st	%TMP0, [ %GLOBALS + GLUE_TMP4_OFFSET ]		! retaddr

	! Check stack overflow

	ld	[ %GLOBALS + STK_LIMIT_OFFSET ], %TMP0
	cmp	%STKP, %TMP0
	bge	Lscheme_call_1
	nop
	call	_mem_internal_stkoflow
	nop

	! Allocate the stack frames, then save values.

Lscheme_call_1:
	sub	%STKP, (32*4+8)+(2*4+8), %STKP		! both frames at once.

	! Do the top (small) frame

	ld	[ %GLOBALS + MILLICODE_SUPPORT_OFFSET ], %TMP0
	ld	[ %TMP0 - GLOBAL_CELL_TAG + CELL_VALUE_OFFSET ], %TMP0
	cmp	%TMP0, UNSPECIFIED_CONST
	bne	Lscheme_call_2
	nop

	! Here, there was no vector in the magic global, probably because
	! the system is still being initialized. This is a fatal error,
	! so we simply print an error message and abort. While this should
	! never happen in a working system, it is nice to have as a 
	! diagnostic when bringing up a new heap.

	set	Lnovector, %o0
	call	_printf
	nop
	call	_exit
	mov	1, %o0

	.seg	"data"
Lnovector:
	.asciz	"No vector in the MILLCODE_SUPPORT global.\n"
	.seg	"data"

Lscheme_call_2:
	add	%TMP0, 4 - VEC_TAG, %TMP0
	ld	[ %GLOBALS + GLUE_TMP3_OFFSET ], %TMP2		! proc idx
	ld	[ %TMP0 + %TMP2 ], %TMP2			! proc to call
	st	%TMP2, [ %GLOBALS + GLUE_TMP3_OFFSET ]
	ld	[ %TMP0 + MS_SCHEME2SCHEME ], %TMP0
	ld	[ %TMP0 - PROC_TAG +  CODEVECTOR ], %TMP1
	add	%TMP1, 4 - BVEC_TAG - 8, %TMP1			! adj retaddr
	st	%TMP1, [ %STKP ]				! retaddr
	mov	(2*4+4), %TMP1
	st	%TMP1, [ %STKP+4 ]				! size
	st	%TMP0, [ %STKP+8 ]				! proc

	! Do the bottom (large) frame
	
! Offset within bottom stack frame where REG0 is stored.
#define BASE	16

	! This could be scheduled; unclear that it would make a difference.

	ld	[ %GLOBALS + GLUE_TMP4_OFFSET ], %TMP0
	ld	[ %REG0 - PROC_TAG + CODEVECTOR ], %TMP1
	sub	%TMP1, BVEC_TAG, %TMP1
	add	%TMP1, %TMP0, %TMP1
	st	%TMP1, [ %STKP + BASE ]		! return address
	mov	32*4+8, %TMP0
	st	%TMP0, [ %STKP + 4 + BASE ]	! size
	std	%REG0, [ %STKP + 8*0 + 8 + BASE ]
	std	%REG2, [ %STKP + 8*1 + 8 + BASE ]
	std	%REG4, [ %STKP + 8*2 + 8 + BASE ]
	std	%REG6, [ %STKP + 8*3 + 8 + BASE]
	ld	[ %GLOBALS + REG8_OFFSET ], %REG0
	ld	[ %GLOBALS + REG9_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*4 + 8 + BASE ]
	ld	[ %GLOBALS + REG10_OFFSET ], %REG0
	ld	[ %GLOBALS + REG11_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*5 + 8 + BASE ]
	ld	[ %GLOBALS + REG12_OFFSET ], %REG0
	ld	[ %GLOBALS + REG13_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*6 + 8 + BASE ]
	ld	[ %GLOBALS + REG14_OFFSET ], %REG0
	ld	[ %GLOBALS + REG15_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*7 + 8 + BASE ]
	ld	[ %GLOBALS + REG16_OFFSET ], %REG0
	ld	[ %GLOBALS + REG17_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*8 + 8 + BASE ]
	ld	[ %GLOBALS + REG18_OFFSET ], %REG0
	ld	[ %GLOBALS + REG19_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*9 + 8 + BASE ]
	ld	[ %GLOBALS + REG20_OFFSET ], %REG0
	ld	[ %GLOBALS + REG21_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*10 + 8 + BASE ]
	ld	[ %GLOBALS + REG22_OFFSET ], %REG0
	ld	[ %GLOBALS + REG23_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*11 + 8 + BASE ]
	ld	[ %GLOBALS + REG24_OFFSET ], %REG0
	ld	[ %GLOBALS + REG25_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*12 + 8 + BASE]
	ld	[ %GLOBALS + REG26_OFFSET ], %REG0
	ld	[ %GLOBALS + REG27_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*13 + 8 + BASE ]
	ld	[ %GLOBALS + REG28_OFFSET ], %REG0
	ld	[ %GLOBALS + REG29_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*14 + 8 + BASE ]
	ld	[ %GLOBALS + REG30_OFFSET ], %REG0
	ld	[ %GLOBALS + REG31_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*15 + 8 + BASE ]

	mov	%RESULT, %REG1
	mov	%ARGREG2, %REG2
	mov	%ARGREG3, %REG3
	
	ld	[ %GLOBALS + GLUE_TMP2_OFFSET ], %TMP1		! argc
	cmp	%TMP1, 4
	be,a	.+8
	ld	[ %GLOBALS + GLUE_TMP1_OFFSET ], %REG4		! get 4th arg
	ld	[ %GLOBALS + GLUE_TMP3_OFFSET ], %REG0		! proc
	ld	[ %REG0 - PROC_TAG + CODEVECTOR ], %TMP0
	jmp	%TMP0 - BVEC_TAG + CODEOFFSET
	sll	%TMP1, 2, %RESULT

#undef BASE


! I/O primitives -- these tap right into the OS.
!
! These all assume that the right types are passed; no error checking is
! performed. How wise this is, remains to be seen.

! Open and unlink require a null-terminated string for the filename
! argument. Since the argument in is not on that form (rather, it has a length
! field), we have to convert it.
!
! When we get generalized Scheme-to-C calling, these should go away and be
! replaced by something using that mechaninsm [I think].

! OPEN: filename (string) in RESULT.
!       flags (fixnum) in ARGREG2.
!       mode (fixnum) in ARGREG3.

_m_open_file:
	save	%sp, -96, %sp

	call	Lcopystring
	nop

	set	Lfnbuf, %o0
	srl	%SAVED_ARGREG2, 2, %o1
	call	_open
	srl	%SAVED_ARGREG3, 2, %o2
	sll	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

! UNLINK: filename (string) in RESULT.

_m_unlink_file:
	save	%sp, -96, %sp

	call	Lcopystring
	nop

	set	Lfnbuf, %o0
	call	_unlink
	nop
	sll	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

! Copy the string into the local buffer, truncating if necessary, and null-
! terminating. Assumes the string pointer is in %SAVED_RESULT.
! This is not particularly efficient.

Lcopystring:
	ld	[ %SAVED_RESULT - BVEC_TAG ], %l0	! get hdr
	srl	%l0, 8, %l0				! get length
	andcc	%l0, 255, %l0				! truncate
	set	Lfnbuf, %l2				! dest ptr
	stb	%g0, [ %l2 + %l0 ]			! terminator
	b	Lcopy1
	add	%SAVED_RESULT, 4 - BVEC_TAG, %l1	! src ptr
Lcopy0:
	ldub	[ %l1+%l0 ], %l3			! get
	stb	%l3, [ %l2+%l0 ]			! put
Lcopy1:
	subcc	%l0, 1, %l0				! dec
	bge	Lcopy0					! again?
	nop
	
	jmp	%o7+8
	nop

! CLOSE: file descriptor (fixnum) in RESULT.

_m_close_file:
	save	%sp, -96, %sp

	call	_close
	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! READ: file descriptor (fixnum) in RESULT.
!       buffer (string) in ARGREG2.
!       byte count (fixnum) in ARGREG3.

_m_read_file:
	save	%sp, -96, %sp

	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	add	%SAVED_ARGREG2, 4 - BVEC_TAG, %o1	! buffer pointer
	call	_read
	srl	%SAVED_ARGREG3, 2, %o2			! byte count
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! WRITE: file descriptor (fixnum) in RESULT.
!        buffer (string) in ARGREG2.
!        byte count (fixnum) in ARGREG3.

_m_write_file:
	save	%sp, -96, %sp

	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	add	%SAVED_ARGREG2, 4 - BVEC_TAG, %o1	! buffer pointer
	call	_write
	srl	%SAVED_ARGREG3, 2, %o2			! byte count
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! RESOURCE_USAGE takes a vector argument and fills in the vector with
!                resource data. It's all done in C.

_m_resource_usage:
	save	%sp, -96, %sp

	call	_C_resource_usage
	mov	%SAVED_RESULT, %o0

	jmp	%i7+8
	restore

! Millicode for the `apply' instruction.
!
! 1. Check that %RESULT has a procedure, and fault if it is not.
! 2. Decrement timer, fault if it reaches 0.
! 3. Map the (head of the) list in REG1 onto registers REG1-REG30, put the
!    tail, if any, into REG31. Fault if (the initial) REG1 is not a proper 
!    list.
! 4. Move RESULT to REG0, set RESULT to the length of the list, and invoke
!    the procedure in REG0.
!
! The Scheme and MAL code which precedes the millicode in a call to apply
! check for a proper procedure and a non-list argument.
!
! We still check for those here, but these tests could/should go away.
! Old code.

_m_apply:
	! Is the value of %RESULT a procedure? If not, we must signal an
	! exception.

	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, PROC_TAG
	be	Lapply1
	nop
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_APPLY, %TMP0

Lapply1:
	! Decrement timer, check for expiration. Returns back into the Scheme
	! code, for consistency. This is not really an exception, and we
	! adjust the return address to pretend nothing happened.

	subcc	%TIMER, 1, %TIMER
	bne	Lapply2
	nop
	jmp	%MILLICODE + M_TIMER_EXCEPTION		! *NOT* jmpl
	sub	%o7, 8, %o7

Lapply2:
	! Is REG1 a proper list, and what is its length?
	! This loop maps the list onto the software registers (as many
	! of them as there are), then goes on to calculate the length of
	! the list and returns that in TMP0. Finally it copies hardware-
	! mapped registers in from the globals file.

	mov	%REG1, %TMP0				! list ptr
	mov	%GLOBALS, %TMP1				! pointer to globals

	save	%sp, -96, %sp

	add	%SAVED_TMP1, REG1_OFFSET, %l0		! destination ptr
	mov	%SAVED_TMP0, %l1			! source ptr
	mov	30, %l2					! counter
	mov	0, %l3					! list length

Lapply3:
	cmp	%l1, NIL_CONST				! done yet?
	be	Lapply5
	nop
	and	%l1, TAGMASK, %l5			! pair?
	cmp	%l5, PAIR_TAG
	bne	Lapply9					! not a proper list
	nop
	ld	[ %l1 - PAIR_TAG ], %l4			! car
	add	%l3, 1, %l3				! one more
	st	%l4, [ %l0 ]				! stuff in table
	add	%l0, 4, %l0
	subcc	%l2, 1, %l2				! one less
	bg	Lapply3
	ld	[ %l1 + 4 - PAIR_TAG ], %l1		! cdr

	! Counter expired. We must save the list tail in REG31 and then
	! cdr down the tail to find the length and make sure the list is
	! proper.

	st	%l1, [ %SAVED_TMP1 + REG31_OFFSET ]	! store tail
Lapply4:
	cmp	%l1, NIL_CONST
	be	Lapply5
	nop
	and	%l1, TAGMASK, %l5
	cmp	%l5, PAIR_TAG
	bne	Lapply9
	nop
	add	%l3, 1, %l3
	b	Lapply4
	ld	[ %l1 + 4 - PAIR_TAG ], %l1

Lapply5:
	! The length is now in %l3, and all software registers are set up 
	! appropriately. Must load the hardware-mapped ones in, and then 
	! setup the length.

	sll	%l3, 2, %SAVED_TMP1			! fixnum it.
	restore

	! One day we'll doubleword-align the software register file...

	ld	[ %GLOBALS + REG1_OFFSET ], %REG1
	ld	[ %GLOBALS + REG2_OFFSET ], %REG2
	ld	[ %GLOBALS + REG3_OFFSET ], %REG3
	ld	[ %GLOBALS + REG4_OFFSET ], %REG4
	ld	[ %GLOBALS + REG5_OFFSET ], %REG5
	ld	[ %GLOBALS + REG6_OFFSET ], %REG6
	ld	[ %GLOBALS + REG7_OFFSET ], %REG7

	mov	%RESULT, %REG0

	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	jmp	%TMP0 + A_CODEOFFSET
	mov	%TMP1, %RESULT

Lapply9:
	! The list was not proper. We have an error that must be handled.
	! This code depends on %RESULT and %REG1 still having their
	! original values.

	restore
	mov	%REG1, %ARGREG2
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_APPLY, %TMP0


! Millicode for the 'args>=' instruction.
! Most of the operation has been punted to C code; see comments in that code
! for illumination.
!
! The 0-extra-args case ought to be handled here for efficiency.

_m_varargs:
	cmp	%RESULT, %ARGREG2
	bge	Lvararg2
	nop
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_VARGC, %TMP0

Lvararg2:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_mem_save_scheme_context
	nop

	save	%sp, -96, %sp
	call	_C_varargs			! Can't fail.
	nop
	restore

	call	_mem_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7

	jmp	%o7+8
	nop

! Extract typetag from vector or bytevector header, given a pointer to either.

_m_typetag:
	and	%RESULT, 7, %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Ltypetag1
	ld	[ %RESULT - VEC_TAG ], %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltypetag1
	ld	[ %RESULT - BVEC_TAG ], %TMP0
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_TYPETAG, %TMP0
Ltypetag1:
	jmp	%o7+8
	and	%TMP0, 0x1C, %RESULT

! Set the typetag of a vector or bytevector header. The pointer to the 
! structure is passed in %RESULT. The new tag is in %ARGREG2. That tag must
! be a fixnum in the range 0-8 (appropriately shifted).

_m_typetag_set:
	and	%RESULT, 7, %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Ltypetagset1
	xor	%RESULT, VEC_TAG, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltypetagset1
	xor	%RESULT, BVEC_TAG, %TMP0
Ltypetagset0:
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_TYPETAGSET, %TMP0
Ltypetagset1:
	ld	[ %TMP0 ], %TMP1
	andncc	%ARGREG2, 0x1C, %g0
	bne	Ltypetagset0
	nop
	and	%TMP1, 0x1C, %TMP2
	xor	%TMP1, %TMP2, %TMP1
	or	%TMP1, %ARGREG2, %TMP1
	jmp	%o7 + 8
	st	%TMP1, [ %TMP0 ]

! This procedure is entered only if the two arguments are not eq?.
! Note that fixnums and immediates are always eq? if they are eqv?, so we need
! only concern ourselves with larger structures here.

_m_eqv:
	! Do fixnums first to get them out of the way completely.
	! If operands are fixnums, then they are not eqv?.

	tsubcc	%RESULT, %ARGREG2, %g0
	bvs,a	Leqv_others
	xor	%RESULT, %ARGREG2, %TMP0
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_others:
	andcc	%TMP0, TAGMASK, %g0
	bne,a	Leqv_done
	mov	FALSE_CONST, %RESULT

	! Tags are equal, but addresses are not (they are not eq?). This
	! lets us get rid of all non-numeric types.

	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, PAIR_TAG
	be,a	Leqv_done
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, PROC_TAG
	be,a	Leqv_done
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, BVEC_TAG
	be	Leqv_bvec
	nop
	cmp	%TMP0, VEC_TAG
	be	Leqv_vec
	nop
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_bvec:
	! Bytevector-like

	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1

	cmp	%TMP0, BIGNUM_HDR
	be,a	Leqv_bvec2
	mov	0, %TMP0
	cmp	%TMP0, FLONUM_HDR
	be,a	Leqv_bvec2
	mov	1, %TMP0
	cmp	%TMP0, COMPNUM_HDR
	be,a	Leqv_bvec2
	mov	1,%TMP0
	b	Leqv_done
	mov	FALSE_CONST, %RESULT
Leqv_bvec2:
	cmp	%TMP1, BIGNUM_HDR
	be,a	Leqv_number
	mov	0, %TMP1
	cmp	%TMP1, FLONUM_HDR
	be,a	Leqv_number
	mov	1, %TMP1
	cmp	%TMP1, COMPNUM_HDR
	be,a	Leqv_number
	mov	1, %TMP1
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_vec:
	! We know it has a vector tag here. The header tags must be the same,
	! and both must be either ratnum or rectnum.

	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1

	cmp	%TMP0, %TMP1
	bne,a	Leqv_done
	mov	FALSE_CONST, %RESULT

	mov	0, %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Leqv_number
	mov	0, %TMP0
	cmp	%TMP0, RECTNUM_HDR
	be,a	Leqv_number
	mov	0, %TMP0
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_number:
	! Numbers. They are eqv if they are of the same exactness and they
	! test #t with `='. The exactness is encoded in TMP0 and TMP1: 0s
	! mean exact, 1s mean inexact.

	cmp	%TMP0, %TMP1
	bne,a	Leqv_done
	mov	FALSE_CONST, %RESULT

	! Same exactness. Test for equality.

	jmp	%MILLICODE + M_NUMEQ
	nop

Leqv_done:
	jmp	%o7+8
	nop

! Debugger entry point. Probably does not make sense in Larceny.

_m_debug:
	b	_not_supported
	nop

! Reset the system. Should arguably be written entirely in Scheme.

_m_reset:
	b	_not_supported
	nop

! Exit -- simply terminate the program by calling exit(), which should 
! clean up most things. This is certainly good enough for the time being.
! We may assume that there is a Scheme wrapper around this millicode which
! takes care of flushing buffers, etc.

_m_exit:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_mem_save_scheme_context
	call	_exit
	mov	0, %o0

! Dump the heap to a file. Takes two arguments, the file name and the
! startup procedure. 

_m_dumpheap:
	and	%ARGREG2, TAGMASK, %TMP0
	cmp	%TMP0, PROC_TAG
	bne	Ldump_err
	nop
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	bne	Ldump_err
	nop
	ld	[ %RESULT - BVEC_TAG ], %TMP0
	and	%TMP0, 0xFF, %TMP1
	cmp	%TMP1, (BV_HDR | STR_SUBTAG)
	bne	Ldump_err
	nop
	sra	%TMP0, 8, %TMP0
	cmp	%TMP0, 255
	bge	Ldump_err
	nop

	! Data is validated as much as possible; what is left is that the
	! init proc may not take the right number of arguments, but we
	! cannot check that.

	! Now install the startup procedure. No transaction need be recorded
	! because we are about to do a major collection.

	st	%ARGREG2, [ %GLOBALS + SCHEME_ENTRY_OFFSET ]

	! Collect.

	st	%RESULT, [ %GLOBALS + SAVED_RESULT_OFFSET ]
	mov	%o7, %TMP0
	call	_mem_internal_collect
	mov	-2 << 2, %RESULT
	mov	%TMP0, %o7
	ld	[ %GLOBALS + SAVED_RESULT_OFFSET ], %RESULT

	! Dump.

	save	%sp, -96, %sp
	
	call	Lcopystring		! copies the file name into Lfnbuf
	nop

	set	Lfnbuf, %o0
	call	_C_dump_heap
	nop
	cmp	%o0, -1
	be	Ldump_err2
	restore

	jmp	%o7+8
	nop

Ldump_err:
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_DUMP, %TMP0
Ldump_err2:
	jmp	%MILLICODE + M_GENERIC_EXCEPTION
	mov	EX_DUMPFAIL, %TMP0


! list->vector is a partial primop because the Scheme implementation 
! (make the vector, bang the elements) causes a lot of unneccesary side
! effect checking in the generation-scavenging collector. It is not a full
! primop because of the harrowing details of dealing with non-lists etc.
! 
! The list is passed in %RESULT, and its length is passed in %ARGREG2.
! The vector is returned in %RESULT.
!
! The correctness of this code depends on the vector being allocated in
! the ephemeral space.

_m_partial_list2vector:
	mov	%o7, %TMP0
	mov	%RESULT, %ARGREG3		! save for later
	call	_mem_internal_alloc
	add	%ARGREG2, 4, %RESULT		! length of vector
	mov	%TMP0, %o7
	sll	%ARGREG2, 8, %TMP0
	or	%TMP0, VEC_HDR, %TMP0
	st	%TMP0, [ %RESULT ]		! vector header
	add	%RESULT, 4, %TMP0		! destination pointer
	mov	%ARGREG3, %TMP1			! list pointer
	mov	%ARGREG2, %TMP2			! counter (fixnum)
	b	Ll2v_1
	tst	%TMP2
Ll2v_2:	
	st	%ARGREG3, [ %TMP0 ]		! store in vector
	add	%TMP0, 4, %TMP0			! next element
	ld	[ %TMP1 - PAIR_TAG + 4 ], %TMP1	! get cdr
	subcc	%TMP2, 4, %TMP2			! one less
Ll2v_1:
	bne,a	Ll2v_2
	ld	[ %TMP1 - PAIR_TAG ], %ARGREG3	! get car

	jmp	%o7+8
	or	%RESULT, VEC_TAG, %RESULT


! Breakpoints are expensive, as the machine state must be saved and restored;
! this is so that the break handler can inspect the state (if desired).
!
! This breakpoint handler is only for breakpoints entered with the call to
! the "break" procedure. Breakpoints entered through the trap instruction
! are handled below (?) in _break_trap.

_m_break:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_mem_save_scheme_context
	nop
	call	_C_break
	nop
	call	_mem_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	nop

! Ditto single step handler. Single stepping was added to support CIS 561,
! and single steps on the granularity of MacScheme assembly language 
! instructions, with some compiler support.
!
! This procedure takes one argument in the %TMP0 register, namely a 
! (fixnum) index into the constant vector of the currently executing 
! procedure. This constant slot has to contain a string, and that string
! will usually be the printable representation of the MacScheme instruction
! to be executed next. 
!
! If the singlestep flag in the globals array is on, then the state is
! saved, the string is fetched from the constant vector, and the
! C_singlestep() procedure is invoked. When it returns, the state is 
! restored, and control is returned to the running program. The singlestep
! handler may *not* invoke the Scheme system again, as there is state
! here. We could probably get rid of that problem.

_m_singlestep:
	ld	[ %GLOBALS + SINGLESTEP_OFFSET ], %TMP1
	cmp	%TMP1, TRUE_CONST
	be,a	Lsinglestep1
	nop
	jmp	%o7+8
	nop
Lsinglestep1:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	st	%TMP0, [ %GLOBALS + GLUE_TMP1_OFFSET ]
	call	_mem_save_scheme_context
	nop
	ld	[ %GLOBALS + GLUE_TMP1_OFFSET ], %TMP0
	ld	[ %REG0 - PROC_TAG + CONSTVECTOR ], %TMP1
	sub	%TMP1, VEC_TAG, %TMP1
	call	_C_singlestep
	ld	[ %TMP1 + %TMP0 ], %o0            ! tagged string ptr
	call	_mem_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7 + 8
	nop

! Exception handler entry point.
!
! This exception handler attempts to find the Scheme exception handler in
! the support routines. If it can, it calls that handler with the exception
! code and other arguments. If not, it jumps to Lgeneric_exception, below,
! which enters the default debugging environment (primitive).
!
! Arguments to this procedure:
!  - %TMP0 has the exception code
!  - %o7 has a valid return address to Scheme code. The return address must
!    point to the instruction which would have been returned to if the
!    operation had succeeded (minus the usual 8 bytes), i.e. jumping to %o7+8
!    with the correct(ed) result in %RESULT will continue the computation.
!    The return address may not, under any circumstances, point into 
!    millicode.
!  - %RESULT, %ARGREG2, and %ARGREG3 have the arguments of the operation that
!    faulted.

_m_generic_exception:
	ld	[ %GLOBALS + MILLICODE_SUPPORT_OFFSET ], %TMP1
	ld	[ %TMP1 - GLOBAL_CELL_TAG + CELL_VALUE_OFFSET ], %TMP1
	cmp	%TMP1, UNSPECIFIED_CONST
	be	Lgeneric_exception
	mov	%TMP0, %TMP1
	mov	4, %TMP1
	b	_scheme_call
	mov	MS_EXCEPTION_HANDLER, %TMP2

! Print an error message detailing the program counter, then enter the
! debugger in the run-time system. This routine will go away when we get
! debugging support on the Scheme level.

_not_supported:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_mem_save_scheme_context
	nop
	save	%sp, -96, %sp
	set	Lemsg, %o0
	call	_printf
	mov	%o7, %o1
	call	_C_localdebugger
	nop
	restore
	call	_mem_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET  ], %o7
	jmp	%o7 + 8
	nop

! Most of the exception handlers are now obsolete.
!
! Exception handlers. A coherent protocol for passing of information needs
! to be designed here.
!
! A "timer exception" is raised when the software timer reaches 0.
!
! A "type exception" is raised when a procedure is applied to an argument
! of a type it cannot handle, e.g. "car" on a non-pair.
!
! A "procedure exception" is raised when the user program attempts to
! apply a non-procedure.
!
! An "arg exception" is raised when the user program applies a procedure
! to the wrong number of arguments.
!
! Currently, the timer exception resets the timer and returns, while the
! others call _C_exception (which aborts the program, presumably). 

! Just reset the timer and return, for now. Later, we'll want to implement
! "process" switching, and so we'll have to jump into Scheme if necessary.

_timer_exception:
	b	Lgeneric_exception
	mov	TIMER_EXCEPTION, %TMP1

!	jmp	%o7+8
!	ld	[ %GLOBALS + INITIAL_TIMER_OFFSET ], %TIMER

_type_exception:
	b	Lgeneric_exception
	mov	TYPE_EXCEPTION, %TMP1

_proc_exception:
	b	Lgeneric_exception
	mov	PROC_EXCEPTION, %TMP1

_arg_exception:
	b	Lgeneric_exception
	mov	ARG_EXCEPTION, %TMP1

_arith_exception:
	b	Lgeneric_exception
	mov	ARITH_EXCEPTION, %TMP1

! This is obsolete, too.

_undef_exception:
	ld	[ %GLOBALS + MILLICODE_SUPPORT_OFFSET ], %TMP0
	ld	[ %TMP0 - GLOBAL_CELL_TAG + CELL_VALUE_OFFSET ], %TMP0
	cmp	%TMP0, UNSPECIFIED_CONST
	be,a	Lgeneric_exception
	mov	UNDEF_EXCEPTION, %TMP1

	mov	1, %TMP1
	mov	MS_UNDEF_GLOBAL, %TMP2
	b	_scheme_call
	mov	%ARGREG2, %RESULT

! Very basic exception handler; calls the C exception handler.
! If the hander returns, we return to the caller and hope for the best.
! This handler is usually only used during bootstrapping failures,
! as the Scheme exception handler is otherwise used.
!
! THIS PROCEDURE IS NOT REENTRANT! The C exception handler may not
! re-invoke Scheme code and later return to this handler.

Lgeneric_exception:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
!	st	%TMP0, [ %GLOBALS + GLUE_TMP1_OFFSET ]
	call	_mem_save_scheme_context
	nop
!	ld	[ %GLOBALS + GLUE_TMP1_OFFSET ], %TMP0
	mov	%TMP1, %g1

	save	%sp, -96, %sp
	call	_C_exception
	mov	%SAVED_TMP0, %o0
	restore

	call	_mem_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	nop

! Static data for this module.

	.seg	"data"

Lfnbuf:	.skip	256
Lfstr:	.asciz  "Retaddr=%x\n"
Lemsg:	.asciz	"Unsupported millicode procedure at PC=%lX\n"
Lsinglestep_msg:
	.asciz	"Singlestep break at %s\n"

	! end of file

! -*- Fundamental -*-
!
! Scheme 313 Run-time system
! Miscellaneous assembly language "glue" and millicode.
!
! $Id: glue.s,v 1.5 92/01/30 18:02:35 lth Exp Locker: lth $

#include "registers.s.h"
#include "millicode.s.h"
#include "offsets.s.h"
#include "layouts.s.h"
#include "exceptions.s.h"


	.seg	"text"

	.global	_schemestart
	.global	_scheme_call
	.global	_open_file
	.global	_close_file
	.global	_unlink_file
	.global	_read_file
	.global	_write_file
	.global _m_getrusage
	.global _apply
	.global _scheme_varargs
	.global	_typetag
	.global	_typetag_set
	.global	_eqv
	.global	_m_debug
	.global	_m_reset
	.global	_m_exit
	.global	_m_break
	.global	_not_supported
	.global	_type_exception
	.global	_timer_exception
	.global	_proc_exception
	.global _arg_exception
	.global	_arith_exception
	.global	_undef_exception

! The procedure _schemestart is called from the C-language initialization
! code. _schemestart sets up the virtual machine and then calls the
! application specific startup procedure in the globals slot 'SCHEME_ENTRY'.
! If that procedure returns, then _schemestart returns to its caller.
! 
! If the C startup wishes to pass arguments to SCHEME_ENTRY, it should 
! intialize the appropriate register save areas in globals[], as we avoid
! touching the registers here. (If there are no arguments, the startup
! must set %RESULT to 0, at least).
!
! [This may be a specialization of the C-to-scheme calling stuff; if so,
!  it should later be merged with the general case.]

_schemestart:
	save	%sp, -96, %sp			! Standard stack frame
	st	%i7, [ %fp + 0x44 ]

	call	_restore_scheme_context
	nop

! Call application code: setup a minimal continuation and jump. The
! continuation contains a dummy procedure field; the stack flusher
! had better deal with this.

	set	L1-8, %TMP0		! this is ok since this code won't move
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
	jmpl	%MILLICODE + M_PROC_EXCEPTION, %o7
	add	%o7, (L2-(.-4))-8, %o7
L0:
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	jmp	%TMP0 + A_CODEOFFSET
	nop

	! Return to C code
L1:
	call	_save_scheme_context
	nop

	ld	[ %fp + 0x44 ], %i7
	ret
	restore

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
! The top frame has no saved registers save R0, which is a pointer to the
! global procedure "scheme2scheme-helper" (defined in "Lib/Sparc/glue.mal").
! The return address in this frame points to the very start of that procedure.
! The second frame has a full set of saved registers, and its R0 is a pointer
! to the original caller. The return address is the return address in the
! original caller -- the point to which millicode should have returned.
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
!  * %RESULT, %ARGREG1, and %ARGREG2 are used for arguments to be passed on
!    to the Scheme procedure
!  * %o7 is the return address to the original caller (who is in %REG0).
!  * %TMP0 is the argument count (0, 1, 2, or 3)
!  * %TMP1 is the vector index into the global vector of Scheme procedures
!    callable from millicode. A pointer to a value cell which holds the
!    vector pointer is in the root MILLICODE_SUPPORT.
!
! This procedure then sets up the two stack frames and invokes the requested
! scheme procedure, which returns through the helper procedure in the top
! stack frame. This procedure never returns to its caller.

! Offset within bottom stack frame where REG0 is stored.

#define REG0P	8

_scheme_call:
	
	save	%sp, -96, %sp
	set	Lemsg2, %o0
	call	_printf
	nop
	call	_exit
	mov	1, %o0
	/*NOTREALLYREACHED*/
	restore

	! Save parameters which were passed in registers we need to use

	st	%o7,   [ %GLOBALS + GLUE_TMP1_OFFSET ]	! WRONG. Must calc offs
	st	%TMP0, [ %GLOBALS + GLUE_TMP2_OFFSET ]
	st	%TMP1, [ %GLOBALS + GLUE_TMP3_OFFSET ]

	! Check stack overflow

	ld	[ %GLOBALS + STK_LIMIT_OFFSET ], %TMP0
	cmp	%STKP, %TMP0
	bge	scheme_call_1
	nop
	jmpl	%MILLICODE + M_STKOFLOW, %o7
	nop

	! Allocate the stack frames, then save values.

scheme_call_1:
	sub	%STKP, 32*4+8, %STKP		! 
	mov	32*4+8, %TMP0
	st	%TMP0, [ %STKP + 4 ]
	ld	[ %GLOBALS + GLUE_TMP1_OFFSET ], %TMP0
	! recalc the real address
	st	%TMP0, [ %STKP + 0 ]

	
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %TMP1
	sub	%TMP1, %TMP0, %TMP0
	sub	%TMP0, BVEC_TAG - 4, %TMP0
	st	%TMP0, [ %STKP + 16 ]

	std	%REG0, [ %STKP + 8*0 + REG0P ]
	std	%REG2, [ %STKP + 8*1 + REG0P ]
	std	%REG4, [ %STKP + 8*2 + REG0P ]
	std	%REG6, [ %STKP + 8*3 + REG0P]
	ld	[ %GLOBALS + REG8_OFFSET ], %REG0
	ld	[ %GLOBALS + REG9_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*4 + REG0P ]
	ld	[ %GLOBALS + REG10_OFFSET ], %REG0
	ld	[ %GLOBALS + REG11_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*5 + REG0P ]
	ld	[ %GLOBALS + REG12_OFFSET ], %REG0
	ld	[ %GLOBALS + REG13_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*6 + REG0P ]
	ld	[ %GLOBALS + REG14_OFFSET ], %REG0
	ld	[ %GLOBALS + REG15_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*7 + REG0P ]
	ld	[ %GLOBALS + REG16_OFFSET ], %REG0
	ld	[ %GLOBALS + REG17_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*8 + REG0P ]
	ld	[ %GLOBALS + REG18_OFFSET ], %REG0
	ld	[ %GLOBALS + REG19_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*9 + REG0P ]
	ld	[ %GLOBALS + REG20_OFFSET ], %REG0
	ld	[ %GLOBALS + REG21_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*10 + REG0P ]
	ld	[ %GLOBALS + REG22_OFFSET ], %REG0
	ld	[ %GLOBALS + REG23_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*11 + REG0P ]
	ld	[ %GLOBALS + REG24_OFFSET ], %REG0
	ld	[ %GLOBALS + REG25_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*12 + REG0P]
	ld	[ %GLOBALS + REG26_OFFSET ], %REG0
	ld	[ %GLOBALS + REG27_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*13 + REG0P ]
	ld	[ %GLOBALS + REG28_OFFSET ], %REG0
	ld	[ %GLOBALS + REG29_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*14 + REG0P ]
	ld	[ %GLOBALS + REG30_OFFSET ], %REG0
	ld	[ %GLOBALS + REG31_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*15 + REG0P ]

	mov	%RESULT, %REG1
	mov	%ARGREG2, %REG2
	
	ld	[ %ARGREG3 + A_CODEVECTOR ], %TMP0
	mov	8, %RESULT
	jmpl	%TMP0 + A_CODEOFFSET, %o7
	st	%o7, [ %STKP ]


! I/O primitives -- these tap right into the OS.
!
! These all assume that the right types are passed; no error checking is
! performed. How wise this is, remains to be seen.

! Open and unlink require a null-terminated string for the filename
! argument. Since the argument in is not on that form (rather, it has a length
! byte), we have to convert it.

! OPEN: filename (string) in RESULT.
!       flags (fixnum) in ARGREG2.
!       mode (fixnum) in ARGREG3.

_open_file:
	save	%sp, -96, %sp

	call	copystring
	nop

	set	fnbuf, %o0
	srl	%SAVED_ARGREG2, 2, %o1
	call	_open
	srl	%SAVED_ARGREG3, 2, %o2
	sll	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

! UNLINK: filename (string) in RESULT.

_unlink_file:
	save	%sp, -96, %sp

	call	copystring
	nop

	set	fnbuf, %o0
	call	_unlink
	nop
	sll	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

! Copy the string into the local buffer, truncating if necessary, and null-
! terminating.
! This is not particularly efficient.

copystring:
	ld	[ %SAVED_RESULT - BVEC_TAG ], %l0	! get hdr
	srl	%l0, 8, %l0				! get length
	andcc	%l0, 255, %l0				! truncate
	set	fnbuf, %l2				! dest ptr
	stb	%g0, [ %l2 + %l0 ]			! terminator
	b	Lopen1
	add	%SAVED_RESULT, 4 - BVEC_TAG, %l1	! src ptr
Lopen0:
	ldub	[ %l1+%l0 ], %l3			! get
	stb	%l3, [ %l2+%l0 ]			! put
Lopen1:
	subcc	%l0, 1, %l0				! dec
	bge	Lopen0					! again?
	nop
	
	jmp	%o7+8
	nop

! CLOSE: file descriptor (fixnum) in RESULT.

_close_file:
	save	%sp, -96, %sp

	call	_close
	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! READ: file descriptor (fixnum) in RESULT.
!       buffer (string) in ARGREG2.
!       byte count (fixnum) in ARGREG3.

_read_file:
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

_write_file:
	save	%sp, -96, %sp

	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	add	%SAVED_ARGREG2, 4 - BVEC_TAG, %o1	! buffer pointer
	call	_write
	srl	%SAVED_ARGREG3, 2, %o2			! byte count
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! GETRUSAGE: there are no arguments. The result is the user time field from
!            the rusage struct converted to milliseconds, as a fixnum.

_m_getrusage:
	save	%sp, -96, %sp

	call	_C_getrusage
	nop
	sll	%o0, 2, %SAVED_RESULT
	set	0x7fffffff, %o0
	and	%o0, %SAVED_RESULT, %SAVED_RESULT

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
! If there are any exceptions in here, the return address is set up to be that
! of the instruction which called this procedure; i.e. the operation is
! retried rather than returning into the millicode. While the latter would
! probably work, it is conceptually simpler to never have any exceptions
! return into the millicode -- the exception handler can be simpler, for
! example.
!
! It is likely that the Scheme/MAL wrapper takes care of some of the
! grudge, like checking for a proper list. If so, we may be able to clean
! things up a bit here. FIXME.

_apply:
	! Is the value of %RESULT a procedure? If not, we must signal an
	! exception. However, where do we return to? The `best' way is to
	! set it up so that we return to the call to this procedure,
	! i.e. we use the return address already in %o7, -8, as the return
	! address.

	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, PROC_TAG
	be	Lapply1
	nop
	jmp	%MILLICODE + M_PROC_EXCEPTION		! *NOT* jmpl
	sub	%o7, 8, %o7

Lapply1:
	! Decrement timer, check for expiration. Returns back into the Scheme
	! code, for consistency.

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

	mov	%REG1, %ARGREG2		! list ptr
	mov	%GLOBALS, %ARGREG3	! pointer to globals
	save	%sp, -96, %sp

	add	%SAVED_ARGREG3, REG1_OFFSET, %l0	! destination ptr
	mov	%SAVED_ARGREG2, %l1			! source ptr
	mov	30, %l2					! counter
	mov	0, %l3					! list length

Lapply3:
	cmp	%l0, NIL_CONST
	be	Lapply5
	nop
	and	%l0, TAGMASK, %TMP0
	cmp	%TMP0, PAIR_TAG
	bne	Lapply9					! not a proper list
	nop
	ld	[ %l0 - PAIR_TAG ], %l4			! get car
	add	%l3, 1, %l3				! one more
	st	%l4, [ %l0 ]				! stuff in table
	subcc	%l2, 1, %l2
	bg	Lapply3
	ld	[ %l0 + 4 - PAIR_TAG ], %l0		! get cdr

	! Counter expired. We must save the list tail in REG31 and then
	! cdr down the tail to find the length and make sure the list is
	! proper.

	st	%l0, [ %SAVED_ARGREG3 + REG31_OFFSET ]	! store tail
Lapply4:
	cmp	%l0, NIL_CONST
	be	Lapply5
	nop
	and	%l0, TAGMASK, %TMP0
	cmp	%TMP0, PAIR_TAG
	bne	Lapply9
	nop
	add	%l3, 1, %l3
	b	Lapply4
	ld	[ %l0 + 4 - PAIR_TAG ], %l0

Lapply5:
	! The length is now in %l3, and all software registers are set up 
	! appropriately. Must load the hardware-mapped ones in, and then 
	! setup the length.

	sll	%l3, 2, %SAVED_ARGREG2			! fixnum it.
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
	mov	%ARGREG2, %RESULT

Lapply9:
	! The list was not proper. We have an error that must be handled.
	! We take a 'type' exception for now and setup the return address
	! to be in the Scheme code.

	restore
	jmp	%MILLICODE + M_TYPE_EXCEPTION		! *NOT* jmpl
	sub	%o7, 8, %o7


! Millicode for the 'args>=' instruction.
! Most of the operation has been punted to C code; see comments in that code
! for illumination.
!
! The 0-extra-args case ought to be handled here for efficiency.

_scheme_varargs:
	cmp	%RESULT, %ARGREG2
	bge	Lvararg2
	nop
	jmp	%MILLICODE + M_ARG_EXCEPTION		! *NOT* jmpl
	sub	%o7, 8, %o7				! retry

Lvararg2:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_save_scheme_context
	nop

	save	%sp, -96, %sp
	call	_C_scheme_varargs			! Can't fail.
	nop
	restore

	call	_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7

	jmp	%o7+8
	nop

! Extract typetag from vector or bytevector header, given a pointer to either.

_typetag:
	and	%RESULT, 7, %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Ltypetag1
	xorcc	%RESULT, VEC_TAG, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltypetag1
	xorcc	%RESULT, BVEC_TAG, %TMP0
	jmp	%MILLICODE + M_TYPE_EXCEPTION
	nop
Ltypetag1:
	ld	[ %TMP0 ], %TMP0
	jmp	%o7+8	
	and	%TMP0, 0x1C, %RESULT

! Set the typetag of a vector or bytevector header. The pointer to the 
! structure is passed in %RESULT. The new tag is in %ARGREG2. That tag must
! be a fixnum in the range 0-8 (appropriately shifted).

_typetag_set:
	and	%RESULT, 7, %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Ltypetagset1
	xorcc	%RESULT, VEC_TAG, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltypetagset1
	xorcc	%RESULT, BVEC_TAG, %TMP0
Ltypetagset0:
	jmp	%MILLICODE + M_TYPE_EXCEPTION
	nop
Ltypetagset1:
	mov	0xFFFFFFE3, %TMP1
	andcc	%ARGREG2, %TMP1, %g0
	bne	Ltypetagset0
	nop
	ld	[ %TMP0 ], %TMP1
	or	%TMP1, %ARGREG2, %TMP1
	jmp	%o7 + 8
	st	%TMP1, [ %TMP0 ]

! This procedure is entered only if the two arguments are not eq?.
! Note that fixnums and immediates are always eq? if they are eqv?, so we need
! only concern ourselves with larger structures here.

_eqv:
	and	%RESULT, TAGMASK, %TMP0
	xor	%ARGREG2, %TMP0, %TMP0
	andcc	%TMP0, TAGMASK, %g0
	bne,a	Leqv1
	mov	FALSE_CONST, %RESULT

	! Tags are equal, but addresses are not (they are not eq?). This
	! gets rid of pairs, strings, procedures, vectors, and symbols, and
	! leaves only numbers (below).

	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, PAIR_TAG
	be,a	Leqv1
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, PROC_TAG
	be,a	Leqv1
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, BVEC_TAG
	bne	Leqv2
	nop

	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1

	cmp	%TMP0, BIGNUM_HDR
	be,a	Leqv3
	mov	0, %TMP0
	cmp	%TMP0, FLONUM_HDR
	be,a	Leqv3
	mov	1, %TMP0
	cmp	%TMP0, COMPNUM_HDR
	be,a	Leqv3
	mov	1,%TMP0
	b	Leqv1
	mov	FALSE_CONST, %RESULT
Leqv3:
	cmp	%TMP1, BIGNUM_HDR
	be,a	Leqv4
	mov	0, %TMP1
	cmp	%TMP1, FLONUM_HDR
	be,a	Leqv4
	mov	1, %TMP1
	cmp	%TMP1, COMPNUM_HDR
	be,a	Leqv4
	mov	1, %TMP1
	b	Leqv1
	mov	FALSE_CONST, %RESULT

Leqv2:
	! We know it has a vector tag here. The header tags must be the same,
	! and both must be either ratnum or rectnum.

	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1

	cmp	%TMP0, %TMP1
	bne,a	Leqv1
	mov	FALSE_CONST, %RESULT

	mov	0, %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Leqv4
	mov	0, %TMP0
	cmp	%TMP0, RECTNUM_HDR
	be,a	Leqv4
	mov	0, %TMP0
	b	Leqv1
	mov	FALSE_CONST, %RESULT

Leqv4:
	! Numbers. They are eqv if they are of the same exactness and they
	! test #t with `='. The exactness is encoded in TMP0 and TMP1: 0s
	! mean exact, 1s mean inexact.

	cmp	%TMP0, %TMP1
	bne,a	Leqv1
	mov	FALSE_CONST, %RESULT

	! Same exactness. Test for equality.

	jmp	%MILLICODE + M_NUMEQ
	nop

Leqv1:
	jmp	%o7+8
	nop

! Debugger entry point.

_m_debug:
	b	_not_supported
	nop

! Exit -- simply terminate the program by calling exit(), which should 
! clean up most things. This is certainly good enough for the time being.
! We may assume that there is a Scheme wrapper around this millicode which
! takes care of flushing buffers, etc.

_m_exit:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_save_scheme_context
	call	_exit
	mov	0, %o0

! Reset the system.

_m_reset:
	b	_not_supported
	nop

! Breakpoints are expensive, as the machine state must be saved and restored;
! this is so that the break handler can inspect the state (if desired).
!
! This breakpoint handler is only for breakpoints entered with the call to
! the "break" procedure. Breakpoints entered through the trap instruction
! are handled below in _break_trap.

_m_break:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_save_scheme_context
	nop
	call	_C_break
	nop
	call	_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	nop

! Print an error message detailing the program counter, then enter the
! debugger in the run-time system. This routine will go away when we get
! debugging support on the Scheme level.

_not_supported:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_save_scheme_context
	nop
	save	%sp, -96, %sp
	set	Lemsg, %o0
	call	_printf
	mov	%o7, %o1
	call	_localdebugger
	nop
	restore
	call	_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET  ], %o7
	jmp	%o7 + 8
	nop

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
	b	generic_exception
	mov	TIMER_EXCEPTION, %TMP1

!	jmp	%o7+8
!	ld	[ %GLOBALS + INITIAL_TIMER_OFFSET ], %TIMER

_type_exception:
	b	generic_exception
	mov	TYPE_EXCEPTION, %TMP1

_proc_exception:
	b	generic_exception
	mov	PROC_EXCEPTION, %TMP1

_arg_exception:
	b	generic_exception
	mov	ARG_EXCEPTION, %TMP1

_arith_exception:
	b	generic_exception
	mov	ARITH_EXCEPTION, %TMP1

_undef_exception:
	b	generic_exception
	mov	UNDEF_EXCEPTION, %TMP1	

! Generic exception handler for now. Jumps to the C exception handler.
! This handler will later be (re)written in Scheme. If the hander returns,
! we return to the caller and hope for the best.

generic_exception:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]
	call	_save_scheme_context
	nop
	mov	%TMP1, %g1

	save	%sp, -96, %sp
	call	_C_exception
	mov	%g1, %o0
	restore

	call	_restore_scheme_context
	nop
	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	nop

! Static data for this module.

	.seg	"data"

fnbuf:	.skip	256
Lemsg:	.asciz	"Unsupported millicode procedure at PC=%lX\n"
Lemsg2:	.asciz	"In _schemecall (the buck stops here).\n"

	! end of file

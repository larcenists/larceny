! -*- Fundamental -*-
!
! Scheme 313 Run-time system
! Miscellaneous assembly language "glue" and millicode.
!
! $Id: glue.s,v 1.3 91/08/27 13:44:34 lth Exp Locker: lth $

#include "registers.s.h"
#include "millicode.h"
#include "offsets.h"
#include "layouts.s.h"

	.seg	"text"

	.global	_scheme_call
	.global	_open_file
	.global	_close_file
	.global	_unlink_file
	.global	_read_file
	.global	_write_file
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

! `_scheme_call'
!
! Call Scheme when the VM is in Scheme mode already. The problem here is
! that when Scheme code calls a millicode procedure, it is not required to
! save any of its registers. Thus, when the millicode must call on Scheme
! again to do some work for it, the caller's context must be saved before
! the new Scheme procedure is invoked.
!
! Given a pointer to a scheme procedure in %ARGREG3, and two operands in
! %RESULT and %ARGREG2, call the scheme procedure with the given arguments.
! The return address into the calling millicode is in %o7, and the return
! address into the Scheme code that called the millicode must be in
! globals[ SAVED_RETADDR_OFFSET ].
!
! Before calling the Scheme procedure, the entire register set is saved
! in a local continuation. The saved stack frame is somewhat nonstandard:
!
!       | etc...        |
!       | saved reg 2   |
!       | saved reg 1   |
!       | saved reg 0   |
!       | return offset |   (return address into Scheme, relative to REG0)
!       | retaddr       |   (return address into calling millicode)
!	| 0             |
!       | frame size    |
! SP -> | retaddr       |   (return address into _Scheme_call)


! Offset in frame of REG0 slot.

#define REG0P	20

_scheme_call:
	ld	[ %GLOBALS + STK_LIMIT_OFFSET ], %TMP0
	cmp	%STKP, %TMP0
	bge	scheme_call_1
	nop
	jmpl	%MILLICODE + M_STKOFLOW, %o7
	nop
scheme_call_1:
	sub	%STKP, 32*4+24, %STKP		! all regs + bookkeeping + pad
	mov	32*4+16, %TMP0
	st	%TMP0, [ %STKP + 4 ]
	st	%g0, [ %STKP + 8 ]
	st	%o7, [ %STKP + 12 ]
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

	ldd	[ %STKP + 8*15 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG30_OFFSET ]
	st	%REG1, [ %GLOBALS + REG31_OFFSET ]
	ldd	[ %STKP + 8*14 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG28_OFFSET ]
	st	%REG1, [ %GLOBALS + REG29_OFFSET ]
	ldd	[ %STKP + 8*13 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG26_OFFSET ]
	st	%REG1, [ %GLOBALS + REG27_OFFSET ]
	ldd	[ %STKP + 8*12 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG24_OFFSET ]
	st	%REG1, [ %GLOBALS + REG25_OFFSET ]
	ldd	[ %STKP + 8*11 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG22_OFFSET ]
	st	%REG1, [ %GLOBALS + REG23_OFFSET ]
	ldd	[ %STKP + 8*10 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG20_OFFSET ]
	st	%REG1, [ %GLOBALS + REG21_OFFSET ]
	ldd	[ %STKP + 8*9 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG18_OFFSET ]
	st	%REG1, [ %GLOBALS + REG19_OFFSET ]
	ldd	[ %STKP + 8*8 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG16_OFFSET ]
	st	%REG1, [ %GLOBALS + REG17_OFFSET ]
	ldd	[ %STKP + 8*7 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG14_OFFSET ]
	st	%REG1, [ %GLOBALS + REG15_OFFSET ]
	ldd	[ %STKP + 8*6 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG12_OFFSET ]
	st	%REG1, [ %GLOBALS + REG13_OFFSET ]
	ldd	[ %STKP + 8*5 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG10_OFFSET ]
	st	%REG1, [ %GLOBALS + REG11_OFFSET ]
	ldd	[ %STKP + 8*4 + REG0P ], %REG0
	st	%REG0, [ %GLOBALS + REG8_OFFSET ]
	st	%REG1, [ %GLOBALS + REG9_OFFSET ]
	ldd	[ %STKP + 8*3 + REG0P ], %REG6
	ldd	[ %STKP + 8*2 + REG0P ], %REG4
	ldd	[ %STKP + 8*1 + REG0P ], %REG2
	ldd	[ %STKP + 8*0 + REG0P ], %REG0

	ld	[ %STKP + 16 ], %TMP0
	ld	[ %REG0 + A_CODEVECTOR ], %TMP1
	add	%TMP1, BVEC_TAG - 4, %TMP1
	add	%TMP0, %TMP1, %TMP0
	st	%TMP0, [ %GLOBALS + SAVED_RETADDR_OFFSET ]

	ld	[ %STKP+12 ], %o7
	jmp	%o7+8
	add	%STKP, 32*4 + 24, %STKP


! I/O primitives -- these tap right into the OS.
!
! These all assume that the right types are passed; no error checking is
! performed. How wise this is, remains to be seen.

! Open and unlink require a null-terminated string for the filename
! argument. Since the argument in is not on that form (rather, it has a length
! byte), we have to convert it.

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
! This is not particularly efficient. I couldn't care less.

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

! Close is trivial.

_close_file:
	save	%sp, -96, %sp

	call	_close
	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! Reading is straightforward -- move args to registers, call _read.

_read_file:
	save	%sp, -96, %sp

	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	add	%SAVED_ARGREG2, 4 - BVEC_TAG, %o2	! buffer pointer
	call	_read
	srl	%SAVED_ARGREG3, 2, %o1			! byte count
	sll	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! Ditto for writing.

_write_file:
	save	%sp, -96, %sp

	srl	%SAVED_RESULT, 2, %o0			! file descriptor
	add	%SAVED_ARGREG2, 4 - BVEC_TAG, %o2	! buffer pointer
	call	_write
	srl	%SAVED_ARGREG3, 2, %o1			! byte count
	sll	%o0, 2, %SAVED_RESULT			! setup result

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
! If there are any exceptions in here, the return value is set up to be that
! of the instruction which called this procedure; i.e. the operation is
! retried rather than returning into the millicode. While the latter would
! probably work, it is conceptually simpler to never have any exceptions
! return into the millicode -- the exception handler can be simpler, for
! example.

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
!
! Deals with variable-length argument lists. This is extremely hairy, and
! in addition we want good performance. One major problem is dealing with
! memory overflow requiring a collection &c.
!
! Initial state:
!  %RESULT must be a fixnum. This is not verified. Call this number 'j'.
!  `j' is the actual number of arguments.
!  %ARGREG2 is another fixnum (set up by the caller). Call this number `n'.
!  `n' is the minimum expected number of arguments.
!
! 1. If j < n then fault (wrong number of arguments).
! 2. Pick a case:
!    0. If n < 30 and j < 31, then load REGn+1 with a fresh list formed from
!       REGn+1 through REGj.
!    1. 
!
! In order to keep the code manageable, all registers are software-mapped.
! We *always* keep software and hardware copies in synch (or we die when
! the collector kicks in).

_scheme_varargs:
	cmp	%RESULT, %ARGREG2
	bge	Lvararg2
	nop
	jmp	%MILLICODE + M_ARG_EXCEPTION		! *NOT* jmpl
	sub	%o7, 8, %o7				! retry

Lvararg2:
	mov	%o7, %TMP0	
	call	_save_scheme_context
	nop
	mov	%TMP0, %o7

	save	%sp, -96, %sp
	call	_C_scheme_varargs			! Temporary.
	nop
	restore

	mov	%o7, %TMP0
	call	_restore_scheme_context
	nop
	mov	%o7, %TMP0

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

_m_debug:
	b	_not_supported
	nop

! Exit -- simply terminate the program by calling exit(), which should clean up
! most things. This is certainly good enough for the time being.

_m_exit:
	call	_save_scheme_context
	call	_exit
	mov	0, %o0

_m_reset:
	b	_not_supported
	nop

_m_break:
	b	_not_supported
	nop

! Print an error message detailing the program counter, then die.
! This routine will go away eventually.

_not_supported:
	set	emsg, %o0
	call	_save_scheme_context
	nop
	call	_printf
	mov	%o7, %o1
	call	_exit
	mov	1, %o0

! Static data for this module.

	.seg	"data"

fnbuf:	.skip	256
emsg:	.asciz	"Unsupported millicode procedure at PC=%lX\n"

	! end of file

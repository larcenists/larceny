! Scheme 313 Run-time system
! Miscellaneous assembly language "glue".
!
! $Id: glue.s,v 1.1 91/08/22 01:43:48 lth Exp Locker: lth $

#define ASSEMBLY
#include "millicode.h"
#include "offsets.h"
#include "layouts.s.h"

	.seg	"text"

	.global	_scheme_call
	.global	_open_file
	.global	_close_file
	.global	_create_file
	.global	_unlink_file
	.global	_read_file
	.global	_write_file

! `_scheme_call':
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
	st	[ %GLOBALS + REG30_OFFSET ], %REG0
	st	[ %GLOBALS + REG31_OFFSET ], %REG1
	ldd	[ %STKP + 8*14 + REG0P ], %REG0
	st	[ %GLOBALS + REG28_OFFSET ], %REG0
	st	[ %GLOBALS + REG29_OFFSET ], %REG1
	ldd	[ %STKP + 8*13 + REG0P ], %REG0
	st	[ %GLOBALS + REG26_OFFSET ], %REG0
	st	[ %GLOBALS + REG27_OFFSET ], %REG1
	ldd	[ %STKP + 8*12 + REG0P ], %REG0
	st	[ %GLOBALS + REG24_OFFSET ], %REG0
	st	[ %GLOBALS + REG25_OFFSET ], %REG1
	ldd	[ %STKP + 8*11 + REG0P ], %REG0
	st	[ %GLOBALS + REG22_OFFSET ], %REG0
	st	[ %GLOBALS + REG23_OFFSET ], %REG1
	ldd	[ %STKP + 8*10 + REG0P ], %REG0
	st	[ %GLOBALS + REG20_OFFSET ], %REG0
	st	[ %GLOBALS + REG21_OFFSET ], %REG1
	ldd	[ %STKP + 8*9 + REG0P ], %REG0
	st	[ %GLOBALS + REG18_OFFSET ], %REG0
	st	[ %GLOBALS + REG19_OFFSET ], %REG1
	ldd	[ %STKP + 8*8 + REG0P ], %REG0
	st	[ %GLOBALS + REG16_OFFSET ], %REG0
	st	[ %GLOBALS + REG17_OFFSET ], %REG1
	ldd	[ %STKP + 8*7 + REG0P ], %REG0
	st	[ %GLOBALS + REG14_OFFSET ], %REG0
	st	[ %GLOBALS + REG15_OFFSET ], %REG1
	ldd	[ %STKP + 8*6 + REG0P ], %REG0
	st	[ %GLOBALS + REG12_OFFSET ], %REG0
	st	[ %GLOBALS + REG13_OFFSET ], %REG1
	ldd	[ %STKP + 8*5 + REG0P ], %REG0
	st	[ %GLOBALS + REG10_OFFSET ], %REG0
	st	[ %GLOBALS + REG11_OFFSET ], %REG1
	ldd	[ %STKP + 8*4 + REG0P ], %REG0
	st	[ %GLOBALS + REG8_OFFSET ], %REG0
	st	[ %GLOBALS + REG9_OFFSET ], %REG1
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

! Open, create, and unlink require a null-terminated string for the filename
! argument. Since the argument in is not on that form (rather, it has a length
! byte), we have to convert it.

_open_file:
	save	%sp, -96, %sp

	call	copystring
	nop

	set	_fnbuf, %o0
	call	_open
	shrl	%SAVED_ARGREG2, 2, %o1
	shl	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

_create_file:
	save	%sp, -96, %sp

	call	copystring
	nop

	set	_fnbuf, %o0
	call	_creat
	shrl	%SAVED_ARGREG2, 2, %o1
	shl	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

_unlink_file:
	save	%sp, -96, %sp

	call	copystring
	nop

	set	_fnbuf, %o0
	call	_unlink
	nop
	shl	%o0, 2, %SAVED_RESULT

	jmp	%i7+8
	restore

! Copy the string into the local buffer, truncating if necessary, and null-
! terminating.
! This is not particularly efficient. I couldn't care less.

copystring:
	ld	[ %SAVED_RESULT - BVEC_TAG ], %l0	! get hdr
	shrl	%l0, 8, %l0				! get length
	andcc	%l0, 255, %l0				! truncate
	set	_fnbuf, %l2				! dest ptr
	stb	%g0, [ %l2 + %l0 ]			! terminator
	b	Lopen1
	add	%SAVED_RESULT, 4 - BVEC_TAG, %l1	! src ptr
Lopen0:
	ldb	[ %l1+%l0 ], %l3			! get
	stb	%l3, [ %l2+l0 ]				! put
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
	shrl	%SAVED_RESULT, 2, %o0			! file descriptor
	shr	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! Reading is straightforward -- move args to registers, call _read.

_read_file:
	save	%sp, -96, %sp

	shrl	%SAVED_RESULT, 2, %o0			! file descriptor
	shrl	%SAVED_ARGREG2, 2, %o1			! byte count
	call	_read
	add	%SAVED_ARGREG3, 4 - BVEC_TAG, %o2	! buffer pointer
	shl	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! Ditto for writing.

_write_file:
	save	%sp, -96, %sp

	shrl	%SAVED_RESULT, 2, %o0			! file descriptor
	shrl	%SAVED_ARGREG2, 2, %o1			! byte count
	call	_write
	add	%SAVED_ARGREG3, 4 - BVEC_TAG, %o2	! buffer pointer
	shl	%o0, 2, %SAVED_RESULT			! setup result

	jmp	%i7+8
	restore

! Static data for this module.

	.seg	"data"

fnbuf:	.skip	256

	! end of file

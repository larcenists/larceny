! -*- Fundamental -*-
! $Id: bench1.s,v 1.6 91/07/01 12:09:29 lth Exp Locker: lth $
!
! Hand-compiled code for the following program:
!
! (define (loop1 n)
!   (if (zero? n)
!       #t
!       (begin (f) (loop1 (- n 1)))))
!
! (define (f) #t)

#define ASSEMBLY
#include "layouts.s.h"
#include "registers.s.h"
#include "millicode.h"
#include "offsets.h"

!----------
!
! Entry code

	.global	S_ENTRY

	.seg	"text"

S_ENTRY:
	set	P_loop1, %REG0
	or	%REG0, PROC_TAG, %REG0
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	jmp	%TMP0 + A_CODEOFFSET
	nop


!----------
!
! Code for 'loop1'

	.seg	"text"

loop1:
	.word	0					! header
	ld	[ %GLOBALS+STK_LIMIT_OFFSET ], %g1	! Lower limit
	cmp	%STKP, %g1				! Overflow?
	bgt,a	L1
	sub	%STKP, 16, %STKP			! allocate frame
	ld	[ %MILLICODE+M_STKOFLOW ], %g1
	jmpl	%g1, %o7
	nop
	sub	%STKP, 16, %STKP			! allocate frame
L1:
	std	%REG0, [ %STKP+8 ]			! save proc and arg 1
	cmp	%RESULT, 4				! check for 1 argument
	mov	16, %g1					! frame size
	beq	L2					! skip if args ok
	std	%g0, [ %STKP+0 ]			! save retn & fsize
	call	Lexception				! args *not* ok
	add	%o7, (L1-(.-4))-8, %o7			! return to L1
L2:
	tsubcc	%REG1, %g0, %g0				! (= n 0) ?
	bvc	L3
	nop						! Can't fill this?

	! Generic case
	ld	[ %MILLICODE + M_ZEROP ], %TMP0		! fetch ptr
	jmpl	%TMP0, %o7				! call it
	mov	%REG1, %RESULT				! millicode argument 1
	cmp	%RESULT, TRUE_CONST			! Set condition codes

L3:
	bne,a	L4
	nop

L5:	! 'True' case
	mov	TRUE_CONST, %RESULT			! %RESULT <-- #t
	ld 	[%STKP+16], %TMP0			! Fetch return address
	jmp	%TMP0+8					! Return
	add	%STKP, 16, %STKP			! Deallocate frame

L4:	! 'False' case

	! Fake procedure fetch; in reality there'd be at least one memory
	! reference here. It probably comes out pretty even.

	set	P_f, %REG0				! %REG0 <-- f
	or	%REG0, PROC_TAG, %REG0

	! Now do non-tail call

	and	%REG0, TAGMASK, %TMP1
	cmp	%TMP1, PROC_TAG
	beq,a	L11
	deccc	%TIMER

	! not a procedure
	call	Lexception
	add	%o7, (L4-(.-4))-8, %o7			! return to L4

L11:
	bne	L12
	mov	0, %RESULT				! 0 args

	! Timer expired
	call	Lexception
	add	%o7, (L12-(.-4))-8, %o7			! return to L12

L12:
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	call	.+8
	add	%o7, (L13-(.-4))-8, %o7			! return to L13
	jmp	%TMP0 + A_CODEOFFSET			! call it!
	st	%o7, [ %STKP ]				! setup return address

L13:
	ldd	[ %STKP+8 ], %REG0			! restore proc & arg
	tsubcc	%REG1, 4, %TMP0				! n - 1
	bvc,a	L6
	mov	%TMP0, %REG1				! move arg in place

	! Generic case
	ld	[ %MILLICODE + M_SUB ], %TMP0
	mov	%REG1, %RESULT
	jmpl	%TMP0, %o7
	mov	4, %ARGREG2
	mov	%RESULT, %REG1				! move arg in place

L6:
	deccc	%TIMER					! tick
	bne	L2					! loop if not zero
	st	%REG1, [ %STKP+12 ]			! update frame!
	call	Lexception				! timer expired
	add	%o7, (L2-(.-4)), %o7			! return to L2 after

! Exception handler. Handles timer expirations, non-procedure calls, and
! wrong argument counts. Operates on the existing machine state; needs no 
! arguments.

Lexception:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop


	.seg	"data"

	.align	8
P_loop1:
	.word	0x000004FE
	.word	loop1 + BVEC_TAG

!-------------------
!
! Code for 'f'
!
! Notice that the recorded frame size (12) is less than the actual frame
! size (16) because the allocated frames must be doubleword aligned.
! This is how it should be.

	.seg	"text"

f:
	.word	0					! header
	cmp	%RESULT, 0				! check for 0 arguments
	beq	L22					! skip if args ok
	nop
	call	Lexception2				! args *not* ok
	add	%o7, (L1-(.-4))-8, %o7			! return to L1
L22:
	ld	[ %STKP ], %TMP0
	jmp	%TMP0+8
	mov	TRUE_CONST, %RESULT

Lexception2:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop

	.seg	"data"

	.align	8
P_f:
	.word	0x000004FE
	.word	f + BVEC_TAG

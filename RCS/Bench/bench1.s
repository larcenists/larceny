! -*- Fundamental -*-
! $Id: bench1.s,v 1.5 91/06/29 16:12:11 lth Exp Locker: lth $
!
! Hand-compiled code for the following program:
!
! (define (loop1 n)
!   (if (zero? n)
!       'done
!       (begin (f) (loop1 (- n 1)))))
!
! (define (f) #t)
!
! Assumptions:
! - the tail recursion in 'loop1' can be compiled to a branch.
! - f is "unknown".
! - f sets up a continuation (although it is strictly speaking not necessary.)
! - count a store double as two instructions.
! - open-code arithmetic and ``zero?''
!
! Analysis:
! - 38+X dynamic instructions per iteration (X depends on what it takes to
!   fetch a procedure from the environment.)
! - 25 dynamic instructions for the call to f and the return from f. 
! - 14 dynamic instructions from f is entered until it is left.
! - a 0-argument procedure has 10 instructions of setup overhead (creating
!   stack frame, setting up continuation, checking argument count).
! - a non-tail call takes 11 instructions after arguments have been evaluated
!   and set up (including procedure in %REG0).

!----------
!
! Code for 'loop1'

	.word	...					! Bytevector header
loop1:
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
	! ...						! %RESULT <-- 'done
	ld 	[%STKP+16], %TMP0			! Fetch return address
	jmp	%TMP0+8					! Return
	add	%STKP, 16, %STKP			! Deallocate frame

L4:	! 'False' case
	! ...						! %REG0 <-- f
	and	%REG0, TAGMASK, %TMP1
	cmp	%TMP1, PROCTAG
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
	call	.+8
	ld	[ %REG0 + CODEVECTOR ], %TMP0
	add	%o7, (L13-(.-8))-8, %o7			! return to L13
	jmp	%TMP0 + CODEOFFSET			! call it!
	st	%o7, [ %STKP ]				! setup return value

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
	nop
	call	Lexception				! timer expired
	add	%o7, (L2-(.-4)), %o7			! return to L2 after

! Exception handler. Handles timer expirations, non-procedure calls, and
! wrong argument counts. Operates on the existing machine state; needs no 
! arguments.

Lexception:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop


!-------------------
!
! Code for 'f'
!
! Notice that the recorded frame size (12) is less than the actual frame
! size (16) because the allocated frames must be doubleword aligned.
! This is how it should be.

	.word	....					! Bytevector header
f:
	ld	[ %GLOBALS+STK_LIMIT_OFFSET ], %g1	! Lower limit
	cmp	%STKP, %g1				! Overflow?
	bgt,a	L1
	sub	%STKP, 16, %STKP			! allocate frame
	ld	[ %MILLICODE+M_STKOFLOW ], %g1
	jmpl	%g1, %o7
	nop
	sub	%STKP, 16, %STKP			! allocate frame
L1:
	st	%REG0, [ %STKP+8 ]			! save proc
	cmp	%RESULT, 0				! check for 0 arguments
	mov	12, %g1					! frame size
	beq	L2					! skip if args ok
	std	%g0, [ %STKP+0 ]			! save retn & fsize
	call	Lexception				! args *not* ok
	add	%o7, (L1-(.-4))-8, %o7			! return to L1
L2:
	mov	TRUE_CONST, %RESULT
	ld	[ %STKP+16 ], %TMP0
	jmp	%TMP0+8
	add	%STKP, 16, %STKP

Lexception:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop

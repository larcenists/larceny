! -*- Fundamental -*-
! $Id$
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
! We assume that the tail recursion in 'loop1' can be compiled to a 
! branch, and that 'f' needs no prologue.
!
! Analysis in RCS/bench.txt

loop1:
	ld	[ %GLOBALS+SP_LIMIT_OFFSET ], %g1
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
	beq	L2
	std	%g0, [ %STKP+0 ]			! save retn & fsize
	ld	[ %MILLICODE+M_WRONGARGS ], %g1
	jmpl	%g1, %o7
	nop

L2:
	tsubcc	%REG1, %g0, %g0				! (= n 0) ?
	bvc	L3
	nop						! Can't fill this
	! ...						! Generic case
L3:
	bne,a	L4
	nop

L5:	! 'True' case
	! ...						! %RESULT <-- 'done
	ld 	[%STKP+16], %TMP0			! Fetch return address
	jmp	%TMP0+8					! Return
	add	%STKP, 16, %STKP			! Deallocate frame

L4:	! 'False' case
	! ...						! %REG0 <-- 'f'
	and	%REG0, TAGMASK, %TMP1
	cmp	%TMP, PROCTAG
	beq,a	L11
	deccc	%TIMER

	! not a procedure
	call	L8
	add	%o7, (L4-(.-4))-8, %o7

L11:
	bne	L12
	mov	0, %RESULT				! 0 args

	! Timer expired
	call	L8
	add	%o7, (L12-(.-4))-8, %o7

L12:
	call	.+8
	ld	[ %REG0+... ], %TMP0
	add	%o7, (.+4-L12), %o7
	jmp	%TMP0+...				! call it!
	st	%o7, [ %STKP ]

L13:	! Now returning
	tsubcc	%R1, 4, %TMP0		! n - 1
	bvc,a	L6
	mov	%TMP0, %R1		! move argument in place
	! ...				! Generic case
L6:
	deccc	%TIMER
	bne	L2
	nop
	call	L8
	add	%o7, (L2-(.-4)), %o7

! Exception handler

L8:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop


! Code for 'f'

f:
	cmp	%RESULT, 0
	beq	L1
	nop
	ld	[ %MILLICODE+M_WRONGARGS ], %g1
	jmpl	%g1, %o7
	nop
L1:
	ld	[ %STKP ], %TMP0
	jmp	%TMP0+8
	mov	TRUE_CONSTANT, %RESULT

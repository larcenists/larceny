! -*- Fundamental -*-
! $Id: bench1.s,v 1.2 91/06/26 12:21:17 lth Exp Locker: lth $
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

!----------
!
! Code for 'loop1'

	.word	...					! Bytevector header
loop1:
	ld	[ %GLOBALS+SP_LIMIT_OFFSET ], %g1	! Lower limit
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
	add	%o7, (L1-(.-4))-8, %o7
L2:
	tsubcc	%REG1, %g0, %g0				! (= n 0) ?
	bvc	L3
	nop						! Can't fill this

	! Generic case

	ld	[ %MILLICODE + M_ZEROP ], %TMP0
	jmpl	%TMP0, %o7
	mov	%REG1, %RESULT
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
	! ...						! %REG0 <-- 'f'
	and	%REG0, TAGMASK, %TMP1
	cmp	%TMP, PROCTAG
	beq,a	L11
	deccc	%TIMER

	! not a procedure
	call	Lexception
	add	%o7, (L4-(.-4))-8, %o7

L11:
	bne	L12
	mov	0, %RESULT				! 0 args

	! Timer expired
	call	Lexception
	add	%o7, (L12-(.-4))-8, %o7

L12:
	call	.+8
	ld	[ %REG0 + CODEVECTOR ], %TMP0
	add	%o7, (L13-(.-8))-8, %o7
	jmp	%TMP0 + CODEOFFSET			! call it!
	st	%o7, [ %STKP ]

	! Now returning
L13:
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

! Exception handler

Lexception:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop


!-------------------
!
! Code for 'f'
!

	.word	....					! Bytevector header
f:
	cmp	%RESULT, 0
	beq	L1
	nop
	call	Lexception
	add	%o7, (f-(.-4))-8, %o7
L1:
	ld	[ %STKP ], %TMP0
	jmp	%TMP0+8
	mov	TRUE_CONSTANT, %RESULT

Lexception:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop

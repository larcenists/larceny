! -*- Fundamental -*-
! $Id$
!
! Hand-compiled code for the following program:
!
! (define (loop2 n)
!   (if (zero? n)
!       'done
!       (loop3 (- n 1))))
!
! (define (loop3 n)
!   (if (zero? n)
!       'done
!       (loop2 (- n 1))))
!
! The analysis of the program is in RCS/bench.txt.
!
! Since the loops are identical, the code for the second one is not very
! interesting once you've seen the code for the first. Hence it's been left
! out.

	.word	....					! header
loop2:
	! Prologue

	ld	[ %GLOBALS+SP_LIMIT_OFFSET ], %g1
	cmp	%STKP, %g1				! Overflow?
	bgt,a	L1
	sub	%STKP, 16, %STKP			! allocate frame
	ld	[ %MILLICODE+M_STKOFLOW ], %g1
	jmpl	%g1, %o7
	nop
	sub	%STKP, 16, %STKP			! allocate frame
L1:
	std	%R0, [ %STKP+8 ]			! save proc and arg 1
	cmp	%RESULT, 4				! check for 1 argument
	mov	16, %g1					! frame size
	beq	L2
	std	%g0, [ %STKP+0 ]			! save retn & fsize
	ld	[ %MILLICODE+M_WRONGARGS ], %g1
	jmpl	%g1, %o7
	nop

	! Real procedure starts here
L2:
	tsubcc	%R1, %g0, %g0				! (= n 0) ?
	bvc	L3
	nop						! Can't fill this
	! ...						! Generic case
L3:
	bne,a	L4
	tsubcc	%R1, 4, %TMP0				! n - 1
L5:
	! 'True' case
	! ...						! RESULT <-- 'done
	ld 	[%STKP+16], %TMP0			! Fetch return address
	jmp	%TMP0					! Return
	add	%STKP, 16, %STKP			! Deallocate frame

L4:	! 'False' case
	bvc,a	L6
	mov	%TMP0, %REG1				! setup argument
	! ...						! Generic case


L6:	! Now do a tail call to 'loop3'
	! ...						! %REG0 <-- 'loop3'
	and	%REG0, TAGMASK, %TMP1
	cmp	%TMP1, PROCTAG				! procedure?
	beq,a	L7
	deccc	%TIMER					! Count down timer
	call	L8
	add	%o7, (L6-(.-4))-8, %o7			! Return to L6
L7:
	bne	L9					! Timer zero?
	mov	4, %RESULT				! arg count
	call	L8
	add	%o7, (L9-(.-4))-8, %o7			! Return to L9
L9:
	ld	[ %TMP0+... ], %TMP1			! get code vector
	jmp	%TMP1+...				! invoke procedure
	add	%STKP, 16, %STKP			! deallocate frame

! Exception handler for timer/non-procedure

L8:
	ld	[ %MILLICODE+M_EXCEPTION ], %TMP0
	jmp	%TMP0
	nop

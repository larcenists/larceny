!
! Scheme 313 Runtime System
! Millicode for Generic Arithmetic, SPARC.
!
! $Id$
!
! Generic arithmetic operations are daisy-chained so as to speed up operations
! of same-representation arithmetic. If representations are not the same, then
! a contagion routine is invoked to coerce operands as necessary, and the
! operation is retried. A compnum with a 0 imaginary part is treated as a
! flonum.
!
! Chain order: fixnum, flonum, compnum, bignum, ratnum, rectnum.
!
! If both operands are fixnums, then we know that the original (in-line)
! calculation overflowed, and we create a bignum.
!
! For the non-fixnum case, the chain splits: we distinguish between 
! vector-like (rectnum, ratnum) and bytevector-like (flonum, compnum, bignum)
! structures.
!
! Arithmetic for bignums, ratnums, and rectnums are done externally (C code),
! while fixnums, flonums, and compnums are handled in-line in this file.
!
! When a generic arithmetic routine is called, the operands must be in the
! millicode argument registers, and the address of the instruction which 
! invoked the arithmetic routine *must* be in %o7 so that operations can
! be retried.

	.global	_generic_add
	.global	_generic_sub
	.global	_generic_mul
	.global	_generic_neg
	.global	_generic_zerop
	.global	_generic_equalp
	.global	_generic_lessp
	.global	_generic_less_or_equalp
	.global	_generic_greaterp
	.global	_generic_greater_or_equalp


	.seg	"text"
_generic_add:
	st	%o7, [ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ]
	andcc	%RESULT, 3, %g0
	bnz	addother
	andcc	%ARGREG2, 3, %g0
	bnz	_contagion
	nop

	! add two fixnums and produce a bignum

	mov	%RESULT, %ARGREG3
	call	_alloc				! grab some memory
	mov	12, %RESULT

	sra	%ARGREG3, 2, %TMP0
	sra	%ARGREG2, 2, %TMP1
	addcc	%TMP0, %TMP1, %TMP0
	bge	addfix2
	mov	0, %TMP1
	neg	%TMP0
	mov	0xff, %TMP1
addfix2:
	sll	%TMP0, 8, %TMP2
	or	%TMP1, %TMP2, %TMP2
	st	%TMP2, [ %RESULT + 4 ]		! store sign & 3 digits
	sra	%TMP0, 24, %TMP2
	st	%TMP2, [ %RESULT + 8 ]		! store 4th digit
	mov	(5 << 8) | BIG_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	or	%RESULT, BVEC_TAG, %RESULT
	
addother:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG			! flonum, bignum, compnum?
	bnz	addother1
	cmp	%TMP1, BVEC_TAG
	bz	addother2
	nop
addother1:
	cmp	%TMP0, VEC_TAG			! ratnum, rectnum?
	bnz	_nonnumeric
	cmp	%TMP1, VEC_TAG
	bz	addother3
	nop
	b	_nonnumeric
	nop

! Daisy-chain for floats, compnums, and bignums starts here.

addother2:
	ldub	[ %RESULT + 3 - BVEC_TAG ], %TMP0
	ldub	[ %ARGREG2 + 3 - BVEC_TAG ], %TMP1
	set	_dzero, %TMP2
	lddf	[ %TMP2 ], %f0

addfloat:
	cmp	%TMP0, FLOAT_HDR
	bz	addfloat2
	cmp	%TMP0, COMP_HDR
	bnz	addcomp
	nop
	lddf	[ %RESULT + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	nop
	fbne	addcomp
	nop
addfloat2:
	cmp	%TMP1, FLOAT_HDR
	bz	addfloat3
	cmp	%TMP1, COMP_HDR
	bnz	_contagion
	nop
	lddf	[ %ARGREG3 + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	nop
	fbe	addfloat3
	cmp	%TMP0, COMP_HDR
	be	addcomp				! two comps, one with 0i
	nop
	b	_contagion			! one comp w/!0i, one float
	nop
addfloat3:

	! Add two flonums (or compnums with 0 imag part) and produce a flonum.

	mov	%RESULT, %ARGREG3
	call	_alloc
	mov	16, %RESULT

	lddf	[ %ARGREG3 + 8 - BVEC_TAG ], %f2
	lddf	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	faddd	%f2, %f4, %f2

	stdf	%f2, [ %RESULT + 8 ]
	mov	(16 << 8) | FLOAT_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	or	%RESULT, BVEC_TAG, %RESULT

addcomp:
	cmp	%TMP0, COMP_HDR
	bnz	addbig
	cmp	%TMP1, COMP_HDR
	bnz	_contagion
	nop

	! Add two compnums and produce a compnum

	mov	%RESULT, %ARGREG3
	call	_alloc
	mov	24, %RESULT

	lddf	[ %ARGREG3 + 8 - BVEC_TAG ], %f2
	lddf	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	faddd	%f2, %f4, %f2
	stdf	%f2, [ %RESULT + 8 ]

	lddf	[ %ARGREG3 + 16 - BVEC_TAG ], %f2
	lddf	[ %ARGREG2 + 16 - BVEC_TAG ], %f4
	fadd	%f2, %f4, %f2
	stdf	%f2, [ %RESULT + 16 ]

	mov	(24 << 8) | COMP_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	or	%RESULT, BVEC_TAG, %RESULT

addbig:
	cmp	%TMP0, BIG_HDR
	bnz	_nonnumeric
	cmp	%TMP1, BIG_HDR
	bnz	_contagion
	nop

	! add two bignums producing a bignum

	set	_bigadd, %g1
	b	scheme_call
	nop

! Daisy-chain for ratnums and rectnums starts here.

addother3:
	ldbu	[ %RESULT + 3 - VEC_TAG ], %TMP0
	ldbu	[ %ARGREG2 + 3 - VEC_TAG ], %TMP1

addrat:
	cmp	%TMP0, RAT_HDR
	bnz	_addrect
	cmp	%TMP1, RAT_HDR
	bnz	_contagion
	nop

	! add two ratnums producing a ratnum

	set	_ratadd, %g1
	b	scheme_call
	nop

addrect:
	cmp	%TMP0, RECT_HDR
	bnz	_nonnumeric
	cmp	%TMP1, RECT_HDR
	bnz	_contagion
	nop

	! add two rectnums producing a rectnum

	set	_rectadd, %g1
	b	scheme_call
	nop

_generic_sub:
_generic_mul:
_generic_div:
_generic_quo:
_generic_rem:
	jmp	%o7+8
	nop

! `zero?' does not have a fixnum case since that case is always handled
! fully in-line. Furthermore, there are no bignums, rectnums, or ratnums
! which are zero, but we must check for these types anyway, so that we can
! signal non-numeric exceptions.

_generic_zerop:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	bz	zerop2
	cmp	%TMP0, VEC_TAG
	bz	zerop5
	nop
	b	_nonnumeric1
	nop
zerop2:
	ldub	[ %RESULT + 3 - BVEC_TAG ], %TMP0
	set	dzero, %TMP1
	lddf	[ %TMP1 ], %f0
	cmp	%TMP0, FLOAT_HDR
	bnz	zerop3
	nop
	lddf	[ %RESULT + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	! this is a bit tricky...
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
zerop3:
	cmp	%TMP0, COMP_HDR
	bnz	zerop4
	nop
	lddf	[ %RESULT + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	nop
	fbne,a	zerop3.1
	mov	FALSE_CONST, %RESULT
	lddf	[ %RESULT + 16 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
zerop3.1:
	jmp	%o7+8
	nop
zerop4:
	cmp	%TMP0, BIG_HDR
	bz	zerop6
	nop
	b	_nonnumeric1
	nop
zerop5:
	ldub	[ %RESULT + 3 - VEC_HDR ], %TMP0
	cmp	%TMP0, RAT_HDR
	bz	zerop6
	cmp	%TMP0, RECT_HDR
	bnz	_nonnumeric1
	nop
zerop6:
	jmp	%o7+8
	mov	FALSE_CONST, %RESULT

! Given a pointer to a scheme procedure in %ARGREG3, and two operands in
! %RESULT and %ARGREG2, call the scheme procedure with the given arguments.
! The return address is already saved in ARITH_SAVED_RETADDR_OFFSET, but is
! fetched from there and stored in a stack frame which we set up here.
!
! Before calling the Scheme procedure, the entire register set is saved
! in a local continuation (this is because the caller is not required to
! save the registers before invoking an arithmetic millicode routine).
! The saved stack frame is somewhat nonstandard:
!
!       | etc...        |
!       | saved reg 2   |
!       | saved reg 1   |
!       | saved reg 0   |
!       | return offset |  Scheme return address in offset form
!       | 0             |
!       | 0             |
!       | frame size    |
! SP -> | retaddr slot  |
!
! This procedure calculates the return offset, if any, and stores it in
! the middle of the frame. The alternative is to setup two frames here,
! but that creates a mess in the event of a stack flush, since we have to
! check for a frame underflow after the Scheme code returns (since the second
! frame would not be restored automagiva

scheme_call:
	ld	[ %GLOBALS + STK_LIMIT_OFFSET ], %TMP0
	cmp	%STKP, %TMP0
	bge	scheme_call_1
	nop
	ld	[ %MILLICODE + M_STKOFLOW ], %TMP0
	jmpl	%TMP0, %o7
	nop
scheme_call_1:
	sub	%STKP, 32*4+8, %STKP		! all regs plus bookkeeping
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %TMP1
	sub	%TMP1, %TMP0, %TMP1
	sub	%TMP1, 4, %TMP1
	
	std	%REG0, [ %STKP + 8*1 ]
	std	%REG2, [ %STKP + 8*2 ]
	std	%REG4, [ %STKP + 8*3 ]
	std	%REG6, [ %STKP + 8*4 ]
	ld	[ %GLOBALS + REG8_OFFSET ], %REG0
	ld	[ %GLOBALS + REG9_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*5 ]
	ld	[ %GLOBALS + REG10_OFFSET ], %REG0
	ld	[ %GLOBALS + REG11_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*6 ]
	ld	[ %GLOBALS + REG12_OFFSET ], %REG0
	ld	[ %GLOBALS + REG13_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*7 ]
	ld	[ %GLOBALS + REG14_OFFSET ], %REG0
	ld	[ %GLOBALS + REG15_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*8 ]
	ld	[ %GLOBALS + REG16_OFFSET ], %REG0
	ld	[ %GLOBALS + REG17_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*9 ]
	ld	[ %GLOBALS + REG18_OFFSET ], %REG0
	ld	[ %GLOBALS + REG19_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*10 ]
	ld	[ %GLOBALS + REG20_OFFSET ], %REG0
	ld	[ %GLOBALS + REG21_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*11 ]
	ld	[ %GLOBALS + REG22_OFFSET ], %REG0
	ld	[ %GLOBALS + REG23_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*12 ]
	ld	[ %GLOBALS + REG24_OFFSET ], %REG0
	ld	[ %GLOBALS + REG25_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*13 ]
	ld	[ %GLOBALS + REG26_OFFSET ], %REG0
	ld	[ %GLOBALS + REG27_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*14 ]
	ld	[ %GLOBALS + REG28_OFFSET ], %REG0
	ld	[ %GLOBALS + REG29_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*15 ]
	ld	[ %GLOBALS + REG30_OFFSET ], %REG0
	ld	[ %GLOBALS + REG31_OFFSET ], %REG1
	std	%REG0, [ %STKP + 8*16 ]
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %g1
	st	%g1, [ %STKP ]
	mov	32*4+8, %g1
	st	%g1, [ %STKP + 4 ]

	sub	%STKP, 16, %STKP	! make a dummy frame
	mov	12, %g1
	std	%g0, [ %STKP ]
	st	%g0, [ %STKP+8 ]

	mov	%RESULT, %REG1
	mov	%ARGREG2, %REG2
	
	ld	[ %ARGREG3 + A_CODEVECTOR ], %TMP0
	jmpl	%TMP0 + A_CODEOFFSET, %ARGREG3
	mov	8, %RESULT

	add	%STKP, 16, %STKP	! toss dummy frame

	! aren't allowed to assume this frame exists on stack!!

	ldd	[ %STKP + 8*16 ], %REG0
	st	%REG0, [ %GLOBALS + REG30_OFFSET ]
	st	%REG1, [ %GLOBALS + REG31_OFFSET ]


! '_contagion' implements the coercion matrix for arithmetic operations.
! It assumes that the two operands are passed in %RESULT and %ARGREG2 and
! that the address of the instruction which invoked the generic arithmetic
! routine is in globals[ ARTIH_SAVED_RETADDR_OFFSET ].
!
! When the operands have been properly treated, _contagion branches to
! the address in globals[ ... ] in order to retry the operation.
!
! Also see '_pcontagion' and '_econtagion', below, which implement the matrices
! for predicates (excluding equal) and equal, respectively.

_contagion:
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7
	nop

_pcontagion:
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7
	nop

_econtagion:
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7
	nop


! '_nonnumeric' signals an exception due to non-numeric operands.
! The return address into the Scheme code must be in the variable
! globals[ ARITH_SAVED_RETADDR_OFFSET ]; if it is not, but is still in %o7,
! then use the entry point '_nonnumeric1'.

_nonnumeric1:
	st	%o7, [ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ]
_nonnumeric:

	! do something interesting; for now, just panic

	call	_panic
	mov	errmsg, %o0

! Interesting data for the generic arithmetic system.

	.seg	"data"
	.align	8
dzero:
	.double	0.0
errmsg:
	.asciz	"Non-numeric operand(s) to arithmetic operation."

	! end

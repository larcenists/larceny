! -*- Fundamental -*-
!
! Scheme 313 Runtime System
! Millicode for Generic Arithmetic, SPARC.
!
! $Id: generic.s,v 1.5 92/01/30 18:02:30 lth Exp Locker: lth $
!
! Generic arithmetic operations are daisy-chained so as to speed up operations
! of same-representation arithmetic. If representations are not the same, then
! a contagion routine is invoked to coerce operands as necessary, and the
! operation is retried. A compnum with a 0 imaginary part is treated as a
! flonum.
!
! Chain order: flonum, compnum, (fixnum,) bignum, ratnum, rectnum.
!
! For the non-fixnum case, the chain splits: we distinguish between 
! vector-like (rectnum, ratnum) and bytevector-like (flonum, compnum, bignum)
! structures.
!
! Arithmetic for bignums, ratnums, and rectnums are done externally,
! while fixnums, flonums, and compnums are handled in-line in this file.
!
! When a generic arithmetic routine is called, the operands must be in the
! millicode argument registers, and the Scheme return address must be in %o7.
!
! ACKNOWLEDGMENTS
! - Compnum algorithms were taken from Press et al: "Numerical Recipes in C", 
!   Cambridge University Press, 1988.
!
! BUGS
! - Some of this code depends on TMP0 being a global register (which 
!   is currently the case).
! - Division code (and quotient, remainder, modulus) do not check for 
!   a zero divisor.

#include "registers.s.h"
#include "milliprocs.s.h"
#include "millicode.s.h"
#include "offsets.s.h"
#include "layouts.s.h"

	.global	_generic_add			! (+ a b)
	.global	_generic_sub			! (- a b)
	.global	_generic_mul			! (* a b)
	.global	_generic_div			! (/ a b)
	.global	_generic_quo			! (quotient a b)
	.global	_generic_rem			! (remainder a b)
	.global	_generic_mod			! (modulo a b)
	.global	_generic_neg			! (- a)
	.global	_generic_abs			! (abs x)
	.global	_generic_zerop			! (zero? a)
	.global	_generic_equalp			! (= a b)
	.global	_generic_lessp			! (< a b)
	.global	_generic_less_or_equalp		! (<= a b)
	.global	_generic_greaterp		! (> a b)
	.global	_generic_greater_or_equalp	! (>= a b)
	.global	_generic_complexp		! (complex? a)
	.global	_generic_realp			! (real? a)
	.global	_generic_rationalp		! (rational? a)
	.global	_generic_integerp		! (integer? a)
	.global	_generic_exactp			! (exact? a)
	.global	_generic_inexactp		! (inexact? a)
	.global	_generic_exact2inexact		! (exact->inexact a)
	.global	_generic_inexact2exact		! (inexact->exact a)
	.global _generic_make_rectangular	! (make-rectangular a b)
	.global	_generic_real_part		! (real-part z)
	.global	_generic_imag_part		! (imag-part z)
	.global	_generic_sqrt			! (sqrt z)
	.global	_generic_round			! (round x)
	.global	_generic_truncate		! (truncate x)
	.global	_generic_negativep		! (negative? x)
	.global	_generic_positivep		! (positive? x)

	.seg	"text"

! Addition
! The fixnum case is done in line, so if the operands are fixnums, we had
! an overflow and must box the result in a bignum.

_generic_add:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Ladd_bvec
	cmp	%TMP1, BVEC_TAG
	or	%RESULT, %ARGREG2, %TMP2
	andcc	%TMP2, 3, %g0
	be,a	Ladd_fix
	nop
	cmp	%TMP0, VEC_TAG
	be,a	Ladd_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Ladd_bvec:
	be,a	Ladd_bvec2
	ldub	[ %RESULT + 3 - BVEC_TAG ], %TMP0
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_bvec2:
	ldub	[ %ARGREG2 + 3 - BVEC_TAG ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Ladd_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Ladd_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Ladd_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Ladd_flo:
	be,a	Ladd_flo2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	! Got a flonum and something; check for compnum with 0i.
	cmp	%TMP1, COMPNUM_HDR
	bne	_contagion
	mov	MS_GENERIC_ADD, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Ladd_flo2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_flo2:
	ldd	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	b	_box_flonum
	faddd	%f2, %f4, %f2
Ladd_comp:
	be,a	Ladd_comp2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	! op1 is a compnum, but perhaps op2 is a flonum and op1 has 0i.
	cmp	%TMP1, FLONUM_HDR
	bne	_contagion
	mov	MS_GENERIC_ADD, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Ladd_flo2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_comp2:
	ldd	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	ldd	[ %RESULT + 16 - BVEC_TAG ], %f6
	faddd	%f2, %f4, %f2
	ldd	[ %ARGREG2 + 16 - BVEC_TAG ], %f8
	b	_box_compnum
	faddd	%f6, %f8, %f4
Ladd_big:
	be,a	Ladd_big2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_big2:
	b	_scheme_call
	mov	MS_BIGNUM_ADD, %TMP2
Ladd_vec:
	be,a	Ladd_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_vec2:
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Ladd_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	Ladd_rect
	cmp	%TMP1, RECTNUM_HDR
	b	_non_numeric2
	nop
Ladd_rat:
	be,a	Ladd_rat2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_rat2:
	b	_scheme_call
	mov	MS_RATNUM_ADD, %TMP2
Ladd_rect:
	be,a	Ladd_rect2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_ADD, %TMP2
Ladd_rect2:
	b	_scheme_call
	mov	MS_RECTNUM_ADD, %TMP2
Ladd_fix:
	sra	%RESULT, 2, %TMP0
	sra	%ARGREG2, 2, %TMP1
	b	_box_single_bignum
	add	%TMP0, %TMP1, %TMP0

! Subtraction.
! The fixnum case is handled in line, so if the operands are fixnums, we
! had an underflow (negative result too large in magnitude) and must box
! the result in a bignum.

_generic_sub:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Lsub_bvec
	cmp	%TMP1, BVEC_TAG
	or	%RESULT, %ARGREG2, %TMP2
	andcc	%TMP2, 3, %g0
	be,a	Lsub_fix
	nop
	cmp	%TMP0, VEC_TAG
	be,a	Lsub_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Lsub_bvec:
	be,a	Lsub_bvec2
	ldub	[ %RESULT + 3 - BVEC_TAG ], %TMP0
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_bvec2:
	ldub	[ %ARGREG2 + 3 - BVEC_TAG ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Lsub_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lsub_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lsub_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Lsub_flo:
	be,a	Lsub_flo2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	! op1 is a flonum; maybe op2 is a compnum with 0i.
	cmp	%TMP1, COMPNUM_HDR
	bne	_contagion
	mov	MS_GENERIC_SUB, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lsub_flo2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_flo2:
	ldd	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	b	_box_flonum
	fsubd	%f2, %f4, %f2
Lsub_comp:
	be,a	Lsub_comp2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	! op1 is a compnum, but perhaps op2 is a flonum and op1 has 0i.
	cmp	%TMP1, FLONUM_HDR
	bne	_contagion
	mov	MS_GENERIC_SUB, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lsub_flo2
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_comp2:
	ldd	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	ldd	[ %RESULT + 16 - BVEC_TAG ], %f6
	fsubd	%f2, %f4, %f2
	ldd	[ %ARGREG2 + 16 - BVEC_TAG ], %f8
	b	_box_compnum
	fsubd	%f6, %f8, %f4
Lsub_big:
	be,a	Lsub_big2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_big2:
	b	_scheme_call
	mov	MS_BIGNUM_SUB, %TMP2
Lsub_vec:
	be,a	Lsub_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_vec2:
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Lsub_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	Lsub_rect
	cmp	%TMP1, RECTNUM_HDR
	b	_non_numeric2
	nop
Lsub_rat:
	be,a	Lsub_rat2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_rat2:
	b	_scheme_call
	mov	MS_RATNUM_SUB, %TMP2
Lsub_rect:
	be,a	Lsub_rect2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_SUB, %TMP2
Lsub_rect2:
	b	_scheme_call
	mov	MS_RECTNUM_SUB, %TMP2
Lsub_fix:
	sra	%RESULT, 2, %TMP0
	sra	%ARGREG2, 2, %TMP1
	b	_box_single_bignum
	sub	%TMP0, %TMP1, %TMP0

! Multiplication.
! Fixnums may or may not be handled in line (depending on the availablity of
! hardware multiply on the target implementation); either way we must redo the
! operation here and check for the fixnum->bignum case.

_generic_mul:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Lmul_bvec
	cmp	%TMP1, BVEC_TAG
	or	%RESULT, %ARGREG2, %TMP2
	andcc	%TMP2, 3, %g0
	be	Lmul_fix
	nop
	cmp	%TMP0, VEC_TAG
	be,a	Lmul_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Lmul_bvec:
	be,a	Lmul_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_bvec2:
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Lmul_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lmul_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lmul_big
	cmp	%TMP1, BIGNUM_HDR
	b	_contagion
	nop
Lmul_flo:
	be,a	Lmul_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	! op1 is flonum; but perhaps op2 is a compnum with 0i.
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_contagion
	mov	MS_GENERIC_MUL, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lmul_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	b	_box_flonum
	fmuld	%f2, %f4, %f2
Lmul_comp:
	be,a	Lmul_comp2
	nop
	! op1 is compnum, but perhaps op2 is a flonum and op1 has 0i.
	cmp	%TMP1, FLONUM_HDR
	bne	_contagion
	mov	MS_GENERIC_MUL, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lmul_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_comp2:
	! Needs scheduling. 
	! After scheduling, move the first load into the slot above.
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f6
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f8
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f10
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f12
	fmuld	%f6, %f10, %f14
	fmuld	%f8, %f12, %f16
	fsubd	%f14, %f16, %f2
	fmuld	%f8, %f10, %f18
	fmuld	%f16, %f12, %f20
	faddd	%f18, %f20, %f4
	b	_box_compnum
	nop
Lmul_big:
	be,a	Lmul_big2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_big2:
	b	_scheme_call
	mov	MS_BIGNUM_MUL, %TMP2
Lmul_vec:
	be,a	Lmul_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Lmul_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	Lmul_rect
	cmp	%TMP1, RECTNUM_HDR
	b	_non_numeric2
	nop
Lmul_rat:
	be,a	Lmul_rat2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_rat2:
	b	_scheme_call
	mov	MS_RATNUM_MUL, %TMP2
Lmul_rect:
	be,a	Lmul_rect2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_MUL, %TMP2
Lmul_rect2:
	b	_scheme_call
	mov	MS_RECTNUM_MUL, %TMP2
Lmul_fix:
	! This code depends on %TMP0 being a global register.
	! The %y hack below is necessary because other TMPs may not be
	! global registers. Really, all TMPs ought to be global regs.

	save	%sp, -96, %sp
	mov	%SAVED_RESULT, %o0
	call	.mul
	sra	%SAVED_ARGREG2, 2, %o1

	cmp	%o1, 0				! fit in a fixnum?
	be,a	Lmul_fix2			! yes
	mov	%o0, %SAVED_RESULT
	cmp	%o1, -1
	be,a	Lmul_fix2			! ditto
	mov	%o0, %SAVED_RESULT
	sra	%o1, 2, %o2
	cmp	%o2, 0				! fit in a one-word bignum?
	be,a	Lmul_fix3			! yes
	nop
	cmp	%o2, -1
	be,a	Lmul_fix3			! yes
	nop

	! Fits in two-word bignum. Mush together and box.
	sll	%o1, 30, %o1
	srl	%o0, 2, %o0
	or	%o0, %o1, %TMP0
	wr	%o2, %g0, %y
	b	_box_double_bignum
	restore

Lmul_fix3:
	! Fits in one-word bignum. Mush together and box.
	sll	%o1, 30, %o1
	srl	%o0, 2, %o0
	or	%o0, %o1, %TMP0
	b	_box_single_bignum
	restore

Lmul_fix2:
	jmp	%i7+8
	restore

! Division.
! Fixnum case may or may not be handled in line (depending on the availability
! of hardware divide on the target architecture); either way we have to redo
! the operation here and check for the fixnum->bignum case.

_generic_div:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Ldiv_bvec
	cmp	%TMP1, BVEC_TAG
	or	%RESULT, %ARGREG2, %TMP0
	andcc	%TMP0, 3, %g0
	be	Ldiv_fix
	nop
	cmp	%TMP0, VEC_TAG
	be,a	Ldiv_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Ldiv_bvec:
	be,a	Ldiv_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_bvec2:
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Ldiv_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Ldiv_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Ldiv_big
	cmp	%TMP1, BIGNUM_HDR
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_flo:
	be,a	Ldiv_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	! op1 is flonum; but perhaps op2 is a compnum with 0i.
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_contagion
	mov	MS_GENERIC_DIV, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Ldiv_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	b	_box_flonum
	fdivd	%f2, %f4, %f2
Ldiv_comp:
	be,a	Ldiv_comp2
	nop
	! op1 is compnum, but perhaps op2 is a flonum and op1 has 0i.
	cmp	%TMP1, FLONUM_HDR
	bne	_contagion
	mov	MS_GENERIC_DIV, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Ldiv_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_comp2:
	! needs scheduling badly.
	! when scheduled, move one instruction into the slot above.

	ldd	[ %RESULT - BVEC_TAG + 8 ], %f6
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f8
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f10
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f12
	fabsd	%f10, %f14
	fabsd	%f12, %f16
	fcmpd	%f14, %f16
	nop
	fbl	Ldiv_comp3
	nop

	! case 1: (>= (abs (real-part b)) (abs (imag-part a)))

	fdivd	%f12, %f10, %f14  ! r:   (/ (imag-part b) (real-part b))
	fmuld	%f14, %f12, %f16
	faddd	%f10, %f16, %f16  ! den: (+ (real-part b) (* r (imag-part b)))

	fmuld	%f14, %f8, %f18
	faddd	%f6, %f18, %f2
	fdivd	%f2, %f16, %f2	  ! c.r

	fmuld	%f14, %f6, %f18
	fsubd	%f8, %f18, %f4
	fdivd	%f4, %f16, %f4	  ! c.i
	b	_box_compnum
	nop

Ldiv_comp3:

	! case 2: (< (abs (real-part b)) (abs (imag-part a)))

	fdivd	%f10, %f12, %f14  ! r:   (/ (real-part b) (imag-part b))
	fmuld	%f14, %f10, %f16
	faddd	%f12, %f16, %f16  ! den: (+ (imag-part b) (* r (real-part b)))

	fmuld	%f6, %f16, %f18
	faddd	%f18, %f8, %f2
	fdivd	%f2, %f16, %f2	  ! c.r

	fmuld	%f8, %f16, %f18
	fsubd	%f18, %f6, %f4
	fdivd	%f4, %f16, %f4
	b	_box_compnum
	nop

Ldiv_big:
	be,a	Ldiv_big2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_big2:
	b	_scheme_call
	mov	MS_BIGNUM_DIV, %TMP2
Ldiv_vec:
	be,a	Ldiv_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Ldiv_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	Ldiv_rect
	cmp	%TMP1, RECTNUM_HDR
	b	_non_numeric2
	nop
Ldiv_rat:
	be,a	Ldiv_rat2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_rat2:
	b	_scheme_call
	mov	MS_RATNUM_DIV, %TMP2
Ldiv_rect:
	be,a	Ldiv_rect2
	mov	2, %TMP1
	b	_contagion
	mov	MS_GENERIC_DIV, %TMP2
Ldiv_rect2:
	b	_scheme_call
	mov	MS_RECTNUM_DIV, %TMP2
Ldiv_fix:
	! Oh, joy.
	! If the remainder of the division is 0, then we return a result as
	! expected; otherwise, the operation will generate a ratnum and the
	! whole thing is pushed into Scheme.

	save	%sp, -96, %sp
	sra	%SAVED_RESULT, 2, %o0
	call	.rem
	sra	%SAVED_ARGREG2, 2, %o1
	cmp	%o0, 0
	bne	Ldiv_fix2
	restore
	sra	%SAVED_RESULT, 2, %o0
	call	.div
	sra	%SAVED_ARGREG2, 2, %o1
	sll	%o0, 2, %SAVED_RESULT
	jmp	%i7+8
	restore
Ldiv_fix2:
	mov	2, %TMP1
	b	_scheme_call
	mov	MS_FIXNUM2RATNUM_DIV, %TMP2

! Quotient.
! Quotient must work on all integer operands, including flonums and compnums
! which are representable as integers. In order to perserve the programmer's
! sanity, only fixnums are dealt with in millicode; all other arguments are 
! passed to the "generic-quotient" procedure (in Scheme).

_generic_quo:
	or	%RESULT, %ARGREG2, %TMP0
	andcc	%TMP0, 3, %g0
	bne	Lquotient1
	nop
	save	%sp, -96, %sp
	sra	%SAVED_RESULT, 2, %o0
	call	.div
	sra	%SAVED_ARGREG2, 2, %o1
	sll	%o0, 2, %SAVED_RESULT
	jmp	%i7+8
	restore
Lquotient1:
	mov	2, %TMP1
	b	_scheme_call
	mov	MS_HEAVY_QUOTIENT, %TMP2

! Remainder.
! Same treatment of arguments as for quotient, above.
!
! The .rem procedure produces the correct signs and values for "remainder".

_generic_rem:
	or	%RESULT, %ARGREG2, %TMP0
	andcc	%TMP0, 3, %g0
	bne	Lremainder1
	nop
	save	%sp, -96, %sp
	sra	%SAVED_RESULT, 2, %o0
	call	.rem
	sra	%SAVED_ARGREG2, 2, %o1
	sll	%o0, 2, %SAVED_RESULT
	jmp	%i7+8
	restore
Lremainder1:
	mov	2, %TMP1
	b	_scheme_call
	mov	MS_HEAVY_REMAINDER, %TMP2

! Modulus.
! Same treatment of arguments as for quotient, above.
!
! The .rem procedure does not produce the correct sign and value for "modulo"
! and hence we have to adjust the result.

_generic_mod:
	or	%RESULT, %ARGREG2, %TMP0
	andcc	%TMP0, 3, %g0
	bne	Lmodulo1
	nop
	save	%sp, -96, %sp
	sra	%SAVED_RESULT, 2, %o0
	call	.rem
	sra	%SAVED_ARGREG2, 2, %o1
	xorcc	%SAVED_RESULT, %SAVED_ARGREG2, %g0
	bpos,a	Lmodulo2			! same signs
	sll	%o0, 2, %SAVED_RESULT
	sll	%o0, 2, %o0
	cmp	%SAVED_ARGREG2, 0
	bge,a	Lmodulo2
	add	%SAVED_ARGREG2, %o0, %RESULT
	sub	%SAVED_ARGREG2, %o0, %RESULT
Lmodulo2:
	jmp	%i7+8
	restore
Lmodulo1:
	mov	2, %TMP1
	b	_scheme_call
	mov	MS_HEAVY_MODULO, %TMP2

! Negation
! The fixnum case is always handled in line.

_generic_neg:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Lneg_bvec
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Lneg_vec
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_non_numeric1
	nop
Lneg_bvec:
	cmp	%TMP0, FLONUM_HDR
	be,a	Lneg_flo
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lneg_comp
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lneg_big
	mov	1, %TMP1
	b	_non_numeric1
	nop
Lneg_flo:
	b	_box_flonum
	fnegd	%f2, %f2
Lneg_comp:
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f4
	fnegd	%f2, %f2
	b	_box_compnum
	fnegd	%f4, %f4
Lneg_big:
	b	_scheme_call
	mov	MS_BIGNUM_NEGATE, %TMP2	
Lneg_vec:
	cmp	%TMP0, RATNUM_HDR
	be	Lneg_rat
	mov	1, %TMP1
	cmp	%TMP0, RECTNUM_HDR
	be	Lneg_rect
	mov	1, %TMP1
	b	_non_numeric1
	nop
Lneg_rat:
	b	_scheme_call
	mov	MS_RATNUM_NEGATE, %TMP2
Lneg_rect:
	b	_scheme_call
	mov	MS_RECTNUM_NEGATE, %TMP2

! Absolute value.
! The fixnum case is always handled in line.

_generic_abs:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Labs_bvec
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Labs_vec
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_non_numeric1
	nop
Labs_bvec:
	cmp	%TMP0, FLONUM_HDR
	be,a	Labs_flo
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP0, COMPNUM_HDR
	be,a	Labs_comp
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP0, BIGNUM_HDR
	be,a	Labs_big
	mov	1, %TMP1
	b	_non_numeric1
	nop
Labs_flo:
	b	_box_flonum
	fabsd	%f2, %f2
Labs_comp:
	b	_domain_error1
	nop
Labs_big:
	b	_scheme_call
	mov	MS_BIGNUM_ABS, %TMP2	
Labs_vec:
	cmp	%TMP0, RATNUM_HDR
	be	Labs_rat
	mov	1, %TMP1
	cmp	%TMP0, RECTNUM_HDR
	be	Labs_rect
	mov	1, %TMP1
	b	_non_numeric1
	nop
Labs_rat:
	b	_scheme_call
	mov	MS_RATNUM_ABS, %TMP2
Labs_rect:
	b	_domain_error1
	nop

! Test for zero.
! The fixnum case is always handled in line. Furthermore, there are no 
! bignums, rectnums, or ratnums which are zero, but we must check for
! these types anyway, so that we can signal non-numeric exceptions.

_generic_zerop:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Lzero_bvec
	ldub	[ %RESULT + 3 - BVEC_TAG ], %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Lzero_vec
	ldub	[ %RESULT + 3 - VEC_TAG ], %TMP0
	b	_non_numeric1
	nop
Lzero_bvec:
	cmp	%TMP0, FLONUM_HDR
	be,a	Lzero_flo
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lzero_comp
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lzero_num
	mov	FALSE_CONST, %RESULT
	b	_non_numeric1
	nop
Lzero_flo:
	fcmpd	%f0, %f2
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lzero_comp:
	fcmpd	%f0, %f2
	ldd	[ %RESULT + 16 - BVEC_TAG ], %f4
	.empty
	fbne,a	Lzero_num
	mov	FALSE_CONST, %RESULT
	fcmpd	%f0, %f4
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lzero_vec:
	cmp	%TMP0, RATNUM_HDR
	be,a	Lzero_num
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, RECTNUM_HDR
	be,a	Lzero_num
	mov	FALSE_CONST, %RESULT
	b	_non_numeric1
	nop
Lzero_num:
	jmp	%o7+8
	nop

! Equality.
! The fixnum case is handled in line.

_generic_equalp:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Lequal_bvec
	cmp	%TMP1, BVEC_TAG
	cmp	%TMP0, VEC_TAG
	be,a	Lequal_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Lequal_bvec:
	be,a	Lequal_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_bvec2:
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Lequal_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lequal_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lequal_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Lequal_flo:
	be,a	Lequal_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lequal_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	fcmpd	%f2, %f4
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lequal_comp:
	be,a	Lequal_comp2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP1, FLONUM_HDR
	bne,a	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lequal_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_comp2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f6
	fcmpd	%f2, %f4
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f8
	.empty
	fbe,a	Lequal_comp3
	fcmpd	%f6, %f8
	jmp	%o7+8
	.empty				! the next mov goes here
Lequal_comp3:
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lequal_big:
	be,a	Lequal_big2
	mov	2, %TMP1
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_big2:
	b	_scheme_call
	mov	MS_BIGNUM_EQUAL, %TMP2
Lequal_vec:
	be,a	Lequal_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Lequal_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	Lequal_rect
	cmp	%TMP1, RECTNUM_HDR
	b	_non_numeric2
	nop
Lequal_rat:
	be,a	Lequal_rat2
	mov	2, %TMP2
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_rat2:
	b	_scheme_call
	mov	MS_RATNUM_EQUAL, %TMP2
Lequal_rect:
	be,a	Lequal_rect2
	mov	2, %TMP2
	b	_econtagion
	mov	MS_GENERIC_EQUAL, %TMP2
Lequal_rect2:
	b	_scheme_call
	mov	MS_RECTNUM_EQUAL, %TMP2

! Less-than.
! Fixnums are done in-line.
! Compnums and rectnums are not in the domain of this function, but compnums
! with a 0 imaginary part are and have to be handled specially.

_generic_lessp:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Lless_bvec
	cmp	%TMP1, BVEC_TAG
	cmp	%TMP0, VEC_TAG
	be,a	Lless_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Lless_bvec:
	be,a	Lless_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
Lless_bvec2:
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Lless_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lless_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lless_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Lless_flo:
	be,a	Lless_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lless_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
Lless_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	fcmpd	%f2, %f4
	mov	FALSE_CONST, %RESULT
	blt,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lless_comp:
	be,a	Lless_comp2
	nop
	! op1 was a compnum, op2 was not. If op2 is a flonum and op1 has
	! 0i, then we're fine.
	cmp	%TMP1, FLONUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	be,a	Lless_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_domain_error2
	nop
Lless_comp2:
	! op1 and op2 were both compnums; if they both have 0i, then
	! we're fine.
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f4
	fcmpd	%f0, %f2
	nop
	fbne	_domain_error2
	nop
	fcmpd	%f0, %f4
	nop
	fbne	_domain_error2
	nop
	b	Lless_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
Lless_big:
	be,a	Lless_big2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
Lless_big2:
	b	_scheme_call
	mov	MS_BIGNUM_LESS, %TMP2
Lless_vec:
	be,a	Lless_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
Lless_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Lless_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	_domain_error2
	nop
	b	_non_numeric2
	nop
Lless_rat:
	be,a	Lless_rat2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_LESS, %TMP2
Lless_rat2:
	b	_scheme_call
	mov	MS_RATNUM_LESS, %TMP2

! Less-than-or-equal.
! Fixnums are done in-line.
! Compnums and rectnums are not in the domain of this function, but compnums
! with a 0 imaginary part are and have to be handled specially.

_generic_less_or_equalp:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Llesseq_bvec
	cmp	%TMP1, BVEC_TAG
	cmp	%TMP0, VEC_TAG
	be,a	Llesseq_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Llesseq_bvec:
	be,a	Llesseq_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
Llesseq_bvec2:
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Llesseq_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Llesseq_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Llesseq_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Llesseq_flo:
	be,a	Llesseq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Llesseq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
Llesseq_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	fcmpd	%f2, %f4
	mov	FALSE_CONST, %RESULT
	ble,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Llesseq_comp:
	be,a	Llesseq_comp2
	nop
	! op1 was a compnum, op2 was not. If op2 is a flonum and op1 has
	! 0i, then we're fine.
	cmp	%TMP1, FLONUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	be,a	Llesseq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_domain_error2
	nop
Llesseq_comp2:
	! op1 and op2 were both compnums; if they both have 0i, then
	! we're fine.
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f4
	fcmpd	%f0, %f2
	nop
	fbne	_domain_error2
	nop
	fcmpd	%f0, %f4
	nop
	fbne	_domain_error2
	nop
	b	Llesseq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
Llesseq_big:
	be,a	Llesseq_big2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
Llesseq_big2:
	b	_scheme_call
	mov	MS_BIGNUM_LESSEQ, %TMP2
Llesseq_vec:
	be,a	Llesseq_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
Llesseq_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Llesseq_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	_domain_error2
	nop
	b	_non_numeric2
	nop
Llesseq_rat:
	be,a	Lless_rat2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_LESSEQ, %TMP2
Llesseq_rat2:
	b	_scheme_call
	mov	MS_RATNUM_LESSEQ, %TMP2

! Greater-than.
! Fixnums are done in-line.
! Compnums and rectnums are not in the domain of this function, but compnums
! with a 0 imaginary part are and have to be handled specially.

_generic_greaterp:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Lgreater_bvec
	cmp	%TMP1, BVEC_TAG
	cmp	%TMP0, VEC_TAG
	be,a	Lgreater_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Lgreater_bvec:
	be,a	Lgreater_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
Lgreater_bvec2:
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Lgreater_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lgreater_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lgreater_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Lgreater_flo:
	be,a	Lgreater_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lgreater_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
Lgreater_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	fcmpd	%f2, %f4
	mov	FALSE_CONST, %RESULT
	bgt,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lgreater_comp:
	be,a	Lgreater_comp2
	nop
	! op1 was a compnum, op2 was not. If op2 is a flonum and op1 has
	! 0i, then we're fine.
	cmp	%TMP1, FLONUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	be,a	Lgreater_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_domain_error2
	nop
Lgreater_comp2:
	! op1 and op2 were both compnums; if they both have 0i, then
	! we're fine.
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f4
	fcmpd	%f0, %f2
	nop
	fbne	_domain_error2
	nop
	fcmpd	%f0, %f4
	nop
	fbne	_domain_error2
	nop
	b	Lgreater_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
Lgreater_big:
	be,a	Lgreater_big2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
Lgreater_big2:
	b	_scheme_call
	mov	MS_BIGNUM_GREATER, %TMP2
Lgreater_vec:
	be,a	Lgreater_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
Lgreater_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Lgreater_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	_domain_error2
	nop
	b	_non_numeric2
	nop
Lgreater_rat:
	be,a	Lgreater_rat2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_GREATER, %TMP2
Lgreater_rat2:
	b	_scheme_call
	mov	MS_RATNUM_GREATER, %TMP2

! Greater-than-or-equal
! Fixnums are done in-line.
! Compnums and rectnums are not in the domain of this function, but compnums
! with a 0 imaginary part are and have to be handled specially.

_generic_greater_or_equalp:
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG
	be,a	Lgreatereq_bvec
	cmp	%TMP1, BVEC_TAG
	cmp	%TMP0, VEC_TAG
	be,a	Lgreatereq_vec
	cmp	%TMP1, VEC_TAG
	b	_non_numeric2
	nop
Lgreatereq_bvec:
	be,a	Lgreatereq_bvec2
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
Lgreatereq_bvec2:
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1
	cmp	%TMP0, FLONUM_HDR
	be,a	Lgreatereq_flo
	cmp	%TMP1, FLONUM_HDR
	cmp	%TMP0, COMPNUM_HDR
	be,a	Lgreatereq_comp
	cmp	%TMP1, COMPNUM_HDR
	cmp	%TMP0, BIGNUM_HDR
	be,a	Lgreatereq_big
	cmp	%TMP1, BIGNUM_HDR
	b	_non_numeric2
	nop
Lgreatereq_flo:
	be,a	Lgreatereq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	cmp	%TMP1, COMPNUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f2
	fcmpd	%f0, %f2
	nop
	fbe,a	Lgreatereq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
Lgreatereq_flo2:
	ldd	[ %ARGREG2 - BVEC_TAG + 8 ], %f4
	fcmpd	%f2, %f4
	mov	FALSE_CONST, %RESULT
	bge,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
Lgreatereq_comp:
	be,a	Lgreatereq_comp2
	nop
	! op1 was a compnum, op2 was not. If op2 is a flonum and op1 has
	! 0i, then we're fine.
	cmp	%TMP1, FLONUM_HDR
	bne,a	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	be,a	Lgreatereq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	b	_domain_error2
	nop
Lgreatereq_comp2:
	! op1 and op2 were both compnums; if they both have 0i, then
	! we're fine.
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	ldd	[ %ARGREG2 - BVEC_TAG + 16 ], %f4
	fcmpd	%f0, %f2
	nop
	fbne	_domain_error2
	nop
	fcmpd	%f0, %f4
	nop
	fbne	_domain_error2
	nop
	b	Lgreatereq_flo2
	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
Lgreatereq_big:
	be,a	Lgreatereq_big2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
Lgreatereq_big2:
	b	_scheme_call
	mov	MS_BIGNUM_GREATEREQ, %TMP2
Lgreatereq_vec:
	be,a	Lgreatereq_vec2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
Lgreatereq_vec2:
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Lgreatereq_rat
	cmp	%TMP1, RATNUM_HDR
	cmp	%TMP0, RECTNUM_HDR
	be,a	_domain_error2
	nop
	b	_non_numeric2
	nop
Lgreatereq_rat:
	be,a	Lgreatereq_rat2
	mov	2, %TMP1
	b	_pcontagion
	mov	MS_GENERIC_GREATEREQ, %TMP2
Lgreatereq_rat2:
	b	_scheme_call
	mov	MS_RATNUM_GREATEREQ, %TMP2


! The tower of numeric types.
!
! The implementation of the predicates is rather interweaved, as we strive for
! at least some semblance of efficiency while keeping the code small.

! (define (complex? x)
!   (or (compnum? x) (rectnum? x) (real? x)))

_generic_complexp:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	bne	Lcomplexp1
	nop
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%TMP0, COMPNUM_HDR
	bne	_generic_realp_b			! other bytevector-like
	nop
	jmp	%o7+8
	mov	TRUE_CONST, %RESULT
Lcomplexp1:
	cmp	%TMP0, VEC_TAG
	bne	_generic_integerp_f			! got to be fixnum
	nop
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	cmp	%TMP0, RECTNUM_HDR
	bne	_generic_rationalp_v			! other vector-like
	nop
	jmp	%o7+8
	mov	TRUE_CONST, %RESULT

! (define (real? x)
!   (or (and (compnum? x) (= (imag-part x) 0.0)) (flonum? x) (rational? x)))

_generic_realp:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	bne	Lrealp1
	nop
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
_generic_realp_b:
	cmp	%TMP0, FLONUM_HDR
	be,a	Lrealp0
	mov	TRUE_CONST, %RESULT
	cmp	%TMP0, COMPNUM_HDR
	bne	_generic_integerp_b			! got to be bignum
	nop
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	fcmpd	%f0, %f2
	mov	TRUE_CONST, %RESULT
	fbne,a	Lrealp0
	mov	FALSE_CONST, %RESULT
Lrealp0:
	jmp	%o7+8
	nop
Lrealp1:
	cmp	%TMP1, VEC_TAG
	bne	_generic_integerp_f			! got to be fixnum
	nop
	b	_generic_rationalp_v			! vector-like
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0

! (define (rational? x)
!   (or (ratnum? x) (integer? x)))

_generic_rationalp:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, VEC_TAG
	bne	_generic_integerp
	nop
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
_generic_rationalp_v:
	cmp	%TMP0, RATNUM_HDR
	mov	TRUE_CONST, %RESULT
	bne,a	.+8
	mov	FALSE_CONST, %RESULT
	jmp	%o7+8
	nop

! (define (integer? x)
!   (or (bignum? x)
!       (fixnum? x)
!       (or (and (flonum? x) (representable-as-int? x))
!           (and (compnum? x) 
!                (= (imag-part x) 0.0)
!                (representable-as-int? (real-part x))))))

_generic_integerp:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	bne	_generic_integerp_f
	nop
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
_generic_integerp_b:
	cmp	%TMP0, BIGNUM_HDR
	bne	Lintegerp1
	nop
	jmp	%o7+8
	mov	TRUE_CONST, %RESULT
Lintegerp1:
	! It is a bytevector, and it is not a bignum. Ergo, it is a flonum or
	! a compnum. Check to see if the real part is representable as an
	! integer. If so, jump to `real?' for the final test (this test could
	! be done in-line for greater efficiency). Otherwise, return #f.
	!
	! The real part is representable as an integer if the magnitude of
	! the unbiased exponent is <= the number of bits in the mantissa 
	! (including the hidden bit):
	!
	! (define (representable-as-integer? x)
	!   (let ((e (- (exponent x) bias)))
	!     (and (>= x 0) (<= x 53))))

	ld	[ %RESULT - BVEC_TAG + 8 ], %TMP1	! get hi word
	srl	%TMP1, 20, %TMP1
	and	%TMP1, 0x7FF, %TMP1			! now has biased expt
	cmp	%TMP1, 1023
	blt	Lintegerp2				! < 0
	mov	FALSE_CONST, %RESULT
	cmp	%TMP1, 1023+53
	bgt	Lintegerp2				! > 53
	mov	FALSE_CONST, %RESULT
	b	_generic_realp_b
	nop	
_generic_integerp_f:
	andcc	%RESULT, 3, %g0
	mov	TRUE_CONST, %RESULT
	bne,a	.+8
	mov	FALSE_CONST, %RESULT
Lintegerp2:
	jmp	%o7+8
	nop


! Exactness maps trivially to representation (or the other way around.)

! (define (exact? x)
!   (cond ((or (fixnum? x) (bignum? x) (ratnum? x) (rectnum? x)) #t)
!         ((or (compnum? x) (flonum? x)) #f)
!         (else (error ...))))

_generic_exactp:
	mov	TRUE_CONST, %ARGREG2
	mov	FALSE_CONST, %ARGREG3
_generic_exactness_test:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, VEC_TAG
	bne	Lexactp1
	nop
	! It's a vector.
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	cmp	%TMP0, RATNUM_HDR
	be	Lexactp99
	cmp	%TMP0, RECTNUM_HDR
	be	Lexactp99
	nop
	b	_non_numeric1
	nop
Lexactp1:
	cmp	%TMP0, BVEC_TAG
	bne	Lexactp2
	nop
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%TMP0, BIGNUM_HDR
	be	Lexactp99
	cmp	%TMP0, FLONUM_HDR
	be	Lexactp98
	cmp	%TMP0, COMPNUM_HDR
	be	Lexactp98
	nop
	b	_non_numeric1
	nop
Lexactp2:
	andcc	%RESULT, 3, %g0
	bne	_non_numeric1
	nop
Lexactp99:
	jmp	%o7+8
	mov	%ARGREG2, %RESULT
Lexactp98:
	jmp	%o7+8
	mov	%ARGREG3, %RESULT

! (define (inexact? x)
!   (cond ((or (compnum? x) (flonum? x)) #t)
!         ((or (fixnum? x) (flonum? x) (ratnum? x) (rectnum? x)) #f)
!         (else (error ...))))

_generic_inexactp:
	mov	FALSE_CONST, %ARGREG2
	b	_generic_exactness_test
	mov	TRUE_CONST, %ARGREG3


! Fixnum->flonum and identity operations are interesting; everything else
! is not, and should be handled by Scheme.
!
! (define (exact->inexact a)
!   (cond ((inexact? a) a)
!         ((rectnum? a) (rectnum->compnum a))
!         ((ratnum? a)  (ratnum->flonum a))
!         ((bignum? a)  (bignum->flonum a))
!         ((fixnum? a)  (fixnum->flonum a))
!         (else ???)))

_generic_exact2inexact:
	andcc	%RESULT, 3, %g0
	be,a	Lfixnum2flonum
	st	%RESULT, [ %GLOBALS + RESULT_OFFSET ]
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Le2i_maybe
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
Le2i_noway:
	! Not fixnum, not identity operation.
	! Drop into scheme (in a tailcall)

	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

Le2i_maybe:
	! Could be flonum or compnum (neither of which is exact in the 
	! first place).

	cmp	%TMP0, FLONUM_HDR
	be	Le2i_identity
	cmp	%TMP0, COMPNUM_HDR
	bne	Le2i_noway
	nop
Le2i_identity:
	jmp	%o7+8
	nop

Lfixnum2flonum:
	ld	[ %GLOBALS + RESULT_OFFSET ], %f2
	b	_box_flonum
	fitod	%f2, %f2

! Identity operations are handled in-line, as is the flonum->integer case.
! The compnum->rectnum case drops into Scheme.
!
! (define (inexact->exact a)
!   (cond ((exact? a) a)
!         ((flonum? a) (flonum->integer a))
!         ((compnum? a) (compnum->rectnum a))
!         (else ???)))

_generic_inexact2exact:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	bne	Li2e_identity
	nop
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%TMP0, FLONUM_HDR
	bne	Li2e_noway
	nop

	! flonum->integer. Must distinguish between integers fitting in a
	! fixnum and those fitting in a bignum (latter must be boxed and all
	! that; should go to Scheme?).

	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

Li2e_noway:
	! Others. Drop into Scheme (tailcall).

	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

Li2e_identity:
	jmp	%o7+8
	nop

! `make-rectangular' is actually a bit hairy. Should it just go into Scheme?
! (Possibly flonum+flonum->compnum case should be in line, for speed).
!
! (define (make-rectangular a b)
!   (if (and (exact? a) (exact? b))
!       (if (not (zero? b))
!           (make-rectnum a b)
!           a)
!       (make-compnum a b)))
!
! (define (make-rectnum a b)
!   (let ((v (make-vector 2)))
!     (vector-like-set! v 0 a)
!     (vector-like-set! v 1 b)
!     (typetag-set! v RECTNUM_TYPETAG)
!     v))
!
! (define (make-compnum a b)
!   (if (or (compnum? a) (compnum? b) (rectnum? a) (rectnum? b))
!       (error ...)
!       (box-compnum (exact->inexact a) (exact->inexact b))))

_generic_make_rectangular:
	mov	2, %TMP1
	b	_scheme_call
	mov	MS_GENERIC_MAKE_RECTANGULAR, %TMP2


! `real-part' and `imag-part'.
!
! (define (real-part z)
!   (cond ((compnum? z) (compnum-real-part z))
!         ((rectnum? z) (rectnum-real-part z))
!         ((number? z) z)
!         (else (error ...))))

_generic_real_part:
	mov	8-BVEC_TAG, %TMP1
	mov	0-VEC_TAG, %TMP2
	set	Lcompop4, %ARGREG2

Lcompop0:
	! Given fixnum indices into compnums and rectnums in TMP1 and 
	! TMP2, and a pointer to a resolution routine for non-complex 
	! numbers in ARGREG2, do real_part/imag_part in one piece of code.
	! The exactness is kept track of in ARGREG3: 0=exact, 4=inexact.

	and	%RESULT, TAGMASK, %TMP0
	cmp	%RESULT, BVEC_TAG
	be	Lcompop1
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%RESULT, VEC_TAG
	be	Lcompop2
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	b	Lcompop3
	nop

Lcompop1:
	! It is a bytevector
	cmp	%TMP0, COMPNUM_HDR
	be,a	_box_flonum
	ldd	[ %RESULT + %TMP1 ], %f2
	cmp	%TMP0, BIGNUM_HDR
	be	Lgoto
	mov	0, %ARGREG3
	cmp	%TMP0, FLONUM_HDR
	be	Lgoto
	mov	4, %ARGREG3
	b	_non_numeric1
	nop

Lcompop2:
	! It is a vector
	cmp	%TMP0, RECTNUM_HDR
	be,a	Lcompop4
	ld	[ %RESULT + %TMP2 ], %RESULT
	cmp	%TMP0, RATNUM_HDR
	be	Lgoto
	mov	0, %ARGREG3
	b	_non_numeric1
	nop

Lcompop3:
	! It is neither bytevector nor vector.
	andcc	%RESULT, 3, %g0
	be	Lgoto
	mov	0, %ARGREG3
	b	_non_numeric1
	nop

Lcompop4:
	jmp	%o7+8
	nop

Lgoto:
	jmp	%ARGREG2
	nop

! (define (imag-part z)
!   (cond ((compnum? z) (compnum-imag-part z))
!         ((rectnum? z) (rectnum-imag-part z))
!         ((number? z) (if (exact? z) #e0 #i0))
!         (else (error ...))))

_generic_imag_part:
	set	Limag_part2, %ARGREG2
	mov	16-BVEC_TAG, %TMP1
	b	Lcompop0
	mov	4-VEC_TAG, %TMP2

Limag_part2:
	! Getting the imag part from a non-complex: return 0, with the
	! correct exactness. Recall that the exactness spec is in ARGREG3.

	tst	%ARGREG3
	bne,a	_box_flonum
	fmovd	%f0, %f2
	jmp	%o7+8
	mov	%g0, %RESULT

! These will return the argument in the case of fixnum, bignum, or ratnum;
! will return a new number in the case of a flonum (or compnum with 0i);
! and will give a domain error for compnums with non-0i and rectnums.

_generic_round:
	set	Lround, %TMP1
	b	Lgeneric_trund
	nop

_generic_truncate:
	set	Ltrunc, %TMP1
	b	Lgeneric_trund
	nop

Lround:
	! Flonum is pointed to by %RESULT. Round it, box it, and return.
	! The simple way to round is to add .5 and then truncate.
	! [There ought to be a way to move stuff from flonum regs to int regs
	!  short of pushing it thru memory. >groan<]

	ldd	[ %RESULT - BVEC_TAG + 8 ], %f2
	set	Ldhalf, %TMP0 
	ldd	[ %TMP0 ], %f4
	faddd	%f2, %f4, %f2
	std	%f2, [ %TMP0 + 8 ]
	save	%sp, -96, %sp
	b	Ltrunc2
	ldd	[ %TMP0 + 8 ], %l0

Ltrunc:
	! flonum is pointed to by %RESULT. Trunc it, box it, and return.

	save	%sp, -96, %sp
	ldd	[ %SAVED_RESULT - BVEC_TAG + 8 ], %l0
Ltrunc2:
	srl	%l0, 20, %l2			! get at exponent
	and	%l2, 0x7FF, %l2			! toss sign
	subcc	%l2, 1023, %l2			! unbias
	blt,a	Ltrunc2zero
	mov	%g0, %l1
	cmp	%l2, 52
	bge	Ltrunc_moveback
	nop
	cmp	%l2, 20
	ble,a	Ltrunc_high
	mov	%g0, %l1

	! mask off low word.
	mov	52, %l3
	sub	%l3, %l2, %l2
	srl	%l1, %l2, %l1
	b	Ltrunc_moveback
	sll	%l1, %l2, %l1

Ltrunc_high:
	! zero out lo word, mask off high word
	mov	20, %l3
	sub	%l3, %l2, %l2
	srl	%l0, %l2, %l0
	b	Ltrunc_moveback
	sll	%l0, %l2, %l0

Ltrunc2zero:
	sethi	%hi( 0x80000000 ), %l2
	and	%l0, %l2, %l0			! get sign right

Ltrunc_moveback:
	! move back into fp regs

	std	%l0, [ %TMP0 + 2 ]
	ldd	[ %TMP0 + 2 ], %f2
	b	_box_flonum
	restore

! Generic code for rounding and truncation. Address of final procedure 
! is in %TMP1.

Lgeneric_trund:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltrund_bvec
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	andcc	%RESULT, 3, %g0
	be,a	Ltrund_def
	nop
	cmp	%TMP0, VEC_TAG
	be,a	Ltrund_vec
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	b	_non_numeric1
	nop
Ltrund_bvec:
	cmp	%TMP0, FLONUM_HDR
	be,a	Ltrund_flo
	nop
	cmp	%TMP0, COMPNUM_HDR
	be,a	Ltrund_comp
	ldd	[ %RESULT - BVEC_TAG + 16 ], %f2
	cmp	%TMP0, BIGNUM_HDR
	be,a	Ltrund_def
	nop
	b	_non_numeric1
	nop
Ltrund_flo:
	jmp	%TMP1
	nop
Ltrund_comp:
	fcmpd	%f0, %f2
	nop
	fbne,a	_domain_error1
	nop
	jmp	%TMP1
	nop
Ltrund_vec:
	cmp	%TMP0, RATNUM_HDR
	be,a	Ltrund_def
	nop
	cmp	%TMP0, RECTNUM_HDR
	be,a	_domain_error1
	nop
	b	_non_numeric1
	nop
Ltrund_def:
	jmp	%o7+8
	nop
! Not yet done in millicode.

_generic_negativep:
_generic_positivep:
_generic_sqrt:
	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop


! '_contagion' implements the coercion matrix for arithmetic operations.
! It assumes that the two operands are passed in %RESULT and %ARGREG2 and
! that the scheme return address is in %o7.
! In addition, %TMP2 has the index into the millicode support vector
! of the procedure which is to be called on to retry the operation.
!
_contagion:
	b	Lcontagion
	mov	MS_CONTAGION, %TMP0
_pcontagion:
	b	Lcontagion
	mov	MS_PCONTAGION, %TMP0
_econtagion:
	mov	MS_ECONTAGION, %TMP0
Lcontagion:
	ld	[ %GLOBALS + MILLICODE_SUPPORT_OFFSET ], %TMP1
	ld	[ %TMP1 - GLOBAL_CELL_TAG + CELL_VALUE_OFFSET ], %TMP1
	add	%TMP1, 4 - VEC_TAG, %TMP1	! bump ptr
	ld	[ %TMP1 + %TMP2 ], %ARGREG3	! scheme proc to retry
	ld	[ %TMP1 + %TMP0 ], %TMP2	! contagion proc
	b	_scheme_call
	mov	3, %TMP1			! argument count

! Various exception conditions.

_non_numeric1:
_non_numeric2:
	set	Lerrmsg1, %TMP0
	b	Lerror
	nop

_domain_error1:
_domain_error2:
	set	Lerrmsg3, %TMP0
	b	Lerror
	nop

! Generic error handler for now.
! The procedure localdebugger() must not call Scheme!

Lerror:
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]

	call	_save_scheme_context
	nop

	save	%sp, -96, %sp
	mov	%TMP0, %o0
	call	_printf
	nop
	call	_localdebugger
	nop
	restore

	call	_restore_scheme_context
	nop

	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7 + 8
	nop


!-----------------------------------------------------------------------------
! Box various numbers.

! Box the double in %f2/f3 as a flonum. Return tagged pointer in RESULT.
! Scheme return address in %o7.

_box_flonum:
	mov	%o7, %TMP0
	call	_internal_alloc
	mov	16, %RESULT
	mov	%TMP0, %o7

	std	%f2, [ %RESULT + 8 ]
	set	(16 << 8) | FLONUM_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	jmp	%o7 + 8
	add	%RESULT, BVEC_TAG, %RESULT


! Box the two doubles in %f2/%f3 and %f4/%f5 as a compnum.
! Return tagged pointer in RESULT.
! Scheme return address in %o7.

_box_compnum:
	mov	%o7, %TMP0
	call	_internal_alloc
	mov	24, %RESULT
	mov	%TMP0, %o7

	std	%f2, [ %RESULT + 8 ]
	std	%f4, [ %RESULT + 16 ]
	set	(24 << 8) | COMPNUM_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	jmp	%o7+8
	add	%RESULT, BVEC_TAG, %RESULT

! Box an integer in a bignum with one digit. The integer is passed in %TMP0.
! %o7 has the Scheme return address.

_box_single_bignum:
	st	%TMP0, [ %GLOBALS + GEN_TMP1_OFFSET ]
	mov	%o7, %TMP0
	call	_internal_alloc
	mov	12, %RESULT
	mov	%TMP0, %o7
	ld	[ %GLOBALS + GEN_TMP1_OFFSET ], %TMP0

	cmp	%TMP0, 0
	bge	Lbox_s_fix2
	set	(0 << 16) + 1, %TMP1		! sign + length FIXME?
	neg	%TMP0
	set	(1 << 16) + 1, %TMP1		! ditto, negative
Lbox_s_fix2:
	st	%TMP1, [ %RESULT + 4 ]		! store sign, length
	st	%TMP0, [ %RESULT + 8 ]		! store number
	set	(8 << 8) | BIGNUM_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	jmp	%o7+8
	or	%RESULT, BVEC_TAG, %RESULT

! Box an integer in a bignum with two digits. The integer is passed in
! %TMP0 (low word) and %y (high word). [This is a hack but it works well.]
! %o7 has the Scheme return address.

_box_double_bignum:
	st	%TMP0, [ %GLOBALS + GEN_TMP1_OFFSET ]
	mov	%o7, %TMP0
	call	_internal_alloc
	mov	16, %RESULT
	mov	%TMP0, %o7
	ld	[ %GLOBALS + GEN_TMP1_OFFSET ], %TMP0

	/*SCAFFOLDING CODE*/
	save	%sp, -96, %sp
	set	Lerrmsg3, %o0
	call	_printf
	nop
	call	_exit
	mov	1, %o0
	/*NOTREALLYREACHED*/
	restore

	jmp	%o7+8
	nop

! Interesting data for the generic arithmetic system.

	.seg	"data"
Lerrmsg1:
	.asciz	"Non-numeric operand(s) to arithmetic operation.\n"
Lerrmsg2:
	.asciz	"Domain error in operand(s) to arithmetic operation.\n"
Lerrmsg3:
	.asciz	"Can't box a double bignum (yet).\n"

	.align 8
Ldhalf:
	.double	0r0.5		! 0.5; leave it here.
	.double 0r0.0		! this is a temp and DON'T MOVE IT!!

	! end

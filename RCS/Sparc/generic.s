! -*- Fundamental -*-
!
! Scheme 313 Runtime System
! Millicode for Generic Arithmetic, SPARC.
!
! $Id: generic.s,v 1.2 91/09/09 18:58:35 lth Exp Locker: lth $
!
! Generic arithmetic operations are daisy-chained so as to speed up operations
! of same-representation arithmetic. If representations are not the same, then
! a contagion routine is invoked to coerce operands as necessary, and the
! operation is retried. A compnum with a 0 imaginary part is treated as a
! flonum.
!
! Chain order: flonum, compnum, (fixnum,) bignum, ratnum, rectnum.
!
! If both operands are fixnums, then we know that the original (in-line)
! calculation overflowed, and we create a bignum.
!
! For the non-fixnum case, the chain splits: we distinguish between 
! vector-like (rectnum, ratnum) and bytevector-like (flonum, compnum, bignum)
! structures.
!
! Arithmetic for bignums, ratnums, and rectnums are done externally,
! while fixnums, flonums, and compnums are handled in-line in this file.
!
! When a generic arithmetic routine is called, the operands must be in the
! millicode argument registers, and the address of the instruction which 
! invoked the arithmetic routine *must* be in %o7 so that operations can
! be retried.

#include "registers.s.h"
#include "millicode.h"
#include "offsets.h"
#include "layouts.s.h"

	.global	_generic_add			! (+ a b)
	.global	_generic_sub			! (- a b)
	.global	_generic_mul			! (* a b)
	.global	_generic_div			! (/ a b)
	.global	_generic_quo			! (quotient a b)
	.global	_generic_rem			! (remainder a b)
	.global	_generic_mod			! (modulo a b)
	.global	_generic_neg			! (- a)
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

	.seg	"text"
_generic_add:
	! Not needed in bootstrap system, and not tested, so let's not
	! take any chances.

	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

	! Real code

	st	%o7, [ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ]
	and	%RESULT, TAGMASK, %TMP0
	and	%ARGREG2, TAGMASK, %TMP1
	cmp	%TMP0, BVEC_TAG			! flonum, bignum, compnum?
	bnz	addother
	cmp	%TMP1, BVEC_TAG
	bz	addbytevec
	nop
addother:
	andcc	%RESULT, 3, %g0
	bnz	addother1
	andcc	%ARGREG2, 3, %g0
	bz	addfix
	nop
addother1:
	cmp	%TMP0, VEC_TAG			! ratnum, rectnum?
	bnz	_contagion
	cmp	%TMP1, VEC_TAG
	bz	addvec
	nop
	b	_contagion
	nop

	! Add two fixnums and produce a bignum.

addfix:
	mov	%RESULT, %ARGREG3
	call	_alloc				! grab some memory
	mov	12, %RESULT			! 12 bytes is enough

	sra	%ARGREG3, 2, %TMP0
	sra	%ARGREG2, 2, %TMP1
	addcc	%TMP0, %TMP1, %TMP0
	bge	addfix2
	set	(0 << 16) + 1, %TMP1		! sign + length
	neg	%TMP0
	set	(1 << 16) + 1, %TMP1		! ditto, negative
addfix2:
	st	%TMP1, [ %RESULT + 4 ]		! store sign, length
	st	%TMP0, [ %RESULT + 8 ]		! store number
	set	(8 << 8) | BIGNUM_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	or	%RESULT, BVEC_TAG, %RESULT
	
! Daisy-chain for floats, compnums, and bignums starts here.

addbytevec:
	ldub	[ %RESULT + 3 - BVEC_TAG ], %TMP0
	ldub	[ %ARGREG2 + 3 - BVEC_TAG ], %TMP1

addfloat:
	cmp	%TMP0, FLONUM_HDR
	bz	addfloat2
	cmp	%TMP0, COMPNUM_HDR
	bnz	addcomp
	nop
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	nop
	fbne	addcomp
	nop
addfloat2:
	cmp	%TMP1, FLONUM_HDR
	bz	addfloat3
	cmp	%TMP1, COMPNUM_HDR
	bnz	_contagion
	nop
	ldd	[ %ARGREG3 + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	nop
	fbe	addfloat3
	cmp	%TMP0, COMPNUM_HDR
	be	addcomp				! two comps, one with 0i
	nop
	b	_contagion			! one comp w/!0i, one float
	nop
addfloat3:

	! Add two flonums (or compnums with 0 imag part) and produce a flonum.

	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	ldd	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	faddd	%f2, %f4, %f2

	b	_box_flonum
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7

addcomp:
	cmp	%TMP0, COMPNUM_HDR
	bnz	addbig
	cmp	%TMP1, COMPNUM_HDR
	bnz	_contagion
	nop

	! Add two compnums and produce a compnum

	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	ldd	[ %ARGREG2 + 8 - BVEC_TAG ], %f4
	faddd	%f2, %f4, %f2

	ldd	[ %RESULT + 16 - BVEC_TAG ], %f2
	ldd	[ %ARGREG2 + 16 - BVEC_TAG ], %f4
	faddd	%f2, %f4, %f2

	b	_box_compnum
	ld	[ %GLOBALS + ARITH_SAVED_RETADDR_OFFSET ], %o7

addbig:
	cmp	%TMP0, BIGNUM_HDR
	bnz	_nonnumeric
	cmp	%TMP1, BIGNUM_HDR
	bnz	_contagion
	nop

	! add two bignums producing a bignum

	b	_not_supported
	nop

!	set	_bigadd, %g1
!	b	_scheme_call
!	nop

! Daisy-chain for ratnums and rectnums starts here.

addvec:
	ldub	[ %RESULT + 3 - VEC_TAG ], %TMP0
	ldub	[ %ARGREG2 + 3 - VEC_TAG ], %TMP1

addrat:
	cmp	%TMP0, RATNUM_HDR
	bnz	addrect
	cmp	%TMP1, RATNUM_HDR
	bnz	_contagion
	nop

	! add two ratnums producing a ratnum

	b	_not_supported
	nop

!	set	_ratadd, %g1
!	b	_scheme_call
!	nop

addrect:
	cmp	%TMP0, RECTNUM_HDR
	bnz	_nonnumeric
	cmp	%TMP1, RECTNUM_HDR
	bnz	_contagion
	nop

	! add two rectnums producing a rectnum

	b	_not_supported
	nop

!	set	_rectadd, %g1
!	b	_scheme_call
!	nop

! Should never be invoked in bootstrap system.

_generic_sub:
	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

! This multiplication code is temporary; it handles fixnums which multiply w/o
! overflow. Other types are caught, but overflow is not.

_generic_mul:
	or	%RESULT, %ARGREG2, %TMP0
	and	%TMP0, 3, %g0
	bne	Lmul1
	nop
	save	%sp, -96, %sp
	mov	%SAVED_RESULT, %o0
	call	.mul
	sra	%SAVED_ARGREG2, 2, %o1
	mov	%o0, %SAVED_RESULT		! this assumes no overflow!
	jmp	%i7+8
	restore
Lmul1:
	jmp	%MILLICODE + M_TYPE_EXCEPTION
	nop

! Division is treated as quotient in the bootstrap system (no ratnums 
! supported).

_generic_div:
	b	_generic_quo
	nop

! Similar to multiplication (above) in that it is temporary.

_generic_quo:
	or	%RESULT, %ARGREG2, %TMP0
	and	%TMP0, 3, %g0
	bne	Lquo1
	nop
	save	%sp, -96, %sp
	mov	%SAVED_RESULT, %o0
	call	.div
	sra	%SAVED_ARGREG2, 2, %o1
	mov	%o0, %SAVED_RESULT
	jmp	%i7+8
	restore
Lquo1:
	jmp	%MILLICODE + M_TYPE_EXCEPTION
	nop

! Similar to multiplication (above) in that it is temporary.

_generic_rem:
	or	%RESULT, %ARGREG2, %TMP0
	and	%TMP0, 3, %g0
	bne	Lrem1
	nop
	save	%sp, -96, %sp
	mov	%SAVED_RESULT, %o0
	call	.rem
	sra	%SAVED_ARGREG2, 2, %o1
	mov	%o0, %SAVED_RESULT
	jmp	%i7+8
	restore
Lrem1:
	jmp	%MILLICODE + M_TYPE_EXCEPTION
	nop

! Modulus

_generic_mod:
	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

! Negation

_generic_neg:
	jmp	%MILLICODE + M_NOT_SUPPORTED
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
	cmp	%TMP0, FLONUM_HDR
	bnz	zerop3
	nop
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	! this is a bit tricky...
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
	jmp	%o7+8
	nop
zerop3:
	cmp	%TMP0, COMPNUM_HDR
	bnz	zerop4
	nop
	ldd	[ %RESULT + 8 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	nop
	fbne,a	zerop3.1
	mov	FALSE_CONST, %RESULT
	ldd	[ %RESULT + 16 - BVEC_TAG ], %f2
	fcmpd	%f0, %f2
	mov	FALSE_CONST, %RESULT
	fbe,a	.+8
	mov	TRUE_CONST, %RESULT
zerop3.1:
	jmp	%o7+8
	nop
zerop4:
	cmp	%TMP0, BIGNUM_HDR
	bz	zerop6
	nop
	b	_nonnumeric1
	nop
zerop5:
	ldub	[ %RESULT + 3 - VECTOR_HDR ], %TMP0
	cmp	%TMP0, RATNUM_HDR
	bz	zerop6
	cmp	%TMP0, RECTNUM_HDR
	bnz	_nonnumeric1
	nop
zerop6:
	jmp	%o7+8
	mov	FALSE_CONST, %RESULT

! These are not needed to bootstrap the system.

_generic_equalp:
_generic_lessp:
_generic_less_or_equalp:
_generic_greaterp:
_generic_greater_or_equalp:
	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop

! The tower of numbers.
! The implementation of the predicates is rather interweaved, as we strive for
! at least some semblance of efficiency while keeping the code small.

! (define (complex? x)
!   (or (compnum? x) (rectnum? x) (real? x)))

_generic_complexp:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%RESULT, BVEC_TAG
	bne	Lcomplexp1
	nop
	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	cmp	%TMP0, COMPNUM_HDR
	bne	_generic_realp_b			! other bytevector-like
	nop
	jmp	%o7+8
	mov	TRUE_CONST, %RESULT
Lcomplexp1:
	cmp	%RESULT, VEC_TAG
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
	cmp	%RESULT, BVEC_TAG
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
	cmp	%RESULT, VEC_TAG
	bne	_generic_integerp_f			! got to be fixnum
	nop
	b	_generic_rationalp_v			! vector-like
	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0

! (define (rational? x)
!   (or (ratnum? x) (integer? x)))

_generic_rationalp:
	and	%RESULT, TAGMASK, %TMP0
	cmp	%RESULT, VEC_TAG
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
	cmp	%RESULT, BVEC_TAG
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
	and	%RESULT, 3, %g0
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
	b	_nonnumeric1
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
	b	_nonnumeric1
	nop
Lexactp2:
	andcc	%RESULT, 3, %g0
	bne	_nonnumeric1
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
!   (cond ((inexact? a) a)
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
	jmp	%o7+8
	nop


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
	b	_nonnumeric1
	nop

Lcompop2:
	! It is a vector
	cmp	%TMP0, RECTNUM_HDR
	be,a	Lcompop4
	ld	[ %RESULT + %TMP2 ], %RESULT
	cmp	%TMP0, RATNUM_HDR
	be	Lgoto
	mov	0, %ARGREG3
	b	_nonnumeric
	nop

Lcompop3:
	! It is neither bytevector nor vector.
	andcc	%RESULT, 3, %g0
	be	Lgoto
	mov	0, %ARGREG3
	b	_nonnumeric
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

! These do the quick thing in the obvious cases (hw-supported and identity)
! and fall into Scheme in the other cases.

_generic_sqrt:
_generic_round:
_generic_truncate:
	jmp	%MILLICODE + M_NOT_SUPPORTED
	nop


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


!-----------------------------------------------------------------------------
! Box various numbers.

! Box the double in %f2/f3 as a flonum. Return tagged pointer in RESULT.

_box_flonum:
	call	_alloc
	mov	16, %RESULT

	std	%f2, [ %RESULT + 8 ]
	set	(16 << 8) | FLONUM_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	jmp	%o7 + 8
	add	%RESULT, BVEC_TAG, %RESULT


! Box the two doubles in %f2/%f3 and %f4/%f5 as a compnum.
! Return tagged pointer in RESULT.

_box_compnum:
	call	_alloc
	mov	24, %RESULT

	std	%f2, [ %RESULT + 8 ]
	std	%f4, [ %RESULT + 16 ]
	set	(24 << 8) | COMPNUM_HDR, %TMP0
	st	%TMP0, [ %RESULT ]
	jmp	%o7+8
	add	%RESULT, BVEC_TAG, %RESULT


! Interesting data for the generic arithmetic system.

	.seg	"data"
errmsg:
	.asciz	"Non-numeric operand(s) to arithmetic operation."

	! end

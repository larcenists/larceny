! Memory management test program
! Sparc assembly language procedures
!
! $Id: memtest2.s,v 1.1 91/06/27 16:35:05 lth Exp Locker: lth $
!
! All public procedures here are called from C.
	
#define ASSEMBLY
#include "registers.s.h"
#include "millicode.h"

	.global	_millicall

	.seg "text"

! Generalized interface to calling millicode from C
!
! word millicall( word (*proc)(), word result, word arg2, word arg3 )

_millicall:
	save	%sp, -96, %sp
	st	%i0, [ %fp+44 ]
	st	%i1, [ %fp+48 ]
	st	%i2, [ %fp+52 ]
	st	%i3, [ %fp+56 ]
	st	%i7, [ %fp+60 ]
	call	_restore_scheme_context
	nop

	mov	0, %REG0
	ld	[ %fp+44 ], %TMP0
	ld	[ %fp+48 ], %RESULT
	ld	[ %fp+52 ], %ARGREG2
	ld	[ %fp+56 ], %ARGREG3
	jmpl	%TMP0, %o7
	nop

	call	_save_scheme_context
	nop

	mov	%RESULT, %i0
	ld	[ %fp+60 ], %i7
	ret
	restore

	! EOF

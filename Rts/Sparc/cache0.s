! -*- fundamental -*-
!
! Larceny RTS -- cache flush test code.
!
! lth@cs.uoregon.edu / August 24, 1995 (Death to Windows 95!)
! $Id: cache0.s,v 1.1.1.1 1998/11/19 21:51:49 lth Exp $

#include "asmdefs.h"
#include "asmmacro.h"

	.global EXTNAME(test_cache)
	
! int test_cache(void)
!
! Returns >0 if the architeture requires a cache flush, and 0 if it does not.
! Can be called repeatedly.
!
! In order to guard against interrupts while the program is running, it
! performs the test three times. A zero result every time virtually
! guarantees that the architecture does not need a flush; nonzero at
! any time means it definitely needs it. If 3 times turns out not to be
! enough to get a good answer, just up the loop count.

	.text
	.align 8
EXTNAME(test_cache):
	save	%sp, -96, %sp
	set	0, %l3			! accumulator of results
	set	3, %l4			! loop counter
1:	set	hack, %l0		! get a
	ld	[ %l0 ], %l1		!   nop instr
	set	test, %l0		! get instr
	ld	[ %l0 ], %l2		!   to overwrite
	st	%l1, [ %l0 ]		! overwrite with nop
	call	test			! test it; result in %o0
	nop
	st	%l2, [ %l0 ]		! restore old instr
	iflush 	%l0			!   and flush it
	nop
	nop
	nop
	nop
	nop
	add	%o0, %l3, %l3
	deccc	1, %l4
	bne	1b
	nop
	mov	%l3, %i0		! setup result
	ret				!   and
	restore				!     return
hack: 	nop				! a nop instr

	.data				! to make instructions writeable
	.align 8
test:	ba	test2			! will be overwritten
	nop
	set	0, %o0			! executed if no split cache
	retl
	nop
test2:	set 	1, %o0			! executed if split cache
	retl
	nop

! eof

/* Rts/Sparc/bdw-memory.s
 * Larceny run-time system -- wrapper for memory.s, with Boehm collector.
 *
 * $Id: bdw-memory.s,v 1.2 1997/05/31 01:47:56 lth Exp $
 */

#define BDW_GC
#include "memory.s"


! bdw_allocate: allocate memory for the Boehm-Demers-Weiser conservative gc.
! bdw_allocate_atomic: allocate non-pointer-containing memory for ditto.
! 
! Input    : RESULT = fixnum: words to allocate
!            o7 = return address (Scheme or millicode)
! Output   : RESULT = Untagged pointer to possibly initialized memory
! Destroys : RESULT, Temporaries, globals[ G_ALLOCI_TMP ]

bdw_allocate:
	save	%sp, -96, %sp
	add	%SAVED_RESULT, 4, %o0		! fixnum words = native bytes
	call	EXTNAME(GC_malloc)		! Allocate
	andn	%o0, 7, %o0			! round up
	cmp	%o0, 0				! error?
	bne	1f				! skip if not
	nop
	set	mallocfail, %o0
	call	EXTNAME(panic)			! that's error handling for you
	nop
1:	mov	%o0, %SAVED_RESULT
	ret
	restore	


bdw_allocate_atomic:
	save	%sp, -96, %sp
	add	%SAVED_RESULT, 4, %o0		! fixnum words = native bytes
	call	EXTNAME(GC_malloc_atomic)	! Allocate
	andn	%o0, 7, %o0
	cmp	%o0, 0				! error?
	bne	1f				! skip if not
	nop
	set	mallocfail, %o0
	call	EXTNAME(panic)			! that's error handling for you
	nop
1:	mov	%o0, %SAVED_RESULT
	ret
	restore	
	
	.data
mallocfail:
	.asciz "GC_malloc returned 0"
	.text

/* eof */

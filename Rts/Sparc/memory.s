/* Copyright 1998 Lars T Hansen.                 -*- fundamental -*-
 *
 * $Id$
 *
 * SPARC memory management primitives.
 *
 * All publicly available procedures are named mem_something.
 * Procedures which use internal calling conventions (i.e., which are
 * called from millicode) are named mem_internal_something.
 */
#define ASSEMBLER 1
#include "../Sys/config.h"

#include "asmdefs.h"
#include "asmmacro.h"

	.global EXTNAME(mem_alloc)			/* allocate raw RAM */
	.global EXTNAME(mem_alloc_bv)			/* allocate raw RAM */
	.global EXTNAME(mem_alloci)			/* allocate cooked RAM */
	.global	EXTNAME(mem_internal_alloc)		/* allocate raw RAM */
	.global	EXTNAME(mem_internal_alloc_bv)		/* allocate raw RAM */
	.global EXTNAME(mem_morecore)			/* get more memory */
	.global EXTNAME(mem_stkoflow)			/* handle stack oflow */
	.global	EXTNAME(mem_internal_stkoflow)		/* handle stack oflow */
	.global EXTNAME(mem_stkuflow)			/* handle stack uflow */
	.global EXTNAME(mem_capture_continuation)	/* creg-get */
	.global EXTNAME(mem_restore_continuation)	/* creg-set! */
	.global EXTNAME(mem_icache_flush)		/* flush some icache */

	.seg "text"

#if defined( BDW_GC )
# error "This file cannot be used with the BDW collector."
#endif


/* _mem_alloc_bv: allocate uninitialized pointer-non-containing memory.
 * _mem_alloc: allocate uninitialized pointer-containing memory.
 *
 * Call from: Scheme
 * Input    : RESULT = fixnum: size of structure in words
 *            o7 = scheme return address
 * Output   : RESULT = untagged ptr to uninitialized memory
 * Destroys : RESULT, Temporaries
 *
 * These procedures could call _mem_internal_alloc, but do their work
 * in-line for performance reasons.
 */
EXTNAME(mem_alloc):
EXTNAME(mem_alloc_bv):
	sub	%E_LIMIT, %E_TOP, %TMP0		/* Compute space */
	and	%RESULT, 0x04, %TMP1		/* Get odd bit */
	cmp	%RESULT, %TMP0			/* Space? */
	bleu,a	Lalloc1				/*   Skip and */
	add	%E_TOP, %RESULT, %E_TOP		/*     allocate if so */

	/* Heap overflow. %RESULT still has the number of words to alloc. */

	st	%o7, [ %GLOBALS + G_RETADDR ]	/* save scheme return address */
	call	heap_overflow
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7	/* restore return address */
	jmp	%o7+8
	nop

Lalloc1:
	sub	%E_TOP, %RESULT, %RESULT	/* result ptr */
	jmp	%o7+8
	add	%E_TOP, %TMP1, %E_TOP		/* Round up. We can round */
						/* without checking for */
						/* overflow because everything */
						/* is 8-byte aligned. */
	
/* _mem_alloci: allocate initialized memory
 *
 * Call from: Scheme
 * Input    : RESULT = fixnum: size of structure, in words
 *            ARGREG2 = object: to initialize with
 *            o7 = Scheme return address
 * Output   : RESULT = untagged ptr to uninitialized structure
 * Destroys : RESULT, Temporaries
 */
EXTNAME(mem_alloci):
	st	%o7, [ %GLOBALS + G_RETADDR ]	/* save Scheme retaddr */
	st	%ARGREG3, [ %GLOBALS + G_ALLOCI_TMP ]

	call	EXTNAME(mem_internal_alloc)	/* allocate memory */
	mov	%RESULT, %ARGREG3		/* save size for later */

	ld	[ %GLOBALS + G_RETADDR ], %o7	/* restore Scheme retaddr */

	/* %RESULT now has ptr, %ARGREG3 has count, %ARGREG2 has obj */

#if 1 /* Old initialization code */

	sub	%RESULT, 8, %TMP1		/* dest = RESULT - 8 */
	b	Lalloci2
	tst	%ARGREG3
Lalloci3:
	st	%ARGREG2, [ %TMP1 ]		/* init a word */
	st	%ARGREG2, [ %TMP1+4 ]		/* and another */
	deccc	8, %ARGREG3			/* n -= 8, test n */
Lalloci2:
	bgt	Lalloci3
	add	%TMP1, 8, %TMP1			/* dest += 8 */

#else	/* New initialization code */

	/* Duff's device, for an unrolling of 4 (generalizes to longer).
	 * This code works, but does not seem to offer any performance
	 * improvements for small vectors (not surprising).  It might
	 * be worthwhile to use for longer vectors, which is why the
	 * code is still here.
	 */
	andcc	%ARGREG3, 15, %TMP1		/* inverse offset */
	set 	16, %TMP0
	sub	%TMP0, %TMP1, %TMP1		/* actual offset (16-x) */
	add	%ARGREG3, %TMP1, %ARGREG3	/* round iterations up */
	set	Lalloci3, %TMP0			/* branch target */
	jmp	%TMP0 + %TMP1			/* jump into loop! */
	sub	%RESULT, %TMP1, %TMP1		/* set up initialization pointer */
Lalloci3:
	st	%ARGREG2, [ %TMP1 ]
	st	%ARGREG2, [ %TMP1+4 ]
	st	%ARGREG2, [ %TMP1+8 ]
	st	%ARGREG2, [ %TMP1+12 ]
Lalloci2:
	subcc	%ARGREG3, 16, %ARGREG3
	bg	Lalloci3
	add	%TMP1, 16, %TMP1

#endif

	jmp	%o7+8
	ld	[ %GLOBALS + G_ALLOCI_TMP ], %ARGREG3


/* _mem_internal_alloc: allocate uninitialized pointer-containing memory.
 * _mem_internal_alloc_bv: allocate uninitialized non-pointer-containing 
 *                         memory.
 *
 * Call from: Millicode
 * Input    : RESULT = fixnum: size of structure in words
 *            o7 = millicode return address
 *            globals[ G_RETADDR ] = scheme return address
 * Output   : RESULT = untagged ptr to uninitialized memory
 * Destroys : RESULT, Temporaries
 */
EXTNAME(mem_internal_alloc):
EXTNAME(mem_internal_alloc_bv):
	sub	%E_LIMIT, %E_TOP, %TMP0		/* Compute space */
	and	%RESULT, 0x04, %TMP1		/* get 'odd' bit */
	cmp	%RESULT, %TMP0			/* Space? */
	bleu,a	Lialloc1			/*   skip and */
	add	%E_TOP, %RESULT, %E_TOP		/*     allocate if so */

	/* Heap overflow. */

	b	heap_overflow			/* returns to caller */
	nop

Lialloc1:
	sub	%E_TOP, %RESULT, %RESULT	/* setup result */
	jmp	%o7+8
	add	%E_TOP, %TMP1, %E_TOP		/* round up; see justification */
						/* in code for _mem_alloc. */

/* heap_overflow: Heap overflow handler for allocation primitives.
 * ETOP must point to first free word in ephemeral area.
 *
 * Call from: allocation millicode procedures only
 * Input    : RESULT = fixnum: size of structure to allocate, in words
 *            globals[ G_RETADDR ] = scheme return address
 *            %o7 = millicode return address
 * Output   : pointer to allocated structure
 * Destroys : Temporaries
 */
heap_overflow:
	set	EXTNAME(C_allocate), %TMP0
	b	internal_callout_to_C
	mov	%RESULT, %TMP1


/* _mem_morecore: obtain more memory. */

EXTNAME(mem_morecore):
	set	EXTNAME(C_morecore), %TMP0
	b	callout_to_C
	nop


/* _mem_stkuflow: stack underflow handler.
 *
 * Call from: Don't, it should be returned to.
 * Input    : Nothing
 * Output   : Nothing
 * Destroys : Temporaries
 *
 * This is designed to be returned through on a stack cache underflow.
 * It _can_ be called from scheme, and is, in the current implementation,
 * due to the compiler bug with spill frames.
 */
EXTNAME(mem_stkuflow):
	/* This is where it starts when called */
	b	Lstkuflow1
	nop
	/* This is where it starts when returned into; must be 8 bytes from */
	/* the label _mem_stkuflow. */
#if 1
	/* The code in the #if ... #endif is a transcription of
	 * the code in the C procedure restore_frame() in stack.c.
	 * By moving it in-line we save two context switches, a very
	 * significant part of the cost since it is incurred on
	 * every underflow. On deeply recursive code (like append-rec)
	 * this fix pays off with a speedup of 15-50%.
	 *
	 * If you change this code, be sure to check the C code as well!
	 * The code is in an #if ... #endif so that it can be turned off
	 * to measure gains or look for bugs.
	 */
	ld	[ %GLOBALS + G_CONT ], %TMP0	/* get heap frame ptr */
	ld	[ %TMP0 - VEC_TAG ], %TMP1	/* get header */
	srl	%TMP1, 10, %TMP1		/* size in words */
	inc	%TMP1				/* need to copy header too */
	/* Round up to even words */
	and	%TMP1, 1, %TMP2
	add	%TMP1, %TMP2, %TMP1
	/* Allocate frame, check for overflow */
	sll	%TMP1, 2, %TMP2			/* must subtract bytes... */
	sub	%STKP, %TMP2, %STKP
	cmp	%STKP, %E_TOP
	bl,a	1f
	add	%STKP, %TMP2, %STKP
	/* Need a temp */
	st	%RESULT, [ %GLOBALS + G_RESULT ]
	/* While more frames to copy...
	 *  TMP1 has loop count (even # of words),
	 *  TMP0 has src (heap frame),
	 *  TMP2 has dest (stack pointer).
	 */
	mov	%STKP, %TMP2
	dec	VEC_TAG, %TMP0
	b	2f
	tst	%TMP1
3:
	inc	4, %TMP0
	st	%RESULT, [ %TMP2 ]
	inc	4, %TMP2
	ld	[ %TMP0 ], %RESULT
	inc	4, %TMP0
	st	%RESULT, [ %TMP2 ]
	inc	4, %TMP2
	deccc	2, %TMP1
2:
	bne,a	3b
	ld	[ %TMP0 ], %RESULT
	/* Restore that temp */
	ld	[ %GLOBALS + G_RESULT ], %RESULT
	/* follow continuation chain */
	ld	[ %STKP + 8 ], %TMP0
	st	%TMP0, [ %GLOBALS + G_CONT ]
	/* convert size field in frame */
	ld	[ %STKP ], %TMP0
	sra	%TMP0, 8, %TMP0
	st	%TMP0, [ %STKP ]
	/* convert return address */
	ld	[ %STKP+4 ], %TMP0		/* return offset */
	call	internal_fixnum2retaddr
	ld	[ %STKP+12 ], %REG0		/* procedure */
#if STACK_UNDERFLOW_COUNTING
	ld	[ %GLOBALS+G_STKUFLOW ], %TMP1
	add	%TMP1, 1, %TMP1
	st	%TMP1, [ %GLOBALS + G_STKUFLOW ]
#endif
	jmp	%TMP0+8
	st	%TMP0, [ %STKP+4 ]		/* to be on the safe side */

	/* If we get to this point, the heap overflowed, so just call */
	/* the C version and let it deal with it. */
1:
#endif
	set	EXTNAME(C_restore_frame), %TMP0
	mov	0, %REG0			/* procedure no longer valid */
	call	internal_callout_to_C
	nop
	ld	[ %STKP+4 ], %o7
	jmp	%o7+8
	nop
	/* This code goes away when the compiler is fixed. */
Lstkuflow1:
	set	EXTNAME(C_restore_frame), %TMP0
	st	%o7, [ %GLOBALS + G_RETADDR ]
	call	internal_callout_to_C
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7
	jmp	%o7+8
	nop

/* _mem_stkoflow: stack overflow handler
 *
 * Call from: Scheme
 * Input    : Nothing
 * Output   : Nothing
 * Destroys : Temporaries
 */
EXTNAME(mem_stkoflow):
	set	EXTNAME(C_stack_overflow), %TMP0
	b	callout_to_C
	nop


/* _mem_internal_stkoflow: millicode-internal stack overflow handler.
 *
 * Call from: Millicode
 * Input    : Nothing
 * Output   : Nothing
 * Destroys : Temporaries
 */
EXTNAME(mem_internal_stkoflow):
	set	EXTNAME(C_stack_overflow), %TMP0
	b	internal_callout_to_C
	nop


/* _mem_capture_continuation: perform a creg-get
 *
 * Call from: Scheme
 * Input    : Nothing
 * Output   : RESULT = obj: continuation object
 * Destroys : Temporaries, RESULT
 */
EXTNAME(mem_capture_continuation):
	set	EXTNAME(C_creg_get), %TMP0
	b	callout_to_C
	nop


/* _mem_restore_continuation: perform a creg-set!
 *
 * Call from: Scheme
 * Input    : RESULT = obj: continuation object
 * Output   : Nothing
 * Destroys : Temporaries
 */
EXTNAME(mem_restore_continuation):
	set	EXTNAME(C_creg_set), %TMP0
	b	callout_to_C
	nop


/* _mem_icache_flush: flush instructions in given range from the icache.
 *
 * Call from: C
 * Prototype: extern void mem_icache_flush( void *start, void *end );
 *
 * Flushes all instructions in the address range from start (inclusive)
 * through end (exclusive) from the instruction cache. It may flush some
 * instructions outside this range.
 *
 * Remarks
 *  In the best of all worlds this should be done as part of the copy loop
 *  to avoid the extra loop overhead. But that would mean writing the copy
 *  loop in assembly, which seems like overkill.
 *
 * Assumptions: 
 *  The iflush instruction is implemented in hardware and executes in
 *  one cycle. By definition, it flushes 8 bytes at a time and does not
 *  trap even if given an address outside the address space of the executing
 *  process.
 *
 *  The caller is required to execute at least 2 instructions before it
 *  executes newly-flushed code.
 *
 *  Based on simulations of the flushing cost on a large heap image,
 *  a loop unrolling factor of 24 has the best performance, flushing
 *  192 bytes per iteration. The simulation should be rerun if the code
 *  generator becomes better at generating tight code (or starts generating
 *  larger code :-)
 *
 * Implementation:
 *  Start and ending addresses are rounded down to nearest 8-byte boundary 
 *  before the loop; an extra iflush at the ending address is always 
 *  performed after the loop to catch the last word.
 *
 *  Another option is to unroll the loop massively (say, 1000 times) and
 *  use Duff's device; this will very nearly always be a win.
 *
 * Cost:
 *  Roughly 3+(u+3)*ceil(n/(u*8))+3 cycles for n bytes of code and an 
 *  unrolling factor of u. The exact cost depends on the cost of branches
 *  in the processor implementation. In addition, the cost of a procedure
 *  call and a return.
 */
EXTNAME(mem_icache_flush):
	andn	%o0, 0x07, %o0		/* round start down to 8-boundary */
	b	1f
	andn	%o1, 0x07, %o1		/* ditto for end */
0:	iflush	%o0+8
	iflush	%o0+16
	iflush	%o0+24
	iflush	%o0+32
	iflush	%o0+40
	iflush  %o0+48
	iflush	%o0+56
	iflush	%o0+64
	iflush	%o0+72
	iflush	%o0+80
	iflush	%o0+88
	iflush	%o0+96
	iflush	%o0+104
	iflush	%o0+112
	iflush	%o0+120
	iflush	%o0+128
	iflush	%o0+136
	iflush	%o0+144
	iflush	%o0+152
	iflush	%o0+160
	iflush	%o0+168
	iflush	%o0+176
	iflush	%o0+184
	add	%o0, 192, %o0
1:	cmp	%o0, %o1
	blt	0b			/* no annull! */
	iflush	%o0+0			/* must be in slot */
	retl
	nop

/* eof */


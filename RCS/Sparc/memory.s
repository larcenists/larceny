! Assembly-language routines for allocation and mutation.
! Sparc version.
! All code goes into this file; definitions are in "memory.s.h"
!
! $Id$
!
! This file defines the following builtins:
!
!   alloc( n )		  allocate n uninitialized words
!   alloci( n, v )	  allocate n words initialized to v
!   setcar( p, v )	  set to v the car field of the pair pointed to by p
!   setcdr( p, v )	  set to v the cdr field of the pair pointed to by p
!   vectorset( p, i, v )  set to v the ith slot of the vector pointed to by p
!   gcstart( n )	  Saves the virtual machine state in a predictable
!			  place, flushes stack, calls collector, syncs the
!			  cache, and allocates the requested number of words,
!			  returning a pointer to those words. If n is the
!			  fixnum -1, then no memory is allocated; rather,
!			  a tenuring collection is invoked. (The default is
!			  to do an ephemeral collection.)
!
! 'gcstart' is made public for use by open-coded 'cons' calls and can also
! be used to implement a user-level procedure which invokes the collector;
! to do an ephemeral collection, simply request to allocate 0 bytes.
!
! Arguments are always passed in registers RESULT, ARGREG2, and ARGREG3, all
! of which are rootable. The result, if any, is returned in register RESULT.
! If no result is required, RESULT remains untouched. ARGREG2 and ARGREG2 are
! never destroyed by the call.
!
! THIS CODE ASSUMES THE AVAILABILITY OF TWO NONROOTABLE TEMPORARY REGISTERS
! CALLED MTMP1 AND MTMP2 (Millicode Temporaries) WHICH NEED NOT BE SAVED
! BEFORE USE. THEY DO NOT SURVIVE A CALL.
!
! %GLOBALS should never be allocated to an %o register! If it is, some of 
! these procedures will not work right.
!
! Assemble with 'as -P' to run the preprocessor before the assembler.

#include "memory.s.h"

! Adjusted offsets into data structures.
! For pairs, the offset adjusts for the tag alone; a pair tag is 1.
! For vectors, the offset adjusts for the tag and for the header word;
! the tag is 3 and the size of the header word is 4.

#define CAR_OFFSET	-1
#define CDR_OFFSET	3
#define VEC_OFFSET	1

! Pointer tags

#define TAG_PAIR	0x01
#define TAG_VEC		0x03

	.globl alloc, alloci, setcar, setcdr, vectorset, gcstart

	.text

!-----------------------------------------------------------------------------
! 'alloc' takes one parameter, a fixnum which is the number of words to
! allocate. It returns an untagged pointer to this many words.
!
! Note that the delayed roundup (i.e. done after test for overflow) makes
! sense because all allocations and limits are in an even number of words.
!
! alloc( n )
! {
!   p = E_TOP;
!   E_TOP += n;					; increment heap
!   if (n & 0x04) E_TOP += 0x04;		; round up if necessary
!   if (E_TOP > E_LIMIT) p = gc( n );		; check for overflow
!   return p;
! }

alloc:
	add	%E_TOP, %RESULT, %E_TOP		! allocate optimistically
	and	%RESULT, 0x04, %MTMP1		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	blt,a	Lalloc1				! skip of no overflow
	sub	%E_TOP, %RESULT, %RESULT	! setup result
	call	gcstart				! deal with overflow
	nop
	retl
	nop
Lalloc1:
	retl
	add	%E_TOP, %MTMP1, %E_TOP		! round up


!-----------------------------------------------------------------------------
! 'alloci' takes two parameters, a fixnum which is the number of words to
! allocate, and the value (a word) with which to initialize the memory.
! It returns an untagged pointer to the requested number of words.
!
! alloci( n, v )
! {
!   p = E_TOP;
!   E_TOP += n;					; increment heap
!   if (n & 0x04) E_TOP += 0x04;		; round up if necessary
!   if (E_TOP > E_LIMIT) p = gc( n );		; check for overflow
!   q = p;
!   while( n > 0 ) {
!     *q = v;
!     q += 4;
!     n -= 4;
!   }
!   return p;
! }

alloci:
	mov	%RESULT, %MTMP1			! count into MTMP1
	mov	%E_TOP, %RESULT			! resulting pointer
	add	%E_TOP, %MTMP1, %E_TOP		! allocate optimistically
	and	%MTMP1, 0x04, %MTMP2		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	ble,a	Lalloci1			! no overflow, start init
	add	%E_TOP, %MTMP2, %E_TOP		! adjust if necessary
	call	gcstart				! deal with overflow
	mov	%MTMP1, %RESULT			! restore count for argument

	! restore byte/word count for use in initialization

	sub	%E_TOP, %RESULT, %MTMP1
	add	%MTMP1, 0x04, %MTMP1

	! Now have pointer to memory in %RESULT, count in %MTMP1

Lalloci1:
	sub	%RESULT, 0x04, %MTMP2		! start off early
	b	Lalloci2
	tst	%MTMP1
Lalloci3:
	st	%ARGREG2, [ %MTMP2+0 ]		! init a word
	subcc	%MTMP1, 0x04, %MTMP1		! n -= 4, test n
Lalloci2:
	bne	Lalloci3
	add	%MTMP2, 0x04, %MTMP2		! q += 4
	retl
	nop


!-----------------------------------------------------------------------------
! 'setcar' takes two parameters: a tagged pointer, which is assumed to point
! to a pair, and a value, and sets the 'car' cell of the pair to the value.
!
! If the pair is in the tenured space, then a transaction must be added
! to the transaction list.
!
! setcar( p, v )
! {
!   if (ptrof( p ) >= T_BASE) addtrans( p );     ; add transaction to list
!   *ptrof( p ) = v;
! }

setcar:
	ld	[ %GLOBALS+T_BASE_OFFSET ], %MTMP1	! fetch tenured base
	xor	%RESULT, TAG_PAIR, %MTMP2	! strip tag
	cmp	%MTMP2, %MTMP1
	blt	Lsetcar1
	nop
	call	addtrans			! add transaction to list
	nop
Lsetcar1:
	st	%ARGREG2, [%ARGREG1+CAR_OFFSET]	! CAR_OFFSET compensates right
	retl
	nop


!-----------------------------------------------------------------------------
! 'setcdr' takes two parameters: a tagged pointer, which is assumed to point
! to a pair, and a value, and sets the 'cdr' cell of the pair to the value.
!
! If the pair is in the tenured space, then a transaction must be added
! to the transaction list.
!
! setcdr( p, v )
! {
!   if (ptrof( p ) >= T_BASE) addtrans( p );     ; add transaction to list
!   *(ptrof( p )+4) = v;
! }

setcdr:
	ld	[%GLOBALS+T_BASE_OFFSET], %MTMP1 ! fetch tenured base
	xor	%RESULT, TAG_PAIR, %MTMP2	! strip tag
	cmp	%MTMP2, %MTMP1
	blt	Lsetcdr1
	nop
	call	addtrans			! add transaction to list
	nop
Lsetcdr1:
	st	%ARGREG2, [%ARGREG1+CDR_OFFSET]	! CDR_OFFSET compensates right
	retl
	nop


!-----------------------------------------------------------------------------
! 'vectorset' takes three parameters: a tagged pointer, which is assumed to
! point to a vector, a fixnum index, which is assumed to be valid for the
! given vector, and a value, and sets the specified slot of the vector to
! the value.
!
! If the vector is in the tenured space, then a transaction must be added to
! the transaction list.
!
! vectorset( p, i, v )
! {
!   if (ptrof( p ) >= T_BASE) addtrans( p );	; add transaction?
!   *(ptrof( p )+i+4) = v;			; compensate for header
! }

vectorset:
	ld	[%GLOBALS+T_BASE_OFFSET], %MTMP1 ! fetch tenured base
	xor	%RESULT, TAG_VEC, %MTMP2	! strip tag
	cmp	%MTMP2, %MTMP1
	blt	Lvectorset1			! not in tenured space
	nop
	call	addtrans
	nop
Lvectorset1:
	add	%RESULT, %ARGREG2, %MTMP1	! pointer which does not
						! compensate for header
						! or tag
	st	%ARGREG3, [%MTMP1+VEC_OFFSET]	! VEC_OFFSET compensates
	retl
	nop


!-----------------------------------------------------------------------------
! 'gcstart' saves the state and invokes the collector. It also takes an
! argument, a fixnum indicating the number of words that was attempted 
! allocated when the heap overflow occured. If the overflow was due to
! an entry list overflow, this word must be 0xFFFFFFFC, and a tenuring
! collection must be performed.
!
! The return value from 'gcstart' is a pointer to the requested amount of
! memory (unless the argument was 0xFFFFFFFC).
!
! 'gcstart' saves the state which is kept in registers and then calls
! the C-language routine '_gcstart2' with the number of words to allocate
! as a parameter. When '_gcstart2' returns, the number of words indicated
! (if not 0xFFFFFFFC) can safely be allocated.

gcstart:
	! Save context

	st	%REG0, [ %GLOBALS+REG0_OFFSET ]
	st	%REG1, [ %GLOBALS+REG1_OFFSET ]
	st	%REG2, [ %GLOBALS+REG2_OFFSET ]
	st	%REG3, [ %GLOBALS+REG3_OFFSET ]
	st	%REG4, [ %GLOBALS+REG4_OFFSET ]
	st	%REG5, [ %GLOBALS+REG5_OFFSET ]
	st	%REG6, [ %GLOBALS+REG6_OFFSET ]
	st	%REG7, [ %GLOBALS+REG7_OFFSET ]
	! temps ?
	st	%ARGREG2, [ %GLOBALS+ARGREG2_OFFSET ]
	st	%ARGREG3, [ %GLOBALS+ARGREG3_OFFSET ]
	st	%RESULT, [ %GLOBALS+RESULT_OFFSET ]
	st	%E_TOP, [ %GLOBALS+E_TOP_OFFSET ]

	! C-language call

	mov	%ARGREG1, %o0
	call	_gcstart2
	nop

	! Restore context

	set	_globals, %GLOBALS			! this is fundamental
	
	ld	[ %GLOBALS+REG0_OFFSET ], %REG0
	ld	[ %GLOBALS+REG1_OFFSET ], %REG1
	ld	[ %GLOBALS+REG2_OFFSET ], %REG2
	ld	[ %GLOBALS+REG3_OFFSET ], %REG3
	ld	[ %GLOBALS+REG4_OFFSET ], %REG4
	ld	[ %GLOBALS+REG5_OFFSET ], %REG5
	ld	[ %GLOBALS+REG6_OFFSET ], %REG6
	ld	[ %GLOBALS+REG7_OFFSET ], %REG7
	! temps?
	ld	[ %GLOBALS+ARGREG2_OFFSET ], %ARGREG2
	ld	[ %GLOBALS+ARGREG3_OFFSET ], %ARGREG3
	ld	[ %GLOBALS+RESULT_OFFSET ], %RESULT
	ld	[ %GLOBALS+E_TOP_OFFSET ], %E_TOP
	ld	[ %GLOBALS+E_LIMIT_OFFSET ], %E_LIMIT

	! Must now allocate memory!

	set	0xFFFFFFFC, %MTMP1
	cmp	%RESULT, %MTMP1
	beq	Lgcstart1
	nop

	! Allocate...

	add	%E_TOP, %RESULT, %E_TOP
	and	%RESULT, 0x04, %MTMP1
	add	%E_TOP, %MTMP1, %E_TOP
	
Lgcstart1:
	retl
	nop


!-----------------------------------------------------------------------------
! 'addtrans' takes one parameter, which must be a tagged pointer, and adds it
! to the transaction list. If the transaction list is full (i.e. there is
! an overflow of tenured space (shudder)) then 'addtrans' will call the C
! procedure '_compact_trans', which compacts the transaction list. If
! '_compact_trans' determines that the compaction did not help sufficiently,
! it returns a 0 value, and 'addtrans' must trigger a tenuring collection.
! After the collection there is no sense in adding the transaction to the
! list.
!
! Otherwise, '_compact_trans' returns a nonzero value, and 'addtrans' can
! go ahead and add the transaction.
!
! Big Q: Do we need to save the context before we call _compact_trans?
!        It probebly depends on what is in the %o registers; presumably
!        we can get away with just saving those.

addtrans:

	! Get tenured-space limits and check for overflow

	ld	[ %GLOBALS+T_ENTRIES_OFFSET ], %MTMP1
	ld	[ %GLOBALS+T_TOP_OFFSET ], %MTMP2
	cmp	%MTMP1, %MTMP2
	bgt	Laddtrans1
	sub	%MTMP1, 4, %MTMP2

	! %GLOBALS had better not be a %o register!

	std	%o0, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET ]
	std	%o2, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+8 ]
	std	%o4, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+16 ]
	std	%o6, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+24 ]

	call	_compact_trans
	nop

	tst	%C_RESULT
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET ], %o0
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+8 ], %o2
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+16 ], %o4
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+24 ], %o6

	bne	Laddtrans1
	sub	%MTMP1, 4, %MTMP2

	! We've lost. Go ahead and collect.

	set	0xFFFFFFFC, %RESULT
	call	gcstart
	nop
	retl
	nop

Laddtrans1:
	st	%RESULT, [ %MTMP1+0 ]
	retl
	st	%MTMP2, [ %GLOBALS+T_ENTRIES_OFFSET ]

	! end of file

! Assembly-language routines for allocation and mutation.
! Sparc version.
! All code goes into this file; definitions are in "memory.s.h"
!
! $Id: memory.s,v 1.2 91/06/17 18:52:17 lth Exp Locker: lth $
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
!   stkuflow()		  Procedure to be called on a stack cache underflow.
!			  It will restore a single continuation frame from
!			  the heap-based continuations, if there are any.
!			  After restoring the frame, it will branch to the
!			  return address in the frame.
!   stkoflow()		  Procedure to be called on a stack cache overflow.
!			  It will flush the stack cache to memory and setup
!			  the continuation pointer, and return to its caller.
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
! THIS CODE USES AND CHANGES %TMP0 AND %TMP1, BOTH OF WHICH MUST BE 
! NONROOTABLE.
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
	.globl stkoflow, stkuflow

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
	and	%RESULT, 0x04, %TMP0		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	blt,a	Lalloc1				! skip of no overflow
	sub	%E_TOP, %RESULT, %RESULT	! setup result
	call	gcstart				! deal with overflow
	nop
	retl
	nop
Lalloc1:
	retl
	add	%E_TOP, %TMP0, %E_TOP		! round up


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
	mov	%RESULT, %TMP0			! count into TMP0
	mov	%E_TOP, %RESULT			! resulting pointer
	add	%E_TOP, %TMP0, %E_TOP		! allocate optimistically
	and	%TMP0, 0x04, %TMP1		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	ble,a	Lalloci1			! no overflow, start init
	add	%E_TOP, %TMP1, %E_TOP		! adjust if necessary
	call	gcstart				! deal with overflow
	mov	%TMP0, %RESULT			! restore count for argument

	! restore byte/word count for use in initialization

	sub	%E_TOP, %RESULT, %TMP0
	add	%TMP0, 0x04, %TMP0

	! Now have pointer to memory in %RESULT, count in %TMP0

Lalloci1:
	sub	%RESULT, 0x04, %TMP1		! start off early
	b	Lalloci2
	tst	%TMP0
Lalloci3:
	st	%ARGREG2, [ %TMP1+0 ]		! init a word
	subcc	%TMP0, 0x04, %TMP0		! n -= 4, test n
Lalloci2:
	bne	Lalloci3
	add	%TMP1, 0x04, %TMP1		! q += 4
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
	ld	[ %GLOBALS+T_BASE_OFFSET ], %TMP0	! fetch tenured base
	xor	%RESULT, TAG_PAIR, %TMP1	! strip tag
	cmp	%TMP1, %TMP0
	blt	Lsetcar1
	nop
	call	addtrans			! add transaction to list
	nop
Lsetcar1:
	retl
	st	%ARGREG2, [%ARGREG1+CAR_OFFSET]	! CAR_OFFSET compensates right


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
	ld	[%GLOBALS+T_BASE_OFFSET], %TMP0 ! fetch tenured base
	xor	%RESULT, TAG_PAIR, %TMP1	! strip tag
	cmp	%TMP1, %TMP0
	blt	Lsetcdr1
	nop
	call	addtrans			! add transaction to list
	nop
Lsetcdr1:
	retl
	st	%ARGREG2, [%ARGREG1+CDR_OFFSET]	! CDR_OFFSET compensates right


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
	ld	[%GLOBALS+T_BASE_OFFSET], %TMP0 ! fetch tenured base
	xor	%RESULT, TAG_VEC, %TMP1	! strip tag
	cmp	%TMP1, %TMP0
	blt	Lvectorset1			! not in tenured space
	nop
	call	addtrans
	nop
Lvectorset1:
	add	%RESULT, %ARGREG2, %TMP0	! pointer which does not
						! compensate for header
						! or tag
	retl
	st	%ARGREG3, [%TMP0+VEC_OFFSET]	! VEC_OFFSET compensates


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

	mov	%RESULT, %o0
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

	set	0xFFFFFFFC, %TMP0
	cmp	%RESULT, %TMP0
	beq	Lgcstart1
	nop

	! Allocate...

	add	%E_TOP, %RESULT, %E_TOP
	and	%RESULT, 0x04, %TMP0
	add	%E_TOP, %TMP0, %E_TOP
	
Lgcstart1:
	retl
	nop


!-----------------------------------------------------------------------------
! 'stkuflow' is designed to be returned through on a stack cache underflow.
! The address of 'stkuflow' should be in a dummy continuation at the bottom
! of the stack (top of the stack cache). On a return which underflows the
! stack cache, 'stkuflow' is entered. It restores a single continuation frame
! and jumps to the return address in the newly restored frame.

stkuflow:
	std	%o0, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET ]
	std	%o2, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+8 ]
	std	%o4, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+16 ]
	std	%o6, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+24 ]

	call	_restore_frame
	st	%SP, [ %GLOBALS+SP_OFFSET ]

	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET ], %o0
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+8 ], %o2
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+16 ], %o4
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+24 ], %o6

	ld	[ %GLOBALS+SP_OFFSET ], %SP
	ld	[ %SP ], %TMP0
	jump	%TMP0+8
	nop


!-----------------------------------------------------------------------------
! 'stkoflow' handles stack overflow. When the mutator detects stack overflow,
! then 'stkoflow' should be called. It flushes the stack cache and invokes
! the garbage collector if necessary, and then returns to its caller.

stkoflow:
	std	%o0, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET ]
	std	%o2, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+8 ]
	std	%o4, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+16 ]
	std	%o6, [ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+24 ]

	call	_flush_stack_cache
	st	%SP, [ %GLOBALS+SP_OFFSET ]

	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET ], %o0
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+8 ], %o2
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+16 ], %o4
	ldd	[ %GLOBALS+SAVED_OUTPUT_REGS_OFFSET+24 ], %o6

	ld	[ %GLOBALS+SP_OFFSET ], %sp
	cmp	%E_TOP, %E_LIMIT
	blt	Lstkoflow1
	nop
	call	gcstart
	nop
Lstkoflow1:
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

	ld	[ %GLOBALS+T_ENTRIES_OFFSET ], %TMP0
	ld	[ %GLOBALS+T_TOP_OFFSET ], %TMP1
	cmp	%TMP0, %TMP1
	bgt	Laddtrans1
	sub	%TMP0, 4, %TMP1

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
	sub	%TMP0, 4, %TMP1

	! We've lost. Go ahead and collect.

	set	0xFFFFFFFC, %RESULT
	call	gcstart
	nop
	retl
	nop

Laddtrans1:
	st	%RESULT, [ %TMP0+0 ]
	retl
	st	%TMP1, [ %GLOBALS+T_ENTRIES_OFFSET ]

	! end of file

! Assembly-language routines for allocation and mutation.
! Sparc version.
! All code goes into this file; definitions are in "memory.s.h"
!
! $Header$
!
! This file defines the following builtins:
!
!   alloc( n )		  allocate n uninitialized words
!   alloci( n, v )	  allocate n words initialized to v
!   setcar( p, v )	  set to v the car field of the pair pointed to by p
!   setcdr( p, v )	  set to v the cdr field of the pair pointed to by p
!   vectorset( p, i, v )  set to v the ith slot of the vector pointed to by p
!
! In addition, the following helper procedures are defined:
!
!   gcstart		  takes one parameter. Saves state in a predictable
!			  place, flushes stack, calls collector, syncs the
!			  cache, and allocates the requested number of words,
!			  returning a pointer to those words.
!   addtrans		  add a transaction to the transaction list, invoking
!			  the collector if necessary.
!
! 'gcstart' is made public for use by open-coded 'cons' calls.
!
! Arguments are always passed in registers ARGREG1, ARGREG2, and ARGREG3, all
! of which are rootable. The result, if any, is returned in register RESULT.
! If no result is required, RESULT remains untouched.
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


	.common	_gcstart2, _compact_trans
	.globl alloc, alloci, setcar, setcdr, vectorset, gcstart

	.text

!------------------------------------------------------------------------------
! 'alloc' takes one parameter, a fixnum which is the number of words to
! allocate. It returns an untagged pointer to this many words.
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
	mov	%E_TOP, %RESULT
	add	%E_TOP, %ARGREG1, %E_TOP	! increment heap ptr
	andcc	%ARGREG1, #0x04, %MTMP1
	add	%E_TOP, %MTMP1, %E_TOP		! round up
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	ble	Lalloc1
	nop
	call	gcstart				! deal with overflow
	mov	#0, %RESULT			! RESULT is rootable
Lalloc1:
	retl
	nop


!------------------------------------------------------------------------------
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
    }
!   return p;
! }

alloci:
	mov	%E_TOP, %RESULT
	add	%E_TOP, %ARGREG1, %E_TOP	! increment heap ptr
	andcc	%ARGREG1, #0x04, %MTMP1
	add	%E_TOP, %MTMP1, %E_TOP		! round up
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	ble	Lalloci2			! no overflow, start init
	movcc	%RESULT, %MTMP1			! %MTMP1 is 'q'
	call	gcstart				! deal with overflow
	mov	#0, %RESULT			! RESULT is rootable
	b	Lalloci2			! goto init
	movcc	%RESULT, %MTMP1
Lalloci3:
	st	%ARGREG2, 0[%MTMP1]		! init a word
	add	%MTMP1, #0x04, %MTMP1		! q += 4 (put in slot?)
	subcc	%ARGREG1, #0x04, %ARGREG1	! n -= 4, test n
Lalloci2:
	bne	Lalloci3
	nop
	retl
	nop


!------------------------------------------------------------------------------
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
	ld	T_BASE_OFFSET[%GLOBALS], %MTMP1	! fetch tenured base
	xor	%ARGREG1, #TAG_PAIR, %MTMP2	! strip tag
	cmp	%MTMP2, %MTMP1
	blt	Lsetcar1
	nop
	call	addtrans			! add transaction to list
	nop
Lsetcar1:
	st	%ARGREG2, CAR_OFFSET[%ARGREG1]	! CAR_OFFSET compensates right
	retl
	nop


!------------------------------------------------------------------------------
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
	ld	T_BASE_OFFSET[%GLOBALS], %MTMP1	! fetch tenured base
	xor	%ARGREG1, #TAG_PAIR, %MTMP2	! strip tag
	cmp	%MTMP2, %MTMP1
	blt	Lsetcdr1
	nop
	call	addtrans			! add transaction to list
	nop
Lsetcdr1:
	st	%ARGREG2, CDR_OFFSET[%ARGREG1]	! CDR_OFFSET compensates right
	retl
	nop


!------------------------------------------------------------------------------
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
	ld	T_BASE_OFFSET[%GLOBALS], %MTMP1	! fetch tenured base
	xor	%ARGREG1, #TAG_VEC, %MTMP2	! strip tag
	cmp	%MTMP2, %MTMP1
	blt	Lvectorset1			! not in tenured space
	nop
	call	addtrans
	nop
Lvectorset1:
	add	%ARGREG1, %ARGREG2, %MTMP1	! pointer which does not
						! compensate for header
						! or tag
	st	%ARGREG3, VEC_OFFSET[%MTMP1]	! VEC_OFFSET compensates
	retl
	nop


!------------------------------------------------------------------------------
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

	st	%REG0, REG0_OFFSET[ %GLOBALS ]
	st	%REG1, REG1_OFFSET[ %GLOBALS ]
	st	%REG2, REG2_OFFSET[ %GLOBALS ]
	st	%REG3, REG3_OFFSET[ %GLOBALS ]
	st	%REG4, REG4_OFFSET[ %GLOBALS ]
	st	%REG5, REG5_OFFSET[ %GLOBALS ]
	st	%REG6, REG6_OFFSET[ %GLOBALS ]
	st	%REG7, REG7_OFFSET[ %GLOBALS ]
	! temps ?
	st	%ARGREG1, ARGREG1_OFFSET[ %GLOBALS ]
	st	%ARGREG2, ARGREG2_OFFSET[ %GLOBALS ]
	st	%ARGREG3, ARGREG3_OFFSET[ %GLOBALS ]
	st	%RESULT, RESULT_OFFSET[ %GLOBALS ]
	st	%E_TOP, E_TOP_OFFSET[ %GLOBALS ]

	! C-language call

	mov	%ARGREG1, %o0
	call	_gcstart2
	nop

	! Restore context

	set	_globals, %GLOBALS			! this is fundamental
	
	ld	REG0_OFFSET[ %GLOBALS ], %REG0
	ld	REG1_OFFSET[ %GLOBALS ], %REG1
	ld	REG2_OFFSET[ %GLOBALS ], %REG2
	ld	REG3_OFFSET[ %GLOBALS ], %REG3
	ld	REG4_OFFSET[ %GLOBALS ], %REG4
	ld	REG5_OFFSET[ %GLOBALS ], %REG5
	ld	REG6_OFFSET[ %GLOBALS ], %REG6
	ld	REG7_OFFSET[ %GLOBALS ], %REG7
	! temps?
	ld	ARGREG1_OFFSET[ %GLOBALS ], %ARGREG1
	ld	ARGREG2_OFFSET[ %GLOBALS ], %ARGREG2
	ld	ARGREG3_OFFSET[ %GLOBALS ], %ARGREG3
	ld	RESULT_OFFSET[ %GLOBALS ], %RESULT
	ld	E_TOP_OFFSET[ %GLOBALS ], %E_TOP
	ld	E_LIMIT_OFFSET[ %GLOBALS ], %E_LIMIT

	! Must now allocate memory!

	set	#0xFFFFFFFC, %MTMP1
	cmp	%ARGREGS1, %MTMP1
	beq	Lgcstart1
	nop

	! Allocate...

	add	%E_TOP, %RESULT
	andcc	%ARGREG1, #0x04, %MTMP1
	add	%E_TOP, %MTMP1, %E_TOP
	
Lgcstart1:
	retl
	nop


!------------------------------------------------------------------------------
! 'addtrans' takes one parameter, which must be a tagged pointer, and adds it
! to the transaction list. If the transaction list is full (i.e. there is
! an overflow of tenured space (shudder)) then 'addtrans' will call the C
! procedure '_compact_trans', which compacts the transaction list. If
! '_compact_trans' determines that the compaction did not help sufficiently,
! it returns a 0 value, and 'addtrans' must trigger a tenuring collection.
! After the collection there is no sense in adding the transaction to the list.
!
! Otherwise, '_compact_trans' returns a nonzero value, and 'addtrans' can
! go ahead and add the transaction.
!
! Big Q: Do we need to save the context before we call _compact_trans?
!        It probebly depends on what is in the %o registers; presumably
!        we can get away with just saving those.

addtrans:

	! Get tenured-space limits and check for overflow

	ld	T_ENTRIES_OFFSET[ %GLOBALS ], %MTMP1
	ld	T_TOP_OFFSET[ %GLOBALS ], %MTMP2
	cmp	%MTMP1, %MTMP2
	bgt	Laddtrans1
	sub	%MTMP1, #4, %MTMP2

	! %GLOBALS had better not be a %o register!

	std	%o0, SAVED_OUTPUT_REGS_OFFSET[ %GLOBALS ]
	std	%o2, SAVED_OUTPUT_REGS_OFFSET+8[ %GLOBALS ]
	std	%o4, SAVED_OUTPUT_REGS_OFFSET+16[ %GLOBALS ]
	std	%o6, SAVED_OUTPUT_REGS_OFFSET+24[ %GLOBALS ]

	call	_compact_trans
	nop

	test	%C_RESULT
	ldd	SAVED_OUTPUT_REGS_OFFSET[ %GLOBALS ], %o0
	ldd	SAVED_OUTPUT_REGS_OFFSET+8[ %GLOBALS ], %o2
	ldd	SAVED_OUTPUT_REGS_OFFSET+16[ %GLOBALS ], %o4
	ldd	SAVED_OUTPUT_REGS_OFFSET+24[ %GLOBALS ], %o6

	bne	Laddtrans1
	sub	%MTMP1, #4, %MTMP2

	! We've lost. Go ahead and collect.

	set	#0xFFFFFFFC, %ARGREGS1
	call	gcstart
	nop
	retl
	nop

Laddtrans1:
	st	%ARGREGS1, 0[ %MTMP1 ]
	retl
	st	%MTMP2, T_ENTRIES_OFFSET[ %GLOBALS ]

	! end of file

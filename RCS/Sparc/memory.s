! -*- Fundamental -*-
! Scheme 313 Run-time System.
!
! Assembly-language millicode routines for memory management.
! Sparc version.
!
! $Id: memory.s,v 1.19 92/02/10 03:38:34 lth Exp Locker: lth $
!
! This file defines the following builtins:
!
!   _internal_alloc( n )
!   _alloc( n )		  Allocate n uninitialized words.
!   _alloci( n, v )	  Allocate n words initialized to v.
!   _setcar( p, v )	  Set to v the car field of the pair pointed to by p.
!   _setcdr( p, v )	  Set to v the cdr field of the pair pointed to by p.
!   _vectorset( p, i, v ) Set to v the ith slot of the vector pointed to by p.
!   _gcstart( n )	  Performs a garbage collection, allocates n words,
!			  and returns a pointer to the allocated words.
!			  'n' is a fixnum.
!			  IT IS LEGAL FOR THE VALUE OF "E_TOP" TO BE INVALID 
!			  UPON ENTRY TO THIS PROCEDURE.
!   _garbage_collect( n ) Initiate a garbage collection. The argument is a
!			  fixnum specifying the type of collection: 0 for
!			  an ephemeral collection, -1 for a tenuring 
!			  collection, -2 for a full collection.
!   _stkuflow()		  Procedure to be called on a stack cache underflow.
!			  It will restore a single continuation frame from
!			  the heap-based continuations, if there are any.
!			  After restoring the frame, it will branch to the
!			  return address in the frame.
!   _stkoflow()		  Procedure to be called on a stack cache overflow.
!			  It will flush the stack cache to memory and setup
!			  the continuation pointer, and return to its caller.
!   _save_scheme_context()  Saves all machine-mapped virtual machine registers
!			  in the "globals" table.
!   _restore_scheme_context()  Restores all machine-mapped virtual machine
!			  registers from the "globals" table.
!   _capture_continuation()  Capture the current continuation and return a
!			  pointer to the continuation structure.
!   _restore_continuation() Reinstate the given continuation, discarding the
!			  current one.
!   _restore_frame()      Restore a frame from the continuation chain.
!
! '_gcstart' is made public for use by open-coded 'cons' calls.
!
! '_save_scheme_context' and '_restore_scheme_context' are useful in
! inter-language calls.
!
! '_capture_continuation' and '_restore_continuation' can be used to implement
! the Scheme procedure 'call-with-current-continuation'.
!
! Arguments are always passed in registers RESULT, ARGREG2, and ARGREG3, all
! of which are rootable. The result, if any, is returned in register RESULT.
! If no result is required, RESULT is set to 0. ARGREG2 and ARGREG2 are
! never destroyed by the call. '_stkoflow' and '_stkuflow' never alter RESULT.
!
! On entry to a millicode procedure, %o7 must contain the return address,
! and %REG0 must contain the pointer to the calling procedure. See the
! file "conventions.txt" for calling convention details.
!
! --
! [This paragraph is confusing.]
!
! The user program may enter '_gcstart' with a value in E_TOP which
! is invalid in the sense that it is greater than or equal to E_LIMIT.
! '_gcstart' will correct this error, if necessary, since user code may
! under no circumstances allocate space above E_LIMIT. However, millicode may
! violate this requirement (that is what the overflow area between E_LIMIT and
! E_MAX is for). It follows that millicode which interfaces with user code
! must check (and possibly adjust) E_TOP before proceeding.
!
! --
!
! Internal calling conventions: on entry to internal procedures (procedures
! which do not have underscores prefixing their names), the external
! (Scheme) return address must be saved in %TMP0, where the internal
! procedure will expect to find it, in case it must be adjusted due to a
! collection. In addition, since procedures like 'gcstart' and 'addtrans'
! mangle %RESULT, it will have to be saved and restored (in the globals
! slot SAVED_RESULT_OFFSET) as appropriate around calls to these subroutines.
!
! One could argue that this calling convention is a gross hack. It is also
! becoming increasingly difficult to program around it. A millicode stack
! for saving millicode return addresses on would be better.
!
! --
!
! Assemble with 'as -P -DASSEMBLY'

#include "registers.s.h"
#include "offsets.s.h"
#include "layouts.s.h"

! Macros

#define fixnum( x )	((x) << 2)

	.global _alloc
	.global	_internal_alloc
	.global _alloci
	.global _setcar
	.global _setcdr
	.global _vectorset
	.global _gcstart
	.global _garbage_collect
	.global _stkoflow
	.global _stkuflow
	.global _save_scheme_context
	.global _restore_scheme_context
	.global _capture_continuation
	.global _restore_continuation
	.global	_m_restore_frame

#ifdef DEBUG
	.global	gcstart, addtrans
#endif

	.seg "text"

!-----------------------------------------------------------------------------
! '_alloc' takes one parameter, a fixnum which is the number of words to
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
!
! Note that the delayed roundup (i.e. done after test for overflow) makes
! sense because all allocations and limits are in an even number of words.

		
_alloc:
	add	%E_TOP, %RESULT, %E_TOP		! allocate optimistically
	and	%RESULT, 0x04, %TMP1		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	blt,a	Lalloc1				! skip of no overflow
	sub	%E_TOP, %RESULT, %RESULT	! setup result

	! Overflow; need to collect.

	mov	%o7, %TMP0
	call	gcstart				! deal with overflow
	nop
	jmp	%TMP0+8				! return to Scheme code
	nop

Lalloc1:
	jmp	%o7+8
	add	%E_TOP, %TMP1, %E_TOP		! round up


! _internal_alloc() is like _alloc(), but %TMP0 is a Scheme return
! address to be saved if a collection is triggered.

_internal_alloc:
	add	%E_TOP, %RESULT, %E_TOP		! allocate optimistically
	and	%RESULT, 0x04, %TMP1		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	blt,a	Lialloc1				! skip of no overflow
	sub	%E_TOP, %RESULT, %RESULT	! setup result

	! Overflow; need to collect.

	st	%o7, [ %GLOBALS + MEM_TMP1_OFFSET ]
	call	gcstart				! deal with overflow
	nop
	ld	[ %GLOBALS + MEM_TMP1_OFFSET ], %o7
	jmp	%o7+8				! return to Scheme code
	nop

Lialloc1:
	jmp	%o7+8
	add	%E_TOP, %TMP1, %E_TOP		! round up

!-----------------------------------------------------------------------------
! '_alloci' takes two parameters, a fixnum which is the number of words to
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

_alloci:
	mov	%RESULT, %TMP0			! count into TMP0
	mov	%E_TOP, %RESULT			! resulting pointer
	add	%E_TOP, %TMP0, %E_TOP		! allocate optimistically
	and	%TMP0, 0x04, %TMP1		! get 'odd' bit
	cmp	%E_TOP, %E_LIMIT		! check for overflow
	ble,a	Lalloci1			! no overflow, start init
	add	%E_TOP, %TMP1, %E_TOP		! adjust if necessary

	! Overflow; need to collect

	mov	%TMP0, %RESULT			! restore count for argument
	mov	%o7, %TMP0
	call	gcstart				! deal with overflow
	nop
	mov	%TMP0, %o7			! restore return address

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
	jmp	%o7+8
	nop


!-----------------------------------------------------------------------------
! '_setcar' takes two parameters: a tagged pointer, which is assumed to point
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

_setcar:
	ld	[ %GLOBALS+T_BASE_OFFSET ], %TMP0	! fetch tenured base
	xor	%RESULT, PAIR_TAG, %TMP1	! strip tag
	cmp	%TMP1, %TMP0
	blt	Lsetcar1
	mov	%o7, %TMP0

	! Must add transaction to list

	st	%RESULT, [ %GLOBALS + SAVED_RESULT_OFFSET ]
	call	addtrans			! add transaction to list
	nop
	ld	[ %GLOBALS + SAVED_RESULT_OFFSET ], %RESULT

Lsetcar1:
	st	%ARGREG2, [%RESULT+A_CAR_OFFSET]
	jmp	%TMP0+8
	mov	0, %RESULT


!-----------------------------------------------------------------------------
! '_setcdr' takes two parameters: a tagged pointer, which is assumed to point
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

_setcdr:
	ld	[%GLOBALS+T_BASE_OFFSET], %TMP0 ! fetch tenured base
	xor	%RESULT, PAIR_TAG, %TMP1	! strip tag
	cmp	%TMP1, %TMP0
	blt	Lsetcdr1
	mov	%o7, %TMP0

	st	%RESULT, [ %GLOBALS + SAVED_RESULT_OFFSET ]
	call	addtrans			! add transaction to list
	nop
	ld	[ %GLOBALS + SAVED_RESULT_OFFSET ], %RESULT

Lsetcdr1:
	st	%ARGREG2, [%RESULT+A_CDR_OFFSET]
	jmp	%TMP0+8
	mov	0, %RESULT


!-----------------------------------------------------------------------------
! '_vectorset' takes three parameters: a tagged pointer, which is assumed to
! point to a structure of vector semblance (meaning vector-like or procedure),
! a fixnum index, which is assumed to be valid for the given structure, and a
! value, and sets the specified slot of the structure to the value.
! The index is the untranslated fixnum indicating the slot in the structure,
! e.g. fixnum( 0 ) for the first slot or fixnum( 4 ) for the second slot.
!
! If the structure is in the tenured space, then a transaction must be added to
! the transaction list.
!
! While we could tweak this ever so slightly for vector-like pointers only,
! the generality is nice (we'd save only 1 cycle in the case where a 
! transaction is not needed).
!
! vectorset( p, i, v )
! {
!   if (ptrof( p ) >= T_BASE) addtrans( p );	; add transaction?
!   *(ptrof( p )+i+4) = v;			; compensate for header
! }

_vectorset:
	ld	[%GLOBALS+T_BASE_OFFSET], %TMP0 ! fetch tenured base
	! strip tag
	sra     %RESULT, 3, %TMP1
	sll	%TMP1, 3, %TMP1
	!
	cmp	%TMP1, %TMP0
	ble,a	Lvectorset1			! not in tenured space
	add	%TMP1, %ARGREG2, %TMP1

	! add  transaction

	mov	%o7, %TMP0
	st	%RESULT, [ %GLOBALS + SAVED_RESULT_OFFSET ]
	call	addtrans
	nop
	ld	[ %GLOBALS + SAVED_RESULT_OFFSET ], %RESULT
	mov	%TMP0, %o7
	sra     %RESULT, 3, %TMP1
	sll	%TMP1, 3, %TMP1
	add	%TMP1, %ARGREG2, %TMP1
Lvectorset1:
	jmp	%o7+8
	st	%ARGREG3, [%TMP1+4]


!-----------------------------------------------------------------------------
! '_gcstart' merely calls 'gcstart', with a bit of protocol. We *must* make
! sure that E_TOP, as passed to 'gcstart', has a sensible value, something it
! may not have, coming from user code.
!
! _gcstart( n )
! {
!   E_TOP = min( E_TOP, E_LIMIT );
!   return gcstart( n );
! }

_gcstart:
	cmp	%E_TOP, %E_LIMIT
	ble	L_gcstart1
	nop
	mov	%E_LIMIT, %E_TOP

L_gcstart1:
	mov	%o7, %TMP0
	call	gcstart
	nop
	jmp	%TMP0+8
	nop


!-----------------------------------------------------------------------------
! '_garbage_collect' initiates a garbage collection of the specified type.
! The type is a fixnum and may be either 0 for an ephemeral collection, 
! -1 for a tenuring collection, or -2 for a full collection. Other values
! are invalid.
!
! _garbage_collect( n )
! {
!   return gcstart( n );
! }

_garbage_collect:
	mov	%o7, %TMP0
	call	gcstart
	nop
	jmp	%TMP0+8
	nop


!-----------------------------------------------------------------------------
! '_stkuflow' is designed to be returned through on a stack cache underflow.
! The address of '_stkuflow' should be in a dummy continuation at the bottom
! of the stack (top of the stack cache). On a return which underflows the
! stack cache, '_stkuflow' is entered. It restores a single continuation frame
! and jumps to the return address in the newly restored frame.
!
! We are assuming that the stack pointer is pointing to the initial
! word of the dummy continuation on entry to this handler; given the 
! calling conventions, this is reasonable.

_stkuflow:
	st	%STKP, [ %GLOBALS+SP_OFFSET ]

	save	%sp, -96, %sp
	call	_restore_frame
	nop
	restore

	ld	[ %GLOBALS+SP_OFFSET ], %STKP
	ld	[ %STKP ], %TMP0
	jmp	%TMP0+8
	nop


!-----------------------------------------------------------------------------
! '_stkoflow' handles stack overflow. When the mutator detects stack overflow,
! then '_stkoflow' should be called. It flushes the stack cache and invokes
! the garbage collector if necessary, and then returns to its caller.

_stkoflow:
	st	%E_TOP, [ %GLOBALS+E_TOP_OFFSET ]
	st	%STKP, [ %GLOBALS+SP_OFFSET ]

	save	%sp, -96, %sp
	call	_flush_stack_cache
	nop
	restore

	ld	[ %GLOBALS+SP_OFFSET ], %STKP
	ld	[ %GLOBALS+E_TOP_OFFSET ], %E_TOP
	cmp	%E_TOP, %E_LIMIT
	blt	Lstkoflow1
	nop

	! Overflow; must collect. The collector restores any necessary
	! frames, though, so after collecting we can simply return to the
	! caller.
	!
	! The saving of %RESULT is necessary since we guarantee that
	! stkoflow will not alter it.

	st	%RESULT, [ %GLOBALS + SAVED_RESULT_OFFSET ]

	mov	%o7, %TMP0
	call	gcstart
	set	fixnum( 0 ), %RESULT

	jmp	%TMP0+8
	ld	[ %GLOBALS + SAVED_RESULT_OFFSET ], %RESULT

	! Heap did not overflow. We will need to restore one frame from the
	! continuation chain in order for the caller not to be confused.
	! However, we restore from the chain only if there is a frame to
	! be restored.

Lstkoflow1:
	ld	[ %GLOBALS + CONTINUATION_OFFSET ], %TMP0
	cmp	%TMP0, FALSE_CONST
	beq	Lstkoflow2
	nop

	! restore frame

	st	%STKP, [ %GLOBALS + SP_OFFSET ]
	save	%sp, -96, %sp
	call	_restore_frame
	nop
	restore
	ld	[ %GLOBALS + SP_OFFSET ], %STKP

Lstkoflow2:
	jmp	%o7+8
	nop


!-----------------------------------------------------------------------------
! '_restore_scheme_context' is a null wrapper which simply calls 
! 'restore_scheme_context' before returning.
!
! Use in inter-language calls.

_restore_scheme_context:
	mov	%o7, %TMP0
	call	restore_scheme_context
	nop
	jmp	%TMP0+8
	nop


!-----------------------------------------------------------------------------
! '_save_scheme_context' is a null wrapper which simply calls 
! 'save_scheme_context' before returning.
!
! Use in inter-language calls.

_save_scheme_context:
	mov	%o7, %TMP0
	call	save_scheme_context
	nop
	jmp	%TMP0+8
	nop


!-----------------------------------------------------------------------------
! '_capture_continuation' flushes the stack, performs a collection if 
! necessary, and returns a pointer to the continuation which was current
! at the time of the call to this procedure.
!
! DO WE NEED TO RESTORE A FRAME AFTER FLUSHING THE STACK?
!
! _capture_continuation()
! {
!   flush_stack_cache();
!   if (globals[ E_TOP_OFFSET ] >= globals[ E_LIMIT_OFFSET ])
!     gcstart( 0 );
!   return globals[ CONTINUATION_OFFSET ];
! }

_capture_continuation:
	st	%STKP, [ %GLOBALS + SP_OFFSET ]
	st	%E_TOP, [ %GLOBALS + E_TOP_OFFSET ]

	save	%sp, -96, %sp
	call	_flush_stack_cache
	nop
	restore

	ld	[ %GLOBALS + E_TOP_OFFSET ], %E_TOP
	ld	[ %GLOBALS + SP_OFFSET ], %STKP

	ld	[ %GLOBALS + CONTINUATION_OFFSET ], %RESULT

	cmp	%E_TOP, %E_LIMIT
	blt,a	Lcapture_cont1
	st	%o7, [ %GLOBALS + SAVED_RETADDR_OFFSET ]

	! heap filled up, so we must collect

	mov	%o7, %TMP0
	st	%RESULT, [ %GLOBALS + SAVED_RESULT_OFFSET ]
	call	gcstart
	mov	fixnum( 0 ), %RESULT

	! return

	jmp	%TMP0+8
	ld	[ %GLOBALS + SAVED_RESULT_OFFSET ], %RESULT

	! pop a frame into the stack cache

Lcapture_cont1:
	save	%sp, -96, %sp
	call	_restore_frame
	nop
	restore

	! return

	ld	[ %GLOBALS + SAVED_RETADDR_OFFSET ], %o7
	jmp	%o7+8
	ld	[ %GLOBALS+SP_OFFSET ], %STKP


!-----------------------------------------------------------------------------
! '_restore_continuation' throws away the current continuation (by bumping
! the stack pointer and resetting the value of globals[ CONTINUATION_OFFSET ])
! and reinstates the continuation which is an argument to this procedure.
!
! _restore_continuation( k )
! {
!   globals[ SP_OFFSET ] = globals[ STK_START_OFFSET ];
!   globals[ CONTINUATION_OFFSET ] = k;
!   if (globals[ CONTINUATION_OFFSET ] != FALSE_CONST)
!     restore_frame();
!   return 0;
! }

_restore_continuation:
	! Why do we have to adjust the stack start to get the stack pointer?
	! Because the stack start is the first free word, whereas the stack
	! pointer must point to the top (used) word of the stack, one word
	! above.
	ld	[ %GLOBALS + STK_START_OFFSET ], %TMP0
	add	%TMP0, 4, %TMP0
	st	%RESULT, [ %GLOBALS + CONTINUATION_OFFSET ]

	! Is there a frame to restore?

	cmp	%RESULT, FALSE_CONST
	beq	Lrestore_cont2
	st	%TMP0, [ %GLOBALS + SP_OFFSET ]

	! restore a frame

	save	%sp, -96, %sp
	call	_restore_frame
	nop
	restore

Lrestore_cont2:
	jmp	%o7+8
	ld	[ %GLOBALS + SP_OFFSET ], %STKP

!-----------------------------------------------------------------------------
! '_m_restore_frame'
!
! Simply restore a frame from the continuation chain and return to the caller.
! If there is no frame, skip it.

_m_restore_frame:
	ld	[ %GLOBALS + CONTINUATION_OFFSET ], %TMP0
	cmp	%TMP0, FALSE_CONST
	bne,a	Lrestore_frame1
	st	%STKP, [ %GLOBALS + SP_OFFSET ]

	jmp	%o7+8
	nop

Lrestore_frame1:
	save	%sp, -96, %sp
	call	_restore_frame
	nop
	restore

	jmp	%o7+8
	ld	[ %GLOBALS + SP_OFFSET ], %STKP

!-----------------------------------------------------------------------------
! 'gcstart'
!
! On entry, %TMP0 has return address pointing to Scheme code, and %REG0 is
! assumed to have a valid procedure pointer. %o7 must have the return address
! to the millicode caller.
!
! 'gcstart' saves the state and invokes the collector. It also takes an
! argument, a fixnum indicating the number of words that was attempted 
! allocated when the heap overflow occured. This word may validly be 0, -1,
! or -2, as well as positive. If it is 0 or positive, then an ephemeral
! collection is performed. If it is -1, then a tenuring collection is 
! performed, and no space is allocated. If it is -2, then a full collection
! is performed, and no space is allocated.
!
! The return value from 'gcstart' is a pointer to the requested amount of
! memory (unless the argument was 0, -1, or -2, in which case the return
! value is 0).
!
! 'gcstart' saves the state which is kept in registers and then calls
! the C-language routine '_gcstart2' with the number of words to allocate
! as a parameter. When '_gcstart2' returns, the number of words indicated
! can safely be allocated.
!
! There's a bit of hair associated with the stack, as it will be flushed
! during a collection, but it must have a coherent (i.e. non-empty) state
! when we return to the caller. [While the logic to deal with this could
! have been put in _gcstart2, it is better to keep it here since it is really
! quite dependent on millicode calling conventions, which are implementation
! dependent.]

gcstart:
	! Setup a continuation

	sub	%STKP, 16, %STKP
	st	%TMP0, [ %STKP+0 ]	! return address
	mov	16, %TMP0
	st	%TMP0, [ %STKP+4 ]	! size
 	st	%REG0, [ %STKP+8 ]	! procedure
	st	%g0, [ %STKP+12 ]	! dummy
	
	mov	%o7, %TMP0
	call	save_scheme_context
	nop
	mov	%TMP0, %o7

	!-----------
	! C-language call

	mov	%RESULT, %g1		! %RESULT not valid after save...
	save	%sp, -96, %sp
	call	_gcstart2		! This *will* flush the stack!
	mov	%g1, %o0
	call	_restore_frame		! Restore our frame
	nop
	restore
	!-----------

	mov	%o7, %TMP0
	call	restore_scheme_context
	nop
	mov	%TMP0, %o7

	! Must now allocate memory! First check for exception cases...

	cmp	%RESULT, fixnum( 0 )
	ble,a	Lgcstart1
	mov	fixnum( 0 ), %RESULT

	! Allocate...

	mov	%E_TOP, %TMP1
	add	%E_TOP, %RESULT, %E_TOP
	and	%RESULT, 0x04, %TMP0
	add	%E_TOP, %TMP0, %E_TOP
	mov	%TMP1, %RESULT

Lgcstart1:

	! Return to caller.
	!
	! We have to deallocate the frame that was created in here, restore
	! the previous frame from the heap (otherwise the caller will be
	! terribly confused!), and then return.
	! However, we can only restore a frame if one exists!

	st	%RESULT, [ %GLOBALS + RESULT_OFFSET ]	! save %RESULT for now
	ld	[ %STKP+0 ], %RESULT			! get return address
	add	%STKP, 16, %STKP			! deallocate our frame

	! Is there another frame?

	ld	[ %GLOBALS + CONTINUATION_OFFSET ], %TMP0
	cmp	%TMP0, FALSE_CONST
	beq	Lgcstart2
	nop

	! Fetch a frame

	st	%STKP, [ %GLOBALS + SP_OFFSET ]
	save	%sp, -96, %sp				! get a frame
	call	_restore_frame
	nop
	restore
	ld	[ %GLOBALS + SP_OFFSET ], %STKP

Lgcstart2:
	mov	%RESULT, %TMP0				! "external" retaddr!
	jmp	%o7+8					! return to millicode
	ld	[ %GLOBALS + RESULT_OFFSET ], %RESULT	! result

	
!-----------------------------------------------------------------------------
! 'addtrans' takes one parameter, which must be a tagged pointer, and adds it
! to the transaction list. If the transaction list is full (i.e. there is
! an overflow of tenured space (shudder)) then 'addtrans' will simply perform
! a tenuring collection.
! 
! A more sophisticated approach would be to attempt to compact the transaction
! list before resorting to collection; this should probably be investigated
! at some point.
!
! On entry, the "external" return address is in %TMP0.

addtrans:

	! Get tenured-space limits and check for overflow

	ld	[ %GLOBALS+T_TRANS_OFFSET ], %TMP1
	ld	[ %GLOBALS+T_TOP_OFFSET ], %TMP2
	cmp	%TMP1, %TMP2
	bgt	Laddtrans1
	sub	%TMP1, 4, %TMP2

	! We've lost. Go ahead and collect; never return to this procedure.
	! (There's no need to add a transaction after a tenuring collection.)

	b	gcstart
	set	fixnum( -2 ), %RESULT

Laddtrans1:
	st	%RESULT, [ %TMP1 ]
	jmp	%o7+8
	st	%TMP2, [ %GLOBALS+T_TRANS_OFFSET ]

	! end of file


!-----------------------------------------------------------------------------
! 'restore_scheme_context' restores the Scheme context from the saved state
! in the "globals" table.
!
! It is intended for use in inter-language calls.

restore_scheme_context:
	set	_globals, %GLOBALS
	set	_millicode, %MILLICODE
	
	ld	[ %GLOBALS+REG0_OFFSET ], %REG0
	ld	[ %GLOBALS+REG1_OFFSET ], %REG1
	ld	[ %GLOBALS+REG2_OFFSET ], %REG2
	ld	[ %GLOBALS+REG3_OFFSET ], %REG3
	ld	[ %GLOBALS+REG4_OFFSET ], %REG4
	ld	[ %GLOBALS+REG5_OFFSET ], %REG5
	ld	[ %GLOBALS+REG6_OFFSET ], %REG6
	ld	[ %GLOBALS+REG7_OFFSET ], %REG7

	ld	[ %GLOBALS+ARGREG2_OFFSET ], %ARGREG2
	ld	[ %GLOBALS+ARGREG3_OFFSET ], %ARGREG3
	ld	[ %GLOBALS+RESULT_OFFSET ], %RESULT
	ld	[ %GLOBALS+E_TOP_OFFSET ], %E_TOP
	ld	[ %GLOBALS+E_LIMIT_OFFSET ], %E_LIMIT
	ld	[ %GLOBALS+TIMER_OFFSET ], %TIMER
	ld	[ %GLOBALS + SAVED_F2_OFFSET ], %f2
	ld	[ %GLOBALS + SAVED_F3_OFFSET ], %f3
	ld	[ %GLOBALS + SAVED_F4_OFFSET ], %f4
	ld	[ %GLOBALS + SAVED_F5_OFFSET ], %f5
	set	dzero, %TMP1
	ldd	[ %TMP1 ], %f0
	jmp	%o7+8
	ld	[ %GLOBALS+SP_OFFSET ], %STKP


!-----------------------------------------------------------------------------
! 'save_scheme_context' saves the Scheme context in the "globals" table.
!
! It is intended for use in inter-language calls.

save_scheme_context:	
	st	%REG0, [ %GLOBALS+REG0_OFFSET ]
	st	%REG1, [ %GLOBALS+REG1_OFFSET ]
	st	%REG2, [ %GLOBALS+REG2_OFFSET ]
	st	%REG3, [ %GLOBALS+REG3_OFFSET ]
	st	%REG4, [ %GLOBALS+REG4_OFFSET ]
	st	%REG5, [ %GLOBALS+REG5_OFFSET ]
	st	%REG6, [ %GLOBALS+REG6_OFFSET ]
	st	%REG7, [ %GLOBALS+REG7_OFFSET ]

	st	%ARGREG2, [ %GLOBALS+ARGREG2_OFFSET ]
	st	%ARGREG3, [ %GLOBALS+ARGREG3_OFFSET ]
	st	%RESULT, [ %GLOBALS+RESULT_OFFSET ]
	st	%E_TOP, [ %GLOBALS+E_TOP_OFFSET ]
	st	%STKP, [ %GLOBALS+SP_OFFSET ]
	st	%f2, [ %GLOBALS + SAVED_F2_OFFSET ]
	st	%f3, [ %GLOBALS + SAVED_F3_OFFSET ]
	st	%f4, [ %GLOBALS + SAVED_F4_OFFSET ]
	st	%f5, [ %GLOBALS + SAVED_F5_OFFSET ]
	jmp	%o7+8
	st	%TIMER, [ %GLOBALS+TIMER_OFFSET ]

!-----------------------------------------------------------------------------
! Some data

	.seg	"data"

	.align	8
dzero:
	.double 0r0.0
_diag1:
	.asciz	"Catch\n"
_diag2:
	.asciz	"Throw\n"

	! end of file

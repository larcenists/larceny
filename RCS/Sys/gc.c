/*
 * Scheme 313 Runtime System.
 * Garbage collector.
 *
 * Full internals documentation is in the files "gc.txt" and 
 * "gcinterface.txt".
 *
 * $Id: gc.c,v 2.7 91/07/12 03:16:19 lth Exp Locker: lth $
 *
 * IMPLEMENTATION
 *   We use "old" C; this has the virtue of letting us use 'lint' on the code
 *   as well as Sun's C compiler (on the Sparc): the Sun compiler produces
 *   better code than the (current) GNU compiler.
 *
 * ASSUMPTIONS
 * - We assume a representation with 32 bits; the code will work for
 *   other word sizes if the #definition of BIT_MASK is changed
 *   correctly. 
 * - The number of bytes in a word is assumed to be 4 in a number of places.
 *   This should probably be fixed. I don't think we assume that a byte has
 *   8 bits.
 * - We *must* have sizeof( unsigned long ) >= sizeof( unsigned long * ).
 * - No 2's complement is assumed; all values are unsigned.
 * - UNIX-style malloc() must be provided.
 *
 * BUGS
 * - Does not detect overflow of tenured space during collection; detects it
 *   after the fact if no segmentation fault occured.
 * - Does not remove transactions with no pointers into the ephemeral space
 *   from the transaction list.
 * - Retains local state (copyspace pointers, statistics variables) which
 *   should be moved entirely into the globals[] table. LOMEM and HIMEM should
 *   go away altogether.
 * - Some of the diagnostic messages should be controlled by some value in the
 *   globals[] table, not by the DEBUG switch, since they can be generally
 *   useful.
 * - In fact, the diagnostic messages should not be printed by colect(),
 *   but by collect()'s caller, depending on the return value from collect()
 *   and the statistics variables.
 * - collect() should return a value indicating the kind of collection that
 *   was performed, for diagnostic purposes.
 * - init_collector() should take an additional argument specifying alignment
 *   of the spaces, and align spaces accordingly. This way, we can force e.g.
 *   page alignment, which can be useful for advising the VM system.
 *
 * POSSIBLE FUTURE ENHANCEMENTS
 * - Detect tenured overflow by passing a limit to forward().
 * - gc_trap() should expand area(s) when called, particularly the tenured
 *   area.
 * - Instead of taking a word and returning the forwarded word, forward()
 *   could take a pointer to the word and do all the assigning itself,
 *   if necessary. This is likely a win.
 * - Detecting transactions with no pointers into the ephemeral area can be
 *   done by returning (or passing a pointer to) a flag which indicates
 *   whether something was indeed forwarded, during traversal of the 
 *   transaction list. Possibly, we should have a separate version of
 *   forward() for this if keeping track of the flag makes a lot of 
 *   difference. (It shouldn't.)
 * - Separate version of forward() for ephemeral space use which does not
 *   worry about the tenured-space overflow?
 * - We could malloc() the tenured areas separately. This gives considerable
 *   flexibility in reallocating areas when they must grow. As long as
 *   the ephemeral areas are never reallocated (they must remain below the
 *   tenured areas!), there are no problems with allocating tenured areas
 *   separately (except VM overflow).
 * - We should have two different kinds of tenuring collections: If there is
 *   at least as much space left in the tenured area as the size of the
 *   ephemeral area at the time the tenuring collection is invoked, then
 *   we should merely tenure all objects in the ephemeral space. If, on the
 *   other hand, there is less space left in the tenured area than the size
 *   of the ephemeral area, then we should perform an actual tenuring
 *   collection and copy all objects into the new tenured space. This is
 *   and *easy* modification, and excluding paging behavior, it should
 *   always be a win.
 */

#ifdef __STDC__
  #include <stdlib.h>                  /* for malloc() */
#else
  extern char *malloc();
#endif
#include "gcinterface.h"
#include "main.h"
#include "gc.h"
#include "macros.h"
#include "offsets.h"

/* this is the usual one */
#define NULL                0

/* Private globals */
static unsigned long ecollections, tcollections;
static unsigned long words_collected, words_allocated;
static word *e_base, *e_max, *e_top, *e_mark;
static word *e_new_base, *e_new_max, *e_new_mark;
static word *t_base, *t_max, *t_top, *t_trans;
static word *t_new_base, *t_new_max;
static word *s_base, *s_max;
static word *stk_base, *stk_max;

#ifdef DEBUG
static unsigned pairs_copied;
static unsigned vectors_copied;
#endif

static word forward();
static unsigned words_used();
static ephemeral_collection(),
       tenuring_collection();

/*
 * Procedure to intialize garbage collector.
 *
 * This procedure should be called once and once only, before any memory 
 * allocation is performed by the Scheme code. This code uses malloc() and 
 * hence peacefully coexists with other code that uses malloc().
 *
 * Arguments:
 * - 's_size' is the requested size of the static area, in bytes.
 * - 't_size' is the requested size of the tenured area, in bytes.
 * - 'e_size' is the requested size of the ephemeral area, in bytes.
 * - 'e_lim' is the requested watermark of the ephemeral area, measured
 *   in bytes from the bottom of the area.
 * - 'stk_size' is the requested size of the stack cache area, in bytes.
 *
 * First, all sizes and marks are adjusted for proper alignment and for
 * compliance with the constraints set forth in "gc.h".
 * Then, space is allocated and partitioned up. 
 * Finally, the global pointers are set up.
 *
 * If the initialization succeeded, a nonzero value is returned. Otherwise,
 * 0 is returned.
 */
init_collector( s_size, t_size, e_size, e_lim, stack_size )
unsigned int s_size, t_size, e_size, e_lim, stack_size;
{
  word *p;
  word lomem, himem;

  /* 
   * The size of a space must be divisible by the size of a doubleword,
   * which is 8. It must be larger than the minimum size specified in "gc.h".
   */
  s_size = max( MIN_S_SIZE, roundup8( s_size ) );
  t_size = max( MIN_T_SIZE, roundup8( t_size ) );
  e_size = max( MIN_E_SIZE, roundup8( e_size ) );
  stack_size = max( MIN_STK_SIZE, roundup8( stack_size ) );

  /*
   * There are no limits on the ephemeral watermark, as a bad limit will
   * simply cause poor memory behavior. Makes no sense to have it bigger
   * than the ephemeral area, though.
   */
  e_lim = min( roundup8( e_lim ), e_size );

  /* 
   * Allocate memory for all the spaces.
   * The extra "+ 7" is for a little leeway in adjusting the alignment on
   * machines that do not have alignment restrictions. 
   * (For example, malloc() under Utek will align on a word boundary.)
   */
  p = (word *) malloc( s_size + t_size*2 + e_size*2 + stack_size + 7 );

  if (p == NULL)
    return 0;

  p = (word *) roundup8( (word) p );    /* adjust to doubleword ptr */

  lomem = (word) p;

  /* The stack cache goes at the bottom of memory. */
  stk_base = p;                                 /* lowest word */
  stk_max = p + stack_size / 4 - 1;               /* highest word */
  p += stack_size / 4;

  /* The epehemral areas are the lowest of the heap spaces */
  e_base = e_top = p;
  e_max = p + e_size / 4 - 1;
  e_mark = e_base + e_lim / 4 - 1;
  p += e_size / 4;

  e_new_base = p;
  e_new_max = p + e_size / 4 - 1;
  e_new_mark = e_new_base + e_lim / 4 - 1;
  p += e_size / 4;

  /* The static area goes in the middle */
  s_base = p;
  s_max = p + s_size / 4 - 1;
  p += s_size / 4;

  /* The tenured areas go on the top */
  t_base = t_top = p;
  t_max = p + t_size / 4 - 1;
  t_trans = t_max;
  p += t_size / 4;
  
  t_new_base = p;
  t_new_max = p + t_size / 4 - 1;
  p += t_size / 4;

  himem = (word) p;

  globals[ E_BASE_OFFSET ] = (word) e_base;
  globals[ E_TOP_OFFSET ] = (word) e_top;
  globals[ E_MARK_OFFSET ] = (word) e_mark;
  globals[ E_MAX_OFFSET ] = (word) e_max;

  globals[ T_BASE_OFFSET ] = (word) t_base;
  globals[ T_TOP_OFFSET ] = (word) t_top;
  globals[ T_MAX_OFFSET ] = (word) t_max;
  globals[ T_TRANS_OFFSET ] = (word) t_trans;

  globals[ S_BASE_OFFSET ] = (word) s_base;
  globals[ S_MAX_OFFSET ] = (word) s_max;

  globals[ STK_BASE_OFFSET ] = (word) stk_base;
  globals[ STK_MAX_OFFSET ] = (word) stk_max;

  globals[ LOMEM_OFFSET ] = lomem;
  globals[ HIMEM_OFFSET ] = himem;

  return 1;
}


/*
 * Garbage collector trap entry point. In later versions, when we trap on
 * a memory overflow, we will attempt to allocate more space.
 */
gc_trap( type )
unsigned int type;
{
  if (type == EPHEMERAL_TRAP)
    panic( "GC: Memory overflow in ephemeral area." );
  else if (type == TENURED_TRAP)
    panic( "GC: Memory overflow in tenured area." );
  else
    panic( "GC: Invalid trap." );
}


/*
 * We invoke different collections based on the effect of the last collection
 * and depending on the parameter given.
 * The internal state overrides the parameter.
 */
int collect( type )
unsigned int type;
{
  static unsigned must_tenure = 0;         /* 1 if we need to do a major gc */
  static unsigned words2 = 0;              /* # words in use after last gc */
  unsigned words1;                         /* # words in use before this gc */
  unsigned words_copied, collection_type;

  e_top = (word *) globals[ E_TOP_OFFSET ];
  t_top = (word *) globals[ T_TOP_OFFSET ];     /* enables heap loading */
  t_trans = (word *) globals[ T_TRANS_OFFSET ];

  words1 = words_used();
  words_allocated += words1 - words2;

#ifdef DEBUG
  pairs_copied = vectors_copied = 0;
#endif

  if (must_tenure || type != EPHEMERAL_COLLECTION) {
    tenuring_collection();
    tcollections++;
    must_tenure = 0;
    words_copied = (word) t_top - (word) t_base;
    collection_type = TENURED_COLLECTION;
  }
  else {
    ephemeral_collection();
    ecollections++;
    must_tenure = e_top > e_mark;
    words_copied = (word) e_top - (word) e_base;
    collection_type = EPHEMERAL_COLLECTION;
  }

  words2 = words_used();
  words_collected += words1 - words2;

  globals[ E_BASE_OFFSET ] = (word) e_base;
  globals[ E_TOP_OFFSET ] = (word) e_top;
  globals[ E_MARK_OFFSET ] = (word) e_mark;
  globals[ E_MAX_OFFSET ] = (word) e_max;

  globals[ T_BASE_OFFSET ] = (word) t_base;
  globals[ T_TOP_OFFSET ] = (word) t_top;
  globals[ T_MAX_OFFSET ] = (word) t_max;
  globals[ T_TRANS_OFFSET ] = (word) t_trans;

  globals[ WCOLLECTED_OFFSET ] = words_collected;
  globals[ WALLOCATED_OFFSET ] = words_allocated;
  globals[ TCOLLECTIONS_OFFSET ] = tcollections;
  globals[ ECOLLECTIONS_OFFSET ] = ecollections;

  return collection_type;
}


/*
 * Calculate how much memory we're using in the tenured and ephemeral areas.
 * Is there any reason why/why not the static area should be included?
 */
static unsigned words_used()
{
  return (e_top - e_base) + (t_top - t_base) + (t_max - t_trans);
}


/*
 * The ephemeral collection copies all reachable objects in the ephemeral 
 * area into the new ephemeral area. An object is reachable if
 *  - it is reachable from the set of root pointers in `roots', or
 *  - it is reachable from the transaction list, or
 *  - it is reacable from a reachable object.
 */
static ephemeral_collection()
{
  word *dest, *tmp, *ptr, *head, *tail;
  unsigned int i, tag, size;

  /* 
   * 'dest' is the pointer into newspace at which location the next copied
   * object will go.
   */
  dest = e_new_base;

  /*
   * Do roots. All roots are in the globals array, between FIRST_ROOT
   * and LAST_ROOT, inclusive.
   */
  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    globals[ i ] = forward( globals[ i ], e_base, e_max, &dest );

  /*
   * Do entry list. The entry list is in the tenured area between t_max
   * and t_trans (including the former but not the latter.)
   */
  for ( tail = head = t_max ; head > t_trans ; head-- ) {
    tag = tagof( *head );
    ptr = ptrof( *head );
    if (tag == VEC_TAG || tag == PROC_TAG) {
      if (get_bit( *ptr ) == 0) {        /* haven't done this structure yet */
	set_bit( *ptr );
	size = sizefield( *ptr ) >> 2;
	for (i = 0, ++ptr ; i < size ; i++, ptr++ )
	  *ptr = forward( *ptr, e_base, e_max, &dest );
	*tail-- = *head;
      }
    }
    else if (tag == PAIR_TAG) {
      /* have done pair if either word is pointer into neswpace! */
      if (pointsto( *ptr, e_new_base, e_new_max )
       || pointsto( *(ptr+1), e_new_base, e_new_max ))
	;
      else {
	*ptr = forward( *ptr, e_base, e_max, &dest );
	*(ptr+1) = forward( *(ptr+1), e_base, e_max, &dest );
	*tail-- = *head;
      }
    }
    else
      panic( "Failed invariant in ephemeral_collection().\n" );
  }
  t_trans = tail;

  /*
   * Clear the header bits in the tenured area.
   */
  for (head = t_max ; head > t_trans ; head-- ) {
    tag = tagof( *head );
    if (tag == VEC_TAG || tag == PROC_TAG)
      reset_bit( *ptrof( *head ) );
  }

  /*
   * Now do all the copied objects in newspace, until all are done.
   * Caching the global variables in locals helps the compiler figure
   * out that they won't change when forward() is called.
   */
  { word *my_e_base = e_base;
    word *my_e_max = e_max;

    ptr = e_new_base;
    while (ptr < dest) {
      if (ishdr( *ptr ) && header( *ptr ) == BV_HDR) {
	size = roundup4( sizefield( *ptr ) );
	ptr = (word *) ((word) ptr + (size + 4));
      }
      else if isptr( *ptr ) {
	*ptr = forward( *ptr, my_e_base, my_e_max, &dest );
	ptr++;
      }
      else
	ptr++;
    }
  }

  /* 
   * Flip the spaces.
   */
  tmp = e_base; e_base = e_new_base; e_new_base = tmp;
  tmp = e_max; e_max = e_new_max; e_new_max = tmp;
  tmp = e_mark; e_mark = e_new_mark; e_new_mark = tmp;
  e_top = dest;
}


/*
 * A tenuring collection copies all reachable objects into the tenured area
 * and leaves the ephemeral area empty.
 */
static tenuring_collection()
{
  word *dest, *tmp;
  unsigned int i;

  /*
   * 'Dest' is a pointer into newspace, at which point to place the object
   * being copied.
   */
  dest = t_new_base;

  /*
   * Do the roots. Scan twice to get both spaces.
   */
  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    globals[ i ] = forward( globals[ i ], t_base, t_max, &dest );

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    globals[ i ] = forward( globals[ i ], e_base, e_max, &dest );

  /*
   * Do the copied objects until there are no more.
   * Caching the globals in locals are beneficial because the compiler
   * can then make assumptions about them not changing.
   */
  { word *my_e_base = e_base;
    word *my_e_max = e_max;
    word *my_t_base = t_base;
    word *my_t_max = t_max;
    word *p =  t_new_base;
    unsigned size;

    while (p < dest) {
      if (ishdr( *p ) && header( *p ) == BV_HDR) {
	size = roundup4( sizefield( *p ) );
	p = (word *) ((word) p + (size + 4));
      }
      else {
	if (isptr( *p )) {
	  if (ptrof( *p ) <= my_e_max)
	    *p = forward( *p, my_e_base, my_e_max, &dest );
	  else
	    *p = forward( *p, my_t_base, my_t_max, &dest );
	}
	p++;
      }
    }
  }

  if (dest > t_new_max)
    panic( "Tenured-area overflow." );

  /*
   * Flip.
   */
  tmp = t_base; t_base = t_new_base; t_new_base = tmp;
  tmp = t_max; t_max = t_new_max; t_new_max = tmp;
  t_top = dest;
  t_trans = t_max;
  e_top = e_base;
}


/*
 * "forward()" takes a word "w", the limits "base" and "max" of oldspace,
 * and a pointer to a pointer into newspace, "dest".
 * "forward()" returns the forwarding value of w, which is:
 *  - w if w is not a pointer
 *  - w if w is a pointer not into oldspace
 *  - a pointer into newspace at which location the object that the old w
 *    pointed to (in oldspace).
 * When an object is copied from oldspace to newspace, a forwarding pointer
 * is left behind in oldspace. In case 3 above, if the object pointed to
 * by w is a pointer not into oldspace, then that pointer is returned. A
 * forwarding pointer is indistinguishable from a pointer which is to a space
 * different from oldspace. 
 */
static word forward( w, base, limit, dest )
word w, *base, *limit, **dest;
{
  word tag, q, forw;
  word *ptr, *ptr2, *newptr;

  if (!isptr( w )) return w;

  ptr = ptrof( w );
  if (ptr < base || ptr > limit) return w;

  q = *ptr;
  if (isptr( q )) {
    ptr2 = ptrof( q );
    if (ptr2 < base || ptr2 > limit)   /* "forwarding" */
      return q;
  }

  /*
   * At this point we know that w is a pointer into oldspace. We must copy
   * the structure into newspace and then pad out the structure if necessary.
   */
  tag = tagof( w );
  newptr = *dest;
  if (tag == PAIR_TAG) {
    *newptr = *ptr++;
    *(newptr+1) = *ptr++;
    *dest += 2;
#ifdef DEBUG
    pairs_copied++;
#endif
  }
  else {    /* vector-like (bytevector, vector, procedure) */
    unsigned size;
    word *p1, *p3;

    /* The size is the number of bytes in the body and then the header. */

    size = roundup4( sizefield( q ) ) + 4;

    /* We can unroll the loop because everything is doubleword-aligned.
     * While this causes an unneeded store occasionally, it may help the 
     * compiler figure out what is going on and generate better code.
     */
    p1 = *dest;
    p3 = (word *) ((word) *dest + size);
    while (p1 < p3) {
      *p1++ = *ptr++;
      *p1++ = *ptr++;
    }

    *dest = p1;

    /* Might need to zero out a padding word */

    if (size % 8 != 0)
      *(p1-1) = (word) 0;

#ifdef DEBUG
    vectors_copied++;
#endif
  }

  forw = (word) tagptr( newptr, tag );
  *ptrof( w ) = forw;                   /* leave forwarding pointer */
  return forw;
}

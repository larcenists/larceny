/*
 * Scheme 313 Runtime System.
 * Garbage collector.
 *
 * $Id: gc.c,v 3.4 91/07/24 11:49:34 lth Exp Locker: lth $
 *
 * THE COLLECTOR
 *   There are two kinds of spaces: the tenured space and the ephemeral space.
 *   All objects are created in the ephemeral space. When the ephemeral area
 *   fills up (see below), live objects are copied into the tenured area.
 *
 *   There are three kinds of collections: ephemeral, tenuring, and full.
 *
 *   Ephemeral collection
 *   --------------------
 *   When the ephemeral area fills up with new objects, all reachable objects
 *   are copied into the new ephemeral space. If, after the copy, the new
 *   ephemeral space is fuller than what is specified by the watermark E_MARK,
 *   then the switch MUST_TENURE is set, and the next collection will be a
 *   tenuring collection.
 *
 *   Tenuring collection
 *   -------------------
 *   A tenuring collection is like an ephemeral collection except that objects
 *   are copied from the ephemeral space into the tenured space (starting at
 *   the current top of the tenured space) rather than into an ephemeral
 *   newspace. If, at the start of a tenuring collection, there is less space
 *   left in the current tenured area than the size of the ephemeral area,
 *   then a full collection is performed instead.
 *
 *   Full collection
 *   ---------------
 *   During a full collection both the tenured area and the ephemeral area are
 *   copied into the tenured newspace. [This requires a considerable amount of
 *   extra memory and should be changed.]
 *
 *   The transaction list
 *   --------------------
 *   The mutator must keep a list of tagged pointers to those objects in the 
 *   tenured area which have pointers into the ephemeral area. This list is
 *   maintained at the high end of the tenured area, and is known as the
 *   transaction list. All entries in the transaction list *must* be tagged
 *   pointers into the tenured area. There may be duplicates.
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
 * - Does not remove transactions with no pointers into the ephemeral space
 *   from the transaction list.
 * - init_collector() should take an additional argument specifying alignment
 *   of the spaces, and align spaces accordingly. This way, we can force e.g.
 *   page alignment, which can be useful for advising the VM system (I think).
 *
 * POSSIBLE FUTURE ENHANCEMENTS
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
 * - A different tenuring strategy: initially, there is only the two e-spaces.
 *   When it becomes time to tenure, simply rename the current e-space and
 *   call it a t-space (some trickery here to keep the space relationship
 *   right?). Do this a few times until we have "enough" t-spaces. Then do
 *   a full collection of the t-spaces on a space-basis. A simple variation
 *   is to do the tenuring collection like before but into a small t-space
 *   which is grown in chunks which can be allocated and maintained 
 *   "separately", as this reduces memory waste quite a bit. However, it
 *   requires that we keep cross-generation entry lists to quite some degree!
 *   This may be a hassle.
 * - It is possible that a certain flexibility with the entries in the
 *   transaction list is desirable, since it could simplify (speed up) the
 *   mutators.
 */

#include "gcinterface.h"
#include "main.h"
#include "gc.h"
#include "layouts.h"
#include "macros.h"
#include "offsets.h"

extern char *malloc();

/* this is the usual one */
#define NULL                0

/* Useful macros compute sizes in WORDS */
#define esize()         (e_max - e_base + 1)
#define free_t_space()  (t_trans - t_top + 1)
#define words_used      ((e_top-e_base) + (t_top-t_base) + (t_max-t_trans))

/* 
 * Private globals used during collections. These are not used for keeping
 * local state between collections; all such local state is kept in the
 * globals[] array.
 */
static word *e_base, *e_max, *e_top;
static word *e_new_base, *e_new_max;
static word *t_base, *t_max, *t_top, *t_trans;
static word *t_new_base, *t_new_max;

static word forward();
static ephemeral_collection(),
       tenuring_collection(),
       full_collection();
static word *fast_collection();

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
 * - 'stk_size' is the requested size of the stack cache area, in bytes.
 *
 * First, all sizes are adjusted for proper alignment and for compliance with
 * the constraints set forth in "gc.h".
 * Then, space is allocated and partitioned up. 
 * Finally, the global pointers are set up.
 *
 * If the initialization succeeded, a nonzero value is returned. Otherwise,
 * 0 is returned.
 */
int init_collector( s_size, t_size, e_size, stack_size )
unsigned int s_size, t_size, e_size, stack_size;
{
  word *p;
  word lomem, himem;
  word *s_base, *s_max;
  word *stk_base, *stk_max;

  /* 
   * The size of a space must be divisible by the size of a doubleword,
   * which is 8. It must be larger than the minimum size specified in "gc.h".
   */
  s_size = max( MIN_S_SIZE, roundup8( s_size ) );
  t_size = max( MIN_T_SIZE, roundup8( t_size ) );
  e_size = max( MIN_E_SIZE, roundup8( e_size ) );
  stack_size = max( MIN_STK_SIZE, roundup8( stack_size ) );

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
  p += e_size / 4;

  e_new_base = p;
  e_new_max = p + e_size / 4 - 1;
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
  globals[ E_MAX_OFFSET ] = (word) e_max;
  globals[ E_NEW_BASE_OFFSET ] = (word) e_new_base;
  globals[ E_NEW_MAX_OFFSET ] = (word) e_new_max;

  globals[ T_BASE_OFFSET ] = (word) t_base;
  globals[ T_TOP_OFFSET ] = (word) t_top;
  globals[ T_MAX_OFFSET ] = (word) t_max;
  globals[ T_TRANS_OFFSET ] = (word) t_trans;
  globals[ T_NEW_BASE_OFFSET ] = (word) t_new_base;
  globals[ T_NEW_MAX_OFFSET ] = (word) t_new_max;

  globals[ S_BASE_OFFSET ] = (word) s_base;
  globals[ S_MAX_OFFSET ] = (word) s_max;

  globals[ STK_BASE_OFFSET ] = (word) stk_base;
  globals[ STK_MAX_OFFSET ] = (word) stk_max;

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
int type;
{
  word must_tenure;
  word words_copied, collection_type;
  word words_collected, words_allocated;

  e_base = (word *) globals[ E_BASE_OFFSET ];
  e_top = (word *) globals[ E_TOP_OFFSET ];
  e_max = (word *) globals[ E_MAX_OFFSET ];
  e_new_base = (word *) globals[ E_NEW_BASE_OFFSET ];
  e_new_max = (word *) globals[ E_NEW_MAX_OFFSET ];

  t_base = (word *) globals[ T_BASE_OFFSET ];
  t_top = (word *) globals[ T_TOP_OFFSET ];
  t_trans = (word *) globals[ T_TRANS_OFFSET ];
  t_max = (word *) globals[ T_MAX_OFFSET ];
  t_new_base = (word *) globals[ T_NEW_BASE_OFFSET ];
  t_new_max = (word *) globals[ T_NEW_MAX_OFFSET ];

  words_collected = words_allocated = 0;

  must_tenure = globals[ MUST_TENURE_OFFSET ] || type == TENURING_COLLECTION;

  if (type == FULL_COLLECTION || 
      must_tenure && free_t_space() < esize()) {
    full_collection();
    globals[ F_COLLECTIONS_OFFSET ]++;
    must_tenure = 0;
    words_copied = t_top - t_base;
    collection_type = FULL_COLLECTION;
  }
  else if (must_tenure) {
    word *old_t_top = t_top;

    tenuring_collection();
    globals[ T_COLLECTIONS_OFFSET ]++;
    must_tenure = 0;
    words_copied = t_top - old_t_top;
    collection_type = TENURING_COLLECTION;
  }
  else if (type == EPHEMERAL_COLLECTION) {
    ephemeral_collection();
    globals[ E_COLLECTIONS_OFFSET ]++;
    must_tenure = e_top > e_base + globals[ E_MARK_OFFSET ];
    words_copied = e_top - e_base;
    collection_type = EPHEMERAL_COLLECTION;
  }
  else {
    panic( "collect(): Invalid collection type." );
  }

  globals[ MUST_TENURE_OFFSET ] = must_tenure;

  globals[ E_BASE_OFFSET ] = (word) e_base;
  globals[ E_TOP_OFFSET ] = (word) e_top;
  globals[ E_MAX_OFFSET ] = (word) e_max;
  globals[ E_NEW_BASE_OFFSET ] = (word) e_new_base;
  globals[ E_NEW_MAX_OFFSET ] = (word) e_new_max;

  globals[ T_BASE_OFFSET ] = (word) t_base;
  globals[ T_TOP_OFFSET ] = (word) t_top;
  globals[ T_MAX_OFFSET ] = (word) t_max;
  globals[ T_TRANS_OFFSET ] = (word) t_trans;
  globals[ T_NEW_BASE_OFFSET ] = (word) t_new_base;
  globals[ T_NEW_MAX_OFFSET ] = (word) t_new_max;

  globals[ WCOPIED_OFFSET ]    += words_copied;

  /* Fixme -- these need to be maintained somehow. */
  globals[ WCOLLECTED_OFFSET ] += words_collected;
  globals[ WALLOCATED_OFFSET ] += words_allocated;

  return collection_type;
}


/*
 * An ephemeral collection copies the reachable ephemeral objects into the
 * new ephemeral space and then flips the spaces.
 */
static ephemeral_collection()
{
  word *tmp;

  e_top = fast_collection( e_new_base, e_new_max );
  tmp = e_base; e_base = e_new_base; e_new_base = tmp;
  tmp = e_max; e_max = e_new_max; e_new_max = tmp;
}


/*
 * A tenuring collection copies the reachable ephemeral objects to the end
 * of the tenured space, and then readjusts the e-space pointer.
 */
static tenuring_collection()
{
  t_top = fast_collection( t_top, t_trans );
  e_top = e_base;
  t_trans = t_max;
}


/*
 * The fast collection copies all reachable objects in the ephemeral 
 * area into the area given by the parameter 'base', which is either 
 * the new ephemeral area or the end of the tenured area.
 *
 * An object is reachable if
 *  - it is reachable from the set of root pointers in `roots', or
 *  - it is reachable from the transaction list, or
 *  - it is reacable from a reachable object.
 *
 * It returns the pointer to the first free word of the new area after the
 * collection.
 */
static word *fast_collection( base, top )
word *base;
word *top;
{
  word *dest, *ptr, *head, *tail;
  unsigned int i, tag, size;

  dest = base;

  /*
   * Do roots. All roots are in the globals array, between FIRST_ROOT
   * and LAST_ROOT, inclusive.
   */
  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    globals[ i ] = forward( globals[ i ], e_base, e_max, &dest, top );

#ifdef DEBUG
  if (t_max != t_trans) {
    printf( "Doing transaction list: max = %lx, trans = %lx\n", t_max, t_trans );
  }
#endif

  /*
   * Do entry list. The entry list is in the tenured area between t_max
   * and t_trans (including the former but not the latter.)
   */
  { word *my_e_base = e_base;
    word *my_e_max = e_max;

    for ( tail = head = t_max ; head > t_trans ; head-- ) {
      tag = tagof( *head );
      ptr = ptrof( *head );
      if (tag == VEC_TAG || tag == PROC_TAG) {
	if (get_bit( *ptr ) == 0) {
	  /* haven't done this structure yet */
	  size = sizefield( *ptr ) / 4;
	  set_bit( *ptr );
	  for (i = 0, ++ptr ; i < size ; i++, ptr++ )
	    *ptr = forward( *ptr, my_e_base, my_e_max, &dest, top );
	  *tail-- = *head;
	}
      }
      else if (tag == PAIR_TAG) {
	/* have done pair if either word is pointer into neswpace! */
	if (pointsto( *ptr, base, top ) || pointsto( *(ptr+1), base, top )) {
	  /* nothing */
	}
	else {
	  *ptr = forward( *ptr, my_e_base, my_e_max, &dest, top );
	  *(ptr+1) = forward( *(ptr+1), my_e_base, my_e_max, &dest, top );
	  *tail-- = *head;
	}
      }
      else {
	/* it's possible that we should just ignore the entry here,
	   rather than panicking. */
	panic( "Failed invariant in fast_collection().\n" );
      }
    }
    t_trans = tail;
  }

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

    ptr = base;
    while (ptr < dest) {
      if (ishdr( *ptr ) && header( *ptr ) == BV_HDR) {
	size = roundup4( sizefield( *ptr ) );
	ptr = (word *) ((word) ptr + (size + 4));
      }
      else if isptr( *ptr ) {
	*ptr = forward( *ptr, my_e_base, my_e_max, &dest, top );
	ptr++;
      }
      else
	ptr++;
    }
  }

  return dest;
}


/*
 * A full collection copies all reachable objects into the new tenured area
 * and leaves the ephemeral area empty.
 */
static full_collection()
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
    globals[ i ] = forward( globals[ i ], t_base, t_max, &dest, t_new_max );

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    globals[ i ] = forward( globals[ i ], e_base, e_max, &dest, t_new_max );

  /*
   * Do the copied objects until there are no more.
   * Caching the globals in locals are beneficial because the compiler
   * can then make assumptions about them not changing.
   */
  { word *my_e_base = e_base;
    word *my_e_max = e_max;
    word *my_t_base = t_base;
    word *my_t_max = t_max;
    word *my_t_new_max = t_new_max;
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
	    *p = forward( *p, my_e_base, my_e_max, &dest, my_t_new_max );
	  else
	    *p = forward( *p, my_t_base, my_t_max, &dest, my_t_new_max );
	}
	p++;
      }
    }
  }

  /* Flip tenured spaces; adjust e-space pointer. */

  tmp = t_base; t_base = t_new_base; t_new_base = tmp;
  tmp = t_max; t_max = t_new_max; t_new_max = tmp;
  t_top = dest;
  t_trans = t_max;
  e_top = e_base;
}


/*
 * "forward()" takes a word "w", the limits "base" and "limit" of oldspace,
 * and a pointer to a pointer into newspace, "dest", and the limit of
 * newspace, "oflo".
 *
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
static word forward( w, base, limit, dest, oflo )
word w, *base, *limit, **dest, *oflo;
{
  word tag, q, forw;
  word *ptr, *ptr2, *newptr;
  unsigned size;

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
   *
   * Old version had special case code for pairs to speed up that (common)
   * case; may want to go back to that. Should measure.
   */
  tag = tagof( w );
  newptr = *dest;
  if (tag == PAIR_TAG)
    size = 8;
  else
    size = roundup4( sizefield( q ) ) + 4;


  /* Do the copy; check for overflow. */

  {
    word *p1, *p3;

    /* We can unroll the loop because everything is doubleword-aligned.
     * While this causes an unneeded store occasionally, it may help the 
     * compiler figure out what is going on and generate better code.
     */
    p1 = *dest;
    p3 = (word *) ((word) *dest + size);

    if (p3 > oflo)
      gc_trap( TENURED_TRAP );

    while (p1 < p3) {
      *p1++ = *ptr++;
      *p1++ = *ptr++;
    }

    *dest = p1;

    /* Might need to zero out a padding word */

    if (size % 8 != 0)
      *(p1-1) = (word) 0;
  }

  forw = (word) tagptr( newptr, tag );
  *ptrof( w ) = forw;                   /* leave forwarding pointer */
  return forw;
}

/*
 * Ephemeral garbage collector (for Scheme).
 *
 * $Header$
 *
 * IMPLEMENTATION
 *  We use "old" C; this has the virtue of letting us use 'lint'
 *  on the code.
 *
 * ASSUMPTIONS
 *  - We assume a representation with at least 32 bits; I think this
 *    code will work if words are bigger than that, since all bit
 *    tests are explicit with respect to bit position. However, to
 *    use excess bits to store data, you must change the #definition
 *    of BIT_MASK below. It might also work on shorter words, but I
 *    haven't really bothered to check carefully.
 *  - We *must* have sizeof( unsigned long ) >= sizeof( unsigned long * ).
 *  - I don't think 2's complement arithmetic is assumed anywhere;
 *    I've tried to keep all values unsigned.
 *  - UNIX-style malloc() and memcpy() must be provided.
 *
 * BUGS
 *  - Does not detect overflow of tenured space during collection.
 */

#ifdef __STDC__
  #include <stdlib.h>                  /* for malloc() */
  #include <memory.h>                  /* for memcpy() */
#else
  extern char *malloc();
  extern void memcpy();
#endif
#include "gc.h"

/* this is the usual one */
#define NULL                0

/* Type tags. Not all of these are used by the collector. */
#define FIX1_TAG            0x0
#define FIX2_TAG            0x4
#define IMM1_TAG            0x2
#define IMM2_TAG            0x6
#define PAIR_TAG            0x1
#define VEC_TAG             0x3
#define BVEC_TAG            0x5
#define PROC_TAG            0x7

/* Header tags. Not all of these are used by the collector. */
#define RES_HDR             0x82
#define VEC_HDR             0xA2
#define BV_HDR              0xC2
#define PROC_HDR            0xFE

/* Various masks. Change BIT_MASK if your word is bigger than 32 bits. */
#define TAG_MASK            0x00000007     /* extract bits 2, 1, and 0 */
#define ISHDR_MASK          0x00000083     /* extract bits 7, 1, and 0 */
#define HDR_SIGN            0x00000082     /* header signature */
#define HDR_MASK            0x000000E3     /* Mask to extract header info */
#define BIT_MASK            0x80000000     /* Mask for 'traced' bit */

/* Given tagged pointer, return tag */
#define tagof( w )          ((word)(w) & TAG_MASK)

/* Given tagged pointer, return pointer */
#define ptrof( w )          (word *) ((word)(w) & ~TAG_MASK)

/* Given pointer and tag, return tagged pointer */
#define tagptr( w, tag )    (word *)((word)(w) | (tag))

/* Manipulating 'traced' bit in vector headers */
#define get_bit( w )        ((w) & BIT_MASK)
#define set_bit( w )        ((w) |= BIT_MASK)
#define reset_bit( w )      ((w) &= ~BIT_MASK)

/* extract header tag from a header word */
#define header( w )         ((word) (w) & HDR_MASK)

/* a word is a pointer if the low bit is set */
#define isptr( w )          ((word) (w) & 0x01)

/* a word is a header if it has a header mask layout */
#define ishdr( w )          (((word) (w) & ISHDR_MASK) == HDR_SIGN)

/* extract size field from a header word, accounting for a set hi bit */
#define sizefield( w )      ((((word) (w)) & ~BIT_MASK) >> 8)

/* miscellaneous */
#define max( a, b )         ((a) > (b) ? (a) : (b))
#define min( a, b )         ((a) < (b) ? (a) : (b))
#define roundup4( a )       (((a) + 3) & ~0x03)
#define roundup8( a )       (((a) + 7) & ~0x07)

/* convenient types */
typedef unsigned long word;                 /* 32-bit quantity */

/* variables to be defined by the invoker */
extern word *roots;                         /* pointer to first root */
extern unsigned int rootcnt;                /* number of roots */

/* The current-space variables */
word *e_base, *e_limit, *e_top, *e_mark;
word *t_base, *t_limit, *t_top, *t_entries;
word *s_base, *s_limit, *s_top;
word *stack_base, *stack_limit, *stack_mark;

/* Statistics variables */
unsigned collections;
unsigned words_collected;
unsigned words_allocated;

/* The new-space variables */
static word *t_new_base, *t_new_limit;
static word *e_new_base, *e_new_limit, *e_new_mark;

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
 * 's_size' is the requested size of the static area, in bytes.
 * 't_size' is the requested size of the tenured area, in bytes.
 * 'e_size' is the requested size of the ephemeral area, in bytes.
 * 'stk_size' is the requested size of the stack cache area, in bytes.
 * 'e_lim' is the requested watermark of the ephemeral are, in bytes.
 * 'stk_lim' is the requested watermark of the stack cache, in bytes.
 *
 * First, all sizes and marks are adjusted for proper alignment and for
 * compliance with the constraints set forth in "gc.h".
 * Then, space is allocated and partitioned up. 
 * Finally, the global pointers are set up.
 *
 * If the initialization succeeded, a nonzero value is returned. Otherwise,
 * 0 is returned.
 */
init_collector( s_size, t_size, e_size, e_lim, stk_size, stk_lim )
unsigned int s_size, t_size, e_size, e_lim, stk_size, stk_lim;
{
  word *p;

  /* 
   * The size of a space must be divisible by the size of a doubleword,
   * which is 8. It must be larger than the minimum size specified in "gc.h".
   */
  s_size = max( MIN_S_SIZE, roundup8( s_size ) );
  t_size = max( MIN_T_SIZE, roundup8( t_size ) );
  e_size = max( MIN_E_SIZE, roundup8( e_size ) );
  stk_size = max( MIN_STK_SIZE, roundup8( stk_size ) );

  /*
   * We stipulate that the ephemeral watermark must be at least 1/16 of
   * the size of the space from the bottom of the ephemeral space,
   * and that it may be no higher than the space needed for the 
   * overflow area.
   *
   * The epehemeral area must have room for a flush of a completely full
   * stack cache. The number of words that need to be set aside for this
   * is computed by oflosize().
   */
  { unsigned int e_words = e_size / 4;
    unsigned int e_limit = roundup8( e_lim / 4 );
    unsigned int e_max = e_words - oflosize( stk_size );
    
    e_lim = max( e_words / 16, min( e_limit, e_max ) );
  }

  /*
   * The stack watermark must leave room for at least one full continuation.
   * The reason for this is that we can then check for stack overflow at the
   * start of a continuation-creating procedure and not worry about it after
   * that.
   * We give a lower limit here, 1/32 of the stack size.
   */
  { unsigned int stk_words = stk_size / 4;
    unsigned int stk_limit = roundup8( stk_lim / 4 );
    unsigned int stk_min = stk_words / 32;

    stk_lim = max( stk_min, min( stk_words - MAX_CONT_SIZE, stk_limit ));
  }

  /* 
   * Allocate memory for all the spaces.
   * The extra "+ 7" is for a little leeway in adjusting the alignment on
   * machines that do not have alignment restrictions. 
   * (For example, malloc() under Utek will align on a word boundary.)
   */
  p = (word *) malloc( s_size + t_size*2 + e_size*2 + stk_size + 7 );

  if (p == NULL)
    return 0;

  p = (word *) roundup8( (word) p );    /* adjust to doubleword ptr */

  /* The stack cache goes at the bottom of memory. */
  stack_base = p;                                 /* lowest word */
  stack_limit = p + stk_size / 4 - 1;             /* highest word */
  stack_mark = stack_base + stk_lim - 1;
  p += stk_size / 4;

  /* The epehemral areas are the lowest of the heap spaces */
  e_base = e_top = p;
  e_limit = p + e_size / 4 - 1;
  e_mark = e_base + e_lim - 1;
  p += e_size / 4;

  e_new_base = p;
  e_new_limit = p + e_size / 4 - 1;
  e_new_mark = e_new_base + e_lim - 1;
  p += e_size / 4;

  /* The static area goes in the middle */
  s_base = s_top = p;
  s_limit = p + s_size / 4 - 1;
  p += s_size / 4;

  /* The tenured areas go on the top */
  t_base = t_top = p;
  t_limit = p + t_size / 4 - 1;
  t_entries = t_limit;
  p += t_size / 4;
  
  t_new_base = p;
  t_new_limit = p + t_size / 4 - 1;

  return 1;
}


/*
 * Garbage collector trap entry point. In later versions, when we trap on
 * a memory overflow, we will attempt to allocate more space.
 */
gc_trap( type )
unsigned int type;
{
  if (type == 0)
    panic( "GC: Memory overflow in ephemeral area." );
  else if (type == 1)
    panic( "GC: Memory overflow in tenured area." );
  else
    panic( "GC: Invalid trap." );
}


/*
 * We invoke different collections based on the effect of the last collection
 * and depending on the parameter given: 0 = ephemeral, 1 = tenuring.
 * The internal state overrides the parameter.
 */
collect( type )
unsigned int type;
{
  static unsigned int must_tenure = 0;     /* 1 if we need to do a major gc */
  static unsigned words2;                  /* # words in use after last gc */
  unsigned words1;                         /* # words in use before this gc */

  collections++;
  words1 = words_used();
  words_allocated += words2 - words1;

  if (must_tenure || type == 1) {
    tenuring_collection();
    must_tenure = 0;
  }
  else {
    ephemeral_collection();
    must_tenure = e_top > e_mark;
  }

  words2 = words_used();
  words_collected += words1 - words2;
}


/*
 * Calculate how much memory we're using in the tenured and ephemeral areas.
 * Is there anyreason why/why not the static area should be included? Stack?
 */
static unsigned words_used()
{
  return (e_top - e_base) + (t_top - t_base) + (t_limit - t_entries);
}


/*
 * The ephemeral collection copies all reachable objects in the ephemeral 
 * area into the new ephemeral area. An object is reachable if
 *  - it is reachable from the set of root pointers in `roots', or
 *  - it is reachable from the entry list, or
 *  - it is reacable from a reachable object.
 */
static ephemeral_collection()
{
  word *p, *dest, *tmp, *ptr, *head, *tail;
  unsigned int i, tag, size;

  /* 
   * 'dest' is the pointer into newspace at which location the next copied
   * object will go.
   */
  dest = e_new_base;

  /*
   * Do roots. All roots are in an array somewhere, and the first root is
   * pointed to by 'roots'. 'rootcnt' is the number of roots. They must all
   * be consecutive.
   */
  for (i = rootcnt, p = roots ; i ; i--, p++ )
    *p = forward( *p, e_base, e_limit, &dest );

  /*
   * Do entry list. The entry list is in the tenured area between t_limit
   * and t_entries (including the former but not the latter.)
   */
  for ( tail = head = t_limit ; head > t_entries ; head-- ) {
    tag = tagof( *head );
    ptr = ptrof( *head );
    if (tag == VEC_TAG || tag == PROC_TAG) {
      if (get_bit( *ptr ) == 0) {         /* haven't done this structure yet */
	set_bit( *ptr );
	size = sizefield( *ptr ) >> 2;
	for (i = 0, ++ptr ; i < size ; i++, ptr++ )
	  *ptr = forward( *ptr, e_base, e_limit, &dest );
	*tail-- = *head;
      }
    }
    else if (tag == PAIR_TAG) {
      /* have done pair if either word is pointer into neswpace! */
      if (isptr( *ptr ) && 
	  ptrof( *ptr ) >= e_new_base && ptrof( *ptr ) <= e_new_limit
	  || 
	  isptr( *(ptr+1) && 
	  ptrof( *(ptr+1) ) >= e_new_base && ptrof( *(ptr+1) ) <= e_new_limit))
	;
      else {
	*ptr = forward( *ptr, e_base, e_limit, &dest );
	*(ptr+1) = forward( *(ptr+1), e_base, e_limit, &dest );
	*tail-- = *head;
      }
    }
    else
      panic( "Failed invariant in ephemeral_collection().\n" );
  }
  t_entries = tail;

  /*
   * Clear the header bits in the tenured area.
   */
  for (head = t_limit ; head > t_entries ; head-- ) {
    tag = tagof( *head );
    if (tag == VEC_TAG || tag == PROC_TAG)
      reset_bit( *ptrof( *head ) );
  }

  /*
   * Now do all the copied objects in newspace, until all are done.
   */
  ptr = e_new_base;
  while (ptr < dest) {
    if (ishdr( *ptr ) && header( *ptr ) == BV_HDR) {
      size = (sizefield( *ptr ) + 3) >> 2;
      ptr += size + 1;
    }
    else {
      *ptr = forward( *ptr, e_base, e_limit, &dest );
      ptr++;
    }
  }

  /* 
   * Flip the spaces.
   */
  tmp = e_base; e_base = e_new_base; e_new_base = tmp;
  tmp = e_limit; e_limit = e_new_limit; e_new_limit = tmp;
  tmp = e_mark; e_mark = e_new_mark; e_new_mark = tmp;
  e_top = dest;
}


/*
 * A tenuring collection copies all reachable objects into the tenured area
 * and leaves the ephemeral area empty.
 */
static tenuring_collection()
{
  word *p, *dest, *tmp;
  unsigned int i, size;

  /*
   * 'Dest' is a pointer into newspace, at which point to place the object
   * being copied.
   */
  dest = t_new_base;

  /*
   * Do the roots. Scan twice to get both spaces.
   */
  for (p = roots, i = rootcnt ; i ; i--, p++ )
    *p = forward( *p, t_base, t_limit, &dest );

  for (p = roots, i = rootcnt ; i ; i--, p++ )
    *p = forward( *p, e_base, e_limit, &dest );


  /*
   * Do the copied objects until there are no more.
   * Does this adequately deal with static space?
   */
  p = t_new_base;
  while (p < dest) {
    if (ishdr( *p ) && header( *p ) == BV_HDR) {
      size = (sizefield( *p ) + 3) >> 2;
      p += size + 1;
    }
    else {
      if (isptr( *p )) {
	if (ptrof( *p ) <= e_limit)
	  *p = forward( *p, e_base, e_limit, &dest );
	else
	  *p = forward( *p, t_base, t_limit, &dest );
      }
      p++;
    }
  }

  /*
   * Flip.
   */
  tmp = t_base; t_base = t_new_base; t_new_base = tmp;
  tmp = t_limit; t_limit = t_new_limit; t_new_limit = tmp;
  t_top = dest;
  t_entries = t_limit;
  e_top = e_base;
}


/*
 * "Forward" takes a word "w", the limits "base" and "limit" of oldspace,
 * and a pointer to a pointer into newspace, "dest". It returns the forwarding
 * value of w, which is:
 *  - w if w is a literal (header or fixnum)
 *  - w if w is a pointer not into oldspace
 *  - a pointer into newspace at which location the object that the old w
 *    pointed to (in oldspace).
 * When an object is copied from oldspace to newspace, a forwarding pointer
 * is left behind in oldspace. In case 3 above, if the object pointed to
 * by w is a pointer not into oldspace, then that pointer is returned. A
 * forwarding pointer is indistinguishable from a pointer which is to a space
 * different from oldspace. 
 *
 * [Implementation note: can we do better than memcpy() here because we know
 *  that the structure is aligned?]
 */
static word forward( w, base, limit, dest )
word w, *base, *limit, **dest;
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
   */
  tag = tagof( w );
  newptr = *dest;
  if (tag == PAIR_TAG) {
    *((*dest)++) = *ptr++;
    *((*dest)++) = *ptr++;
  }
  else {                      /* vector-like (bytevector, vector, procedure) */
    size = roundup4( sizefield( q ) + 4 );
    memcpy( (char *) *dest, (char *) ptr, size );
    *dest += size / 4;
    if (size % 8 != 0)         /* need to pad out to doubleword? */
      *((*dest)++) = (word) 0;
  }

  forw = (word) tagptr( newptr, tag );
  *ptrof( w ) = forw;                   /* leave forwarding pointer */
  return forw;
}

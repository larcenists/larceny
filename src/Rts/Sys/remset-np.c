/* Copyright 1998 Lars T Hansen.
 *
 * $Id: remset.c 5049 2007-11-01 21:16:07Z pnkfelix $
 *
 * Remembered set extensions for non-predicative collector.
 *
 * Go see the notes at top of remset.c
 * 
 * (This file is the result of factoring out a number of definitions
 *  that are specific to the DOF and ROF collectors.)
 *
 */

#define GC_INTERNAL

#include <stdlib.h>

#include "larceny.h"
#include "macros.h"
#include "memmgr.h"
#include "remset_t.h"
#include "gclib.h"
#include "stats.h"
#include "gc_t.h"

/* This is an artifact of the low-level implementation of the hash pool;
   see comments above. */

#define WORDS_PER_POOL_ENTRY     2

typedef struct pool pool_t;
typedef struct remset_data remset_data_t;

struct pool {
  word   *bot;
  word   *top;
  word   *lim;
  pool_t *next;
};

struct remset_data {
  stats_id_t     self;		/* identity */
  word           *tbl_bot;	/* Hash table bottom */
  word           *tbl_lim;	/* Hash table limit */
  pool_t         *first_pool;	/* Pointer to first pool */
  pool_t         *curr_pool;	/* Pointer to current pool */
  int            pool_entries;	/* Number of entries in a pool */
  int            numpools;	/* Number of pools */
  remset_stats_t stats;		/* Remset statistics */
};

#define DATA(rs)                ((remset_data_t*)(rs->data))
#define hash_object( w, mask )  (((w) >> 4) & (mask))

static word retagptr( word w ) 
{
  if (tagof(w) == 0) {
    switch (header(*(word*)w)) {
    case VEC_HDR :
      return (word)tagptr( w, VEC_TAG );
    case BV_HDR : 
      return 0; /* signal that entry should be removed! */
    case PROC_HDR :
      return (word)tagptr( w, PROC_TAG );
    default:
      panic_abort( "remset.c: word is nonptr." );
    }
  } else {
    return w;
  }
}

static pool_t *allocate_pool_segment( unsigned pool_entries );

static void handle_overflow( remset_t *rs, unsigned recorded, word *pooltop ) 
{
  DATA(rs)->stats.recorded += recorded;
  rs->live += recorded;
  DATA(rs)->curr_pool->top = pooltop;
  
  annoyingmsg( "Remset @0x%p overflow, entries=%d", (void*)rs, rs->live);
  
  rs->has_overflowed = TRUE;
  if (DATA(rs)->curr_pool->next == 0) {
    DATA(rs)->curr_pool->next = allocate_pool_segment( DATA(rs)->pool_entries );
    DATA(rs)->numpools++;
  }
  DATA(rs)->curr_pool = DATA(rs)->curr_pool->next;
  assert( DATA(rs)->curr_pool != 0 );
}

/* Optimize: can _copy_ r2 into r1 if r1 is empty */
void rs_assimilate( remset_t *r1, remset_t *r2 )  /* r1 += r2 */
{
  pool_t *ps;
  word *p, *q;

  assert( WORDS_PER_POOL_ENTRY == 2 );
  supremely_annoyingmsg( "REMSET @0x%x assimilate @0x%x.", 
			 (word)r1, (word)r2 );

  ps = DATA(r2)->first_pool;

  while (1) {
    p = ps->bot;
    q = ps->top;
    while (p < q) {
      if (*p != 0) {
	/* enqueue the word *p in r1 */
	rs_add_elem( r1, *p );
      }
      p += 2;
    }
    if (ps == DATA(r2)->curr_pool) break;
    ps = ps->next;
  }
}

static void free_pool_segments( pool_t *first, unsigned pool_entries );

void rs_assimilate_and_clear( remset_t *r1, remset_t *r2 )  /* r1 += r2 */
{
  int hashsize;
  
  assert( WORDS_PER_POOL_ENTRY == 2 );

  /* Use the straightforward code if destination is nonempty or
     if the source is fairly empty, to avoid the expense of copying
     the hash table when it won't pay off.  
     */
  hashsize = DATA(r2)->tbl_lim-DATA(r2)->tbl_bot;
  if (r1->live > 0 || r2->live * 0.10 < hashsize)
    rs_assimilate( r1, r2 );
  else if (r2->live > 0) {
    /* r1 is empty, but r2 is not, so just copy the hash table and move the
       memory blocks.
       */
    /* Copy the hash table bits */
    remset_data_t *data1 = DATA(r1);
    remset_data_t *data2 = DATA(r2);
    
    memcpy( data1->tbl_bot, 
	    data2->tbl_bot, 
	    sizeof(word)*(data1->tbl_lim - data1->tbl_bot) );
    free_pool_segments( data1->first_pool, data1->pool_entries );
    data1->first_pool = data2->first_pool;
    data1->curr_pool = data2->curr_pool;
    data1->numpools = data2->numpools;
    data2->first_pool = allocate_pool_segment( data1->pool_entries );
    data2->curr_pool = data2->first_pool;
    data2->numpools = 1;
  }
  rs_clear( r2 );		/* Clear hash table, free extra nodes */
}

int rs_size( remset_t *rs )
{
  remset_data_t *data = DATA(rs);
  
  return (  data->pool_entries*data->numpools*WORDS_PER_POOL_ENTRY
          + data->tbl_lim - data->tbl_bot ) * sizeof(word);
}

/* Perform a consistency check and print some statistics.  Useful mainly
   when called from an interactive debugger.  If gen_no >= 0 then a
   check will be performed that every entry in the set points to a
   heap page with that generation number, and that it points to
   an apparently valid object.
*/
void rs_consistency_check( remset_t *rs, int gen_no )
{
  word *p, *q, *t, obj;
  int i, pi, qi;
  int removed_nodes = 0, empty_buckets = 0, actual_nodes = 0;

  assert( WORDS_PER_POOL_ENTRY == 2 );

  for ( t=DATA(rs)->tbl_bot, i=0 ; t < DATA(rs)->tbl_lim ; t++, i++ ) {
    p = (word*)*t;
    pi = 0;
    if (p == 0) empty_buckets++;
    while (p != 0) {
      if (*p == 0) 
	removed_nodes++; 
      else {
	actual_nodes++;
	q = (word*)*(p+1);
	qi = pi+1;
	while (q != 0) {
	  if (*p == *q) 
	    consolemsg( "Chain %d: duplicate value (%d,%d): %lx", 
		       i, pi, qi, *p );
	  qi++;
	  q = (word*)*(q+1);
	}
	obj = *p;
	if (gen_no >= 0) {
	  if (gen_of(obj) != gen_no) {
	    consolemsg( "Remset contains ptr to wrong gen: "
			"0x%08x, gen=%d, want=%d",
			obj, gen_of( obj ), gen_no );
	    conditional_abort();
	  }
#if !GCLIB_LARGE_TABLE
	  if ((attr_of(obj) & (MB_ALLOCATED|MB_HEAP_MEMORY)) !=
	      (MB_ALLOCATED|MB_HEAP_MEMORY)) {
	    consolemsg( "Remset entry points to page with bogus attributes: "
			"0x%08x, 0x%08x",
			obj, attr_of(obj) );
	    conditional_abort();
	  }
#endif
	}
	gclib_check_object( obj );
      }
      pi++;
      p = (word*)*(p+1);
    }
  }
#if 0
  consolemsg( "Total buckets: %d", DATA(rs)->tbl_lim - DATA(rs)->tbl_bot );
  consolemsg( "Empty buckets: %d", empty_buckets );
  consolemsg( "Non-null entries: %d", actual_nodes );
  consolemsg( "Null entries: %d", removed_nodes );
#endif
}

static pool_t *allocate_pool_segment( unsigned pool_entries )
{
  pool_t *p;
  word *heapptr;

  p = (pool_t*)must_malloc( sizeof( pool_t ) );

  while (1) {
    heapptr = gclib_alloc_rts( pool_entries*WORDS_PER_POOL_ENTRY*sizeof(word), 
			       MB_REMSET );
    if (heapptr != 0) break;
    memfail( MF_RTS, "Can't allocate remset hash pool." );
  }

  p->bot = p->top = heapptr;
  p->lim = heapptr + pool_entries*WORDS_PER_POOL_ENTRY;
  p->next = 0;

  return p;
}

static void free_pool_segments( pool_t *first, unsigned pool_entries )
{
  pool_t *tmp;

  while (first) {
    gclib_free( first->bot, pool_entries*WORDS_PER_POOL_ENTRY*sizeof(word) );
    tmp = first;
    first = first->next;
    free( tmp );
  }
}

/* If n is a power of 2, return log2(n). Otherwise return -1. */
static int ilog2( unsigned n )
{
  int p;

  p = 0;
  while (n > 0 && (n & 1) == 0) {
    n /= 2;
    p++;
  }
  if (n & 1) {
    n /= 2; p++;
    return (n == 0 ? p : -1);
  }

  return -1;
}

#if defined(REMSET_PROFILE)
/* utility fn for remset_crossing_stats */

static bool remset_crossing_fn( word w, void *data, unsigned *ignored )
{
  unsigned *genv = (unsigned*)data;
  unsigned tag;

  tag = tagof( w );
  if (tag == PAIR_TAG) {
    word car = *ptrof(w);
    word cdr = *(ptrof(w)+1);
    if (isptr(car)) genv[gen_of(car)]++;
    if (isptr(cdr)) genv[gen_of(cdr)]++;
  }
  else if (tag == VEC_TAG || tag == PROC_TAG) {
    unsigned size = sizefield(*ptrof(w))/4;
    word *p = ptrof(w)+1;
    while (size-- > 0) {
      word k = *p++;
      if (isptr(k)) genv[gen_of(k)]++;
    }
  }
  else 
    hardconsolemsg( "remset_crossing_fn: ouch!!" );

  return TRUE;
}

/* Examine each remembered object and count pointers from those objects
 * into each younger generation, finally producing a listing of how
 * many pointers point into which generation
 *
 * WARNING! Compacts the remset before start; this may affect the
 *          subsequent execution behavior.
 *
 * WARNING! Uses enumerate_remset, so it will skew the scanned statistics 
 *          for the remset.
 */
void rs_print_crossing_stats( remset_t *rs )
{
  unsigned genv[ MAX_GENERATIONS  ];
  int i, j;

  memset( genv, 0, sizeof(genv) );

  rs_compact( rs );
  rs_enumerate( rs, remset_crossing_fn, genv );

  for ( i=GENERATIONS-1 ; i >= 0 ; i-- ) /* find highest nonzero entry */
    if (genv[i] > 0) break;

  for ( j=0 ; j <= i ; j++ )	         /* print stats */
    consolemsg( "gen=%d: %u pointers", j, genv[j] );
}

#endif

/* eof */

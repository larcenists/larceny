/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Remembered set implementation.
 *
 * The set is implemented as a sequential store buffer (SSB) for recording
 * references, combined with a simple chaining hash table to remove 
 * duplicates.  The structure remset_data_t below has all private data.
 *
 * Sequential store buffer.
 *
 * There is a fixed-size sequential store buffer (SSB) in which the mutator
 * records references to objects when intergenerational pointers are created.
 * The SSB starts at *rs->ssb_bot and extends to *rs->ssb_lim, traced by 
 * *rs->ssb_top.  Its size is fixed.  When the SSB fills up, it is compacted
 * into the hash table.
 *
 * Hash table.
 *
 * The hash table starts at data->tbl_bot and extends to data->tbl_lim;
 * it is allocated at startup.  Its size is fixed, and always a power of
 * two.  Each entry is a single word and is a pointer into the pool of nodes.
 *
 * Node pool.
 *
 * The pool of nodes is a linked list of pool segments (to handle overflow
 * when the SSB is compacted).  Each pool node has a chunk of memory for
 * the segment; the memory extends from pool->bot to pool->lim, with
 * pool->top marking the next available word.  The pool is a sequential
 * array of words.  Each node takes up two words: the first is a Scheme
 * object, the second is either 0 or a pointer to the next node in the chain.
 * Nodes whose first word is zero represent objects that have been removed.
 * The pointer data->first_pool points to the first pool segment in use, 
 * and data->curr_pool points to the current.  Unused segments may exist 
 * past the current one.
 *
 * Related work.
 *
 * The implementation was inspired by a description in the following paper:
 *   Anthony L. Hosking, J. Eliot B. Moss, and Darko Stefanovic:
 *   "A comparative performance evaluation of write barrier implementations"
 *   Proceedings of OOPSLA '92, pp 92-109.
 * but differs in the details.  
 *
 * The main advantages of this implementation are thought to be that it's
 * simpler (no table resizing) and that scanning takes time proportional to
 * the number of remembered objects.  
 * 
 * The main disadvantage of a hash table is that a fair bit of work
 * has to be performed when it is cleared (it has to be zeroed).
 *
 * Notes.
 *
 * The main reason for the low-level implementation of the node pool
 * (an array of two-word structures would have been more natural) is
 * to allow the remembered-set forwarding scanner to scan more than one
 * object at a time, an optimization that is not currently implemented.
 *
 * The SSB may contain raw pointers (pointers where the tag is #b000).
 * These pointers are placed in the SSB by the large-object allocator in
 * the generational collector; it was either that or requiring every 
 * allocation call to pass a type descriptor.  When compacting the SSB,
 * we must follow the pointer and tag it with a tag that corresponds to the
 * object header (always bytevector, vector, or procedure) before placing 
 * it in the remembered set.
 */

#define GC_INTERNAL

#include <stdlib.h>

#include "larceny.h"
#include "macros.h"
#include "memmgr.h"
#include "remset_t.h"
#include "gclib.h"
#include "stats.h"

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


/* Internal */

static int identity = 0;
  /* Counter for assigning identity to remembered sets.
     */

static int    log2( unsigned n );
static pool_t *allocate_pool_segment( unsigned entries );
static void   free_pool_segments( pool_t *first, unsigned entries );
static void ssb_consistency_check( remset_t *rs );


remset_t *
create_remset( int tbl_entries,    /* size of hash table, 0 = default */
	       int pool_entries,   /* size of remset, 0 = default */
	       int ssb_entries,    /* size of ssb, 0 = default */
	       word **ssb_bot_loc, /* cell for ssb_bot */
	       word **ssb_top_loc, /* cell for ssb_top */
	       word **ssb_lim_loc  /* cell for ssb_lim */
	      )
{
  return create_labelled_remset( tbl_entries,
				 pool_entries,
				 ssb_entries,
				 ssb_bot_loc,
				 ssb_top_loc,
				 ssb_lim_loc,
				 ++identity,
				 0 );
}

remset_t *
create_labelled_remset( int tbl_entries,    /* size of hash table, 0=default */
			int pool_entries,   /* size of remset, 0 = default */
			int ssb_entries,    /* size of ssb, 0 = default */
			word **ssb_bot_loc, /* cell for ssb_bot */
			word **ssb_top_loc, /* cell for ssb_top */
			word **ssb_lim_loc, /* cell for ssb_lim */
			int major_id,       /* for stats */
			int minor_id        /* for stats */
			)
{
  word *heapptr;
  remset_t *rs;
  remset_data_t *data;
  pool_t *p;

  assert( tbl_entries >= 0 && (tbl_entries == 0 || log2( tbl_entries ) != -1));
  assert( pool_entries >= 0 );
  assert( ssb_entries >= 0 );

  if (pool_entries == 0) pool_entries = DEFAULT_REMSET_POOLSIZE;
  if (tbl_entries == 0) tbl_entries = DEFAULT_REMSET_TBLSIZE;
  if (ssb_entries == 0) ssb_entries = DEFAULT_SSB_SIZE;

  annoyingmsg( "Allocated remembered set\n  hash=%d ssb=%d pool=%d",
	       tbl_entries, ssb_entries, pool_entries );

  rs   = (remset_t*)must_malloc( sizeof( remset_t ) );
  data = (remset_data_t*)must_malloc( sizeof( remset_data_t ) );

  while(1) {
    heapptr = gclib_alloc_rts( (tbl_entries + ssb_entries)*sizeof(word), 
			       MB_REMSET );
    if (heapptr != 0) break;
    memfail( MF_RTS, "Can't allocate table and SSB for remembered set." );
  }

  /* Hash table */
  data->tbl_bot = heapptr;
  heapptr += tbl_entries;
  data->tbl_lim = heapptr;

  /* SSB */
  rs->ssb_bot = ssb_bot_loc;
  rs->ssb_top = ssb_top_loc;
  rs->ssb_lim = ssb_lim_loc;
  *rs->ssb_bot = *rs->ssb_top = heapptr;
  heapptr += ssb_entries;
  *rs->ssb_lim = heapptr;

  /* Node pool */
  p = allocate_pool_segment( pool_entries );
  data->first_pool = data->curr_pool = p;
  data->numpools = 1;

  /* Misc */
  memset( &data->stats, 0, sizeof( data->stats ));
  data->pool_entries = pool_entries;
  data->self = stats_new_remembered_set( major_id, minor_id );

  rs->live = 0;
  rs->has_overflowed = FALSE;
  rs->data = data;

  rs_clear( rs );

  return rs;
}

void rs_clear( remset_t *rs )
{
  remset_data_t *data = DATA(rs);
  word *p;
  int i;

  supremely_annoyingmsg( "REMSET @0x%p: clear", (void*)rs );

  /* Clear SSB */
  *rs->ssb_top = *rs->ssb_bot;

  /* Clear hash table */
  for ( p=data->tbl_bot, i=data->tbl_lim-data->tbl_bot ; i > 0 ; p++, i-- )
    *p = (word)(word*)0;

  /* Clear pools */
  data->first_pool->top = data->first_pool->bot;
  data->curr_pool = data->first_pool;
  free_pool_segments( data->first_pool->next, data->pool_entries );
  data->first_pool->next = 0;

  rs->has_overflowed = FALSE;
  rs->live = 0;
  data->numpools = 1;
  data->stats.cleared++;
}

bool rs_compact( remset_t *rs )
{
  word *p, *q, mask, *tbl, w, *b, *pooltop, *poollim, tblsize, h;
  unsigned recorded;
  remset_data_t *data = DATA(rs);

  assert( WORDS_PER_POOL_ENTRY == 2 );

  supremely_annoyingmsg( "REMSET @0x%p: compact", (void*)rs );
  data->stats.ssb_recorded += *rs->ssb_top - *rs->ssb_bot;

  p = *rs->ssb_bot;
  q = *rs->ssb_top;
  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;
  recorded = 0;

  /* (The scan is down for historical reasons that no longer apply.) */

  while (q > p) {
    q--;
    w = *q;
    if (tagof( w ) == 0) {	/* See NOTE above. */
      switch (header(*(word*)w)) {
      case VEC_HDR :
	w = (word)tagptr( w, VEC_TAG );
	break;
      case BV_HDR :
	continue;		/* Remove the entry! */
      case PROC_HDR :
	w = (word)tagptr( w, PROC_TAG );
	break;
      default :
	panic_abort( "rs_compact" );
      }
    }
    h = hash_object( w, mask );
    b = (word*)tbl[ h ];
    while (b != 0 && *b != w) 
      b = (word*)*(b+1);
    if (b == 0) {
      if (pooltop == poollim) {
	data->stats.recorded += recorded;
	rs->live += recorded;
	recorded = 0;
	data->curr_pool->top = pooltop;

	annoyingmsg( "Remset @0x%p overflow, entries=%d", (void*)rs, rs->live);

	rs->has_overflowed = TRUE;
	if (data->curr_pool->next == 0) {
	  data->curr_pool->next = allocate_pool_segment( data->pool_entries );
	  data->numpools++;
	}
	data->curr_pool = data->curr_pool->next;

	pooltop = data->curr_pool->top;
	poollim = data->curr_pool->lim;
      }
      *pooltop = w;
      *(pooltop+1) = tbl[h];
      tbl[h] = (word)pooltop;
      pooltop += 2;
      recorded++;
    }
  }

  data->stats.recorded += recorded;
  rs->live += recorded;
  data->curr_pool->top = pooltop;
  *rs->ssb_top = *rs->ssb_bot;
  data->stats.compacted++;

  supremely_annoyingmsg( "REMSET @0x%x: Added %u elements (total %u). oflo=%d",
                         (word)rs, recorded, rs->live, rs->has_overflowed);

  return rs->has_overflowed;
}

bool rs_compact_nocheck( remset_t *rs )
{
  word *p, *q, mask, *tbl, w, *pooltop, *poollim, tblsize, h;
  unsigned recorded;
  remset_data_t *data = DATA(rs);

  assert( WORDS_PER_POOL_ENTRY == 2 );

  supremely_annoyingmsg( "REMSET @0x%p: fast compact", (void*)rs );
  data->stats.ssb_recorded += *rs->ssb_top - *rs->ssb_bot;

  p = *rs->ssb_bot;
  q = *rs->ssb_top;
  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;
  recorded = 0;

  /* (The scan is down for historical reasons that no longer apply.) */

  while (q > p) {
    q--;
    w = *q;
    if (tagof( w ) == 0) {      /* See NOTE above. */
      switch (header(*(word*)w)) {
      case VEC_HDR :
	w = (word)tagptr( w, VEC_TAG );
	break;
      case BV_HDR :
	continue;		/* Remove the entry! */
      case PROC_HDR :
	w = (word)tagptr( w, PROC_TAG );
	break;
      default :
	panic_abort( "rs_compact_nocheck" );
      }
    }
    h = hash_object( w, mask );
    if (pooltop == poollim) {
      data->stats.recorded += recorded;
      rs->live += recorded;
      recorded = 0;
      data->curr_pool->top = pooltop;

      annoyingmsg( "Remset @0x%p overflow, entries=%d", (void*)rs, rs->live );

      rs->has_overflowed = TRUE;
      if (data->curr_pool->next == 0) {
	data->curr_pool->next = allocate_pool_segment( data->pool_entries );
	data->numpools++;
      }
      data->curr_pool = data->curr_pool->next;

      pooltop = data->curr_pool->top;
      poollim = data->curr_pool->lim;
    }
    *pooltop = w;
    *(pooltop+1) = tbl[h];
    tbl[h] = (word)pooltop;
    pooltop += 2;
    recorded++;
  }

  data->stats.recorded += recorded;
  rs->live += recorded;
  data->curr_pool->top = pooltop;
  *rs->ssb_top = *rs->ssb_bot;
  data->stats.compacted++;

  return rs->has_overflowed;
}

/* FIXME: Worth optimizing!  Pass more than one object to the scanner. */
void rs_enumerate( remset_t *rs, 
		   bool (*scanner)( word, void*, unsigned* ),
		   void *data )
{
  pool_t *ps;
  word *p, *q;
  unsigned word_count = 0;
  unsigned removed_count=0;
  unsigned scanned = 0;
  unsigned scanned_all = 0;

  assert( WORDS_PER_POOL_ENTRY == 2 );

  supremely_annoyingmsg( "REMSET @0x%p: scan", (void*)rs );

  if ( *rs->ssb_top != *rs->ssb_bot )
    rs_compact( rs );

  ps = DATA(rs)->first_pool;
  while (1) {
    p = ps->bot;
    q = ps->top;
    scanned_all += (q-p)/2;	/* Zero entries also */
    while (p < q) {
      if (*p != 0) {
#if !GCLIB_LARGE_TABLE		/* These attributes not defined then */
	assert2( (attr_of(*p) & (MB_ALLOCATED|MB_HEAP_MEMORY)) ==
		 (MB_ALLOCATED|MB_HEAP_MEMORY) );
#endif
	if (!scanner( *p, data, &word_count )) {
	  /* Clear the slot by setting the pointer to 0. */
	  *p = (word)(word*)0;
	  removed_count++;
	}
	scanned++;		/* Only nonzero entries */
      }
      p += 2;
    }
    if (ps == DATA(rs)->curr_pool) break;
    ps = ps->next;
  }
  DATA(rs)->stats.objs_scanned += scanned;
  DATA(rs)->stats.words_scanned += word_count;
  DATA(rs)->stats.removed += removed_count;
  rs->live -= removed_count;
  DATA(rs)->stats.scanned++;
  supremely_annoyingmsg( "REMSET @0x%x: removed %d elements (total %d).", 
			 (word)rs, removed_count, 
			 DATA(rs)->stats.removed );
}

/* Optimize: can _copy_ r2 into r1 if r1 is empty */
void rs_assimilate( remset_t *r1, remset_t *r2 )  /* r1 += r2 */
{
  pool_t *ps;
  word *p, *q, *top, *lim;

  assert( WORDS_PER_POOL_ENTRY == 2 );
  supremely_annoyingmsg( "REMSET @0x%x assimilate @0x%x.", 
			 (word)r1, (word)r2 );

  rs_compact( r2 );
  ps = DATA(r2)->first_pool;
  top = *r1->ssb_top;
  lim = *r1->ssb_lim;
  while (1) {
    p = ps->bot;
    q = ps->top;
    while (p < q) {
      if (*p != 0) {
	*top++ = *p;
	if (top == lim) {
	  *r1->ssb_top = top;
	  rs_compact( r1 );
	  top = *r1->ssb_top;
	}
      }
      p += 2;
    }
    if (ps == DATA(r2)->curr_pool) break;
    ps = ps->next;
  }
  *r1->ssb_top = top;
  rs_compact( r1 );
}

void rs_assimilate_and_clear( remset_t *r1, remset_t *r2 )  /* r1 += r2 */
{
  int hashsize;
  
  assert( WORDS_PER_POOL_ENTRY == 2 );

  rs_compact( r1 );
  rs_compact( r2 );

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
          + data->tbl_lim - data->tbl_bot
	  + *rs->ssb_lim - *rs->ssb_bot ) * sizeof(word);
}

void rs_stats( remset_t *rs )
{
  remset_data_t *data = DATA(rs);

  data->stats.allocated = 
    (data->tbl_lim - data->tbl_bot) +
    (*rs->ssb_lim - *rs->ssb_bot) +
    (data->pool_entries*data->numpools*WORDS_PER_POOL_ENTRY);

  data->stats.used =
    (data->tbl_lim - data->tbl_bot) +
    (*rs->ssb_lim - *rs->ssb_bot) +
    data->pool_entries*(data->numpools-1)*WORDS_PER_POOL_ENTRY +
    (data->curr_pool->top - data->curr_pool->bot);

  data->stats.live = 
    (data->tbl_lim - data->tbl_bot) +
    (*rs->ssb_lim - *rs->ssb_bot) +
    rs->live;

  stats_add_remset_stats( data->self, &data->stats );
  memset( &data->stats, 0, sizeof( remset_stats_t ) );
}

bool rs_isremembered( remset_t *rs, word w )
{
  word mask, *tbl, *b, tblsize, h;
  remset_data_t *data = DATA(rs);

  assert( WORDS_PER_POOL_ENTRY == 2 );

  /* Clear SSB first */
  rs_compact( rs );

  /* Search hash table */
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;

  h = hash_object( w, mask );
  b = (word*)tbl[ h ];
  while (b != 0 && *b != w) 
    b = (word*)*(b+1);

  return b != 0;
}

void rs_consistency_check( remset_t *rs, int gen_no )
{
  word *p, *q, *t, obj;
  int i, pi, qi;
  int removed_nodes = 0, empty_buckets = 0, actual_nodes = 0;

  assert( WORDS_PER_POOL_ENTRY == 2 );

  ssb_consistency_check( rs );

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

static void ssb_consistency_check( remset_t *rs )
{
  word *p, *q;

  p = *rs->ssb_bot;
  q = *rs->ssb_top;

  while (p < q) {
    gclib_check_object( *p );
    p++;
  }
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
static int log2( unsigned n )
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

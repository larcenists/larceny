/* Rts/Sys/remset.c.
 * Larceny run-time system -- remembered set implementation.
 *
 * $Id: remset.c,v 1.9 1997/09/23 19:57:44 lth Exp lth $
 *
 *
 * The remembered set is an ADT with a public interface as described
 * in Rts/Sys/memmgr.h.
 *
 * The set is implemented as a sequential store buffer (SSB) for recording
 * references, combined with a simple chaining hash table to remove 
 * duplicates.  The structure remset_data_t below has all private data.
 *
 * Sequential store buffer.
 *
 * There is a fixed-size sequential store buffer (SSB) in which the mutator
 * records references to tenured objects when intergenerational pointers
 * are created.  This buffer starts at *remset->ssb_bot and extends to
 * *remset->ssb_lim, traced by *rs->ssb_top.  Its size is fixed.  When the
 * SSB fills up, it is compacted into the hash table.
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
 * object (a root, as it were), the second is either 0 or a pointer to 
 * the next node in the chain.  The pointer data->first_pool points to
 * the first pool segment in use, and data->curr_pool points to the
 * current (last segment).  Unused segments may exist past the current one.
 *
 * SSB compaction  may overflow the node pool, so the garbage collector
 * should check remset->has_overflowed now and again to see if a promotion 
 * is due.
 *
 * The node pool is garbage collected (!) when, after a segment has filled
 * up, it is discovered that the pool contains more than 50% garbage.
 * The precise amount of garbage is known because deallocation is explicit.
 * It is not practical to use a free list of nodes for two reasons:
 *  - when a node is deallocated it cannot be used until it is removed
 *    from the hash chain it is on, so garbage collection must take
 *    place anyway.
 *  - a free list complicates the fast path of the SSB compaction algorithm.
 *
 * Related work.
 *
 * The implementation was inspired by a description in the following paper:
 *   Anthony L. Hosking, J. Eliot B. Moss, and Darko Stefanovic:
 *   "A comparative performance evaluation of write barrier implementations"
 *   Proceedings of OOPSLA '92, pp 92-109.
 * Their implementation has a rather more complex hash table.
 *
 * The main advantages of Larceny's implementation are:
 *  - simplicity (actually, it's becoming less and less simple...)
 *  - scanning during an ephemeral collection takes time proportional to
 *    the number of remembered objects (Hosking's scheme appears to take
 *    time proportional to the size of the hash table).
 *
 * The main disadvantage of a hash table is that a fair bit of work
 * has to be performed when it is cleared (it has to be zeroed). If this
 * is too expensive, we can use a card-marking scheme to clear only 
 * those parts of the table that have been dirtied.
 *
 * Notes.
 *
 * The internal data structures (hash table, pool segments) use very
 * low-level representations; notably, the pool segments are arrays of
 * words where the even-numbered word holds a Scheme object and the odd-
 * numbered word holds the 'next' pointer for the hash chain.  This is
 * painful.  The main reason for using this structure is to allow the
 * remembered-set forwarding scanner to scan more than one object at a
 * time (an optimization that is not currently implemented).
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

/* This is an artifact of the low-level implementation of the hash pool;
   see comments above. */

#define WORDS_PER_POOL_ENTRY     2


/* This is a pool segment */
typedef struct pool pool_t;

struct pool {
  word *bot;
  word *top;
  word *lim;
  pool_t *next;
};


/* This is remembered-set internal data */
typedef struct remset_data remset_data_t;

struct remset_data {
  unsigned ssb_recorded;  /* # of ssb entries recorded */
  unsigned hash_recorded; /* # of hash entries added */
  unsigned hash_scanned;  /* # of hash entries scanned */
  unsigned word_count;    /* # of words of remembered objects scanned */
  unsigned removed_count; /* # of hash tbl entries removed from table */
  unsigned garbage;       /* # of known garbage items in table */
  unsigned live;          /* # of known live items in table */
  word     *tbl_bot;      /* Hash table bottom */
  word     *tbl_lim;      /* Hash table limit */
  pool_t   *first_pool;   /* Pointer to first pool */
  pool_t   *curr_pool;    /* Pointer to current pool */
  unsigned pool_size;     /* Number of entries in a pool */
  int      has_overflowed;
};

#define DATA(rs)   ((remset_data_t*)(rs->data))


/* ADT operations */
static void clear_remset( remset_t *rs );
static int  compact_ssb( remset_t *rs );
static void enumerate_remset( remset_t *rs,
			      int (*scanner)(word, void*, unsigned*), void* );
static void get_stats( remset_t *rs, remset_stats_t *stats );
static int  has_overflowed( remset_t *rs );
static int  isremembered( remset_t *rs, word w );
static void assimilate( remset_t *r1, remset_t *r2 );


/* Internal */
static int log2( unsigned n );
static pool_t *allocate_pool_segment( unsigned entries );
static void free_pool_segments( pool_t *first, unsigned entries );
static void hash_table_gc( remset_t *rs );


/* Create a remembered set object.
 * 
 * Tblsize must be power of 2; if it is not, the default size is used.
 */
remset_t *
create_remset( unsigned tbl_entries,  /* size of hash table, 0 = default */
	       unsigned pool_entries, /* size of remset, 0 = default */
	       unsigned ssb_entries,  /* size of ssb, 0 = default */
	       word **ssb_bot_loc,    /* cell for ssb_bot */
	       word **ssb_top_loc,    /* cell for ssb_top */
	       word **ssb_lim_loc     /* cell for ssb_lim */
	      )
{
  word *heapptr;
  remset_t *rs;
  remset_data_t *data;
  pool_t *p;

  if (pool_entries == 0) pool_entries = DEFAULT_REMSET_POOLSIZE;
  if (tbl_entries == 0) tbl_entries = DEFAULT_REMSET_TBLSIZE;
  if (ssb_entries == 0) ssb_entries = DEFAULT_SSB_SIZE;

  if (log2( tbl_entries ) == -1)
    tbl_entries = DEFAULT_REMSET_TBLSIZE;   /* too fascist, but works */
  
  rs = (remset_t*)must_malloc( sizeof( remset_t ) );
  data = (remset_data_t*)must_malloc( sizeof( remset_data_t ) );

 again2:
  heapptr = gclib_alloc_rts( tbl_entries*sizeof(word)+ssb_entries*sizeof(word),
			     MB_REMSET );
  if (heapptr == 0) {
    memfail( MF_RTS, "Can't allocate SSB for remembered set." );
    goto again2;
  }

  /* Hash table: fixed size */
  data->tbl_bot = heapptr;
  heapptr += tbl_entries;
  data->tbl_lim = heapptr;

  /* SSB: fixed size */
  rs->ssb_bot = ssb_bot_loc;
  rs->ssb_top = ssb_top_loc;
  rs->ssb_lim = ssb_lim_loc;
  *rs->ssb_bot = *rs->ssb_top = heapptr;
  heapptr += ssb_entries;
  *rs->ssb_lim = heapptr;

  p = allocate_pool_segment( pool_entries );

  data->ssb_recorded = 0;
  data->hash_scanned = 0;
  data->hash_recorded = 0;
  data->has_overflowed = 0;
  data->removed_count = 0;
  data->word_count = 0;
  data->garbage = 0;
  data->live = 0;

  data->first_pool = data->curr_pool = p;
  data->pool_size = pool_entries;

  rs->clear = clear_remset;
  rs->compact = compact_ssb;
  rs->enumerate = enumerate_remset;
  rs->stats = get_stats;
  rs->has_overflowed = has_overflowed;
  rs->isremembered = isremembered;
  rs->assimilate = assimilate;

  rs->data = data;

  clear_remset( rs );

  return rs;
}


/* clear_remset(): Clear the remembered set. 
 *
 * The hash table has to be zeroed, the SSB reset, and the pool pointers reset.
 */
static void clear_remset( remset_t *rs )
{
  remset_data_t *data = DATA(rs);
  pool_t *ps;
  word *p;

  debugmsg( "[debug] clearing remset @0x%x.", (word)rs );

  /* Clear SSB */
  *rs->ssb_top = *rs->ssb_bot;

  /* Clear hash table */
  for ( p = data->tbl_bot ; p < data->tbl_lim ; p++ )
    *p = (word)(word*)0;

  /* Clear pools */
  ps = data->first_pool;
  while (ps != 0) {
    ps->top = ps->bot;
    ps = ps->next;
  }
  data->curr_pool = data->first_pool;

  /* Clear overflow bit */
  data->has_overflowed = 0;
  data->live = 0;
  data->garbage = 0;
}


/* compact_ssb(): compact SSB.
 *
 * Expands the remembered-set pool if the current pool overflows;
 * the SSB will be empty when this procedure returns.
 *
 * NOTE:  The SSB may contain raw pointers (pointers where the tag is 000).
 * These pointers are placed in the SSB by the large-object allocator in
 * the generational collector (it was either that or requiring every 
 * allocation call to pass a type descriptor).  When compacting the SSB,
 * we follow the pointer and tag it with a tag that corresponds to the
 * object header (always bytevector, vector, or procedure) before placing 
 * it in the remembered set.
 */
static int compact_ssb( remset_t *rs )
{
  word *p, *q, mask, *tbl, w, *b, *pooltop, *poollim, tblsize, h;
  unsigned recorded;
  remset_data_t *data = DATA(rs);

/*  debugmsg( "   *** compact SSB for remset @0x%p", rs ); */
  data->ssb_recorded += *rs->ssb_top - *rs->ssb_bot;

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
    if (tagof( w ) == 0) {
      /* See NOTE above. */
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
	panic_abort( "compact_ssb" );
      }
    }
    h = (w >> 4) & mask;    /* experimental */
    b = (word*)tbl[ h ];
    while (b != 0 && *b != w) 
      b = (word*)*(b+1);
    if (b == 0) {
      if (pooltop == poollim) {
	int need_new = 0;

	data->hash_recorded += recorded;
	data->live += recorded;
	recorded = 0;
	data->curr_pool->top = pooltop;

	/* We can always GC the remset, but should only do it when it
	 * pays off.  (It's a feature that it's always OK to GC -- it
	 * makes it _much_ easier to test the remset GC!  Just || with 1
	 * in the test expression.)
	 */
	if (data->garbage > data->live / 2) {
	  hash_table_gc( rs );
	  need_new = data->curr_pool->top == data->curr_pool->lim;
	}
	else {
	  data->has_overflowed = 1;
	  need_new = 1;
	}
	if (need_new) {
	  if (data->curr_pool->next == 0)
	    data->curr_pool->next = allocate_pool_segment( data->pool_size );
	  data->curr_pool = data->curr_pool->next;
	}
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

  data->hash_recorded += recorded;
  data->live += recorded;
  data->curr_pool->top = pooltop;
  *rs->ssb_top = *rs->ssb_bot;

#if 0
  supremely_annoyingmsg( "Added %u elements to remembered set (total %u). %d",
                         recorded, data->live, data->has_overflowed);
#endif

  return data->has_overflowed;
}


/* enumerate_remset(): scanning old objects during gc.
 *
 * Given a procedure and an opaque argument for it, call the procedure 
 * with  each member of the remembered set.
 */
static void
enumerate_remset( remset_t *rs, int (*scanner)( word, void*, unsigned* ),
		  void *data )
{
  pool_t *ps;
  word *p, *q;
  unsigned word_count = 0;
  unsigned removed_count=0;

  assert( *rs->ssb_top == *rs->ssb_bot );

  debugmsg( "[debug] Scanning remset @0x%x.", (word)rs );

  ps = DATA(rs)->first_pool;
  while (1) {
    p = ps->bot;
    q = ps->top;
    DATA(rs)->hash_scanned += (q-p)/2;
    while (p < q) {
      if (*p != 0) {
	if (!scanner( *p, data, &word_count )) {
	  /* Mark the slot by setting the pointer to 0. */
	  *p = (word)(word*)0;
	  removed_count++;
	}
      }
      p += 2;
    }
    if (ps == DATA(rs)->curr_pool) break;
    ps = ps->next;
  }
  DATA(rs)->garbage += removed_count;
  DATA(rs)->word_count += word_count;
  DATA(rs)->removed_count += removed_count;
}


/* Fold the entries of r2 into r1. */
static void
assimilate( remset_t *r1, remset_t *r2 )
{
  pool_t *ps;
  word *p, *q;

  r2->compact( r2 );

  debugmsg( "[debug] Assimilating remset @0x%x into remset @0x%x.", 
	    (word)r2, (word)r1 );

  ps = DATA(r2)->first_pool;
  while (1) {
    p = ps->bot;
    q = ps->top;
    while (p < q) {
      if (*p != 0) {
	**r1->ssb_top = *p;
	*r1->ssb_top = *r1->ssb_top + 1;
	if (*r1->ssb_top == *r1->ssb_lim)
	  r1->compact( r1 );
      }
      p += 2;
    }
    if (ps == DATA(r2)->curr_pool) break;
    ps = ps->next;
  }
}


/* Node pool garbage collection.
 *
 * Traverse the hash table, copying all chains into a new node pool
 * but skipping 0 nodes.  Since a node is on at most one chain, 
 * this is completely straightforward.
 */
static void
hash_table_gc( remset_t *rs )
{
  unsigned live;
  word *bot, *lim, *p, *q;
  pool_t *ps, *ps1;

  hardconsolemsg( "[debug] Remset @0x%lx: garbage collection\n", (long)rs );

  bot = DATA(rs)->tbl_bot;
  lim = DATA(rs)->tbl_lim;
  live = 0;

  ps1 = ps = allocate_pool_segment( DATA(rs)->pool_size );
  while (bot < lim) {
    p = (word*)*bot;
    *bot = (word)(word*)0;
    while (p != 0) {
      if (*p != (word)(word*)0) {
	if (ps->top == ps->lim) {
	  ps->next = allocate_pool_segment( DATA(rs)->pool_size );
	  ps = ps->next;
	}
	q = ps->top;
	ps->top += 2;
	*q = *p;
	*(q+1) = (word)(word*)*bot;
	*bot = (word)q;
	live++;
      }
      p = (word*)*(p+1);
    }
    bot++;
  }

  free_pool_segments( DATA(rs)->first_pool, DATA(rs)->pool_size );
  DATA(rs)->first_pool = ps1;
  DATA(rs)->curr_pool= ps;
  DATA(rs)->live = live;
  DATA(rs)->garbage = 0;
}


/* Fill in the statistics buffer with accumulated data. */

static void get_stats( remset_t *rs, remset_stats_t *stats )
{
  stats->ssb_recorded = DATA(rs)->ssb_recorded;
  stats->hash_recorded = DATA(rs)->hash_recorded;
  stats->hash_scanned = DATA(rs)->hash_scanned;
  stats->words_scanned = DATA(rs)->word_count;
  stats->hash_removed = DATA(rs)->removed_count;
  DATA(rs)->ssb_recorded = 0;
  DATA(rs)->hash_recorded = 0;
  DATA(rs)->hash_scanned = 0;
  DATA(rs)->word_count = 0;
  DATA(rs)->removed_count = 0;
}


/* Returns 1 if the remembered-set hash table has overflowed since the
 * last time the set was cleared.
 */
static int has_overflowed( remset_t *rs )
{
  return DATA(rs)->has_overflowed;
}


/* Is the object denoted by w remembered by this set?
 * This procedure is used by the simulator for the new write barrier.
 */

static int isremembered( remset_t *rs, word w )
{
  word mask, *tbl, *b, tblsize, h;
  remset_data_t *data = DATA(rs);

  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;

  h = (w >> 4) & mask;
  b = (word*)tbl[ h ];
  while (b != 0 && *b != w) b = (word*)*(b+1);
  return b != 0;
}


/* Allocate a pool segment. */

static pool_t *allocate_pool_segment( unsigned pool_entries )
{
  pool_t *p;
  word *heapptr;

  p = (pool_t*)must_malloc( sizeof( pool_t ) );

 again2:
  heapptr = gclib_alloc_rts( pool_entries*WORDS_PER_POOL_ENTRY*sizeof(word), 
			     MB_REMSET );
  if (heapptr == 0) {
    memfail( MF_RTS, "Can't allocate remset hash pool." );
    goto again2;
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
  else
    return -1;
}

/*
 * The following functions are not called from within the RTS, but
 * from an interactive debugger.
 */

/* Remebered-set consistency check -- way useful for debugging. */

static void 
remset_consistency_check( remset_t *rs )
{
  word *p, *q, *t;
  int i, pi, qi;
  int removed_nodes = 0, empty_buckets = 0, actual_nodes = 0;

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
	    consolemsg( "Chain %d: duplicate value (%d,%d): %lx\n", 
		       i, pi, qi, *p );
	  qi++;
	  q = (word*)*(q+1);
	}
      }
      pi++;
      p = (word*)*(p+1);
    }
  }
  consolemsg( "Total buckets: %d", DATA(rs)->tbl_lim - DATA(rs)->tbl_bot );
  consolemsg( "Empty buckets: %d", empty_buckets );
  consolemsg( "Non-null entries: %d", actual_nodes );
  consolemsg( "Null entries: %d", removed_nodes );
}


/* utility fn for remset_crossing_stats */

static int remset_crossing_fn( word w, void *data, unsigned *ignored )
{
  unsigned *genv = (unsigned*)data;
  unsigned tag;

  tag = tagof( w );
  if (tag == PAIR_TAG) {
    word car = *ptrof(w);
    word cdr = *(ptrof(w)+1);
    if (isptr(car)) genv[gclib_desc_g[pageof(car)]]++;
    if (isptr(cdr)) genv[gclib_desc_g[pageof(cdr)]]++;
  }
  else if (tag == VEC_TAG || tag == PROC_TAG) {
    unsigned size = sizefield(*ptrof(w))/4;
    word *p = ptrof(w)+1;
    while (size-- > 0) {
      word k = *p++;
      if (isptr(k)) genv[gclib_desc_g[pageof(k)]]++;
    }
  }
  else 
    hardconsolemsg( "remset_crossing_fn: ouch!!" );

  return 1;
}

/* Look at each remembered object and count pointers from those objects
 * into each younger generation, finally producing a listing of how
 * many pointers point into which generation
 *
 * WARNING! Compacts the remset before start; this may affect the
 *          subsequent execution behavior.
 *
 * WARNING! Uses enumerate_remset, so it will skew the scanned statistics 
 *          for the remset.
 */

/* For some reason gcc throws this function away if it's static */

void remset_crossing_stats( remset_t *rs )
{
#define ASIZE 32
  unsigned genv[ ASIZE  ];  /* FIXME */
  int i, j;

  for ( i=0 ; i<ASIZE ; i++) genv[i] = 0;

  rs->compact( rs );
  rs->enumerate( rs, remset_crossing_fn, genv );

  for ( i=ASIZE-1 ; i >= 0 ; i-- )
    if (genv[i] > 0) break;
  for ( j=0 ; j <= i ; j++ )
    consolemsg( "gen=%d: %u pointers", j, genv[j] );
}

/* eof */

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
 * NOTE 1 The main reason for the low-level implementation of the node pool
 * (an array of two-word structures would have been more natural) is
 * to allow the remembered-set forwarding scanner to scan more than one
 * object at a time, an optimization that is not currently implemented.
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
#include "summary_t.h"

/* This is an artifact of the low-level implementation of the hash pool;
   see comments above. */

#define WORDS_PER_POOL_ENTRY     2

#define ATTEMPT_TO_REUSE_POOL_SEGMENTS 0

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
  unsigned       mem_attribute;	/* Attr identifying which Rts part owns mem */
};

#define DATA(rs)                ((remset_data_t*)(rs->data))
#define hash_object( w, mask )  (((w) >> 4) & (mask))


/* Internal */

static pool_t *recycled_pool = NULL;
static int recycled_pool_entries_per = -1;

static int identity = 0;
  /* Counter for assigning identity to remembered sets.
     */

static int    ilog2( unsigned n );
static pool_t *allocate_pool_segment( unsigned entries, unsigned attr );
static void   free_pool_segments( pool_t *first, unsigned entries );

static remset_t *
create_labelled_remset_with_owner_attrib
  ( int tbl_entries,    /* size of hash table, 0=default */
    int pool_entries,   /* size of remset, 0 = default */
    int major_id,       /* for stats */
    int minor_id,       /* for stats */
    unsigned owner_attrib
    );

remset_t *
create_remset( int tbl_entries,    /* size of hash table, 0 = default */
	       int pool_entries    /* size of remset, 0 = default */
	      )
{
  return create_labelled_remset( tbl_entries,
				 pool_entries,
				 ++identity,
				 0 );
}

remset_t *
create_summset( int tbl_entries,    /* size of hash table, 0 = default */
	        int pool_entries    /* size of remset, 0 = default */
	      )
{
  return create_labelled_remset_with_owner_attrib( tbl_entries,
						   pool_entries,
						   ++identity,
						   0,
						   MB_SUMMARY_SETS );
}

remset_t *
create_labelled_remset( int tbl_entries,    /* size of hash table, 0=default */
			int pool_entries,   /* size of remset, 0 = default */
			int major_id,       /* for stats */
			int minor_id        /* for stats */
			)
{
  return create_labelled_remset_with_owner_attrib( tbl_entries,
						   pool_entries,
						   major_id,
						   minor_id,
						   MB_REMSET );
}

static remset_t *
create_labelled_remset_with_owner_attrib
  ( int tbl_entries,    /* size of hash table, 0=default */
    int pool_entries,   /* size of remset, 0 = default */
    int major_id,       /* for stats */
    int minor_id,       /* for stats */
    unsigned owner_attrib
    )
{
  word *heapptr;
  remset_t *rs;
  remset_data_t *data;
  pool_t *p;

  assert( tbl_entries >= 0 && (tbl_entries == 0 || ilog2( tbl_entries ) != -1));
  assert( pool_entries >= 0 );

  if (pool_entries == 0) pool_entries = DEFAULT_REMSET_POOLSIZE;
  if (tbl_entries == 0) tbl_entries = DEFAULT_REMSET_TBLSIZE;

  annoyingmsg( "Allocated remembered set\n  hash=%d pool=%d",
	       tbl_entries, pool_entries );

  rs   = (remset_t*)must_malloc( sizeof( remset_t ) );
  data = (remset_data_t*)must_malloc( sizeof( remset_data_t ) );

  while(1) {
    heapptr = gclib_alloc_rts( tbl_entries*sizeof(word), 
			       owner_attrib );
    if (heapptr != 0) break;
    memfail( MF_RTS, "Can't allocate table and SSB for remembered set." );
  }

  /* Hash table */
  data->tbl_bot = heapptr;
  heapptr += tbl_entries;
  data->tbl_lim = heapptr;

  /* Node pool */
  p = allocate_pool_segment( pool_entries, data->mem_attribute ); /* XXX */
  data->first_pool = data->curr_pool = p;
  assert( data->curr_pool != 0 );
  data->numpools = 1;

  /* Misc */
  memset( &data->stats, 0, sizeof( data->stats ));
  data->pool_entries = pool_entries;
  data->self = stats_new_remembered_set( major_id, minor_id );
  data->mem_attribute = owner_attrib;

  rs->live = 0;
  rs->has_overflowed = FALSE;
  rs->data = data;

  rs_clear( rs );

  return rs;
}

static void rs_clear_opt( remset_t *rs, bool use_recycle_pool )
{
  remset_data_t *data = DATA(rs);
  word *p;
  int i;

  supremely_annoyingmsg( "REMSET @0x%p: clear", (void*)rs );

  /* Clear hash table */
  for ( p=data->tbl_bot, i=data->tbl_lim-data->tbl_bot ; i > 0 ; p++, i-- )
    *p = (word)(word*)0;

  /* Clear pools */
  data->first_pool->top = data->first_pool->bot;
  data->curr_pool = data->first_pool;
  if (use_recycle_pool) {
    assert( recycled_pool == NULL );
    assert( recycled_pool_entries_per <= 0 );
    recycled_pool = data->first_pool->next;
    recycled_pool_entries_per = data->pool_entries;
  } else {
    free_pool_segments( data->first_pool->next, data->pool_entries );
  }
  data->first_pool->next = 0;

  rs->has_overflowed = FALSE;
  rs->live = 0;
  data->numpools = 1;
  data->stats.cleared++;
}

void rs_clear( remset_t *rs ) 
{
  rs_clear_opt( rs, FALSE );
}

void rs_recycle( remset_t *rs ) 
{
  rs_clear_opt( rs, TRUE );
}

void rs_empty_recycling() 
{
  assert( recycled_pool_entries_per > 0 );
  free_pool_segments( recycled_pool, recycled_pool_entries_per );
  recycled_pool = NULL;
  recycled_pool_entries_per = -1;
}

static void handle_overflow( remset_t *rs, unsigned recorded, word *pooltop ) 
{
  DATA(rs)->stats.recorded += recorded;
  rs->live += recorded;
  DATA(rs)->curr_pool->top = pooltop;
  
  annoyingmsg( "Remset @0x%p overflow, entries=%d", (void*)rs, rs->live);
  
  rs->has_overflowed = TRUE;
  if (DATA(rs)->curr_pool->next == 0) {
    
    if ( recycled_pool == NULL ) {
      DATA(rs)->curr_pool->next = 
        allocate_pool_segment( DATA(rs)->pool_entries, 
                               DATA(rs)->mem_attribute );
    } else {
      pool_t* p = recycled_pool;
      p->top = p->bot;
      p->next = 0;
      DATA(rs)->curr_pool->next = p;
      recycled_pool = p->next;
    }
    DATA(rs)->numpools++;
  }
  DATA(rs)->curr_pool = DATA(rs)->curr_pool->next;
  assert( DATA(rs)->curr_pool != 0 );
}

void rs_del_elem( remset_t *rs, word w ) 
{
  word mask, *tbl, *b, *pooltop, *poollim, tblsize, h;
  bool overflowed = FALSE;
  remset_data_t *data = DATA(rs);
  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;

  h = hash_object( w, mask );
  b = (word*)tbl[ h ];
  while (b != 0 && *b != w) {
    b = (word*)*(b+1);
  }
  if (b != 0) {
    *b = (word)(word*)0;
    rs->live -= 1;
  }
}

bool rs_add_elem_new( remset_t *rs, word w ) 
{
  word mask, *tbl, *b, *pooltop, *poollim, tblsize, h;
  bool overflowed = FALSE;
  remset_data_t *data = DATA(rs);

  assert2(! rs_isremembered( rs, w ));

  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;

  h = hash_object( w, mask );

  if (pooltop == poollim) {
    handle_overflow( rs, 0, pooltop );
    pooltop = data->curr_pool->top;
    poollim = data->curr_pool->lim;
    overflowed = TRUE;
  }
  *pooltop = w;
  *(pooltop+1) = tbl[h];
  tbl[h] = (word)pooltop;
  pooltop += 2;
  data->curr_pool->top = pooltop;
  data->curr_pool->lim = poollim;
  data->stats.recorded += 1;
  rs->live += 1;

  return overflowed;
}

/* Adds w to remset rs.  Returns true if rs overflowed when inserting w. */
bool rs_add_elem( remset_t *rs, word w ) 
{
  word mask, *tbl, *b, *pooltop, *poollim, tblsize, h;
  bool overflowed = FALSE;
  remset_data_t *data = DATA(rs);
  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;

  h = hash_object( w, mask );
  b = (word*)tbl[ h ];
  while (b != 0 && *b != w) 
    b = (word*)*(b+1);
  if (b == 0) {
    if (pooltop == poollim) {
      handle_overflow( rs, 0, pooltop );
      pooltop = data->curr_pool->top;
      poollim = data->curr_pool->lim;
      overflowed = TRUE;
    }
    *pooltop = w;
    *(pooltop+1) = tbl[h];
    tbl[h] = (word)pooltop;
    pooltop += 2;
    data->curr_pool->top = pooltop;
    data->curr_pool->lim = poollim;
    data->stats.recorded += 1;
    rs->live += 1;
  }

  return overflowed;
}

bool rs_add_elems_distribute( remset_t **remset, word *bot, word *top ) 
{
  word *p, *q, mask, *tbl, w, *b, *pooltop, *poollim, tblsize, h;
  remset_t *rs;
  int gno;
  bool added_word; 
  bool overflowed = FALSE;

  assert( WORDS_PER_POOL_ENTRY == 2 );

  p = bot;
  q = top;

  /* (The scan is down for historical reasons that no longer apply.) */

  while (q > p) {
    q--;
    w = *q;
    if ( is_fixnum(w) ) {
      /* fixnums in SSB log indicate which field was written to. */
      continue;
    }
    gno = gen_of(w);
    rs = remset[gno];
    overflowed |= rs_add_elem( rs, w );
  }

  return overflowed;
}

bool rs_add_elems_funnel( remset_t *rs, word *bot, word *top ) 
{
  word *p, *q, mask, *tbl, w, *b, *pooltop, *poollim, tblsize, h;
  int gno;
  bool added_word; 
  bool overflowed = FALSE;

  assert( WORDS_PER_POOL_ENTRY == 2 );

  p = bot;
  q = top;

  /* (The scan is down for historical reasons that no longer apply.) */

  while (q > p) {
    q--;
    w = *q;
    if ( is_fixnum(w) ) {
      /* fixnums in SSB log indicate which field was written to. */
      continue;
    }
    overflowed |= rs_add_elem( rs, w );
  }

  return overflowed;
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

  ps = DATA(rs)->first_pool;
  while (1) {
    p = ps->bot;
    q = ps->top;
    scanned_all += (q-p)/2;	/* Zero entries also */
    while (p < q) {
      if (*p != 0) {
#if !GCLIB_LARGE_TABLE		/* These attributes not defined then */
#ifndef NDEBUG2
	if ( (attr_of(*p) & (MB_ALLOCATED|MB_HEAP_MEMORY)) !=
	     (MB_ALLOCATED|MB_HEAP_MEMORY) ) {
	  assert2(attr_of(*p) & MB_ALLOCATED);
	  assert2(attr_of(*p) & MB_HEAP_MEMORY);
	}
#endif 
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
  DATA(rs)->stats.max_objs_scanned = 
    max( DATA(rs)->stats.max_objs_scanned, scanned );
  DATA(rs)->stats.words_scanned += word_count;
  DATA(rs)->stats.max_words_scanned =
    max( DATA(rs)->stats.max_words_scanned, word_count );
  DATA(rs)->stats.removed += removed_count;
  rs->live -= removed_count;
  DATA(rs)->stats.scanned++;
  supremely_annoyingmsg( "REMSET @0x%x: removed %d elements (total %d).", 
			 (word)rs, removed_count, 
			 DATA(rs)->stats.removed );
}

void rs_stats( remset_t *rs )
{
  remset_data_t *data = DATA(rs);

  data->stats.allocated = 
    (data->tbl_lim - data->tbl_bot) +
    (data->pool_entries*data->numpools*WORDS_PER_POOL_ENTRY);

  data->stats.used =
    (data->tbl_lim - data->tbl_bot) +
    data->pool_entries*(data->numpools-1)*WORDS_PER_POOL_ENTRY +
    (data->curr_pool->top - data->curr_pool->bot);

  data->stats.live = 
    (data->tbl_lim - data->tbl_bot) +
    rs->live;

  stats_add_remset_stats( data->self, &data->stats );
  memset( &data->stats, 0, sizeof( remset_stats_t ) );
}

bool rs_isremembered( remset_t *rs, word w )
{
  word mask, *tbl, *b, tblsize, h;
  remset_data_t *data = DATA(rs);

  assert( WORDS_PER_POOL_ENTRY == 2 );

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

static bool rs_pool_next_chunk( summary_t *this, word **start, word **lim, 
                                bool *duplicate_entries ) 
{
  pool_t *ps;
  word *p, *q;
  p = (word*) this->cursor1;
  q = (word*) this->cursor2;

  if (p < q) {
    assert( WORDS_PER_POOL_ENTRY == 2 );
    *start = p;
    *lim = p+1;
    *duplicate_entries = FALSE;

    /* set up next step of iteration */
    p += 2;
    this->cursor1 = p;
    /* if p has reached q, then move on to the next pool segment. */
    if (!(p < q)) {
      ps = (pool_t*) this->cursor3;
      while (ps != NULL) {
        p = ps->bot;
        q = ps->top;
        ps = ps->next;
        if (p < q) {
          this->cursor1 = p;
          this->cursor2 = q;
          this->cursor3 = ps;
          break;
        }
      }
    }
    return TRUE;
  } else {
    return FALSE;
  }
}

void rs_init_summary( remset_t *rs, int max_words_per_step, 
                      /* out parameter */ summary_t *s )
{
  pool_t *ps;
  word *p, *q;
  assert( max_words_per_step == -1 ); /* no support for incremental yet */
  summary_init( s, rs->live, &rs_pool_next_chunk );
  ps = DATA(rs)->first_pool;
  p = NULL;
  q = NULL;
  while (ps != NULL) {
    p = ps->bot;
    q = ps->top;
    ps = ps->next;
    if (p < q) 
      break;
  }
  s->cursor1 = p;
  s->cursor2 = q;
  s->cursor3 = ps;
}

static pool_t *allocate_pool_segment( unsigned pool_entries, unsigned attr )
{
  pool_t *p;
  word *heapptr;

  p = (pool_t*)must_malloc( sizeof( pool_t ) );

  while (1) {
    heapptr = gclib_alloc_rts( pool_entries*WORDS_PER_POOL_ENTRY*sizeof(word), 
			       attr );
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

/* FIXME: sum_sqrcount is declared and incremented but never used. */

void rs_describe_self( remset_t *rs ) 
{
  word *tbl, *b, mask, h; 
  int tblsize, count, maxcount, sum_count, sum_sqrcount, num_nonzero, i;
  remset_data_t *data = DATA(rs);

#define HIST_LEN 500
  int counthist[HIST_LEN];

  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize-1;
  h = 0;
  consolemsg("TBL%d tblsize: %d", rs->identity, tblsize);

  maxcount = -1;
  sum_count = 0;
  sum_sqrcount = 0;
  num_nonzero = 0;
  for( i = 0; i < HIST_LEN; i++ ) {
    counthist[i] = 0;
  }
  while ( h < tblsize ) {
    count = 0;
    b = (word*)tbl[ h ];
    while ( b != NULL ) {
      count++;
      b = (word*)*(b+1);
    }
    sum_count += count;
    sum_sqrcount += (count*count);
    if (count > 0) {
      assert( count < HIST_LEN );
      counthist[count] += 1;
      num_nonzero += 1;
    }
    if (count > maxcount)
      maxcount = count;
    h++;
  }
  consolemsg("tblsize: %d entries: %d sum_count: %d mean: %d max: %d\n", 
             tblsize, num_nonzero, sum_count, sum_count/num_nonzero, maxcount);
  for( i = 0; i < HIST_LEN; i++ ) {
    if (counthist[i] != 0)
      consolemsg("  #%d : %d", i, counthist[i]);
  }
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

#endif

/* eof */

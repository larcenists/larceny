/* Copyright 2010 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Location set implementation.
 *
 * See locset_t.h header file for design notes that determined the API
 * structure.
 *
 * See remset.c for the basic ideas about the hashtable implementation
 * (much of the code there was used as the basis for this code).  Felix
 * did decide to deviate when it made sense (e.g. he sees no reason to
 * continue attempting to support the low-level node pool when no one has
 * ever followed through on the optimization to scan more than one
 * object at a time, documented in comments at top of remset.c).
 */
#include "larceny.h"
#include "locset_t.h"

#include "gclib.h"

typedef struct pool pool_t;
typedef struct ent ent_t;
typedef struct locset_data locset_data_t;

/* consider adding some preprocessor magic to allow easy switching
 * between explicit <obj+offset> storage and direct slot storage... */
struct ent { 
  loc_t  loc;
  ent_t *next;
};

struct pool {
  ent_t  *bot;
  ent_t  *top;
  ent_t  *lim;
  pool_t *next;
};

struct locset_data {
  ent_t          **tbl_bot;      /* Hash table bottom */
  ent_t          **tbl_lim;      /* Hash table limit */
  pool_t         *first_pool;	/* Pointer to first pool */
  pool_t         *curr_pool;    /* Pointer to current pool */
  int            pool_entries;  /* Number of entries in a pool */
  int            numpools;      /* Number of pools */
  unsigned       mem_attribute; /* Attr identifying which Rts part owns mem */
};

#define DATA(ls)                ((locset_data_t*)(ls->data))
#define hash_object( w, mask )  (((w) >> 4) & (mask))

static int    ilog2( unsigned n );
static pool_t *allocate_pool_segment( unsigned entries, unsigned attr );
static void   free_pool_segments( pool_t *first, unsigned entries );
static void   handle_overflow( locset_t *rs, ent_t *pooltop );

static locset_t *
create_locset_with_owner_attrib( int tbl_entries, int pool_entries, 
                                 unsigned owner_attrib );

locset_t *create_locset( int tbl_entries, int pool_entries )
{
  return create_locset_with_owner_attrib( tbl_entries, 
                                          pool_entries,
					  MB_SUMMARY_SETS );
}

static locset_t *
create_locset_with_owner_attrib( int tbl_entries, int pool_entries, 
                                 unsigned owner_attrib ) 
{
  ent_t **heapptr;
  locset_t *ls;
  locset_data_t *data;
  pool_t *p;

  assert( tbl_entries >= 0 && (tbl_entries == 0 || ilog2( tbl_entries ) != -1));
  assert( pool_entries >= 0 );

  if (pool_entries == 0) pool_entries = DEFAULT_LOCSET_POOLSIZE;
  if (tbl_entries == 0) tbl_entries = DEFAULT_LOCSET_TBLSIZE;

  ls   = (locset_t*)must_malloc( sizeof( locset_t ) );
  data = (locset_data_t*)must_malloc( sizeof( locset_data_t ) );

  while(1) {
    heapptr = gclib_alloc_rts( tbl_entries*sizeof(word), owner_attrib );
    if (heapptr != 0) break;
    memfail( MF_RTS, "Can't allocate table for location set." );
  }

  data->tbl_bot = heapptr;
  data->tbl_lim = heapptr + tbl_entries;

  p = allocate_pool_segment( pool_entries, owner_attrib );
  data->first_pool=  data->curr_pool = p;
  assert( data->curr_pool != 0 );
  data->numpools = 1;

  data->pool_entries = pool_entries;
  data->mem_attribute = owner_attrib;

  ls->live = 0;
  ls->data = data;
  
  ls_clear( ls );

  return ls;
}

void ls_clear( locset_t *ls )
{
  locset_data_t *data = DATA(ls);
  ent_t **p;
  int i;

  /* Clear hash table */
  for ( p=data->tbl_bot, i=data->tbl_lim-data->tbl_bot; i > 0; p++, i-- )
    *p = (ent_t*)NULL;

  /* Clear pools. */
  data->first_pool->top = data->first_pool->bot;
  data->curr_pool = data->first_pool;
  free_pool_segments( data->first_pool->next, data->pool_entries );
  data->first_pool->next = NULL;

  ls->live = 0;
  data->numpools = 1;
}

void ls_add_loc( locset_t *ls, loc_t loc );

void ls_add_obj_offset( locset_t *ls, word objptr, int offset)
{
  word *w = (word*)((char*)ptrof(objptr)+offset);

  bool overflowed;
  word mask;
  ent_t **tbl;
  ent_t *b;
  ent_t *pooltop, *poollim;
  int tblsize;
  word h;
  locset_data_t *data = DATA(ls);

  loc_t loc;

  assert( (offset % sizeof(word)) == 0 );

  loc = make_loc( objptr, offset );
  assert2( w == loc_to_slot(loc ));

  ls_add_loc( ls, loc );
}

void ls_add_loc( locset_t *ls, loc_t loc ) 
{
  word *w = loc_to_slot(loc);

  bool overflowed;
  word mask;
  ent_t **tbl;
  ent_t *b;
  ent_t *pooltop, *poollim;
  int tblsize;
  word h;
  locset_data_t *data = DATA(ls);
  
  overflowed = FALSE;
  FIXME_UNUSED_VARIABLE(overflowed);
  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize - 1;

  h = hash_object( (word)w, mask );
  if (0) consolemsg("ls_add_obj_offset hash_object(w=0x%08x, tblsize-1=%d) => %d", w, mask, h);
  b = tbl[ h ];
  while (b != NULL && loc_to_slot(b->loc) != w)
    b = b->next;

  if (b == NULL) {
    if (pooltop == poollim) {
      handle_overflow( ls, pooltop );
      pooltop = data->curr_pool->top;
      poollim = data->curr_pool->lim;
      overflowed = TRUE;
      if (0) consolemsg("ls_add_obj_offset insert overflow; pooltop: 0x%08x poollim: 0x%08x", pooltop, poollim);
    } else {
      if (0) consolemsg("ls_add_obj_offset insert withroom; pooltop: 0x%08x poollim: 0x%08x", pooltop, poollim);
    }
    pooltop->loc = loc;
    pooltop->next = tbl[h];
    tbl[h] = pooltop;
    pooltop += 1;
    data->curr_pool->top = pooltop;
    data->curr_pool->lim = poollim;
    ls->live += 1;
  } else {
    /* already present in table; do nothing */
  }
}

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

void ls_add_nonpair( locset_t *ls, word *ptr )
{
  word *hdr_search;
  loc_t loc;
  word obj;
  int offset;

  hdr_search = ptr;
  do {
    hdr_search -= 1;
    if (ishdr(*hdr_search)) {
      break;
    }
  } while (1);

  obj = retagptr((word)hdr_search);
  offset = ((byte*)ptr) - ((byte*)hdr_search);
  loc = make_loc( obj, offset );
  assert( loc_to_slot(loc) == ptr );
  ls_add_obj_offset( ls, obj, offset );
}

void ls_add_paircar( locset_t *ls, word *loc )
{
  ls_add_obj_offset( ls, tagptr((word)loc, PAIR_TAG), 0 ); /* &car */
}

void ls_add_paircdr( locset_t *ls, word *loc )
{
  ls_add_obj_offset( ls, tagptr((word)(loc-1), PAIR_TAG), sizeof(word) ); /* &cdr */
}

bool ls_ismember_loc( locset_t *ls, loc_t loc )
{
  word mask;
  ent_t **tbl;
  ent_t *b;
  int tblsize;
  word h;
  locset_data_t *data = DATA(ls);

  /* Search hash table */
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize - 1;

  h = hash_object( (word)loc_to_slot(loc), mask );
  b = tbl[ h ];
  while (b != NULL && ! (loc_equal_p( b->loc, loc )))
    b = b->next;

  return (b != NULL);
}

bool ls_ismember( locset_t *ls, word *loc )
{
  word mask;
  ent_t **tbl;
  ent_t *b;
  int tblsize;
  word h;
  locset_data_t *data = DATA(ls);

  /* Search hash table */
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize - 1;

  h = hash_object( (word)loc, mask );
  b = tbl[ h ];
  while (b != NULL && loc_to_slot(b->loc) != loc) 
    b = b->next;

  return (b != NULL);
}

void ls_enumerate( locset_t *ls, 
                   bool (*scanner)(word *loc, void *data ),
                   void *data )
{
  pool_t *ps;
  ent_t *p, *q;
  unsigned removed_count;

  removed_count = 0;
  ps = DATA(ls)->first_pool;
  while (1) {
    p = ps->bot;
    q = ps->top;
    while (p < q) {
      if (! loc_clear_p( p->loc )) {
        if (! scanner( loc_to_slot(p->loc), data )) {
          clear_loc( &p->loc );
          removed_count += 1;
        }
      }
      p += 1;
    }
    if (ps == DATA(ls)->curr_pool)
      break;

    ps = ps->next;
  }

  ls->live -= removed_count;
  supremely_annoyingmsg( "LOCSET @0x%x: removed %d elements.", 
                         (word)ls, removed_count );
}

void ls_enumerate_locs( locset_t *ls, 
                        bool (*scanner)(loc_t loc, void *data ),
                        void *data )
{
  pool_t *ps;
  ent_t *p, *q;
  unsigned removed_count;

  removed_count = 0;
  ps = DATA(ls)->first_pool;
  while (1) {
    p = ps->bot;
    q = ps->top;
    while (p < q) {
      if (! loc_clear_p( p->loc )) {
        if (! scanner( p->loc, data )) {
          clear_loc( &p->loc );
          removed_count += 1;
        }
      }
      p += 1;
    }
    if (ps == DATA(ls)->curr_pool)
      break;

    ps = ps->next;
  }

  ls->live -= removed_count;
  supremely_annoyingmsg( "LOCSET @0x%x: removed %d elements.", 
                         (word)ls, removed_count );
}

static bool scan_add_loc( loc_t loc, void *data ) 
{
  locset_t *ls = (locset_t*)data;
  ls_add_loc( ls, loc );
  return TRUE;
}

void ls_copy_all_from( locset_t *ls, locset_t *source ) 
{
  ls_enumerate_locs( source, scan_add_loc, ls );
}

static bool ls_pool_next_chunk_locs( summary_t *this, loc_t **start, loc_t **lim,
                                     bool *duplicate_entries )
{
  pool_t *ps;
  ent_t *p, *q;

  p = (ent_t*) this->cursor1;
  q = (ent_t*) this->cursor2;

  if (p < q) {
    loc_t *one_loc_addr = &p->loc;
    *start = one_loc_addr;
    *lim   = one_loc_addr+1; /* a limit, not a loc */
    *duplicate_entries = FALSE;

    /* set up next step of iteration */
    p++;
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

void ls_init_summary( locset_t *ls, int max_words_per_step,
                      /* out parameter */ summary_t *s )
{
  pool_t *ps;
  ent_t *p, *q;
  assert( max_words_per_step == -1 ); /* no support for incremental yet */
  summary_init_locs_dispose( s, ls->live, &ls_pool_next_chunk_locs, NULL, NULL );
  ps = DATA(ls)->first_pool;
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

void ls_add_elems_funnel( locset_t *ls, word *bot, word *top )
{
  word *p, *q, w1, w2;

  /* format of SSB log is a series of word pair obj/offset entries. */
  assert( ((top - bot) % 2) == 0 );

  p = bot;
  q = top;
  while (p < q) {
    w1 = *p;
    p++;
    w2 = *p;
    p++;

    assert( is_ptr( w1 ));
    assert( is_fixnum( w2 ));

    /* fixnum representation corresponds to a byte-offset from object start, 
     * because its tag is two low-order zero bits. */
    ls_add_obj_offset( ls, w1, (int) w2 );
  }
}

#if 0
/* Adds w to locset ls.  Returns true if ls overflowed when inserting w. */
void ls_add_wordptr( locset_t *ls, word *w )
{
  bool overflowed;
  word mask;
  ent_t **tbl;
  ent_t *b;
  ent_t *pooltop, *poollim;
  int tblsize;
  word h;
  locset_data_t *data = DATA(ls);

  overflowed = FALSE;
  pooltop = data->curr_pool->top;
  poollim = data->curr_pool->lim;
  tbl = data->tbl_bot;
  tblsize = data->tbl_lim - tbl;
  mask = tblsize - 1;

  h = hash_object( (word)w, mask );
  b = tbl[ h ];
  while (b != NULL && loc_to_slot(b->loc) != w)
    b = b->next;

  if (b == NULL) {
    if (pooltop == poollim) {
      handle_overflow( ls, pooltop );
      pooltop = data->curr_pool->top;
      poollim = data->curr_pool->lim;
      overflowed = TRUE;
    }
    pooltop->loc = w;
    pooltop->next = tbl[h];
    tbl[h] = pooltop;
    pooltop += 1;
    data->curr_pool->top = pooltop;
    data->curr_pool->lim = poollim;
    ls->live += 1;
  } else {
    /* already present in table; do nothing */
  }
}
#endif 

static pool_t *allocate_pool_segment( unsigned pool_entries, unsigned attr )
{
  pool_t *p;
  ent_t *heapptr;

  p = (pool_t*)must_malloc( sizeof( pool_t ) );

  while (1) {
    heapptr = gclib_alloc_rts( pool_entries*sizeof(ent_t), attr );
    if (heapptr != 0) break;
    memfail( MF_RTS, "Can't allocate remset hash pool." );
  }

  p->bot = p->top = heapptr;

  /* XXX really felix?  is ptr arith that hard to grok? */
  p->lim = (ent_t*)(((byte*)heapptr) + pool_entries*sizeof(ent_t)); 

  p->next = 0;

  if (0) consolemsg( "allocate_pool_segment(%u, attr) "
              "=> 0x%08x{bot: 0x%08x, top: 0x%08x, lim: 0x%08x}",
              pool_entries, p, p->bot, p->top, p->lim );

  return p;
}

static void   free_pool_segments( pool_t *first, unsigned pool_entries )
{
  pool_t *tmp;

  while (first) {
    { pool_t *p = first;
      if (0) consolemsg( "free_pool_segments free"
                  " 0x%08x{bot: 0x%08x, top: 0x%08x, lim: 0x%08x}",
                  p, p->bot, p->top, p->lim ); }

    gclib_free( first->bot, pool_entries*sizeof(ent_t) );
    tmp = first;
    first = first->next;
    free( tmp );
  }
}

static void   handle_overflow( locset_t *rs, ent_t *pooltop )
{
  annoyingmsg( "Locset @0x%p overflow, entries=%d", (void*)rs, rs->live);
  if (DATA(rs)->curr_pool->next == 0) {
    DATA(rs)->curr_pool->next = 
      allocate_pool_segment( DATA(rs)->pool_entries, 
                             DATA(rs)->mem_attribute );
    DATA(rs)->numpools++;
  }
  DATA(rs)->curr_pool = DATA(rs)->curr_pool->next;
  assert( DATA(rs)->curr_pool != 0 );
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

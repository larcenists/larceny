/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * SRO -- 'standing room only'
 *
 * word sro( gc_t *gc, int p_tag, int h_tag, int limit )
 *
 * `p_tag' is -1 a valid pointer tag (PAIR_TAG, VEC_TAG, BVEC_TAG, PROC_TAG)
 * `h_tag' is -1 or a valid header subtag (a number in the range 0..7)
 * `limit' is -1 or greater than 0; 0 is invalid.
 *
 * The returned value is a tagged pointer to a vector that contains
 * all live objects of the type indicated by the p_tag/h_tag combination,
 * where the object has at least 1 and no more than `limit' references to
 * it from live objects.  Each such object will appear once in the vector.
 * A p_tag -1 denotes 'any object'.
 * A h_tag -1 denotes 'any object subject to pointertag constraints'.
 * A limit -1 denotes 'no limit'.
 *
 * If not enough memory can be allocated to hold the resulting vector,
 * then #f is returned (at least in principle -- the low-level allocator
 * may actually raise an exception).
 *
 * The algorithm executes a full trace of the heap, accumulating objects
 * and reference counts in an external table.  When the trace is done,
 * the table is sweeped and objects are selected based on reference
 * counts and type tags.  A second sweep of the table copies object pointers
 * into the result vector.
 *
 * The table is a page map that contains pointers to shadow pages for each
 * page in the heap with a live object on it (the table is built lazily).
 * Given a heap pointer w, a shadow object is created that contains w and
 * a reference count.
 */

#include <string.h>
#include "larceny.h"
#include "gc_t.h"
#include "gclib.h"

/* Using smaller pages may increase performance (or not). */
#define SMALLER_PAGES   1

#if SMALLER_PAGES

/* Override definitions from gclib.h; PAGEMASK, pageof(), and roundup_page()
 * will use these values.
 */
# undef PAGESIZE
# define PAGESIZE           512

# undef PAGESHIFT
# define PAGESHIFT          9

#endif  /* SMALLER_PAGES */

typedef struct sro_w_t sro_w_t;	/* A workspace node */
typedef struct sro_t sro_t;	/* The top-level table structure */
typedef struct sro_stack_t sro_stack_t;  /* A stack segment */

struct sro_w_t {
  char *memory;			/* Raw memory block */
  unsigned size;		/* Size in bytes */
  sro_w_t *next;		/* Next memory block */
};

struct sro_t {
  sro_w_t *workspace;		/* List of workspace nodes */
  word **buckets;		/* Single array of buckets */
  char *freep;			/* A pointer into a workspace block */
  int free;		        /* Free bytes in that block */
  sro_stack_t *stack;           /* Current stack segment */
  word *stkp;                   /* First free element */
  word *stkbot;                 /* Current stack segment lower limit */
  word *stklim;                 /* Current stack segment upper limit */
};

#define STACKSIZE  32767        /* Arbitrary */

struct sro_stack_t {
  word stack[ STACKSIZE ];
  sro_stack_t *prev;            /* Previous segment (older) */
  sro_stack_t *next;            /* Next segment (younger) */
};

static void sro_traverse( sro_t *tbl );
static int sro_mark( sro_t *tbl, word w );
static int sro_ok( word w, int n, int p_tag, int h_tag, int limit );
static void *sro_alloc( sro_t *tbl, unsigned n_bytes );
static void sro_free_all( sro_w_t *w );
static void sro_push( sro_t *tbl, word w );
static bool sro_pop( sro_t *tbl, word *wp );
static void sro_push_segment( sro_t *tbl );
static bool sro_pop_segment( sro_t *tbl );
static void sro_push_root( word *loc, void *data );

/* Makes this module independent of gclib */
static caddr_t sro_pagebase;

word sro( gc_t *gc, int p_tag, int h_tag, int limit )
{
  sro_t tbl;
  word *b;
  int i, j, cnt, pages;
  word *p, *x;
  caddr_t lowest, highest;

  if (h_tag != -1) h_tag = h_tag << 2;  /* Matches value of typetag() */

  gc_creg_set( gc, gc_creg_get( gc ) ); /* Flush the stack! */

  /* Setup workspace */
  gclib_memory_range( &lowest, &highest );
  sro_pagebase = lowest;
  pages = roundup_page( highest-lowest )/PAGESIZE;
  tbl.workspace = 0;
  tbl.freep = 0;
  tbl.free = 0;
  tbl.buckets = (word**)sro_alloc( &tbl, pages*sizeof(word*) );
  for ( i=0 ; i < pages ; i++ )
    tbl.buckets[i] = 0;
  tbl.stack = 0;
  sro_push_segment( &tbl );
  
  /* Phase 1: mark */
  gc_enumerate_roots( gc, sro_push_root, (void*)&tbl );
  sro_traverse( &tbl );

  /* Phase 2: filter and count */
  cnt = 0;
  for ( i=0 ; i < pages ; i++ )
    if (tbl.buckets[i] != 0)
      for ( b = tbl.buckets[i], j=0 ; j < PAGESIZE/sizeof(word) ; j+= 2 )
	if (b[j] != 0)
	  if (sro_ok( b[j], b[j+1], p_tag, h_tag, limit ))
	    cnt++;
	  else
	    b[j] = 0;

  /* Allocate result vector without GC. */
  x = gc_allocate( gc, (cnt+1)*sizeof(word), 1, 0 );
  *x = mkheader( cnt*sizeof(word), VECTOR_HDR );

  /* Phase 3: insert elements into vector */
  p = x+1;
  for ( i=0 ; i < pages ; i++ )
    if (tbl.buckets[i] != 0)
      for ( b = tbl.buckets[i], j=0 ; j < PAGESIZE/sizeof(word) ; j+= 2 )
	if (b[j] != 0)
	  *p++ = b[j];
  
  /* Free workspace */
  sro_free_all( tbl.workspace );

  return tagptr( x, VEC_TAG );
}

static void sro_push_root( word *loc, void *data )
{
  sro_push( (sro_t*)data, *loc );
}

static void sro_traverse( sro_t *tbl )
{
  unsigned n, i;
  word w;

  while (sro_pop( tbl, &w )) {
    if (sro_mark( tbl, w ) > 1) continue;  /* marked previously */

  switch (tagof( w )) {
  case PAIR_TAG :
      sro_push( tbl, pair_cdr( w ) );
      sro_push( tbl, pair_car( w ) );
      break;
  case VEC_TAG :
  case PROC_TAG :
    n = sizefield( *ptrof(w) ) / sizeof(word);
    for ( i=0 ; i < n ; i++ )
        sro_push( tbl, vector_ref( w, i ) );
    break;
  case BVEC_TAG :
    break;
  }
}
}

static void sro_push( sro_t *tbl, word w )
{
  if (!isptr( w )) return;
  
  if (tbl->stkp == tbl->stklim)
    sro_push_segment( tbl );
  *(tbl->stkp++) = w;
}

static bool sro_pop( sro_t *tbl, word *wp )
{
  if (tbl->stkp == tbl->stkbot)
    if (!sro_pop_segment( tbl )) return 0;

  *wp = *--tbl->stkp;
  return 1;
}

static void sro_push_segment( sro_t *tbl )
{
  sro_stack_t *sp;
  
  if (tbl->stack != 0 && tbl->stack->next != 0)
    sp = tbl->stack->next;
  else {
    sp = (sro_stack_t*)sro_alloc( tbl, sizeof(sro_stack_t) );
    sp->prev = tbl->stack;
    sp->next = 0;
  }
  
  tbl->stack = sp;
  tbl->stkbot = tbl->stack->stack;
  tbl->stklim = tbl->stack->stack+STACKSIZE;
  tbl->stkp = tbl->stkbot;
}

static bool sro_pop_segment( sro_t *tbl )
{
  if (tbl->stack->prev == 0) return 0;
  
  tbl->stack = tbl->stack->prev;
  tbl->stkbot = tbl->stack->stack;
  tbl->stklim = tbl->stack->stack+STACKSIZE;
  tbl->stkp = tbl->stklim;
  
  return 1;
}

static int sro_mark( sro_t *tbl, word w )
{
  int i;
  unsigned page, offs;
  word *t;

  page = pageof_pb( w, sro_pagebase );
  if (tbl->buckets[page] == 0) {
    t = tbl->buckets[page] = (word*)sro_alloc( tbl, PAGESIZE );
    for ( i=0 ; i < PAGESIZE/sizeof(word) ; i++ )
      t[i] = 0;
  }
  else
    t = tbl->buckets[page];
  offs = ((word)ptrof(w) & PAGEMASK) >> 2;   /* Doubleword index */
  t[offs] = w;
  return ++t[offs+1];
}

static int sro_ok( word w, int refs, int p_tag, int h_tag, int limit )
{
  if (p_tag == -1) goto lcheck;
  if (tagof( w ) != p_tag) return 0;
  if (p_tag == PAIR_TAG || h_tag == -1) goto lcheck;
  if (typetag(*ptrof(w)) != h_tag) return 0;
 lcheck:
  return limit == -1 || refs <= limit;
}

/* Memory management: a simple, growable region that is freed all at once. */

/* Memory management parameters */
#define BIG_BLOCK_CUTOFF    (2*PAGESIZE)
#define BLOCKSIZE           (1024*1024)
#define SRO_MEM_ATTRIB      MB_RTS_MEMORY

static void *sro_alloc( sro_t *tbl, unsigned n_bytes )
{
  void *ptr;
  char *p;
  sro_w_t *w;
  int k;

  n_bytes = roundup8( n_bytes );
  if (tbl->free < n_bytes) {
    if (n_bytes > BIG_BLOCK_CUTOFF) {
      /* Allocate a block for this request alone, and return */
      n_bytes = roundup_page( n_bytes );
      p = gclib_alloc_rts( n_bytes, SRO_MEM_ATTRIB );
      w = sro_alloc( tbl, sizeof( sro_w_t ) );
      w->memory = (char*)p;
      w->size = n_bytes;
      w->next = tbl->workspace;
      tbl->workspace = w;
      return (void*)p;
    }

    /* Allocate a new block, abandon the current */
    p = gclib_alloc_rts( BLOCKSIZE, SRO_MEM_ATTRIB );
    w = (sro_w_t*)p;
    k = roundup8( sizeof( sro_w_t ) );
    w->memory = p;		/* [sic] */
    w->size = BLOCKSIZE;
    w->next = tbl->workspace;
    tbl->workspace = w;
    tbl->freep = p+k;
    tbl->free = BLOCKSIZE-k;
  }
  ptr = (void*)tbl->freep;
  tbl->freep += n_bytes;
  tbl->free -= n_bytes;
  assert( tbl->free >= 0 );
  return ptr;
}

/* This is recursive to allow the stack to serve as an unbounded temporary
 * variable.  It's important to copy out all memory/size fields before
 * freeing anything, because the memory about to be freed is used for the
 * linked list itself!
 */
static void sro_free_all( sro_w_t *w )
{
  if (w != 0) {
    void *m = w->memory;
    int s = w->size;
    sro_free_all( w->next );
    gclib_free( m, s );
  }
}

/* eof */

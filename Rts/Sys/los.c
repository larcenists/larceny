/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- large object space.
 *
 * A large object is a Scheme object that is allocated in a dedicated set
 * of pages.  Large objects have a pre-header consisting of the four words
 * before the normal object header.  The header contains a size field, a
 * pointer to a previous object, and a pointer to a next object.  The linked
 * list of objects is doubly linked, circular, with a header node.
 */

#include "larceny.h"
#include "los_t.h"
#include "gclib.h"
#include "gc.h"			/* For MAX_GENERATIONS */

#define HEADER_WORDS     4	/* Number of header words */
#define HEADER_UNUSED    -4     /* Unused field (could be mark?) */
#define HEADER_SIZE      -3     /* Offset of size field */
#define HEADER_NEXTP     -2	/* Offset of 'next' pointer */
#define HEADER_PREVP     -1	/* Offset of 'previous' pointer */

#define size( x )         (((int*)(x))[ HEADER_SIZE ])
#define next( x )         (((word**)(x))[ HEADER_NEXTP ])
#define prev( x )         (((word**)(x))[ HEADER_PREVP ])

#define set_size( a, b )  size(a)=b
#define set_next( a, b )  next(a)=b
#define set_prev( a, b )  prev(a)=b

struct los_list {
  word *header;		/* Points to the word following the header node */
  int  bytes;		/* Total number of allocated bytes */
};

static los_list_t *make_los_list( void );
static void remove( word *w );
static void insert_at_end( word *w, los_list_t *list );
static void set_generation_number( los_list_t *list, int gen_no, bool clear );
static void append_and_clear( los_list_t *left, los_list_t *right );
static void dump_list( los_list_t *l, char *tag, int nbytes );
static void clear_list( los_list_t *l );

los_t *create_los( int generations )
{
  los_t *los;
  int i;

  assert( generations > 0 );

  los = (los_t*)must_malloc( sizeof( los_t ) );
  los->generations = generations;
  los->object_lists =
    (los_list_t**)must_malloc( generations*sizeof( los_list_t* ) );
  for ( i=0 ; i < generations ; i++ )
    los->object_lists[i] = make_los_list();
  los->mark1 = make_los_list();
  los->mark2 = make_los_list();

  return los;
}

los_list_t *create_los_list(void)
{
  return make_los_list();
}

void los_free_list( los_list_t *list )
{
  clear_list( list );
  free( list->header - HEADER_WORDS );
  free( list );
}

int los_bytes_used( los_t *los, int gen_no )
{
  assert( 0 <= gen_no && gen_no < los->generations 
	  || gen_no == LOS_MARK1
	  || gen_no == LOS_MARK2 );

  if (gen_no == LOS_MARK2)
    return los->mark2->bytes;
  else if (gen_no == LOS_MARK1)
    return los->mark1->bytes;
  else
    return los->object_lists[gen_no]->bytes;
}

word *los_allocate( los_t *los, int nbytes, int gen_no )
{
  word *w;
  int size;

  assert( 0 <= gen_no && gen_no < los->generations && nbytes > 0 );

  size = roundup_page( nbytes + sizeof(word)*HEADER_WORDS );
  w = gclib_alloc_heap( size, gen_no );
  gclib_add_attribute( w, size, MB_LARGE_OBJECT );

  w += HEADER_WORDS;
  set_size( w, size );
  insert_at_end( w, los->object_lists[ gen_no ] );

  supremely_annoyingmsg( "{LOS} Allocating large object size %d at 0x%p", 
			 size, w );

  return w;
}

bool los_mark( los_t *los, los_list_t *marked, word *w, int gen_no )
{
  word *p = prev( w );

  /* assert( w is the address of a live large object ); */

  if (p == 0)
    return 1;	/* Already marked and moved */

  assert( ishdr( *w ) );
  remove( w );
  los->object_lists[ gen_no ]->bytes -= size( w );
  /* marked->bytes += size( w );  WRONG! -- insert_at_end does this too */
  insert_at_end( w, marked );
  set_prev( w, 0 );
  return 0;
}

void los_sweep( los_t *los, int gen_no )
{
  word *p, *n, *h;
  int nbytes;

  assert( 0 <= gen_no && gen_no < los->generations );

  h = los->object_lists[gen_no]->header;
  p = next( h );
  while ( p != h ) {
    n = next( p );
    remove( p );
    nbytes = size( p );
    gclib_free( p - HEADER_WORDS, nbytes );
    supremely_annoyingmsg( "{LOS} Freeing large object %d bytes at 0x%p",
			   nbytes, (void*)p );
    p = n;
  }
  clear_list( los->object_lists[ gen_no ] );
}

/* Appending a mark list implies cleaning up the gc marks (the prev() 
   pointers), so we always do that.  It causes no harm if the list is
   not a mark list.
   */
void los_append_and_clear_list( los_t *los, los_list_t *l, int to_gen )
{
  assert( 0 <= to_gen && to_gen < los->generations );

  set_generation_number( l, to_gen, TRUE );
  append_and_clear( los->object_lists[ to_gen ], l );
}

void los_list_set_gen_no( los_list_t *list, int gen_no )
{
  set_generation_number( list, gen_no, FALSE );
}

word *los_walk_list( los_list_t *list, word *p )
{
  word *n;

  /* assert( p==0 or p is on the list ); */

  if (p == 0)
    p = list->header;

  n = next( p );
  if (n == list->header )
    return 0;
  else {
    assert( ishdr( *n ) );
    return n;
  }
}

void los_permute_object_lists( los_t *los, int permutation[] )
{
  los_list_t *old[ MAX_GENERATIONS ];
  int i, k;

  for ( i=0 ; i < los->generations ; i++ )
    old[i] = los->object_lists[i];

  for ( i=0 ; i < los->generations ; i++ ) {
    k = permutation[i];
    los->object_lists[k] = old[i];
    los_list_set_gen_no( los->object_lists[k], k );
  }
}

static los_list_t *make_los_list( void )
{
  los_list_t *list;

  list = (los_list_t*)must_malloc( sizeof( los_list_t ) );
  list->header = (word*)must_malloc( HEADER_WORDS*sizeof(word) )+HEADER_WORDS;
  set_size( list->header, 0 );
  clear_list( list );

  return list;
}

static void remove( word *w )
{
  word *n = next( w );
  word *p = prev( w );

  set_next( p, n );
  set_prev( n, p );
  set_next( w, 0 );
  set_prev( w, 0 );
}

/* Note that in this case, for any n != h, prev(n) may be invalid. */

static void insert_at_end( word *w, los_list_t *list )
{
  word *h, *last;

  h = list->header;
  last = prev( h );
  set_next( last, w );         /* add links from last end */
  set_prev( w, last );
  set_next( w, h );            /* add links from header */
  set_prev( h, w );
  list->bytes += size( w );
}

static void set_generation_number( los_list_t *list, int gen_no, bool clear )
{
  word *header, *this, *prev;

  header = list->header;
  this = next( header );
  prev = header;
  while (this != header) {
    gclib_set_generation( this - HEADER_WORDS, size( this ), gen_no );
    if (clear) 
      set_prev( this, prev );
    prev = this;
    this = next( this );
  }
}

static void append_and_clear( los_list_t *left, los_list_t *right )
{
  word *left_first = next( left->header );
  word *left_last = prev( left->header );
  word *right_first = next( right->header );
  word *right_last = prev( right->header );

  if (right_first == right->header) return;   /* Right is empty */

  /* Splice in the right list */
  if (left_first != left->header) {           /* Left is nonempty  */
    set_next( left_last, right_first );       /* Join lists */
    set_prev( right_first, left_last );
  }
  else {			              /* Left is empty */
    set_next( left->header, right_first );    /* Move right to left */
    set_prev( right_first, left->header );
  }

  /* Complete circle */
  set_next( right_last, left->header );
  set_prev( left->header, right_last );

  left->bytes += right->bytes;
  clear_list( right );
}

/* Dump the list during a forward walk, and compute sizes forwards and
   backwards.  Useful during debugging.
   WARNING: don't run this on a mark list during GC because the prev() 
   pointers are not right during GC.
   */
static void dump_list( los_list_t *l, char *tag, int nbytes )
{
  word *p;
  int fwd_n, fwd_size, backwd_n, backwd_size;

  consolemsg( "{LOS} list dump %s for %d bytes", tag, nbytes );
  consolemsg( "{LOS}   header at 0x%p", l->header - HEADER_WORDS );
  fwd_n = fwd_size = 0;
  for ( p = next( l->header ) ; p != l->header ; p = next( p ) ) {
    consolemsg( "{LOS}   > %d bytes at 0x%p", size( p ), p - HEADER_WORDS );
    fwd_n++;
    fwd_size += size( p );
  }
  backwd_size = 0;
  backwd_n = 0;
  for ( p = prev( l->header ) ; p != l->header ; p = prev( p ) ) {
    backwd_n++;
    backwd_size += size( p );
  }
  consolemsg( "{LOS}   l->bytes=%d, fwd=%d/%d, backwd=%d/%d",
	      l->bytes, fwd_n, fwd_size, backwd_n, backwd_size );
  if (fwd_size != l->bytes || backwd_size != l->bytes || fwd_n != backwd_n)
    consolemsg( "{LOS}    WARNING: sizes computed differently!" );
}

static void clear_list( los_list_t *l )
{
  set_next( l->header, l->header );
  set_prev( l->header, l->header );
  l->bytes = 0;
}

/* eof */

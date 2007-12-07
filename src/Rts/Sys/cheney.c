/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id$
 *
 * Larceny -- copying garbage collector library for stopcopy and
 * generational GGs.  Extensions in cheney-*.c files add support for
 * non-predictive (ROF) and splitting collectors, though many
 * artifacts from supporting those extensions remain as idiosyncrasies
 * in the code in this file.
 * 
 * Entry points (from outside cheney-* files):
 *   gclib_stopcopy_promote_into 
 *   gclib_stopcopy_collect 
 *   gclib_stopcopy_collect_and_scan_static
 * 
 */

#define GC_INTERNAL

#ifdef UNIX
# include <sys/time.h>
#endif
#include <string.h>
#include "larceny.h"
#include "memmgr.h"
#include "gc_t.h"
#include "semispace_t.h"
#include "los_t.h"
#include "static_heap_t.h"
#include "gclib.h"
#include "barrier.h"
#include "stats.h"
#include "cheney.h"

/* Forwarding macros for normal copying collection and promotion.

   Forw_oflo() forwards the contents of a location after obtaining
   enough memory for the copy, if needed.

   Forw_oflo_record() is like forw_oflo() but is used during remembered-set
   scanning.  In addition to forwarding a location in a remembered object,
   it remembers whether any locations in the object were changed.
 
   Forw_core() implements the meat of the forwarding operation.

   check_space_expand() checks whether the semispace has room 
   for 'wanted+wiggle' bytes, and if not, it expands the semispace 
   and updates the 'dest' and 'lim' variables.
   - The wiggle parameter is to differentiate the wanted amount from
     the object's actual size, which affects whether an object is
     allocated in the large object space.
   - The uses of check_space in the macros are performed via the
     check_spaceI macro parameter, so that client code can override
     the behavior and instantiate a different policy for handling
     space exhaustion (such as allocating a new region).

   Scan_core() implements the semispace scanning operation.  It is
   parameterized by an expression that calls the appropriate forwarding macro.

   Remset_scanner_core() scans a single remembered set entry.  It is
   parameterized by an expression that calls the appropriate forwarding macro.
   */

/* Assumes that all parameters except forw_limit_gen are lvalues whose
   evaluation has no side effect and whose value will not change in ways
   not controlled by the macro.  forw_limit_gen is an expression with no
   side effect.
   */
#define forw_oflo( loc, forw_limit_gen, dest, lim, e, check_spaceI )            \
  do { word T_obj = *loc;                                                       \
       if (isptr(T_obj) && gen_of(T_obj) < (forw_limit_gen)){                   \
          forw_core( T_obj, loc, dest, lim, e, (forw_limit_gen), check_spaceI); \
       }                                                                        \
  } while( 0 )

/* Old_obj_gen is the generation of the object being scanned.
   Note that if the object being scanned is in the generation being collected,
   which happens during a promotion into that generation, then some objects
   will be believed to have intergenerational pointers even though they
   don't, because T_obj_gen is computed only once.  This is OK, because
   the collectors always clear the required remembered sets after a promotion.
   The alternative, to recompute T_obj_gen after forwarding, is more
   expensive.  Since the remembered set should be cleaned anyway to 
   reset pointers and so on, I forego the expense of fixing this macro.
   Same goes for forw_np_record.
   */
#define forw_oflo_record( loc, forw_limit_gen, dest, lim, has_intergen_ptr, \
                          old_obj_gen, e, check_spaceI )                    \
  do { word T_obj = *loc;                                                   \
       if (isptr( T_obj )) {                                                \
          unsigned T_obj_gen = gen_of(T_obj);                               \
          if (T_obj_gen < (forw_limit_gen)) {                               \
            forw_core( T_obj, loc,dest, lim, e, (forw_limit_gen),           \
                       check_spaceI );                                      \
          }                                                                 \
          if (T_obj_gen < old_obj_gen) has_intergen_ptr=1;                  \
       }                                                                    \
  } while( 0 )

#define FORW_PAIR( TMP_P, loc, dest, lim, e, forw_limit_gen, check_spaceI ) \
  do {                                                                 \
    word next_obj;                                                     \
    check_spaceI(dest,lim,8,0,e);                                           \
    *dest = *TMP_P;                                                    \
    *(dest+1) = next_obj = *(TMP_P+1);                                 \
    check_address( TMP_P );                                            \
    *TMP_P = FORWARD_HDR;                                              \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);                  \
    check_memory( dest, 2 );                                           \
    dest += 2;                                                         \
  } while ( 0 )

#define forw_core( T_obj, loc, dest, lim, e, forw_limit_gen, check_spaceI ) \
  word *TMP_P = ptrof( T_obj );                         \
  word TMP_W = *TMP_P;                                  \
  if (TMP_W == FORWARD_HDR)                             \
    *loc = *(TMP_P+1);                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                \
    FORW_PAIR( TMP_P, loc, dest, lim, e, forw_limit_gen, check_spaceI ); \
  }                                                     \
  else {                                                \
    word *TMPD;                                         \
    check_spaceI(dest,lim,sizefield(TMP_W)+4,8,e);       \
    TMPD = dest;                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD;     \
  }

/* Large objects must be handled here so we don't allocate space to
   handle them; by letting the check succeed for large objects, the
   subsequent call to forward() will handle the object properly.
   */
#define check_space_expand( dest, lim, wanted, wiggle, e )                   \
  if ((char*)lim-(char*)dest < (wanted+wiggle) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    expand_semispace( tospace_scan(e), &CS_LIM, &CS_DEST, (wanted+wiggle) ); \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

#define check_space_alloc( dest, lim, wanted, wiggle, e )                    \
  if ((char*)lim-(char*)dest < (wanted+wiggle) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    fresh_generation( e, &CS_LIM, &CS_DEST, (wanted+wiggle) );               \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

#define scan_and_forward( loc, iflush, gno, dest, lim, e, check_spaceI ) \
  scan_core( loc, iflush, forw_oflo( loc, gno, dest, lim, e, check_spaceI ) )

/* External */

extern void mem_icache_flush( void *start, void *end );

/* Private procedures */

static void scan_static_area( cheney_env_t *e );
static void root_scanner_oflo( word *addr, void *data );
static bool remset_scanner_oflo( word obj, void *data, unsigned *count );
static word forward_large_object( cheney_env_t *e, word *ptr, int tag );
static void
fresh_generation( cheney_env_t *e, word **lim, word **dest, unsigned bytes );

static const int tospaces_init_buf_size = 10;

/* FIXME: Lars clearly avoided invoking malloc/free within the
 * collector (but they of course trickle through, via expand_semispace
 * invocation of ss_expand). Felix would also like to continue this
 * practice; e.g. the spaces buffer could be maintained scross
 * collector invocations...
 */
static semispace_t**
begin_semispaces_buffer( int init_capacity ) 
{
  return (semispace_t**)
    must_malloc( sizeof( semispace_t* )*init_capacity );
}
static void
finis_semispaces_buffer( semispace_t** spaces, int capacity )
{
  free( spaces );
}

void gclib_stopcopy_promote_into( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  int init_size = tospaces_init_buf_size;
  
  spaces = begin_semispaces_buffer( init_size );
  spaces[0] = tospace;
  CHENEY_TYPE( 0 );
  init_env( &e, gc, spaces, 1, init_size, 
            0, tospace->gen_no, 0, scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );
  
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void gclib_stopcopy_collect( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  int init_size = tospaces_init_buf_size;

  spaces = begin_semispaces_buffer( init_size );
  spaces[0] = tospace;
  CHENEY_TYPE( 1 );
  init_env( &e, gc, spaces, 1, init_size, 
            0, tospace->gen_no+1, 0, scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void gclib_stopcopy_collect_and_scan_static( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  int init_size = tospaces_init_buf_size;

  spaces = begin_semispaces_buffer( init_size );
  spaces[0] = tospace;
  CHENEY_TYPE( 1 );
  init_env( &e, gc, spaces, 1, init_size, 
            0, tospace->gen_no+1, SCAN_STATIC, scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void sweep_large_objects( gc_t *gc, 
                                 int sweep_oldest, 
                                 int dest, 
                                 int dest2 )
{
  int i;

  for ( i=0 ; i <= sweep_oldest ; i++ )
    los_sweep( gc->los, i );
  los_append_and_clear_list( gc->los, gc->los->mark1, dest );
  if (dest2 >= 0) los_append_and_clear_list( gc->los, gc->los->mark2, dest2 );
}

void init_env( cheney_env_t *e, 
               gc_t *gc,
               semispace_t **tospaces,
               int tospaces_len,
               int tospaces_cap,
               semispace_t *tospace2,
               int effective_generation,
               int attributes,
               void (*scanner)( cheney_env_t * ) )
{
  memset( e, 0, sizeof( cheney_env_t ) );
  e->gc = gc;
  e->gclib_desc_g = gclib_desc_g;
  e->effective_generation = effective_generation;
  e->scan_static = attributes & SCAN_STATIC;
  e->splitting = attributes & SPLITTING_GC;
  e->iflush = gc_iflush( gc );
  e->tospaces = tospaces;
  e->tospaces_len = tospaces_len;
  e->tospaces_cap = tospaces_cap;
  assert( tospaces_len > 0 );
  e->tospaces_cur_scan = 0;
  e->tospaces_cur_dest = 0;
  e->tospace2 = tospace2;
  e->dest = tospace_dest(e)->chunks[tospace_dest(e)->current].top;
  e->dest2 = (tospace2 ? tospace2->chunks[tospace2->current].top : 0);
  e->lim = tospace_dest(e)->chunks[tospace_dest(e)->current].lim;
  e->lim2 = (tospace2 ? tospace2->chunks[tospace2->current].lim : 0);
  e->los = (e->splitting ? 0 : gc->los);

  e->scan_from_globals = root_scanner_oflo;
  e->scan_from_remsets = remset_scanner_oflo;

  e->scan_from_tospace = scanner;
}

void oldspace_copy( cheney_env_t *e )
{
  /* Setup */
  e->scan_idx = tospace_scan(e)->current;
  e->scan_idx2 = (e->tospace2 ? e->tospace2->current : 0);
  e->scan_ptr = tospace_scan(e)->chunks[e->scan_idx].top;
  e->scan_ptr2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].top : 0);
  e->scan_lim = tospace_scan(e)->chunks[e->scan_idx].lim;
  e->scan_lim2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].lim : 0);

  /* Collect */
  start( &cheney.root_scan_prom, &cheney.root_scan_gc );
  gc_enumerate_roots( e->gc, e->scan_from_globals, (void*)e );
  gc_enumerate_remsets_older_than( e->gc,
                                   e->effective_generation-1,
                                   e->scan_from_remsets,
                                   (void*)e,
                                   e->enumerate_np_remset );
  if (e->scan_static && e->gc->static_area)
    scan_static_area( e );
  stop();

  start( &cheney.tospace_scan_prom, &cheney.tospace_scan_gc );
  e->scan_from_tospace( e );
  stop();

  /* Shutdown */
  tospace_dest(e)->chunks[tospace_dest(e)->current].top = e->dest;
  if (e->tospace2)
    e->tospace2->chunks[e->tospace2->current].top = e->dest2;
  assert2( tospace_dest(e) == tospace_scan(e) );
  assert2( tospace_dest(e)->chunks[tospace_dest(e)->current].bot
           <= tospace_dest(e)->chunks[tospace_dest(e)->current].top );
}

static void scan_static_area( cheney_env_t *e )
{
  int         forw_limit_gen = e->effective_generation;
  semispace_t *s_data = e->gc->static_area->data_area;
  word        *dest = e->dest;
  word        *lim = e->lim;
  word        *loc, *limit;
  int         i;
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif
  
  for ( i=0 ; i <= s_data->current ; i++ ) {
    loc = s_data->chunks[i].bot;
    limit = s_data->chunks[i].top;
    while ( loc < limit )
      scan_and_forward( loc, e->iflush, forw_limit_gen, dest, lim, e, check_space_expand );
  }

  e->dest = dest;
  e->lim = lim;
}

static void root_scanner_oflo( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  forw_oflo( ptr, e->effective_generation, e->dest, e->lim, e, check_space_expand );
}

static bool remset_scanner_oflo( word object, void *data, unsigned *count )
{
  cheney_env_t *e = (cheney_env_t*)data;
  unsigned     forw_limit_gen = e->effective_generation;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *dest = e->dest;
  word         *lim = e->lim;
  word         *loc;            /* Used as a temp by scanner and fwd macros */

  remset_scanner_core( object, loc, 
                       forw_oflo_record( loc, forw_limit_gen, dest, lim,
                                         has_intergen_ptr, old_obj_gen, e, 
                                         check_space_expand ),
                       *count );

  e->dest = dest;
  e->lim = lim;
  return has_intergen_ptr;
}

void scan_oflo_normal( cheney_env_t *e )
{
  unsigned gno = e->effective_generation;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  word     *dest = e->dest;
  word     *copylim = e->lim;
  word     *los_p = 0, *p;
  int      morework;
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  do {
    morework = 0;

    while (scanptr != dest) {
      while (scanptr != dest && scanptr < scanlim) {
        scan_and_forward( scanptr, e->iflush, gno, dest, copylim, e, check_space_expand );
      }

      if (scanptr != dest) {
        e->scan_idx++;
        if (e->scan_idx == tospace_scan(e)->n) {
          e->tospaces_cur_scan++;
          assert(e->tospaces_cur_scan < e->tospaces_len);
          e->scan_idx = 0;
        }
        scanptr = tospace_scan(e)->chunks[e->scan_idx].bot;
        scanlim = tospace_scan(e)->chunks[e->scan_idx].lim;
        
        /* A corner case when we fill up all of the to-space chunk
         * (that is, when dest == copylim).  In this situation, dest
         * does not point to a valid location in to-space; it may be
         * pointing at the _next_ chunk that we would scan when the
         * scan_idx is incremented below, which leads to a premature
         * scan loop termination. */
        if (dest == copylim) {
          /* Set dest and copylim to values that we *know* cannot
           * alias the new scanptr. */
          dest = copylim = 0;
        }
      }
    }

    while ((p = los_walk_list( e->los->mark1, los_p )) != 0) {
      los_p = p;
      morework = 1;
      assert2( ishdr( *p ) );
      scan_and_forward( p, e->iflush, gno, dest, copylim, e, check_space_expand );
    }
  } while (morework);

  e->dest = dest;
  e->lim = copylim;
}

/* "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address.
 *
 * Most objects are smallish, so this code should be biased in favor
 * of small objects.
 */
word forward( word p, word **dest, cheney_env_t *e )
{
  word hdr, newptr, *p1, *p2, tag, *ptr;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* experimentally keeping bytevectors 4-word aligned;
   * insert padding when dest is only 2-word aligned. */
  if (tag == BVEC_TAG) {
    if ((((word)*dest) & 0xF) == 0x8) {
      p1 = *dest;
      *p1 = 0;
      p1++;
      *p1 = 0;
      p1++;
      *dest = p1;
    }
    assert((((word)*dest) & 0xF) == 0x0);
  }

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)p1;    /* really *dest, but the compiler is dumb. */
  p2 = ptr;

  hdr = *ptr;
  assert2( ishdr( hdr ) );

#if FORW_BY_LOOP
  { unsigned words;

    /* gcc gets this right, so no sense in being obscure.
       words = (((hdr >> 8) + 11) >> 3) << 1; */
    words = roundup8( sizefield( hdr ) + 4 ) / 4;

#if CHECK_EVERY_WORD
    switch (tag) {
    case VEC_TAG : case PROC_TAG :
      gclib_check_memory_validity( p2, (sizefield( hdr ) + 4)/4 );
    }
#endif
    /* 32 is pretty arbitrary; chosen to match overhead of memcpy(). */
    /* gcc doesn't schedule real well. */
    if (words < 32) {
      while (words > 0) {
        p1[0] = p2[0];
        p1 += 2;
        p1[-1] = p2[1];
        p2 += 2;
        words -= 2;
      }
    }
    else if (words > GC_LARGE_OBJECT_LIMIT/4 && e->los) 
      return forward_large_object( e, ptr, tag );
    else {
      memcpy( p1, p2, words*4 );
      p1 += words;
    }
    *dest = p1;
  }
#endif

#if FORW_BY_DUFF
  { unsigned bytes;

  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los) 
    return forward_large_object( e, ptr, tag );

  switch (bytes >> 3) {
    case 8  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 7  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 6  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 5  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 4  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 3  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 2  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 1  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 0  : break;
    default : memcpy( p1, p2, bytes );  p1 += (bytes >> 2);
  }
  *dest = p1;
  }
#endif

#if FORW_BY_MEMCPY
  { unsigned bytes;
  /* This appears to be slowest */
  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los)
    return forward_large_object( e, ptr, tag );

  memcpy( p1, p2, bytes );
  *dest = p1 + (bytes >> 2);
  }
#endif

  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  check_address( ptr );
  *ptr = FORWARD_HDR;
  *(ptr+1) = newptr;

  return newptr;
}


/* Expand the semispace by switching to a new chunk.  To make things
   simple for the scanning code, a bignum header is put into the
   heap at the current location (unless we're at the end of the chunk)
   with a length field that covers the rest of the chunk.  The scanning
   code will simply skip the data.  The bignum's first four bytes
   are initialized with a recognizable but unlikely pattern, in the event
   any code needs to grovel over the heap and count padding.

   A bignum header is used rather than a generic bytevector header since
   the latter value would cause the scanner to flush the icache for the
   garbage area.
   */
void seal_chunk( semispace_t *ss, word *lim, word *dest )
{
  if (dest < lim) {
    word len = (lim - dest)*sizeof(word);
    *dest = mkheader(len-sizeof(word),BIGNUM_HDR);
    *(dest+1) = 0xABCDABCD;
  }
  if (dest == NULL) {
    /* A NULL dest indicates that we exhausted the chunk; only happens
     * in scan_oflo_normal corner case where set dest = copylim = 0 */
    ss->chunks[ ss->current ].top = ss->chunks[ ss->current ].lim;
  } else {
    ss->chunks[ ss->current ].top = dest;
  }
  assert2( ss->chunks[ss->current].bot <= ss->chunks[ss->current].top );
}

void
expand_semispace( semispace_t *ss, word **lim, word **dest, unsigned bytes )
{
  seal_chunk( ss, *lim, *dest );
  ss_expand( ss, max( bytes, GC_CHUNK_SIZE ) );
  *lim = ss->chunks[ss->current].lim;
  *dest = ss->chunks[ss->current].top;
}

void
fresh_generation( cheney_env_t *e, word **lim, word **dest, unsigned bytes )
{
  semispace_t *ss;
  int gno;

  annoyingmsg("   fresh_generation( e, 0x%08x, 0x%08x, %d )", 
              *lim, *dest, bytes );

  ss = tospace_dest(e);
  seal_chunk( ss, *lim, *dest );

  ss = gc_fresh_space( e->gc );

  e->tospaces_cur_dest++;
  assert(e->tospaces_len == e->tospaces_cur_dest);

  /* FIXME: not an actual invariant; may need to realloc tospaces. */
  assert(e->tospaces_len < e->tospaces_cap); 

  e->tospaces[e->tospaces_len] = ss;
  e->tospaces_len++;
  *lim = ss->chunks[ss->current].lim;
  *dest = ss->chunks[ss->current].top;
}

/* FIXME: Note a problem with the following code.  When a large object is
   forwarded, its generation bits are not changed until after scanning
   is over.  That means that any code that checks the generation bits
   of a large object, eg like a GC write barrier might during scanning,
   will get the wrong generation: the number will be too low.
   That can be fixed here by using los_mark_and_set_generation()
   rather than just los_mark(), and allocating the object with the
   correct generation instead of that of the existing generation.
   I don't want to fix that now.  See similar, but fixed, code in
   the DOF collector.

   Note that the code that is sensitive to it, forw_np_partial,
   uses the test in such a way that the bug does not trigger incorrect
   behavior, though a large object might be added to the remembered
   set when it should not have been.
*/
static word forward_large_object( cheney_env_t *e, word *ptr, int tag )
{
  los_t *los = e->los;
  los_list_t *mark_list;
  word hdr, ret;
  int bytes, was_marked, sub1 = 0, sub2 = 0;

  hdr = *ptr;
  bytes = roundup8( sizefield( hdr ) + 4 );

#if CHECK_EVERY_WORD
    switch (tag) {
    case VEC_TAG : case PROC_TAG :
      gclib_check_memory_validity( ptr, (sizefield(hdr)+4)/4 );
      break;
    }
#endif
  mark_list = los->mark1;
  sub1 = bytes;
  if (e->np_promotion) {
#if 0                           /* Obsolete */
    int free1, free2;

    free1 = e->np.old_steps_remaining*GC_CHUNK_SIZE - 
      los_bytes_used( los, LOS_MARK1 );
    free2 = e->np.young_steps_remaining*GC_CHUNK_SIZE - 
      los_bytes_used(los, LOS_MARK2);

    if (bytes > free1 && bytes <= free2) {
      mark_list = los->mark2;
      sub2 = sub1; sub1 = 0;
    }
#else
    if (e->np.has_switched)
      mark_list = los->mark2;
#endif
  }

  if (attr_of(ptr) & MB_LARGE_OBJECT) {
    was_marked = los_mark( los, mark_list, ptr, gen_of(ptr) );
    ret = tagptr( ptr, tag );
  }
  else {
    /* The large object was not allocated specially, so we must move it. */
    word *new;

    hdr = *ptr;
    bytes = roundup8( sizefield( hdr ) + 4 );
    new = los_allocate( los, bytes, gen_of( ptr ) );
    memcpy( new, ptr, bytes );
    
    /* Must mark it also! */
    was_marked = los_mark( los, mark_list, new, gen_of( ptr ));
    
    /* Leave a forwarding pointer */
    check_address( ptr );
    *ptr = FORWARD_HDR;
    *(ptr+1) = tagptr( new, tag );
    ret = *(ptr+1);
  }

  if (e->np_promotion && !was_marked) {
    e->np.old_los_bytes += sub1;
    e->np.young_los_bytes += sub2;
    e->np.old_los_steps = ceildiv( e->np.old_los_bytes, GC_CHUNK_SIZE );
    e->np.young_los_steps = ceildiv( e->np.young_los_bytes, GC_CHUNK_SIZE );
  }
  return ret;
}

/* eof */

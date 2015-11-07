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
 *   gclib_stopcopy_collect_genset
 *   gclib_stopcopy_collect_locs
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
#include "gset_t.h"
#include "semispace_t.h"
#include "los_t.h"
#include "static_heap_t.h"
#include "gclib.h"
#include "barrier.h"
#include "stats.h"
#include "cheney.h"
#include "remset_t.h"
#include "seqbuf_t.h"
#include "msgc-core.h"
#include "smircy.h"
#include "smircy_internal.h"

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

/* Assumes that all parameters except fwdgens* are lvalues whose
   evaluation has no side effect and whose value will not change in ways
   not controlled by the macro.  
   (fwdgens,fwdgens_data) are pure expressions that produce 
   Exists X : ((Gen X) -> Bool) * X
   */
#define forw_oflo( ctxt, loc, fwdgens, fwdgens_data, dest, lim, e, check_spaceI ) \
  do { word T_obj = *loc;                                                     \
       if (isptr(T_obj) && fwdgens( gen_of(T_obj), (fwdgens_data))) {         \
          forw_core( T_obj, loc, dest, lim, e, check_spaceI);                 \
       }                                                                      \
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
#define forw_oflo_record_track_old2young( loc, fwdgens, fwdgens_data,       \
                                          dest, lim, has_intergen_ptr,      \
                                          old_obj_gen, e, check_spaceI )    \
  do { word T_obj = *loc;                                                   \
       if (isptr( T_obj )) {                                                \
          unsigned T_obj_gen = gen_of(T_obj);                               \
          if (fwdgens(T_obj_gen, fwdgens_data)) {                           \
            forw_core( T_obj, loc,dest, lim, e, check_spaceI );             \
          }                                                                 \
          if (T_obj_gen < old_obj_gen) has_intergen_ptr=1;                  \
       }                                                                    \
  } while( 0 )

#define forw_oflo_record_track_any2other( loc, fwdgens, fwdgens_data,       \
                                          dest, lim, has_intergen_ptr,      \
                                          old_obj_gen, e, check_spaceI )    \
  do { word T_obj = *loc;                                                   \
       if (isptr( T_obj )) {                                                \
          unsigned T_obj_gen = gen_of(T_obj);                               \
          if (fwdgens(T_obj_gen, fwdgens_data)) {                           \
            forw_core( T_obj, loc,dest, lim, e, check_spaceI );             \
          }                                                                 \
          if (gen_of(*loc) != old_obj_gen) has_intergen_ptr=1;              \
       }                                                                    \
  } while( 0 )

/* Installs a forwarding pointer to 'newaddr' with tag 'tag' at 'addr' */
static word install_fwdptr( word *addr, word *newaddr, word tag ) {
  /* factored routine; should later double check whether this is being
   * inlined, or if I should instead implemented it as a macro */
  word ret;
  check_address( addr );
  /* Proposed, currently unchecked invariant: 
   * Since object at addr is about to be forwarded, addr should not be
   * in any remembered sets.
   */
  ret = (word)tagptr( newaddr, tag );
  *addr = FORWARD_HDR;
  *(addr+1) = ret;
  return ret;
}

#define FORWARDED( e, ctxt, old_obj, old_gno, new_obj, new_gno, words ) \
  do {                                                                  \
    if (old_gno == 0) {                                                 \
      e->words_forwarded_from_nursery += words;                         \
    }                                                                   \
    if (e->forwarded) {                                                 \
      e->forwarded( e, ctxt, old_obj, old_gno, new_obj, new_gno );      \
    }                                                                   \
  } while( 0 )

#define FORW_PAIR( TMP_P, loc, dest, lim, e, check_spaceI ) \
  do {                                                                 \
    word next_obj;                                                     \
    word new_obj, old_obj; int new_gno, old_gno;                       \
    check_spaceI(dest,lim,8,0,e);                                      \
    *dest = *TMP_P;                                                    \
    *(dest+1) = next_obj = *(TMP_P+1);                                 \
    new_obj = install_fwdptr( TMP_P, dest, PAIR_TAG);                  \
    *loc = new_obj;                                                    \
    old_obj = tagptr( TMP_P, PAIR_TAG );                               \
    new_gno = gen_of( new_obj );   /* XXX gen_of slow? */              \
    old_gno = gen_of( old_obj );   /* XXX gen_of slow? */              \
    FORWARDED( e,"FORW_PAIR", old_obj,old_gno, new_obj,new_gno, 2 );   \
    check_memory( dest, 2 );                                           \
    dest += 2;                                                         \
  } while ( 0 )

#define forw_core( T_obj, loc, dest, lim, e, check_spaceI )                   \
  word *TMP_P = ptrof( T_obj );                                               \
  word TMP_W = *TMP_P;                                                        \
  if (TMP_W == FORWARD_HDR)                                                   \
    *loc = *(TMP_P+1);                                                        \
  else if (tagof( T_obj ) == PAIR_TAG) {                                      \
    FORW_PAIR( TMP_P, loc, dest, lim, e, check_spaceI );                      \
  }                                                                           \
  else {                                                                      \
    word *TMPD;                                                               \
    check_spaceI(dest,lim,sizefield(TMP_W)+4,8,e);                            \
    TMPD = dest;                                                              \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD;                           \
  }

/* Large objects must be handled here so we don't allocate space to
   handle them; by letting the check succeed for large objects, the
   subsequent call to forward() will handle the object properly.
   */
#define check_space_expand( dest, lim, wanted, wiggle, e )                   \
  if (((wanted <= GC_LARGE_OBJECT_LIMIT) &&                                  \
       (((char*)lim-(char*)dest) < ((wanted)+(wiggle))))                     \
      || (e->last_forward_was_large)) {                                      \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    expand_space( e, &CS_LIM, &CS_DEST, (wanted+wiggle) );                   \
    e->last_forward_was_large = FALSE;                                       \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

#define scan_and_forward( loc, iflush, fwdgens, fwdgens_data,                 \
                          dest, lim, e, check_spaceI )                        \
  scan_core( e, loc, iflush,                                                  \
             forw_oflo( "scan_and_forward forw_oflo", loc,                    \
                        fwdgens, fwdgens_data,                                \
                        dest, lim, e, check_spaceI ) )

#define scan_and_forward_update_rs( loc, iflush, fwdgens, fwdgens_data,       \
                                    dest, lim, e, check_spaceI )              \
  scan_update_rs( e, loc, iflush,                                             \
                  forw_oflo( "scan_and_forward_update_rs forw_oflo", loc,     \
                             fwdgens, fwdgens_data,                           \
                             dest, lim, e, check_spaceI ),                    \
                  update_remset )

/* External */

extern void mem_icache_flush( void *start, void *end );

/* Private procedures */

static void sweep_large_objects_in( gc_t *gc, gset_t genset );
static void scan_static_area( cheney_env_t *e );
static void scan_static_area_update_rs( cheney_env_t *e );
static void root_scanner_oflo( word *addr, void *data );
static bool remset_scanner_oflo( word obj, void *data );
static bool remset_scanner_oflo_update_rs( word obj, void *data );
static word forward_large_object( cheney_env_t * const e, word * const ptr, const int tag, const int tgt_gen );
static bool forward_lessthan( int gno, int gno_bound ) {
  return gno < gno_bound; }
static bool forward_nursery_and( int gno, gset_t gset ) { 
  return gno == 0 || gset_memberp( gno, gset ); }
static const int tospaces_init_buf_size = 10;

static void 
init_env_with_cursors( cheney_env_t *e, 
                       gc_t *gc,
                       semispace_t **tospaces,
                       int tospaces_len,
                       int tospaces_cap,
                       semispace_cursor_t *cursors, 
                       semispace_t *tospace2,
                       gset_t forw_gset,
                       int attributes,
                       void (*scanner)( cheney_env_t * ) );


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

static semispace_t**
enlarge_semispaces_buffer( semispace_t** spaces, int len, int new_capacity ) 
{
  int i;
  semispace_t** new_spaces = 
    (semispace_t**) must_malloc( sizeof( semispace_t* )*new_capacity );
  for( i=0; i < len; i++) {
    new_spaces[i] = spaces[i];
  }
  free( spaces );
  return new_spaces;
}

static void
finis_semispaces_buffer( semispace_t** spaces, int capacity )
{
  free( spaces );
}

static semispace_cursor_t* 
begin_semispace_cursors( int init_capacity ) 
{
  return (semispace_cursor_t*)
    must_malloc( sizeof( semispace_cursor_t )*init_capacity); 
}

static void 
finis_semispace_cursors( semispace_cursor_t *objs, int final_cap ) 
{
  free( objs ); 
}

static semispace_cursor_t* 
enlarge_semispace_cursors( semispace_cursor_t *oldobjs, int len, int new_cap ) 
{
  int i;
  semispace_cursor_t *newobjs = (semispace_cursor_t*) 
    must_malloc( sizeof( semispace_cursor_t )*new_cap );
  for( i=0 ; i < len; i++ ) { 
    newobjs[i].chunks_index = oldobjs[i].chunks_index;
    newobjs[i].chunk_ptr = oldobjs[i].chunk_ptr;
  }
  free( oldobjs ); 
  return newobjs; 
}

void gclib_stopcopy_collect_genset( gc_t *gc, gset_t gs, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  semispace_cursor_t *cursors;
  int init_size = tospaces_init_buf_size;
  
  spaces = begin_semispaces_buffer( init_size );
  cursors = begin_semispace_cursors( init_size );
  spaces[0] = tospace;
  cursors[0].chunks_index = tospace->current;
  cursors[0].chunk_ptr = tospace->chunks[ tospace->current ].top;

  CHENEY_TYPE( 2 ); /* Felix has never used GC_HIRES_TIMERS... */
  init_env_with_cursors
    ( &e, gc, spaces, 1, init_size, cursors, 
      0, gs, 0, 
      gc->scan_update_remset ? scan_oflo_normal_update_rs : scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects_in( gc, gs );
  stats_set_gc_event_stats( &cheney );
  
  finis_semispace_cursors( e.cursors, e.tospaces_cap );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void oldspace_copy_using_locations( cheney_env_t *e );

void gclib_stopcopy_collect_locs( gc_t *gc, gset_t gs, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  semispace_cursor_t *cursors;
  int init_size = tospaces_init_buf_size;
  
  spaces = begin_semispaces_buffer( init_size );
  cursors = begin_semispace_cursors( init_size );
  spaces[0] = tospace;
  cursors[0].chunks_index = tospace->current;
  cursors[0].chunk_ptr = tospace->chunks[ tospace->current ].top;

  CHENEY_TYPE( 2 ); /* Felix has never used GC_HIRES_TIMERS... */
  init_env_with_cursors
    ( &e, gc, spaces, 1, init_size, cursors, 
      0, gs, POINTS_ACROSS_FCN, 
      gc->scan_update_remset ? scan_oflo_normal_update_rs : scan_oflo_normal );
  oldspace_copy_using_locations( &e );
  sweep_large_objects_in( gc, gs );
  stats_set_gc_event_stats( &cheney );
  
  finis_semispace_cursors( e.cursors, e.tospaces_cap );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void gclib_stopcopy_promote_into( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  semispace_cursor_t *cursors; 
  int init_size = tospaces_init_buf_size;
  
  spaces = begin_semispaces_buffer( init_size );
  cursors = begin_semispace_cursors( init_size );
  spaces[0] = tospace;
  cursors[0].chunks_index = tospace->current;
  cursors[0].chunk_ptr = tospace->chunks[ tospace->current ].top;

  CHENEY_TYPE( 0 );
  init_env_with_cursors
    ( &e, gc, spaces, 1, init_size, cursors, 
      0, gset_younger_than( tospace->gen_no ), 0, 
      gc->scan_update_remset ? scan_oflo_normal_update_rs : scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );
  
  finis_semispace_cursors( e.cursors, e.tospaces_cap );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void gclib_stopcopy_collect( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  semispace_cursor_t *cursors;
  int init_size = tospaces_init_buf_size;

  spaces = begin_semispaces_buffer( init_size );
  cursors = begin_semispace_cursors( init_size );
  spaces[0] = tospace;
  cursors[0].chunks_index = tospace->current;
  cursors[0].chunk_ptr = tospace->chunks[ tospace->current ].top;

  CHENEY_TYPE( 1 );
  init_env_with_cursors
    ( &e, gc, spaces, 1, init_size, cursors, 
      0, gset_younger_than( tospace->gen_no+1 ), 0, 
      gc->scan_update_remset ? scan_oflo_normal_update_rs : scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );

  finis_semispace_cursors( e.cursors, e.tospaces_cap );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void gclib_stopcopy_collect_and_scan_static( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;
  semispace_t **spaces;
  semispace_cursor_t *cursors;
  int init_size = tospaces_init_buf_size;

  spaces = begin_semispaces_buffer( init_size );
  cursors = begin_semispace_cursors( init_size );
  spaces[0] = tospace;
  cursors[0].chunks_index = tospace->current;
  cursors[0].chunk_ptr = tospace->chunks[ tospace->current ].top;

  CHENEY_TYPE( 1 );
  init_env_with_cursors
    ( &e, gc, spaces, 1, init_size, cursors, 
      0, gset_younger_than( tospace->gen_no+1 ), SCAN_STATIC, 
      gc->scan_update_remset ? scan_oflo_normal_update_rs : scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );

  finis_semispace_cursors( e.cursors, e.tospaces_cap );
  finis_semispaces_buffer( e.tospaces, e.tospaces_cap );
}

void sweep_large_objects_in( gc_t *gc, gset_t genset )
{
  int i;

  for ( i=0; i <= gset_max_elem( genset ); i++ )
    if (gset_memberp( i, genset ))
      los_sweep( gc->los, i );
  los_append_and_clear_list_infer_gen( gc->los, gc->los->mark1 );
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

static word *last_origin_gen_added; 
static const int gf_filter_remset_lhs = 0;
static word gf_last_lhs;

static bool update_remset( cheney_env_t *e,
                           word *origin_ptr, int origin_gen, int origin_tag,
                           int offset, 
                           word target_ptr ) {
  if (e->points_across == NULL)
    return FALSE;
  if ( is_ptr(target_ptr) &&
       last_origin_gen_added != origin_ptr ) {
    int target_gen = gen_of(target_ptr);
    if (origin_gen != target_gen) {
      bool added = e->points_across( e, tagptr(origin_ptr,origin_tag),
                                     offset, target_ptr );
      if (added)
        last_origin_gen_added = origin_ptr;
      return added;
    } else {
      return FALSE;
    }
  } else {
    return FALSE;
  }
}

static bool points_across_noop( cheney_env_t* e, word lhs, int offset, word rhs ) 
{
  return FALSE;
}

static bool points_across( cheney_env_t* e, word lhs, int offset, word rhs )
{
  gc_points_across( e->gc, lhs, offset, rhs );
  return FALSE;
}

static void forwarded( cheney_env_t* e, char *ctxt, 
                       word obj_orig, int gen_orig, 
                       word obj_new, int gen_new )
{
  smircy_when_object_forwarded( e->gc->smircy, 
                                obj_orig, gen_orig, 
                                obj_new, gen_new );
}

static void 
init_env_with_cursors( cheney_env_t *e, 
                       gc_t *gc,
                       semispace_t **tospaces,
                       int tospaces_len,
                       int tospaces_cap,
                       semispace_cursor_t *cursors, 
                       semispace_t *tospace2,
                       gset_t forw_gset,
                       int attributes,
                       void (*scanner)( cheney_env_t * ) )
{
  memset( e, 0, sizeof( cheney_env_t ) );
  e->gc = gc;
  e->gclib_desc_g = gclib_desc_g;
  e->forw_gset = forw_gset;
  e->scan_static = attributes & SCAN_STATIC;
  e->splitting = attributes & SPLITTING_GC;
  e->iflush = gc_iflush( gc );
  e->tospaces = tospaces;
  e->tospaces_len = tospaces_len;
  e->tospaces_cap = tospaces_cap;
  e->cursors = cursors;
  assert( tospaces_len > 0 );
  e->tospaces_cur_scan = 0;
  e->tospaces_cur_dest = 0;
  e->last_forward_was_large = 0;
  e->tospace2 = tospace2;
  e->dest = tospace_dest(e)->chunks[tospace_dest(e)->current].top;
  e->dest2 = (tospace2 ? tospace2->chunks[tospace2->current].top : 0);
  e->lim = tospace_dest(e)->chunks[tospace_dest(e)->current].lim;
  e->lim2 = (tospace2 ? tospace2->chunks[tospace2->current].lim : 0);
  e->los = (e->splitting ? 0 : gc->los);

  e->scan_from_globals = root_scanner_oflo;
  e->scan_from_remsets = ( (e->gc->scan_update_remset)
                           ? remset_scanner_oflo_update_rs
                           : remset_scanner_oflo );

  e->scan_from_tospace = scanner;
  e->points_across = (attributes & POINTS_ACROSS_FCN)
                     ? points_across
                     : points_across_noop;
  e->forwarded = (e->gc->smircy != NULL) ? forwarded : NULL;
}

void init_env( cheney_env_t *e, gc_t *gc,
               semispace_t **tospaces, int tospaces_len, int tospaces_cap,
               semispace_t *tospace2,
               gset_t forw_gset,
               int attributes,
               void (*scanner)( cheney_env_t * ) )
{
  init_env_with_cursors( e, gc, tospaces, tospaces_len, tospaces_cap, NULL, 
                         tospace2, forw_gset, attributes, scanner );
}

static signed objects_scanned;

static void remset_loc_scanner_oflo( loc_t loc, void *data ) {
  assert_loc_ok( loc );
  objects_scanned++; /* a tiny lie, but better than leaving it at 0 */
  root_scanner_oflo( loc_to_slot(loc), data );
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
  e->words_forwarded_from_nursery = 0;

  last_origin_gen_added = (word*)-1;
  gf_last_lhs = -1;

  /* Collect */
  start( &cheney.root_scan_prom, &cheney.root_scan_gc );
  gc_enumerate_smircy_roots( e->gc, e->scan_from_globals, (void*)e );
  gc_enumerate_roots( e->gc, e->scan_from_globals, (void*)e );
  { 
    stats_id_t timer1, timer2;
    int elapsed, cpu;
    gc_t *gc = e->gc;
    timer1 = stats_start_timer( TIMER_ELAPSED );
    timer2 = stats_start_timer( TIMER_CPU );

    objects_scanned = 0;
    gc_enumerate_remsets_complement( e->gc,
                                     e->forw_gset,
                                     e->scan_from_remsets,
                                     (void*)e );


    elapsed = stats_stop_timer( timer1 );
    cpu     = stats_stop_timer( timer2 );
    
    gc->stat_max_entries_remset_scan =
      max( gc->stat_max_entries_remset_scan, objects_scanned );
    gc->stat_max_remset_scan = max( gc->stat_max_remset_scan, elapsed );
    gc->stat_max_remset_scan_cpu = max( gc->stat_max_remset_scan_cpu, cpu );
    gc->stat_total_entries_remset_scan += objects_scanned;
    assert( gc->stat_total_entries_remset_scan >= 0 );
    gc->stat_total_remset_scan += elapsed;
    gc->stat_total_remset_scan_cpu += cpu;
    gc->stat_remset_scan_count++;
    objects_scanned = 0;
  }
  if (e->scan_static && e->gc->static_area) {
    if (e->gc->scan_update_remset) {
      scan_static_area_update_rs( e );
    } else {
      scan_static_area( e );
    }
  }
  stop();

  start( &cheney.tospace_scan_prom, &cheney.tospace_scan_gc );
  e->scan_from_tospace( e );
  stop();

  e->gc->words_from_nursery_last_gc = e->words_forwarded_from_nursery;

  /* Shutdown */
  tospace_dest(e)->chunks[tospace_dest(e)->current].top = e->dest;
  if (e->tospace2)
    e->tospace2->chunks[e->tospace2->current].top = e->dest2;
  assert2( tospace_dest(e) == tospace_scan(e) );
  assert2( tospace_dest(e)->chunks[tospace_dest(e)->current].bot
           <= tospace_dest(e)->chunks[tospace_dest(e)->current].top );
}

void oldspace_copy_using_locations( cheney_env_t *e )
{
  /* Setup */
  e->scan_idx = tospace_scan(e)->current;
  e->scan_idx2 = (e->tospace2 ? e->tospace2->current : 0);
  e->scan_ptr = tospace_scan(e)->chunks[e->scan_idx].top;
  e->scan_ptr2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].top : 0);
  e->scan_lim = tospace_scan(e)->chunks[e->scan_idx].lim;
  e->scan_lim2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].lim : 0);
  e->words_forwarded_from_nursery = 0;

  last_origin_gen_added = (word*)-1;
  gf_last_lhs = -1;

  /* Collect */
  start( &cheney.root_scan_prom, &cheney.root_scan_gc );
  gc_enumerate_smircy_roots( e->gc, e->scan_from_globals, (void*)e );
  gc_enumerate_roots( e->gc, e->scan_from_globals, (void*)e );

  { 
    stats_id_t timer1, timer2;
    int elapsed, cpu;
    gc_t *gc = e->gc;
    timer1 = stats_start_timer( TIMER_ELAPSED );
    timer2 = stats_start_timer( TIMER_CPU );
    objects_scanned = 0;

    gc_enumerate_remembered_locations
      ( e->gc, e->forw_gset, 
        remset_loc_scanner_oflo, (void*)e,
        e->scan_from_remsets, (void*)e );

    elapsed = stats_stop_timer( timer1 );
    cpu     = stats_stop_timer( timer2 );
    
    gc->stat_max_entries_remset_scan =
      max( gc->stat_max_entries_remset_scan, objects_scanned );
    gc->stat_max_remset_scan = max( gc->stat_max_remset_scan, elapsed );
    gc->stat_max_remset_scan_cpu = max( gc->stat_max_remset_scan_cpu, cpu );
    gc->stat_total_entries_remset_scan += objects_scanned;
    assert( gc->stat_total_entries_remset_scan >= 0 );
    gc->stat_total_remset_scan += elapsed;
    gc->stat_total_remset_scan_cpu += cpu;
    gc->stat_remset_scan_count++;
    objects_scanned = 0;
  }
  if (e->scan_static && e->gc->static_area) {
    if (e->gc->scan_update_remset) {
      scan_static_area_update_rs( e );
    } else {
      scan_static_area( e );
    }
  }
  stop();

  start( &cheney.tospace_scan_prom, &cheney.tospace_scan_gc );
  e->scan_from_tospace( e );
  stop();

  e->gc->words_from_nursery_last_gc = e->words_forwarded_from_nursery;

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
  gset_t       forw_gset = e->forw_gset;
  semispace_t *s_data = e->gc->static_area->data_area;
  word        *dest = e->dest;
  word        *lim = e->lim;
  word        *loc, *limit;
  int          i;
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif
  
  for ( i=0 ; i <= s_data->current ; i++ ) {
    loc = s_data->chunks[i].bot;
    limit = s_data->chunks[i].top;
    while ( loc < limit )
      scan_and_forward( loc, e->iflush, forward_nursery_and, forw_gset, 
                        dest, lim, e, 
                        check_space_expand );
  }

  e->dest = dest;
  e->lim = lim;
}

static void scan_static_area_update_rs( cheney_env_t *e )
{
  gset_t      forw_gset = e->forw_gset;
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
      scan_and_forward_update_rs
        ( loc, e->iflush, forward_nursery_and, forw_gset, 
          dest, lim, e, 
          check_space_expand );
  }

  e->dest = dest;
  e->lim = lim;
}

static void root_scanner_oflo( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  forw_oflo( "root_scanner_oflo forw_oflo", ptr,
             forward_nursery_and, e->forw_gset, 
             e->dest, e->lim, e, check_space_expand );
}

static bool remset_scanner_oflo( word object, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  gset_t       forw_gset = e->forw_gset;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *dest = e->dest;
  word         *lim = e->lim;
  word         *loc;            /* Used as a temp by scanner and fwd macros */

  objects_scanned++;
  assert( objects_scanned >= 0 );
  assert2( *ptrof(object) != FORWARD_HDR );
  remset_scanner_core( e, object, loc, 
                       forw_oflo_record_track_old2young 
                                       ( loc, forward_nursery_and, forw_gset,
                                         dest, lim,
                                         has_intergen_ptr, old_obj_gen, e, 
                                         check_space_expand ) );

  e->dest = dest;
  e->lim = lim;
  return has_intergen_ptr;
}

static bool remset_scanner_oflo_update_rs( word object, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  gset_t       forw_gset = e->forw_gset;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *dest = e->dest;
  word         *lim = e->lim;
  word         *loc;            /* Used as a temp by scanner and fwd macros */

  objects_scanned++;
  assert( objects_scanned >= 0 );
  assert2( *ptrof(object) != FORWARD_HDR );
  remset_scanner_update_rs
    ( e, object, loc, 
      forw_oflo_record_track_any2other( loc, 
                        forward_nursery_and, forw_gset, dest, lim,
                        has_intergen_ptr, old_obj_gen, e, 
                        check_space_expand ),
      update_remset );

  e->dest = dest;
  e->lim = lim;
  return has_intergen_ptr;
}

void scan_oflo_normal( cheney_env_t *e )
{
  gset_t   forw_gset = e->forw_gset;
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
        scan_and_forward( scanptr, e->iflush, forward_nursery_and, forw_gset, 
                          dest, copylim, e, check_space_expand );
      }

      if (scanptr != dest) {
        e->scan_idx++;
        if (e->scan_idx > tospace_scan(e)->current) {
          e->tospaces_cur_scan++;
          assert(e->tospaces_cur_scan < e->tospaces_len);
          e->scan_idx = e->cursors[ e->tospaces_cur_scan ].chunks_index;
          scanptr     = e->cursors[ e->tospaces_cur_scan ].chunk_ptr;
          scanlim = tospace_scan(e)->chunks[e->scan_idx].lim;
        } else {
          scanptr = tospace_scan(e)->chunks[e->scan_idx].bot;
          scanlim = tospace_scan(e)->chunks[e->scan_idx].lim;
        }
        
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
      scan_and_forward( p, e->iflush, forward_nursery_and, forw_gset, 
                        dest, copylim, e, check_space_expand );
    }
  } while (morework);

  e->dest = dest;
  e->lim = copylim;
}

void scan_oflo_normal_update_rs( cheney_env_t *e )
{
  gset_t   forw_gset = e->forw_gset;
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
        scan_and_forward_update_rs( scanptr, e->iflush,
                                    forward_nursery_and, forw_gset,
                                    dest, copylim, e, check_space_expand );
      }

      if (scanptr != dest) {
        e->scan_idx++;
        if (e->scan_idx > tospace_scan(e)->current) {
          e->tospaces_cur_scan++;
          assert(e->tospaces_cur_scan < e->tospaces_len);
          assert(e->tospaces_cur_scan <= e->tospaces_cur_dest);
          e->scan_idx = e->cursors[ e->tospaces_cur_scan ].chunks_index;
          scanptr     = e->cursors[ e->tospaces_cur_scan ].chunk_ptr;
          scanlim = tospace_scan(e)->chunks[e->scan_idx].lim;
        } else {
          scanptr = tospace_scan(e)->chunks[e->scan_idx].bot;
          scanlim = tospace_scan(e)->chunks[e->scan_idx].lim;
        }
        
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

    assert( scanptr == dest );

    while ((p = los_walk_list( e->los->mark1, los_p )) != 0) {
      los_p = p;
      morework = 1;
      assert2( ishdr( *p ) );
      scan_and_forward_update_rs( p, e->iflush,
                                  forward_nursery_and, forw_gset, 
                                  dest, copylim, e, check_space_expand );
    }

    assert( e->tospaces_cur_scan <= e->tospaces_cur_dest );

  } while (morework);

  assert2( tospace_dest(e) == tospace_scan(e) );

  e->dest = dest;
  e->lim = copylim;
}

/* For whatever reason, we were flushing the cache on bytevectors
 * found in the from space.  This flushes the cache on bytevectors
 * after they've been copied to the new space.
 */

void copied_icache_flush( word *bv ) {
  word hdr = *bv;
  word T_h = header( hdr );
  word T_bytes = sizefield( hdr );
  word *start = bv;
  word *end = (word *) (((char *) (start + 1)) + roundup8( T_bytes ));
  if ( T_h == BV_HDR )
    mem_icache_flush( start, end );
}

/* "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address (or, in the case
 * of an unmoved large objects, returning ptr (but still marking 
 * it within the LOS; see forward_large_object(..)).
 *
 * *dest is updated to reflect allocation of a copy in the to-space.
 *
 * Most objects are smallish, so this code should be biased in favor
 * of small objects.
 */

word forward( const word p, word **dest, cheney_env_t *e )
{
  word hdr, *newptr, *p1, *p2;
  word ret;
  int wordsz;

  const word tag = tagof( p ); 
  word * const ptr = ptrof( p );

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
  newptr = p1;    /* really *dest, but the compiler is dumb. */
  p2 = ptr;

  hdr = *ptr;
  assert2( ishdr( hdr ) );

#if FORW_BY_LOOP
  { unsigned words;

    /* gcc gets this right, so no sense in being obscure.
       words = (((hdr >> 8) + 11) >> 3) << 1; */
    words = roundup8( sizefield( hdr ) + 4 ) / 4;
    wordsz = words;

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
      return forward_large_object( e, ptr, tag, tospace_dest(e)->gen_no );
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
    return forward_large_object( e, ptr, tag, gen_of(p1) );

  wordsz = bytes / sizeof(word);

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
    return forward_large_object( e, ptr, tag, gen_of(p1) );

  wordsz = bytes / sizeof(word);

  memcpy( p1, p2, bytes );
  *dest = p1 + (bytes >> 2);
  }
#endif

  ret = install_fwdptr( ptr, newptr, tag );
  FORWARDED( e, "forward", p, gen_of(p), ret, gen_of(ret), wordsz);
  if ( tag == BVEC_TAG )
    copied_icache_flush( newptr );
  return ret;
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

   The fake bignum is not treated as allocated by the metadata
   of ss itself.  That is, a later attempt to allocate into ss will happily
   overwrite the fake bignum header, since the top field of current chunk
   ("Pointer to neext free word") will still point at the inserted bignum.
   */

void seal_chunk( semispace_t *ss, word *lim, word *dest )
{
  if (dest < lim) {
    word len = (lim - dest)*sizeof(word);
    *dest = mkheader(len-sizeof(word),BIGNUM_HDR);
    if (dest+1 < lim) *(dest+1) = 0xABCDABCD;
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

static 
void enqueue_tospace( cheney_env_t *e, semispace_t *ss ) 
{
  int idx;

  if (e->tospaces_len == e->tospaces_cap) {
    int new_cap = e->tospaces_cap * 2;
    e->tospaces
      = enlarge_semispaces_buffer( e->tospaces, e->tospaces_len, new_cap );
    e->cursors
      = enlarge_semispace_cursors( e->cursors, e->tospaces_len, new_cap );
    e->tospaces_cap = new_cap;
  }
  
  idx = e->tospaces_len;
  e->tospaces[idx] = ss;
  /* after reaching ss, scan objects forwarded into ss during this gc;
   * i.e. those above top (as of now), *not* starting from bot. */
  e->cursors[idx].chunks_index = ss->current;
  e->cursors[idx].chunk_ptr = ss->chunks[ ss->current ].top;
  e->tospaces_len = idx + 1;
}

void
expand_space( cheney_env_t *e, word **lim, word **dest, unsigned bytes )
{
  semispace_t *ss;
  int i;

  supremely_annoyingmsg("   expand_space( e, 0x%08x, 0x%08x, %d )", 
                        *lim, *dest, bytes );

  ss = tospace_dest(e);
  seal_chunk( ss, *lim, *dest );
  
  assert( e->tospaces_cur_scan <= e->tospaces_cur_dest );
  assert( /* not an inherent structural invariant! */
          e->tospaces_cur_dest == e->tospaces_len - 1 );
  assert( e->tospaces_len <= e->tospaces_cap );

  ss = gc_find_space( e->gc, bytes, ss );

  /* check that gc_find_space obeys its contract and did not return a
     filtered space. */
  for( i = 0; i < e->tospaces_len - 1; i++ ) 
    assert( ss != e->tospaces[ i ] );
  
  if ( ss == tospace_dest( e ) ) {
    /* e->gc chose to expand the current semispace, so we do not need
       to adjust the tospaces array. */
  } else {
    e->tospaces_cur_dest++;
    assert(e->tospaces_len == e->tospaces_cur_dest);
    enqueue_tospace( e, ss );
  }
  
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

static word forward_large_object( cheney_env_t * const e,
                                  word * const ptr,
                                  const int tag,
                                  const int tgt_gen )
{
  const word p = tagptr( ptr, tag );
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
    int src_gen = gen_of(ptr);
    was_marked
      = los_mark_and_set_generation( los, mark_list, ptr, src_gen, tgt_gen );
    ret = tagptr( ptr, tag );
    /* This is a slight lie; ptr was not copied, but its gno 
     * may have changed, which SMIRCY needs to know about... */
    FORWARDED( e, "forwarded_large_object 1", ret, src_gen, ret, tgt_gen, 
               (bytes/sizeof(word)) );
  }
  else {
    /* The large object was not allocated specially, so we must move it. */
    word *new;

    hdr = *ptr;
    bytes = roundup8( sizefield( hdr ) + 4 );
    new = los_allocate( los, bytes, gen_of( ptr ) );
    memcpy( new, ptr, bytes );
    
    /* Must mark it also! */
    was_marked
      = los_mark_and_set_generation( los, mark_list, new,
                                     gen_of( ptr ), tgt_gen );
    
    ret = install_fwdptr( ptr, new, tag );
    FORWARDED( e,"forwarded_large_object 2", p, gen_of(p), ret, tgt_gen,
               (bytes/sizeof(word)) );
    if ( tag == BVEC_TAG )
      copied_icache_flush( new );
  }

  if (e->np_promotion && !was_marked) {
    e->np.old_los_bytes += sub1;
    e->np.young_los_bytes += sub2;
    e->np.old_los_steps = ceildiv( e->np.old_los_bytes, GC_CHUNK_SIZE );
    e->np.young_los_steps = ceildiv( e->np.young_los_bytes, GC_CHUNK_SIZE );
  }

  e->last_forward_was_large = TRUE;
  return ret;
}

/* eof */

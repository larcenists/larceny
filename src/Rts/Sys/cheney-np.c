/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: cheney.c 5158 2007-11-26 20:55:58Z pnkfelix $
 *
 * Larceny -- cheney extension for non-predictive (ROF) collection.
 * 
 * Entry points (from outside cheney-* files):
 *   gclib_stopcopy_promote_into_np
 *   gclib_stopcopy_collect_np
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
#include "seqbuf_t.h"

/* Additional forwarding macros for non-predictive promotion.

   During non-predictive promotion, objects are moved first into the old
   area until it fills up, then into the young area.  The counter
   e.np.old_steps_remaining keeps track of available space in the old
   area; when a chunk is full and this counter is found to be 0, then
   the old area is full.

   Forw_np() is like forw_oflo() but is only suitable for scanning the
   'old' area after promotion.

   Forw_np_record() is like forw_oflo_record().

   Forw_np_partial() is like forw_np(), but suitable for scanning the
   non-predictive 'young' area after promotion into it.  It records
   whether an object needs to be added to the non-predictive extra
   remembered set.

   Forw_core_np() is like forw_core().

   Check_space_np() is like check_space().

   Scan_core_partial() is like scan_core(), but is useful only when
   scanning the non-predictive 'young' area, and adds an object to the
   non-predictive extra remembered set if forw_np_partial() indicates
   that it must be done.
   */

#define forw_np( loc, genset, dest, lim, e )                           \
  do { word T_obj = *loc;                                              \
       if (isptr( T_obj ) && gset_memberp( gen_of(T_obj), (genset))) { \
          forw_core_np( T_obj, loc, dest, lim, e );                    \
       }                                                               \
  } while( 0 )

/* See comments for forw_oflo_record regarding this implementation. 
   */
#define forw_np_record( loc, genset, dest, lim, has_intergen_ptr, \
                        old_obj_gen, e )                          \
  do { word T_obj = *loc;                                         \
       if (isptr( T_obj )) {                                      \
          unsigned T_obj_gen = gen_of(T_obj);                     \
          if ( gset_memberp( T_obj_gen, (genset))) {        \
            forw_core_np( T_obj, loc, dest, lim, e );             \
          }                                                       \
          if (T_obj_gen < (old_obj_gen)) has_intergen_ptr=1;      \
       }                                                          \
  } while( 0 )

/* In general, the generation lookup must be done twice because
   it is possible for an object to be forwarded into the non-predictive
   'old' space during a promotion to both 'old' and 'young'.
   In particular, this happens if one object is forwarded into 'old',
   then a second object, pointing to the first, is forwarded into 'young',
   and then scanned.  The pointer in the second object will then
   change from a pointer into the ephemeral area to a pointer into
   'old'.  The second object must now be added to the non-predictive
   remembered set.

   In the old non-predictive collector, this did not happen, and the logic 
   was less expensive.
   */
#define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen,  \
                         must_add_to_extra, e )                         \
  do { word T_obj = *loc;                                               \
       if ( isptr( T_obj ) ) {                                          \
           if (gen_of(T_obj) < (forw_limit_gen)) {                      \
             forw_core_np( T_obj, loc, dest, lim, e );                  \
             T_obj = *loc;                                              \
           }                                                            \
           if (gen_of(T_obj) < (np_young_gen))                          \
             must_add_to_extra = 1;                                     \
       }                                                                \
  } while( 0 )

#define forw_core_np( T_obj, loc, dest, lim, e )        \
  word *TMP_P = ptrof( T_obj );                         \
  word TMP_W = *TMP_P;                                  \
  if (TMP_W == FORWARD_HDR)                             \
    *loc = *(TMP_P+1);                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                \
    check_space_np(dest,lim,8,e);                       \
    *dest = TMP_W;                                      \
    *(dest+1) = *(TMP_P+1);                             \
    check_address( TMP_P );                             \
    *TMP_P = FORWARD_HDR;                               \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);   \
    check_memory( dest, 2 );                            \
    dest += 2;                                          \
  }                                                     \
  else {                                                \
    word *TMPD;                                         \
    check_space_np(dest,lim,sizefield(TMP_W)+4,e);      \
    TMPD = dest;                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD; \
  }

#define check_space_np( dest, lim, wanted, e )                               \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    expand_semispace_np( &CS_LIM, &CS_DEST, (wanted), e );                   \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

#define scan_core_partial( ptr, iflush, FORW, must_add_to_extra, e )          \
  do {                                                                        \
    word T_w = *ptr;                                                          \
    assert2( T_w != FORWARD_HDR);                                             \
    if (ishdr( T_w )) {                                                       \
      word T_h = header( T_w );                                               \
      if (T_h == BV_HDR) {                                                    \
        /* bytevector: skip it, and flush the icache if code */               \
        word *T_oldptr = ptr;                                                 \
        word T_bytes = roundup4( sizefield( T_w ) );                          \
        ptr = (word *)((word)ptr + (T_bytes + 4)); /* doesn't skip padding */ \
        if (!(T_bytes & 4)) *ptr++ = 0;            /* pad. */                 \
        /* Only code vectors typically use a plain bytevector typetag,        \
         * so almost any bytevector will be a code vector that must           \
         * be flushed.                                                        \
         */                                                                   \
        if (iflush && typetag( T_w ) == BVEC_SUBTAG)                          \
          mem_icache_flush( T_oldptr, ptr );                                  \
      }                                                                       \
      else {                                                                  \
        /* vector or procedure: scan in a tight loop */                       \
        word T_words = sizefield( T_w ) >> 2;                                 \
        word* T_objp = ptr;                                                   \
        int must_add_to_extra = 0;                                            \
        ptr++;                                                                \
        while (T_words--) {                                                   \
          FORW;                                                               \
          ptr++;                                                              \
        }                                                                     \
        if (must_add_to_extra) remember_vec( tagptr( T_objp, VEC_TAG ), e );  \
        if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */                   \
      }                                                                       \
    }                                                                         \
    else {                                                                    \
      int must_add_to_extra = 0;                                              \
      FORW;                                                                   \
      ptr++;                                                                  \
      FORW;                                                                   \
      ptr++;                                                                  \
      if (must_add_to_extra) remember_pair( tagptr( ptr-2, PAIR_TAG ), e );   \
    }                                                                         \
  } while (0)

#define remember_vec( w, e )                    \
 do {  word *X = *e->np.ssbtop;                 \
       *X = w; X += 1; *e->np.ssbtop = X;       \
       if (X == *e->np.ssblim) {                \
         compact_np_ssb( e->gc );               \
       }                                        \
 } while(0)

#define remember_pair( w, e ) remember_vec( w, e )

#define FORW_NP_ENV_BEGIN( e_, dest_, lim_ )                    \
  word *dest_ = (e_->np.has_switched ? e_->dest2 : e_->dest);   \
  word *lim_ = (e_->np.has_switched ? e_->lim2 : e_->lim);

#define FORW_NP_ENV_END( e_, dest_, lim_ )                              \
  if (e_->np.has_switched) { e_->dest2 = dest_; e_->lim2 = lim_; }      \
  else { e_->dest = dest_; e_->lim = lim_; }

/* External */

extern void mem_icache_flush( void *start, void *end );

/* Private procedures (private to cheney* at least). */

static void init_np_env( cheney_env_t *e, gc_t *gc,
                         semispace_t *tospace, semispace_t *tospace2,
                         int  effective_generation,
                         int  attributes,
                         void (*scanner)( cheney_env_t * ) );

#if ROF_COLLECTOR
static void root_scanner_np( word *ptr, void *data );
static bool remset_scanner_np( word obj, void *data, unsigned *count );
static void scan_oflo_np_promote( cheney_env_t *e );
static void expand_semispace_np( word **, word **, unsigned, cheney_env_t* );
#endif

#if ROF_COLLECTOR
static void compact_np_ssb( gc_t *gc )
{
  int i;
  /* The below was rs_compact_nocheck( gc->remset[gc->np_remset] );
   * but that function has been removed.  This does the same job,
   * but probably less efficiently because it is going to check for
   * membership before adding elements to the remset. */
  for (i = 0; i < gc->remset_count ; i++ ) {
    process_seqbuf( gc, gc->ssb[i] );
  }
}


void gclib_stopcopy_promote_into_np( gc_t *gc,
                                     semispace_t *old, semispace_t *young,
                                     int old_remaining, int young_remaining )
{
  cheney_env_t e;

  CHENEY_TYPE( 0 );
  init_np_env( &e, gc, old, young, old->gen_no, NP_PROMOTION, 
               scan_oflo_np_promote);
  e.np.old_steps_remaining = old_remaining / GC_CHUNK_SIZE;
  e.np.young_steps_remaining = young_remaining / GC_CHUNK_SIZE;
  gc_np_remset_ptrs( gc, &e.np.ssbtop, &e.np.ssblim );

  oldspace_copy( &e );
  compact_np_ssb( gc );
  sweep_large_objects( gc, old->gen_no-1, old->gen_no, young->gen_no );
  stats_set_gc_event_stats( &cheney );
}

void gclib_stopcopy_collect_np( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  CHENEY_TYPE( 1 );
  init_np_env( &e, gc, tospace, 0, tospace->gen_no, ENUMERATE_NP_REMSET, 
               scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
  stats_set_gc_event_stats( &cheney );
}
#endif /* ROF_COLLECTOR */

static void init_np_env( cheney_env_t *e, 
                         gc_t *gc,
                         semispace_t *tospace, 
                         semispace_t *tospace2,
                         int effective_generation,
                         int attributes,
                         void (*scanner)( cheney_env_t * ) )
{
  init_env( e, gc, &tospace, 1, 1, tospace2, gset_younger_than( effective_generation ), 
            attributes, scanner );

  e->np_promotion = attributes & NP_PROMOTION;
  e->enumerate_np_remset = attributes & ENUMERATE_NP_REMSET;

  if (e->np_promotion)    e->scan_from_globals = root_scanner_np;

  if (e->np_promotion)    e->scan_from_remsets = remset_scanner_np;
} 

#if ROF_COLLECTOR
static void root_scanner_np( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  FORW_NP_ENV_BEGIN( e, dest, lim )

  forw_np( ptr, e->forw_gset, dest, lim, e );

  FORW_NP_ENV_END( e, dest, lim )
}
#endif

#if ROF_COLLECTOR
static bool remset_scanner_np( word object, void *data, unsigned *count )
{
  cheney_env_t *e = (cheney_env_t*)data;
  gset_t       forw_gset = e->forw_gset;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *loc;            /* Used as a temp by scanner and fwd macros */
  FORW_NP_ENV_BEGIN( e, dest, lim )

  remset_scanner_core( e, object, loc, 
                       forw_np_record( loc, forw_gset, dest, lim,
                                       has_intergen_ptr, old_obj_gen, e ),
                       *count );

  FORW_NP_ENV_END( e, dest, lim )
  return has_intergen_ptr;
}
#endif

#if ROF_COLLECTOR
static void scan_np_old( cheney_env_t *e );
static void scan_np_young( cheney_env_t *e );
static void scan_np_los_old( cheney_env_t *e, word **los_p_arg );
static void scan_np_los_young( cheney_env_t *e, word **los_p_arg );

static void scan_oflo_np_promote( cheney_env_t *e )
{
  word *los_p_old = 0, *los_p_young = 0;
  bool work;

  do {
    if (e->scan_ptr == e->scan_lim && e->scan_idx < tospace_scan(e)->current) {
      e->scan_idx++;
      e->scan_ptr = tospace_scan(e)->chunks[ e->scan_idx ].bot;
      e->scan_lim = tospace_scan(e)->chunks[ e->scan_idx ].lim;
    }
    if (e->scan_ptr2 == e->scan_lim2 && e->scan_idx2 < e->tospace2->current) {
      e->scan_idx2++;
      e->scan_ptr2 = e->tospace2->chunks[ e->scan_idx2 ].bot;
      e->scan_lim2 = e->tospace2->chunks[ e->scan_idx2 ].lim;
    }

    /* Explicitly die rather than infinite loop below. */
    assert( e->scan_ptr <= e->scan_lim );
    assert( e->scan_ptr2 <= e->scan_lim2 );

    work = 0;
    if (e->scan_ptr != e->scan_lim && e->scan_ptr != e->dest) {
      scan_np_old( e );
      work=1;
    }
    if (los_walk_list( e->los->mark1, los_p_old ) != 0) {
      scan_np_los_old( e, &los_p_old );
      work=1;
    }
    if (e->scan_ptr2 != e->scan_lim2 && e->scan_ptr2 != e->dest2) {
      scan_np_young( e );
      work=1;
    }
    if (los_walk_list( e->los->mark2, los_p_young ) != 0) {
      scan_np_los_young( e, &los_p_young );
      work=1;
    }
  } while( work );

  assert( e->scan_idx == tospace_scan(e)->current );
  assert( e->scan_idx2 == e->tospace2->current );
}

static void scan_np_old( cheney_env_t *e )
{
  unsigned forw_limit_gen = tospace_scan(e)->gen_no;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  while (scanptr != dest && scanptr < scanlim) {
    scan_core( e, scanptr, e->iflush,
               forw_np( scanptr, gset_younger_than( forw_limit_gen ), 
                        dest, copylim, e ) );
  }

  e->scan_ptr = scanptr;
  e->scan_lim = scanlim;
  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_young( cheney_env_t *e )
{
  unsigned forw_limit_gen = tospace_dest(e)->gen_no; /* [sic] */
  unsigned np_young_gen = e->tospace2->gen_no;
  word     *scanptr = e->scan_ptr2;
  word     *scanlim = e->scan_lim2;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
#if GCLIB_LARGE_TABLE && SHADOW
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  /* Ensure above choice for forw_limit_gen was irrelevant. */
  assert( e->tospaces_cur_scan == e->tospaces_cur_dest ); 

  /* must_add_to_extra is a name used by the scanning and fwd macros as a 
     temp */
  while (scanptr != dest && scanptr < scanlim) {
    scan_core_partial( scanptr, e->iflush,
                       forw_np_partial( scanptr, forw_limit_gen, dest, copylim,
                                        np_young_gen, must_add_to_extra, e ),
                       must_add_to_extra, e );
  }

  e->scan_ptr2 = scanptr;
  e->scan_lim2 = scanlim;
  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_los_old( cheney_env_t *e, word **los_p )
{
  unsigned forw_limit_gen = tospace_dest(e)->gen_no;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
  word     *p;
#if GCLIB_LARGE_TABLE && SHADOW
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  /* Ensure above choice for forw_limit_gen was irrelevant. */
  assert( e->tospaces_cur_scan == e->tospaces_cur_dest ); 

  while ((p = los_walk_list( e->los->mark1, *los_p )) != 0) {
    *los_p = p;
    assert2( ishdr( *p ) );
    scan_core( e, p, e->iflush,
               forw_np( p, gset_younger_than( forw_limit_gen ), dest, copylim, e ) );
  }

  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_los_young( cheney_env_t *e, word **los_p )
{
  unsigned forw_limit_gen = tospace_dest(e)->gen_no;      /* [sic] */
  unsigned np_young_gen = e->tospace2->gen_no;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
  word     *p;
#if GCLIB_LARGE_TABLE && SHADOW
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  /* Ensure above choice for forw_limit_gen was irrelevant. */
  assert( e->tospaces_cur_scan == e->tospaces_cur_dest ); 

  /* must_add_to_extra is a name used by the scanning and fwd macros as a 
     temp */
  while ((p = los_walk_list( e->los->mark2, *los_p )) != 0) {
    *los_p = p;
    assert2( ishdr( *p ) );
    scan_core_partial( p, e->iflush,
                       forw_np_partial( p, forw_limit_gen, dest, copylim,
                                        np_young_gen, must_add_extra, e ),
                       must_add_extra, e );
  }

  FORW_NP_ENV_END( e, dest, copylim )
}
#endif /* ROF_COLLECTOR */

#if ROF_COLLECTOR
static void
expand_semispace_np( word **lim, word **dest, unsigned bytes, cheney_env_t *e )
{
  semispace_t *ss;

 again:
  if (e->np.has_switched) {
    ss = e->tospace2;
    e->np.young_steps_remaining--;
  }
  else if (e->np.old_steps_remaining - e->np.old_los_steps > 0) {
    ss = tospace_dest(e);
    e->np.old_steps_remaining--;
  }
  else {
    e->np.has_switched = 1;
    e->dest = *dest;
    e->lim = *lim;
    seal_chunk( tospace_dest(e), *lim, *dest );
    ss = e->tospace2;
    *dest = ss->chunks[ ss->current ].top; /* [sic] */
    *lim = ss->chunks[ ss->current ].lim;
    if ((char*)*lim - (char*)*dest < bytes) goto again; else return;
  }
  seal_chunk( ss, *lim, *dest );
  ss_expand( ss, max( bytes, GC_CHUNK_SIZE ) );
  *dest = ss->chunks[ ss->current ].bot;
  *lim = ss->chunks[ ss->current ].lim;
}
#endif /* ROF_COLLECTOR */

/* eof */

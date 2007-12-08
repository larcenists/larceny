/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: cheney.c 5158 2007-11-26 20:55:58Z pnkfelix $
 *
 * Larceny -- cheney extension for heap splitting. 
 *
 * Entry points (from outside cheney-* files):
 *   gclib_stopcopy_split_heap
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

/* Additional forwarding macros for a splitting garbage collection.

   This is a hack and should go away, but it's still used to support
   the Boehm collector by allowing the creation of a clean split heap.

   A splitting gc uses two destination semispaces: one for pointer data
   (ss_data) and one for non-pointer data (ss_text).

   Forw_oflo2() is like forw_oflo(); forw_core2() is like forw_core().
   The same scanning macros are used for this type of collection as
   for a normal collection.

   Check_space2() differs from check_space() in that large objects are
   not handled specially -- during splitting gc, large objects are
   copied the same way as other objects.
   */

#define forw_oflo2( loc, forw_limit_gen, dest, dest2, lim, lim2, e )          \
  do { word T_obj = *loc;                                                     \
       if (isptr( T_obj ) && gen_of(T_obj) < (forw_limit_gen)){ \
          forw_core2( T_obj, loc, dest, dest2, lim, lim2, e, forw_limit_gen ); \
       }                                                                       \
  } while( 0 )

#define FORW_PAIR( TMP_P, loc, dest, lim, e, forw_limit_gen )            \
  do {                                                                   \
    word next_obj;                                                       \
    check_space2(dest,lim,8,e->tospace2);                                \
    *dest = *TMP_P;                                                      \
    *(dest+1) = next_obj = *(TMP_P+1);                                   \
    check_address( TMP_P );                                              \
    *TMP_P = FORWARD_HDR;                                                \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);                    \
    check_memory( dest, 2 );                                             \
    dest += 2;                                                           \
  } while ( 0 )

#define forw_core2( T_obj, loc, dest, dest2, lim, lim2, e, forw_limit_gen )             \
  word *TMP_P = ptrof( T_obj );                                         \
  if (*TMP_P == FORWARD_HDR)                                            \
    *loc = *(TMP_P+1);                                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                                \
    FORW_PAIR( TMP_P, loc, dest, lim, e, forw_limit_gen);               \
  }                                                                     \
  else if (tagof( T_obj ) == BVEC_TAG) {                                \
    word *TMPD;                                                         \
    check_space2(dest2,lim2,sizefield(*TMP_P)+4,e->tospace2); /*text*/  \
    TMPD = dest2;                                                       \
    *loc = forward( T_obj, &TMPD, e ); dest2 = TMPD;              \
  }                                                                     \
  else {                                                                \
    word *TMPD;                                                         \
    check_space2(dest,lim,sizefield(*TMP_P)+4,tospace_dest(e));/*data*/ \
    TMPD = dest;                                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD;               \
  }

#define check_space2( dest, lim, wanted, semispace )            \
  if ((char*)lim-(char*)dest < (wanted)) {                      \
    word *CS_LIM=lim, *CS_DEST=dest;                            \
    expand_semispace( semispace, &CS_LIM, &CS_DEST, (wanted) ); \
    dest = CS_DEST; lim = CS_LIM;                               \
  }

static void scan_oflo_splitting( cheney_env_t *e );

void gclib_stopcopy_split_heap( gc_t *gc, semispace_t *data, semispace_t *text)
{
  cheney_env_t e;

  init_env( &e, gc, &data, 1, 1, text, data->gen_no+1, SPLITTING_GC,
            scan_oflo_splitting );
  oldspace_copy( &e );
  /* Note: No LOS sweeping */
}

static void scan_oflo_splitting( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->effective_generation;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  word     *dest = e->dest;
  word     *dest2 = e->dest2;
  word     *copylim = e->lim;
  word     *copylim2 = e->lim2;

  while (scanptr != dest) {
    while (scanptr != dest && scanptr < scanlim) {
      scan_core( e, scanptr, e->iflush,
                 forw_oflo2( scanptr, forw_limit_gen, dest, dest2,
                             copylim, copylim2, e ), 
                 no_update_remset );
    }

    if (scanptr != dest) {
      e->scan_idx++;
      scanptr = tospace_scan(e)->chunks[e->scan_idx].bot;
      scanlim = tospace_scan(e)->chunks[e->scan_idx].lim;
    }
  }

  e->dest = dest;
  e->dest2 = dest2;
  e->lim = copylim;
  e->lim2 = copylim2;
}
